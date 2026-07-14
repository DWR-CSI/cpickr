library(testthat)
library(dplyr)
library(readr)
library(tibble)

test_that("write_integra_files errors on invalid inputs", {
  # Input data is not a data frame or tibble
  expect_error(write_integra_files(matrix(1:4, 2, 2)), "Input 'data' must be a tibble or a data frame.")

  # Empty data frame
  expect_error(write_integra_files(tibble(SampleID = character(), PlateID = character(), WellID = character())), "Input 'data' must contain at least one row.")

  # Missing columns
  expect_error(write_integra_files(tibble(a = 1)), "Input 'data' must contain a column named 'PlateID'.")
  expect_error(write_integra_files(tibble(PlateID = "P1")), "Input 'data' must contain a column named 'SampleID'.")
  expect_error(write_integra_files(tibble(PlateID = "P1", SampleID = "S1")), "Input 'data' must contain a column named 'WellID'.")

  valid_data <- tibble(SampleID = "S1", PlateID = "P1", WellID = "A1")

  # Invalid target_plate_prefix
  expect_error(write_integra_files(valid_data, target_plate_prefix = 123), "Input 'target_plate_prefix' must be a single non-NA character string.")
  expect_error(write_integra_files(valid_data, target_plate_prefix = c("A", "B")), "Input 'target_plate_prefix' must be a single non-NA character string.")

  # Invalid file_prefix
  expect_error(write_integra_files(valid_data, file_prefix = NA_character_), "Input 'file_prefix' must be a single non-NA character string.")

  # Invalid transfer_volume
  expect_error(write_integra_files(valid_data, transfer_volume = -10), "Input 'transfer_volume' must be a positive number.")
  expect_error(write_integra_files(valid_data, transfer_volume = "50"), "Input 'transfer_volume' must be a positive number.")

  # Invalid auto_dest_well
  expect_error(write_integra_files(valid_data, auto_dest_well = "TRUE"), "Input 'auto_dest_well' must be a single logical value.")

  # Invalid max_source_plates
  expect_error(write_integra_files(valid_data, max_source_plates = -1), "Input 'max_source_plates' must be an integer between 1 and 3.")
  expect_error(write_integra_files(valid_data, max_source_plates = 4), "Input 'max_source_plates' must be an integer between 1 and 3.")
  expect_error(write_integra_files(valid_data, max_source_plates = "2"), "Input 'max_source_plates' must be an integer between 1 and 3.")
  expect_error(write_integra_files(valid_data, max_source_plates = 2.5), "Input 'max_source_plates' must be an integer between 1 and 3.")
})

test_that("write_integra_files processes example_plate_data and splits correctly by plate capacity", {
  # Load the example dataset from the package
  data("example_plate_data", package = "cpickr", envir = environment())
  
  # Clean out empty wells to simulate real cherrypicking
  real_samples <- example_plate_data %>%
    dplyr::filter(SampleID != "EMPTY") %>%
    head(300) # Select 300 samples to test multi-plate behavior

  prefix <- paste0(tempfile(), "_")
  
  expect_no_error(write_integra_files(real_samples, target_plate_prefix = "INTEGRA_TARGET", file_prefix = prefix))

  # 300 samples should require multiple target plates since each holds 96.
  # 300 / 96 = 3.125 -> 4 target plates in total:
  # TARGET_1: 96 samples
  # TARGET_2: 96 samples
  # TARGET_3: 96 samples
  # TARGET_4: 12 samples
  #
  # Note: Splits can also happen earlier if unique source plate count exceeds 3.
  # Let's count actual files generated.
  summary_file <- paste0(prefix, "summary.tsv")
  expect_true(file.exists(summary_file))

  # Read and check summary
  summary_df <- readr::read_tsv(summary_file, show_col_types = FALSE)
  expect_true(nrow(summary_df) >= 4)
  expect_equal(names(summary_df), c("Round", "FileName", "TargetPlateID", "SourcePlateIDs", "SampleCount"))

  # Check that each file in the summary exists and contains the correct number of rows and columns
  all_runs_list <- list()
  for (i in 1:nrow(summary_df)) {
    run_file <- file.path(dirname(prefix), summary_df$FileName[i])
    expect_true(file.exists(run_file))

    # Read run file (semicolon delimited)
    run_df <- readr::read_delim(run_file, delim = ";", show_col_types = FALSE)
    expect_equal(nrow(run_df), summary_df$SampleCount[i])
    expect_equal(names(run_df), c("SampleID", "SourcePlateID", "SourceWell", "TargetPlateID", "TargetWell", "TransferVolume"))
    
    # Check that TargetPlateID matches summary
    expect_true(all(run_df$TargetPlateID == summary_df$TargetPlateID[i]))
    
    # Check that number of unique source plates in run is <= 3
    expect_true(length(unique(run_df$SourcePlateID)) <= 3)

    all_runs_list[[i]] <- run_df

    # Clean up file
    unlink(run_file)
  }

  all_runs_df <- dplyr::bind_rows(all_runs_list)

  # Check that no destination/target plate contains more than 96 samples,
  # and each well position per target plate is unique.
  target_plate_summary <- all_runs_df %>%
    dplyr::group_by(TargetPlateID) %>%
    dplyr::summarize(
      TotalSamples = dplyr::n(),
      UniqueWells = dplyr::n_distinct(TargetWell),
      .groups = "drop"
    )

  expect_true(all(target_plate_summary$TotalSamples <= 96))
  expect_equal(target_plate_summary$UniqueWells, target_plate_summary$TotalSamples)

  unlink(summary_file)
})

test_that("write_integra_files splits on 3 source plate limit, keeping same target plate", {
  # Create a sequence of samples:
  # P1 (x2) -> P2 (x2) -> P3 (x2) -> P4 (x2) -> P5 (x2)
  # All target Wells should go to TARGET_1 (total 10 wells used).
  # Run 1: P1, P2, P3 (6 samples)
  # Run 2: P4, P5 (4 samples)
  data <- tibble(
    SampleID = paste0("S", 1:10),
    PlateID = rep(paste0("P", 1:5), each = 2),
    WellID = "A1"
  )

  prefix <- paste0(tempfile(), "_")
  expect_no_error(write_integra_files(data, target_plate_prefix = "TPLATE", file_prefix = prefix))

  summary_file <- paste0(prefix, "summary.tsv")
  expect_true(file.exists(summary_file))

  summary_df <- readr::read_tsv(summary_file, show_col_types = FALSE)
  expect_equal(nrow(summary_df), 2) # 2 runs/rounds

  # Check details of each round
  expect_equal(summary_df$TargetPlateID, rep("TPLATE_1", 2)) # Same target plate!
  expect_equal(summary_df$SampleCount, c(6, 4))
  expect_equal(summary_df$SourcePlateIDs, c("P1, P2, P3", "P4, P5"))

  # Read run files to verify target well assignments
  well_ID_order <- paste0(rep(LETTERS[1:8], 12), rep(1:12, each = 8))

  # Round 1
  f1 <- file.path(dirname(prefix), summary_df$FileName[1])
  df1 <- readr::read_delim(f1, delim = ";", show_col_types = FALSE)
  expect_equal(df1$TargetWell, well_ID_order[1:6])
  unlink(f1)

  # Round 2
  f2 <- file.path(dirname(prefix), summary_df$FileName[2])
  df2 <- readr::read_delim(f2, delim = ";", show_col_types = FALSE)
  expect_equal(df2$TargetWell, well_ID_order[7:10])
  unlink(f2)

  unlink(summary_file)
})

test_that("write_integra_files handles TransferVolume column and default transfer_volume", {
  # 1. Test default volume
  data1 <- tibble(
    SampleID = "S1",
    PlateID = "P1",
    WellID = "A1"
  )
  prefix1 <- paste0(tempfile(), "_")
  write_integra_files(data1, file_prefix = prefix1, transfer_volume = 120)
  
  f1 <- paste0(prefix1, "1.csv")
  df1 <- readr::read_delim(f1, delim = ";", show_col_types = FALSE)
  expect_equal(df1$TransferVolume, 120)
  unlink(c(f1, paste0(prefix1, "summary.tsv")))

  # 2. Test input data with TransferVolume column (with random volumes < 300)
  set.seed(42)
  random_vols <- sample(10:290, 5)
  data2 <- tibble(
    SampleID = paste0("S", 1:5),
    PlateID = "P1",
    WellID = paste0("A", 1:5),
    TransferVolume = random_vols
  )
  prefix2 <- paste0(tempfile(), "_")
  write_integra_files(data2, file_prefix = prefix2, transfer_volume = 50) # default is ignored

  f2 <- paste0(prefix2, "1.csv")
  df2 <- readr::read_delim(f2, delim = ";", show_col_types = FALSE)
  expect_equal(df2$TransferVolume, random_vols)
  unlink(c(f2, paste0(prefix2, "summary.tsv")))
})

test_that("write_integra_files respects auto_dest_well = FALSE", {
  data <- tibble(
    SampleID = paste0("S", 1:5),
    PlateID = "P1",
    WellID = paste0("A", 1:5)
  )
  prefix <- paste0(tempfile(), "_")
  write_integra_files(data, file_prefix = prefix, auto_dest_well = FALSE)

  f <- paste0(prefix, "1.csv")
  df <- readr::read_delim(f, delim = ";", show_col_types = FALSE)
  expect_true(all(is.na(df$TargetWell) | df$TargetWell == ""))
  unlink(c(f, paste0(prefix, "summary.tsv")))
})

test_that("write_integra_files writes to user-specified output_dir", {
  data <- tibble(
    SampleID = paste0("S", 1:5),
    PlateID = "P1",
    WellID = paste0("A", 1:5)
  )
  
  # Create a unique temporary directory name that does not exist yet
  temp_dir <- file.path(tempdir(), "test_integra_output")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  
  prefix <- "custom_run_"
  expect_no_error(write_integra_files(data, file_prefix = prefix, output_dir = temp_dir))
  
  # Check that directory was created and files written inside it
  expect_true(dir.exists(temp_dir))
  
  f1 <- file.path(temp_dir, paste0(prefix, "1.csv"))
  fsummary <- file.path(temp_dir, paste0(prefix, "summary.tsv"))
  
  expect_true(file.exists(f1))
  expect_true(file.exists(fsummary))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_integra_files splits correctly with max_source_plates = 2", {
  # 5 source plates, 2 samples each
  # Run 1: P1, P2 (4 samples)
  # Run 2: P3, P4 (4 samples)
  # Run 3: P5 (2 samples)
  data <- tibble(
    SampleID = paste0("S", 1:10),
    PlateID = rep(paste0("P", 1:5), each = 2),
    WellID = "A1"
  )

  prefix <- paste0(tempfile(), "_")
  expect_no_error(write_integra_files(data, target_plate_prefix = "TPLATE", file_prefix = prefix, max_source_plates = 2))

  summary_file <- paste0(prefix, "summary.tsv")
  expect_true(file.exists(summary_file))

  summary_df <- readr::read_tsv(summary_file, show_col_types = FALSE)
  expect_equal(nrow(summary_df), 3) # 3 runs/rounds due to max_source_plates = 2

  expect_equal(summary_df$TargetPlateID, rep("TPLATE_1", 3)) # Same target plate!
  expect_equal(summary_df$SampleCount, c(4, 4, 2))
  expect_equal(summary_df$SourcePlateIDs, c("P1, P2", "P3, P4", "P5"))

  # Read run files to verify target well assignments
  well_ID_order <- paste0(rep(LETTERS[1:8], 12), rep(1:12, each = 8))

  # Round 1
  f1 <- file.path(dirname(prefix), summary_df$FileName[1])
  df1 <- readr::read_delim(f1, delim = ";", show_col_types = FALSE)
  expect_equal(df1$TargetWell, well_ID_order[1:4])
  unlink(f1)

  # Round 2
  f2 <- file.path(dirname(prefix), summary_df$FileName[2])
  df2 <- readr::read_delim(f2, delim = ";", show_col_types = FALSE)
  expect_equal(df2$TargetWell, well_ID_order[5:8])
  unlink(f2)

  # Round 3
  f3 <- file.path(dirname(prefix), summary_df$FileName[3])
  df3 <- readr::read_delim(f3, delim = ";", show_col_types = FALSE)
  expect_equal(df3$TargetWell, well_ID_order[9:10])
  unlink(f3)

  unlink(summary_file)
})
