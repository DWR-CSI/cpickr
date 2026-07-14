library(testthat)
library(dplyr)
library(readr)
library(tibble)

test_that("write_hamilton_csvs errors on invalid inputs", {
  # Input data is not a tibble
  expect_error(write_hamilton_csvs(data.frame(a = 1)), "Input 'data' must be a tibble.")

  # Missing columns
  expect_error(write_hamilton_csvs(tibble(a = 1)), "Input 'data' must contain a column named 'PlateID'.")
  expect_error(write_hamilton_csvs(tibble(PlateID = 1)), "Input 'data' must contain a column named 'SampleID'.")
  expect_error(write_hamilton_csvs(tibble(PlateID = 1, SampleID = 1)), "Input 'data' must contain a column named 'WellID'.")

  valid_data <- tibble(SampleID = "S1", PlateID = "P1", WellID = "A1")

  # Invalid rows_per_file
  expect_error(write_hamilton_csvs(valid_data, rows_per_file = -1), "Input 'rows_per_file' must be a positive integer.")
  expect_error(write_hamilton_csvs(valid_data, rows_per_file = 97), "Input 'rows_per_file' cannot be greater than 96.")

  # Invalid max_plate_ids
  expect_error(write_hamilton_csvs(valid_data, max_plate_ids = 25), "Input 'max_plate_ids' must be less than or equal to 24.")

  # Invalid file_prefix
  expect_error(write_hamilton_csvs(valid_data, file_prefix = 123), "Input 'file_prefix' must be a character string.")

  # Invalid auto_dest_well
  expect_error(write_hamilton_csvs(valid_data, auto_dest_well = "yes"), "Input 'auto_dest_well' must be a logical value.")
})

test_that("write_hamilton_csvs correctly splits data by rows_per_file", {
  # Create a dataset with 10 rows
  data <- tibble(
    SampleID = paste0("S", 1:10),
    PlateID = "PlateA",
    WellID = paste0("A", 1:10)
  )

  prefix <- paste0(tempfile(), "_")
  expect_no_error(write_hamilton_csvs(data, rows_per_file = 3, file_prefix = prefix))

  # 10 rows with rows_per_file = 3 should produce 4 files:
  # File 1: 3 rows
  # File 2: 3 rows
  # File 3: 3 rows
  # File 4: 1 row
  # Plus the key file.

  files <- paste0(prefix, 1:4, ".txt")
  key_file <- paste0(prefix, "key.csv")

  for (f in files) {
    expect_true(file.exists(f))
  }
  expect_true(file.exists(key_file))

  # Read first file and verify content
  df1 <- readr::read_csv(files[1], show_col_types = FALSE)
  expect_equal(nrow(df1), 3)
  expect_equal(names(df1), c("SampleID", "PlateID", "SourceWellID", "DestWellID"))
  expect_equal(df1$SampleID, c("S1", "S2", "S3"))
  expect_equal(df1$PlateID, rep("Plate1", 3)) # mapped plate ID
  expect_equal(df1$SourceWellID, c("A1", "A2", "A3"))
  expect_equal(df1$DestWellID, c("A1", "B1", "C1")) # sequential destination wells

  # Read fourth file and verify content
  df4 <- readr::read_csv(files[4], show_col_types = FALSE)
  expect_equal(nrow(df4), 1)
  expect_equal(df4$SampleID, "S10")
  expect_equal(df4$DestWellID, "A1")

  # Read key file and verify mapping
  key_df <- readr::read_csv(key_file, show_col_types = FALSE)
  expect_equal(nrow(key_df), 4) # 4 mappings (1 for each file)
  expect_equal(key_df$Original_PlateID, rep("PlateA", 4))
  expect_equal(key_df$New_PlateID, rep("Plate1", 4))

  # Cleanup
  unlink(c(files, key_file))
})

test_that("write_hamilton_csvs correctly splits data by max_plate_ids", {
  # Create a dataset with 5 samples, each on a different plate
  data <- tibble(
    SampleID = paste0("S", 1:5),
    PlateID = paste0("Plate_", 1:5),
    WellID = "A1"
  )

  prefix <- paste0(tempfile(), "_")
  # Set max_plate_ids to 2
  expect_no_error(write_hamilton_csvs(data, max_plate_ids = 2, file_prefix = prefix))

  # Max 2 plates per file.
  # Row 1: Plate_1 (1 plate)
  # Row 2: Plate_2 (2 plates)
  # Row 3: Plate_3 (starts new file because 3 plates > 2)
  # Row 4: Plate_4 (2 plates in file 2)
  # Row 5: Plate_5 (starts new file because 3 plates > 2 in file 2)
  # Should produce 3 files:
  # File 1: S1, S2 (Plate_1, Plate_2)
  # File 2: S3, S4 (Plate_3, Plate_4)
  # File 3: S5 (Plate_5)

  files <- paste0(prefix, 1:3, ".txt")
  key_file <- paste0(prefix, "key.csv")

  for (f in files) {
    expect_true(file.exists(f))
  }
  expect_true(file.exists(key_file))

  # Verify file 1
  df1 <- readr::read_csv(files[1], show_col_types = FALSE)
  expect_equal(nrow(df1), 2)
  expect_equal(df1$PlateID, c("Plate1", "Plate2")) # Mapped

  # Verify key file
  key_df <- readr::read_csv(key_file, show_col_types = FALSE)
  expect_equal(nrow(key_df), 5) # 5 total plates mapped across files
  expect_equal(key_df$Original_PlateID, paste0("Plate_", 1:5))
  expect_equal(key_df$New_PlateID, c("Plate1", "Plate2", "Plate1", "Plate2", "Plate1"))

  unlink(c(files, key_file))
})

test_that("write_hamilton_csvs respects auto_dest_well = FALSE", {
  data <- tibble(
    SampleID = paste0("S", 1:3),
    PlateID = "PlateA",
    WellID = c("A1", "A2", "A3")
  )

  prefix <- paste0(tempfile(), "_")
  expect_no_error(write_hamilton_csvs(data, auto_dest_well = FALSE, file_prefix = prefix))

  file <- paste0(prefix, "1.txt")
  key_file <- paste0(prefix, "key.csv")

  df1 <- readr::read_csv(file, show_col_types = FALSE)
  # DestWellID should be empty/NA
  expect_true(all(is.na(df1$DestWellID)))

  unlink(c(file, key_file))
})
