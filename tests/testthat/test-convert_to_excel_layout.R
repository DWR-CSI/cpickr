library(testthat)
library(dplyr)
library(readxl)

# Improved helper function
create_sample_data <- function(n_wells, empty_wells = NULL, plate_id = "Plate1") {
  if (n_wells == 96) {
    rows <- LETTERS[1:8]
    cols <- 1:12
  } else {
    rows <- LETTERS[1:16]
    cols <- 1:24
  }

  wellids <- expand.grid(Row = rows, Col = cols) %>%
    mutate(WellID = paste0(Row, Col)) %>%
    pull(WellID)

  if (!is.null(empty_wells)) {
    wellids <- setdiff(wellids, empty_wells)
  }

  tibble(
    SampleID = paste0("Sample", seq_along(wellids)),
    PlateID = plate_id,
    WellID = wellids
  )
}

test_that("create_sample_data creates correct data frame", {
  plate_data <- create_sample_data(96)
  expect_s3_class(plate_data, "data.frame")
  expect_equal(nrow(plate_data), 96)
  expect_equal(ncol(plate_data), 3)
  expect_equal(names(plate_data), c("SampleID", "PlateID", "WellID"))
})

test_that("convert_to_excel_layout handles 96-well plate correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  plate_data <- create_sample_data(96)

  expect_no_error(convert_to_excel_layout(plate_data, temp_file, n_wells = 96))

  expect_true(file.exists(temp_file), "Excel file was not created")

  # Read the Excel file and check its contents
  excel_data <- readxl::read_excel(temp_file)
  expect_equal(nrow(excel_data), 8) # 8 rows
  expect_equal(ncol(excel_data), 13) # 12 columns + 1 row name column
  expect_equal(excel_data[[1]], c(LETTERS[1:8]))
  expect_equal(as.character(colnames(excel_data)), c("Row", as.character(1:12)))


  # Check content of cells
  expect_equal(excel_data[1, 2] %>% pull(), "Sample1") # A1
  expect_equal(excel_data[8, 13] %>% pull(), "Sample96") # H12

  unlink(temp_file)
})



test_that("convert_to_excel_layout handles 384-well plate correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  plate_data <- create_sample_data(384)

  expect_no_error(convert_to_excel_layout(plate_data, temp_file, n_wells = 384))

  # Read the Excel file and check its contents
  excel_data <- readxl::read_excel(temp_file)
  expect_equal(nrow(excel_data), 16) # 16 rows
  expect_equal(ncol(excel_data), 25) # 24 columns + 1 row name column
  expect_equal(excel_data[[1]], LETTERS[1:16])
  expect_equal(as.character(colnames(excel_data)), as.character(c("Row", 1:24)))

  # Check content of cells
  expect_equal(excel_data[1, 2] %>% pull(), "Sample1") # A1
  expect_equal(excel_data[16, 25] %>% pull(), "Sample384") # P24

  unlink(temp_file)
})

test_that("convert_to_excel_layout handles empty wells correctly", {
  temp_file <- tempfile(fileext = ".xlsx")
  empty_wells <- c("A1", "B2", "C3")
  plate_data <- create_sample_data(96, empty_wells)

  expect_no_error(convert_to_excel_layout(plate_data, temp_file, n_wells = 96))

  # Read the Excel file and check its contents
  excel_data <- readxl::read_excel(temp_file)
  expect_true(is.na(excel_data[1, 2] %>% pull())) # A1 should be empty
  expect_true(is.na(excel_data[2, 3] %>% pull())) # B2 should be empty
  expect_true(is.na(excel_data[3, 4] %>% pull())) # C3 should be empty
  expect_false(is.na(excel_data[1, 3] %>% pull())) # A2 should not be empty

  unlink(temp_file)
})

test_that("convert_to_excel_layout handles fewer wells than plate size", {
  temp_file <- tempfile(fileext = ".xlsx")
  plate_data <- create_sample_data(96) %>% slice(1:50)  # Only 50 wells filled

  expect_no_error(convert_to_excel_layout(plate_data, temp_file, n_wells = 96))

  # Read the Excel file and check its contents
  excel_data <- readxl::read_excel(temp_file)
  expect_equal(sum(!is.na(excel_data[1:8, 2:13])), 50)  # 50 non-NA cells
  expect_equal(sum(is.na(excel_data[1:8, 2:13])), 46)   # 46 NA cells

  unlink(temp_file)
})

test_that("convert_to_excel_layout errors on invalid input", {
  plate_data <- create_sample_data(96)

  # Test for invalid n_wells
  expect_error(convert_to_excel_layout(plate_data, "test.xlsx", n_wells = 100),
               "This function only supports 96-well or 384-well plates")

  # Test for missing required columns
  expect_error(convert_to_excel_layout(select(plate_data, -SampleID), "test.xlsx", n_wells = 96),
               "Input data must have columns: SampleID, PlateID, and WellID")

  # Test for multiple PlateIDs
  multi_plate_data <- bind_rows(
    plate_data,
    mutate(plate_data, PlateID = "Plate2")
  )
  expect_error(convert_to_excel_layout(multi_plate_data, "test.xlsx", n_wells = 96),
               "More than one PlateID found in the data")

  # Test for invalid WellIDs
  invalid_data <- plate_data %>%
    add_row(SampleID = "InvalidSample", PlateID = "Plate1", WellID = "Z1")
  expect_error(convert_to_excel_layout(invalid_data, "test.xlsx", n_wells = 96),
               "Invalid WellIDs found for the specified plate size: Z1")
})
