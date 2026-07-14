library(testthat)
library(dplyr)
library(readr)
library(tibble)

test_that("result_from errors on missing columns", {
  # Missing all required columns
  expect_error(result_from(tibble(a = 1)), "Input data must contain columns SampleID, PlateID, SourceWellID, and DestWellID.")
  
  # Partially missing columns
  expect_error(result_from(tibble(SampleID = "S1", PlateID = "P1")), "Input data must contain columns SampleID, PlateID, SourceWellID, and DestWellID.")
})

test_that("result_from processes tibble input correctly", {
  input_data <- tibble(
    SampleID = c("S1", "S2"),
    PlateID = c("P1", "P1"),
    SourceWellID = c("A1", "A2"),
    DestWellID = c("B1", "B2")
  )

  # No platename provided (default to "NewPlate" with message)
  expect_message(
    res1 <- result_from(input_data),
    "No or invalid plate name provided. Default plate name used: NewPlate."
  )

  expect_equal(res1$SampleID, c("S1", "S2"))
  expect_equal(res1$PlateID, c("NewPlate", "NewPlate"))
  expect_equal(res1$WellID, c("B1", "B2"))
  expect_equal(names(res1), c("SampleID", "PlateID", "WellID"))

  # Custom platename provided
  expect_no_message(
    res2 <- result_from(input_data, platename = "MyPlate")
  )
  expect_equal(res2$PlateID, c("MyPlate", "MyPlate"))
})

test_that("result_from processes file input correctly", {
  input_data <- tibble(
    SampleID = c("S1", "S2"),
    PlateID = c("P1", "P1"),
    SourceWellID = c("A1", "A2"),
    DestWellID = c("B1", "B2")
  )

  temp_file <- tempfile(fileext = ".csv")
  readr::write_csv(input_data, temp_file)

  # No platename provided (default to file basename with message)
  expect_message(
    res1 <- result_from(temp_file),
    paste0("No plate name provided. Default plate name \\(filename\\) used: ", basename(temp_file))
  )

  expect_equal(res1$SampleID, c("S1", "S2"))
  expect_equal(res1$PlateID, rep(basename(temp_file), 2))
  expect_equal(res1$WellID, c("B1", "B2"))

  # Custom platename provided for file input
  expect_no_message(
    res2 <- result_from(temp_file, platename = "FilePlate")
  )
  expect_equal(res2$PlateID, c("FilePlate", "FilePlate"))

  unlink(temp_file)
})
