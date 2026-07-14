library(testthat)
library(dplyr)

test_that("subsample_plate_data errors on invalid inputs", {
  # plate_data not a data frame
  expect_error(subsample_plate_data(123, c("S1")), "plate_data must be a data frame")

  # plate_data missing SampleID column
  expect_error(subsample_plate_data(data.frame(a = 1), c("S1")), "plate_data must contain a column named 'SampleID'")

  # sample_ids not a character vector
  expect_error(subsample_plate_data(data.frame(SampleID = "S1"), 123), "sample_ids must be a character vector")
})

test_that("subsample_plate_data filters matching samples correctly", {
  plate_data <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4"),
    PlateID = "Plate1",
    WellID = c("A1", "A2", "A3", "A4")
  )

  # Subsample matching some samples
  expect_message(
    res <- subsample_plate_data(plate_data, c("S2", "S4", "S5")),
    "Found 2 matching samples out of 3 provided SampleIDs"
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$SampleID, c("S2", "S4"))
})

test_that("subsample_plate_data warns when no samples match", {
  plate_data <- data.frame(
    SampleID = c("S1", "S2"),
    PlateID = "Plate1",
    WellID = c("A1", "A2")
  )

  expect_warning(
    res <- subsample_plate_data(plate_data, c("S3", "S4")),
    "No matching SampleIDs found in the plate data"
  )

  expect_equal(nrow(res), 0)
})
