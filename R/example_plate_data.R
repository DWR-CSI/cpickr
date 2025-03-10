#' Example Plate Data for Laboratory Sample Tracking
#'
#' A dataset containing unprocessed plate data for multiple 96-well source plates
#' to demonstrate cpickr functionality. Empty wells are marked with a SampleID of "EMPTY".
#' This dataset can be used for testing functions or understanding the expected
#' data format for the package.
#'
#' @format A tibble with 384 rows and 3 variables:
#' \describe{
#'   \item{SampleID}{Unique alphanumeric identifier for each sample (format: "CXXXXXXCVP")}
#'   \item{PlateID}{Identifier for the source plate (format: "CVPXXXXXXXXCDWFXX")}
#'   \item{WellID}{Well position on the plate in standard format (e.g., "A1", "H12")}
#' }
#' @source {Generated by package authors based on typical laboratory plate layouts
#' used with Hamilton Nimbus robots for cherrypicking operations}
"example_plate_data"
