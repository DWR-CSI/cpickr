#' Subsample Plate Data for Cherrypicking
#'
#' This function subsamples a plate dataset to include only the specified SampleIDs,
#' which is useful for creating cherrypicking lists for Hamilton robots. The function
#' filters the input dataset to retain only the samples of interest while preserving
#' their plate and well locations.
#'
#' @param plate_data A data frame or tibble containing plate data with at least a SampleID 
#'   column. Typically also contains PlateID and WellID columns for use with other cpickr 
#'   functions.
#' @param sample_ids A character vector of SampleIDs to keep in the subsampled dataset.
#'   These represent the specific samples to be cherrypicked.
# Note: file_prefix parameter was removed from the function
#'
#' @return A data frame containing only the rows with SampleIDs present in the sample_ids list,
#'   maintaining all original columns and their values. The function provides a message indicating
#'   how many samples were found compared to the number requested.
#'
#' @export
#'
#' @examples
#' plate_data <- data.frame(
#'   SampleID = c("S1", "S2", "S3", "S4", "S5"),
#'   PlateID = c("P1", "P1", "P1", "P2", "P2"),
#'   Well = c("A1", "A2", "A3", "B1", "B2")
#' )
#' sample_ids_to_keep <- c("S1", "S3", "S5")
#' subsample_plate_data(plate_data, sample_ids_to_keep)
subsample_plate_data <- function(plate_data, sample_ids) {
  # Check if plate_data is a data frame
  if (!is.data.frame(plate_data)) {
    stop("plate_data must be a data frame")
  }

  # Check if SampleID column exists in plate_data
  if (!"SampleID" %in% names(plate_data)) {
    stop("plate_data must contain a column named 'SampleID'")
  }

  # Check if sample_ids is a character vector
  if (!is.character(sample_ids)) {
    stop("sample_ids must be a character vector")
  }

  # Subsample the data
  subsampled_data <- plate_data[plate_data$SampleID %in% sample_ids, ]

  # Check if any samples were found
  if (nrow(subsampled_data) == 0) {
    warning("No matching SampleIDs found in the plate data")
  } else {
    message(sprintf("Found %d matching samples out of %d provided SampleIDs",
                    nrow(subsampled_data), length(sample_ids)))
  }

  return(subsampled_data)
}
