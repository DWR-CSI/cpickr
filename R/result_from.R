#' Take a Hamilton input file and return the resulting destination plate.
#'
#' This function takes a Hamilton robot input file (or equivalent tibble) and creates a 
#' representation of the destination plate after the robot has performed the cherrypicking 
#' operation. It is useful for tracking samples through the laboratory workflow and 
#' understanding the resulting plate layout.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @param data A tibble or file path to a Hamilton input file. Must contain columns SampleID, 
#'   PlateID, SourceWellID, and DestWellID.
#' @param platename A character string with the name of the destination plate being created. 
#'   If not provided and data is a file path, the filename will be used as the plate name.
#'   If not provided and data is a tibble, "NewPlate" will be used as the default name.
#'
#' @return Returns a tibble with columns SampleID, PlateID, and WellID.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   SampleID = c("C234323CVP", "C240026CVP", "C240036CVP", "C240199CVP"),
#'   PlateID = c("CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240218CDFW02"),
#'   SourceWellID = c("A1", "A2", "A3","A1"),
#'   DestWellID = c("A1", "A2", "A3","A4")
#' )
#' result_plate <- result_from(data)
#' result_plate
#' }
result_from <- function(data, platename = NULL) {
  is_file = NULL
  # If data is a filepath, read it in with read_csv
  if (is.character(data)) {
    plate_filename = basename(data)
    data <- readr::read_csv(data)
    is_file = TRUE
  } else {
    is_file = FALSE
  }
  # Check if the data has the correct columns
  if (!all(c("SampleID", "PlateID", "SourceWellID", "DestWellID") %in% names(data))) {
    stop("Input data must contain columns SampleID, PlateID, SourceWellID, and DestWellID. Are you sure the file is in the correct format?")
  }
  if (is.null(platename) & is_file == TRUE) {
    message(paste0("No plate name provided. Default plate name (filename) used: ", plate_filename))
    platename = plate_filename
  } else if (is.character(platename)) {
    # valid platename
    # do nothing
  } else {
    message("No or invalid plate name provided. Default plate name used: NewPlate.")
    platename = "NewPlate"
  }
  result_plate <- data %>%
    dplyr::mutate(WellID = DestWellID, PlateID = platename) %>%
    dplyr::select(SampleID, PlateID, WellID)
  return(result_plate)
}
