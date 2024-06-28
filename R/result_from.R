#' Take a Hamilton input file and return the resulting destination plate.
#'
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @param data A tibble or input file with columns SampleID, PlateID, WellID, and DestWellID.

#'
#' @return Returns a tibble with columns SampleID, PlateID, and WellID.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   SampleID = c("C234323CVP", "C240026CVP", "C240036CVP", "C240199CVP"),
#'   PlateID = c("CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240218CDFW02"),
#'   WellID = c("A1", "A2", "A3","A1"),
#'   DestWellID = c("A1", "A2", "A3","A4")
#' )
#' result_plate <- result_from(data)
#' result_plate
#' }
result_from <- function(data) {
  # If data is a filepath, read it in with read_csv
  if (is.character(data)) {
    data <- readr::read_csv(data)
  }
  # Check if the data has the correct columns
  if (!all(c("SampleID", "PlateID", "WellID", "DestWellID") %in% names(data))) {
    stop("Input data must contain columns SampleID, PlateID, WellID, and DestWellID. Are you sure the file is in the correct format?")
  }
  result_plate <- data %>%
    dplyr::mutate(WellID = DestWellID) %>%
    dplyr::select(SampleID, PlateID, WellID)
  return(result_plate)
}
