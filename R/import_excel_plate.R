#' Import 96-well plate data from Excel in rectangular format
#'
#' This function imports data from a 96-well plate (8 rows by 12 columns)
#' embedded within an Excel spreadsheet. The function expects the data to be in 
#' a rectangular format that visually represents the plate layout, with rows A-H
#' and columns 1-12, where each cell contains a sample identifier.
#'
#' @param file Path to the Excel file containing the plate data
#' @param sheet Sheet name or number containing the plate data
#' @param start_cell Top-left cell of the plate data (e.g., "B2"). This should be the cell
#'   containing the value for well A1. Do not include headers or row/column labels.
#' @param plate_id Optional identifier for the plate, usually the plate name or label. 
#'   If not provided, the filename (without extension) will be used as the plate ID.
#'
#' @return A tibble with three columns:
#'   \itemize{
#'     \item SampleID: The sample identifier from each well
#'     \item PlateID: The plate identifier (from plate_id parameter or filename)
#'     \item WellID: The well location in format "A1", "B2", etc.
#'   }
#' @export
#'
#' @importFrom readxl read_excel cell_limits
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom cellranger as.cell_limits
#'
#' @examples
#' \dontrun{
#' plate_data <- import_plate_from_excel("path/to/file.xlsx", sheet = 1, start_cell = "B2")
#' }
import_excel_plate <- function(file, sheet, start_cell, plate_id = NULL) {
  # Checks
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  if (is.null(plate_id)) {
    plate_id <- tools::file_path_sans_ext(basename(file))
  } else if (!is.character(plate_id)) {
    stop("plate_id must be a character string")
  }

  # Convert start_cell to numeric
  cell_limits <- cellranger::as.cell_limits(start_cell)
  start_col <- cell_limits$ul[2]
  start_row <- cell_limits$ul[1]
  end_col <- start_col + 11  # 12 columns total
  end_row <- start_row + 7   # 8 rows total

  # Read the data
  data <- readxl::read_excel(file, sheet = sheet,
                             range = readxl::cell_limits(
                               c(start_row, start_col),
                               c(end_row, end_col)
                             ),
                             col_names = FALSE)

  # Check dimensions
  if (nrow(data) != 8 || ncol(data) != 12) {
    stop("Data does not appear to be from a 96-well plate (8 rows, 12 columns)")
  }

  # Create a tibble with Row and Column information
  long_data <- tibble::tibble(
    Row = rep(LETTERS[1:8], each = 12),
    Column = rep(1:12, times = 8),
    SampleID = as.vector(t(data))
  )

  # Add PlateID and WellID
  long_data <- long_data %>%
    dplyr::mutate(
      SampleID = as.character(SampleID),
      PlateID = plate_id,
      WellID = paste0(Row, Column)
    ) %>%
    dplyr::select(SampleID, PlateID, WellID)

  return(long_data)
}
