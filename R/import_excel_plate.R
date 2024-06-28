#' Import 96-well plate data from Excel
#'
#' This function imports data from a 96-well plate (8 rows, 12 columns)
#' embedded within an Excel spreadsheet.
#'
#' @param file Path to the Excel file
#' @param sheet Sheet name or number containing the plate data
#' @param start_cell Top-left cell of the plate data (e.g., "B2"). Do not include headers.
#' @param plate_id Optional identifier for the plate, usually the plate name or label. If not provided, the filename will be used.
#'
#' @return A tibble with columns for Sample ID, Plate ID, and Well ID.
#' @export
#'
#' @importFrom readxl read_excel cell_rows cell_cols
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom tibble rownames_to_column
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

  # Add column and row names
  colnames(data) <- 1:12
  rownames(data) <- LETTERS[1:8]

  # Reshape the data
  long_data <- data %>%
    tibble::rownames_to_column("Row") %>%
    tidyr::pivot_longer(cols = -Row, names_to = "Column", values_to = "SampleID") %>%
    dplyr::mutate(
      SampleID = as.character(SampleID),
      PlateID = plate_id,
      WellID = paste0(Row, Column)
      ) %>%
    dplyr::select(SampleID, PlateID, WellID)

  return(long_data)
}
