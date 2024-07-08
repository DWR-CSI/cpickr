#' Convert plate data to Excel spreadsheet
#'
#' This function takes a tibble with plate data and converts it to an Excel spreadsheet
#' with a 96-well or 384-well plate layout, including row and column labels.
#'
#' @param plate_data A tibble with columns SampleID, PlateID, and WellID
#' @param output_file Path to the output Excel file where the data will be written using `write_xlsx()`. The filename should contain the .xlsx file extension.
#' @param n_wells Number of wells in the plate (96 or 384)
#'
#' @details This function writes the provided data to the specified Excel-formatted file. If successful, it returns TRUE invisibly.
#'
#' @return Invisibly returns TRUE if successful.
#'
#' @section Side effects: This function writes data to a file on the file system.
#' @export
#'
#' @importFrom dplyr mutate select arrange n_distinct bind_rows filter
#' @importFrom tidyr pivot_wider
#' @importFrom writexl write_xlsx
#'
#' @examples
#' \dontrun{
#' plate_data <- import_excel_plate("input.xlsx", sheet = 1, start_cell = "A1")
#' convert_to_excel_layout(plate_data, "output.xlsx", n_wells = 96)
#' }
convert_to_excel_layout <- function(plate_data, output_file, n_wells) {
  if (!n_wells %in% c(96, 384)) {
    stop("This function only supports 96-well or 384-well plates")
  }

  # Check if the input data has the required columns
  required_cols <- c("SampleID", "PlateID", "WellID")
  if (!all(required_cols %in% colnames(plate_data))) {
    stop("Input data must have columns: SampleID, PlateID, and WellID")
  }

  # Check if more than one PlateID is present
  if (dplyr::n_distinct(plate_data$PlateID) > 1) {
    stop("More than one PlateID found in the data. This function only supports one plate at a time.")
  }

  # Set plate dimensions based on n_wells
  if (n_wells == 96) {
    rows <- LETTERS[1:8]
    cols <- 1:12
  } else {  # 384-well plate
    rows <- LETTERS[1:16]
    cols <- 1:24
  }

  # Generate all possible WellIDs for the specified plate size
  valid_wellids <- expand.grid(Row = rows, Col = cols) %>%
    dplyr::mutate(WellID = paste0(Row, Col)) %>%
    dplyr::pull(WellID)

  # Check if all WellIDs in the data are valid
  invalid_wells <- plate_data %>%
    dplyr::filter(!WellID %in% valid_wellids) %>%
    dplyr::pull(WellID)

  if (length(invalid_wells) > 0) {
    stop(paste("Invalid WellIDs found for the specified plate size:",
               paste(invalid_wells, collapse = ", ")))
  }

  # Extract row and column from WellID
  plate_data <- plate_data %>%
    dplyr::mutate(
      Row = substr(WellID, 1, 1),
      Col = as.integer(substr(WellID, 2, nchar(WellID)))
    )

  # Pivot the data to wide format
  plate_layout <- plate_data %>%
    dplyr::select(Row, Col, SampleID) %>%
    tidyr::pivot_wider(names_from = Col, values_from = SampleID)

  # Ensure all columns are present and in the correct order
  all_cols <- as.character(cols)
  missing_cols <- setdiff(all_cols, colnames(plate_layout))

  for (col in missing_cols) {
    plate_layout[[col]] <- NA_character_
  }

  plate_layout <- plate_layout %>%
    dplyr::select(Row, all_of(all_cols))

  # Ensure all rows are present and in the correct order
  missing_rows <- setdiff(rows, plate_layout$Row)

  if (length(missing_rows) > 0) {
    missing_data <- data.frame(Row = missing_rows)
    for (col in all_cols) {
      missing_data[[col]] <- NA_character_
    }
    plate_layout <- dplyr::bind_rows(plate_layout, missing_data)
  }

  plate_layout <- plate_layout %>%
    dplyr::arrange(factor(Row, levels = rows))

  # Replace NA with character(NA)
  plate_layout[is.na(plate_layout)] <- NA_character_


  writexl::write_xlsx(plate_layout, output_file)

  # In convert_to_excel_layout function, after writing to Excel:
  if (!file.exists(output_file)) {
    stop("Failed to create Excel file: ", output_file)
  } else {
    invisible(TRUE)
  }
}
