#' Write a tibble to multiple CSV files for Hamilton robot input
#'
#' This function splits a tibble into multiple CSV files based on specified criteria,
#' formatting the output for use with Hamilton robots in cherrypicking operations. 
#' The function creates formatted text files with required columns (SampleID, PlateID, 
#' SourceWellID, DestWellID) and also generates a key file that maps original plate IDs 
#' to the simplified format required by Hamilton robots.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr write_csv
#' @importFrom stats setNames
#' @importFrom tibble is_tibble tibble
#' @param data A tibble to be split and written to CSV files. Must contain columns 
#'   'SampleID', 'PlateID', and 'WellID'.
#' @param rows_per_file Maximum number of rows per file. Default is 93. Cannot exceed 96 
#'   (the number of wells in a standard 96-well plate).
#' @param max_plate_ids Maximum number of unique plate IDs per file. Default is 20, but 
#'   the hard limit is 24 (maximum supported by Hamilton robots).
#' @param file_prefix Prefix for the output CSV filenames. Default is "plate_input_".
#' @param auto_dest_well If TRUE, the destination well will be automatically filled in 
#'   sequentially by column (A1,B1,C1, etc.). Default is TRUE. If FALSE, an empty 
#'   destination well column will be created for manual entry.
#'
#' @return Nothing is returned, but CSV files are written to the current working directory:
#' \itemize{
#'   \item Individual input files with format \code{[file_prefix][index].txt}
#'   \item A key file \code{[file_prefix]key.csv} that maps simplified plate IDs to original IDs
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   SampleID = c("C234323CVP", "C240026CVP", "C240036CVP", "C240199CVP"),
#'   PlateID = c("CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240218CDFW02"),
#'   WellID = c("A1", "A2", "A3","A1")
#' )
#' write_hamilton_csvs(data)
#' }
write_hamilton_csvs <- function(data, rows_per_file = 93, max_plate_ids = 20, file_prefix = "plate_input_", auto_dest_well = TRUE) {
  if (!tibble::is_tibble(data)) {
    stop("Input 'data' must be a tibble.")
  }
  if (!is.numeric(rows_per_file) || rows_per_file <= 0) {
    stop("Input 'rows_per_file' must be a positive integer.")
  }
  if (max_plate_ids > 24) {
    stop("Input 'max_plate_ids' must be less than or equal to 24.")
  }
  if (!is.character(file_prefix)) {
    stop("Input 'file_prefix' must be a character string.")
  }
  if (!is.logical(auto_dest_well)) {
    stop("Input 'auto_dest_well' must be a logical value.")
  }
  if (!"PlateID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'PlateID'.")
  }
  if(!"SampleID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'SampleID'.")
  }
  if(!"WellID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'WellID'.")
  }
  if(rows_per_file > 96) {
    stop("Input 'rows_per_file' cannot be greater than 96.")
  }

  well_ID_order <- paste0(rep(LETTERS[1:8], 12), rep(1:12, each = 8))

  # Initialize variables
  current_chunk <- tibble::tibble()
  current_plate_ids <- character()
  file_index <- 1
  keys <- list()

  # Function to write the current chunk to a CSV file
  write_chunk <- function(chunk, index) {
    file_name <- paste0(file_prefix, index, ".txt")
    readr::write_csv(chunk, file_name)
  }

  # Process data
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    new_plate_id <- row$PlateID

    if (length(unique(c(current_plate_ids, new_plate_id))) > max_plate_ids || nrow(current_chunk) >= rows_per_file) {
      # Process and write current chunk
      unique_plate_ids <- unique(current_plate_ids)
      new_plate_ids <- paste0("Plate", seq_along(unique_plate_ids))
      plate_id_map <- stats::setNames(new_plate_ids, unique_plate_ids)

      keys[[file_index]] <- tibble::tibble(
        File_Name = paste0(file_prefix, file_index, ".txt"),
        New_PlateID = new_plate_ids,
        Original_PlateID = unique_plate_ids
      )

      current_chunk <- current_chunk %>%
        dplyr::mutate(PlateID = plate_id_map[.data$PlateID])

      if (auto_dest_well) {
        current_chunk <- current_chunk %>%
          dplyr::mutate(
            DestWellID = well_ID_order[1:nrow(current_chunk)],
            SourceWellID = .data$WellID
            ) %>%
          select(SampleID, PlateID, SourceWellID, DestWellID)
      } else {
        current_chunk <- current_chunk %>%
          dplyr::mutate(
            DestWellID = "",
            SourceWellID = .data$WellID
            ) %>%
          select(SampleID, PlateID, SourceWellID, DestWellID)
      }

      write_chunk(current_chunk, file_index)

      # Reset for next chunk
      file_index <- file_index + 1
      current_chunk <- tibble::tibble()
      current_plate_ids <- character()
    }

    # Add the new row to the current chunk
    current_chunk <- dplyr::bind_rows(current_chunk, row)
    current_plate_ids <- unique(c(current_plate_ids, new_plate_id))
  }

  # Process last chunk if any data remains
  if (nrow(current_chunk) > 0) {
    unique_plate_ids <- unique(current_plate_ids)
    new_plate_ids <- paste0("Plate", seq_along(unique_plate_ids))
    plate_id_map <- stats::setNames(new_plate_ids, unique_plate_ids)

    keys[[file_index]] <- tibble::tibble(
      File_Name = paste0(file_prefix, file_index, ".txt"),
      New_PlateID = new_plate_ids,
      Original_PlateID = unique_plate_ids
    )

    current_chunk <- current_chunk %>%
      dplyr::mutate(
        SourceWellID = .data$WellID,
        PlateID = plate_id_map[.data$PlateID]
        )

    if (auto_dest_well) {
      current_chunk <- current_chunk %>%
        dplyr::mutate(DestWellID = well_ID_order[1:nrow(current_chunk)])
    } else {
      current_chunk <- current_chunk %>%
        dplyr::mutate(DestWellID = "")
    }
    current_chunk <- current_chunk %>%
      select(SampleID, PlateID, SourceWellID, DestWellID)
    write_chunk(current_chunk, file_index)
  }

  # Write the key file
  key_df <- dplyr::bind_rows(keys, .id = "File_Index")
  key_file_name <- paste0(file_prefix, "key.csv")
  readr::write_csv(key_df, key_file_name)
}
