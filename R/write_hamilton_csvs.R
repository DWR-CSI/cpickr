#' Write a tibble to multiple CSV files for Hamilton robot input
#'
#' This function splits a tibble into multiple CSV files based on specified criteria,
#' formatting the output for use with Hamilton robots.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @param data A tibble to be split and written to CSV files.
#' @param rows_per_file Maximum number of rows per file. Default is 93.
#' @param max_plate_ids Maximum number of unique plate IDs per file. Default is 20.
#' @param file_prefix Prefix for the output CSV filenames. Default is "plate_input_".
#' @param auto_dest_well If TRUE, the destination well will be automatically filled in sequentially, i.e. A1,B1,C1, etc. Default is TRUE.
#'
#' @return Nothing is returned, but CSV files are written to the current working directory.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   SampleID = c("C234323CVP", "C240026CVP", "C240036CVP", "C240199CVP"),
#'   PlateID = c("CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240204CDFW01", "CVP20240218CDFW02"),
#'   Well = c("A1", "A2", "A3","A1")
#' )
#' write_hamilton_csvs(data)
#' }
write_hamilton_csvs <- function(data, rows_per_file = 93, max_plate_ids = 20, file_prefix = "plate_input_", auto_dest_well = TRUE) {
  if (!tibble::is_tibble(data)) {
    stop("Input 'data' must be a tibble.")
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
          dplyr::mutate(DestWellID = well_ID_order[1:nrow(current_chunk)])
      } else {
        current_chunk <- current_chunk %>%
          dplyr::mutate(DestWellID = "")
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
      dplyr::mutate(PlateID = plate_id_map[.data$PlateID])

    if (auto_dest_well) {
      current_chunk <- current_chunk %>%
        dplyr::mutate(DestWellID = well_ID_order[1:nrow(current_chunk)])
    } else {
      current_chunk <- current_chunk %>%
        dplyr::mutate(DestWellID = "")
    }

    write_chunk(current_chunk, file_index)
  }

  # Write the key file
  key_df <- dplyr::bind_rows(keys, .id = "File_Index")
  key_file_name <- paste0(file_prefix, "_key.csv")
  readr::write_csv(key_df, key_file_name)
}
