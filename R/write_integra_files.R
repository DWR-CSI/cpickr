#' Write a tibble to multiple semi-colon delimited files for Integra Assist Plus robot input
#'
#' This function splits a tibble into multiple semi-colon delimited files based on specified criteria,
#' formatting the output for use with Integra Assist Plus robots in cherrypicking operations.
#' The function creates formatted files with required columns (SampleID, SourcePlateID, SourceWell,
#' TargetPlateID, TargetWell, TransferVolume) and also generates a summary file (TSV) mapping the rounds
#' on the robot to their source and destination plates.
#'
#' @importFrom dplyr mutate bind_rows select
#' @importFrom magrittr %>%
#' @importFrom readr write_delim write_tsv
#' @importFrom tibble is_tibble tibble as_tibble
#' @param data A tibble or data.frame to be split and written. Must contain columns
#'   'SampleID', 'PlateID', and 'WellID'.
#' @param target_plate_prefix Prefix for the destination plate IDs. Default is "TARGET".
#' @param file_prefix Prefix for the output filenames. Default is "integra_input_".
#' @param output_dir Directory where the output files should be written. Default is "." (current working directory).
#' @param max_source_plates Maximum number of unique source plates allowed per run file. Default is 3.
#' @param transfer_volume Default volume (in microliters) to transfer if a 'TransferVolume'
#'   column is not present in 'data'. Default is 25.
#' @param auto_dest_well If TRUE, the target well will be automatically filled in
#'   sequentially by column (A1, B1, C1, etc.). Default is TRUE. If FALSE, an empty
#'   target well column will be created.
#'
#' @return Nothing is returned, but files are written to the target directory:
#' \itemize{
#'   \item Individual input files with format \code{[file_prefix][index].csv} (semi-colon delimited)
#'   \item A summary file \code{[file_prefix]summary.tsv} (tab-separated values) listing the rounds
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   SampleID = c("S1", "S2", "S3", "S4"),
#'   PlateID = c("PlateA", "PlateA", "PlateB", "PlateC"),
#'   WellID = c("A1", "A2", "B1", "C5")
#' )
#' write_integra_files(data)
#' }
write_integra_files <- function(data,
                                target_plate_prefix = "TARGET",
                                file_prefix = "integra_input_",
                                output_dir = ".",
                                max_source_plates = 3,
                                transfer_volume = 25,
                                auto_dest_well = TRUE) {
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a tibble or a data frame.")
  }
  
  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }
  
  if (nrow(data) == 0) {
    stop("Input 'data' must contain at least one row.")
  }
  
  if (!is.character(target_plate_prefix) || length(target_plate_prefix) != 1 || is.na(target_plate_prefix)) {
    stop("Input 'target_plate_prefix' must be a single non-NA character string.")
  }
  
  if (!is.character(file_prefix) || length(file_prefix) != 1 || is.na(file_prefix)) {
    stop("Input 'file_prefix' must be a single non-NA character string.")
  }
  
  if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
    stop("Input 'output_dir' must be a single non-NA character string.")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (!is.numeric(transfer_volume) || length(transfer_volume) != 1 || is.na(transfer_volume) || transfer_volume <= 0) {
    stop("Input 'transfer_volume' must be a positive number.")
  }
  
  if (!is.logical(auto_dest_well) || length(auto_dest_well) != 1 || is.na(auto_dest_well)) {
    stop("Input 'auto_dest_well' must be a single logical value.")
  }
  
  if (!is.numeric(max_source_plates) || length(max_source_plates) != 1 || is.na(max_source_plates) || 
      max_source_plates < 1 || max_source_plates > 3 || max_source_plates != as.integer(max_source_plates)) {
    stop("Input 'max_source_plates' must be an integer between 1 and 3.")
  }
  
  if (!"PlateID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'PlateID'.")
  }
  if (!"SampleID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'SampleID'.")
  }
  if (!"WellID" %in% names(data)) {
    stop("Input 'data' must contain a column named 'WellID'.")
  }

  well_ID_order <- paste0(rep(LETTERS[1:8], 12), rep(1:12, each = 8))

  dest_plate_index <- 1
  dest_wells_used <- 0
  
  runs <- list()
  current_run_rows <- list()
  current_run_source_plates <- character()
  
  has_vol_col <- "TransferVolume" %in% names(data)

  for (i in 1:nrow(data)) {
    row <- data[i, ]
    sp_id <- as.character(row$PlateID)
    
    dest_full <- (dest_wells_used >= 96)
    source_limit_exceeded <- FALSE
    if (!(sp_id %in% current_run_source_plates) && length(current_run_source_plates) == max_source_plates) {
      source_limit_exceeded <- TRUE
    }
    
    if ((dest_full || source_limit_exceeded) && length(current_run_rows) > 0) {
      runs[[length(runs) + 1]] <- list(
        rows = dplyr::bind_rows(current_run_rows),
        target_plate_id = paste0(target_plate_prefix, "_", dest_plate_index),
        source_plates = current_run_source_plates
      )
      
      current_run_rows <- list()
      current_run_source_plates <- character()
      
      if (dest_full) {
        dest_plate_index <- dest_plate_index + 1
        dest_wells_used <- 0
      }
    }
    
    dest_wells_used <- dest_wells_used + 1
    target_well <- if (auto_dest_well) well_ID_order[dest_wells_used] else ""
    vol <- if (has_vol_col) row$TransferVolume else transfer_volume
    
    new_row <- tibble::tibble(
      SampleID = as.character(row$SampleID),
      SourcePlateID = sp_id,
      SourceWell = as.character(row$WellID),
      TargetPlateID = paste0(target_plate_prefix, "_", dest_plate_index),
      TargetWell = target_well,
      TransferVolume = vol
    )
    
    current_run_rows[[length(current_run_rows) + 1]] <- new_row
    current_run_source_plates <- unique(c(current_run_source_plates, sp_id))
  }
  
  if (length(current_run_rows) > 0) {
    runs[[length(runs) + 1]] <- list(
      rows = dplyr::bind_rows(current_run_rows),
      target_plate_id = paste0(target_plate_prefix, "_", dest_plate_index),
      source_plates = current_run_source_plates
    )
  }
  
  summary_rows <- list()
  
  if (output_dir == ".") {
    full_prefix <- file_prefix
  } else {
    full_prefix <- file.path(output_dir, file_prefix)
  }
  
  for (idx in seq_along(runs)) {
    run <- runs[[idx]]
    run_file_name <- paste0(full_prefix, idx, ".txt")
    
    out_df <- run$rows %>%
      dplyr::select(SampleID, SourcePlateID, SourceWell, TargetPlateID, TargetWell, TransferVolume)
    
    readr::write_delim(out_df, run_file_name, delim = ";")
    
    summary_rows[[idx]] <- tibble::tibble(
      Round = idx,
      FileName = basename(run_file_name),
      TargetPlateID = run$target_plate_id,
      SourcePlateIDs = paste(run$source_plates, collapse = ", "),
      SampleCount = nrow(out_df)
    )
  }
  
  summary_df <- dplyr::bind_rows(summary_rows)
  summary_file_name <- paste0(full_prefix, "summary.tsv")
  readr::write_tsv(summary_df, summary_file_name)
  
  invisible(NULL)
}
