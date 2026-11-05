#' Split trajectory by time gap
#'
#' @description Splits individual tracking trajectories into multiple segments if the time difference between consecutive locations exceeds a specified threshold.
#'
#' @param data A data.frame containing the tracking data.
#' @param ids Character. The name of the column containing individual identifiers.
#' @param time.col Character. The name of the column containing timestamps (must be POSIXct or Date).
#' @param gap.days Numeric or named list. The maximum allowed gap in days before splitting the track. Can be a single numeric value for all individuals, or a named list specifying different thresholds per individual.
#'
#' @return A data.frame identical to the input but with an additional column `id_split` representing the new segmented identifiers.
#' @examples
#' # Split tracks wherever two consecutive fixes are more than 3 hours apart
#' split_trk <- divide.track.by.gap(sim_tracks, ids = "ID",
#'                                  time.col = "timestamp", gap.days = 3 / 24)
#' table(split_trk$id_split)
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup select lag
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
divide.track.by.gap <- function(data,
                               ids, 
                               time.col,
                               gap.days) {
  
  # Initial checks
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (!ids %in% names(data)) {
    stop(paste0("Column '", ids, "' not found in data"))
  }
  if (!time.col %in% names(data)) {
    stop(paste0("Column '", time.col, "' not found in data"))
  }
  if (!inherits(data[[time.col]], "POSIXct") && !inherits(data[[time.col]], "Date")) {
    stop(paste0("Column '", time.col, "' must be of class POSIXct or Date"))
  }
  if (is.list(gap.days)) {
    missing_ids <- setdiff(as.character(unique(data[[ids]])), names(gap.days))
    if (length(missing_ids) > 0) {
      stop("Missing gap.days values for IDs: ", paste(missing_ids, collapse = ", "))
    }
  } else if (!is.numeric(gap.days) || length(gap.days) != 1 || gap.days <= 0) {
    stop("'gap.days' must be a positive numeric value or a named list of positive numerics")
  }
  
  # Order by individual and time
  data <- data[order(data[[ids]], data[[time.col]]), ]
  
  # Calculate difference of time in days and split based on that
  data <- data %>%
    dplyr::group_by(.data[[ids]]) %>%
    dplyr::mutate(
      time_diff = as.numeric(difftime(.data[[time.col]], dplyr::lag(.data[[time.col]]), units = "days")),
      gap_val = if (is.list(gap.days)) gap.days[[as.character(unique(.data[[ids]]))]] else gap.days,
      sequence = cumsum(tidyr::replace_na(time_diff > gap_val, FALSE)) + 1,
      id_split = paste0(.data[[ids]], "_", sequence)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-time_diff, -sequence, -gap_val)
  
  return(data)
}