#' Summarise movement phases
#'
#' @description Builds a table with one row per movement phase of each individual, as
#'   classified by \code{\link{class.mov}}: its start, end, duration, number of locations,
#'   distance travelled, net displacement and, for movements, the ARPs where they start and end.
#'
#' @param data An object of class \code{disperseR} returned by \code{\link{class.mov}}, or a
#'   data.frame with the same columns.
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#' @param time.col Character. Name of the column containing timestamps (POSIXct). Default is "time".
#' @param phase.col Character. Name of the column containing the movement phases. Default is
#'   "phase".
#' @param coord.names Character vector of length 2 with the names of the X and Y coordinate
#'   columns (projected, in metres). Default is \code{c("x", "y")}.
#'
#' @details Consecutive locations with the same phase form one row. Consecutive ARP locations
#'   belonging to different linked clusters (\code{linked_clusters}) form separate rows.
#'   \code{distance_km} is the sum of the distances between consecutive locations of the phase,
#'   and \code{displacement_km} the straight-line distance between its first and last location.
#'   \code{from} and \code{to} give the ARP (\code{linked_clusters}) before and after each phase.
#'
#' @return A data.frame with columns \code{ID} (named as \code{id.col}), \code{n_phase},
#'   \code{phase}, \code{arp}, \code{from}, \code{to}, \code{start}, \code{end},
#'   \code{duration_days}, \code{n_locations}, \code{distance_km} and \code{displacement_km}.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' mov <- class.mov(link.arp(arp, thres = 24 * 3600))
#' ph <- phase.summary(mov)
#' head(ph)
#'
#' # Total distance travelled outside ARPs per individual
#' tapply(ph$distance_km[ph$phase != "ARP"], ph$ID[ph$phase != "ARP"], sum)
#' @export
#'
#' @importFrom dplyr %>% arrange group_by mutate summarise ungroup first last lag lead n
#'   consecutive_id if_else
phase.summary <- function(data,
                          id.col = "ID",
                          time.col = "time",
                          phase.col = "phase",
                          coord.names = c("x", "y")) {

  # Checks
  df <- .get_data(data)

  missing_cols <- setdiff(c(id.col, time.col, phase.col, coord.names), names(df))
  if (length(missing_cols) > 0) {
    stop("Columns not found in the data: ", paste(missing_cols, collapse = ", "),
         ". Run class.mov() first.")
  }
  if (!"linked_clusters" %in% names(df)) df$linked_clusters <- NA_character_

  df <- df %>%
    dplyr::mutate(id_x_field = .data[[id.col]],
                  t = .data[[time.col]],
                  ph = .data[[phase.col]],
                  xx = .data[[coord.names[1]]],
                  yy = .data[[coord.names[2]]],
                  arp_x = dplyr::if_else(ph %in% "ARP", as.character(linked_clusters),
                                         NA_character_)) %>%
    dplyr::arrange(id_x_field, t) %>%
    dplyr::group_by(id_x_field) %>%
    dplyr::mutate(
      block = dplyr::consecutive_id(ph, arp_x),
      step_m = sqrt((xx - dplyr::lag(xx))^2 + (yy - dplyr::lag(yy))^2),
      step_m = ifelse(block == dplyr::lag(block), step_m, 0)
    ) %>%
    dplyr::ungroup()

  # One row per phase / Una fila por fase
  out <- df %>%
    dplyr::group_by(id_x_field, block) %>%
    dplyr::summarise(
      phase = dplyr::first(ph),
      arp = dplyr::first(arp_x),
      start = dplyr::first(t),
      end = dplyr::last(t),
      duration_days = (as.numeric(dplyr::last(t)) - as.numeric(dplyr::first(t))) / 86400,
      n_locations = dplyr::n(),
      distance_km = sum(step_m, na.rm = TRUE) / 1000,
      displacement_km = sqrt((dplyr::last(xx) - dplyr::first(xx))^2 +
                               (dplyr::last(yy) - dplyr::first(yy))^2) / 1000,
      .groups = "drop"
    ) %>%
    dplyr::group_by(id_x_field) %>%
    dplyr::mutate(
      n_phase = dplyr::row_number(),
      from = dplyr::if_else(phase %in% "ARP", NA_character_, dplyr::lag(arp)),
      to = dplyr::if_else(phase %in% "ARP", NA_character_, dplyr::lead(arp))
    ) %>%
    dplyr::ungroup()

  out <- as.data.frame(out[, c("id_x_field", "n_phase", "phase", "arp", "from", "to",
                               "start", "end", "duration_days", "n_locations",
                               "distance_km", "displacement_km")])
  names(out)[1] <- id.col
  out
}
