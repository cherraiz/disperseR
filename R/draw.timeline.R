#' Plot the timeline of movement phases
#'
#' @description Draws, for each individual, the sequence of movement phases (or any other
#'   categorical variable) along time. Each location is drawn as a bar that lasts until the next
#'   location, so monitoring gaps appear as empty spaces.
#'
#' @param data A data.frame or an object of class \code{disperseR}, usually the output of
#'   \code{\link{class.mov}}.
#' @param phase.col Character. Name of the column to display. Default is "phase".
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#' @param time.col Character. Name of the column containing timestamps (POSIXct). Default is "time".
#' @param max.gap Numeric. Intervals between consecutive locations longer than \code{max.gap}
#'   times the median interval of the individual are left empty. Default is 3.
#' @param palette Optional named character vector of colours. If \code{NULL}, a default palette
#'   is used for the \code{phase} column and the default \code{ggplot2} palette otherwise.
#'
#' @return A \code{ggplot} object.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' mov <- class.mov(link.arp(arp, thres = 24 * 3600))
#' draw.timeline(mov)
#'
#' # Behavioural states instead of phases
#' draw.timeline(sim_states, phase.col = "state")
#' @export
#'
#' @importFrom dplyr %>% arrange group_by mutate ungroup lead
#' @importFrom stats median
draw.timeline <- function(data,
                          phase.col = "phase",
                          id.col = "ID",
                          time.col = "time",
                          max.gap = 3,
                          palette = NULL) {

  # Checks
  df <- .get_data(data)

  missing_cols <- setdiff(c(phase.col, id.col, time.col), names(df))
  if (length(missing_cols) > 0) {
    stop("Columns not found in the data: ", paste(missing_cols, collapse = ", "))
  }
  if (!inherits(df[[time.col]], "POSIXct")) {
    stop(paste0("Column '", time.col, "' must be of class POSIXct."))
  }

  # Each location lasts until the next one, except across long gaps
  # Cada localización dura hasta la siguiente, salvo en los huecos largos
  df <- df %>%
    dplyr::mutate(id_x_field = as.character(.data[[id.col]]),
                  t_start = .data[[time.col]],
                  value = as.factor(.data[[phase.col]])) %>%
    dplyr::arrange(id_x_field, t_start) %>%
    dplyr::group_by(id_x_field) %>%
    dplyr::mutate(
      dt = as.numeric(dplyr::lead(t_start)) - as.numeric(t_start),
      dt_med = stats::median(dt, na.rm = TRUE),
      dt = ifelse(is.na(dt) | dt > max.gap * dt_med, dt_med, dt),
      t_end = t_start + dt
    ) %>%
    dplyr::ungroup()

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = t_start, xmax = t_end,
                                    ymin = as.numeric(factor(id_x_field)) - 0.35,
                                    ymax = as.numeric(factor(id_x_field)) + 0.35,
                                    fill = value)) +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(factor(df$id_x_field))),
                                labels = levels(factor(df$id_x_field))) +
    ggplot2::scale_x_datetime() +
    ggplot2::labs(x = NULL, y = NULL, fill = phase.col) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  if (is.null(palette) && phase.col == "phase") palette <- .phase_palette
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette, name = phase.col)
  }

  p
}
