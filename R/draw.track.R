#' Plot movement tracks
#'
#' @description Draws the trajectory of each individual, with locations optionally coloured by
#'   any column of the data, such as the movement \code{phase} from \code{\link{class.mov}},
#'   the behavioural \code{state} from \code{\link{fitHMM.wrap}} or the \code{cluster_id} from
#'   \code{\link{get.arp}}.
#'
#' @param data A data.frame or an object of class \code{disperseR}.
#' @param colour.col Character. Optional name of the column used to colour the locations.
#'   Default is \code{NULL} (no colour).
#' @param ids Character vector. Optional selection of individuals to plot. Default is \code{NULL}
#'   (all individuals).
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#' @param time.col Character. Name of the column containing timestamps, used to order the
#'   locations. Default is "time".
#' @param coord.names Character vector of length 2 with the names of the X and Y coordinate
#'   columns. Default is \code{c("x", "y")}.
#' @param colour.path Logical. If \code{TRUE}, the path between locations is also coloured by
#'   \code{colour.col}; otherwise it is drawn in grey. Default is \code{FALSE}.
#' @param km Logical. If \code{TRUE}, projected coordinates in metres are shown in kilometres.
#'   Default is \code{FALSE}.
#' @param palette Optional named character vector of colours for the values of \code{colour.col}.
#'   If \code{NULL}, a default palette is used for the \code{phase} column and the default
#'   \code{ggplot2} palette otherwise.
#' @param point.size Numeric. Size of the location points. Default is 0.6.
#'
#' @details With one individual the plot keeps the true aspect ratio (\code{coord_equal}). With
#'   several individuals each one is drawn in its own panel with free scales.
#'
#' @return A \code{ggplot} object.
#' @examples
#' draw.track(sim_tracks, time.col = "timestamp", km = TRUE)
#'
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' mov <- class.mov(link.arp(arp, thres = 24 * 3600))
#' draw.track(mov, colour.col = "phase", km = TRUE)
#' draw.track(mov, colour.col = "phase", ids = "M01", colour.path = TRUE, km = TRUE)
#' @export
#'
#' @importFrom dplyr %>% arrange filter
#' @importFrom rlang .data
draw.track <- function(data,
                       colour.col = NULL,
                       ids = NULL,
                       id.col = "ID",
                       time.col = "time",
                       coord.names = c("x", "y"),
                       colour.path = FALSE,
                       km = FALSE,
                       palette = NULL,
                       point.size = 0.6) {

  # Checks
  df <- .get_data(data)

  needed <- c(id.col, time.col, coord.names, colour.col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Columns not found in the data: ", paste(missing_cols, collapse = ", "))
  }
  if (length(coord.names) != 2) {
    stop("'coord.names' must have length 2.")
  }

  df$id_x_field <- as.character(df[[id.col]])
  if (!is.null(ids)) {
    df <- df %>% dplyr::filter(id_x_field %in% as.character(ids))
    if (nrow(df) == 0) stop("None of the selected 'ids' is present in the data.")
  }

  # Order by individual and time / Ordenar por individuo y tiempo
  df <- df %>% dplyr::arrange(id_x_field, .data[[time.col]])

  scale_factor <- if (km) 1000 else 1
  df$x_plot <- df[[coord.names[1]]] / scale_factor
  df$y_plot <- df[[coord.names[2]]] / scale_factor
  unit_lab <- if (km) " (km)" else ""

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x_plot, y = y_plot))

  if (is.null(colour.col)) {
    p <- p +
      ggplot2::geom_path(ggplot2::aes(group = id_x_field), colour = "grey70", linewidth = 0.3) +
      ggplot2::geom_point(size = point.size, colour = "grey25")
  } else {
    df$colour_x_field <- df[[colour.col]]
    if (!is.numeric(df$colour_x_field)) df$colour_x_field <- as.factor(df$colour_x_field)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x_plot, y = y_plot))

    if (colour.path) {
      p <- p + ggplot2::geom_path(ggplot2::aes(group = id_x_field, colour = colour_x_field),
                                  linewidth = 0.5)
    } else {
      p <- p + ggplot2::geom_path(ggplot2::aes(group = id_x_field), colour = "grey80",
                                  linewidth = 0.3)
    }
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = colour_x_field), size = point.size)

    if (is.null(palette) && colour.col == "phase") palette <- .phase_palette
    if (!is.null(palette) && !is.numeric(df$colour_x_field)) {
      p <- p + ggplot2::scale_colour_manual(values = palette, name = colour.col)
    } else {
      p <- p + ggplot2::labs(colour = colour.col)
    }
    if (!is.numeric(df$colour_x_field)) {
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2.5)))
    }
  }

  p <- p +
    ggplot2::labs(x = paste0(coord.names[1], unit_lab), y = paste0(coord.names[2], unit_lab)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  # One panel per individual, or true aspect ratio for a single one
  # Un panel por individuo, o proporción real para uno solo
  if (length(unique(df$id_x_field)) > 1) {
    p <- p + ggplot2::facet_wrap(~id_x_field, scales = "free")
  } else {
    p <- p + ggplot2::coord_equal() + ggplot2::labs(title = unique(df$id_x_field))
  }

  p
}

# Default colours of the movement phases / Colores por defecto de las fases de movimiento
.phase_palette <- c("ARP" = "#4d4d4d", "Type 0" = "#2a78d6", "Type 1" = "#eb6834",
                    "Type 2" = "#1baf7a", "Type 3" = "#e87ba4",
                    "Type 2 or Type 1" = "#b3b3b3")

# Data element of a disperseR object, or the data.frame itself
# Elemento data de un objeto disperseR, o el propio data.frame
.get_data <- function(data) {
  if (inherits(data, "disperseR")) {
    if (!"data" %in% names(data)) stop("The 'disperseR' object must contain a 'data' element.")
    return(as.data.frame(data$data))
  }
  if (is.data.frame(data)) return(as.data.frame(data))
  stop("'data' must be a data.frame or an object of class 'disperseR'.")
}
