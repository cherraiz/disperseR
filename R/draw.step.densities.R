#' Plot Step Length Densities by Behavioral State
#'
#' @description Visualizes the density distribution of step lengths for different 
#'   behavioral states using \code{ggplot2}. It can plot the distributions for a 
#'   single individual or for the entire dataset, and optionally overlay a calculated 
#'   threshold.
#'
#' @param data A data.frame or an object of class \code{disperseR} containing movement data.
#' @param ind Character or Numeric. The specific individual ID to plot. If \code{NULL}, 
#'   densities for all individuals are plotted combined. Default is \code{NULL}.
#' @param ids Character. The name of the column containing individual identifiers. 
#'   Required if \code{ind} is provided. Default is \code{NULL}.
#' @param state.col Character. The name of the column containing the behavioral states. 
#'   Default is \code{"state"}.
#' @param step.col Character. The name of the column containing the step lengths. 
#'   Default is \code{"step"}.
#' @param threshold Numeric. An optional threshold value (e.g., \code{ε} threshold) to 
#'   plot as a vertical dashed line. Default is \code{NULL}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A \code{ggplot} object showing the density curves.
#' @examples
#' eps <- get.epsilon(sim_states)
#' draw.step.densities(sim_states, threshold = eps)
#' draw.step.densities(sim_states, ind = "M01", ids = "ID")
#' @export
#'
#' @import ggplot2
draw.step.densities <- function(data, 
                                ind = NULL,
                                ids = NULL,
                                state.col = "state", 
                                step.col = "step", 
                                threshold = NULL,
                                ...) {
  
  if (is.list(data) && all(c("data", "models") %in% names(data))) {
    if (!inherits(data, "disperseR")) {
      stop("If 'data' is a list with 'data' and 'models', it must be of class 'disperseR'.")
    }
    data <- data$data
  } else if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or an object of class 'disperseR'.")
  }
  
  if (is.null(ind)) {
    message("Plotting densities for all individuals")
  } else {
    if (is.null(ids)) {
      stop("Column containing individuals' IDs must be provided") 
    } else {
      message(paste0("Plotting densities for individual ", ind))
    }
  }
  
  # Filtrar NAs en las columnas de interés
  data <- data[!is.na(data[[step.col]]) & !is.na(data[[state.col]]), , drop = FALSE]
  
  # CORRECCIÓN: Filtrar por individuo SOLO si ind no es NULL
  if (!is.null(ind)) {
    data <- data[data[[ids]] == ind, , drop = FALSE]
  }
  
  # Extraer solo las columnas necesarias en un nuevo data.frame limpio
  plot_data <- data.frame(
    step = data[[step.col]],
    state = data[[state.col]],
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = step, fill = as.factor(state))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("1" = "lightskyblue", "2" = "salmon1"),
                               labels = c("1" = "Transit (state 1)", "2" = "Encamped (state 2)")) +
    ggplot2::labs(
      x = step.col,
      fill = NULL,
      title = ifelse(is.null(ind), 
                     "Step length densities by state",
                     paste0("Step length densities by state for individual ", ind))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.title.position = "plot"
    )
  
  if (!is.null(threshold)) {
    # Extraer la densidad máxima para posicionar bien la etiqueta
    max_density <- max(ggplot2::ggplot_build(p)$data[[1]]$density)
    
    p <- p + ggplot2::geom_vline(xintercept = threshold, linetype = "dashed", color = "red2") +
      ggplot2::geom_label(data = data.frame(x = threshold, y = max_density * 0.9, 
                                            label = paste0("Threshold: ", round(threshold, 2))),
                          ggplot2::aes(x = x, y = y, label = label),
                          hjust = -0.1, vjust = 1.5, color = "red2",
                          inherit.aes = FALSE, fill = "white", linewidth = NA)
  } else {
    message("No threshold provided, plotting only step densities")
  }
  
  return(p)
}