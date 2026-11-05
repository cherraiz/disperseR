#' Print a disperseR object
#'
#' @description Prints a short summary of a \code{disperseR} object: number of individuals and
#'   locations, the steps of the workflow already applied, and the number of locations in each
#'   movement phase when \code{\link{class.mov}} has been run.
#'
#' @param x An object of class \code{disperseR}.
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#' @param ... Not used.
#'
#' @return \code{x}, invisibly.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' arp
#' class.mov(link.arp(arp, thres = 24 * 3600))
#' @export
print.disperseR <- function(x, id.col = "ID", ...) {

  df <- x$data
  cat("<disperseR object>\n")

  if (is.null(df) || !is.data.frame(df)) {
    cat("  No data element.\n")
    return(invisible(x))
  }

  n_ind <- if (id.col %in% names(df)) length(unique(df[[id.col]])) else NA
  cat("  Individuals:", n_ind, "| Locations:", nrow(df), "\n")

  # Steps of the workflow / Pasos del flujo de trabajo
  steps <- c(
    "HMM states (fitHMM.wrap)" = "state" %in% names(df) && !is.null(x$models),
    "ARP clusters (get.arp)" = "arp" %in% names(df),
    "Linked ARPs (link.arp)" = "linked_clusters" %in% names(df),
    "Movement phases (class.mov)" = "phase" %in% names(df)
  )
  cat("  Steps:", if (any(steps)) paste(names(steps)[steps], collapse = ", ") else "none", "\n")

  if (!is.null(x$models)) cat("  Models:", length(x$models), "\n")

  if ("arp" %in% names(df) && id.col %in% names(df)) {
    n_arp <- tapply(df$cluster_id[df$arp %in% TRUE], df[[id.col]][df$arp %in% TRUE],
                    function(z) length(unique(z)))
    cat("  ARP clusters per individual:\n")
    print(n_arp[!is.na(n_arp)])
  }

  if ("phase" %in% names(df) && id.col %in% names(df)) {
    cat("  Locations per phase:\n")
    print(table(df[[id.col]], df$phase, dnn = NULL))
  }

  invisible(x)
}
