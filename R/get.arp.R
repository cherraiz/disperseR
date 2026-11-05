#' Identify Area-Restricted Phase (ARP)  clusters
#'
#' @description Identifies spatial Area-Restricted Phase (ARP) clusters using DBSCAN based on spatial 
#'   proximity, duration, and state classification proportions.
#'
#' @param data A data.frame or an object of class \code{disperseR}.
#' @param ids Character. Name of the column containing individual identifiers. Default is "ID".
#' @param epsilon Numeric or named list. Spatial distance threshold for DBSCAN. Can be a single value, 
#'   or a named list per individual or group.
#' @param minPts Integer. Minimum number of points required to form a cluster in DBSCAN. Default is 3.
#' @param time.col Character. Name of the column containing timestamps (must be POSIXct). 
#'   Default is "time".
#' @param state.col Character. Name of the column containing behavioral or movement state 
#'   classifications. Default is "state".
#' @param pct.state1 Numeric. Maximum proportion threshold of state 1 points allowed in a cluster. 
#'   If not provided (NULL), this filter is not applied and a warning is triggered.
#' @param delta.days Numeric. Minimum duration threshold in days for a cluster to be classified as a 
#'   ARP area. Duration is computed from the first to the last location assigned to the cluster, 
#'   regardless of intermediate locations. If not provided (NULL), this filter is not applied and a 
#'   warning is triggered.
#' @param group.col Character. Optional column name used for grouping when \code{epsilon} is provided 
#'   as a named list.
#'
#' @return An object of class \code{disperseR} containing the updated dataset with cluster assignments, 
#'   a list of cluster summaries, and list of filtered ARP clusters.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' arp$arp_clusters
#' @export
#'
#' @importFrom dplyr mutate group_by summarise filter pull n
#' @importFrom dbscan dbscan
#' @importFrom rlang .data
get.arp <- function(data,
                         ids = "ID",
                         epsilon,
                         minPts = 3,
                         time.col = "time",
                         state.col = "state",
                         pct.state1 = NULL,
                         delta.days = NULL,
                         group.col = NULL) {
  
  # Throw warnings if important parameters are NULL
  if (is.null(delta.days)) {
    warning("'delta.days' is NULL. The duration filter will not be applied. This is an important parameter for ARP identification.")
  }
  if (is.null(pct.state1)) {
    warning("'pct.state1' is NULL. The state proportion filter will not be applied. This is an important parameter for ARP identification.")
  }
  
  # Check if dataframe or disperseR object
  if (is.list(data) && all(c("data", "models") %in% names(data))) {
    if (!inherits(data, "disperseR")) {
      stop("If 'data' is a list with 'data' and 'models', it must be of class 'disperseR'.")
    }
    models <- data$models
    data <- data$data
  } else if (is.data.frame(data)) {
    models <- NULL
  } else {
    stop("'data' must be either a data.frame or an object of class 'disperseR'")
  }
  
  # Check ID column
  if (!ids %in% names(data)) {
    stop("The specified ID column does not exist in the data.")
  }
  
  # Check and create time_s column if it doesn't exist
  if (!"time_s" %in% names(data)) {
    if (!time.col %in% names(data)) {
      stop(paste0("Column '", time.col, "' not found in the data."))
    }
    
    # Verify time.col is POSIXct
    if (!inherits(data[[time.col]], "POSIXct")) {
      stop(paste0("Column '", time.col, "' must be of class POSIXct."))
    }
    
    # Create time_s as numeric seconds from Unix epoch
    data$time_s <- as.numeric(data[[time.col]])
  }
  
  unique_ids <- unique(data[[ids]])
  
  # Check epsilon
  if (is.list(epsilon)) {
    if (!is.null(group.col)) {
      missing_groups <- setdiff(unique(data[[group.col]]), names(epsilon))
      if (length(missing_groups) > 0) {
        stop("Missing epsilon values for groups: ", paste(missing_groups, collapse = ", "))
      }
    } else {
      missing_ids <- setdiff(unique_ids, names(epsilon))
      if (length(missing_ids) > 0) {
        stop("Missing epsilon values for: ", paste(missing_ids, collapse = ", "))
      }
    }
  }
  
  # Create new columns and lists
  data$cluster_id <- NA
  data$arp <- NA
  cluster_summary_list <- list()
  cluster_filtered_list <- list()
  
  # Loop for each individual
  for (id in unique_ids) {
    dat <- data[data[[ids]] == id, ]
    
    # Get individual/group epsilon
    eps_val <- if (is.list(epsilon)) {
      if (!is.null(group.col)) epsilon[[as.character(dat[[group.col]][1])]] else epsilon[[as.character(id)]]
    } else {
      epsilon
    }
    
    # Check that X and Y exist
    if (!all(c("x", "y") %in% names(dat))) {
      stop("Columns 'x' and 'y' must be present in the data.")
    }
    
    coords <- as.matrix(dat[, c("x", "y")])
    
    # Apply DBSCAN (Spatial clustering)
    db <- dbscan::dbscan(coords, eps = eps_val, minPts = minPts)
    data$cluster_id[data[[ids]] == id] <- db$cluster
    
    # Cluster summary
    cluster_summary <- dat %>%
      dplyr::mutate(cluster_id = db$cluster) %>%
      dplyr::group_by(cluster_id) %>%
      dplyr::summarise(
        start = min(.data[[time.col]]),
        end = max(.data[[time.col]]),
        dur_days = as.numeric(difftime(end, start, units = "days")),
        pct_state1 = mean(.data[[state.col]] == 1),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    cluster_summary_list[[as.character(id)]] <- cluster_summary
    
    # Identify ARP clusters as those with
    # 1. Longer duration than delta.days AND
    # 2. Lower percentage in state 1 than pct.state1
    cluster_filtered <- cluster_summary %>%
      dplyr::filter(cluster_id > 0)
    
    if (!is.null(delta.days)) {
      cluster_filtered <- cluster_filtered %>%
        dplyr::filter(dur_days >= delta.days)
    }
    
    if (!is.null(pct.state1)) {
      cluster_filtered <- cluster_filtered %>%
        dplyr::filter(pct_state1 < .env$pct.state1)
    }
    
    clusters_arp <- cluster_filtered %>% dplyr::pull(cluster_id)
    
    cluster_filtered_list[[as.character(id)]] <- cluster_filtered
    
    # Add ARP clusters to dataset
    data$arp[data[[ids]] == id] <- data$cluster_id[data[[ids]] == id] %in% 
      clusters_arp
  }
  
  # Final result
  result <- list(
    data = data,
    models = models,
    cluster_summary = cluster_summary_list,
    arp_clusters = cluster_filtered_list
  )
  
  class(result) <- "disperseR"
  return(result)
}