#' Link Area-Restricted Phase (ARP) Clusters
#'
#' @description Merges temporally close Area-Restricted Phase (ARP) clusters and movement 
#'   gaps based on a specified temporal threshold using graph components from \code{igraph}.
#'
#' @param data An object of class \code{disperseR} containing movement data and ARP cluster definitions.
#' @param thres Numeric. Temporal gap threshold (in seconds) below which adjacent ARP 
#'   clusters will be linked. Default is 0.
#' @param time.col Character. Name of the column containing timestamp or date-time values. 
#'   Default is \code{"time"}.
#' @param id.col Character. Name of the column containing individual identifiers. 
#'   Default is \code{"ID"}.
#'
#' @return An updated object of class \code{disperseR} containing linked cluster IDs 
#'   and associated graph network structures under \code{cluster_graphs}.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' # Link ARP clusters separated by less than 24 hours
#' linked <- link.arp(arp, thres = 24 * 3600)
#' table(linked$data$ID, linked$data$linked_clusters)
#' @export
#'
#' @importFrom dplyr %>% mutate filter arrange summarise pull left_join ungroup case_when lag bind_rows
#' @importFrom purrr map map2 list_flatten set_names map_chr
#' @importFrom igraph graph_from_data_frame components
#' @importFrom tibble tibble
#' @importFrom utils head
#' @importFrom graphics pairs
#' @importFrom stats end start step
link.arp <- function(data, 
                         thres = 0, 
                         time.col = "time", 
                         id.col = "ID") {
  
  # Checks
  if (!inherits(data, "disperseR")) {
    stop("Input must be an object of class 'disperseR'.")
  }
  if (!"data" %in% names(data)) {
    stop("The 'disperseR' object must contain an element named 'data'.")
  }
  
  dat <- data$data
  
  if (!id.col %in% names(dat)) {
    stop(paste0("ID column '", id.col, "' not found in the data."))
  }
  
  if (!"arp" %in% names(dat) || !"cluster_id" %in% names(dat)) {
    stop("Data must contain both 'arp' and 'cluster_id' columns.")
  }
  if (!time.col %in% names(dat)) {
    stop(paste0("Time column '", time.col, "' is not present in the data."))
  }
  
  # Define internal ID field for consistency
  dat$id_x_field <- dat[[id.col]]
  
  # Convert time column to numeric seconds for comparison
  dat <- dat %>% dplyr::mutate(.time_sec = as.numeric(.data[[time.col]]))
  
  # Process each individual
  results <- dat %>%
    split(.$id_x_field) %>%
    purrr::map(function(hmm_data) {
      
      # Ensure chronological order within individual / Asegurar orden cronológico por individuo
      hmm_data <- hmm_data[order(hmm_data$.time_sec), , drop = FALSE]
      
      if (all(!hmm_data$arp)) {
        warning(sprintf(
          "No ARP clusters detected for individual '%s' - cannot link ARP clusters.",
          unique(hmm_data$id_x_field)
        ))
        return(list(
          id_x_field = unique(hmm_data$id_x_field),
          data = hmm_data,
          edges = NULL,
          nodes = NULL,
          all_edges = NULL,
          g = NULL,
          linked_clusters = NULL
        ))
      }
      
      # Initialize linking fields
      hmm_data$cluster_id_linking <- hmm_data$cluster_id
      hmm_data$arp_linking <- hmm_data$arp
      
      # TEMPORAL GAP FILLING 
      r <- rle(hmm_data$arp)
      idx_start <- cumsum(c(1, utils::head(r$lengths, -1)))
      idx_end   <- cumsum(r$lengths)
      
      for (i in seq_along(r$values)) {
        
        # We only care about arp == FALSE sections (movement)
        if (!r$values[i]) {
          
          gap_start <- idx_start[i]
          gap_end   <- idx_end[i]
          
          # Need ARP blocks on both sides
          if (i > 1 && r$values[i - 1]) {
            # Left boundary (last ARP before gap)
            left_end_idx <- idx_end[i - 1]
            t_left <- hmm_data$.time_sec[left_end_idx]
            left_cluster <- hmm_data$cluster_id_linking[left_end_idx]
          } else {
            next
          }
          
          if (i < length(r$values) && r$values[i + 1]) {
            # Right boundary (first ARP after gap)
            right_start_idx <- idx_start[i + 1]
            t_right <- hmm_data$.time_sec[right_start_idx]
            right_cluster <- hmm_data$cluster_id_linking[right_start_idx]
          } else {
            next
          }
          
          # Compute temporal gap between ARP blocks flanking the movement block
          delta_t <- t_right - t_left
          
          # Fill gap if delta_t <= thres
          if (!is.na(delta_t) && delta_t <= thres) {
            hmm_data$arp_linking[gap_start:gap_end] <- TRUE
            # Assign the cluster of the left ARP block
            hmm_data$cluster_id_linking[gap_start:gap_end] <- left_cluster
          }
        }
      }
      
      # Get edges
      edges <- hmm_data %>%
        dplyr::mutate(seq_id = cumsum(dplyr::lag(arp_linking, default = FALSE) != arp_linking)) %>%
        dplyr::group_by(seq_id) %>%
        dplyr::filter(arp_linking) %>%
        dplyr::arrange(.time_sec) %>%
        dplyr::summarise(
          cluster_seq = list(cluster_id_linking),
          time_seq    = list(.time_sec),
          .groups     = "drop"
        ) %>%
        dplyr::mutate(pairs = purrr::map2(cluster_seq, time_seq, function(cls, tms) {
          transitions <- which(diff(cls) != 0)
          keep <- (tms[transitions + 1] - tms[transitions]) <= thres
          purrr::map(transitions[keep], ~ c(cls[.x], cls[.x + 1]))
        })) %>%
        dplyr::pull(pairs) %>%
        purrr::list_flatten()
      
      # Loop of isolated ARP clusters
      nodes <- hmm_data$cluster_id_linking[hmm_data$arp_linking] %>% unique()
      self_loops <- purrr::map(nodes, ~c(.x, .x))
      
      # Link clusters
      all_edges <- dplyr::bind_rows(purrr::map(c(edges, self_loops), ~tibble::tibble(from = .x[1], to = .x[2])))
      
      # Graph
      g <- igraph::graph_from_data_frame(all_edges, directed = FALSE)
      components <- igraph::components(g)
      
      cluster_to_group <- tibble::tibble(
        cluster_id_linking = as.integer(names(components$membership)),
        linked_clusters = paste0("arp_", components$membership)
      )
      
      hmm_data <- hmm_data %>%
        dplyr::left_join(cluster_to_group, by = "cluster_id_linking") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          mov_seq = with(rle(arp_linking == FALSE), rep(seq_along(values), lengths)),
          linked_clusters = dplyr::case_when(
            arp_linking ~ linked_clusters,
            !arp_linking ~ paste0("mov_", mov_seq)
          )
        )
      
      hmm_data$arp_link <- ifelse(!hmm_data$arp & hmm_data$arp_linking, TRUE, FALSE)
      hmm_data$cluster_id_linking <- NULL
      hmm_data$arp_linking <- NULL
      
      list(
        id_x_field = unique(hmm_data$id_x_field),
        data = hmm_data,
        edges = edges,
        nodes = nodes,
        all_edges = all_edges,
        g = g,
        linked_clusters = cluster_to_group
      )
    })
  
  # Combine data of individuals
  updated_data <- dplyr::bind_rows(purrr::map(results, "data"))
  
  # Save graph info
  arp_graphs <- purrr::map(results, ~list(
    edges = .$edges,
    nodes = .$nodes,
    all_edges = .$all_edges,
    g = .$g,
    linked_clusters = .$linked_clusters
  )) %>% purrr::set_names(purrr::map_chr(results, ~ as.character(.x$id_x_field)))
  
  # Return final object
  data$data <- updated_data
  data$data$mov_seq <- NULL
  data$data$id_x_field <- NULL
  data$data$.time_sec <- NULL
  data$cluster_graphs <- arp_graphs
  
  class(data) <- unique(c("disperseR", class(data)))
  
  return(data)
}