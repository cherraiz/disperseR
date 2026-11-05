#' Classify movement phases
#'
#' @description Classifies movement trajectories into distinct functional phases 
#'   (i.e., ARP, Type 0, Type 1, Type 2, Type 3) based on Area-Restricted Phase (ARP) 
#'   patches, spatial links, and site fidelity, based on network complexity.
#'
#' @details
#' The movement phases follow a 0-3 hierarchy based on spatial network memory:
#' \itemize{
#'   \item \strong{Type 0:} Unanchored movement. Occurs before the first ARP is established.
#'   \item \strong{Type 1:} Loop / Local fidelity. Origin and destination ARPs are identical.
#'   \item \strong{Type 2:} Network expansion. Movement from a known ARP to a newly discovered ARP.
#'   \item \strong{Type 3:} Internal transit. Movement between two different, previously known ARPs.
#' }
#'
#' @param data An object of class \code{disperseR} containing movement data and linked ARP clusters.
#' @param id.col Character. Name of the column containing individual identifiers. Default is "ID".
#' @param time.col Character. Name of the column containing timestamp or date-time values. 
#'   Default is \code{"time"}.
#' @param establish Logical. If \code{TRUE}, applies site fidelity corrections requiring a minimum 
#'   duration at a ARP site to consider it established. Default is \code{FALSE}.
#' @param establish.days Numeric. Minimum duration threshold in days for an ARP cluster to be 
#'   considered an established site. Required if \code{establish = TRUE}. The duration of a stay
#'   goes from the first to the last location of consecutive ARP blocks joined by
#'   \code{\link{link.arp}()}, including the linking locations between them, so a linked ARP made of
#'   several short visits counts as one stay.
#' @param arp.links Character. Method for evaluating links between ARPs. Either \code{"arp"} 
#'   or \code{"mcp"} (Minimum Convex Polygon). Default is \code{"arp"}.
#' \itemize{
#'   \item \code{"arp"}: Classifies locations sequence of ARP cluster links as ARPs.
#'   \item \code{"mcp"}: Calculates a Minimum Convex Polygon (MCP) around the points of the preceding ARP cluster. It evaluates if the sequence of ARP cluster links geometrically falls within this polygon to refine the classification. If so, they are considered ARPs. Otherwise they are considered Type 1. Requires \code{x} and \code{y} coordinate columns.
#' }
#' @return An object of class \code{disperseR} with an updated dataset containing the classified 
#'   \code{phase} and \code{cluster_established} columns.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' linked <- link.arp(arp, thres = 24 * 3600)
#'
#' mov <- class.mov(linked)
#' table(mov$data$ID, mov$data$phase)
#'
#' # Require at least 3 days in an ARP for it to count as established
#' mov_est <- class.mov(linked, establish = TRUE, establish.days = 3)
#' table(mov_est$data$ID, mov_est$data$phase)
#' @export
#'
#' @importFrom dplyr %>%
class.mov <- function(data,
                      id.col = "ID",
                      time.col = "time",
                      establish = FALSE,
                      establish.days = NULL,
                      arp.links = "arp") {
  
  # Checks
  if (!inherits(data, "disperseR")) {
    stop("Input must be an object of class 'disperseR'.")
  }
  if (!"data" %in% names(data)) {
    stop("disperseR object must contain a 'data' element.")
  }
  
  arp.links <- tolower(arp.links)
  if (!arp.links %in% c("arp", "mcp")) {
    stop("'arp.links' must be either 'arp' or 'mcp'.")
  }
  
  if (establish) {
    if (is.null(establish.days) || !is.numeric(establish.days) || establish.days <= 0) {
      stop("'establish.days' must be a positive numeric value when establish = TRUE.")
    }
  }
  
  dat <- data$data
  
  # Handle time
  if (!"time_s" %in% names(dat)) {
    if (!time.col %in% names(dat)) {
      stop(paste0("Time column '", time.col, "' not found in the data."))
    }
    dat$time_s <- as.numeric(dat[[time.col]])
  }
  
  if (!all(c("arp", "linked_clusters", "arp_link") %in% names(dat))) {
    stop("Data must contain columns: 'arp', 'linked_clusters', and 'arp_link'. Run link.arp() first.")
  }
  
  if (!id.col %in% names(dat)) {
    stop(paste0("ID column '", id.col, "' not found in the data."))
  }
  
  if ("phase" %in% names(dat)) {
    stop("Column 'phase' already exists in the data. Please remove it before running this function.")
  }
  
  # Check for monitoring gaps resulting in multiple clusters within continuous ARP
  check_gaps <- dat %>%
    dplyr::group_by(!!rlang::sym(id.col)) %>%
    dplyr::mutate(arp_block = dplyr::consecutive_id(arp)) %>%
    dplyr::filter(arp == TRUE) %>%
    dplyr::group_by(!!rlang::sym(id.col), arp_block) %>%
    dplyr::summarise(
      clusters = paste(unique(linked_clusters), collapse = ", "),
      n_clusters = dplyr::n_distinct(linked_clusters),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_clusters > 1)
  
  if (nrow(check_gaps) > 0) {
    table_text <- paste(utils::capture.output(print(as.data.frame(check_gaps %>% dplyr::select(!!rlang::sym(id.col), clusters)))), collapse = "\n")
    
    warning_msg <- paste0(
      "Multiple linked cluster IDs found within a single continuous ARP sequence.", "\n",
      "This typically occurs when a monitoring gap exists and the individual is later relocated in a different cluster. ", "\n",
      "The function will treat these as distinct ARPs, which could lead to classification artifacts.", "\n",
      "Reviewing the following cases may be convenient:\n\n", 
      table_text, "\n"
    )
    
    warning(warning_msg, call. = FALSE)
  }
  
  dat$id_x_field <- dat[[id.col]]
  
  # Function by individual
  process_individual <- function(hmm_data) {
    
    # Ensure chronological order within individual / Asegurar orden cronológico por individuo
    hmm_data <- hmm_data %>%
      dplyr::arrange(time_s) %>%
      dplyr::mutate(mov_seq = dplyr::consecutive_id(arp, ifelse(arp, linked_clusters, NA)))
    
    phases_df <- hmm_data %>%
      dplyr::group_by(mov_seq) %>%
      dplyr::summarise(
        linked_clusters = unique(linked_clusters),
        arp = unique(arp),
        arp_link = unique(arp_link),
        # Same definition as dur_days in get.arp() / Misma definición que dur_days en get.arp()
        duration_days = (max(time_s) - min(time_s)) / 86400,
        start_s = min(time_s),
        end_s = max(time_s),
        .groups = "drop"
      ) %>%
      dplyr::arrange(mov_seq) %>%
      dplyr::mutate(
        prev_gr = dplyr::lag(linked_clusters),
        next_gr = dplyr::lead(linked_clusters)
      )
    
    # Basic classification (Network Topology Typology: 0, 1, 2, 3)
    seen_clusters <- character()
    phases_df$phase_raw <- NA_character_
    
    for (i in seq_len(nrow(phases_df))) {
      row <- phases_df[i, ]
      if (isTRUE(row$arp) | isTRUE(row$arp_link)) {
        phases_df$phase_raw[i] <- "ARP"
        
      } else if (isFALSE(row$arp) & !is.na(row$prev_gr) & !is.na(row$next_gr) & row$prev_gr == row$next_gr) {
        phases_df$phase_raw[i] <- "Type 1" # Loop (Same node)
        
      } else if (isFALSE(row$arp) & !is.na(row$prev_gr) & !is.na(row$next_gr) & row$prev_gr != row$next_gr) {
        if (row$next_gr %in% seen_clusters) {
          phases_df$phase_raw[i] <- "Type 3" # Internal transit (Between known nodes)
        } else {
          phases_df$phase_raw[i] <- "Type 2" # Network expansion (To a new node)
        }
        
      } else if (isFALSE(row$arp) & is.na(row$prev_gr)) {
        phases_df$phase_raw[i] <- "Type 0" # Unanchored (Initial movement)
        
      } else if (isFALSE(row$arp) & is.na(row$next_gr)) {
        phases_df$phase_raw[i] <- "Type 2 or Type 1" # Ends without knowing destination
      }
      
      seen_clusters <- unique(c(seen_clusters, row$linked_clusters))
    }
    
    phases_df <- phases_df %>%
      dplyr::select(-prev_gr, -next_gr)
    
    ####### MCP for ARP links
    if (arp.links == "mcp") {
      if (any(hmm_data$arp_link, na.rm = TRUE)) {
        
        if (!all(c("x", "y") %in% names(hmm_data))) {
          warning("Columns 'x' and 'y' not found. Skipping MCP analysis.")
        } else {
          
          df_sf <- sf::st_as_sf(hmm_data, coords = c("x", "y"), crs = NA) %>%
            dplyr::mutate(row_id = dplyr::row_number())
          
          movement_links <- df_sf %>%
            dplyr::filter(arp_link == TRUE)
          
          hmm_data$within_arp_hull <- NA
          hmm_data$arp_cluster <- NA
          
          for(i in seq_len(nrow(movement_links))) {
            mov_row <- movement_links$row_id[i]
            
            prev_arp <- df_sf %>%
              dplyr::filter(row_id < mov_row, arp == TRUE) %>%
              utils::tail(1) %>%
              dplyr::pull(linked_clusters)
            
            if(length(prev_arp) > 0) {
              arp_points <- df_sf %>%
                dplyr::filter(linked_clusters == prev_arp, arp == TRUE)
              
              if(nrow(arp_points) >= 3) {
                hull <- sf::st_convex_hull(sf::st_union(arp_points))
                within_hull <- sf::st_within(movement_links[i,], hull, sparse = FALSE)[1,1]
                
                hmm_data$within_arp_hull[mov_row] <- within_hull
                hmm_data$arp_cluster[mov_row] <- prev_arp
              }
            }
          }
          
          phases_df <- phases_df %>%
            dplyr::left_join(
              hmm_data %>%
                dplyr::group_by(mov_seq) %>%
                dplyr::summarise(
                  within_hull = all(within_arp_hull == TRUE, na.rm = TRUE),
                  .groups = "drop"
                ),
              by = "mov_seq"
            )
          
          phases_df <- phases_df %>%
            dplyr::mutate(
              phase_raw = dplyr::case_when(
                arp_link == TRUE & within_hull == TRUE ~ "ARP",
                arp_link == TRUE & within_hull == FALSE ~ "Type 1",
                TRUE ~ phase_raw
              )
            ) %>%
            dplyr::select(-within_hull)
        }
      }
    }
    
    ####### Establishment 
    if (establish) {
      
      phases_df$phase <- NA_character_
      phases_df$cluster_established <- phases_df$linked_clusters
      movement_state <- NULL  
      current_established_cluster <- NA 
      established_history <- character() 
      is_initial_phase <- TRUE 
      
      # Duration of each stay: consecutive ARP blocks joined by links (phase_raw "ARP")
      # count as one stay / Duración de cada estancia: los bloques ARP consecutivos unidos
      # por enlaces (phase_raw "ARP") cuentan como una sola estancia
      phases_df <- phases_df %>%
        dplyr::mutate(
          stay_key = ifelse(phase_raw %in% "ARP", linked_clusters, paste0(".mov_", mov_seq)),
          stay_id = dplyr::consecutive_id(stay_key)
        ) %>%
        dplyr::group_by(stay_id) %>%
        dplyr::mutate(stay_days = (max(end_s) - min(start_s)) / 86400) %>%
        dplyr::ungroup()
      
      clusters_that_establish <- phases_df %>%
        dplyr::filter(arp == TRUE, stay_days >= establish.days) %>%
        dplyr::pull(linked_clusters) %>%
        unique()
      
      if (nrow(phases_df) > 0 && isTRUE(phases_df$arp[1]) && phases_df$linked_clusters[1] %in% clusters_that_establish) {
        established_history <- phases_df$linked_clusters[1]
      }
      
      for (i in seq_len(nrow(phases_df))) {
        curr_cluster <- phases_df$linked_clusters[i]
        
        # ARP Phases
        if (phases_df$arp[i]) {
          
          is_known_site <- curr_cluster %in% established_history
          
          if (is_known_site || phases_df$stay_days[i] >= establish.days) {
            
            phases_df$phase[i] <- "ARP"
            current_established_cluster <- curr_cluster
            phases_df$cluster_established[i] <- current_established_cluster
            movement_state <- NULL 
            is_initial_phase <- FALSE 
            
            if (!is_known_site) {
              established_history <- c(established_history, curr_cluster)
            }
            
          } else {
            # Failed establishment
            if (is_initial_phase) {
              phases_df$phase[i] <- "Type 0"
              movement_state <- "post"
            } else {
              phases_df$phase[i] <- "Type 2"
              if (is.null(movement_state)) movement_state <- "expansion"
            }
            phases_df$cluster_established[i] <- current_established_cluster
          }
          next
        }
        
        # Non-ARP Phases (Movement)
        if (!phases_df$arp[i]) {
          
          if (is.null(movement_state)) {
            if (phases_df$phase_raw[i] == "Type 0") {
              movement_state <- "post"
            } else if (phases_df$phase_raw[i] %in% c("Type 2", "Type 3")) {
              movement_state <- "expansion"
            }
          }
          
          if (is_initial_phase || (!is.null(movement_state) && movement_state == "post")) {
            phases_df$phase[i] <- "Type 0"
            movement_state <- "post"
          } else if (!is.null(movement_state)) {
            phases_df$phase[i] <- "Type 2"
          } else {
            phases_df$phase[i] <- phases_df$phase_raw[i]
          }
          phases_df$cluster_established[i] <- current_established_cluster
        }
      }
      
      ####### POST-CORRECTION: Type 2/0 loops back to origin
      phases_df <- phases_df %>%
        dplyr::mutate(
          is_type_0_2 = phase %in% c("Type 2", "Type 0"),
          mov_block = with(rle(is_type_0_2), rep(seq_along(values), lengths))
        )
      
      if (any(phases_df$is_type_0_2)) {
        
        mov_info <- phases_df %>%
          dplyr::filter(is_type_0_2) %>%
          dplyr::group_by(mov_block) %>%
          dplyr::summarise(
            block_start = min(which(phases_df$mov_block == mov_block[1])),
            block_end = max(which(phases_df$mov_block == mov_block[1])),
            .groups = "drop"
          )
        
        for (i in seq_len(nrow(mov_info))) {
          start_idx <- mov_info$block_start[i]
          end_idx <- mov_info$block_end[i]
          
          prev_established <- phases_df %>%
            dplyr::slice(1:(start_idx - 1)) %>%
            dplyr::filter(phase == "ARP") %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::pull(cluster_established)
          
          if (end_idx < nrow(phases_df)) {
            next_idx <- end_idx + 1
            next_cluster <- phases_df$cluster_established[next_idx]
            
            if (length(prev_established) > 0 && !is.na(next_cluster)) {
              
              if (prev_established == next_cluster) {
                
                phases_df$phase[start_idx:end_idx] <- "Type 1"
                
                if (phases_df$arp[next_idx] == TRUE) {
                  phases_df$phase[next_idx] <- "ARP"
                }
                
              } else {
                previously_established <- phases_df %>%
                  dplyr::slice(1:(start_idx - 1)) %>%
                  dplyr::filter(phase == "ARP") %>%
                  dplyr::pull(cluster_established) %>%
                  unique()
                
                if (next_cluster %in% previously_established) {
                  phases_df$phase[start_idx:end_idx] <- "Type 3"
                  
                  if (phases_df$arp[next_idx] == TRUE) {
                    phases_df$phase[next_idx] <- "ARP"
                  }
                }
              }
            }
          }
        }
      }
      
      phases_df <- phases_df %>%
        dplyr::select(-is_type_0_2, -mov_block)
      #######
      
    } else {
      phases_df$phase <- phases_df$phase_raw
      phases_df$cluster_established <- phases_df$linked_clusters
    }
    
    # Rejoin the data
    hmm_data <- hmm_data %>%
      dplyr::left_join(phases_df %>% dplyr::select(mov_seq, phase), by = "mov_seq") %>%
      dplyr::select(-mov_seq)
    
    hmm_data
  }
  
  # Apply throughout the dataset
  updated_data <- dat %>%
    dplyr::group_split(id_x_field) %>%
    purrr::map_dfr(process_individual)
  
  data$data <- updated_data
  data$data$id_x_field <- NULL
  class(data) <- unique(c("disperseR", class(data)))
  
  return(data)
}