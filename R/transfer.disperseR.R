#' Transfer classification data from disperseR object to a base dataset
#'
#' @description Merges behavioral or movement phase classifications generated in a disperseR object back into an external base data.frame using temporal or spatial matching methods.
#'
#' @param base.df A data.frame containing the target dataset where variables will be transferred.
#' @param disperseR.obj An object of class \code{disperseR} containing the analyzed data.
#' @param id.col Character. Column name in \code{base.df} representing individual identifiers.
#' @param time.col Character. Column name in \code{base.df} representing timestamps.
#' @param method Character. The matching method to use for transferring data. Choose one of:
#' \itemize{
#'   \item \code{"temporal"}: Matches each row to the closest point in time.
#'   \item \code{"spatial"}: Matches each row to the closest point in space using minimum Euclidean distance.
#'   \item \code{"forward"}: Matches each row to the nearest future location in the disperseR object.
#'   \item \code{"backward"}: Matches each row to the nearest past location in the disperseR object].
#'   \item \code{"movement"}: Uses temporal matching but applies a specific criteria for phase transitions. If the target point falls between two identical phases, it assigns that phase. If it falls exactly between a ARP (Area-Restricted Phase) and a movement phase, it prioritizes assigning the movement phase.
#'   \item \code{"arp"}: Uses temporal matching but applies a specific criteria for phase transitions. If the target point falls between two identical phases, it assigns that phase. If it falls exactly between a ARP (Area-Restricted Phase) and a movement phase, it prioritizes assigning the ARP phase.
#' }
#' @param coord.names Character vector of length 2. Names of the X and Y coordinate columns in \code{base.df}, required if \code{method = "spatial"}.
#' @param max.diff Numeric. Optional. Maximum allowed time difference (in seconds) for a match to be considered valid.
#'
#' @return A data.frame based on \code{base.df} with appended classification columns from the \code{disperseR} object.
#' @examples
#' eps <- get.epsilon(sim_states)
#' arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
#' mov <- class.mov(link.arp(arp, thres = 24 * 3600))
#'
#' # Transfer phases back to the original (irregular) fixes
#' out <- transfer.disperseR(sim_tracks, mov, id.col = "ID",
#'                           time.col = "timestamp", max.diff = 3600)
#' table(out$ID, out$phase)
#' @export
#'
#' @importFrom dplyr mutate group_by group_split bind_rows select filter rowwise ungroup across everything
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
transfer.disperseR <- function(base.df,
                          disperseR.obj,
                          id.col = "name",
                          time.col = "timestamp",
                          method = "temporal",
                          coord.names = c("x", "y"),
                          max.diff = NULL) {
  
  # General validations
  
  if (!inherits(disperseR.obj, "disperseR")) {
    stop("disperseR.obj must be of class 'disperseR'.")
  }
  
  if (!"data" %in% names(disperseR.obj)) {
    stop("disperseR.obj must contain a 'data' element.")
  }
  
  valid_methods <- c("temporal", "spatial", "forward", "backward", "movement", "arp")
  if (!method %in% valid_methods) {
    stop(paste0("Invalid method '", method, "'. Choose one of: ",
                paste(valid_methods, collapse = ", "), "."))
  }
  
  if (!id.col %in% names(base.df)) {
    stop(paste0("Column '", id.col, "' not found in base.df."))
  }
  
  if (!time.col %in% names(base.df)) {
    stop(paste0("Column '", time.col, "' not found in base.df."))
  }
  
  if (!"time_s" %in% names(disperseR.obj$data)) {
    stop("Column 'time_s' not found in disperseR.obj$data.")
  }
  
  if (!id.col %in% names(disperseR.obj$data)) {
    stop(paste0("Column '", id.col, "' not found in disperseR.obj$data."))
  }
  
  # Validations for method = "spatial"
  
  if (method == "spatial") {
    
    # Coordenadas en disperseR.obj$data
    disp_coords <- names(disperseR.obj$data)
    disp_x <- disp_coords[tolower(disp_coords) == "x"]
    disp_y <- disp_coords[tolower(disp_coords) == "y"]
    
    if (length(disp_x) == 0 || length(disp_y) == 0) {
      stop("Columns 'x'/'y' (or 'X'/'Y') not found in disperseR.obj$data.")
    }
    
    # Coords in base.df
    base_x <- coord.names[1]
    base_y <- coord.names[2]
    
    if (!base_x %in% names(base.df) || !base_y %in% names(base.df)) {
      stop(paste0("For method 'spatial', columns '", base_x, "' and '", base_y,
                  "' not found in base.df."))
    }
    
    # Warning sif names are not x/y standard
    if (!tolower(base_x) %in% c("x", "y") || !tolower(base_y) %in% c("x", "y")) {
      disp_crs <- if (!is.null(disperseR.obj$crs)) {
        paste0(" (disperseR.obj CRS: ", disperseR.obj$crs, ")")
      } else {
        ""
      }
      warning(paste0("coord.names are not standard 'x'/'y'. Make sure base.df coordinates ",
                     "are in the same projected coordinate system (meters) as disperseR.obj$data",
                     disp_crs, "."))
    }
  }
  
  # Columns to transfer
  
  cols_to_transfer <- c("state", "cluster_id", "arp",
                        "linked_clusters", "arp_link", "phase")
  
  missing_cols <- setdiff(cols_to_transfer, names(disperseR.obj$data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns missing in disperseR.obj$data:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # Helpers
  
  # Applies max.diff: returns NA if differences over the threshold
  apply_maxdiff <- function(val, diff_val, na_val) {
    if (!is.null(max.diff) && diff_val > max.diff) na_val else val
  }
  
  # Transfers all columns from idx row in disperseR_ind
  transfer_row <- function(ind_data_row, disperseR_ind, idx, diff_val) {
    ind_data_row$time_diff    <- diff_val
    ind_data_row$state        <- apply_maxdiff(disperseR_ind$state[idx],        diff_val, NA_integer_)
    ind_data_row$cluster_id   <- apply_maxdiff(disperseR_ind$cluster_id[idx],   diff_val, NA_integer_)
    ind_data_row$arp    <- apply_maxdiff(disperseR_ind$arp[idx],     diff_val, NA)
    ind_data_row$linked_clusters <- apply_maxdiff(disperseR_ind$linked_clusters[idx], diff_val, NA_character_)
    ind_data_row$arp_link  <- apply_maxdiff(disperseR_ind$arp_link[idx],  diff_val, NA)
    ind_data_row$phase        <- apply_maxdiff(disperseR_ind$phase[idx],        diff_val, NA_character_)
    ind_data_row
  }
  
  # Numerical time
  
  base.df <- base.df %>%
    dplyr::mutate(time_s_base = as.numeric(.data[[time.col]]))
  
  # Process by individual
  
  result <- base.df %>%
    dplyr::group_by(.data[[id.col]]) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(ind_data) {
      
      ind_name     <- unique(ind_data[[id.col]])
      disperseR_ind <- disperseR.obj$data %>%
        dplyr::filter(.data[[id.col]] == ind_name)
      
      # Without data for that individual
      if (nrow(disperseR_ind) == 0) {
        ind_data$state           <- NA_integer_
        ind_data$cluster_id      <- NA_integer_
        ind_data$arp       <- NA
        ind_data$linked_clusters <- NA_character_
        ind_data$arp_link  <- NA
        ind_data$phase           <- NA_character_
        ind_data$time_diff       <- NA_real_
        message(paste0("No data in disperseR.obj for individual: ", ind_name))
        return(ind_data)
      }
      
      # Run
      
      ind_data <- ind_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x)) %>% 
        dplyr::ungroup()
      
      warned_na <- FALSE
      ind_data <- dplyr::bind_rows(lapply(seq_len(nrow(ind_data)), function(i) {
        
        row_i    <- ind_data[i, ]
        t_base   <- row_i$time_s_base
        t_disp   <- disperseR_ind$time_s
        t_diffs  <- t_base - t_disp  
        
        if (method == "temporal") {
          
          idx  <- which.min(abs(t_diffs))
          diff_val <- abs(t_diffs[idx])
          transfer_row(row_i, disperseR_ind, idx, diff_val)
          
        } else if (method == "spatial") {
          
          disp_x_col <- names(disperseR_ind)[tolower(names(disperseR_ind)) == "x"]
          disp_y_col <- names(disperseR_ind)[tolower(names(disperseR_ind)) == "y"]
          
          dx   <- row_i[[coord.names[1]]] - disperseR_ind[[disp_x_col]]
          dy   <- row_i[[coord.names[2]]] - disperseR_ind[[disp_y_col]]
          dist <- sqrt(dx^2 + dy^2)
          idx  <- which.min(dist)
          diff_val <- abs(t_diffs[idx])
          transfer_row(row_i, disperseR_ind, idx, diff_val)
          
        } else if (method == "forward") {
          
          future_idx <- which(t_diffs < 0)
          if (length(future_idx) == 0) {
            idx <- which.min(abs(t_diffs))
          } else {
            idx <- future_idx[which.min(abs(t_diffs[future_idx]))]
          }
          diff_val <- abs(t_diffs[idx])
          transfer_row(row_i, disperseR_ind, idx, diff_val)
          
        } else if (method == "backward") {
          
          past_idx <- which(t_diffs > 0)
          if (length(past_idx) == 0) {
            idx <- which.min(abs(t_diffs))
          } else {
            idx <- past_idx[which.min(t_diffs[past_idx])] 
          }
          diff_val <- abs(t_diffs[idx])
          transfer_row(row_i, disperseR_ind, idx, diff_val)
          
        } else if (method %in% c("movement", "arp")) {
          
          past_idx   <- which(t_diffs > 0)
          future_idx <- which(t_diffs < 0)
          
          idx_before <- if (length(past_idx)   == 0) which.min(abs(t_diffs)) else
            past_idx[which.min(t_diffs[past_idx])]
          idx_after  <- if (length(future_idx) == 0) which.min(abs(t_diffs)) else
            future_idx[which.min(abs(t_diffs[future_idx]))]
          
          phase_before <- disperseR_ind$phase[idx_before]
          phase_after  <- disperseR_ind$phase[idx_after]
          
          if (is.na(phase_before) || is.na(phase_after)) {
            if (!warned_na) {
              warning(paste0("Individual '", ind_name, 
                             "': NA phase values found. Assigning closest in time for affected rows."))
              warned_na <<- TRUE
            }
            chosen_idx  <- which.min(abs(t_diffs))
            chosen_diff <- abs(t_diffs[chosen_idx])
            return(transfer_row(row_i, disperseR_ind, chosen_idx, chosen_diff))
            
          } else if (phase_before == phase_after) {
            chosen_idx  <- idx_before
            chosen_diff <- abs(t_diffs[idx_before])
            
          } else {
            # ARP phase label as produced by class.mov() / Etiqueta de fase ARP generada por class.mov()
            is_hr_before <- phase_before == "ARP"
            is_hr_after  <- phase_after  == "ARP"
            
            if (!is_hr_before && !is_hr_after) {
              warning(paste0(
                "Individual '", ind_name, "': phase transition between '",
                phase_before, "' and '", phase_after,
                "' does not involve an 'ARP' phase. ",
                "This should not happen. Assigning closest in time."
              ))
              chosen_idx  <- which.min(abs(t_diffs))
              chosen_diff <- abs(t_diffs[chosen_idx])
              
            } else if (method == "movement") {
              chosen_idx  <- if (!is_hr_before) idx_before else idx_after
            } else {
              chosen_idx  <- if (is_hr_before) idx_before else idx_after
            }
            chosen_diff <- abs(t_diffs[chosen_idx])
          }
          
          transfer_row(row_i, disperseR_ind, chosen_idx, chosen_diff)
        }
      }))
      
      return(ind_data)
    })
  
  # Cleaning
  
  result <- result %>% dplyr::select(-time_s_base)
  
  # Report
  
  if (!is.null(max.diff)) {
    n_no_match <- sum(result$time_diff > max.diff, na.rm = TRUE)
    if (n_no_match > 0) {
      message(paste(n_no_match, "rows without match within the threshold of",
                    max.diff, "seconds"))
    }
  }
  
  message(paste("Data transferred for",
                length(unique(result[[id.col]])), "individuals"))
  
  return(result)
}