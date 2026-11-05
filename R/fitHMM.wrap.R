#' Fit Hidden Markov Models for Movement Data Classification
#'
#' @description Wrapper around \code{momentuHMM} to fit 2-state or 3-state Hidden Markov Models 
#'   (HMM) to animal movement trajectories. Supports global, individual-level, or group-level model fitting.
#'   
#' @details
#'   Step lengths are modeled using a gamma distribution and turning angles using a wrapped Cauchy distribution.
#'   In 2-state models, state 1 corresponds to Transit and state 2 to Encamped. Although the 2-state model
#'   generally performs well, in datasets lacking clear transit movements and containing abundant stationary
#'   periods, movement classification may be biased towards identifying stationary movements as Encamped and all
#'   other movements as Transit. In such cases, a 3-state model including a stationary state can improve state
#'   discrimination. The stationary state is initialized with a step-length prior equal to one tenth of the
#'   Encamped state (\code{mean.step.2} ) prior from the 2-state model.
#'
#' @param data A data.frame containing spatial tracking trajectories.
#' @param ids Character. Column name containing individual identifiers. Default is NULL.
#' @param coordNames Character vector of length 2 specifying the X and Y coordinate column names. 
#'   Default is \code{c("x", "y")}.
#' @param coordtype Character. Coordinate type passed to \code{momentuHMM::prepData} 
#'   (e.g., "UTM" or "LL"). Default is "UTM".
#' @param mean.step.1 Numeric. Initial mean step length for state 1 (Transit).
#' @param sd.step.1 Numeric. Initial standard deviation of step length for state 1 (Transit).
#' @param mean.step.2 Numeric. Initial mean step length for state 2 (Encamped).
#' @param sd.step.2 Numeric. Initial standard deviation of step length for state 2 (Encamped).
#' @param ang.con.1 Numeric. Initial concentration parameter for turning angle in state 1 (Transit).
#' @param ang.con.2 Numeric. Initial concentration parameter for turning angle in state 2 (Encamped).
#' @param stationary Logical. If \code{TRUE}, fits a 3-state model (including a stationary state) 
#'   and re-maps state 3 to state 2 post-estimation. Default is \code{FALSE}.
#' @param mode Character. Fitting mode: \code{"individual"} (one model per ID), \code{"groups"} 
#'   (one model per defined group), or \code{"global"} (single model for all data). Default is \code{"global"}.
#' @param group.col Character. Column name identifying groups when \code{mode = "groups"}. Default is NULL.
#' @param ... Additional arguments passed to fitHMM.
#'
#' @return An object of class \code{disperseR} containing:
#'   \item{data}{A data.frame with movement parameters and Viterbi-decoded state assignments.}
#'   \item{models}{A list of fitted \code{momentuHMM} model objects.}
#' @examples
#' \donttest{
#' trk <- sim_tracks[sim_tracks$ID == "F02", c("ID", "x", "y")]
#' hmm <- fitHMM.wrap(trk, ids = "ID",
#'                    mean.step.1 = 600, sd.step.1 = 400,
#'                    mean.step.2 = 80, sd.step.2 = 50,
#'                    ang.con.1 = 0.7, ang.con.2 = 0.1)
#' table(hmm$data$state)
#' }
#' @export fitHMM.wrap
#'
fitHMM.wrap <- function(data,
                        ids = NULL,
                        coordNames = c("x", "y"),
                        coordtype = "UTM",
                        mean.step.1 = NULL, sd.step.1 = NULL, 
                        mean.step.2 = NULL, sd.step.2 = NULL,
                        ang.con.1 = NULL, ang.con.2 = NULL,
                        stationary = FALSE,
                        mode = "global", # groups, individual, global
                        group.col = NULL,
                        ...) {
  
  # Checks
  if (!all(coordNames %in% names(data))) {
    stop("'coordNames' columns are not found in data.")
  }
  
  if (!mode %in% c("global", "individual", "groups")) {
    stop("Mode must be 'global', 'individual', or 'groups'.")
  }
  
  if (!stationary %in% c(TRUE, FALSE)) {
    stop("stationary must be TRUE or FALSE.")
  }
  
  if (mode == "groups" && is.null(group.col)) {
    stop("Mode 'groups' requires specifying the 'group.col' argument.")
  }
  
  if (mode == "groups" && !group.col %in% names(data)) {
    stop(paste0("'group.col' column '", group.col, "' not found in data."))
  }
  
  if (is.null(ids)) {
    warning("ID field not specified. Assuming 1 individual.")
    data$ID <- "ind1"
    ids <- "ID"
  } else {
    if (!ids %in% names(data)) stop("'ids' columns are not found in data.")
    # momentuHMM needs the identifier in a column named "ID" / momentuHMM necesita la columna "ID"
    if (ids != "ID" && "ID" %in% names(data)) {
      stop("'data' already has a column named 'ID', so 'ids' = '", ids, "' cannot be used. ",
           "Rename or remove the existing 'ID' column first.")
    }
    names(data)[names(data) == ids] <- "ID"
  }
  
  # Function for processing each individual/group
  process_individual <- function(df, use_id = FALSE) {
    hmm_data <- momentuHMM::prepData(
      df, 
      type = coordtype, 
      coordNames = coordNames
    )
    hmm_data$step <- ifelse(hmm_data$step < 0.001, 0.001, hmm_data$step)
    
    if (!stationary) {
      mod <- momentuHMM::fitHMM(data = hmm_data,
                                nbStates = 2,
                                dist = list(step = 'gamma', angle = 'wrpcauchy'),
                                estAngleMean = list(angle = FALSE),
                                Par0 = list(step = c(mean.step.1, mean.step.2,
                                                     sd.step.1, sd.step.2), 
                                            angle = c(ang.con.1, ang.con.2)),
                                formula = ~1,
                                stateNames = c("Transit", "Encamped"),
                                retryFits = 5, 
                                ...)
      
      hmm_data$state <- momentuHMM::viterbi(mod)
      
    } else {
      mean.step.3 <- mean.step.2 / 10
      sd.step.3   <- mean.step.2 / 10
      ang.con.3   <- 0.01
      
      mod <- momentuHMM::fitHMM(data = hmm_data,
                                nbStates = 3,
                                dist = list(step = 'gamma', angle = 'wrpcauchy'),
                                estAngleMean = list(angle = FALSE),
                                Par0 = list(step = c(mean.step.1, mean.step.2, mean.step.3,
                                                     sd.step.1, sd.step.2, sd.step.3), 
                                            angle = c(ang.con.1, ang.con.2, ang.con.3)),
                                formula = ~1,
                                stateNames = c("Transit", "Encamped", "Stationary"),
                                retryFits = 5,
                                ...)
      
      states <- momentuHMM::viterbi(mod)
      hmm_data$state <- ifelse(states == 3, 2, states)
    }
    
    return(list(model = mod, data = hmm_data))
  }
  
  # MODELO GLOBAL
  if (mode == "global") {
    message("Fitting one model for all individuals.")
    result <- process_individual(data, use_id = TRUE)
    final_data <- result$data
    models <- list(global_model = result$model)
    
    # MODELO INDIVIDUAL  
  } else if (mode == "individual") {
    message("Fitting one model per individual.")
    split_data <- split(data, data$ID)
    final_data <- data.frame()
    models <- list()
    
    for (id in names(split_data)) {
      message(paste0("Fitting model for individual '", id, "'..."))
      tryCatch({
        result <- process_individual(split_data[[id]], use_id = FALSE)
        result$data$ID <- id
        final_data <- rbind(final_data, result$data)
        models[[id]] <- result$model
      }, error = function(e) {
        warning(paste("Model fitting failed for individual", id, ":", e$message))
      })
    }
    
    # MODELO POR GRUPOS
  } else if (mode == "groups") {
    message(paste0("Fitting one model per group in '", group.col, "'."))
    split_data <- split(data, data[[group.col]])
    final_data <- data.frame()
    models <- list()
    
    for (grp in names(split_data)) {
      message(paste0("Fitting model for group '", grp, "'..."))
      tryCatch({
        result <- process_individual(split_data[[grp]], use_id = TRUE)
        final_data <- rbind(final_data, result$data)
        models[[grp]] <- result$model
      }, error = function(e) {
        warning(paste("Model fitting failed for group", grp, ":", e$message))
      })
    }
  }
  
  fin.results <- list(data = final_data, models = models)
  class(fin.results) <- "disperseR"
  return(fin.results)
}