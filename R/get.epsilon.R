#' Calculate \eqn{\epsilon}{epsilon} threshold for spatial clustering
#'
#' @description Computes the threshold \eqn{\epsilon}{epsilon} (epsilon) based on step lengths. 
#'   The threshold can be calculated using density curve intersections (requires a 
#'   behavioral state column with exactly two states), or simply by calculating the 
#'   overall mean or median of the step lengths. The calculation can be applied globally, 
#'   per individual, or per group.
#'
#' @param data A data.frame or an object of class \code{disperseR} containing movement data.
#' @param state.col Character. The name of the column containing the behavioral states. 
#'   Only used and required if \code{method = "density"}. Must contain exactly two unique states 
#'   for the evaluated subset. Default is \code{"state"}.
#' @param step.col Character. The name of the column containing the step lengths. Default is \code{"step"}.
#' @param mode Character. The level at which to compute the threshold: \code{"global"}, \code{"individual"}, 
#'   or \code{"groups"}. Default is \code{"global"}.
#' @param method Character. The method used to calculate the threshold: \code{"density"} (intersection of 
#'   density curves for two states), \code{"mean"}, or \code{"median"}. Default is \code{"density"}.
#' @param ids Character. The name of the column containing individual identifiers. Required if \code{mode = "individual"}.
#' @param group.col Character. The name of the column containing group identifiers. Required if \code{mode = "groups"}.
#'
#' @return A numeric value representing the threshold if \code{mode = "global"}, or a named list of numeric 
#'   thresholds if \code{mode = "individual"} or \code{mode = "groups"}.
#' @examples
#' # Intersection of the step-length densities of both states
#' get.epsilon(sim_states)
#'
#' # One threshold per individual, using the median step length
#' get.epsilon(sim_states, mode = "individual", ids = "ID", method = "median")
#' @export
#'
#' @importFrom stats approx density median setNames na.omit
get.epsilon <- function(data, 
                        state.col = "state", 
                        step.col = "step",
                        mode = "global",
                        method = "density",
                        ids = NULL,
                        group.col = NULL) {
  
  # Validate mode
  if (!mode %in% c("individual", "global", "groups")) {
    stop("Argument 'mode' must be 'individual', 'global', or 'groups'.")
  }
  
  # Validate method
  if (!method %in% c("density", "mean", "median")) {
    stop("Argument 'method' must be 'density', 'mean', or 'median'.")
  }
  
  # Handle disperseR object
  if (is.list(data) && all(c("data", "models") %in% names(data))) {
    if (!inherits(data, "disperseR")) {
      stop("If 'data' is a list with 'data' and 'models', it must be of class 'disperseR'.")
    }
    data <- data$data
  } else if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or an object of class 'disperseR'.")
  }
  
  # Validate step.col
  if (!step.col %in% names(data)) {
    stop(paste0("'step.col' column '", step.col, "' not found in data."))
  }
  
  # Validate state.col ONLY if method is density
  if (method == "density") {
    if (!state.col %in% names(data)) {
      stop(paste0("Method 'density' requires 'state.col', but column '", state.col, "' was not found in data."))
    }
    
    # Global check for strictly 2 states in the whole dataset (excluding NAs)
    global_states <- stats::na.omit(unique(data[[state.col]]))
    if (length(global_states) != 2) {
      stop(paste0("Method 'density' requires exactly 2 unique states in '", state.col, "'. Found: ", length(global_states)))
    }
  }
  
  # Validate ids if individual mode
  if (mode == "individual") {
    if (is.null(ids)) stop("Mode 'individual' requires specifying the 'ids' argument.")
    if (!ids %in% names(data)) stop(paste0("'ids' column '", ids, "' not found in data."))
  }
  
  # Validate group.col if groups mode
  if (mode == "groups") {
    if (is.null(group.col)) stop("Mode 'groups' requires specifying the 'group.col' argument.")
    if (!group.col %in% names(data)) stop(paste0("'group.col' column '", group.col, "' not found in data."))
  }
  
  # Warn if ids is provided but irrelevant
  if (mode == "global" && !is.null(ids)) {
    message("Argument 'ids' was provided but will be ignored because mode is set to 'global'.")
  }
  
  # Function to compute epsilon for one dataset
  compute_threshold <- function(df, state.col, step.col, id = NA) {
    
    x <- df[[step.col]][!is.na(df[[step.col]])]
    
    if (method == "mean")   return(mean(x))
    if (method == "median") return(median(x))
    
    # If method is density, we need exactly 2 states for this specific subset
    state_vals <- stats::na.omit(unique(df[[state.col]]))
    
    if (length(state_vals) != 2) {
      warning(sprintf("Subset for '%s' does not have exactly two states in column '%s'. Threshold set to NA.", id, state.col))
      return(NA)
    }
    
    x1 <- df[[step.col]][df[[state.col]] == state_vals[1]]
    x2 <- df[[step.col]][df[[state.col]] == state_vals[2]]
    
    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
    
    d1 <- density(x1)
    d2 <- density(x2)
    
    common_x <- seq(max(min(x1), min(x2)), min(max(x1), max(x2)), length.out = 1000)
    
    y1_interp <- approx(d1$x, d1$y, xout = common_x)$y
    y2_interp <- approx(d2$x, d2$y, xout = common_x)$y
    
    diff <- y1_interp - y2_interp
    sign_change <- which(diff[-1] * diff[-length(diff)] < 0)
    
    if (length(sign_change) == 0) {
      warning(sprintf("No intersection found for '%s'. Using median of the steps.", id))
      return(median(x))
    }
    
    if (length(sign_change) > 1) {
      # ESTRATEGIA: Elegir la intersección donde la densidad total sea mayor.
      densidad_total <- y1_interp[sign_change] + y2_interp[sign_change]
      idx <- sign_change[which.max(densidad_total)]
      message(sprintf("Multiple intersections for '%s'. Threshold set at densest point.", id))
    } else {
      idx <- sign_change
    }
    
    return(common_x[idx])
  }
  
  #### GLOBAL MODE
  if (mode == "global") {
    message("Computing a global threshold using all data combined.")
    return(compute_threshold(data, state.col, step.col, id = "Global"))
  }
  
  #### INDIVIDUAL MODE
  if (mode == "individual") {
    message("Computing thresholds separately for each individual.")
    inds <- unique(data[[ids]])
    thresholds <- stats::setNames(vector("list", length(inds)), inds)
    for (id in inds) {
      sub_data <- data[data[[ids]] == id, ]
      thresholds[[as.character(id)]] <- compute_threshold(sub_data, state.col, step.col, id)
    }
    return(thresholds)
  }
  
  #### GROUPS MODE
  if (mode == "groups") {
    message(paste0("Computing thresholds separately for each group in '", group.col, "'."))
    grps <- unique(data[[group.col]])
    thresholds <- stats::setNames(vector("list", length(grps)), grps)
    for (grp in grps) {
      sub_data <- data[data[[group.col]] == grp, ]
      thresholds[[as.character(grp)]] <- compute_threshold(sub_data, state.col, step.col, id = grp)
    }
    return(thresholds)
  }
}