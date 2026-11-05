#' Resample tracking data using Continuous-Time Movement Models (CTMM)
#'
#' @description Fits continuous-time movement models to individual tracking data and predicts 
#'   locations at regular time intervals. It handles coordinate reprojection, model selection, 
#'   and retains static individual traits.
#'
#' @param dat A data.frame containing the tracking data.
#' @param tint Numeric. The desired time interval for resampling, in seconds.
#' @param ids Character. The name of the column containing individual identifiers. Default is "ID".
#' @param timestamp Character. The name of the column containing timestamps. Default is "timestamp".
#' @param x Character. The name of the column containing the X coordinate or Longitude. Default is "x".
#' @param y Character. The name of the column containing the Y coordinate or Latitude. Default is "y".
#' @param proj.in An object of class \code{crs} (from \code{sf}), an EPSG code, or a PROJ4 string 
#'   representing the input coordinate reference system.
#' @param proj.out An object of class \code{crs} (from \code{sf}), an EPSG code, or a PROJ4 string 
#'   representing the output coordinate reference system. Must be in meters. Defaults to \code{proj.in}.
#' @param static.cols Character vector. Optional names of columns containing static individual 
#'   information (e.g., sex, species) to append to the resampled dataset. Default is NULL.
#' @param verbose Logical. If TRUE, prints progress and model summaries to the console. Default is TRUE.
#'
#' @return A data.frame with the resampled trajectories, including predicted X and Y coordinates, 
#'   time in POSIXct format, time in numeric seconds, and any specified static columns.
#' @examples
#' \donttest{
#' # Regularise the first 150 fixes of one individual to hourly locations
#' trk <- sim_tracks[sim_tracks$ID == "F02", ][1:150, ]
#' reg <- resample.ctmm(trk, tint = 3600, ids = "ID", timestamp = "timestamp",
#'                      proj.in = 25830, static.cols = "sex", verbose = FALSE)
#' head(reg)
#' }
#' @export
#'
#' @importFrom sf st_crs st_as_sf st_transform st_coordinates
resample.ctmm <- function(dat,
                          tint,
                          ids = "ID",
                          timestamp = "timestamp",
                          x = "x",
                          y = "y",
                          proj.in,
                          proj.out = proj.in,
                          static.cols = NULL,
                          verbose = TRUE) {
  
  # Time to POSIXct check
  to_POSIXct_safe <- function(x, tz = "UTC") {
    if (inherits(x, "POSIXct")) {
      return(x)
    } else if (inherits(x, "POSIXlt")) {
      return(as.POSIXct(x, tz = tz))
    } else if (inherits(x, "Date")) {
      warning("Timestamp provided in Date format, assigning time 00:00 to all records")
      return(as.POSIXct(x, tz = tz))
    } else {
      stop("The timestamp column must be of type Date, POSIXlt or POSIXct.")
    }
  }
  
  # Convert time to POSIXct
  dat[[timestamp]] <- to_POSIXct_safe(dat[[timestamp]])
  
  # Projections check
  proj_out_obj <- sf::st_crs(proj.out)
  proj_string <- ifelse(is.numeric(proj.out),
                        proj_out_obj$input,
                        proj_out_obj$proj4string)
  
  if (is.na(proj_out_obj$proj4string)) 
    stop("proj.out not valid or can not include a valid PROJ string field.")
  if (!grepl("+units=m", proj_out_obj$proj4string, ignore.case = TRUE))
    stop("proj.out must be in meters.")
  
  # Reproject for as.telemetry
  sf_points <- sf::st_as_sf(dat, coords = c(x, y), crs = sf::st_crs(proj.in))
  sf_wgs84 <- sf::st_transform(sf_points, crs = 4326)
  
  # Extract columns and create
  coords <- sf::st_coordinates(sf_wgs84)
  timestamps <- sf_wgs84[[timestamp]]
  id_vals <- sf_wgs84[[ids]]
  
  tdat_all <- data.frame(
    timestamp = timestamps,
    location.long = coords[,1],
    location.lat = coords[,2],
    id_x_field = id_vals
  )
  
  # Execute CTMM re-sampling
  process_one <- function(id, data) {
    if(verbose) cat("Working on individual", id, "\n")
    
    # Eliminate duplicates
    tdat <- data[data$id_x_field == id, ]
    tdat <- tdat[order(tdat$timestamp), ]
    tdat <- tdat[!duplicated(tdat$timestamp), ]
    
    if(verbose) cat("  N locations:", nrow(tdat), "\n")
    
    # Create telemetry object
    suppressWarnings({
      telm <- ctmm::as.telemetry(
        tdat[, c("timestamp", "location.long", "location.lat")],
        projection = proj_string
      )
    })
    
    # Fit CTMM with error handling
    fit <- tryCatch({
      guess <- ctmm::ctmm.guess(data = telm, interactive = FALSE)
      
      # Try different models if the first one doesn't work
      fit <- ctmm::ctmm.fit(data = telm, CTMM = guess)
      
      # Verify goodness of fit
      if(verbose) {
        cat("  Model fitted:", summary(fit)$name, "\n")
        print(summary(fit)$CI)
        cat("\n")
      }
      
      # If var is too low, try a simpler model
      if(fit$sigma@par[1] < 1e-6) {
        warning("Very low variance for ID ", id, ". Trying simpler model.")
        guess$tau <- NULL  # Modelo IID (independiente)
        fit <- ctmm::ctmm.fit(data = telm, CTMM = guess)
      }
      
      fit
    }, error = function(e) {
      warning("Error fitting model for ID ", id, ": ", e$message)
      # Usar modelo IID como fallback
      guess <- ctmm::ctmm(sigma=1)
      ctmm::ctmm.fit(data = telm, CTMM = guess)
    })
    
    # Resample at tint time interval
    tdat$t <- as.numeric(tdat$timestamp)
    mint <- min(telm$t)
    maxt <- max(telm$t)
    interp_vals <- seq(min(tdat$t), max(tdat$t), by = tint)
    newt <- interp_vals[interp_vals >= mint & interp_vals <= maxt]
    
    if(verbose) cat("  Predicting for", length(newt), "timepoints\n")
    
    pred <- ctmm::predict(fit, data = telm, t = newt)
    
    # For verifying 
    if(verbose) {
      cat("  Predicted X range:", range(pred$x), "\n")
      cat("  Predicted Y range:", range(pred$y), "\n")
      cat("  Original X range:", range(telm$x), "\n")
      cat("  Original Y range:", range(telm$y), "\n\n")
    }
    
    # Save the results
    pred_dat <- data.frame(
      id_x_field = id,
      x = pred$x,
      y = pred$y,
      time = as.POSIXct(pred$t, origin = "1970-01-01", tz = "UTC"),
      time_s = pred$t
    )
    
    if (!is.null(static.cols)) {
      static.dat <- dat[dat[[ids]] == id, ]
      static_info <- static.dat[1, static.cols, drop = FALSE]
      pred_dat <- cbind(pred_dat, static_info[rep(1, nrow(pred_dat)), , drop = FALSE])
    }
    
    return(pred_dat)
  }
  
  all_ids <- unique(tdat_all$id_x_field)
  pred_list <- lapply(all_ids, function(id) process_one(id, tdat_all))
  pred_dat <- do.call(rbind, pred_list)
  names(pred_dat)[names(pred_dat) == "id_x_field"] <- ids
  
  return(pred_dat)
}