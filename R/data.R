#' Simulated dispersal tracks
#'
#' @description Simulated GPS tracks of five dispersing individuals with hourly fixes, timing
#'   jitter and about 10\% missed fixes. Movement follows a diel rhythm (active at night,
#'   resting during the day, also while dispersing); directed movements are biased correlated
#'   random walks, and some of them include short stopovers (0.5-1.5 days). Coordinates are in
#'   ETRS89 / UTM zone 30N (EPSG:25830), in metres.
#' \itemize{
#'   \item \code{"M01"}: settles in an area made of two nearby patches, shuttling between them,
#'     moves to a second area with a stopover on the way, makes a looping excursion and returns
#'     to the first area (with another stopover).
#'   \item \code{"F02"}: settles in one area, makes a 2.5-day stay and settles in two more areas,
#'     with a stopover before the last one.
#'   \item \code{"M03"}: settles in one area and makes a long transit with a monitoring gap of
#'     about two days.
#'   \item \code{"F04"}: home range made of two nearby patches, with two looping excursions.
#'   \item \code{"F05"}: explores with stopovers, settles briefly, departs again and the tracking
#'     ends while it is still moving.
#' }
#'
#' @format A data.frame with 6 columns:
#' \describe{
#'   \item{ID}{Individual identifier.}
#'   \item{timestamp}{Fix time (POSIXct, UTC).}
#'   \item{x, y}{Easting and northing (m).}
#'   \item{sex}{Static individual attribute.}
#'   \item{behaviour}{Simulated behaviour: \code{"stay"} (area-restricted stay, including trips
#'     between nearby patches), \code{"stopover"} or \code{"travel"} (directed movement,
#'     including its daytime rests).}
#' }
#' @source Simulated; see \code{data-raw/simulate_tracks.R} in the package sources.
#' @seealso \code{\link{sim_states}}
"sim_tracks"

#' Simulated dispersal tracks with behavioural states
#'
#' @description \code{\link{sim_tracks}} split at monitoring gaps longer than one
#'   day with \code{\link{divide.track.by.gap}}, regularised to hourly locations
#'   with \code{\link{resample.ctmm}}, and classified by segment with a hidden Markov model
#'   using \code{\link{fitHMM.wrap}} with \code{stationary = TRUE} (the stationary state,
#'   mostly daytime rest, is merged into the encamped state).
#'
#' @format A data.frame with 10 columns:
#' \describe{
#'   \item{ID}{Individual identifier.}
#'   \item{id_split}{Track segment identifier (individual and segment number).}
#'   \item{time}{Location time (POSIXct, UTC).}
#'   \item{time_s}{Location time in seconds since 1970-01-01.}
#'   \item{x, y}{Easting and northing (m, EPSG:25830).}
#'   \item{step}{Step length (m).}
#'   \item{angle}{Turning angle (radians).}
#'   \item{state}{Viterbi-decoded state (1 = Transit, 2 = Encamped).}
#'   \item{sex}{Static individual attribute.}
#' }
#' @source Simulated; see \code{data-raw/simulate_tracks.R} in the package sources.
"sim_states"
