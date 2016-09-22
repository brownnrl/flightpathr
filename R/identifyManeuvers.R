#' Identify the timepoints in a trajectory that correspond to a change in
#' commanded heading.
#'
#' @param trajectory A matrix or SpatialPoints object indicating the trajectory
#'   of an aircraft.
#' @param hiThresh A bearing change (in degrees); any time point associated with
#'   a change in bearing greater than this value will definitely be labeled a
#'   turn.
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a turn.
#'
#' @export
identifyBearingChanges <- function(trajectory, hiThresh) {
  bearings <- as.flighttrajectory(trajectory)$bearing

  bearingChanges <- c(angleDiff(bearings[1:(length(bearings)-1)],
                                bearings[2:length(bearings)]),
                      NA)

  isBearingChange <- abs(bearingChanges) > hiThresh

  return(isBearingChange)
}

#' Identify the timepoints in a trajectory that correspond to a change in
#' altitude.
#'
#' @param trajectory A matrix or SpatialPoints object indicating the trajectory
#'   of an aircraft.
#' @param hiThresh An altitude change (in feet); any time point associated with
#'   a change in altitude greater than this value will definitely be labeled a
#'   climb or descent.
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a climb or descent.
#'
#' @export
identifyAltitudeChanges <- function(trajectory, hiThresh) {
  trajectoryCoords <- get3dCoords(as.flighttrajectory(trajectory))
  altitudeChanges <- c(diff(trajectoryCoords[, 3]), 0)

  isAltitudeChange <- abs(altitudeChanges) > hiThresh

  return(isAltitudeChange)
}
