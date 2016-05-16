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
  bearings <- coordsToBearing(trajectory)

  bearings[length(bearings)] <- bearings[length(bearings)-1]
  bearingChanges <- c(diff(bearings), NA)

  # Keep bearingChanges in the range [-180, 180]
  bearingChanges[!is.na(bearingChanges) & bearingChanges < -180] <-
    bearingChanges[!is.na(bearingChanges) & bearingChanges < -180] + 360
  bearingChanges[!is.na(bearingChanges) & bearingChanges > 180] <-
    bearingChanges[!is.na(bearingChanges) & bearingChanges > 180] - 360

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
  trajectoryCoords <- get3dCoords(trajectory)
  altitudeChanges <- c(diff(trajectoryCoords[, 3]), 0)

  isAltitudeChange <- abs(altitudeChanges) > hiThresh

  return(isAltitudeChange)
}
