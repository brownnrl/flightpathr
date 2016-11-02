#' Identify the timepoints in a trajectory that correspond to a change in
#' commanded heading.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param bearingThresh A bearing change threshold (in degrees).
#' @param turnThresh A turn rate threshold (in degrees per second).
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a turn.
#'
#' @details A period is marked as a turn if the turn rate is greater than
#'   \code{turnThresh} and the total bearing change is greater than
#'   \code{bearingThresh}.
#'
#' @export
identifyBearingChanges <- function(trajectory, bearingThresh, turnThresh) {
  traj <- as.flighttrajectory(trajectory)

  bearingChanges <- c(NA,
                      angleDiff(traj$bearing[1:(length(traj$bearing)-1)],
                                traj$bearing[2:length(traj$bearing)])) /
    c(NA, diff(traj$time))

  isBearingChange <- identifyChanges(traj$bearing, bearingChanges,
                                     bearingThresh, turnThresh)

  return(isBearingChange)
}

#' Identify the timepoints in a trajectory that correspond to a change in
#' altitude.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param altitudeThresh An altitude threshold (in feet).
#' @param verticalSpeedThresh A vertical speed threshold (in feet per second).
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a climb or descent.
#'
#' @export
identifyAltitudeChanges <- function(trajectory, altitudeThresh, verticalSpeedThresh) {
  traj <- as.flighttrajectory(trajectory)
  altitudeChanges <- c(NA, diff(traj$altitude)) / c(NA, diff(traj$time))

  isAltitudeChange <- identifyChanges(traj$altitude, altitudeChanges,
                                      altitudeThresh, verticalSpeedThresh)

  return(isAltitudeChange)
}

#' Identify the timepoints in a trajectory that correspond to a change in
#' speed.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param speedThresh A speed threshold (in knots).
#' @param accelerationThresh An accelaration threshold (in knots per second).
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a change in speed
#'
#' @export
identifySpeedChanges <- function(trajectory, speedThresh, accelerationThresh) {
  traj <- as.flighttrajectory(trajectory)
  speedChanges <- c(NA, diff(traj$groundspeed)) / c(NA, diff(traj$time))

  isSpeedChange <- identifyChanges(traj$groundspeed, speedChanges,
                                   speedThresh, accelerationThresh)

  return(isSpeedChange)
}

#' Helper function for identifying changes based on change magnitude and change
#' slope.
identifyChanges <- function(x, dx, xThresh, dxThresh) {
  # First apply the threshold to the derivative (w.r.t. time) to identify
  # periods that may contain valid changes.
  aboveThresh <- abs(dx) > dxThresh
  aboveThresh[is.na(aboveThresh)] <- FALSE

  # Find the point before and after each of the change periods
  threshEdges <- diff(c(FALSE, aboveThresh, FALSE))
  regionStart <- which(threshEdges > 0) - 1
  regionEnd <- which(threshEdges < 0)

  # Deal with regions that start or end at the ends of the vector
  regionStart[1] <- max(regionStart[1], 1)
  regionEnd[length(regionEnd)] <- min(regionEnd[length(regionEnd)],
                                      length(dx))

  # Find the magnitude of the change in each region
  changeMagnitude <- x[regionEnd] - x[regionStart]

  # Censor the changes that aren't above the threshold
  censorChanges <- which(abs(changeMagnitude) < xThresh)

  # Loop through the below-threshold changes and censor them
  for (i in censorChanges) {
    aboveThresh[regionStart[i]:regionEnd[i]] <- FALSE
  }

  return(aboveThresh)
}
