#' Identify the timepoints in a trajectory that correspond to a change in
#' commanded heading.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param loThresh A bearing change (in degrees per second)
#' @param hiThresh A bearing change (in degrees per second); any time point
#'   associated with a change in bearing greater than this value will definitely
#'   be labeled a turn.
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a turn.
#'
#' @export
identifyBearingChanges <- function(trajectory, loThresh, hiThresh = NA) {
  t <- as.flighttrajectory(trajectory)

  bearingChanges <- c(NA,
                      angleDiff(t$bearing[1:(length(t$bearing)-1)],
                                t$bearing[2:length(t$bearing)])) /
    c(NA, diff(t$time))

  isBearingChange <- hysteresisThresh(bearingChanges, loThresh, hiThresh)
  # if (length(loThresh) > 1) {
  #   isBearingChange <- isBearingChange |
  #     hysteresisThresh(-bearingChanges, -min(loThresh), -min(hiThresh))
  # }

  return(isBearingChange)
}

#' Identify the timepoints in a trajectory that correspond to a change in
#' altitude.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param hiThresh An altitude change (in feet per second); any time point
#'   associated with a change in altitude greater than this value will
#'   definitely be labeled a climb or descent.
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a climb or descent.
#'
#' @export
identifyAltitudeChanges <- function(trajectory, loThresh, hiThresh = NA) {
  t <- as.flighttrajectory(trajectory)
  altitudeChanges <- c(NA, diff(t$altitude)) / c(NA, diff(t$time))

  isAltitudeChange <- hysteresisThresh(altitudeChanges, loThresh, hiThresh)
  # if (length(loThresh) > 1) {
  #   isAltitudeChange <- isAltitudeChange |
  #     hysteresisThresh(-altitudeChanges, -min(loThresh), -min(hiThresh))
  # }

  return(isAltitudeChange)
}

#' Identify the timepoints in a trajectory that correspond to a change in
#' altitude.
#'
#' @param trajectory A \code{flighttrajectory} object (or input coercable to
#'   one) indicating the trajectory of an aircraft.
#' @param hiThresh An altitude change (in feet per second); any time point
#'   associated with a change in altitude greater than this value will
#'   definitely be labeled a climb or descent.
#'
#' @return A logical vector indicating whether each timepoint can be considered
#'   a climb or descent.
#'
#' @export
identifySpeedChanges <- function(trajectory, loThresh, hiThresh = NA) {
  t <- as.flighttrajectory(trajectory)
  speedChanges <- c(NA, diff(t$groundspeed)) / c(NA, diff(t$time))

  isSpeedChange <- hysteresisThresh(speedChanges, loThresh, hiThresh)
  # if (length(loThresh) > 1) {
  #   isSpeedChange <- isSpeedChange |
  #     hysteresisThresh(-speedChanges, -min(loThresh), -min(hiThresh))
  # }

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

  # Loop through the changes and censor them
  for (i in seq_along(censorChanges)) {
    aboveThresh[regionStart[i]:regionEnd[i]] <- FALSE
  }

  return(aboveThresh)
}
