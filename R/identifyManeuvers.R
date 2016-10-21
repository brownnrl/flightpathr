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

#' Helper function for hysteresis thresholding: if a group of neighboring
#' segments are all above the low threshold and neighbor a segment above the
#' high threshold, they're "above threshold".
hysteresisThresh <- function(x, lo, hi = NA) {
  # Reduces to simple thresholding when a hi threshold isn't specified
  if (is.na(hi)) {
    hi = lo
  }

  aboveThresh <- abs(x) > hi

  ## Having trouble with thresholding for this analysis. Giving up for now. I
  ## apologize to all of the user of this package.

  # x <- abs(x)
  #
  # # Find the beginning and end of all the regions above the low threshold
  # threshEdges <- diff(c(FALSE, x > lo, FALSE))
  # regionStart <- which(threshEdges > 0)
  # regionEnd <- which(threshEdges < 0) - 1
  #
  # # If the sequence ends with a low-threshold region, make sure to capture the
  # # endpoint.
  # if (length(regionEnd) < length(regionStart)) {
  #   regionEnd[length(regionStart)] <- length(x)
  # }
  #
  # # For each region, set to TRUE iff anything inside is above the high threshold
  # aboveThresh <- rep(FALSE, length(x))
  # for (i in seq_along(regionStart)) {
  #   if (any(x[regionStart[i]:regionEnd[i]] > hi)) {
  #     aboveThresh[regionStart[i]:regionEnd[i]] <- TRUE
  #   }
  # }

  return(aboveThresh)
}
