#' Calculate the distance of a flight trajectory from a flight path.
#'
#' @param trajectory A \code{flighttrajectory} object (or an object that can be
#'   coerced into one) indicating the trajectory of an aircraft.
#' @param path A \code{flightpath} object (or an object that can be coerced into
#'   one) indicating the ordered waypoints a pre-defined flight path.
#' @return A data.frame containing two columns representing the distance between
#'   the aircraft and its planned flight path (in feet): \code{horizontal}
#'   indicates the horizontal distance and \code{vertical} indicates the
#'   vertical distance.
#'
#' @export
distanceFromPath <- function(trajectory, path) {
  # Check inputs and get 3D coordinates
  trajectoryCoords <- get3dCoords(as.flighttrajectory(trajectory))
  pathCoords <- get3dCoords(as.flightpath(path))

  numLegs <- nrow(pathCoords)-1

  # Given n points and m legs, store horizontal and vertical distance in an
  # n x m arrays
  hDistanceToLeg <- array(NA, dim = c(nrow(trajectoryCoords), numLegs))
  vDistanceToLeg <- array(NA, dim = c(nrow(trajectoryCoords), numLegs))

  # And the squared "slant range" (euclidean distance) in an n x m array
  slantToLeg <- array(NA, dim = c(nrow(trajectoryCoords), numLegs))

  for (legIdx in seq_len(numLegs)) {
    # For each pair of adjascent waypoints, calculate the horizontal distance
    # from the great circle defined by the pair and all points in the
    # trajectory.
    hDistanceToLeg[, legIdx] <- geosphere::dist2gc(pathCoords[legIdx, c(1,2)],
                                              pathCoords[legIdx+1, c(1,2)],
                                              trajectoryCoords[, c(1,2)],
                                              r = 20925646)


    if (is.na(pathCoords[legIdx, 3]) || is.na(pathCoords[legIdx+1, 3])) {
      # When a waypoint has an altidue of NA, we don't care about its altitude.
      # Therefore, the "vertical deviation" is meaningless. Use 0.
      vDistanceToLeg[, legIdx] <- 0
    } else if (pathCoords[legIdx, 3] == pathCoords[legIdx+1, 3]) {
      # If the waypoints are at the same altitude, just calculate the deviation
      # from this altitude. Easy.
      vDistanceToLeg[, legIdx] <- trajectoryCoords[, 3] - pathCoords[legIdx, 3]
    } else {
      # When waypoints have different altitudes, a flight has "deviated" when
      # it's above the higher altitude or below the lower altitude.
      vDistanceToLeg[, legIdx] <- 0
      deviationAbove <- trajectoryCoords[, 3] - max(pathCoords[c(legIdx, legIdx+1), 3])
      deviationBelow <- trajectoryCoords[, 3] - min(pathCoords[c(legIdx, legIdx+1), 3])
      vDistanceToLeg[deviationAbove > 0, legIdx] <- deviationAbove[deviationAbove > 0]
      vDistanceToLeg[deviationBelow < 0, legIdx] <- deviationBelow[deviationBelow < 0]
    }

    # Squared euclidean distance
    slantToLeg[, legIdx] <- hDistanceToLeg[, legIdx]^2 + vDistanceToLeg[, legIdx]^2
  }

  # Figure out which leg is closer to each point in the trajectory.
  # Note: I can imagine a tortuous path that would result in the closest path
  # alternating between legs. This would need to be rewritten to handle that.
  closestLeg <- apply(slantToLeg, 1, which.min)

  # Return the horizontal and vertical distance to the flight path (distance to
  # the closest leg) as a data.frame.
  distanceToPath <- data.frame(horizontal = hDistanceToLeg[cbind(1:nrow(trajectoryCoords), closestLeg)],
                               vertical = vDistanceToLeg[cbind(1:nrow(trajectoryCoords), closestLeg)])
  return(distanceToPath)
}
