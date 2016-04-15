#' Calculate the distance of a flight trajectory from a flight path.
#'
#' @param trajectory A matrix or SpatialPoints object indicating the trajectory
#'   of an aircraft.
#' @param path A matrix or SpatialPoints object indicating the ordered waypoints
#'   a pre-defined flight path.
#' @return A data.frame containing two columns representing the distance between
#'   the aircraft and its planned flight path (in feet): \code{horizontal}
#'   indicates the horizontal distance and \code{vertical} indicates the
#'   vertical distance.
#'
#' @export
distanceFromPath <- function(trajectory, path) {
  # Check inputs and get 3D coordinates
  trajectoryCoords <- get3dCoords(trajectory)
  pathCoords <- get3dCoords(path)

  numLegs <- nrow(pathCoords)-1

  # Given n points and m legs, store horizontal and vertical distance in an
  # n x m x 2 array
  distanceToLeg <- array(NA, dim = c(nrow(trajectory), 2, numLegs))
  # And the squared "slant range" (euclidean distance) in an n x m array
  slantToLeg <- array(NA, dim = c(nrow(trajectoryCoords), numLegs))

  for (legIdx in seq_len(numLegs)) {
    # For each pair of adjascent waypoints, calculate the horizontal distance
    # from the great circle defined by the pair and all points in the
    # trajectory.
    distanceToLeg[, i, 1] <- geosphere::dist2gc(path[i, c(1,2)],
                                                path[i+1, c(1,2)],
                                                trajectory[, c(1,2)],
                                                r = 2.0904e+7)

    # If the waypoints are at the same altitude, just calculate the deviation
    # from this altitude. Easy.
    # XXX - Not actually handling the case where they don't match.
    if (!isTRUE(all.equal(altitudes(path[i]), altitudes(path[i+1])))) {
      warning("Pretending that waypoint altitudes match when they don't")
    }
    distanceToLeg[, i, 2] <- trajectory[, 3] - path[i, 3]

    # Squared euclidean distance
    slantToLeg[, i] <- distanceToLeg[, i, 1]^2 + distanceToLeg[, i, 2]^2
  }

  # Figure out which leg is closer to each point in the trajectory.
  # Note: I can imagine a tortuous path that would result in the closest path
  # alternating between legs. This would need to be rewritten to handle that.
  closestLeg <- apply(slantToLeg, 1, which.min)

  # Return the horizontal and vertical distance to the flight path (distance to
  # the closest leg) as a data.frame.
  distanceToPath <- as.data.frame(drop(distanceToLeg[, closestLeg, ]))
  colnames(distanceToPath) <- c("horizontal", "vertical")
  return(distanceToPath)
}
