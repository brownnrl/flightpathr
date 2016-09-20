#' Calculate the bearing between successive points in a trajectory.
#'
#' @param trajectory A matrix or SpatialPoints object indicating the trajectory
#'   of an aircraft.
#' @return The numeric vecotor giving the instantaneious bearing at each point
#'   along the trajectory, in degrees. This will have the same length as the
#'   trajectory, but the last element will be \code{NA}.
#'
#' @export
coordsToBearing <- function(trajectory) {
  trajectoryCoords <- get3dCoords(trajectory)
  numPoints <- nrow(trajectoryCoords)
  if (numPoints < 2) {
    stop("At least two time points must be specified")
  }

  bearings <- geosphere::bearing(trajectoryCoords[1:(numPoints-1), 1:2],
                                 trajectoryCoords[2:numPoints, 1:2])

  # If successive points have the SAME lon/lat, geosphere::bearing() produces
  # meaningless output. Set these to NAs.
  distancesMeters <- geosphere::distHaversine(trajectoryCoords[1:(numPoints-1), 1:2],
                                              trajectoryCoords[2:numPoints, 1:2])
  bearings[distancesMeters < 1e-4] <- NA

  return(c(bearings, NA))
}

#' Calculate the signed difference between angles (in degrees)
#'
#' @param angle1 Source angle
#' @param angle2 Target angle
#' @return The signed difference between the two angles
angleDiff <- function(angle1, angle2) {
  return(((angle2 - angle1) + 180) %% 360 - 180)
}
