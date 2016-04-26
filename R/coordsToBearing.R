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
  numCoords <- nrow(trajectoryCoords)
  bearings <- geosphere::bearing(trajectoryCoords[1:(numCoords-1), 1:2],
                                 trajectoryCoords[2:numCoords, 1:2])
  return(c(bearings, NA))
}
