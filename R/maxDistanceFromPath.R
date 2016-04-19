#' Find the maximum distance of a flight trajectory from a flight path.
#'
#' @param trajectory A matrix or SpatialPoints object indicating the trajectory
#'   of an aircraft.
#' @param path A matrix or SpatialPoints object indicating the ordered waypoints
#'   a pre-defined flight path.
#' @return A named vector indicating the horizontal and vertical distance from
#'   the fligh path at which the aircraft reached the maximum slant (euclidean)
#'   distance.
#'
#' @export
maxDistanceFromPath <- function(trajectory, path) {
  distanceFromPathVals <- distanceFromPath(trajectory, path)
  slantDistanceFromPath <- distanceFromPathVals$horizontal^2 +
    distanceFromPathVals$vertical^2
  farthestPoint <- which.max(slantDistanceFromPath)
  return(unlist(distanceFromPathVals[farthestPoint, ]))
}
