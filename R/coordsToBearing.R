#' Calculate the bearing between successive longitude/latitude points.
#'
#' @param longitude Latitude values.
#' @param latitude Longitude values.
#' @return The numeric vecotor giving the instantaneious bearing at each point
#'   along the trajectory, in degrees. This will have the same length as the
#'   trajectory, but the last element will be \code{NA}.
#'
#' @export
coordsToBearing <- function(longitude, latitude) {
  trajectoryCoords <- cbind(longitude, latitude)
  numPoints <- nrow(trajectoryCoords)

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
