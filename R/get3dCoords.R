#' Checks for a \code{flighttrajectory} or \code{flightpath} object and returns
#' it's position in a 3D matrix.
#'
#' @param coordObject An object that hopefully contains some coordinates.
#' @return A nx3 coordinate matrix giving longitude/latitude/altitude.
#'
#' @details It'll be safer to access \code{flighttrajectory} and
#'   \code{flightpath} objects' coordinates through this.
get3dCoords <- function(coordObject) {
  if (!is.flighttrajectory(coordObject) & !is.flightpath(coordObject)) {
    stop("trajectory must be an instance of 'flighttrajectory' or 'flightpath'")
  }

  return(cbind(coordObject$longitude,
               coordObject$latitude,
               coordObject$altitude))
}
