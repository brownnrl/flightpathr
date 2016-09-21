#' Create a flightpath object from coordinate info.
#'
#' @param longitude Required; numeric vector giving aircraft longitude in
#'   degrees.
#' @param latitude Required; numeric vector giving aircraft latitude in degrees.
#' @param altitude Optional; numeric vector giving aircraft altitude (AGL) in
#'   feet.
#' @return A flightpath object encapsulating these parameters (with default
#'   values substituded as necessary).
#'
#' @details \code{longitude} and \code{latitude} must be the same length.
#'   \code{altitude} must also have a length equal to these parameters or be
#'   scalar. If \code{altitude} is \code{NA}, operations that compare values to
#'   flightpaths will be affected.
#'
#' @export
createPath <- function(longitude, latitude, altitude = NA) {
  if (!is.numeric(longitude)) stop("\"longitude\" must be a numeric vector")
  nCoord <- length(longitude)

  # Check latitude type and length
  if (!is.numeric(latitude)) stop("\"latitude\" must be a numeric vector")
  if (length(latitude) != nCoord) stop("\"latitude\" and \"longitude\" length mismatch")

  # If altitude is NA or scalar, coerce its length. Otherwise, raise an error
  if (isTRUE(all(is.na(altitude)))) {
    altitude <- rep(NA, nCoord)
  } else if (length(altitude) == 1) {
    altitude <- rep(altitude, nCoord)
  } else {
    if (!is.numeric(altitude)) stop("\"altitude\" must be NA or a numeric vector")
    if (length(altitude) != nCoord) stop("\"altitude\" has incorrect length")
  }

  flightpath <- list(longitude = longitude,
                     latitude = latitude,
                     altitude = altitude)
  class(flightpath) <- "flightpath"

  return(flightpath)
}

#' Check if an object is a \code{flightpath}
#' @export
is.flightpath <- function(x) inherits(x, "flightpath")
