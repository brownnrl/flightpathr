# Functions to coerce objects into instances of flightpath. Right now
# these are pretty dumb; in the future it would be smart to look for columns
# that look like, e.g., longitude, latitude, and altitude.

#' Attempt to coerce an object into an instance of \code{flightpath}.
#' @export
as.flightpath <- function(x) UseMethod("as.flightpath", x)

#' @method as.flightpath flightpath
#' @export
as.flightpath.flightpath <- function(x) {
  return(x)
}

#' @method as.flightpath data.frame
#' @export
as.flightpath.data.frame <- function(x) {
  return(do.call(createTrajectory, as.list(unname(x))))
}

#' @method as.flightpath matrix
#' @export
as.flightpath.matrix <- function(x) {
  return(as.flightpath.data.frame(as.data.frame(x)))
}

as.flightpath.SpatialPoints <- function(x) {
  return(as.flightpath.matrix(as.matrix(sp::coordinates(x))))
}
