# Functions to coerce objects into instances of flighttrajectory. Right now
# these are pretty dumb; in the future it would be smart to look for columns
# that look like, e.g., longitude, latitude, and altitude.

#' Attempt to coerce an object into an instance of \code{flighttrajectory}.
#' @export
as.flighttrajectory <- function(x) UseMethod("as.flighttrajectory", x)

#' @method as.flighttrajectory flighttrajectory
#' @export
as.flighttrajectory.flighttrajectory <- function(x) {
  return(x)
}

#' @method as.flighttrajectory data.frame
#' @export
as.flighttrajectory.data.frame <- function(x) {
  # TODO: Attempt to match columns to arguments
  return(do.call(createTrajectory, as.list(unname(x))))
}

#' @method as.flighttrajectory matrix
#' @export
as.flighttrajectory.matrix <- function(x) {
  return(as.flighttrajectory.data.frame(as.data.frame(x)))
}

as.flighttrajectory.SpatialPoints <- function(x) {
  return(as.flighttrajectory.matrix(as.matrix(sp::coordinates(x))))
}
