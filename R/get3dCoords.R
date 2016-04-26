#' Checks for appropriate input and coerces it into a nx3 coordinate matrix.
#'
#' @param obj An object that hopefully contains some coordinates.
#' @return A nx3 coordinate matrix. Should be longitude/latitude/altitude.
get3dCoords <- function(obj) {
  # Get/check for a matrix representation
  if (is(obj, "SpatialPoints")) {
    if (!requireNamespace("sp", quietly = TRUE)) {
      # I have no idea how this would happen, but people do funky things.
      stop("Package sp must be installed to handle SpatialPoints objects")
    }
    # TODO: check projection string to make sure this is latitude/longitude and
    # not easting/northing data.
    warning("Assuming lat/long data in SpatialPoints object's coordinates")
    coordMat <- sp::coordinates(obj)
  } else if (is.data.frame(obj)) {
    coordMat <- as.matrix.data.frame(obj)
  } else if (is.matrix(obj)) {
    coordMat <- obj
  } else {
    stop("obj must be an object of class SpatialPoints (or subclass), data.frame, or matrix")
  }

  # Check the dimensions, adding altitude if necessary
  if (ncol(coordMat) == 2) {
    # Add fake altitude info (AGL)
    coordMat <- cbind(coordMat, 0)
  } else if (ncol(coordMat) != 3) {
    stop("Coordinates must be in 2D or 3D")
  }

  return(coordMat)
}
