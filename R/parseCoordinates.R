#' Parse funky lat/long representations.
#'
#' @param coord A character vector representing latitude or longitude data.
#' @return A numeric vector of (decimal) coordinate info, negative for west and
#'   south coordinates.
#'
#' @details This currently only handles coordinates in the format DD-DD-DDL.
#'
#' @export
parseCoordinates <- function(coord) {
  if (!is.character(coord)) {
    stop("Only dealing with character vectors.")
  }

  # Get the direction
  direction <- gsub("[^NSEWnsew]", "", coord)
  direction[is.na(direction) | nchar(direction) == 0] <- "1"
  direction <- unname(vapply(direction, switch, numeric(1), S = -1, E = -1, 1))

  # Continue parsing the coord
  coord <- gsub("[^[:digit:][:punct:]]", "", coord)
  combineParts <- function(x) {
    n <- as.numeric(x)
    if (length(n) != 3) {
      d <- NA
    } else {
      d <- n[1]+n[2]/60+n[3]/3600
    }
    return(d)
  }
  coord <- direction * vapply(strsplit(coord, "[[:punct:]]"),
                              combineParts, numeric(1))
  return(coord)
}
