% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseCoordinates.R
\name{parseCoordinates}
\alias{parseCoordinates}
\title{Parse funky lat/long representations.}
\usage{
parseCoordinates(coord)
}
\arguments{
\item{coord}{A character vector representing latitude or longitude data.}
}
\value{
A numeric vector of (decimal) coordinate info, negative for west and
  south coordinates.
}
\description{
Parse funky lat/long representations.
}
\details{
This currently only handles coordinates in the format DD-DD-DDL.
}

