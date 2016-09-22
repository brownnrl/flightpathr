library(flightpathr)
context("distanceFromPath")

numPoints <- 5

fakeTrajectory <- function(waypoints, n = numPoints) {
  coordList <- vector("list", 2*nrow(waypoints)-1)
  coordList[[1]] <- waypoints[1, ]
  for (i in seq(2, nrow(waypoints))) {
    coordList[[(i-1)*2]] <- geosphere::gcIntermediate(waypoints[i-1, ],
                                                      waypoints[i, ], n)
    coordList[[(i-1)*2+1]] <- waypoints[i, ]
  }
  coordMat <- do.call(rbind, coordList)
  return(createTrajectory(longitude = coordMat[, "lon"],
                          latitude = coordMat[, "lat"],
                          altitude = 0))
}

# Flying from 17N to KACY with a stop over N81. Flying VFR at 3500 msl
pathMat <- matrix(c(-75.0268, 39.7065,
                    -74.7577, 39.6675,
                    -74.5722, 39.4513),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(c("17N", "N81", "KACY"),
                                  c("lon", "lat")))
path <- createPath(longitude = pathMat[, "lon"], latitude = pathMat[, "lat"])
trajectory <- fakeTrajectory(pathMat)
trajectoryLength <- length(trajectory$longitude)

test_that("non-deviating paths have small distances for all input types", {
  expect_equal(distanceFromPath(trajectory, path)$horizontal,
               rep(0, trajectoryLength),
               tolerance = 1)
  # Leaving these here but commented-out. I'd like to be able to accept multiple
  # input types again later, but it's a low priority.

  # expect_true(all(distanceFromPath(cbind(trajectory, 3500), cbind(path, 3500)) < distancePrecision))
  # expect_true(all(distanceFromPath(sp::SpatialPoints(trajectory), sp::SpatialPoints(path)) < distancePrecision))
  # expect_true(all(distanceFromPath(as.data.frame(trajectory), as.data.frame(path)) < distancePrecision))
})

test_that("small horizontal deviations look OK", {
  flownPathMat <- rbind(pathMat[1:2, ],
                        KORDE = c(-74.0948, 39.0976),
                        pathMat[3, , drop = FALSE])
  flownTrajectory <- fakeTrajectory(flownPathMat)
  flownPath <- createPath(flownPathMat[, 1], flownPathMat[, 2])
  trajectoryDistance <- distanceFromPath(flownTrajectory, path)$horizontal
  farthestPoint <- which.max(abs(trajectoryDistance))

  expect_equal(farthestPoint, numPoints*2+3)
  expect_equal(maxDistanceFromPath(flownTrajectory, path)["horizontal"],
               c(horizontal = 42015.6),
               tolerance = 1)
  expect_equal(maxDistanceFromPath(flownTrajectory, flownPath)["horizontal"],
               c(horizontal = 0),
               tolerance = 1)
})

test_that("simple altitude deviation is handled", {
  flownPath1 <- createPath(pathMat[, "lon"], pathMat[, "lat"], 3500)
  flownPath2 <- createPath(pathMat[, "lon"], pathMat[, "lat"], c(3500, 4500, 3500))
  flownPath3 <- createPath(pathMat[, "lon"], pathMat[, "lat"], c(3500, 5500, 3500))
  flownPath4 <- createPath(pathMat[, "lon"], pathMat[, "lat"], c(3500, 5500, 5500))
  flownTrajectory <- createTrajectory(trajectory$longitude, trajectory$latitude,
                                      c(seq(3500, 5500,
                                            length.out = numPoints+2),
                                        seq(5500, 3500,
                                            length.out = length(trajectory$longitude)-(numPoints+2))))

  expect_equal(maxDistanceFromPath(flownTrajectory, flownPath1)["vertical"],
               c("vertical" = 2000), tolerance = 1)
  expect_equal(maxDistanceFromPath(flownTrajectory, flownPath2)["vertical"],
               c("vertical" = 1000), tolerance = 1)
  expect_equal(maxDistanceFromPath(flownTrajectory, flownPath3)["vertical"],
               c("vertical" = 0000), tolerance = 1)
  expect_equal(maxDistanceFromPath(flownTrajectory, flownPath4)["vertical"],
               c("vertical" = -2000), tolerance = 1)
})

test_that("reproducing geosphere vignette example", {
  LA <- c(-118.40, 33.95)
  NY <- c(-73.78, 40.63)
  MS <- c(-93.26, 44.98)
  flownTrajectory <- fakeTrajectory(rbind(LA, MS, NY), n = 1000)
  plannedPathMat <- rbind(LA, NY)
  plannedPath <- createPath(plannedPathMat[, 1], plannedPathMat[, 2], 0)
  feetToMeters <- 0.3048

  expect_equal(maxDistanceFromPath(flownTrajectory, plannedPath)["horizontal"]*feetToMeters,
               c("horizontal" = 547448.8),
               tolerance = 1)
})
