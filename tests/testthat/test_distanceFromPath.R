library(flightpathr)
context("distanceFromPath")

# Flying from 17N to KACY with a stop over N81. Flying VFR at 3500 msl

path <- matrix(c(-75.0268, 39.7065,
                 -74.7577, 39.6675,
                 -74.5722, 39.4513),
               nrow = 3, byrow = TRUE,
               dimnames = list(c("17N", "N81", "KACY"),
                               c("lon", "lat")))

numPoints <- 5

fakeTrajectory <- function(waypoints, n = numPoints) {
  trajectoryList <- vector("list", 2*nrow(waypoints)-1)
  trajectoryList[[1]] <- waypoints[1, ]
  for (i in seq(2, nrow(waypoints))) {
    trajectoryList[[(i-1)*2]] <- geosphere::gcIntermediate(waypoints[i-1, ],
                                                           waypoints[i, ], n)
    trajectoryList[[(i-1)*2+1]] <- waypoints[i, ]
  }
  return(do.call(rbind, trajectoryList))
}

trajectory <- fakeTrajectory(path)

test_that("non-deviating paths have small distances for all input types", {
  expect_true(all(distanceFromPath(trajectory, path) < 1))
  expect_true(all(distanceFromPath(cbind(trajectory, 3500), cbind(path, 3500)) < 1))
  expect_true(all(distanceFromPath(sp::SpatialPoints(trajectory), sp::SpatialPoints(path)) < 1))
  expect_true(all(distanceFromPath(as.data.frame(trajectory), as.data.frame(path)) < 1))
})

test_that("small deviations look OK", {
  flownPath <- rbind(path[1:2, ],
                     KORDE = c(-74.0948, 39.0976),
                     path[3, , drop = FALSE])
  flownTrajectory <- fakeTrajectory(flownPath)
  trajectoryDistance <- distanceFromPath(flownTrajectory, path)$horizontal
  farthestPoint <- which.max(abs(trajectoryDistance))

  expect_equal(farthestPoint, numPoints*2+3)
  # left of course
  expect_lt(trajectoryDistance[farthestPoint], 0)
})
