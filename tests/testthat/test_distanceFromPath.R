library(flightpathr)
context("distanceFromPath")

distancePrecision <- 10
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

# Flying from 17N to KACY with a stop over N81. Flying VFR at 3500 msl
path <- matrix(c(-75.0268, 39.7065,
                 -74.7577, 39.6675,
                 -74.5722, 39.4513),
               nrow = 3, byrow = TRUE,
               dimnames = list(c("17N", "N81", "KACY"),
                               c("lon", "lat")))
trajectory <- fakeTrajectory(path)

test_that("non-deviating paths have small distances for all input types", {
  expect_true(all(distanceFromPath(trajectory, path) < distancePrecision))
  expect_true(all(distanceFromPath(cbind(trajectory, 3500), cbind(path, 3500)) < distancePrecision))
  expect_true(all(distanceFromPath(sp::SpatialPoints(trajectory), sp::SpatialPoints(path)) < distancePrecision))
  expect_true(all(distanceFromPath(as.data.frame(trajectory), as.data.frame(path)) < distancePrecision))
})

test_that("small deviations look OK", {
  flownPath <- rbind(path[1:2, ],
                     KORDE = c(-74.0948, 39.0976),
                     path[3, , drop = FALSE])
  flownTrajectory <- fakeTrajectory(flownPath)
  trajectoryDistance <- distanceFromPath(flownTrajectory, path)$horizontal
  farthestPoint <- which.max(abs(trajectoryDistance))

  expect_equal(farthestPoint, numPoints*2+3)

  expect_lt(abs(maxDistanceFromPath(flownTrajectory, path)["horizontal"] - -42015.6),
            distancePrecision)
  expect_lt(abs(maxDistanceFromPath(flownTrajectory, flownPath)["horizontal"] - 0),
            distancePrecision)
})

test_that("simple altitude deviation is handled", {
  flownPath1 <- cbind(path, alt = 3500)
  flownPath2 <- cbind(path, alt = c(3500, 4500, 3500))
  flownPath3 <- cbind(path, alt = c(3500, 5500, 3500))
  flownTrajectory <- cbind(fakeTrajectory(path),
                           alt = c(seq(3500, 5500, length.out = numPoints+2),
                                   seq(5500, 3500,
                                       length.out = nrow(trajectory)-(numPoints+2))))

  expect_lt(abs(maxDistanceFromPath(flownTrajectory, flownPath1)["vertical"] - 2000),
            distancePrecision)
  expect_lt(abs(maxDistanceFromPath(flownTrajectory, flownPath2)["vertical"] - 1000),
            distancePrecision)
  expect_lt(abs(maxDistanceFromPath(flownTrajectory, flownPath3)["vertical"] - 0000),
            distancePrecision)
})

test_that("reproducing geosphere vignette example", {
  LA <- c(-118.40, 33.95)
  NY <- c(-73.78, 40.63)
  MS <- c(-93.26, 44.98)
  plannedPath <- rbind(LA, NY)
  flownTrajectory <- fakeTrajectory(rbind(LA, MS, NY), n = 1000)
  feetToMeters <- 0.3048

  expect_lt(abs(maxDistanceFromPath(flownTrajectory, plannedPath)["horizontal"]*feetToMeters -
                  -547448.8),
            1)
})
