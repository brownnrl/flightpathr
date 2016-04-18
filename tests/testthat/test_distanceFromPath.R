library(flightpathr)
context("distanceFromPath")

# Flying from 17N to KACY with a stop over N81. Flying VFR at 3500 msl

path <- matrix(c(-75.0268, 39.7065,
                 -74.7577, 39.6675,
                 -74.5722, 39.4513),
               nrow = 3, byrow = TRUE,
               dimnames = list(c("17N", "N81", "KACY"),
                               c("lon", "lat")))

fakeTrajectory <- function(waypoints) {
  trajectoryList <- vector("list", 2*nrow(waypoints)-1)
  trajectoryList[[1]] <- waypoints[1, ]
  for (i in seq(2, nrow(waypoints))) {
    trajectoryList[[(i-1)*2]] <- geosphere::gcIntermediate(path[i-1, ], path[i, ], 5)
    trajectoryList[[(i-1)*2+1]] <- waypoints[i, ]
  }
  return(do.call(rbind, trajectoryList))
}

trajectory <- fakeTrajectory(path)

test_that("non-deviating paths have small distances", {
  expect_true(all(distanceFromPath(trajectory, path) < 1))
  expect_true(all(distanceFromPath(sp::SpatialPoints(trajectory), sp::SpatialPoints(path)) < 1))
  expect_true(all(distanceFromPath(as.data.frame(trajectory), as.data.frame(path)) < 1))
})
