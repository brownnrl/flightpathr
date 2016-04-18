library(flightpathr)
context("distanceFromPath")

# Flying from 17N to KACY with a stop over N81. Flying VFR at 3500 msl

path <- matrix(c(-75.0268, 39.7065,
                 -74.7577, 39.6675,
                 -74.5722, 39.4513),
               nrow = 3, byrow = TRUE,
               dimnames = list(c("17N", "N81", "KACY"),
                               c("lon", "lat")))
trajectory <- rbind(geosphere::gcIntermediate(path[1, ], path[2, ], 5),
                    geosphere::gcIntermediate(path[2, ], path[3, ], 6))

test_that("non-deviating paths have small distances", {
  expect_true(all(distanceFromPath(trajectory, path) < 1))
})
