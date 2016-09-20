library(flightpathr)
context("coordsToBearing")

test_that("Input checking works", {
  expect_error(coordsToBearing(matrix(c(-74.577166, 39.457583), nrow = 1)))
})

test_that("Equatorial routes have constant bearing", {
  trajectory1 <- cbind(longitude = seq(-180, 180, length.out = 100),
                       latitude = 0)
  trajectory2 <- cbind(longitude = -74.6,
                       latitude = seq(89, -89, length.out = 100))

  expect_equal(angleDiff(coordsToBearing(trajectory1), rep(90, 100)),
               c(rep(0, 99), NA))
  expect_equal(angleDiff(coordsToBearing(trajectory2), rep(180, 100)),
               c(rep(0, 99), NA))
})

test_that("Non-equatorial routes look OK", {
  trajectory <- geosphere::gcIntermediate(c(-119.841499, 34.426194),
                                          c(-74.577166, 39.457583),
                                          100)

  expect_equal(coordsToBearing(trajectory)[1],
               geosphere::bearing(trajectory[1, ], trajectory[2, ]))
  expect_equal(coordsToBearing(trajectory)[99],
               geosphere::bearing(trajectory[99, ], trajectory[100, ]))
  expect_true(is.na(coordsToBearing(trajectory)[100]))
})

test_that("Trajectories are somewhat reversable", {
  trajectory <- geosphere::destPoint(c(-119.841499, 34.426194), b = 68.4,
                                     d = seq(0, by = 40281.88, length.out = 100))
  bearings <- coordsToBearing(trajectory)
  destPoints <- matrix(NA, nrow = 100, ncol = 2)
  destPoints[1, ] <- trajectory[1, ]
  for (r in seq(2,100)) {
    destPoints[r, ] <- geosphere::destPoint(trajectory[r-1, ], bearings[r-1],
                                            40281.88)
  }

  expect_equal(destPoints[1:200], trajectory[1:200], tolerance = 1e-8)
})

test_that("Repeated long/lats are handled correctly", {
  trajectory <- geosphere::gcIntermediate(c(-119.841499, 34.426194),
                                          c(-74.577166, 39.457583),
                                          100)
  trajectory <- rbind(trajectory, trajectory[100, ], trajectory[100, ])
  expect_true(all(is.na(coordsToBearing(trajectory)[c(100, 101)])))
})
