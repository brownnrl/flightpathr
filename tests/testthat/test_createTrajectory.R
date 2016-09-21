library(flightpathr)
context("createTrajectory")

library(geosphere)

# Find the lon/lat of the flight path between KACY and K17N flying at 100 knots
kacy <- c(-74.5771667, 39.4575833)
k17n <- c(-75.0330031, 39.7054758)
distMeters <- distHaversine(kacy, k17n)
# distMeters m * (1 hr / 100 nmi) * (1 nmi / 1852 m) * (3600 s / 1 hr)
timeSec <- round(distMeters * (1/100) * (3600/1852))
trajectoryMat <- gcIntermediate(kacy, k17n, n = timeSec-2, addStartEnd = TRUE)
trajectory <- createTrajectory(trajectoryMat[, "lon"],
                               trajectoryMat[, "lat"],
                               rep(2500, timeSec))

test_that("Passed arguments are stored correctly", {
  expect_equal(trajectory$longitude, trajectoryMat[, "lon"])
  expect_equal(trajectory$latitude, trajectoryMat[, "lat"])
  expect_equal(trajectory$altitude, rep(2500, timeSec))
})

test_that("Bearing and groundspeed calculations are correct", {
  expect_equal(trajectory$timestamp, seq(1, timeSec))
  expect_equal(trajectory$groundspeed, rep(100, timeSec), tolerance = 0.5)
  expect_equal(trajectory$bearing[1:(timeSec-1)],
               bearing(trajectoryMat[1:(timeSec-1), ], trajectoryMat[2:timeSec, ]))
})
