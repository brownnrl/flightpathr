library(flightpathr)
context("createPath")

library(geosphere)

# Data for a flight path
pathMat <- matrix(c(-74.5771667, 39.4575833,
                    -74.9671439, 39.5376711,
                    -75.0330031, 39.7054758),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(c("KACY", "VCN", "K17N"),
                                  c("lon", "lat")))

test_that("Handling paths with no altitude", {
  path <- createPath(pathMat[, "lon"], pathMat[, "lat"])
  expect_equal(path$longitude, pathMat[, "lon"])
  expect_equal(path$latitude, pathMat[, "lat"])
  expect_equal(path$altitude, rep(NA, nrow(pathMat)))
})

# test_that("Bearing and groundspeed calculations are correct", {
#   expect_equal(trajectory$timestamp, seq(1, timeSec))
#   expect_equal(trajectory$groundspeed, rep(100, timeSec), tolerance = 0.5)
#   expect_equal(trajectory$bearing[1:(timeSec-1)],
#                bearing(trajectoryMat[1:(timeSec-1), ], trajectoryMat[2:timeSec, ]))
# })
#
# test_that("Input types are checked", {
#   expect_error(createTrajectory(paste(trajectoryMat[, "lon"]), trajectoryMat[, "lat"]),
#                "\"longitude\" must be a numeric vector")
#   expect_error(createTrajectory(trajectoryMat[, "lon"], paste(trajectoryMat[, "lat"])),
#                "\"latitude\" must be a numeric vector")
#   expect_error(createTrajectory(trajectoryMat[, "lon"], trajectoryMat[, "lat"], paste(rep(2500, timeSec))),
#                "\"altitude\" must be a numeric vector")
# })
#
# test_that("Input lengths are checked", {
#   expect_error(createTrajectory(trajectoryMat[, "lon"], trajectoryMat[1, "lat"]),
#                paste("Vector \"latitude\" has length = 1, expected length =", timeSec))
#   # Altitude can have length of 1 or timeSec, otherwise there should be an error
#   expect_error(createTrajectory(trajectoryMat[, "lon"], trajectoryMat[, "lat"], rep(2500, 2)),
#                paste("Vector \"altitude\" has length = 2, expected length =", timeSec))
#   expect_equal(createTrajectory(trajectoryMat[, "lon"], trajectoryMat[, "lat"], 2500)$altitude,
#                rep(2500, timeSec))
# })
