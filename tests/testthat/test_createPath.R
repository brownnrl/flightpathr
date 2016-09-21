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
path <- createPath(pathMat[, "lon"], pathMat[, "lat"])

test_that("Simple flightpath creation works", {
  expect_equal(path$longitude, pathMat[, "lon"])
  expect_equal(path$latitude, pathMat[, "lat"])
})

test_that("Longitude and latitude checking works", {
  expect_error(createPath(paste(pathMat[, "lon"]), pathMat[, "lat"]),
               "\"longitude\" must be a numeric vector")
  expect_error(createPath(pathMat[, "lon"], paste(pathMat[, "lat"])),
               "\"latitude\" must be a numeric vector")
  expect_error(createPath(pathMat[, "lon"], pathMat[1:2, "lat"]),
               "\"latitude\" and \"longitude\" length mismatch")

})

test_that("Altitudes are handled appropriately", {
  altitude <- runif(nrow(pathMat), 2500, 4500)
  expect_equal(createPath(pathMat[, "lon"], pathMat[, "lat"], altitude)$altitude,
               altitude)
  expect_equal(createPath(pathMat[, "lon"], pathMat[, "lat"], altitude[1])$altitude,
               rep(altitude[1], nrow(pathMat)))
  expect_equal(createPath(pathMat[, "lon"], pathMat[, "lat"])$altitude,
               rep(NA, nrow(pathMat)))

  expect_error(createPath(pathMat[, "lon"], pathMat[, "lat"], altitude[1:2]),
               "\"altitude\" has incorrect length")
  expect_error(createPath(pathMat[, "lon"], pathMat[, "lat"], paste(altitude)),
               "\"altitude\" must be NA or a numeric vector")
})

test_that("Trajectories are identified", {
  expect_true(is.flightpath(path))
  expect_false(is.flightpath(pathMat))
})
