library(flightpathr)
context("get3dCoords")

library(geosphere)

kacy <- c(-74.5771667, 39.4575833)
k17n <- c(-75.0330031, 39.7054758)
vcn <- c(-74.9671439, 39.5376711)

expect_unnamed_equal <- function(mat1, mat2)  {
  eval(bquote(expect_equal(unname(.(mat1)), unname(.(mat2)))))
}

test_that("flighttrajectories are recognized", {
  distMeters <- distHaversine(kacy, k17n)
  # distMeters m * (1 hr / 100 nmi) * (1 nmi / 1852 m) * (3600 s / 1 hr)
  timeSec <- round(distMeters * (1/100) * (3600/1852))
  trajectoryMat <- cbind(gcIntermediate(kacy, k17n, n = timeSec-2, addStartEnd = TRUE),
                         alt = rep(2500, timeSec))
  trajectory <- createTrajectory(trajectoryMat[, "lon"],
                                 trajectoryMat[, "lat"],
                                 trajectoryMat[, "alt"])

  expect_unnamed_equal(get3dCoords(trajectory), trajectoryMat)
})

test_that("flightpaths are recognized", {
  pathMat <- rbind(kacy, vcn, k17n)

  path1 <- createPath(pathMat[, 1], pathMat[, 2])
  path2 <- createPath(pathMat[, 1], pathMat[, 2], 7500)

  expect_unnamed_equal(get3dCoords(path1), cbind(pathMat, NA))
  expect_unnamed_equal(get3dCoords(path2), cbind(pathMat, 7500))
})
