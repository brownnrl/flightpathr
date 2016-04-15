library(flightpathr)
context("get3dCoords")


# data and helper functions -----------------------------------------------

coords3d <- matrix(rnorm(100*3), ncol = 3)

check_sp <- function () {
  if (!requireNamespace("sp", quietly = TRUE)) {
    skip("sp is not available")
  }
}

expect_unnamed_equal <- function(mat1, mat2)  {
  eval(bquote(expect_equal(unname(.(mat1)), unname(.(mat2)))))
}


# tests -------------------------------------------------------------------

test_that("different input objects are handled", {
  expect_equal(get3dCoords(coords3d), coords3d)
  expect_unnamed_equal(get3dCoords(as.data.frame(coords3d)), coords3d)
  expect_error(get3dCoords(as.numeric(coords3d)))
})

test_that("different dimensions are handled", {
  coords2d <- coords3d[, c(1,2)]
  coords2dFixed <- cbind(coords2d, 0)

  expect_equal(get3dCoords(coords2d), coords2dFixed)
  expect_error(get3dCoords(coords3d[, 1, drop = FALSE]),
               "Coordinates must be in 2D or 3D")
})

test_that("SpatialPoints are handled", {
  check_sp()
  coordsSP <- sp::SpatialPoints(coords3d)
  coordsSPDF <- sp::SpatialPointsDataFrame(coords3d, as.data.frame(coords3d))

  expect_warning(get3dCoords(coordsSP))
  expect_unnamed_equal(get3dCoords(coordsSP), coords3d)
  expect_unnamed_equal(get3dCoords(coordsSPDF), coords3d)
})

