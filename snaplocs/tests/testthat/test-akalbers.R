context("akalbers")

test_that("transformations return as expected", {
  x <- "Fairbanks"
  x1 <- wgs2ak(get_coords(x))
  x2 <- wgs2ak(as.matrix(get_coords(x)))
  expect_is(x1, "matrix")
  expect_is(x2, "matrix")
  expect_identical(x1, x2)
  expect_identical(dim(x1), c(1L, 2L))
  expect_equal(dim(wgs2ak(locs[3:4])), c(nrow(locs), 2))
})
