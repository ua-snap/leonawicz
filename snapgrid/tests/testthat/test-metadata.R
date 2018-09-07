context("metadata")

x <- snapgrids()

test_that("metadata returned as expected", {
  expect_is(x, "tbl_df")
  expect_identical(dim(x), c(7L, 4L))
  expect_true(all(x$res %in% c(1000, 2000)))
})
