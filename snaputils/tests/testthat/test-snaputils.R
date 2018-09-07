context("snaputils")

test_that("resource paths are returned", {
  x <- snap_res()
  expect_is(x, "character")
  expect_identical(basename(x), "images")
})
