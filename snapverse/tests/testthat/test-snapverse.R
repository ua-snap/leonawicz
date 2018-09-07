context("snapverse")

library(snapverse)
x <- snapverse:::.onAttach()

test_that("loading and updating run", {
  expect_identical(snapverse_update(), NULL)
  expect_identical(x, NULL)
})
