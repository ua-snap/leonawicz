context("create")


test_that("create fucntions return as expected", {
  expect_is(pkg_authors(), "list")
  expect_identical(names(pkg_authors()), "Authors@R")
  expect_identical(pkg_cph(), "Scenarios Network for Alaska and Arctic Planning")
})
