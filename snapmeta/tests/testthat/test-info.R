context("info")


test_that("info functions return as expected", {
  wd <- getwd()
  setwd("../../")
  x <- sv_local_pkgs()
  y <- sv_local_pkgs(self = FALSE)
  setwd(wd)

  expect_is(x, "character")
  expect_is(y, "character")
  expect_equal(length(x), length(y) + 1)

  expect_is(sv_pkgs(), "tbl_df")
  expect_identical(dim(sv_pkgs()), c(25L, 3L))
})
