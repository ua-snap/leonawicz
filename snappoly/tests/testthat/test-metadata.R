context("metadata")

x <- snappolys()
nam <- c("data", "domain", "features", "variables", "id", "multilevel", "description")

test_that("metadata returned as expected", {
  expect_is(x, "tbl_df")
  expect_identical(dim(x), c(8L, 7L))
  expect_identical(names(x), nam)
  expect_is(x$features, "integer")
  expect_is(x$variables, "integer")
  expect_is(x$multilevel, "logical")
  expect_is(x$id, "character")
  expect_identical(x$id[1], as.character(NA))
  expect_true(all(x$domain %in% c("ak", "akcan")))
})
