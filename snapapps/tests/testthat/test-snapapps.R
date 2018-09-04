context("snapapps")

test_that("snapp returns expected errors", {
  err1 <- "Invalid app `id`. See `snapps` for available apps."
  err2 <- "`local_mode` must be 'normal' or 'showcase'."
  expect_error(snapp("a"), err1)
  expect_error(snapp("a", local_mode = "b"), err2)
  expect_error(snapp("a", source = "remote"), err1)
  expect_error(snapp("a", source = "remote", local_mode = "b"), err2)
})

test_that("app launch returns as expected", {
  expect_identical(snapp("rv", source = "remote"), NULL)
})

test_that("metadata returns as expected", {
  x <- snapps()
  y <- c("id", "name", "description", "published", "revised", "rating", "status", "aws", "url", "redirect")
  expect_is(x, "tbl_df")
  expect_equal(ncol(x), 10)
  expect_true(all(y %in% names(x)))
})
