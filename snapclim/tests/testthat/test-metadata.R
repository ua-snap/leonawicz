context("metadata")

test_that("metadata returned as expected", {
  x <- climate_locations("all")
  expect_is(x, "tbl_df")
  expect_identical(dim(x), c(3953L, 2L))
  expect_identical(dim(climate_locations("region")), c(86L, 2L))
  expect_identical(dim(climate_locations("point")), c(3867L, 2L))
  expect_identical(names(x), c("Location", "Group"))

  expect_is(location_sets("region"), "character")
  expect_is(location_sets("point"), "character")
  expect_equal(length(location_sets("region")), 10)
  expect_equal(length(location_sets("point")), 6)

  expect_identical(dim(climate_collections()), c(1L, 11L))
})
