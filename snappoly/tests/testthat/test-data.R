context("data")

test_that("Confirm SPDF class and identical projections", {
  cl <- "SpatialPolygonsDataFrame"
  x <- list(alaska, canada, ecoreg, aklcc, lcc, cavm, fmz, tpa)
  proj4 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0" # nolint
  purrr::map(x, ~expect_is(.x, cl))
  purrr::map(x, ~expect_identical(raster::projection(.x), proj4))
})
