context("data")

test_that("Confirm SPDF class and identical projections", {
  cl <- "SpatialPolygonsDataFrame"
  x <- list(akcan, akcan2, ecoreg1, ecoreg2, ecoreg3, aklcc, lcc, cavm, fmz, tpa)
  proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  purrr::map(x, ~expect_is(.x, cl))
  purrr::map(x, ~expect_identical(raster::projection(.x), proj4))
})
