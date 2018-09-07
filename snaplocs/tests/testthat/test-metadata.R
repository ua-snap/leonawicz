context("metadata")

test_that("metadata helpers return as expected", {
  x <- c("Fairbanks", "Calgary", "Vancouver", "Whitehorse")
  grp <- c("Alaska", "Alberta", "British Columbia", "Yukon")
  country <- c("United States", rep("Canada", 3))
  purrr::walk2(x, grp, ~expect_identical(get_state(.x), .y))
  purrr::walk2(x, country, ~expect_identical(get_country(.x), .y))
  purrr::walk(x, ~expect_is(get_coords(.x), "tbl_df"))
  purrr::walk(x, ~expect_identical(dim(get_coords(.x)), c(1L, 2L)))

  x <- c("Fairbanks", "Calgary", "Vancouver", "Whitehorse")
  grp <- c("Alaska", "Alberta", "British Columbia", "Yukon")
  country <- c("United States", rep("Canada", 3))
  expect_identical(get_province(x), grp)
  expect_identical(get_country(x), country)
  expect_is(get_coords(x), "tbl_df")
  expect_identical(dim(get_coords(x)), c(length(x), 2L))

  x <- "A"
  err <- paste0("'", x, "' is not an available location in `locs`.")
  expect_error(get_state(x), err)
  expect_error(get_country(x), err)
  expect_error(get_coords(x), err)

  x <- c("Fairbanks", "A")
  err <- paste0("At least one location is not an available location in `locs`.")
  expect_error(get_state(x), err)
  expect_error(get_country(x), err)
  expect_error(get_coords(x), err)
})

test_that("metadata group argument works", {
  x <- rep(c("Fairbanks", "Galena"), 2)
  grp <- rep(c("Alaska", "British Columbia"), each = 2)
  country <- rep(c("United States", "Canada"), each = 2)
  good <- c(1:2, 4)
  bad <- 3
  purrr::walk2(x[good], grp[good], ~expect_identical(get_state(.x, .y), .y))
  purrr::walk(good, ~expect_identical(get_country(x[.x], grp[.x]), country[.x]))
  purrr::walk(x[good], ~expect_is(get_coords(.x, .y), "tbl_df"))
  purrr::walk2(x[good], grp[good], ~expect_identical(dim(get_coords(.x, .y)), c(1L, 2L)))

  x <- "Fairbanks"
  grp <- "British Columbia"
  err <- paste0("'", x, "' is not an available location in `locs`.")
  expect_error(get_state(x, grp), err)
  expect_error(get_country(x, grp), err)
  expect_error(get_coords(x, grp), err)

  x <- c("Fairbanks")
  grp <- "A"
  err <- "Invalid `group`."
  expect_error(get_state(x, grp), err)
  expect_error(get_country(x, grp), err)
  expect_error(get_coords(x, grp), err)
})
