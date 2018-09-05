context("climdata")

test_that("climdata returns as expected", {
  x1 <- climdata("ar5stats", "AK-CAN")
  x2 <- climdata("ar5stats", "Fairbanks")

  expect_equal(ncol(x1), 20)
  expect_equal(ncol(x2), 8)
  expect_true("Month" %in% names(x1))

  x1b <- climdata("ar5stats", "AK-CAN", "AK-CAN")
  expect_identical(x1, x1b)

  x1c <- climdata("ar5stats", "AK-CAN", time_scale = "seasonal")
  expect_equal(ncol(x1c), 20)
  expect_true("Season" %in% names(x1c))
  x1d <- climdata("ar5stats", "AK-CAN", time_scale = "annual")
  expect_equal(ncol(x1c), 20)
  expect_true("Season" %in% names(x1d))

  expect_identical(levels(x1$Month), month.abb)
  expect_identical(levels(x1c$Season), c("Winter", "Spring", "Summer", "Autumn"))
  expect_identical(levels(x1d$Season), "Annual")

  x3 <- climdata("ar5stats", "AK-CAN", time_scale = "monthly", decavg = TRUE)
  x3b <- climdata("ar5stats", "AK-CAN", time_scale = "seasonal", decavg = TRUE)
  x3c <- climdata("ar5stats", "AK-CAN", time_scale = "annual", decavg = TRUE)
  expect_equal(ncol(x3), 20)
  expect_equal(nrow(x3), 3 * nrow(x3b))
  expect_equal(nrow(x3), 12 * nrow(x3c))

  x4 <- climdata("ar5stats", "points", decavg = TRUE, variable = "pr")
  expect_equal(as.character(x4$Var[1]), "pr")
  expect_equal(length(unique(paste(x4$Group, x4$Location))), 3867)

  x2 <- climdata("ar5stats", "Galena", "Alaska")

  err <- "Invalid `id`. See `climate_collections`."
  expect_error(climdata("X", "Fairbanks"), err)
  err <- "Invalid `area`. See `climate_locations` for available regions and point locations."
  expect_error(climdata("ar5stats", "X"), err)
  err <- "Invalid `set`. See `location_sets` for available region and point location groups/sets."
  expect_error(climdata("ar5stats", "Fairbanks", "X"), err)
  warn <- "`area` not unique and `set` not provided. Assuming 'Alaska'. Please provide `set`."
  expect_warning(climdata("ar5stats", "Galena"), warn)
  err <- "Using area = 'points' requires decavg = TRUE."
  expect_error(climdata("ar5stats", "points"))
  msg <- "`variable` is NULL. Returning mean temperature: `tas`."
  expect_message(climdata("ar5stats", "points", time_scale = "seasonal", decavg = TRUE))
})
