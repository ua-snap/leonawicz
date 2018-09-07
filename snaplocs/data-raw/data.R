library(dplyr)
load("data-raw/cc4lite_launch_cities_meta.RData")

loc <- purrr::map_chr(cities.meta$Location, ~{
  x <- strsplit(.x, ", ")[[1]]
  len <- length(x)
  paste0(x[1:(len - 1)], collapse = ", ")
})
grp <- purrr::map_chr(cities.meta$Location, ~strsplit(.x, ", ")[[1]] %>% tail(1))
lev <- c("Alaska", "Alberta", "British Columbia", "Manitoba", "Northwest Territories", "Saskatchewan", "Yukon")
locs <- as_data_frame(cities.meta) %>%
  mutate(Location = loc, Group = factor(grp, levels = lev)) %>%
  rename(lon = Lon, lat = Lat) %>% select(Location, Group, lon, lat) %>%
  arrange(Group, Location)

usethis::use_data(locs)
