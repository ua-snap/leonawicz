library(dplyr)
library(ggplot2)

from <- as.numeric(as.POSIXct("2018-01-01 00:00:00 MDT"))
until <- as.numeric(as.POSIXct(Sys.Date()))

apps <- c("ex_leaflet", "cc4liteFinal", "nwtapp", "plot3D", "ak_daily_precipitation", "ak_ice_edge", 
          "ak_station_cru_eda", "akcan_climate", "gbm_example", "monty_hall", "random_forest_example", 
          "RV_distributions", "RV_distributionsV2", "RV_distributionsV3", "RV_distributionsV4", 
          "sea_ice_coverage", "sea_ice_winds", "temp_wind_events", "tree_rings", "mapmate_orthographic", 
          "customiconsdemo", "standage", "jfsp-v10", "ar5eval", "climdist", "ffvtr", "rvdist", "jfsplite")

get_usage <- function(account = "uasnap", from, until, apps = NULL){
  all_apps <- rsconnect::applications(account)
  if(is.null(apps)) apps <- unlist(all_apps$name)
  x <- purrr::map(apps, ~(
    rsconnect::showUsage(appName = .x, account = "uasnap", from = from, until = until) %>%
      tbl_df %>% mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))
  )
  )
  names(x) <- apps
  bind_rows(x, .id = "app") %>% select(app, date, hours) %>% arrange(app, date)
}

system.time( x <- get_usage(from = from, until = until, apps = apps) )

d <- group_by(x, app) %>% summarize(days = n(), hours = sum(hours)) %>% 
  arrange(desc(hours)) %>% mutate(minutes = 60 * hours)

g <- ggplot(slice(d, 1:10), aes(factor(app, levels = rev(app)), minutes)) + geom_bar(stat = "identity") + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  snapplot::theme_snap() + coord_flip() +
  labs(title = "Top ten Shiny apps by usage", subtitle = "2018 YTD shinyapps.io usage statistics", 
       x = "Application", y = "Total minutes of user interaction")
ggsave("app_usage.png", width = 10, height = 8, type = "cairo", dpi = 300)
