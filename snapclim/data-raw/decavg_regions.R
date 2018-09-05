# Annual regional summaries, clim_2km_monthly_stats/ and clim_2km_seasonal_stats/, must be in data-raw/.
library(dplyr)
basepath <- "data-raw"
files <- list.files(basepath, recursive = TRUE)
out <- file.path("decavg", files)

purrr::walk2(files, out, ~({
  x <- readRDS(file.path(basepath, .x)) %>% mutate(Decade = as.integer(Year - Year %% 10)) %>%
    group_by(RCP, GCM, Var, Group, Region, Decade)
  a <- ifelse(strsplit(.x, "_")[[1]][3] == "monthly", "Month", "Season")
  x <- group_by(x, .data[[!!a]], add = TRUE) %>%
    summarise_if(is.double, function(x) round(mean(x), 1)) %>% ungroup()
  dir.create(file.path(basepath, dirname(.y)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, file.path(basepath, .y))
  })
)
