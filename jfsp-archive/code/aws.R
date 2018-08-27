library(raster)
library(dplyr)

files <- split(list.files("data-raw/ba_fmo", full.names = TRUE), rep(1:2, each = 13))
grp <- gsub("fmz", "FMZ", gsub("eco", "Ecoregion", gsub("sw", "JFSP domain", gsub(".rds", "", sapply(strsplit(files[[1]], "_"), "[", 4)))))
reg <- gsub("eco_|fmz_", "", gsub("sw_alaska", "Alaska", gsub(".rds", "", sapply(strsplit(files[[1]], "fmo_"), "[", 2))))

basecost <- tibble::data_frame(
  limited = c(13.9, 0.3, 7.6, 92.0, 10.6, 8.1, 12.0),
  modified = c(16, 22, 164, 356, 31, 56, 534),
  critical = c(2459, 473, 12868, 11160, 198, 6935, 8439),
  full = c(351, 257, 456, 76, 89, 312, 120)
)

format_fmoba <- function(ba_hist, ba_proj){
  ba_hist <- readRDS(ba_hist)
  ba_proj <- readRDS(ba_proj)
  gcms <- c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
  f <- function(x) switch(x, historical = "Historical",
                          rcp45 = "RCP 4.5", rcp60 = "RCP 6.0", rcp85 = "RCP 8.5")
  lev <- c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")
  sets <- strsplit(ba_proj$Set, "\\.")
  fmoba <- mutate(ba_proj, Set = sapply(sets, "[", 1), RCP = sapply(sets, "[", 2),
                  Model = gsub("CRU32", "CRU 3.2", sapply(sets, "[", 3))) %>%
    filter(Tx != "none") %>% mutate(RCP = factor(sapply(as.character(RCP), f), levels = lev)) %>%
    mutate(Tx = ifelse(is.na(Tx) & Set == "fmo99s95i", "Status quo",
                       ifelse(is.na(Tx), "No management",
                              ifelse(Tx == "tx0", "Status quo",
                                     ifelse(Tx == "tx1", "Treatment 1",
                                            ifelse(Tx == "tx2", "Treatment 2",
                                                   ifelse(Tx == "none", "No management", Tx)))))))
  fmoba0 <- mutate(ba_hist, Set = substr(Set, 1, 9), RCP = factor("Historical", levels = lev),
                   Model = "Observed", Tx = "Status quo")
  bind_rows(fmoba, fmoba0) %>%
    mutate(Set = factor(Set, levels = c("Observed", "fmo99s95i")),
           Model = factor(Model, levels = c("Observed", "CRU 3.2", gcms)),
           Tx = factor(Tx, levels = c("No management", "Status quo", "Treatment 1", "Treatment 2"))) %>%
    select(c(1, 2, 10:11, 3:9)) %>% mutate_at(6:11, ~round(247.105 * .x)) %>%
    arrange(Set, Tx, RCP, Model, Year)
}

fmoba <- purrr::map(seq_along(reg), ~format_fmoba(files[[1]][.x], files[[2]][.x]) %>% 
                      mutate(Group = grp[.x], Region = reg[.x])) %>% bind_rows() %>%
  arrange(Group, Region, Set, Tx, RCP, Model, Year)

regba <- mutate(fmoba, Total = Unmanaged + Limited + Modified + Critical + Full + Other) %>% select(c(1:5, 14, 12:13))
r <- snapgrid::swflam
x0 <- round(247.105 * freq(r)[1, 2])
x1 <- reg[grp == "Ecoregion"]
x2 <- reg[grp == "FMZ"]

x1a <- purrr::map_dbl(x1, ~({
  x <- subset(snappoly::ecoreg, snappoly::ecoreg$LEVEL_2 %in% .x)
  round(247.105 * sum(unlist(extract(r, x)), na.rm = TRUE))
}))
x1b <- purrr::map_dbl(x2, ~({
  x <- subset(snappoly::fmz, snappoly::fmz$REGION %in% .x)
  round(247.105 * sum(unlist(extract(r, x)), na.rm = TRUE))
}))

get_acres <- function(id) as.integer(c(x0, x1a, x1b)[match(id, c("Alaska", x1, x2))])
regba <- mutate(regba, Region_Acres = get_acres(Region))

mgmtcost <- filter(fmoba, Set == "fmo99s95i")

mc_cost <- function(data, basecost, n = 1000, seed = 1, decadal = FALSE){
  a <- ifelse(decadal, "Decade", "Year")
  if(decadal) data <- mutate(data, Decade = Year - Year %% 10)
  set.seed(seed)
  x <- purrr::map(1:n, ~({
    x <- as_data_frame(data[, 6:11]*cbind(0, sapply(basecost, sample, nrow(data), replace = TRUE), 0) / 1e6)
    if(decadal){
      y <- select(data, c(12:13, 1:4, 14))
    } else {
      y <- select(data, c(12:13, 1:5))
    }
    bind_cols(y, x)
  })) %>% bind_rows()
  stats <- c("5th percentile", "Mean", "95th percentile")
  cost <- purrr::map2(list(quantile, mean, quantile), list(0.05, NULL, 0.95), ~({
    if(decadal){
      x <- group_by(x, Group, Region, Set, Tx, RCP, Model, Decade)
    } else {
      x <- group_by(x, Group, Region, Set, Tx, RCP, Model, Year)
    }
    mutate(x, Total = Unmanaged + Limited + Modified + Full + Critical + Other) %>%
      summarise_at(1:7, .x, prob = .y)
  }))
  lev <- c("Unmanaged", "Limited", "Modified", "Full", "Critical", "Other", "Total")
  purrr::map2(cost, stats, ~mutate(.x, cost = factor(.y, levels = stats))) %>%
    bind_rows() %>% tidyr::gather("FMO", "value", 8:14, factor_key = TRUE) %>% ungroup() %>%
    select(c(1:2, 4:6, 9:7, 10)) %>% filter(!FMO %in% c("Unmanaged", "Other")) %>%
    mutate_at(8, as.integer) %>%
    arrange(Group, Region, Tx, RCP, Model, FMO, cost, .data[[a]]) %>% 
    mutate(FMO = factor(FMO, levels = lev)) %>% tidyr::spread(cost, value)
}

costann <- mc_cost(mgmtcost, basecost)
costdec <- mc_cost(mgmtcost, basecost, decadal = TRUE)
costdec2 <- filter(costdec, Decade >= 2020)
saveRDS(costann, "data-raw/cost/costann.rds")
saveRDS(costdec, "data-raw/cost/costdec.rds")
saveRDS(costdec2, "data-raw/cost/costdec_2020-2099.rds")
saveRDS(regba, "data-raw/ba_reg/regba.rds")
