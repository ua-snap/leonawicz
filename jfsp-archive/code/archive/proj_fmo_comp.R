library(alfresco)
library(dplyr)
file <- "data-raw/projected_fsv.rds"
x <- c("00", 50, 75, 98)
fmos <- paste0("fmo", x, "s", x, "i")
d <- readRDS(file) %>% filter(FMO %in% fmos)
d_all <- fsdf(d)
d_each <- fsdf(d, by_veg = TRUE)

lev <- c("All", levels(d$Vegetation))
d_all <- mutate(d_all, Vegetation = factor("All", levels = lev))
d_each <- mutate(d_each, Vegetation = factor(as.character(Vegetation), levels = lev))
d <- bind_rows(d_all, d_each)
pcbr <- fmo_cb_reduction(d)
usethis::use_data(pcbr)

lev <- c("Control", "Tx: 50s/50i")
pcba_comp <- select(d, c(11, 2:3, 7:10)) %>% filter(FMO %in% c("fmo00s00i", "fmo50s50i")) %>%
  mutate(
    FMO = factor(ifelse(FMO == "fmo00s00i", lev[1], lev[2]), levels = lev),
    FS = FS / 1000,
    CBA = CBA / 1000)
usethis::use_data(pcba_comp)
