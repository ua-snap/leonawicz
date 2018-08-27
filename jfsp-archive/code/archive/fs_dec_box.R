# testing/temporary
library(alfresco)
library(dplyr)
files <- file.path("data-raw", c("historical_fsv.rds", "projected_fsv.rds"))
x <- c("00", 50, 75, 98)
fmos <- paste0("fmo", x, "s", x, "i")

d1 <- readRDS(files[1]) %>% filter(FMO %in% fmos)
d1 <- group_by(d1, Year, FID, Replicate, FMO) %>% summarise(FS = sum(Val)) %>% ungroup() %>% mutate(Decade = Year - Year %% 10)
d2 <- readRDS(files[2]) %>% filter(FMO %in% fmos & Scenario == "RCP 6.0" & Model == "GFDL-CM3")
d2 <- group_by(d2, Year, FID, Replicate, FMO) %>% summarise(FS = sum(Val)) %>% ungroup() %>% mutate(Decade = Year - Year %% 10)
d <- bind_rows(d1, d2)
d$Decade <- paste0(d$Decade, "s")

library(ggplot2)
clrs <- c("darkred", "orange", "cornflowerblue", "blue")
lab <- list(title = "Decadal fire size distributions by FMO treatment", x = "Decade", subtitle = "32 ALFRESCO simulations")
  
Cairo::CairoPNG("data-raw/dbp_fmo_fs.png", width = 1600, height = 800)
ggplot(d, aes(factor(Decade), FS, colour = FMO)) + geom_boxplot() +
  scale_colour_manual(values = clrs) + theme_gray(base_size = 18) + theme(legend.position = "bottom") + 
  labs(x = lab$x, y = expression(Fire~Size~(km^2)), subtitle = lab$subtitle)
dev.off()

Cairo::CairoPNG("data-raw/dbp_fmo_lfs.png", width = 1600, height = 800)
ggplot(d, aes(factor(Decade), log(FS), colour = FMO)) + geom_boxplot() +
  scale_colour_manual(values = clrs) + theme_gray(base_size = 18) + theme(legend.position = "bottom") + 
  labs(x = lab$x, y = expression(y = Log(Fire~Size)), title = lab$title, subtitle = lab$subtitle)
dev.off()
