library(jfsp)
library(showtext)
font_add_google("Source Sans Pro", "gfont")
showtext_auto()
by_rcp <- F
by_tx <- F

# fire size distributions
outfiles <- file.path("C:/github/jfsp-archive/data-raw/presentation_plots", 
                      paste0(rep(c("fs_bx_", "logfs_bx_"), each = 3), c("1950-2009", "2020-2099", "1950-2099"), ".png"))
years <- rep(list(1950:2009, 2020:2099, 1950:2099), 2)
lg <- rep(c(FALSE, TRUE), each = 3)
purrr::walk(1:6, ~jfsp_plot("fs_box", years[[.x]], by_rcp = by_rcp, by_tx = by_tx, log = lg[.x], family = "gfont", file = outfiles[.x]))

# library(dplyr)
# library(rgl)
# fs <- filter(firesize, Tx == "Status quo") %>% group_by(Decade, FS) %>% summarise(Freq = mean(Freq)) %>% ungroup() %>%
#   tidyr::complete(Decade, FS)
# x <- unique(fs$Decade)
# y <-  log(unique(fs$FS))
# m <- matrix(fs$Freq, nrow = length(x), byrow = TRUE) 
# clrs = colorRampPalette(c("cornflowerblue", "orange", "firebrick1"))(30)
# persp3d(x, y, m, col = clrs[cut(m, 30)], ticktype = "detailed", theta = -35, phi = 10, xlab = "Decade", ylab = "Fire size Log(acres)")

# conifer:decid ratio
outfiles <- file.path("C:/github/jfsp-archive/data-raw/presentation_plots", paste0("conif_decid_ratio_", c("1950-2099", "2014-2099", "2014-2040", "2041-2099"), ".png"))
years <- list(1950:2099, 2014:2099, 2014:2040, 2041:2099)
breaks <- list(c(seq(1950, 2090, by = 10), 2099), c(2014, seq(2020, 2090, by = 10), 2099), 
               c(2014, seq(2020, 2040, by = 10)), c(2041, seq(2050, 2090, by = 10), 2099))
purrr::walk(1:4, ~jfsp_plot("cdratio", years[[.x]], by_rcp = by_rcp, by_tx = by_tx, breaks = breaks[[.x]], family = "gfont", file = outfiles[.x]))

# P(Fire) near Fairbanks, Alaska
file <- "C:/github/jfsp-archive/data-raw/presentation_plots/fireProb_fbks_n100_25kmBuffer.png"
jfsp_plot("pfire", by_rcp = by_rcp, by_tx = by_tx, family = "gfont", file = file)

# Burn area, FMO, cost
years <- list(1950:2013, 2014:2040, 2041:2099, 2014:2099, 1950:2099)
periods <- c("1950-2013", "2014-2040", "2041-2099", "2014-2099")
breaks <- list(seq(1950, 2010, by = 10), c(2014, seq(2020, 2040, by = 10)), c(2041, seq(2050, 2090, by = 10), 2099), 
               c(2014, seq(2025, 2075, by = 25), 2099), c(seq(1950, 2090, by = 25), 2099))
outfiles <- file.path("C:/github/jfsp-archive/data-raw/presentation_plots",
                      c(paste0("fmo_", rep(c("ba_", "logba_"), times = c(8, 4)), rep(c("ts_", "bx_"), each = 4), rep(periods, 3), ".png"), 
                        paste0("cost_", c(periods, "1950-2099"), ".png"), "cost_dec_2014-2099.png",
                        paste0("ba_sd_ma30_", c(periods, "1950-2099"), ".png")))

purrr::walk(1:4, ~jfsp_plot("cba", years[[.x]], by_rcp, by_tx, breaks = breaks[[.x]], family = "gfont", file = outfiles[1:4][.x]))
purrr::walk(1:4, ~jfsp_plot("ba_box", years[[.x]], by_rcp, by_tx, family = "gfont", file = outfiles[5:8][.x]))
purrr::walk(1:4, ~jfsp_plot("ba_box", years[[.x]], by_rcp, by_tx, log = TRUE, family = "gfont", file = outfiles[9:12][.x]))
purrr::walk(1:5, ~jfsp_plot("cost", years[[.x]], by_rcp, by_tx, breaks = breaks[[.x]], family = "gfont", file = outfiles[13:17][.x]))
jfsp_plot("cost_dec", 2020:2099, by_rcp, by_tx, family = "gfont", file = outfiles[18])
purrr::walk(1:5, ~jfsp_plot("ba_sd", years[[.x]], by_rcp, by_tx, breaks = breaks[[.x]], alaska = TRUE, n = 30, continuous = TRUE, family = "gfont", file = outfiles[19:23][.x]))

# Presentation slides
library(jfsp)
library(dplyr)
library(ggplot2)
# library(showtext)
# font_add_google("Source Sans Pro", "gfont")
# showtext_auto()

# slide 1 options: historical observed burn area
lev <- c("Prior to 1988", "Since 1988")
d <- filter(fmoba, Set == "Observed" & FMO %in% c("Full", "Critical")) %>% 
  select(-Set, -Tx, -RCP, -BA_sd_ma10) %>% 
  mutate(Period = factor(ifelse(Year < 1988, lev[1], lev[2]), levels = lev))

oba_summary <- group_by(d, FMO, Period) %>% 
  summarise(Min = min(BA), Q1 = quantile(BA, 0.25), Median = median(BA), Mean = mean(BA), 
            Q3 = quantile(BA, 0.75), Max = max(BA), n_years = n(), n_no_fire = sum(BA == 0))

thm <- snapplot::theme_snap()
ttl <- "Observed burn area"
ylb <- c("Burn area (acres)", "Burn area Log(acres)")
scm <- scale_colour_manual(values = c("black", "orange"))
out_dir <- "C:/github/jfsp-archive/data-raw/presentation_plots2"
dir.create(oba_dir <- file.path(out_dir, "oba"), showWarnings = FALSE)
outfiles <- file.path(oba_dir, paste0(c("obac", "obaf", "oba", "lobac", "lobaf", "loba"), ".png"))

p <- ggplot(data = d, aes(Period, BA)) + thm + ggtitle(ttl)
obac <- p + geom_boxplot(data = filter(d, FMO == "Critical"), fill = 8) + labs(subtitle = "Critical", y = ylb[1])
obaf <- p + geom_boxplot(data = filter(d, FMO == "Full"), fill = 8) + labs(subtitle = "Full", y = ylb[1])
oba <- p + geom_boxplot(aes(x = FMO, colour = Period), fill = 8) + scm + labs(subtitle = "Full vs. Critical", y = ylb[1])
p <- ggplot(data = d, aes(Period, log(BA))) + thm + ggtitle(ttl)
lobac <- p + geom_boxplot(data = filter(d, FMO == "Critical"), fill = 8) + labs(subtitle = "Critical", y = ylb[2])
lobaf <- p + geom_boxplot(data = filter(d, FMO == "Full"), fill = 8) + labs(subtitle = "Full", y = ylb[2])
loba <- p + geom_boxplot(aes(x = FMO, colour = Period), fill = 8) + scm + labs(subtitle = "Full vs. Critical", y = ylb[2])
purrr::walk2(outfiles, list(obac, obaf, oba, lobac, lobaf, loba), ~ggsave(.x, .y, width = 10, height = 6.67, dpi = 300))

save(oba_summary, file = file.path(oba_dir, "oba.RData"))

# slide 2 options: 1950-2099 burn area
thm <- snapplot::theme_snap() + theme(plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), "cm"))
ttl <- "Annual burn area"
stl <- "30-year moving average"
ylb <- "Burn area (acres)"
lbs <- labs(title = ttl, subtitle = stl, y = ylb, x = NULL)
scm <- scale_colour_manual(values = c("cornflowerblue", "orange", "firebrick1"))
sxc <- scale_x_continuous(limits = xlm, expand = c(0, 0), breaks = brks)
gde1 <- guides(colour = guide_legend(order = 1),
               linetype = guide_legend(order = 2, override.aes = list(linetype = c("solid", "22"))))
gde2 <- guides(colour = guide_legend(order = 1))

out_dir <- "C:/github/jfsp-archive/data-raw/presentation_plots2"
dir.create(ba_dir <- file.path(out_dir, "ba"), showWarnings = FALSE)
outfiles <- file.path(ba_dir, paste0("ba_ts_", rep(c("1950-2099", "2014-2040", "2041-2099"), each = 3), rep(c("_rcp_tx", "_rcp", ""), 3), ".png"))

rcp <- c("RCP 4.5", "RCP 6.0", "RCP 8.5")
d <- filter(fmoba, Set != "Observed") %>% select(-Set, - BA_sd_ma10, - CBA)
d0 <- purrr::map(1:3, ~filter(d, RCP == "Historical") %>% mutate(RCP = factor(rcp[.x], levels = rcp))) %>% bind_rows()
d1 <- filter(d, RCP != "Historical") %>% mutate(RCP = factor(RCP, levels = rcp))
d_rcp_tx <- bind_rows(d0, d1) %>% group_by(Tx, RCP, Year) %>% 
  summarise(BA = sum(BA)) %>% mutate(BA30 = RcppRoll::roll_mean(BA, 30, fill = NA)) %>% ungroup()
d <- group_by(d_rcp_tx, Tx, Year) %>% summarise(BA = mean(BA)) %>% 
  mutate(BA30 = RcppRoll::roll_mean(BA, 30, fill = NA)) %>% ungroup()

# 1950 - 2099
sxc <- scale_x_continuous(limits = c(1960, 2090), expand = c(0, 0), breaks = seq(1960, 2090, by = 10))
p <- ggplot(d_rcp_tx, aes(Year, BA30)) + scm + thm + sxc + lbs
p1 <- p + geom_line(aes(colour = RCP, linetype = Tx), size = 1) + gde1
p2 <- p + geom_line(data = filter(d_rcp_tx, Tx == "Status quo"), aes(colour = RCP), size = 1)
p3 <- p + geom_line(data = filter(d, Tx == "Status quo"), size = 1)

# 2014 - 2040
sxc <- scale_x_continuous(limits = c(2014, 2040), expand = c(0, 0), breaks = c(2014, seq(2020, 2040, by = 5)))
p <- ggplot(filter(d_rcp_tx, Year >= 2014 & Year < 2041), aes(Year, BA30)) + scm + thm + sxc + lbs
p4 <- p + geom_line(aes(colour = RCP, linetype = Tx), size = 1) + gde1
p5 <- p + geom_line(data = filter(d_rcp_tx, Year >= 2014 & Year < 2041 & Tx == "Status quo"), aes(colour = RCP), size = 1)
p6 <- p + geom_line(data = filter(d, Year >= 2014 & Year < 2041 & Tx == "Status quo"), size = 1)

# 2041 - 2099
sxc <- scale_x_continuous(limits = c(2041, 2085), expand = c(0, 0), breaks = c(2041, seq(2050, 2090, by = 10)))
p <- ggplot(filter(d_rcp_tx, Year >= 2041), aes(Year, BA30)) + scm + thm + sxc + lbs
p7 <- p + geom_line(aes(colour = RCP, linetype = Tx), size = 1) + gde1
p8 <- p + geom_line(data = filter(d_rcp_tx, Year >= 2041 & Tx == "Status quo"), aes(colour = RCP), size = 1)
p9 <- p + geom_line(data = filter(d, Year >= 2041 & Tx == "Status quo"), size = 1)

plist <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
purrr::walk2(outfiles, plist, ~ggsave(.x, .y, width = 10, height = 6.67, dpi = 300))
