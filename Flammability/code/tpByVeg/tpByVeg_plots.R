# @knitr setup
setwd("C:/github/Flammability/workspaces/tpByVeg")
dir.create(plotDir <- "../../plots/tpByVeg", showWarnings=FALSE)

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)

dlist <- vector("list", 2)
load("tpByVeg_samples100_CRU32_individual.RData")
dlist[[1]] <- d
load("tpByVeg_samples100_CMIP5_individual.RData")
dlist[[2]] <- d
d <- rbindlist(dlist)
rm(dlist)
d[, Month:=factor(Month, levels=month.abb)]

# @knitr plot_group1a
xvar <- "Year"
yvar <- "Temperature"
veg <- c("shrub")
mod <- "GFDL-CM3"
rcp <- "rcp60"
mos <- c("Aug")
outfile <- paste(yvar, veg, mos, rcp, mod, sep="_")

d %>% filter(Month %in% mos) %>% filter(Scenario %in% c("historical", rcp)) %>%
    filter(Model %in% c("CRU32", mod)) %>% filter(Vegetation %in% veg) %>%
    filter(Var==yvar)-> dsub

(g1 <- ggplot(dsub, aes(x=factor(Year), y=Val, colour=Model)) + geom_boxplot() +
    labs(x=xvar, y=yvar, title="n=100 annual sample") + theme(legend.position="bottom") +
    scale_x_discrete(breaks=seq(1950, 2100, by=10), labels=seq(1950, 2100, by=10)))

(g2 <- ggplot(dsub, aes(x=Year, y=Val, group=interaction(Scenario, Model, Month, Var, Vegetation, Obs), fill=Model, colour=Model)) +
    labs(x=xvar, y=yvar, title="n=100 annual sample") +
    theme(legend.position="bottom") + guides(colour=guide_legend(override.aes=list(alpha=1))) +
    geom_line(alpha=0.2) +
    scale_x_continuous(breaks=seq(1950, 2100, by=10), labels=seq(1950, 2100, by=10)))

# @knitr plot_group1b
(g3 <- ggplot(dsub, aes(x=Val, fill=Model)) + geom_density(position="dodge", colour=1, alpha=0.5) +
    labs(x=yvar, y="Density", title="n=100 annual sample, pooled 1950-2013 (CRU) and 2010-2099 (GCM)") + theme(legend.position="bottom"))

# @knitr notrun1
png(file.path(plotDir, paste0("tsBoxplots_", outfile, ".png")), width=3200, height=1600, res=200)
g1
dev.off()

png(file.path(plotDir, paste0("tsByObs_", outfile, ".png")), width=3200, height=1600, res=200)
g2
dev.off()

png(file.path(plotDir, paste0("dist_", outfile, ".png")), width=2400, height=2400, res=200)
g3
dev.off()

# @knitr plot_group2
vars <- c("Temperature", "Precipitation")
mos <- c("Jun", "Jul", "Aug")
outfile <- paste("tpScatter", veg, "JJA", rcp, mod, sep="_")

d %>% filter(Month %in% mos) %>% filter(Scenario %in% c("historical", rcp)) %>%
    filter(Model %in% c("CRU32", mod)) %>% filter(Vegetation %in% veg) %>%
    filter(Var %in% vars) -> dsub
dsub <- data.table(dcast(dsub, Scenario + Model + Year + Month + Vegetation + Obs ~ Var, value.var="Val"))
dsub %>% group_by(Scenario, Model, Year, Month, Vegetation) %>% summarise(MeanP=mean(Precipitation), MeanT=mean(Temperature)) -> dsub2
dsub2 %>% group_by(Scenario, Model, Month, Vegetation) %>% summarise(MeanP=mean(MeanP), MeanT=mean(MeanT)) -> dsub3

(g4 <- ggplot(dsub, aes_string(x=vars[1], y=vars[2], colour="Model")) + geom_point(alpha=0.1) +
    geom_point(data=filter(dsub2, Model=="CRU32"), aes(x=MeanT, y=MeanP), colour="red", alpha=1) +
    geom_point(data=filter(dsub2, Scenario=="rcp60"), aes(x=MeanT, y=MeanP), colour="blue", alpha=1) +
    geom_point(data=dsub3, aes(x=MeanT, y=MeanP), colour="black", alpha=1, size=5) +
    geom_point(data=dsub3, aes(x=MeanT, y=MeanP), colour="white", alpha=1, size=3) +
    labs(x=vars[1], y=vars[2], title="n=100 annual sample, pooled 1950-2013 (CRU) and 2010-2099 (GCM)") + theme(legend.position="bottom") +
    guides(colour=guide_legend(override.aes=list(alpha=1))) +
    facet_wrap(~ Month))

# @knitr notrun2
png(file.path(plotDir, paste0(outfile, ".png")), width=3600, height=1200, res=200)
g4
dev.off()
