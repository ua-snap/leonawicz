##############################################################################
#### This R script explore flammability distributions by vegetation class ####
##############################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   12/09/2015        ####

# @knitr setup1
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")

library(raster)
library(ggplot2)
library(data.table)
library(dplyr)

s3 <- stack(list.files("3m100n", pattern="\\.tif$", full=T))
s5 <- stack(list.files("5m100n", pattern="\\.tif$", full=T))
r.veg <- readAll(raster("../../../../alf2005.cavm.merged.030212.tif"))
noa.shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2.shp")
flam.cavm <- extract(s3, which(r.veg[] >= 5))
flam.shrub <- extract(s5, which(r.veg[] == 5))
flam.gram <- extract(s5, which(r.veg[] == 6))
flam.wet <- extract(s5, which(r.veg[] == 7))
flam.alp <- extract(s5, which(r.veg[] == 1))
flam.for <- extract(s5, which(r.veg[] == 2 | r.veg[] == 3 | r.veg[] == 4))

yrs <- 1950:2013
set.seed(123)
make_dt <- function(m, years, veg, sample=0.10){
    data.table(Vegetation=veg, Year=rep(years, each=nrow(m)), Flammability=as.numeric(m)) %>%
        sample_frac(sample) %>% arrange(Vegetation, Year)
}

d.cavm <- make_dt(flam.cavm, yrs, veg="CAVM")
d.shrub <- make_dt(flam.shrub, yrs, veg="shrub")
d.gram <- make_dt(flam.gram, yrs, veg="graminoid")
d.wet <- make_dt(flam.wet, yrs, veg="wetland")
d.alp <- make_dt(flam.alp, yrs, veg="alptun")
d.for <- make_dt(flam.for, yrs, veg="forest")
d <- rbindlist(list(d.cavm, d.shrub, d.gram, d.wet, d.alp, d.for))

d %>% group_by(Vegetation) %>%
    summarise(Pct05=quantile(Flammability, 0.05, na.rm=T),
              Pct10=quantile(Flammability, 0.10, na.rm=T),
              Pct25=quantile(Flammability, 0.25, na.rm=T),
              Pct50=quantile(Flammability, 0.5, na.rm=T),
              Pct75=quantile(Flammability, 0.75, na.rm=T),
              Pct90=quantile(Flammability, 0.90, na.rm=T),
              Pct95=quantile(Flammability, 0.95, na.rm=T)
    ) -> d.stats
save(d, d.stats, yrs, file="../../../../../workspaces/gbmFlammability/flam_dist.RData")

# @knitr setup2
setwd("C:/github/Flammability/workspaces/gbmFlammability")
dir.create(plotDir <- "../../plots/gbmFlammability/distributions", showWarnings=FALSE)

library(ggplot2)
library(data.table)
library(dplyr)

load("flam_dist.RData")

# @knitr plot_setup
# ggplot setup
g <- ggplot(data=d, aes(x=Flammability)) + theme(legend.position="bottom")

# @knitr plot01a
# flammability marginal distributions (all years) by vegetation
(p01a <- g + geom_line(aes(colour=Vegetation), stat="density", size=1))

# @knitr plot01b
# flammability marginal distributions (all years) by vegetation, truncated to [0.12, 0.27]
(p01b <- g + geom_line(data=subset(d, Flammability >= 0.12 & Flammability <= 0.27), aes(colour=Vegetation), stat="density", size=1))

# @knitr plot02
# flammability distributions by vegetation and year, truncated at to [0.12, 0.27]
(p02 <- g + geom_line(data=subset(d, Flammability >= 0.12 & Flammability <= 0.27), aes(group=Year), stat="density", alpha=0.5) + facet_wrap(~ Vegetation))

# @knitr table01
d.stats %>% knitr::kable(digits=4, caption="Critical values associated with flammability by vegetation class")
