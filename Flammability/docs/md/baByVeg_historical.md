


##
##
## baByVeg_historical.R

The `baByVeg_historical.R` script calculates annual burn area by vegetation class using empirical historical fire perimeter data.
These values inform GBM modeling.

## R code


```r
library(raster)
library(data.table)
library(dplyr)
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data")
outDir <- "../workspaces/gbmFlammability"
shp <- shapefile("../data/shapefiles/noa_basin2/Noa_basin2")

noatak = TRUE
noatak = FALSE

b <- brick("historicalFireObs/firescarbrick_annual_observed_Statewide_lightning_1950_2013.tif")
r.veg <- raster("alf2005.cavm.merged.030212.tif")
if (noatak) {
    r.veg <- mask(crop(r.veg, shp), shp)
    b <- mask(crop(b, shp), shp)
}
v <- r.veg[]
ind.list <- lapply(1:7, function(i, x) which(x == i), x = v)
names(ind.list) <- c("alp.tundra", "mariana", "glauca", "decid", "shrub", "gram", 
    "wetland")
yrs <- 1950:2013

getVegFire <- function(b, v) {
    for (i in 1:nlayers(b)) {
        fire.ind <- which(subset(b, i)[] == 1)
        x <- sapply(v, function(i, x) length(intersect(x, i)), x = fire.ind)
        if (i == 1) 
            ba <- x else ba <- rbind(ba, x)
        print(nlayers(b) - i)
    }
    rownames(ba) <- NULL
    data.frame(Year = yrs, ba)
}

d.ba <- getVegFire(b, ind.list)

head(d.ba)
apply(d.ba[, -1], 2, sum)
apply(d.ba[, -1], 1, sum)

d.ba.pct <- d.ba %>% mutate(total = alp.tundra + mariana + glauca + decid + 
    shrub + gram + wetland) %>% mutate(alp.tundra = alp.tundra/total, marianaBApct = mariana/total, 
    glaucaBApct = glauca/total, decidBApct = decid/total, shrubBApct = shrub/total, 
    gramBApct = gram/total, wetlandBApct = wetland/total) %>% round(3)

if (noatak) {
    d.ba.noatak <- d.ba
    d.ba.pct.noatak <- d.ba.pct
}
if (!noatak) {
    d.ba.sw <- d.ba
    d.ba.pct.sw <- d.ba.pct
}

save(d.ba.pct.sw, d.ba.pct.noatak, file = file.path(outDir, "baByVeg_historical_1950_2013.RData"))
```
