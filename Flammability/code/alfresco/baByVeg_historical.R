# @knitr script
library(raster)
library(data.table)
library(dplyr)
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/data")
outDir <- "../workspaces/gbmFlammability"
shp <- shapefile("../data/shapefiles/noa_basin2/Noa_basin2.shp")

noatak=TRUE
noatak=FALSE

r.veg <- raster("alf2005.cavm.merged.030212.tif")
r.veg[r.veg==0] <- NA
if(noatak){
    b <- brick("historicalFireObs/fireIDbrick_annual_observed_Noatak_lightning_1950_2013.tif")
    r.veg <- mask(crop(r.veg, shp), shp)
    b <- mask(crop(b, shp), shp)
} else b <- brick("historicalFireObs/fireIDbrick_annual_observed_Statewide_lightning_1950_2013.tif")
b <- mask(b, r.veg)
v <- r.veg[]
ind.list <- lapply(1:7, function(i, x) which(x==i), x=v)
names(ind.list) <- c("alp.tundra", "mariana", "glauca", "decid", "shrub", "gram", "wetland")
yrs <- 1950:2013

getVegFire <- function(b, v, yrs){
    d.fs.list <- vector("list", nlayers(b))
    for(i in 1:nlayers(b)){
        r <- subset(b, i)   
        data.table(Year=yrs[i], freq(r)) %>% setnames(c("Year", "FID", "FS")) %>% filter(!is.na(FID)) -> d.fs.list[[i]]
        fire.ind <- which(r[]>0)
        x <- sapply(v, function(i, x) length(intersect(x, i)), x=fire.ind)
        if(i==1) ba <- x else ba <- rbind(ba, x)
        print(nlayers(b)-i)
    }
    rownames(ba) <- NULL
    list(ba=data.frame(Year=yrs, ba), fs=rbindlist(d.fs.list))
}

d <- getVegFire(b, ind.list, yrs)
d.ba <- d$ba
d.fs <- d$fs

head(d.ba)
apply(d.ba[,-1], 2, sum)
apply(d.ba[,-1], 1, sum)

d.ba %>% mutate(total=alp.tundra + mariana + glauca + decid + shrub + gram + wetland) %>%
    mutate(alp.tundra=alp.tundra/total,
           marianaBApct=mariana/total,
           glaucaBApct=glauca/total,
           decidBApct=decid/total,
           shrubBApct=shrub/total,
           gramBApct=gram/total,
           wetlandBApct=wetland/total
           ) %>% round(3) -> d.ba.pct

if(noatak) {
    d.ba.noatak <- d.ba
    d.ba.pct.noatak <- d.ba.pct
    d.fs.noatak <- d.fs
} else {
    d.ba.sw <- d.ba
    d.ba.pct.sw <- d.ba.pct
    d.fs.sw <- d.fs
}

save(d.ba.pct.sw, d.ba.pct.noatak, d.fs.sw, d.fs.noatak, file=file.path(outDir, "baByVeg_historical_1950_2013.RData"))
