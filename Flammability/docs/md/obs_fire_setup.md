


##
##
## obs_fire_setup.R

The `obs_fire_setup.R` script prepares various data objects related to the empirical or historically observed fire data,
including fire area shapefiles, single- and multi-band rasters, subsetting to years of interest,
and creating temporary files used by various ALFRESCO post-processing **R** scripts.

Two examples of scripts which call this script are `fsByVeg.R` and `AlfrescoFRP.R`.

## R code


```r
if (emp.fire.cause == "All") fah <- shapefile("/big_scratch/mfleonawicz/FAH/FireAreaHistory_11182013.shp")
if (emp.fire.cause == "Lightning") fah <- shapefile("/big_scratch/mfleonawicz/FAH/Lightning_Fires_11182013.shp")
fah <- subset(fah, FireYear >= 1950)  # do nto use observed data prior to 1950
yrs.fah <- sort(as.numeric(unique(fah@data$FireYear)))
if (period == "historical") yrs.hist.all <- yrs else yrs.hist.all <- 1950:2013  # default historical years when processing future Alfresco runs

if (substr(tolower(alf.domain), 1, 6) == "noatak") {
    r <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/alf2005.cavm.merged.030212_Noatak.tif")
    shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2.shp")
} else if (substr(tolower(alf.domain), 1, 6) == "statew") {
    r <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/alf2005.cavm.merged.030212.tif")
    shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/statewide_shape/Alaska_Albers_ESRI.shp")
}

suffix <- paste0("_observed_", gsub("_", "", alf.domain), "_", tolower(emp.fire.cause), 
    "_", min(yrs.hist.all), "_", max(yrs.hist.all), ".tif")
b.fid.name <- paste0("/big_scratch/shiny/fireIDbrick_annual", suffix)
result.name <- paste0("/big_scratch/shiny/firescarbrick_annual", suffix)
result2.name <- paste0("/big_scratch/shiny/firescarlayer_total", suffix)
if (file.exists(b.fid.name)) {
    b.fid <- brick(b.fid.name)
    print("b.fid read into memory")
}
if (file.exists(result.name)) {
    result <- brick(result.name)
    print("result read into memory")
}
if (file.exists(result2.name)) {
    result2 <- raster(result2.name)
    print("result2 read into memory")
}

fireScarsFun <- function(year, x, y, years.avail, field = 1) {
    if (year %in% years.avail) {
        x <- x[x$FireYear == year, ]
        x <- rasterize(x, y, field = field)
    } else {
        x <- x[x$FireYear == years.avail[1], ]
        x <- rasterize(x, y, field = 1)
        x[!is.na(x)] <- NA
    }
    x
}

library(parallel)
n.cores <- 32  # hardcoded
# fireScarsFunVec <- Vectorize(fireScarsFun,'year')
if (!exists("b.fid")) {
    b.fid <- mclapply(yrs.hist.all, fireScarsFun, x = fah, y = r, years.avail = yrs.fah, 
        field = "FIREID", mc.cores = n.cores)
    print(summary(b.fid[[1]][]))
    b.fid <- brick(b.fid)
    print("b.fid brick created")
    print(summary(subset(b.fid, 1)[]))
    if (substr(tolower(alf.domain), 1, 6) == "noatak") {
        b.fid <- mask(b.fid, shp)
        print("b.fid brick masked")
    }
    if (!file.exists(b.fid.name)) {
        # writeRaster(b.fid, b.fid.name, datatype='FLT4S', overwrite=T)
        b <- brick(b.fid, values = FALSE)
        b <- writeStart(b, filename = b.fid.name, format = "GTiff", datatype = "FLT4S", 
            overwrite = TRUE)
        tr <- blockSize(b)
        for (i in 1:tr$n) {
            v <- getValuesBlock(b.fid, row = tr$row[i], nrows = tr$nrows[i])
            b <- writeValues(b, v, tr$row[i])
        }
        b <- writeStop(b)
    }
    print("b.fid file written")
    print(summary(subset(b.fid, 1)[]))
}
if (!exists("result")) {
    result <- mclapply(yrs.hist.all, fireScarsFun, x = fah, y = r, years.avail = yrs.fah, 
        mc.cores = n.cores)
    print("result list created")
    print(class(result))
    print(length(result))
    print(class(result[[1]]))
    result <- lapply(result, function(x, r.veg) {
        x[is.na(x) & !is.na(r.veg) & r.veg > 0] <- 0
        x
    }, r.veg = r)
    print("result list processed")
    if (!exists("result2")) 
        result2 <- do.call("sum", c(result, na.rm = T))
    print("result2 raster layer created")
    result <- brick(result)
    print("result brick created")
    if (substr(tolower(alf.domain), 1, 6) == "noatak") {
        b.fid <- mask(b.fid, shp)
        result <- mask(result, shp)
        result2 <- mask(result2, shp)
    }
    names(result) <- names(b.fid) <- yrs.hist.all
    names(result2) <- paste(yrs.hist.all[1], tail(yrs.hist.all, 1), sep = "_")
    if (!file.exists(result.name)) {
        # writeRaster(result, result.name, datatype='FLT4S', overwrite=T)
        b <- brick(result, values = FALSE)
        b <- writeStart(b, filename = result.name, format = "GTiff", datatype = "FLT4S", 
            overwrite = TRUE)
        tr <- blockSize(b)
        for (i in 1:tr$n) {
            v <- getValuesBlock(result, row = tr$row[i], nrows = tr$nrows[i])
            b <- writeValues(b, v, tr$row[i])
        }
        b <- writeStop(b)
        print("result file written")
    }
    if (!file.exists(result2.name)) {
        writeRaster(result2, result2.name, datatype = "FLT4S", overwrite = T)
        print("result2 file written")
    }
}
```
