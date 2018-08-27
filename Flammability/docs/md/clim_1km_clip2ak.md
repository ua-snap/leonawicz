


##
##
## clim_1km_clip2ak.R

The `clim_1km_clip2ak.R` script clips resampled data created by `clim_resample_2km_1km.R` to the classic statewide ALFRESCO extent for eventual inclusion into ALFRESCO modeling.
The clipping is basically a crop and mask.

## R code

### Setup


```r
library(parallel)
library(raster)

comargs <- (commandArgs(TRUE))
if (!length(comargs)) q("no") else for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("cru")) cru <- FALSE
if (!is.logical(cru)) stop("Argument 'cru' must be logical.")

msk <- raster("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/alf2005.cavm.merged.030212.tif")
e <- extent(msk)

mainDir <- "/big_scratch/mfleonawicz/Climate_1km"
if (cru) {
    varid <- c("pr", "tas")
    rcp <- "historical"
    model <- "CRU_TS32"
} else {
    varid <- rep(c("pr", "tas"), 15)
    rcp <- rep(rep(paste0("rcp", c(45, 60, 85)), each = 2), 5)
    model <- rep(list.files(file.path(mainDir, "rcp60")), each = 6)
}
subDir <- file.path(mainDir, rcp, model, varid)
outDir <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", rcp, 
    model, varid)
for (i in 1:length(outDir)) dir.create(outDir[i], recursive = T, showWarnings = F)
```

### Function


```r
f <- function(i, files = NULL, subDir, outDir, msk) {
    if (is.null(files)) {
        files <- list.files(subDir[i], full = T, pattern = ".tif$")
        ind <- 1:length(files)
        ind2 <- i
    } else {
        ind <- i
        ind2 <- 1
    }
    for (j in ind) {
        r <- raster(files[j])
        r <- crop(r, msk)
        r <- mask(r, msk)
        extent(r) <- e
        writeRaster(r, file.path(outDir[ind2], basename(files[j])), datatype = "FLT4S", 
            overwrite = T)
        print(length(files) - j)
    }
    return()
}
```

### Run


```r
if (cru) {
    for (k in 1:length(subDir)) {
        files <- list.files(subDir[k], full = T, pattern = ".tif$")
        mclapply(1:length(files), f, files = files, subDir = NULL, outDir = outDir[k], 
            msk = msk, mc.cores = 32)
    }
} else {
    mclapply(1:length(subDir), f, subDir = subDir, outDir = outDir, msk = msk, 
        cru = cru, mc.cores = length(subDir))
}
```
