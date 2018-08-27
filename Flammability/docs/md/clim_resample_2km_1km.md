


##
##
## clim_resample_2km_1km.R

The `clim_resample_2km_1km.R` script resamples 2-km resolution climate data sets to 1-km resolution for eventual inclusion into ALFRESCO modeling.
Nearest neighbor is used since it is much more efficient given the amount of data to be resampled and,
due to the use case, resampling using bilinear interpolation is not particularly useful, beneficial, or required.

## R code


```r
library(parallel)
library(raster)
msk <- raster("/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/5ModelAvg/sresa2/tas/tas_mean_C_alf_ar4_5modelAvg_sresa2_01_2001.tif")

if (!exists("cru")) cru <- FALSE
if (!is.logical(cru)) stop("Argument 'cru' must be logical.")

if (cru) {
    mainDir <- "/Data/Base_Data/Climate/AK_CAN_2km/historical/CRU"
    varid <- c("pr", "tas")
    rcp <- "historical"
    model <- "CRU_TS32"
    subDir <- file.path(mainDir, model, varid)
} else {
    mainDir <- "/Data/Base_Data/Climate/AK_CAN_2km/projected/AR5_CMIP5_models"
    varid <- rep(c("pr", "tas"), 15)
    rcp <- rep(rep(paste0("rcp", c(45, 60, 85)), each = 2), 5)
    model <- rep(list.files(file.path(mainDir, "rcp60"))[-1], each = 6)
    subDir <- file.path(mainDir, rcp, model, varid)
}

outDir <- file.path("/big_scratch/mfleonawicz/Climate_1km", rcp, model, varid)
for (i in 1:length(outDir)) dir.create(outDir[i], recursive = T, showWarnings = T)

f <- function(i, subDir, outDir, msk) {
    require(raster)
    files <- list.files(subDir[i], full = T, pattern = ".tif$")
    for (j in 1:length(files)) {
        r <- raster(files[j])
        # r <- resample(r, msk, method='ngb') # Nearest neighbor for efficiency,
        # scale change is small and use case doesn't require interpolation
        r <- disaggregate(r, c(2, 2))
        r <- crop(r, msk)
        extent(r) <- extent(msk)
        r <- mask(r, msk)
        r@file@blockcols <- ncol(r)
        writeRaster(r, file.path(outDir[i], basename(files[j])), datatype = "FLT4S", 
            overwrite = T)
        print(length(files) - j)
    }
    return()
}

fcru <- function(i, files, outDir, msk) {
    require(raster)
    r <- raster(files[i])
    # r <- resample(r, msk, method='ngb') # Nearest neighbor for efficiency,
    # scale change is small and use case doesn't require interpolation
    r <- disaggregate(r, c(2, 2))
    r <- crop(r, msk)
    extent(r) <- extent(msk)
    r <- mask(r, msk)
    r@file@blockcols <- ncol(r)
    writeRaster(r, file.path(outDir, basename(files[i])), datatype = "FLT4S", 
        overwrite = T)
    print(length(files) - i)
    return()
}

if (cru) {
    for (j in 1:length(subDir)) {
        files <- list.files(subDir[j], full = T, pattern = ".tif$")
        mclapply(1:length(files), fcru, files = files, outDir = outDir[j], msk = msk, 
            mc.cores = 32)
    }
} else {
    mclapply(1:length(subDir), f, subDir = subDir, outDir = outDir, msk = msk, 
        mc.cores = length(subDir))
}
```
