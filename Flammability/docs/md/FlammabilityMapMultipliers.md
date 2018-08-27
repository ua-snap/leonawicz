


##
##
## FlammabilityMapMultipliers.R

The `FlammabilityMapMultipliers.R` script multiplies each flammability map in a series by either a scalar lightning intensity coefficient
or a spatially explicit lightning intensity map to stretch or compress inter-annual variability in flammability.
Lightning coefficients are stored in a data table loaded by workspace, originally assembled by `gbm_lightning_coefficients.R`.
The three coefficients, 0.05, 0.50, and 0.95, represent low, medium, and high lightning intensity with respect to the inter-annual signal of the gbm lightning point model predictions.
Point model refers to a spatially aggregated model, specifically one using mean climate variables as covariates and having a spatially aggregated total or average lightning response.

For CRU 3.2 historical flammability maps, observation-based coefficients are applied for the years 2003 - 2011.
Years 1950 - 2002 use backcast predictions of annual lightning intensity and a small forecast for years 2012 - 2013.
For GCM-based flammability maps, forecast lightning coefficients are used for years 2014 - 2099.

In the case of spatially explicit lightning maps, maps are similarly available for 2003 - 2011
and are similarly classified into one of three levels of annual intensity corresponding to the scalar coefficients.
Scalar coefficients are either used directly as multipliers
or they are used to make a random map draw from the subset of three of the nine maps similarly classified as low, medium, or high intensity.
In this manner, a lightning map is associated with each year in the backcast and forecast periods and years 2003 - 2011 use their own respective maps,
analogous to the direct application of scalar coefficients.

Backcasts and forecasts can go as far as there is CRU 3.2 (back to 1901) or GCM (forward through 2099) data available.
Beyond this, simple random sampling of the coefficient set or the map set must be used, though relative sampling probabilites can differ by coefficient or map group in accordance with their proportions during known periods.

## R code

### Setup


```r
comargs <- (commandArgs(TRUE))
if (!length(comargs)) q("no") else for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("period")) stop("Argument 'period' not passed at command line.")
if (!exists("model")) stop("Argument 'model' not passed at command line.")
if (!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if (!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", 
    "MRI-CGCM3"))) stop("Invalid data set specified.")
if (!exists("samples")) samples <- TRUE
if (!exists("mapset")) stop("Argument 'mapset' not passed at command line.")
if (!exists("lightning")) lightning <- TRUE
if (!exists("cp2scratch")) cp2scratch <- TRUE
if (!exists("cp_originals")) cp_originals <- FALSE

verDir <- if (samples) "samples_based" else "means_based"
setwd(file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability", 
    verDir, period, model, mapset))
suffix <- if (lightning) "_Lmap" else "_L"
dir.create(outDir <- paste0("../", mapset, suffix), showWarnings = FALSE)
if (cp2scratch) {
    dir.create(outDir2a <- file.path("/big_scratch/mfleonawicz/Alf_Files_20121129/gbmFlamMaps", 
        period, model, mapset), recursive = TRUE, showWarnings = FALSE)
    dir.create(outDir2b <- paste0(outDir2a, suffix), showWarnings = FALSE)
} else outDir2b <- NULL
if (!cp_originals) outDir2a <- NULL

library(raster)
library(parallel)
library(data.table)
library(dplyr)
load("../../../../../../workspaces/gbmFlammability/gbm_lightning_coefficients.RData")  # scalars data table

files <- list.files(pattern = "\\.tif$")
yrs <- as.numeric(gsub("gbm.flamm_", "", gsub("\\.tif", "", files)))
files <- files[order(yrs)]
yrs <- yrs[order(yrs)]
d.sub <- filter(d.all, Period == period & Model == model & Year %in% yrs)

# Sample random coeffcients for unobserved years
set.seed(51)
if (lightning) {
    classes <- sapply(d.sub$Class, function(x) switch(as.character(x), Low = 1, 
        Medium = 2, High = 3))
    load("/workspace/UA/mfleonawicz/leonawicz/projects/Lightning/data/summerLightningMaps_2003_2011/summerLightningMaps.RData")
    light.yrs <- sapply(classes, function(x, d) sample(d$Year[d$Class == x], 
        1), d = d.coef)
    ind <- which(d.sub$Year %in% d.coef$Year)
    if (length(ind)) 
        light.yrs[ind] <- d.sub$Year[ind]
    d.coef$Prob <- sapply(d.coef$Class, function(x) switch(x, `1` = 14/64, `2` = 36/64, 
        `3` = 14/64))
    a <- sample(d.coef$Year, length(yrs), prob = d.coef$Prob, replace = T)
    if (period == "historical" & all(1950:2013 %in% yrs)) 
        a[yrs >= 1950 & yrs <= 2013] <- light.yrs
    if (period != "historical") 
        a <- light.yrs
} else {
    # a <- sample(c(0.05, 0.5, 0.95), length(yrs), prob=c(13/62, 36/62, 13/62),
    # replace=T)
    a <- runif(length(yrs), 0.01, 1)
    if (period == "historical" & all(1950:2013 %in% yrs)) 
        a[yrs >= 1950 & yrs <= 2013] <- d.sub$Coef
    if (period != "historical") 
        a <- d.sub$Coef
}
```

### Run


```r
f <- function(i, a, b = NULL, type = "coef", outDir, files, flam.min = NULL, 
    f_of_xy = NULL, cp.origin = NULL, cp.new = NULL) {
    r <- raster(files[i])
    if (!is.null(cp.origin)) 
        writeRaster(r, file.path(cp.origin, files[i]), datatype = "FLT4S", overwrite = T)
    if (!is.null(flam.min)) 
        r[r < flam.min] <- flam.min
    if (type == "coef") {
        r <- a[i] * r
    } else if (type == "year") {
        if (is.null(b)) 
            stop("b cannot be NULL if type='year'. Change to type='coef' or provide a raster brick b.")
        b.yrs <- as.numeric(substr(names(b), 2, 5))
        ind <- which(b.yrs == a[i])
        if (is.null(f_of_xy)) {
            r2 <- subset(b, ind)
            r <- r2 * r
        } else f_of_xy(x = subset(b, ind), y = r)
    }
    r <- round(r, 8)
    writeRaster(r, file.path(outDir, files[i]), datatype = "FLT4S", overwrite = T)
    if (!is.null(cp.new)) 
        writeRaster(r, file.path(cp.new, files[i]), datatype = "FLT4S", overwrite = T)
    print(i)
}

# @knit run
if (lightning) mclapply(1:length(files), f, a = a, b = kde.maps, type = "year", 
    outDir = outDir, files = files, cp.origin = outDir2a, cp.new = outDir2b, 
    mc.cores = 32)
if (!lightning) mclapply(1:length(files), f, a = a, outDir = outDir, files = files, 
    cp.origin = outDir2a, cp.new = outDir2b, mc.cores = 32)
```
