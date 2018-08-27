


##
##
## gbm_flam_maps.R

The `gbm_flam_maps.R` script creates gradient boosting machine (GBM) model-based, or boosted regresion model-based, vegetation flammability matrices.
It loads a given **R** workspace file created by `gbm_flam_prep.R`.
Other inputs include GBM models built on vegetation-specific, aggregate regional climate data.
The matrices are used to fill in spatially explicit flammability maps in a subsequent script, `gbm_flam_maps2.R`.
These annual maps are used as inputs to ALFRESCO in lieu of basic monthly temperature and precipitation maps.

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
if (!exists("allcavm")) allcavm <- FALSE
if (!exists("samples")) samples <- FALSE
if (!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")
if (!is.logical(samples)) stop("Argument 'samples' must be logical.")
if (samples & !exists("n")) stop("Must provide n if samples=TRUE")

library(gbm)
library(rgdal)
library(raster)
library(rasterVis)
library(parallel)
library(data.table)
rasterOptions(chunksize = 1e+11, maxmemory = 1e+12)
ncores <- 32

verDir <- if (samples) "samples_based" else "means_based"
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")
load(paste0("gbmFlammability/", model, "_", period, "_Jun-AugTP.RData"))
# load(paste0('gbmFlammability/', model, '_', period, '_Jan-SepTP.RData'))
suffix <- if (samples) paste0(n, "n") else "Mean"

# Load gbm models
if (samples) {
    load("gbm_monthly_all_100samples.RData")
    tree.numbers <- c(5000, 5000, 5000, 5000, 5000, 5000)  # order: forest, alpine tundra, shrub, graminoid, wetland, cavm
} else stop("Currently must use samples=TRUE. Also, only month-based GBMs available. No seasonal GBMs.")  #{
# load('gbm_seasonal_all.RData') tree.numbers <- c(3355, 32, 2200, 152,
# 2478, 1554) # order: forest, alpine tundra, shrub, graminoid, wetland,
# cavm }
gbm.names <- c("gbm.forest", "gbm.alp.tundra", "gbm.shrub", "gbm.gram", "gbm.wetland")

if (allcavm) {
    out <- paste0("3m", suffix)
    gbm.gram <- gbm.shrub <- gbm.wetland <- gbm.cavm
    tree.numbers <- tree.numbers[c(1, 2, 6, 6, 6)]  # order: forest, alpine tundra, shrub, graminoid, wetland
} else {
    out <- paste0("5m", suffix)
}
```

### Prep function

```r
f <- function(p, yrs = NULL, bins = 1, standardize = FALSE) {
    d.names <- rownames(summary(get(gbm.names[p])))
    tmp <- c()
    if (any(grep("Summer", d.names))) {
        obj.names.list <- list(ls(pattern = "^m\\..*.P$", envir = .GlobalEnv), 
            ls(pattern = "^m\\..*.T$", envir = .GlobalEnv))  # alphabetical
        ord <- order(d.names)
        for (i in ord) {
            obj.names <- obj.names.list[[i]]
            n <- length(obj.names)
            tmp2 <- 0
            for (j in 1:n) tmp2 <- tmp2 + get(obj.names[j])
            is.temp <- length(grep("T$", obj.names)) > 0
            if (is.temp) 
                tmp2 <- tmp2/n
            tmp2 <- tmp2[get(ind.names[p]), ]
            dims <- dim(tmp2)
            tmp2 <- as.numeric(tmp2)
            if (standardize) 
                tmp2 <- (tmp2 - mean(tmp2, na.rm = TRUE))/sd(tmp2, na.rm = TRUE)
            tmp <- cbind(tmp, tmp2)
            rm(tmp2)
            gc()
        }
    } else {
        for (i in 1:nrow(summary(get(gbm.names[p])))) {
            tmp2 <- get(paste0("m.", d.names[i]))[get(ind.names[p]), ]
            dims <- dim(tmp2)
            tmp2 <- as.numeric(tmp2)
            if (standardize) 
                tmp2 <- (tmp2 - mean(tmp2, na.rm = TRUE))/sd(tmp2, na.rm = TRUE)
            tmp <- cbind(tmp, tmp2)
        }
    }
    colnames(tmp) <- d.names
    rownames(tmp) <- NULL
    tmp <- data.frame(tmp)
    if (length(yrs) == dims[2]) 
        ids <- yrs else ids <- 1:dims[2]
    tmp$ID <- rep(ids, each = dims[1])
    assign("tmp.names", paste0("tmp_", c(paste0(0, c(1:9)), 10:bins)[1:bins]), 
        envir = .GlobalEnv)  # global assignment side effects
    brks <- round(seq(1, nrow(tmp), length.out = bins + 1))
    for (i in 1:bins) {
        if (i == bins) 
            rows <- brks[i]:(brks[i + 1]) else rows <- brks[i]:(brks[i + 1] - 1)
        assign(tmp.names[i], tmp[rows, ], envir = .GlobalEnv)  # global assignment side effects
    }
    rm(tmp)
    gc()
    return(p)
}
```

### Prediction function

```r
getGBMpreds <- function(a, b, nam1, nam2) {
    x0 <- get(nam1[b])
    x1 <- get(nam2[a])
    y <- predict.gbm(x0, x1, n.trees = tree.numbers[b])
    data.frame(Predicted = y, ID = x1$ID)
}
```

### Run predictions and save flammability matrix workspaces


```r
preds <- list()
for (zzz in 1:5) {
    model.index <- f(zzz, bins = ncores)  # Prep data
    tmp.preds <- mclapply(1:ncores, getGBMpreds, b = model.index, nam1 = gbm.names, 
        nam2 = tmp.names, mc.cores = ncores)  # GBM predictions
    tmp.preds <- rbindlist(tmp.preds)
    preds[[model.index]] <- matrix(tmp.preds$Predicted, ncol = length(yrs))
    print(zzz)
}

# Organize results
flam <- matrix(NA, nrow = length(veg), ncol = length(yrs))
for (i in 1:5) flam[which(get(ind.names[i])), ] <- preds[[i]]
flam.range <- range(flam, na.rm = T)
save(flam, flam.range, yrs, r.veg, file = paste0("gbmFlammability/rawFlamPreds_", 
    out, "_", model, "_", period, ".RData"))
```
