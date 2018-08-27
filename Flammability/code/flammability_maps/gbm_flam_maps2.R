########################################################################################################
#### This R script finalizes climate-driven gradient-boosted flammability maps for use by ALFRESCO. ####
########################################################################################################

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("allcavm")) allcavm <- FALSE
if(!exists("samples")) samples <- FALSE
if(!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")
if(!is.logical(samples)) stop("Argument 'samples' must be logical.")
if(samples & !exists("n")) stop("Must provide n if samples=TRUE")

library(rgdal); library(raster); library(rasterVis); library(parallel); library(data.table)
rasterOptions(chunksize=10e10,maxmemory=10e11)
ncores <- 32

verDir <- if(samples) "samples_based" else "means_based"
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/workspaces")
suffix <- if(samples) paste0(n, "n") else "Mean"
out <- if(allcavm) paste0("3m", suffix) else paste0("5m", suffix)
dir.create(outDir <- file.path("../data/gbmFlammability", verDir, period, model, out), recursive=T, showWarnings=F)
dir.create(plotDir <- file.path("../plots/gbmFlammability", verDir, period, model, out), recursive=T, showWarnings=F)
load(paste0("gbmFlammability/rawFlamPreds_", out, "_CRU32_historical.RData")) # workspace contains 'flam' and 'flam.range' for CRU 3.2
cru.range <- flam.range # 3-gbm and 5-gbm CRU 3.2 flammability ranges both = [-5472.929, 34690.204]

if(period!="historical"){
    flam <- (flam - cru.range[1])/(cru.range[2] - cru.range[1])
    d <- data.frame(CRU32=quantile(as.numeric(flam), seq(0,1,by=0.1), na.rm=T))
    load(paste0("gbmFlammability/rawFlamPreds_", out, "_", model, "_", period, ".RData")) # workspace contains 'flam' and 'flam.range' for GCM
    flam[flam < cru.range[1]] <- cru.range[1] # Should almost exclusively affect low-flam CAVM tundra
    flam[flam > cru.range[2]] <- cru.range[2] # Should almost exclusively affect high-flam boreal forest
}

flam <- (flam - cru.range[1])/(cru.range[2] - cru.range[1])

if(period!="historical"){
    d$GCM=quantile(flam, seq(0,1,by=0.1), na.rm=T)
    d$DiffFromCRU=d$GCM-d$CRU32
    names(d)[2] <- paste(period, model, sep="_")
}

# @knitr func_write
partifs <- function(i, r, flam, outDir){
	r <- setValues(r, flam[,i])
	names(r) <- paste0("gbm.flamm_",yrs[1]+i-1)
	writeRaster(r, paste0(outDir, "/gbm.flamm_",yrs[1]+i-1,".tif"), datatype="FLT4S", overwrite=T)
	print(i)
}

# @knitr run
# Write geotiffs
mclapply(1:length(yrs), partifs, r=r.veg, flam=flam, outDir=outDir, mc.cores=32)

# @knitr plot
# Setup
at.vals <- c(0, 0.25, 0.5, 0.75, 1)
colkey <- list(at=at.vals, labels=list(labels=c("Low", "Medium", "High", "Severe"), at=at.vals + 0.125))

# Theme settings
revRasterTheme <- function (pch = 19, cex = 0.7, region=brewer.pal(9, "YlOrRd")[-1], ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

# parallelize levelplot
parplot <- function(i, outDir, dataDir){
	r <- raster(paste0(dataDir, "/gbm.flamm_",yrs[1]+i-1,".tif"))
	png(paste0(outDir, "/gbm.flamm_", yrs[1]+i-1,".png"), height=1600, width=1600, res=200)
	p <- levelplot(r, maxpixels=ncell(r), main=paste(yrs[1]+i-1,"flammability"), par.settings=revRasterTheme, contour=T, margin=F, at=at.vals, colorkey=colkey) #col=rev(heat.colors(30)))
	print(p)
	dev.off()
	print(i)
}

# Write PNGs
mclapply(1:length(yrs), parplot, outDir=plotDir, dataDir=outDir, mc.cores=32)

if(period!="historical") print(round(d,5))
