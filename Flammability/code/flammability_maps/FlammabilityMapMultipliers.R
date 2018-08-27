#######################################################################################################################################
#### This R script generates new annual gbm flammability maps as a function of the originals and other data such as lightning maps ####
#######################################################################################################################################

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("samples")) samples <- TRUE
if(!exists("mapset")) stop("Argument 'mapset' not passed at command line.")
if(!exists("lightning")) lightning <- TRUE
if(!exists("cp2scratch")) cp2scratch <- FALSE
if(!exists("cp_originals")) cp_originals <- FALSE

verDir <- if(samples) "samples_based" else "means_based"
setwd(file.path("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability", verDir, period, model, mapset))
suffix <- if(lightning) "_Lmap" else "_L"
dir.create(outDir <- paste0("../", mapset, suffix), showWarnings=FALSE)
if(cp2scratch){
	dir.create(outDir2a <- file.path("/atlas_scratch/mfleonawicz/alf_files/gbmFlamMaps", period, model, mapset), recursive=TRUE, showWarnings=FALSE)
	dir.create(outDir2b <- paste0(outDir2a, suffix), showWarnings=FALSE)
} else outDir2b <- NULL
if(!cp_originals) outDir2a <- NULL

library(raster)
library(parallel)
library(data.table)
library(dplyr)
load("../../../../../../workspaces/gbmFlammability/gbm_lightning_coefficients.RData") # scalars data table

files <- list.files(pattern="\\.tif$")
yrs <- as.numeric(gsub("gbm.flamm_", "", gsub("\\.tif", "", files)))
files <- files[order(yrs)]
yrs <- yrs[order(yrs)]
d.sub <- filter(d.all, Scenario==period & Model==model & Year %in% yrs)

# Sample random coeffcients for unobserved years
set.seed(51)
if(lightning){
	classes <- sapply(d.sub$Class, function(x) switch(as.character(x), 'Low'=1,'Medium'=2,'High'=3))
	load("/atlas_scratch/mfleonawicz/projects/Lightning/data/summerLightningMaps_2003_2011/summerLightningMaps.RData")
	light.yrs <- sapply(classes, function(x, d) sample(d$Year[d$Class==x], 1), d=d.coef)
	ind <- which(d.sub$Year %in% d.coef$Year)
	if(length(ind)) light.yrs[ind] <- d.sub$Year[ind]
    d.coef$Prob <- sapply(d.coef$Class, function(x) switch(x, '1'=14/64, '2'=36/64, '3'=14/64))
	a <- sample(d.coef$Year, length(yrs), prob=d.coef$Prob, replace=T)
	if(period=="historical" & all(1950:2013 %in% yrs)) a[yrs >= 1950 & yrs <= 2013] <- light.yrs
    if(period!="historical") a <- light.yrs
} else {
	#a <- sample(c(0.05, 0.5, 0.95), length(yrs), prob=c(13/62, 36/62, 13/62), replace=T)
    a <- runif(length(yrs), 0.01, 1)
	if(period=="historical" & all(1950:2013 %in% yrs)) a[yrs >= 1950 & yrs <= 2013] <- d.sub$Coef
    if(period!="historical") a <- d.sub$Coef
}

# @knitr func
f <- function(i, a, b=NULL, type="coef", outDir, files, flam.min=NULL, f_of_xy=NULL, cp.origin=NULL, cp.new=NULL){
	r <- raster(files[i])
	if(!is.null(cp.origin)) writeRaster(r, file.path(cp.origin, files[i]), datatype="FLT4S", overwrite=T)
	if(!is.null(flam.min)) r[r < flam.min] <- flam.min
	if(type=="coef"){
		r <- a[i]*r
	} else if(type=="year"){
		if(is.null(b)) stop("b cannot be NULL if type='year'. Change to type='coef' or provide a raster brick b.")
		b.yrs <- as.numeric(substr(names(b), 2, 5))
		ind <- which(b.yrs==a[i])
		if(is.null(f_of_xy)) { r2 <- subset(b, ind); r <- r2*r } else f_of_xy(x=subset(b, ind), y=r)
	}
	r <- round(r, 8)
	writeRaster(r, file.path(outDir, files[i]), datatype="FLT4S", overwrite=T)
	if(!is.null(cp.new)) writeRaster(r, file.path(cp.new, files[i]), datatype="FLT4S", overwrite=T)
	print(i)
}

# @knit run
if(lightning) mclapply(1:length(files), f, a=a, b=kde.maps, type="year", outDir=outDir, files=files, cp.origin=outDir2a, cp.new=outDir2b, mc.cores=32)
if(!lightning) mclapply(1:length(files), f, a=a, outDir=outDir, files=files, cp.origin=outDir2a, cp.new=outDir2b, mc.cores=32)