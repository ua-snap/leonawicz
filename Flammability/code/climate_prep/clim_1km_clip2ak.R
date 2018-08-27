############################################################################################
#### This R script clips 1-km AK-CAN geotiffs to the classic statewide ALFRESCO domain. ####
############################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   12/09/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period") || !(period %in% c("historical", "projected"))) stop("Must specify period as historical or projected.")
if(!exists("cru")) cru <- FALSE
if(cru & period=="projected") stop("Period cannot be projected if cru is TRUE.")
if(!is.logical(cru)) stop("Argument 'cru' must be logical.")

library(parallel)
library(raster)

msk <- raster("/atlas_scratch/mfleonawicz/projects/Flammability/data/alf2005.cavm.merged.030212.tif")
e <- extent(msk)

mainDir <- "/atlas_scratch/mfleonawicz/Climate_1km"
if(cru){
	varid <- c("pr", "tas")
	rcp <- "historical"
	model <- "CRU_TS32"
} else if(period=="projected"){
	varid <- rep(c("pr","tas"), 15)
	rcp <- rep(rep(paste0("rcp",c(45,60,85)), each=2), 5)
	model <- rep(list.files(file.path(mainDir,"rcp60")), each=6)
} else if(period=="historical"){
    varid <- rep(c("pr","tas"), 5)
    rcp <- "historical"
    model <- list.files(file.path(mainDir, "historical"))
    model <- rep(model[model != "CRU_TS32"], each=2)
}

subDir <- file.path(mainDir, rcp, model, varid)
outDir <- file.path("/atlas_scratch/mfleonawicz/Climate_1km_AKstatewide", rcp, model, varid)
for(i in 1:length(outDir)) dir.create(outDir[i], recursive=T, showWarnings=F)

# @knitr func
f <- function(i, files=NULL, subDir, outDir, msk){
	if(is.null(files)){
		files <- list.files(subDir[i], full=T, pattern=".tif$")
		ind <- 1:length(files)
		ind2 <- i
	} else {
		ind <- i
		ind2 <- 1
	}
	for(j in ind){
		r <- raster(files[j])
		r <- crop(r, msk)
		r <- mask(r, msk)
		extent(r) <- e
		writeRaster(r, file.path(outDir[ind2], basename(files[j])), datatype="FLT4S", overwrite=T)
		print(length(files) - j)
	}
	return()
}

# @knitr run
for(k in 1:length(subDir)){
    files <- list.files(subDir[k], full=T, pattern=".tif$")
    mclapply(1:length(files), f, files=files, subDir=NULL, outDir=outDir[k], msk=msk, mc.cores=32)
}
