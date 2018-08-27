###############################################################################################
#### This R script preps for generating climate-driven gradient-boosted flammability maps. ####
###############################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   12/09/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")

library(rgdal)
library(raster)
rasterOptions(chunksize=10e9,maxmemory=10e10)

setwd("/atlas_scratch/mfleonawicz/projects/Flammability/workspaces")
dir.create(outDir <- file.path("gbmFlammability"), showWarnings=F)

# @knitr eco_veg
# If exluding ecoregions
rm.eco <- T
ecoreg <- raster(as.matrix(read.table("../data/ecoreg_mark_mask_zero.txt", skip=6, header=F)))
drop.ind <- Which(ecoreg==4,cells=T)
if(rm.eco) eco.ind <- values(Which(ecoreg!=0&ecoreg!=4)) else eco.ind <- 1

# Assembling the flammability based on the veg map
r.veg <- raster("../data/alf2005.cavm.merged.030212.tif")
veg <- getValues(r.veg)
if(any(is.na(veg))) veg[is.na(veg)] <- 0
veg.val <- as.numeric(veg!=0)*eco.ind*veg
f.ind <- veg.val==2|veg.val==3|veg.val==4; a.ind <- veg.val==1; s.ind <- veg.val==5; g.ind <- veg.val==6; w.ind <- veg.val==7
land.gray <- as.numeric(veg!=0)*(1-eco.ind)
rm(drop.ind, eco.ind)
ind.names <- c("f.ind", "a.ind", "s.ind", "g.ind", "w.ind") # order: forest, alpine tundra, shrub, graminoid, wetland

# @knitr prep_save
# Prepare and save workspace
if(period=="historical"){
	yrs <- 1950:2013
	tpDir <- file.path("/atlas_scratch/mfleonawicz/Climate_1km_AKstatewide", period, "CRU_TS32")
} else {
	yrs <- 2010:2099
	tpDir <- file.path("/atlas_scratch/mfleonawicz/Climate_1km_AKstatewide", period, model)
}
mo.ind <- 6:8 #1:9
varid <- c("pr", "tas")
pat.mo <- paste0(".*._", c(paste0(0,1:9),10:12), "_.*.tif$")
files.precip <- files.temp <- list()
for(i in mo.ind){
	files.precip[[i]] <- list.files(file.path(tpDir,varid[1]), full=T, pattern=pat.mo[i])
	files.temp[[i]] <- list.files(file.path(tpDir,varid[2]), full=T, pattern=pat.mo[i])
	ind <- which(substr(files.temp[[i]], nchar(files.temp[[i]])-7, nchar(files.temp[[i]])-4) %in% yrs)
	files.precip[[i]] <- files.precip[[i]][ind]
	files.temp[[i]] <- files.temp[[i]][ind]
}

for(i in mo.ind){
	assign(paste0("m.", month.abb[i], "P"), as.matrix(stack(files.precip[[i]])))
	assign(paste0("m.", month.abb[i], "T"), as.matrix(stack(files.temp[[i]])))
	print(i)
}

keep.obj <- c(ls(pattern="^m\\.|\\.ind$"), "r.veg", "veg", "ind.names", "land.gray", "yrs")
save(list=keep.obj, file=paste0(outDir, "/", model, "_", period, "_Jun-AugTP.RData"))
#save(list=keep.obj, file=paste0(outDir, "/", model, "_", period, "_Jan-SepTP.RData"))
