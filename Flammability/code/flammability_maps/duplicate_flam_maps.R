#######################################################################################################################
#### This R script cyclically duplicates flammability maps to simulate a backcast for use in longer ALFRESCO runs. ####
#######################################################################################################################

# @knitr duplicate
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("samples")) samples <- TRUE
if(!exists("mapset")) stop("Argument 'mapset' not passed at command line.")
if(!exists("out")) stop("Argument 'out' not passed at command line.")
if(!exists("n")) n <- 40
if(!exists("yearloop")) stop("Argument 'yearloop' not passed at command line.")

library(parallel)
verDir <- if(samples) "samples_based" else "means_based"
mainDir <- file.path("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability", verDir, period, model, mapset)
dir.create(outDir <- file.path("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability", verDir, period, model, out), showWarnings=FALSE)
setwd(outDir)

files <- list.files(pattern="\\.tif$", full=TRUE)
if(length(files)){
	files.years <- as.numeric(substr(sapply(strsplit(files, "flamm_"), "[", 2), 1, 4))
	ind <- which(files.years %in% yearloop)
	files <- files[ind]
} else {
	originals <- list.files(file.path("..", mapset), pattern="\\.tif$", full=TRUE)
	files.years <- as.numeric(substr(sapply(strsplit(originals, "flamm_"), "[", 2), 1, 4))
	ind <- which(files.years %in% yearloop)
	file.copy(originals[ind], basename(originals[ind]))
	files <- list.files(pattern="\\.tif$", full=TRUE)
}

f1 <- function(k, x, n, period){
	x <- x[k]
	yr <- as.numeric(substr(x, nchar(x)-7, nchar(x)-4))
	yr <- yr - length(period)*(1:n)
	x.out <- paste0(substr(x, 1, nchar(x)-8), yr, ".tif")
	for(i in 1:n) if(yr[i] >= 1) file.copy(x, x.out[i]) # allow for one negative year for testing
	return()
}

mclapply(1:length(files), f1, x=files, n=n, period=yearloop, mc.cores=32)
