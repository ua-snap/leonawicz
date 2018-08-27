##############################################################################################################
#### This R script calculates CRU 3.2 temperature and precipitation means or samples by vegetation class. ####
##############################################################################################################

# @knitr setup
comargs <- (commandArgs(TRUE))
if(length(comargs)) for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("samples")) samples <- TRUE
if(!exists("vc")) vc <- "all"
if(!exists("n") & samples) stop("Must provide n if samples=TRUE.")
if(!is.logical(samples)) stop("Argument 'samples' must be logical.")
if(!exists("firemask")) firemask <- TRUE
if(!(vc %in% c("all", "cavm", "none"))) stop("Argument 'vc' must be one of 'all', 'cavm', or 'none'.")

library(raster)
library(data.table)
library(reshape2)
library(parallel)

setwd("/atlas_scratch/mfleonawicz/projects/Flammability/workspaces")
dataDir <- "/atlas_scratch/mfleonawicz/Climate_1km_AKstatewide"

r.veg <- raster("../data/alf2005.cavm.merged.030212.tif")
veg.vec <- getValues(r.veg)
sort(unique(veg.vec))
r.fire <- Which(raster("../data/historicalFireObs/firescarlayer_total_observed_Statewide_lightning_1950_2013.tif") != 0)
fire.vec <- getValues(r.fire)
rm.eco <- T
ecoreg <- raster(as.matrix(read.table("../data/ecoreg_mark_mask_zero.txt", skip=6, header=F)))
drop.ind <- Which(ecoreg==4,cells=T)
if(rm.eco) eco.ind <- values(Which(ecoreg!=0&ecoreg!=4)) else eco.ind <- 1
veg.vec <- as.numeric(veg.vec!=0)*eco.ind*veg.vec
if(firemask) veg.vec <- fire.vec*veg.vec

if(vc=="none"){
    veg.vec[veg.vec > 1] <- 1 # not conditional on veg, e.g., for lightning analyses
} else { # veg-specific, e.g., for flammability analyses
    veg.vec[veg.vec==3|veg.vec==4] <- 2 # for forest
    if(vc=="cavm") veg.vec[veg.vec==6|veg.vec==7] <- 5 # "cavm" all three shrub, graminoid, wetland combined
}

veg.vals <- if(vc=="cavm") 5 else if(vc=="all") c(1,2,5,6,7) else if(vc=="none") 1
veg.names <- if(vc=="cavm") "cavm" else if(vc=="all") c("tundra","forest","shrub","graminoid","wetland") else if(vc=="none") "region"
modnames <- "CRU_TS32"
scenario <- "historical"
path <- file.path(dataDir, scenario, modnames, c("pr", "tas"))
dir.create(wsDir <- "tpByVeg", showWarnings=F)

yrs <- 1950:2013
n.cores <- min(length(yrs), 32)

# @knitr func
f <- function(k, path, veg.vec, veg.vals, veg.names, samples=FALSE, n=100, seed=NULL){
    if(!samples) n <- 1
	paths <- list.files(path,full=T)
	ind <- which(as.numeric(substr(paths,nchar(paths)-7,nchar(paths)-4))==k)
	paths <- paths[ind]
	precip.paths <- paths[1:12]
	temp.paths <- paths[13:24]
    precip.tmp <- getValues(stack(precip.paths, quick=T))
    temp.tmp <- getValues(stack(temp.paths, quick=T))
    if(samples) na.rows <- unique(c(which(is.na(precip.tmp), arr.ind=TRUE)[,1], which(is.na(temp.tmp), arr.ind=TRUE)[,1]))
	mp <- mt <- c()
	nv <- length(veg.vals)
	if(!is.null(seed)) set.seed(seed)
    if(!samples){
        for(j in 1:nv){
            mp <- cbind(mp, round(colMeans(precip.tmp[veg.vec==veg.vals[j],],na.rm=T)))
            mt <- cbind(mt, round(colMeans(temp.tmp[veg.vec==veg.vals[j],],na.rm=T), 1))
        }
    } else if(samples) {
        for(j in 1:nv){
            ind2 <- veg.vec==veg.vals[j] & !(1:length(veg.vec) %in% na.rows)
            pre <- precip.tmp[ind2,]
            tas <- temp.tmp[ind2,]
            samp <- sample(1:nrow(pre), n)
            if(j==1) print(paste("sample number one is", samp[1]))
            mp <- cbind(mp, as.numeric(pre[samp,]))
            mt <- cbind(mt, as.numeric(tas[samp,]))
        }
    }
	mp <- data.table(mp)
	mt <- data.table(mt)
	setnames(mp, veg.names)
    setnames(mt, veg.names)
	mp$Month <- mt$Month <- rep(month.abb, each=n)
	mp$Var <- "Precipitation"
	mt$Var <- "Temperature"
	d <- rbind(mp, mt)
	d <- cbind(Year=k, d)
	d <- melt(d, id.var=c("Year", "Month", "Var"), variable.name="Vegetation", value.name="Val")
    if(samples) d[, Obs := 1:n] else d[, Obs := 0]
    print(k)
	d
}

# @knitr run
set.seed(55)
if(!samples) f.out <- mclapply(yrs, f, path=path, veg.vec=veg.vec, veg.vals=veg.vals, veg.names=veg.names, mc.cores=n.cores)
if(samples)  f.out <- mclapply(yrs, f, path=path, veg.vec=veg.vec, veg.vals=veg.vals, veg.names=veg.names, samples=TRUE, n=n, seed=55, mc.cores=n.cores)
d <- rbindlist(f.out)
d[, Scenario := "historical"]
d[, Model := "CRU32"]
setcolorder(d, c("Scenario", "Model", names(d)[1:(ncol(d)-2)]))
if(vc=="cavm") d.cavm <- d else if(vc=="none") d.region <- d

# @knitr save
lab <- if(samples) paste0("samples", n) else "means"
if(vc=="cavm"){
    save(d.cavm, file=paste0(wsDir, "/tpByVeg_", lab, "_CRU32_cavm.RData"))
} else if(vc=="all") {
    save(d, file=paste0(wsDir, "/tpByVeg_", lab, "_CRU32_individual.RData"))
} else if(vc=="none") {
    save(d.region, file=paste0(wsDir, "/tpByVeg_", lab, "_CRU32_regional.RData"))
}
