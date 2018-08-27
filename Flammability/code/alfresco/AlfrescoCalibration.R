##################################################################################################################
#### Basic plots for classic statewide AK (Seward Peninsula + Interior) or Noatak domain ALFRESCO Calibration ####
##################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   07/31/2015        ####

# @knitr alf_calib
comArgs <- commandArgs(TRUE)
print(comArgs)
if(length(comArgs>0)){
	arg.mat <- do.call("rbind",strsplit(comArgs,"="))
	options(warn=-1); arg.char <- which(is.na(as.numeric(arg.mat[,2]))); options(warn=0)
	if(length(arg.char>0)) arg.mat[arg.char,2] <- paste("'",arg.mat[arg.char,2],"'",sep="")
	eval(parse(text=apply(arg.mat,1,paste,collapse="=")))
    print(arg.mat)
    print(ls())
}

if(exists("main")) dir.create(mainDir <- main, showWarnings=F) else stop("must provide 'main' directory")
if(exists("input")) dir.create(mainDir <- input, showWarnings=F) else stop("must provide 'input' directory")
if(exists("out")) dir.create(outDir <- out, showWarnings=F) else stop("must provide 'out' directory")
if(!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if(period=="historical") yr.start <- 1950 else yr.start <- baseline.year
if(exists("yr.end")) yrs <- yr.start:yr.end else stop("must provide 'baseline.year' and 'yr.end'")
if(substr(tolower(alf.domain),1,6)=="statew") alf.domain <- "Statewide" else if(substr(tolower(alf.domain),1,6)=="noatak") alf.domain <- substr(alf.domain,1,6)
if(!exists("n.sims")) n.sims <- 32

# @knitr func_fsByVeg
fsByVeg <- function(i, v, f){
	v[v!=i] <- NA
	x <- f[!is.na(v) & !is.na(f)]
	if(length(x)) return(data.table(Vegetation=i, FS=sort(as.numeric(tapply(x, x, length))))) else return(NULL)
}

# @knitr func_fsByRepEmp
fsByRepEmp <- function(i, b, vid, v.veg, yrs){
	v.fid <- getValues(subset(b, i))
	if(all(is.na(v.fid))) return(NULL)
	dl <- lapply(vid, fsByVeg, v=v.veg, f=v.fid)
	d <- rbindlist(dl)
	d[, Year:=yrs[i]]
	d[, Source:="Observed"]
    d[, Replicate:="Observed"]
	setcolorder(d, names(d)[c(4,5,1,3,2)])
	d
}

# @knitr empirical_data_setup
library(raster)
rasterOptions(tmpdir="/big_scratch/shiny", chunksize=10e10, maxmemory=10e11)
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(r)
v.veg[v.veg==3 | v.veg==4] <- 2 # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")

# @knitr run
# Process empirical data
library(data.table)
library(parallel)
n.cores <- 32
d.fs.veg <- mclapply(1:nlayers(b.fid), fsByRepEmp, b=b.fid, vid=vid, v.veg=v.veg, yrs=yrs.hist.all, mc.cores=n.cores)
d.fs.veg <- rbindlist(d.fs.veg)
d.fs.veg[, Vegetation:=v.names[Vegetation]]

lapply(paste0("/big_scratch/mfleonawicz/Alf_Files_20121129/alfresco/", c("CABvsTimePlot.R", "histPrep.R", "AByearPlot.R", "fireSizePlot.R", "CABvsFSPlot.R")), source)

#### Collect ALFRESCO data
alf.fse <- as.matrix(read.table(file.path(mainDir, "FireSizeEvents.txt"), header=T))
fire.reps <- sort(unique(alf.fse[,2]))
if(length(fire.reps)!=n.sims) warning("Not all replicates have fire activity.")
alf.fs <- as.matrix(read.table(file.path(mainDir,"FireSize.txt"),skip=1))[,2:(n.sims + 1)]

# these functions use hardcoded inputs from another script/workspace
CABvsTimePlot(yrs, baseline.year=baseline.year, d.obs.fs=d.fs.veg, period=period) 
AByearPlot(alf.fs, d.obs.fs=d.fs.veg, yrs, domain="total.ab.ha", domain.name="total", baseline=baseline.year, period=period)
fireSizePlot(yrs, d.obs.fs=d.fs.veg, period=period)
CABvsFSPlot(yrs, d.obs.fs=d.fs.veg, period=period, max.years=length(1950:2013))

save.image(file.path(outDir, "postProcess.RData"))

sink(file=file.path(outDir, "message.txt"))
cat(
"This message comes from the shiny user directory.\n
See preliminary ALFRESCO output figures [attached].\n"
)
sink()
