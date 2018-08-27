######################################################################################################################################################
#### This R script generates spatially explicit maps comparing fire rotation period maps for each replicate of an ALFRESCO run with empirical FRP ####
######################################################################################################################################################

# @knitr setup
comArgs <- commandArgs(TRUE)
if(length(comArgs>0)){
        arg.mat <- do.call("rbind",strsplit(comArgs,"="))
        options(warn=-1); arg.char <- which(is.na(as.numeric(arg.mat[,2]))); options(warn=0)
        if(length(arg.char>0)) arg.mat[arg.char,2] <- paste("'",arg.mat[arg.char,2],"'",sep="")
        eval(parse(text=apply(arg.mat,1,paste,collapse="=")))
}
cat(comArgs)
dir.create(outDir <- file.path(out, "FRP"), showWarnings=F)
sink(file=file.path(out, "message.txt"), append=TRUE)
cat("Below you will find a link to a preliminary R Shiny Alfresco FRP/FRI results web application.\n")

library(raster)
library(parallel)
library(data.table)
library(dplyr)

rasterOptions(tmpdir="/big_scratch/shiny", chunksize=10e10, maxmemory=10e11)
mainDir <- file.path(input, "Maps")
dir.create(outDir <- file.path(out, "FRP"), showWarnings=F)
if(!exists("pts")) stop("No coordinates file provided for relative area burned time series extraction.")
if(!exists("buffers")) stop("No buffer(s) provided for relative area burned time series extraction.")
if(!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if(period=="historical") yr.start <- 1950 else yr.start <- baseline.year
if(exists("yr.end")) yrs <- yr.start:yr.end else stop("must provide 'baseline.year' and 'yr.end'")
if(!exists("n.sims")) n.sims <- 32
n.cores <- min(n.sims, 32)

pts <- read.csv(file.path(input,pts))
if(substr(alf.domain,1,6)=="Noatak") pts$ID <- factor(pts$ID, levels=c(paste0(rep(c("", "Shrub_", "Gram_"), each=4), c("Raven", "Uchugrak", "Poktovik", "LittleIsac")), "Fire_RedLake", "Fire_FoxLake"))
#pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)
if(!is.matrix(pts)) stop("No coordinates matrix provided for relative area burned time series extraction")

buffers <- eval(parse(text=buffers))
buffers.labels <- buffers/1000

buffers <- as.list(sort(unique(buffers)))
null.ind <- which(buffers==0)
if(length(null.ind)) buffers[null.ind] <- list(NULL)

propFun <- function(x,...) sum(x,na.rm=T)/length(which(!is.na(x)))
buffer.functions <- buffers
for(i in 1:length(buffers)) if(!is.null(buffers[[i]])) buffer.functions[[i]] <- propFun
if(length(null.ind)) buffer.functions[null.ind] <- list(NULL)

wgs2ak <- function(xy){
	require(rgdal)
	if(class(xy)=="matrix") xy <- data.frame(xy)
	names(xy) <- c("x","y")
	coordinates(xy) <- names(xy)
	proj4string(xy)<- CRS("+proj=longlat +datum=WGS84")
	xy <- coordinates(spTransform(xy,
		CRS=CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")))
}

pts <- wgs2ak(pts)

# @knitr func_modeled
fireEventsFun <- function(k, pts, locs, replicates, source="Modeled", buffer.list=list(NULL), buffer.labels=LETTERS[1:length(buffer.list)], burnable.cells.raster=NULL, mainDir){
	require(raster)
	if(!is.null(burnable.cells.raster)) burnable.cells <- Which(burnable.cells.raster==1)
	reps <- paste0("_",k-1,"_")
	files <- list.files(mainDir,pattern=gsub("expression","",paste(bquote(expression("^FireSc.*.",.(reps),".*.tif$")),collapse="")),full=T)
	yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
	n <- length(yrs)
	ord <- order(yrs)
	files <- files[ord]
	yrs <- yrs[ord]
    d <- vector("list", n)
	for(i in 1:n){
        r <- raster(files[i])
        r[is.na(r)] <- 0
        if(i==1) r <- mask(r, burnable.cells, maskvalue=0)
        r[r>0] <- 1
        if(i==1){
            r.hold <- r
            cells <- vector("list", length(buffer.list))
			for(p in 1:length(buffer.list)){
				tmp <- if(is.null(buffer.list[[p]])) as.list(extract(r, pts, cellnumbers=T)[,1]) else lapply(extract(r, pts, buffer=buffer.list[[p]], cellnumbers=T), function(x) x[,1])
                tmp <- rbindlist(lapply(1:length(tmp), function(m, x, locs) data.table(Location=locs[m], Cell=x[[m]]), x=tmp, locs=locs))
                cells[[p]] <- mutate(tmp, Buffer_km=buffer.labels[p])
			}
            cells <- rbindlist(cells)
        } else r.hold <- r.hold + r
        cells <- mutate(cells, Value=r[Cell])
        d[[i]] <- group_by(cells, Buffer_km, Location) %>% summarise(Value=mean(Value, na.rm=TRUE)) %>% mutate(Year=yrs[i])
	}
    d <- rbindlist(d) %>% mutate(Source=factor(source, levels=c("Observed", "Modeled")), Replicate=factor(replicates[k], levels=unique(c("Observed", replicates)))) %>%
        group_by(Source, Replicate, Buffer_km, Location, Year) %>% arrange(Source, Replicate, Buffer_km, Location, Year) %>%
        setcolorder(c("Source", "Replicate", "Buffer_km", "Location", "Year", "Value"))
	list(rasters=r.hold, points=d, years=n, years.vec=yrs)
}


# @knitr func_empirical
fireEventsFunEmpirical <- function(b, pts, locs, replicates="Observed", source="Observed", buffer.list=list(NULL), buffer.labels=LETTERS[1:length(buffer.list)], burnable.cells.raster=NULL){
	require(raster)
	if(!is.null(burnable.cells.raster)) burnable.cells <- Which(burnable.cells.raster==1)
	n <- nlayers(b)
	d <- vector("list", n)
	for(i in 1:n){
		r <- subset(b,i)
		r[is.na(r)] <- 0
        if(i==1) r <- mask(r, burnable.cells, maskvalue=0)
        r[r>0] <- 1
        if(i==1){
            r.hold <- r
            cells <- vector("list", length(buffer.list))
			for(p in 1:length(buffer.list)){
				tmp <- if(is.null(buffer.list[[p]])) as.list(extract(r, pts, cellnumbers=T)[,1]) else lapply(extract(r, pts, buffer=buffer.list[[p]], cellnumbers=T), function(x) x[,1])
                tmp <- rbindlist(lapply(1:length(tmp), function(m, x, locs) data.table(Location=locs[m], Cell=x[[m]]), x=tmp, locs=locs))
                cells[[p]] <- mutate(tmp, Buffer_km=buffer.labels[p])
			}
            cells <- rbindlist(cells)
        } else r.hold <- r.hold + r
        cells <- mutate(cells, Value=r[Cell])
        d[[i]] <- group_by(cells, Buffer_km, Location) %>% summarise(Value=mean(Value, na.rm=TRUE)) %>% mutate(Year=yrs[i])
	}
	d <- rbindlist(d) %>% mutate(Source=factor(source, levels=c("Observed", "Modeled")), Replicate=factor(replicates[1], levels=unique(c("Observed", replicates)))) %>%
        group_by(Source, Replicate, Buffer_km, Location, Year) %>% arrange(Source, Replicate, Buffer_km, Location, Year) %>%
        setcolorder(c("Source", "Replicate", "Buffer_km", "Location", "Year", "Value"))
	list(rasters=r.hold, points=d, years=n)
}

# @knitr empirical_data_setup
source("/big_scratch/shiny/obs_fire_setup.R")
r.burnable <- Which(r>0)

# @knitr run
# Empirical observations and simulation replicates
reps.emp <- "Observed"
reps.alf <- paste0("Rep_",c(paste0(0,0,0:9), paste0(0,10:99), 100:999))[1:n.sims]
# Process empirical data
out.emp <- fireEventsFunEmpirical(b=result, pts=pts, locs=locs, replicates=c(reps.emp, reps.alf), buffer.list=buffers, buffer.labels=buffers.labels, burnable.cells.raster=r.burnable)
# Process modeled data
n.cores <- min(n.sims, 32)
print(paste("Process modeled fire scar data from entire Alfresco run. Time:"))
system.time( out.alf <- mclapply(1:n.cores, fireEventsFun, pts=pts, locs=locs, replicates=reps.alf, buffer.list=buffers, buffer.labels=buffers.labels, burnable.cells.raster=r.burnable, mainDir=mainDir, mc.cores=n.cores) )

# @knitr FRP_maps
alf.yrs <- out.alf[[1]][[4]]
zlm <- c(0,out.alf[[1]][[3]])
dir.create(file.path(outDir,"Maps_noBuffer/PNGs"), recursive=T, showWarnings=F)

FRPmapsNoBuffer <- function(i, alf.data, emp.data, odir, domain, alf.yrs, emp.yrs){
	if(substr(domain,1,6)=="Noatak") domain <- "Noatak"
	if(substr(domain,1,6)=="Statew") domain <- "Statewide"
	writeRaster(alf.data[[i]][[1]], paste0(odir,"/Maps_noBuffer/FRP_",domain,"_Rep",i-1,".tif"),datatype="FLT4S",overwrite=T)
	pngname <- paste0(odir,"/Maps_noBuffer/PNGs/FRP_",domain,"_Rep",i-1,".png")
	if(domain=="Noatak"){
		png(pngname, width=1200, height=1370)
		layout(matrix(1:2,2,1))
	} else if(domain=="Statewide") {
		png(pngname, width=2400, height=1370)
		layout(matrix(1:2,1,2))
	}
	plot(round(alf.data[[i]][[3]]/alf.data[[i]][[1]]), col=heat.colors(20), zlim=zlm, main=paste0(domain, " ", alf.yrs[1], "-", tail(alf.yrs,1), " replicate ",i-1," FRP"))
	plot(shp, bg='transparent', add=TRUE)
	plot(fah, bg='transparent', add=TRUE)
	plot(round(length(emp.yrs)/emp.data), col=heat.colors(20), zlim=zlm, main=paste0(domain, " ", emp.yrs[1], "-", tail(emp.yrs,1), " observed FRP"))
	plot(shp, bg='transparent',add=TRUE)
	plot(fah, bg='transparent',add=TRUE)
	dev.off()
	if(i==1) file.copy(pngname, file.path(dirname(odir), basename(pngname)))
}

print(paste("Create 2-panel (empirical and modeled) 1-km no-buffer FRP maps. Time:"))
print(system.time( mclapply(1:length(out.alf), FRPmapsNoBuffer, alf.data=out.alf, emp.data=result2, odir=outDir, domain=alf.domain, alf.yrs=alf.yrs, emp.yrs=yrs.hist.all, mc.cores=n.cores) ))

# @knitr FRP_app_setup
# Concatenate all elements in nested list x. Inner to outer levels: year, location, buffer, replicate
dfPrepFun <- function(x, multiple.reps=T){
	if(!multiple.reps) x <- list(x)
	x <- lapply(x, "[[", 2)
	rbindlist(x) %>% group_by(Source, Replicate, Buffer_km, Location, Year)
}

# Organize observed data
d.emp <- dfPrepFun(out.emp, multiple.reps=F)
d.emp %>% group_by(Source, Replicate, Buffer_km, Location) %>% summarise(FRP=length(Year)/sum(Value)) -> d2.emp

# Organize modeled data
d <- dfPrepFun(out.alf)
d %>% group_by(Source, Replicate, Buffer_km, Location) %>% summarise(FRP=length(Year)/sum(Value)) -> d2

# Additional objects to transport to app
buffersize <- unique(d.emp$Buffer_km)
obs.years.range <- range(d.emp$Year)
mod.years.range <- range(alf.yrs)

# Assemble final data frames
d %>% bind_rows(d.emp) %>% setorder(Replicate, Buffer_km, Location) %>% data.table -> rab.dat
rm(d,d.emp)
dummy <- capture.output( gc() )
d2 %>% bind_rows(d2.emp) %>% setorder(Replicate, Buffer_km, Location) %>% data.table -> frp.dat
rm(d2,d2.emp)
dummy <- capture.output( gc() )

rab.dat[, Source:="Observed"]
rab.dat[Replicate!="Observed", Source:="Modeled"]
frp.dat[, Source:="Observed"]
frp.dat[Replicate!="Observed", Source:="Modeled"]

# Make Fire Return Interval data table
# no fires = one FRI of period length; one fire = one FRI of time from fire to period end
save(rab.dat, file=paste0(outDir,"/TESTME.RData"))
censor <- function(x, y) if(all(y==0)) length(x) else if(all(is.na(x))) as.integer(length(y) - which(y!=0)) else as.integer(x[!is.na(x)])
fri.dat <- filter(rab.dat, Value!=0) %>% group_by(Source, Replicate, Buffer_km, Location) %>% mutate(FRI=c(NA, diff(Year)))
#fri.dat <- left_join(rab.dat, fri.dat) %>% group_by(Source, Replicate, Buffer_km, Location) %>% summarise(FRI=censor(FRI, Value)) %>% data.table # deprecated
fri.dat <- left_join(rab.dat, fri.dat) %>% group_by(Source, Replicate, Buffer_km, Location) %>% do(data.frame(FRI=censor(.$FRI, .$Value))) %>% data.table

# Noatak-specific
if(substr(alf.domain,1,6)=="Noatak"){
    lev <- c("RedLake", "Raven", "Uchugrak", "Poktovik", "LittleIsac", "FoxLake")
    rab.dat[, LocGroup:="Origin"]
    rab.dat[substr(Location,1,5)=="Gram_", LocGroup:="Graminoid"]
    rab.dat[substr(Location,1,5)=="Shrub", LocGroup:="Shrub"]
    rab.dat[substr(Location,1,5)=="Fire_", LocGroup:="Fire"]
    rab.dat <- mutate(rab.dat, LocGroup=factor(LocGroup, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Location=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Location))), levels=lev))
    frp.dat[, LocGroup:="Origin"]
    frp.dat[substr(Location,1,5)=="Gram_", LocGroup:="Graminoid"]
    frp.dat[substr(Location,1,5)=="Shrub", LocGroup:="Shrub"]
    frp.dat[substr(Location,1,5)=="Fire_", LocGroup:="Fire"]
    frp.dat <- mutate(frp.dat, LocGroup=factor(LocGroup, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Location=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Location))), levels=lev))
    fri.dat[, LocGroup:="Origin"]
    fri.dat[substr(Location,1,5)=="Gram_", LocGroup:="Graminoid"]
    fri.dat[substr(Location,1,5)=="Shrub", LocGroup:="Shrub"]
    fri.dat[substr(Location,1,5)=="Fire_", LocGroup:="Fire"]
    fri.dat <- mutate(fri.dat, LocGroup=factor(LocGroup, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Location=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Location))), levels=lev))
}

# Load/save objects in a workspace file to be transported to app
dom <- if(substr(tolower(alf.domain),1,6)=="noatak") "Noatak" else if(substr(tolower(alf.domain),1,6)=="statew") "Statewide"
load(paste0(out, "/fsByVeg_df_", dom, ".RData")) # assumed to have run fsByVeg.R
prefix <- ifelse(group.name=="none", "RAB_FRP", paste0(run.name, "_RAB_FRP"))
ws <- ifelse(group.name=="none",
		paste0(outDir,"/",prefix,"_Emp_",yrs.hist.all[1],"_",tail(yrs.hist.all,1),"_Alf_",alf.yrs[1],"_",tail(alf.yrs,1),".RData"),
		paste0(outDir,"/",prefix,".RData")
	)
save(d.fs, buffersize, obs.years.range, mod.years.range, rab.dat, frp.dat, fri.dat, file=ws)
Sys.sleep(0.1)

# @knitr save
# Create new app from template and copy data to app
app.name <- ifelse(group.name=="none", "alf_results", paste0(group.name, "_alf_results"))
alfDir <- "/var/www/shiny-server/shiny-apps/alfresco" # Alfresco apps directory
appDir <- file.path(alfDir, app.name) # App directory to use this run's data
system(paste("ssh eris.snap.uaf.edu mkdir -p", paste0(appDir, "/www"))) # Make directory
system(paste("ssh eris.snap.uaf.edu chmod 2775", appDir)) # Set permissions
system(paste0("ssh eris.snap.uaf.edu cp /var/www/shiny-server/shiny-apps/alfoutdev/*.R ", appDir, "/")) # Copy template app files
system(paste0("ssh eris.snap.uaf.edu cp /var/www/shiny-server/shiny-apps/alfoutdev/www/* ", appDir, "/www/"))
system(paste0("scp ", ws, " eris.snap.uaf.edu:", file.path(appDir, basename(ws)))) # Copy Alfresco run workspace file to new app directory

cat("Alfresco output results:\n\n")
app.url <- paste0("http://eris.snap.uaf.edu/shiny-apps/alfresco/", app.name, "/")
cat(app.url)

sink()
Sys.sleep(0.1)
