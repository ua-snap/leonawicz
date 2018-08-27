# @knitr script
# Alfresco 2005 vegetation input map
library(rasterVis)
setwd("C:/github/Flammability/data")
outDir <- "../plots/alfInputs"
shp <- shapefile("shapefiles/noa_basin2/Noa_basin2.shp")
r.veg <- raster("alf2005.cavm.merged.030212.tif")
r.veg[r.veg==0] <- NA
r.veg <- mask(crop(r.veg, shp), shp)

pts <- read.csv("C:/github/shiny-apps/run_alfresco/pts/Noatak_lake_locations.csv")
pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)

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

cbpal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
at.vals <- seq(0.5,7.5,by=1)
colkey <- list(at=at.vals, labels=list(labels=c("Alpine", "B. Spruce", "W. Spruce", "Deciduous", "Shrub", "Graminoid", "Wetland"), at=at.vals + 0.5))

revRasterTheme <- function (pch = 19, cex = 0.7, region=cbpal, ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

r.pts <- r.veg
r.pts[] <- NA
r.pts[cellFromXY(r.pts, pts)] <- 1

png(file.path(outDir, "noatak_2005vegInputMap.png"), height=1600, width=3200, res=200)
p <- levelplot(r.veg, main="ALFRESCO vegetation input map", par.settings=revRasterTheme, margin=F, at=at.vals, colorkey=colkey) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

# Alfresco 1950-2013 CRU 3.2 temperature and precipitation map summaries
library(data.table)
library(dplyr)
library(ggplot2)
library(rasterVis)
rasterOptions(chunksize=10e10,maxmemory=10e11)
setwd("/big_scratch/mfleonawicz/Climate_1km_AKstatewide/historical/CRU_TS32/")
dir.create(outDir <- "/atlas_scratch/mfleonawicz/projects/Flammability/plots/alfInputs", recur=T, showWarnings=F)
shp <- shapefile("/atlas_scratch/mfleonawicz/projects/Flammability/data/shapefiles/noa_basin2/Noa_basin2.shp")
r.veg <- raster("/atlas_scratch/mfleonawicz/projects/Flammability/data/alf2005.cavm.merged.030212.tif")
r.veg[r.veg==0] <- NA
r.veg <- mask(crop(r.veg, shp), shp)

files <- list.files("tas", full=T)
mos <- as.numeric(substr(files, nchar(files)-10, nchar(files)-9))
yrs <- as.numeric(substr(files, nchar(files)-7, nchar(files)-4))
files <- files[yrs>=1950 & yrs<=2013 & mos %in% 8]
r.t.all <- mask(crop(stack(files), shp), shp)
r.t <- calc(r.t.all, mean)
files <- list.files("pr", full=T)
files <- files[yrs>=1950 & yrs<=2013 & mos %in% 7]
r.p.all <- mask(crop(stack(files), shp), shp)
r.p <- calc(r.p.all, mean)

pts <- read.csv("/atlas_scratch/mfleonawicz/projects/Flammability/data/pts/Noatak_lake_locations.csv")
#pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)

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

revRasterTheme <- function (pch = 19, cex = 0.7, region=rev(brewer.pal(9, "RdBu")), ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

r.pts.fire <- r.pts.gram <- r.pts.shrub <- r.pts <- r.t
r.pts[] <- r.pts.shrub[] <- r.pts.gram[] <- r.pts.fire[] <- NA
r.pts[cellFromXY(r.pts, pts[1:4,])] <- 1
r.pts.shrub[cellFromXY(r.pts.shrub, pts[5:8,])] <- 1
r.pts.gram[cellFromXY(r.pts.gram, pts[9:12,])] <- 1
r.pts.fire[cellFromXY(r.pts.fire, pts[13:14,])] <- 1

png(file.path(outDir, "noatak_tas_Aug_1950-2013mean.png"), height=1600, width=3200, res=200)
p <- levelplot(r.t, main="ALFRESCO implicit JJA mean temperature input 1950-2013", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_pr_Jul_1950-2013mean.png"), height=1600, width=3200, res=200)
p <- levelplot(r.p, main="ALFRESCO implicit JJA total precipitation input 1950-2013", par.settings=GrTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='greenyellow'))
print(p)
dev.off()

e <- extract(stack(r.t, r.p), pts, buffer=10000)
eq <- lapply(e, function(x) apply(x, 2, function(y) quantile(y, c(0.1, 0.9))))

d <- rbindlist(lapply(1:length(e),
    function(i,x, locs) data.frame(Lake=locs[i], Var=rep(c("Temperature", "Precipitation"), each=nrow(x[[i]])), Val=as.numeric(x[[i]])),
    x=e, locs=locs))
d <- mutate(d, Location="Origin", Var=factor(Var, levels=c("Temperature", "Precipitation")))
d[substr(Lake,1,4)=="Gram", Location:="Graminoid"]
d[substr(Lake,1,5)=="Shrub", Location:="Shrub"]
d[substr(Lake,1,4)=="Fire", Location:="Fire"]
d <- mutate(d, Location=factor(Location, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Lake=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Lake))), levels=c("Raven", "Uchugrak", "Poktovik", "LittleIsac", "LakeWest", "LakeEast")))

cbpal <- c("#000000", "#D55E00", "#0072B2", "#FF0000")

png(file.path(outDir, "noatak_10kmClimSpace.png"), height=1600, width=3200, res=200)
ggplot(d, aes(x=Lake, y=Val, colour=Location)) + geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(~ Var, scales="free") +
    scale_colour_manual("Location", values=cbpal) +
    labs(title="1950-2013 climate space by lake using 10th and 90th quantiles of a 10-km buffer") + theme(legend.position="bottom")
dev.off()

png(file.path(outDir, "noatak_10kmClimSpace2.png"), height=2400, width=3200, res=300)
ggplot(d, aes(x=Lake, y=Val)) + geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(Var ~ Location, scales="free") +
    scale_colour_manual("Location", values=cbpal) +
    labs(title="1950-2013 climate space by lake using 10th and 90th quantiles of a 10-km buffer") + theme(legend.position="bottom")
dev.off()

s.newlakes <- do.call(stack, c(lapply(1:(length(eq)/3), f, e=eq, rtp=stack(r.t, r.p), rv=r.veg, v=5), lapply(1:(length(eq)/3), f, e=eq, rtp=stack(r.t, r.p), rv=r.veg, v=6)))
names(s.newlakes) <- paste0(locs[1:4], rep(c("_shrub", "_graminoid"), each=4))
at.vals <- c(0.9, 1, 1.1)
colkey <- list(at=at.vals, labels=list(labels=c("", "Available\nlocations", ""), at=at.vals), x=3, y=1, height=0.2, tck=0)
revRasterTheme <- function (pch = 19, cex = 0.7, region=c("gray40"), ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

png(file.path(outDir, "noatak_newLakeLocs_1090pct_10km.png"), height=1600, width=3200, res=200)
p <- levelplot(s.newlakes, main="Potential lake sites by vegetation type and origin lake climate space", par.settings=revRasterTheme, margin=F, ay=at.vals, colorkey=colkey) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.shrub, sp=T), col=cbpal[2])) +
    layer(sp.points(rasterToPoints(r.pts.gram, sp=T), col=cbpal[3])) +
    layer(sp.polygons(shp, col='black'))
print(p)
dev.off()


# Alfresco 1950-2013 CRU 3.2-based flammability map summaries
library(data.table)
library(dplyr)
library(ggplot2)
library(rasterVis)
rasterOptions(chunksize=10e10,maxmemory=10e11)
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")
dir.create(outDir <- "/atlas_scratch/mfleonawicz/projects/Flammability/plots/alfInputs", recur=T, showWarnings=F)
shp <- shapefile("/atlas_scratch/mfleonawicz/projects/Flammability/data/shapefiles/noa_basin2/Noa_basin2.shp")
#r.frp <- calc(stack(list.files("/big_scratch/shiny/Runs_Noatak/paul.duffy_at_neptuneinc.org/m3TL_31200s_0023775i_historical_CRU32/FRP/Maps_noBuffer", pattern="\\.tif$", full=T)), mean)
r.frp <- raster("/atlas_scratch/mfleonawicz/projects/Flammability/data/alfExOutputs/noatak_frp_64repMean.tif")
r.frp <- mask(crop(r.frp, shp), shp)
names(r.frp) <- "GBM3_FRP"
r.veg <- raster("/atlas_scratch/mfleonawicz/projects/Flammability/data/alf2005.cavm.merged.030212.tif")
r.veg[r.veg==0] <- NA
r.veg <- mask(crop(r.veg, shp), shp)
files <- list.files("3m100n_cavmDistTrunc_loop_L", full=T)
yrs <- as.numeric(sub(".tif", "", gsub("gbm.flamm_", "", basename(files))))
files3 <- files[yrs>=1950 & yrs<=2013]
files <- list.files("5m100n_cavmDistTrunc_loop_L", full=T)
files5 <- files[yrs>=1950 & yrs<=2013]
r3.all <- mask(crop(stack(files3), shp), shp)
r5.all <- mask(crop(stack(files5), shp), shp)
r.veg2 <- r.veg
#r.veg2[r.veg2<5] <- NA
r3.all <- mask(r3.all, r.veg2)
r5.all <- mask(r5.all, r.veg2)
r3 <- calc(r3.all, function(x) c(mean(x), sd(x), min(x), max(x)))
r5 <- calc(r5.all, function(x) c(mean(x), sd(x), min(x), max(x)))
names(r3) <- paste("GBM3", c("Mean", "SD", "Min", "Max"), sep="_")
names(r5) <- paste("GBM5", c("Mean", "SD", "Min", "Max"), sep="_")
#r3 <- calc(r3.all, function(x, ...) c(min(x), quantile(x, seq(0.1, 0.9, by=0.1), na.rm=T), max(x), mean(x), sd(x)))
#r5 <- calc(r5.all, function(x, ...) c(min(x), quantile(x, seq(0.1, 0.9, by=0.1), na.rm=T), max(x), mean(x), sd(x)))
#names(r3) <- paste("GBM3", c("Min", paste0("Pct_0.", 1:9, "0"), "Max", "Mean", "SD"), sep="_")
#names(r5) <- paste("GBM5", c("Min", paste0("Pct_0.", 1:9, "0"), "Max", "Mean", "SD"), sep="_")

pts <- read.csv("/atlas_scratch/mfleonawicz/projects/Flammability/data/pts/Noatak_lake_locations.csv")
#pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)

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

RasterTheme <- function (pch = 19, cex = 0.7, region=rev(brewer.pal(9, "YlOrRd")[-1]), ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

revRasterTheme <- function (pch = 19, cex = 0.7, region=brewer.pal(9, "YlOrRd")[-1], ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

r.pts.fire <- r.pts.gram <- r.pts.shrub <- r.pts <- subset(r3,1)
r.pts[] <- r.pts.shrub[] <- r.pts.gram[] <- r.pts.fire[] <- NA
r.pts[cellFromXY(r.pts, pts[1:4,])] <- 1
r.pts.shrub[cellFromXY(r.pts.shrub, pts[5:8,])] <- 1
r.pts.gram[cellFromXY(r.pts.gram, pts[9:12,])] <- 1
r.pts.fire[cellFromXY(r.pts.fire, pts[13:14,])] <- 1

library(gridExtra)
png(file.path(outDir, "noatak_flamInMean1950-2013_frpOut1-2013.png"), height=3200, width=3200, res=300)
p1 <- levelplot(subset(r3,1), main="ALFRESCO 1950-2013 mean flammability input", par.settings=revRasterTheme, margin=F, at=seq(0.03,  max(subset(r3,1)[], na.rm=T), by=0.01)) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.shrub, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.gram, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.fire, sp=T), col='black'))
p2 <- levelplot(2013/r.frp, main="ALFRESCO 1-2013 64-rep mean FRP output", par.settings=RasterTheme, margin=F, at=seq(0, 2013, by=250)) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.shrub, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.gram, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.fire, sp=T), col='black'))
grid.arrange(p1, p2, ncol=1)
dev.off()

png(file.path(outDir, "noatak_flammability_mean_1950-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,1),subset(r5,1)), main="ALFRESCO 1950-2013 annual shrub/graminoid/wetland flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.shrub, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.gram, sp=T), col='black')) +
    layer(sp.points(rasterToPoints(r.pts.fire, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_sd_1950-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,2),subset(r5,2)), main="ALFRESCO 1950-2013 annual shrub/graminoid/wetland flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_min_1950-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,3),subset(r5,3)), main="ALFRESCO 1950-2013 annual shrub/graminoid/wetland flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_max_1950-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,4),subset(r5,4)), main="ALFRESCO 1950-2013 annual shrub/graminoid/wetland flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

pixel.only <- TRUE
if(pixel.only){
    e.all <- extract(stack(r3.all, r5.all), pts)
    yrs.vec <- as.numeric(substr(gsub("gbm.flamm_", "", colnames(e.all)), 1, 4))
    e.all <- lapply(as.list(as.data.frame(t(e.all))), function(x) matrix(x, 1))
    flamtype <- "point"
    flamtype2 <- "point"
} else {
    e.all <- extract(stack(r3.all, r5.all), pts, buffer=10000)
    yrs.vec <- as.numeric(substr(gsub("gbm.flamm_", "", colnames(e.all[[1]])), 1, 4))
    flamtype <- "10kmMean"
    flamtype2 <- "10-km buffered mean"
}

d.all <- rbindlist(lapply(1:length(e.all),
    function(i,x, locs) data.frame(Lake=locs[i], Var=rep(c("3-GBM", "5-GBM"), each=ncol(x[[i]])/2), Year=yrs.vec, Val=as.numeric(t(x[[i]])), Obs=rep(1:nrow(x[[i]]), each=ncol(x[[i]]))),
    x=e.all, locs=locs))
d.all <- mutate(d.all, Location="Origin", Var=factor(Var, levels=c("3-GBM", "5-GBM")))
d.all[substr(Lake,1,4)=="Gram", Location:="Graminoid"]
d.all[substr(Lake,1,5)=="Shrub", Location:="Shrub"]
d.all[substr(Lake,1,4)=="Fire", Location:="Fire"]
d.all <- mutate(d.all, Location=factor(Location, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Lake=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Lake))), levels=c("Raven", "Uchugrak", "Poktovik", "LittleIsac", "LakeWest", "LakeEast")))

e <- extract(stack(subset(r3,1), subset(r5,1)), pts, buffer=10000)
e <- lapply(e, function(x) { x[is.na(x)] <- 0; x } )
eq <- lapply(e, function(x) apply(x, 2, function(y) quantile(y, c(0.1, 0.9), na.rm=TRUE)))

d <- rbindlist(lapply(1:length(e),
    function(i,x, locs) data.frame(Lake=locs[i], Var=rep(c("3-GBM", "5-GBM"), each=nrow(x[[i]])), Val=as.numeric(x[[i]])),
    x=e, locs=locs))
d <- mutate(d, Location="Origin", Var=factor(Var, levels=c("3-GBM", "5-GBM")))
d[substr(Lake,1,4)=="Gram", Location:="Graminoid"]
d[substr(Lake,1,5)=="Shrub", Location:="Shrub"]
d[substr(Lake,1,4)=="Fire", Location:="Fire"]
d <- mutate(d, Location=factor(Location, levels=c("Origin", "Shrub", "Graminoid", "Fire")), Lake=factor(gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_" , "", Lake))), levels=c("Raven", "Uchugrak", "Poktovik", "LittleIsac", "LakeWest", "LakeEast")))
#d <- mutate(d, Location=factor(Location, levels=c("Origin", "Shrub", "Graminoid")), Lake=factor(gsub("Shrub_", "", gsub("Gram_" , "", Lake)), levels=c("Raven", "Uchugrak", "Poktovik", "LittleIsac")))

cbpal <- c("#000000", "#D55E00", "#0072B2", "#FF0000")
cbpal2 <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

png(paste0(outDir, "/noatak_3m_", flamtype, "Flam_ts_1950-2013.png"), height=1600, width=3200, res=200)
ggplot(d.all %>% filter(Var=="3-GBM") %>% group_by(Var, Location, Lake, Year) %>% summarise(Val=mean(Val, na.rm=TRUE)), aes(x=Year, y=Val, colour=Lake)) +
    geom_line() + geom_point() + facet_wrap(~ Location, scales="fixed") +
    scale_colour_manual("Location", values=cbpal2) +
    labs(title=paste("1950-2013 3-GBM", flamtype2, "flammability by lake")) + theme(legend.position="bottom")
dev.off()

png(paste0(outDir, "/noatak_5m_", flamtype, "Flam_ts_1950-2013.png"), height=1600, width=3200, res=200)
ggplot(d.all %>% filter(Var=="5-GBM") %>% group_by(Var, Location, Lake, Year) %>% summarise(Val=mean(Val, na.rm=TRUE)), aes(x=Year, y=Val, colour=Lake)) +
    geom_line() + geom_point() + facet_wrap(~ Location, scales="fixed") +
    scale_colour_manual("Location", values=cbpal2) +
    labs(title=paste("1950-2013 5-GBM", flamtype2, "flammability by lake")) + theme(legend.position="bottom")
dev.off()

png(paste0(outDir, "/noatak_3m_", flamtype, "Flam_boxplot_1950-2013.png"), height=1600, width=3200, res=200)
ggplot(d.all %>% filter(Var=="3-GBM") %>% group_by(Var, Location, Lake, Year) %>% summarise(Val=mean(Val, na.rm=TRUE)), aes(x=Location, y=Val, colour=Lake)) +
    geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(~ Var, scales="fixed") +
    scale_colour_manual("Location", values=cbpal2) +
    labs(title=paste("1950-2013 3-GBM", flamtype2, "flammability by lake")) + theme(legend.position="bottom")
dev.off()

png(paste0(outDir, "/noatak_5m_", flamtype, "Flam_boxplot_1950-2013.png"), height=1600, width=3200, res=200)
ggplot(d.all %>% filter(Var=="5-GBM") %>% group_by(Var, Location, Lake, Year) %>% summarise(Val=mean(Val, na.rm=TRUE)), aes(x=Location, y=Val, colour=Lake)) +
    geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(~ Var, scales="fixed") +
    scale_colour_manual("Location", values=cbpal2) +
    labs(title=paste("1950-2013 5-GBM", flamtype2, "flammability by lake")) + theme(legend.position="bottom")
dev.off()

if(!pixel.only){
png(paste0(outDir, "/noatak_", flamtype, "Flam_ts2_1950-2013.png"), height=1600, width=3200, res=200)
ggplot(d.all %>% filter(Var=="3-GBM"), aes(x=Year, y=Val, group=interaction(Location, Lake, Obs), colour=Lake)) +
    geom_line() + geom_point() + facet_wrap(~ Location, scales="fixed") +
    geom_line(data=filter(d.all, Var=="3-GBM") %>% group_by(Var, Location, Lake, Year) %>% summarise(Val=mean(Val, na.rm=TRUE)), aes(group=NULL), size=1, linetype=2) +
    scale_colour_manual("Location", values=cbpal2) +
    labs(title="1950-2013 10-km buffered individual and mean flammability by lake") + theme(legend.position="bottom")
dev.off()
}

png(file.path(outDir, "noatak_10kmFlamSpace.png"), height=1600, width=3200, res=200)
ggplot(d, aes(x=Lake, y=Val, colour=Location)) + geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(~ Var, scales="free") +
    scale_colour_manual("Location", values=cbpal) +
    labs(title="1950-2013 flammability space by lake using 10th and 90th quantiles of a 10-km buffer") + theme(legend.position="bottom")
dev.off()

png(file.path(outDir, "noatak_10kmFlamSpace2.png"), height=2400, width=3200, res=300)
ggplot(d, aes(x=Lake, y=Val)) + geom_boxplot(position=position_dodge(width=0.9)) + facet_wrap(Var ~ Location, scales="free") +
    scale_colour_manual("Location", values=cbpal) +
    labs(title="1950-2013 flammability space by lake using 10th and 90th quantiles of a 10-km buffer") + theme(legend.position="bottom")
dev.off()
