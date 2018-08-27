#############################################################
#### This R script compares select gbm flammability maps ####
#############################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   12/09/2015        ####

# @knitr setup
# GBM flammability map comparisons
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")
dir.create(plotDir <- "/atlas_scratch/mfleonawicz/projects/Flammability/plots/gbmFlammability/map_comparisons", showWarnings=FALSE)

library(rasterVis)
(dirs <- list.files())
dirs3 <- dirs[c(1,4)]
dirs5 <- dirs[c(5,8)]

noa.shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2.shp")

yrs <- c(1954, 1957, 1968, 1969, 1977, 2004, 2005, 2007)

at.vals <- c(0, 0.25, 0.499, 0.75, 1)
colkey <- list(at=at.vals, labels=list(labels=c("Low", "Medium", "High", "Severe"), at=at.vals + 0.125))

# Theme settings
revRasterTheme <- function (pch = 19, cex = 0.7, region=colorRampPalette(brewer.pal(9, "YlOrRd")[-1])(30), ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

# @knitr run
for(yr in yrs){

files3 <- unlist(lapply(dirs3, function(x, year) list.files(x, pattern=paste0("_", year, "\\.tif$"), full=TRUE), year=yr))
files5 <- unlist(lapply(dirs5, function(x, year) list.files(x, pattern=paste0("_", year, "\\.tif$"), full=TRUE), year=yr))
s <- stack(c(files3, files5))
names(s) <- paste0(rep(c("GBM3", "GBM5"), each=length(dirs3)), paste0("_Flammability", c("", "_TL")))
s <- brick(s)

# Check for difference between GBM 3-model and GBM 5-model versions in CAVM region
pat <- paste0(yr, ".tif")
r1 <- raster(list.files(dirs[4], pattern=pat, full=T))
r2 <- raster(list.files(dirs[8], pattern=pat, full=T))
r1b <- raster(list.files(dirs[1], pattern=pat, full=T))
r2b <- raster(list.files(dirs[5], pattern=pat, full=T))
s2 <- stack(r1b, r2b, r1b-r2b)
names(s2) <- c("GBM3", "GBM5", "Difference")

s.noa <- mask(crop(s, noa.shp), noa.shp)
s2.noa <- mask(crop(s2, noa.shp), noa.shp)

# Write PNGs
# Statewide
w <- h <- 1600
png(paste0(plotDir, "/gbm.flamm_", yr,"_comparisons_Statewide.png"), height=h, width=w, res=200)
p <- levelplot(s, maxpixels=ncell(s)/10, main=paste(yr, "flammability map comparisons"), par.settings=revRasterTheme, contour=T, margin=F)#, at=at.vals, colorkey=colkey)
print(p)
dev.off()

png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Statewide.png"), height=h, width=w*1.5, res=200)
p <- levelplot(s2, maxpixels=ncell(s)/10, main=paste(yr, "3- vs. 5-GBM flammability maps"), par.settings=revRasterTheme, contour=T, margin=F)#, at=at.vals, colorkey=colkey)
print(p)
dev.off()

# Noatak
png(paste0(plotDir, "/gbm.flamm_", yr,"_comparisons_Noatak.png"), height=h, width=w*1.5, res=200)
p <- levelplot(s.noa, maxpixels=ncell(s.noa), main=paste(yr,"flammability map comparisons"), par.settings=revRasterTheme, contour=T, margin=F)#, at=at.vals, colorkey=colkey)
print(p)
dev.off()

png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Noatak.png"), height=h*1.5, width=w, res=200)
p <- levelplot(s2.noa, maxpixels=ncell(s2.noa), main=paste(yr, "3- vs. 5-GBM flammability maps"), par.settings=revRasterTheme, contour=T, margin=F)#, at=at.vals, colorkey=colkey)
print(p)
dev.off()

print(yr)
} # end yrs loop

# @knitr setup2
# Minimum threshold analysis
# RV's X = flammability and Y = lightning probability, with condition min(XY) = c
setwd("/atlas_scratch/mfleonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")
dir.create(plotDir <- "/atlas_scratch/mfleonawicz/projects/Flammability/plots/gbmFlammability/map_comparisons", showWarnings=FALSE)

library(rasterVis)
library(grid)
library("gridBase")
(dirs <- list.files())
dirs3 <- dirs[4]
dirs5 <- dirs[8]
f <- function(x) list.files(x, pattern=paste0("\\.tif$"), full=TRUE)
files3 <- unlist(lapply(dirs3, f))
files5 <- unlist(lapply(dirs5, f))

noa.shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2.shp")

threshold <- 0.01
#default.pvalue <- 0.25
s3 <- stack(files3)
s5 <- stack(files5)
domain <- "Noatak" # "Statewide"
if(domain=="Noatak"){
    s3 <- crop(s3, noa.shp)
    s5 <- crop(s5, noa.shp)
    s3 <- mask(s3, noa.shp)
    s5 <- mask(s5, noa.shp)
}
qtile3 <- qtile5 <- pval3 <- pval5 <- rep(NA, nlayers(s3))
for(i in 1:nlayers(s3)){
	r3 <- subset(s3, i)
	r5 <- subset(s5, i)
    #qtile3[i] <- as.numeric(quantile(r3[], prob=default.pvalue, na.rm=T))
    #qtile5[i] <- as.numeric(quantile(r5[], prob=default.pvalue, na.rm=T))
	pval3[i] <- mean(r3[] <= threshold, na.rm=T)
	pval5[i] <- mean(r5[] <= threshold, na.rm=T)
	r3[r3 <= threshold] <- threshold
	r5[r5 <= threshold] <- threshold
	r3[r3 > threshold] <- 0
	r5[r5 > threshold] <- 0
	r3[r3 == threshold] <- 1
	r5[r5 == threshold] <- 1
	if(i==1) {
		r3tot <- r3 ; r5tot <- r5
	} else {
		r3tot <- r3tot + r3
		r5tot <- r5tot + r5
	}
	print(nlayers(s3) - i)
}

r3tot <- r3tot/nlayers(s3)
r5tot <- r5tot/nlayers(s5)
s <- stack(r3tot, r5tot)
names(s) <- c("GBM-3", "GBM-5")

# @knitr plot2
# Write PNGs from part two: thresholds
png(paste0(plotDir, "/flam_spacetime_threshDist_", threshold, "_", domain, ".png"), height=2400, width=2400, res=200)
p <- levelplot(s, maxpixels=ncell(s), main=paste("1950-2009", threshold, "flammability threshold distributions"), par.settings=revRasterTheme, contour=T, margin=F)#, at=at.vals, colorkey=colkey)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2, widths=unit(c(1, 1), "null"), heights=unit(c(1, 1), "null"))))

vp <- pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
par(omi=gridOMI())
# base graphics
hist(pval3, main=paste("GBM-3 % area <=", threshold), col="gray", xlab="", ylab="", freq=FALSE)
popViewport()

vp <- pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
par(omi=gridOMI())
par(new=TRUE)
# base graphics
hist(pval5, main=paste("GBM-5 % area <=", threshold), col="gray", xlab="", ylab="", freq=FALSE)
popViewport()

# lattice plot
vp <- pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:2))
print(p, vp=vp, newpage=FALSE)
popViewport()

popViewport()
dev.off()

