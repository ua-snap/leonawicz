# gbm_flam_comparisons.R



##
##
## gbm_flam_comparisons.R

The `gbm_flam_comparisons.R` script makes select comparisons of flammability maps with respect to GBM models used, integration of lightning probability, and ad hoc functions applied to input maps.

## R code

### Setup


```r
# GBM flammability map comparisons
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")
dir.create(plotDir <- "/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/plots/gbmFlammability/map_comparisons", 
    showWarnings = FALSE)

library(rasterVis)
(dirs <- list.files())
dirs3 <- dirs[c(1, 4)]
dirs5 <- dirs[c(5, 8)]

noa.shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2")

yrs <- c(1954, 1957, 1968, 1969, 1977, 2004, 2005, 2007)

at.vals <- c(0, 0.25, 0.499, 0.75, 1)
colkey <- list(at = at.vals, labels = list(labels = c("Low", "Medium", "High", 
    "Severe"), at = at.vals + 0.125))

# Theme settings
revRasterTheme <- function(pch = 19, cex = 0.7, region = colorRampPalette(brewer.pal(9, 
    "YlOrRd")[-1])(30), ...) {
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}
```

### Plots


```r
for (yr in yrs) {
    
    files3 <- unlist(lapply(dirs3, function(x, year) list.files(x, pattern = paste0("_", 
        year, "\\.tif$"), full = TRUE), year = yr))
    files5 <- unlist(lapply(dirs5, function(x, year) list.files(x, pattern = paste0("_", 
        year, "\\.tif$"), full = TRUE), year = yr))
    s <- stack(c(files3, files5))
    names(s) <- paste0(rep(c("GBM3", "GBM5"), each = length(dirs3)), paste0("_Flammability", 
        c("", "_TL")))
    s <- brick(s)
    
    # Check for difference between GBM 3-model and GBM 5-model versions in CAVM
    # region
    pat <- paste0(yr, ".tif")
    r1 <- raster(list.files(dirs[4], pattern = pat, full = T))
    r2 <- raster(list.files(dirs[8], pattern = pat, full = T))
    r1b <- raster(list.files(dirs[1], pattern = pat, full = T))
    r2b <- raster(list.files(dirs[5], pattern = pat, full = T))
    s2 <- stack(r1b, r2b, r1b - r2b)
    names(s2) <- c("GBM3", "GBM5", "Difference")
    
    s.noa <- mask(crop(s, noa.shp), noa.shp)
    s2.noa <- mask(crop(s2, noa.shp), noa.shp)
    
    # Write PNGs Statewide
    w <- h <- 1600
    png(paste0(plotDir, "/gbm.flamm_", yr, "_comparisons_Statewide.png"), height = h, 
        width = w, res = 200)
    p <- levelplot(s, maxpixels = ncell(s)/10, main = paste(yr, "flammability map comparisons"), 
        par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
    print(p)
    dev.off()
    
    png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Statewide.png"), height = h, width = w * 
        1.5, res = 200)
    p <- levelplot(s2, maxpixels = ncell(s)/10, main = paste(yr, "3- vs. 5-GBM flammability maps"), 
        par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
    print(p)
    dev.off()
    
    # Noatak
    png(paste0(plotDir, "/gbm.flamm_", yr, "_comparisons_Noatak.png"), height = h, 
        width = w * 1.5, res = 200)
    p <- levelplot(s.noa, maxpixels = ncell(s.noa), main = paste(yr, "flammability map comparisons"), 
        par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
    print(p)
    dev.off()
    
    png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Noatak.png"), height = h * 1.5, 
        width = w, res = 200)
    p <- levelplot(s2.noa, maxpixels = ncell(s2.noa), main = paste(yr, "3- vs. 5-GBM flammability maps"), 
        par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
    print(p)
    dev.off()
    
    print(yr)
}  # end yrs loop
```
