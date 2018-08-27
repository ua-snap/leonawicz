library(raster)
setwd("/big_scratch/mfleonawicz/Alf_Files_20121129")

comArgs <- commandArgs(trailingOnly=TRUE)
ig_fmo <- comArgs[3]
fs_fmo <- comArgs[4]
ig_fmo_max <- as.numeric(comArgs[5])
fs_fmo_max <- as.numeric(comArgs[6])
outDir <- comArgs[7]
domain <- comArgs[8]

if(substr(domain, 1, 6)=="Statew"){
  msk <- readAll(raster("alf2005.cavm.merged.030212.tif"))
} else if(substr(domain, 1, 6)=="Noatak"){
  msk <- readAll(raster("binary_ignition.tif"))
}

apply_fmo <- function(r, mapid, fmo, fmo_max, domain, ignore.one=TRUE){
  if(fmo=="None") return(r)
  if(fmo=="Standard"){
    infile <-  paste0("fmo_standard_", mapid, ".tif")
  } else if(fmo=="5-km_buffered_full"){
    infile <- paste0("fmo_buffer_full5_", mapid, ".tif")
  } else if(fmo=="15-km_buffered"){
    infile <- paste0("fmo_2017_buffered_", mapid, ".tif")
  }
  x <- readAll(raster(infile))
  if(substr(domain, 1, 6)=="Noatak") x <- mask(x, crop(x, r))
  if(ignore.one) idx <- which(x[]==1)
  x <- 1 - fmo_max * x / max(x[], na.rm=TRUE)
  if(ignore.one && length(idx)) x[idx] <- 1
  return(x*r)
  return(r)
}

ignition <- msk
ignition[ignition > 0] <- as.numeric(comArgs[1])
ignition <- apply_fmo(ignition, "ig", ig_fmo, ig_fmo_max, domain)
ignition[is.na(ignition)] <- 0

writeRaster(ignition, filename=file.path(outDir, 'ignition.tif'), options='COMPRESS=LZW', overwrite=TRUE, datatype='FLT4S')

sensitivity <- msk
if(substr(domain, 1, 6)=="Noatak"){
  dem <- resample(raster("AKCanada_2km_DEM_mosaic.tif"), msk)
  sensitivity[dem > 500] <- 0
} else if(substr(domain, 1, 6)=="Statew"){
  dem <- resample(raster("AKCanada_2km_DEM_mosaic.tif"), msk)
  sensitivity[dem > 750] <- 0
}
sensitivity[sensitivity > 0] <- as.numeric(comArgs[2])
sensitivity <- apply_fmo(sensitivity, "fs", fs_fmo, fs_fmo_max, domain)
sensitivity[is.na(sensitivity)] <- 0
writeRaster(sensitivity, filename=file.path(outDir, 'sensitivity.tif'), options='COMPRESS=LZW', overwrite=TRUE, datatype='FLT4S')
