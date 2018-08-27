


##
##
## alf_gcmRuns_inputPrep.R

The `alf_gcmRuns_inputPrep.R` script prepares geotiff outputs from Noatak domain, CRU 3.2-based, historical calibration ALFRESCO runs for use as inputs to future GCM-based runs.
Historical runs end with year 2013.
2013 geotiffs for age, vegetation, and burn severity history are copied from the final calibrated outputs directory to a new location to be referenced by future ALFRESCO run JSON input files.

In the process, the extents of these geotiffs are enlarged with NA-padding to match that of a template raster layer.
The template is an input geotiff to the original historical calibration runs.
This is necessary because the Noatak runs are set to trim inputs and run on a smaller domain as well as maintain trimming in the resultant output files.
ALFRESCO will throw an exception is trimmed output geotiffs with a smaller spatial extent are then passed as inputs to a new run along with other fixed inputs of larger extent.

## R code

### Input file preparation


```r
library(raster)
library(parallel)

domain <- "Noatak"
# domain <- 'Statewide'
run.name <- "m3TL_33150s_00178i_historical_CRU32"
# run.name <- 'm5TL_30750s_00185i_historical_CRU32' run.name <-
# 'm3TL_45000s_00175i_historical_CRU32'
gbm <- paste0(substr(run.name, 2, 2), substr(run.name, 1, 1))
inDir <- paste0("/big_scratch/shiny/Runs_", domain, "/paul.duffy_at_neptuneinc.org/", 
    run.name, "/Maps")
dir.create(outDir <- paste0("/big_scratch/shiny/Final_", domain, "_", gbm, "/gcmRunInputs"), 
    showWarnings = F, recursive = T)
files <- list.files(inDir, pattern = "2013\\.tif", full = T)
files <- files[-which(substr(basename(files), 1, 8) == "FireScar")]

r.template <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/Spinup300Year_32Reps/Age_0_1900.tif")

par_copy <- function(i, r.template, outDir) {
    r <- raster(files[i])
    r <- extend(r, r.template)
    if (substr(basename(files[i]), 1, 3) == "Age") 
        writeRaster(r, file.path(outDir, basename(files[i])), datatype = "INT4S", 
            overwrite = T)
    if (substr(basename(files[i]), 1, 3) != "Age") 
        writeRaster(r, file.path(outDir, basename(files[i])), datatype = "INT1U", 
            overwrite = T)
    return(NULL)
}

mclapply(1:length(files), par_copy, r.template = r.template, outDir = outDir, 
    mc.cores = 32)
```
