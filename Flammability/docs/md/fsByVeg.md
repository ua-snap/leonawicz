


##
##
## fsByVeg.R

The `fsByVeg.R` script carries out post-processing of ALFRESCO simulation outputs.
It is assumed that `AlfrescoCalibration.R` has already executed. This and any other post-processing **R** script are always run secondary to the primary script.

The script extracts fire sizes (FS) from ALFRESCO output fire scar geotiffs for each simulation replicate,
conditional on vegetation class, and combines the modeled FS values with similarly extracted historical observations of vegetation-specific FS.
The hardcoded vegetation classification includes separate tundra types (alpine, shrub, graminoid, and wetland)
and an aggregate forest type (black spruce, white spruce, and deciduous trees).
Hardcoded years are currently 1950 - 2009.

An **R** workspace file containing a data frame of veg-specific FS is attached to an email which is sent from the Atlas cluster to intended recipients as part of the broader SLURM process.
This script is called by the SLURM script, `CompileData.slurm` after the initial post-processing script, `AlfrescoCalibration.R` has run.

## R code

### Setup


```r
comArgs <- commandArgs(TRUE)
if (length(comArgs > 0)) {
    arg.mat <- do.call("rbind", strsplit(comArgs, "="))
    options(warn = -1)
    arg.char <- which(is.na(as.numeric(arg.mat[, 2])))
    options(warn = 0)
    if (length(arg.char > 0)) 
        arg.mat[arg.char, 2] <- paste("'", arg.mat[arg.char, 2], "'", sep = "")
    eval(parse(text = apply(arg.mat, 1, paste, collapse = "=")))
}
cat(comArgs)

if (!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if (period == "historical") yr.start <- 1950 else yr.start <- baseline.year
if (exists("yr.end")) yrs <- yr.start:yr.end else stop("must provide 'baseline.year' and 'yr.end'")
yrs <- yr.start:yr.end
if (!exists("n.sims")) n.sims <- 32
n.cores <- min(n.sims, 32)

library(raster)
library(data.table)
library(parallel)

rasterOptions(tmpdir = "/big_scratch/shiny", chunksize = 1e+11, maxmemory = 1e+12)
mainDir <- file.path(input, "Maps")
```

### Functions: fseByVeg

`fseByVeg` performs the basic operation of calculating FSEs by vegetation class and tabling the individual observations.



### Functions: fseByRep

`fseByRep` is a parallel processing wrapper to `fseByVeg`, parallelized by simulation replicate.



### Functions: fseByRepEmp

`fseByRepEmp` is a basic wrapper to `fseByVeg` for empirical/historical observational FSE extraction.



# Empirical data setup


```r
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(r)
v.veg[v.veg == 3 | v.veg == 4] <- 2  # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")
```

# Run and save results


```r
# Process empirical data
fs.emp <- mclapply(1:nlayers(b.fid), fsByRepEmp, b = b.fid, vid = vid, v.veg = v.veg, 
    yrs = yrs.hist.all, mc.cores = n.cores)
fs.emp <- rbindlist(fs.emp)
# Process modeled data
fs.alf.list <- mclapply(1:n.sims, fsByRep, mainDir = mainDir, vid = vid, v.veg = v.veg, 
    years = yrs, mc.cores = n.cores)
fs.alf <- rbindlist(fs.alf.list)
d.fs <- rbind(fs.emp, fs.alf)
d.fs[, `:=`(Vegetation, v.names[Vegetation])]
dom <- if (substr(tolower(alf.domain), 1, 6) == "noatak") "Noatak" else if (substr(tolower(alf.domain), 
    1, 6) == "statew") "Statewide"
save(d.fs, file = paste0(out, "/fsByVeg_df_", dom, ".RData"))

sink(file = file.path(out, "message.txt"), append = TRUE)
cat("An R workspace file containing fire event sizes partitioned by vegetation class is attached.\n")
sink()
```
