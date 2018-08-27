


##
##
## AlfrescoFRP.R

The `AlfrescoFRP.R` script carries out post-processing of ALFRESCO simulation outputs.
It is assumed that `AlfrescoCalibration.R` has already executed. This and any other post-processing **R** script are always run secondary to the primary script.
The key features of the script include:

*    Simulation replicate-specific spatially explicit maps of Fire Return Period (FRP) compared with a historically observed FRP map.
*    .RData workspace files containing Fire Return Interval (FRI) and FRP data frames for use in other applications.
*    Export of **R** data objects via **R** workspace to a template Shiny app for dynamic, interactive exploratory analysis by the user.

Files of interest are attached to an email which is sent from the Atlas cluster to intended recipients as part of the broader SLURM process.
This script is called by the SLURM script, `CompileData.slurm` after the initial post-processing script, `AlfrescoCalibration.R` has run.
This script also sources `obs_fire_setup.R` during runtime.

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
dir.create(outDir <- file.path(out, "FRP"), showWarnings = F)
sink(file = file.path(out, "message.txt"), append = TRUE)
cat("Below you will find a link to a preliminary R Shiny Alfresco FRP/FRI results web application.\n")

library(raster)
library(parallel)
library(data.table)
library(dplyr)

rasterOptions(tmpdir = "/big_scratch/shiny", chunksize = 1e+11, maxmemory = 1e+12)
mainDir <- file.path(input, "Maps")
dir.create(outDir <- file.path(out, "FRP"), showWarnings = F)
if (!exists("pts")) stop("No coordinates file provided for relative area burned time series extraction.")
if (!exists("buffers")) stop("No buffer(s) provided for relative area burned time series extraction.")
if (!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if (period == "historical") yr.start <- 1950 else yr.start <- baseline.year
if (exists("yr.end")) yrs <- yr.start:yr.end else stop("must provide 'baseline.year' and 'yr.end'")
if (!exists("n.sims")) n.sims <- 32
n.cores <- min(n.sims, 32)

pts <- read.csv(file.path(input, pts))
pts <- pts[order(pts$ID), ]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon, pts$Lat)
if (!is.matrix(pts)) stop("No coordinates matrix provided for relative area burned time series extraction")

buffers <- eval(parse(text = buffers))
buffers.labels <- buffers/1000

buffers <- as.list(sort(unique(buffers)))
null.ind <- which(buffers == 0)
if (length(null.ind)) buffers[null.ind] <- list(NULL)

propFun <- function(x, ...) sum(x, na.rm = T)/length(which(!is.na(x)))
buffer.functions <- buffers
for (i in 1:length(buffers)) if (!is.null(buffers[[i]])) buffer.functions[[i]] <- propFun
if (length(null.ind)) buffer.functions[null.ind] <- list(NULL)

wgs2ak <- function(xy) {
    require(rgdal)
    if (class(xy) == "matrix") 
        xy <- data.frame(xy)
    names(xy) <- c("x", "y")
    coordinates(xy) <- names(xy)
    proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
    xy <- coordinates(spTransform(xy, CRS = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")))
}

pts <- wgs2ak(pts)
```

### Processing function: modeled outputs


```r
fireEventsFun <- function(d, pts, buffer.list = list(NULL), fun.list = list(NULL), 
    burnable.cells.raster = NULL, mainDir) {
    require(raster)
    if (!is.null(burnable.cells.raster)) 
        burnable.cells <- Which(burnable.cells.raster == 1)
    reps <- paste0("_", d - 1, "_")
    files <- list.files(mainDir, pattern = gsub("expression", "", paste(bquote(expression("^FireSc.*.", 
        .(reps), ".*.tif$")), collapse = "")), full = T)
    yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
    n <- length(yrs)
    ord <- order(yrs)
    files <- files[ord]
    yrs <- yrs[ord]
    e.hold <- cells.list <- list()
    for (i in 1:n) {
        if (i == 1) {
            r.hold <- raster(files[i])
            r.hold[is.na(r.hold)] <- 0
            r.hold <- mask(r.hold, burnable.cells)
            r.hold[r.hold > 0] <- 1
            for (p in 1:length(buffer.list)) {
                if (!is.null(buffer.list[[p]])) {
                  cells.list[[p]] <- lapply(extract(r.hold, pts, buffer = buffer.list[[p]], 
                    cellnumbers = T), function(x) x[, 1])
                  tmp <- c()
                  f <- fun.list[[p]]
                  for (q in 1:length(cells.list[[p]])) tmp <- c(tmp, f(extract(r.hold, 
                    cells.list[[p]][[q]])))
                  e.hold[[p]] <- tmp
                } else if (is.null(buffer.list[[p]])) {
                  cells.list[[p]] <- extract(r, pts, cellnumbers = T)[, 1]
                  e.hold[[p]] <- extract(r.hold, cells.list[[p]])
                }
            }
        } else {
            r <- raster(files[i])
            r[is.na(r)] <- 0
            r <- mask(r, burnable.cells)
            r[r > 0] <- 1
            r.hold <- r + r.hold
            for (p in 1:length(buffer.list)) {
                if (!is.null(buffer.list[[p]])) {
                  tmp <- c()
                  f <- fun.list[[p]]
                  for (q in 1:length(cells.list[[p]])) tmp <- c(tmp, f(extract(r, 
                    cells.list[[p]][[q]])))
                  e.hold[[p]] <- rbind(e.hold[[p]], tmp)
                } else if (is.null(buffer.list[[p]])) {
                  e.hold[[p]] <- rbind(e.hold[[p]], extract(r, cells.list[[p]]))
                }
            }
        }
    }
    list(rasters = r.hold, points = e.hold, years = n, years.vec = yrs)
}
```

### Processing function: observed data


```r
fireEventsFunEmpirical <- function(b, pts, buffer.list = list(NULL), fun.list = list(NULL), 
    burnable.cells.raster = NULL) {
    require(raster)
    if (!is.null(burnable.cells.raster)) 
        burnable.cells <- Which(burnable.cells.raster == 1)
    n <- nlayers(b)
    e.hold <- cells.list <- list()
    for (i in 1:n) {
        if (i == 1) {
            r.hold <- subset(b, i)
            r.hold[is.na(r.hold)] <- 0
            r.hold <- mask(r.hold, burnable.cells)
            r.hold[r.hold > 0] <- 1
            for (p in 1:length(buffer.list)) {
                if (!is.null(buffer.list[[p]])) {
                  cells.list[[p]] <- lapply(extract(r.hold, pts, buffer = buffer.list[[p]], 
                    cellnumbers = T), function(x) x[, 1])
                  tmp <- c()
                  f <- fun.list[[p]]
                  for (q in 1:length(cells.list[[p]])) tmp <- c(tmp, f(extract(r.hold, 
                    cells.list[[p]][[q]])))
                  e.hold[[p]] <- tmp
                } else if (is.null(buffer.list[[p]])) {
                  cells.list[[p]] <- extract(r, pts, cellnumbers = T)[, 1]
                  e.hold[[p]] <- extract(r.hold, cells.list[[p]])
                }
            }
        } else {
            r <- subset(b, i)
            r[is.na(r)] <- 0
            r <- mask(r, burnable.cells)
            r[r > 0] <- 1
            r.hold <- r + r.hold
            for (p in 1:length(buffer.list)) {
                if (!is.null(buffer.list[[p]])) {
                  tmp <- c()
                  f <- fun.list[[p]]
                  for (q in 1:length(cells.list[[p]])) tmp <- c(tmp, f(extract(r, 
                    cells.list[[p]][[q]])))
                  e.hold[[p]] <- rbind(e.hold[[p]], tmp)
                } else if (is.null(buffer.list[[p]])) {
                  e.hold[[p]] <- rbind(e.hold[[p]], extract(r, cells.list[[p]]))
                }
            }
        }
    }
    list(rasters = r.hold, points = e.hold, years = n)
}
```

### Observational data-specific setup


```r
source("/big_scratch/shiny/obs_fire_setup.R")
r.burnable <- Which(r > 0)
```

### Processing


```r
# Process empirical data
out.emp <- fireEventsFunEmpirical(b = result, pts = pts, buffer.list = buffers, 
    fun.list = buffer.functions, burnable.cells.raster = r.burnable)
# Process modeled data
n.cores <- min(n.sims, 32)
print(paste("Process modeled fire scar data from entire Alfresco run. Time:"))
system.time(out.alf <- mclapply(1:n.sims, fireEventsFun, pts = pts, buffer.list = buffers, 
    fun.list = buffer.functions, burnable.cells.raster = r.burnable, mainDir = mainDir, 
    mc.cores = n.cores))
```

### Spatially explicit FRP maps


```r
alf.yrs <- out.alf[[1]][[4]]
zlm <- c(0, out.alf[[1]][[3]])
dir.create(file.path(outDir, "Maps_noBuffer/PNGs"), recursive = T, showWarnings = F)

FRPmapsNoBuffer <- function(i, alf.data, emp.data, odir, domain, alf.yrs, emp.yrs) {
    if (substr(domain, 1, 6) == "Noatak") 
        domain <- "Noatak"
    if (substr(domain, 1, 6) == "Statew") 
        domain <- "Statewide"
    writeRaster(alf.data[[i]][[1]], paste0(odir, "/Maps_noBuffer/FRP_", domain, 
        "_Rep", i - 1, ".tif"), datatype = "FLT4S", overwrite = T)
    pngname <- paste0(odir, "/Maps_noBuffer/PNGs/FRP_", domain, "_Rep", i - 
        1, ".png")
    if (domain == "Noatak") {
        png(pngname, width = 1200, height = 1370)
        layout(matrix(1:2, 2, 1))
    } else if (domain == "Statewide") {
        png(pngname, width = 2400, height = 1370)
        layout(matrix(1:2, 1, 2))
    }
    plot(round(alf.data[[i]][[3]]/alf.data[[i]][[1]]), col = heat.colors(20), 
        zlim = zlm, main = paste0(domain, " ", alf.yrs[1], "-", tail(alf.yrs, 
            1), " replicate ", i - 1, " FRP"))
    plot(shp, bg = "transparent", add = TRUE)
    plot(fah, bg = "transparent", add = TRUE)
    plot(round(length(emp.yrs)/emp.data), col = heat.colors(20), zlim = zlm, 
        main = paste0(domain, " ", emp.yrs[1], "-", tail(emp.yrs, 1), " observed FRP"))
    plot(shp, bg = "transparent", add = TRUE)
    plot(fah, bg = "transparent", add = TRUE)
    dev.off()
    if (i == 1) 
        file.copy(pngname, file.path(dirname(odir), basename(pngname)))
}

print(paste("Create 2-panel (empirical and modeled) 1-km no-buffer FRP maps. Time:"))
print(system.time(mclapply(1:length(out.alf), FRPmapsNoBuffer, alf.data = out.alf, 
    emp.data = result2, odir = outDir, domain = alf.domain, alf.yrs = alf.yrs, 
    emp.yrs = yrs.hist.all, mc.cores = n.cores)))
```

### Shiny app setup


```r
# Concatenate all elements in nested list x. Inner to outer levels: year,
# location, buffer, replicate
dfPrepFun <- function(x, multiple.reps = T) {
    if (!multiple.reps) 
        x <- list(x)
    x <- lapply(x, "[[", 2)
    for (i in 1:length(x)) x[[i]] <- do.call(c, lapply(x[[i]], as.numeric))
    x <- do.call(c, x)
    x
}

# Empirical observations and simulation replicates
reps.emp <- "Observed"
reps.alf <- paste0("Rep_", c(paste0(0, 0, 0:9), paste0(0, 10:99), 100:999))[1:length(out.alf)]

# Organize observed data
x.emp <- dfPrepFun(out.emp, multiple.reps = F)
d.emp <- data.table(rev(expand.grid(Value = NA, Year = yrs.hist.all, Location = locs, 
    Buffer_km = buffers.labels, Replicate = reps.emp, stringsAsFactors = F)))
d.emp[, `:=`(Value, x.emp)]
d2.emp <- d.emp %>% group_by(Replicate, Buffer_km, Location) %>% summarise(FRP = length(Year)/sum(Value))

# Organize modeled data
x <- dfPrepFun(out.alf)
d <- data.table(rev(expand.grid(Value = NA, Year = alf.yrs, Location = locs, 
    Buffer_km = buffers.labels, Replicate = reps.alf, stringsAsFactors = F)))
d[, `:=`(Value, x)]
d2 <- d %>% group_by(Replicate, Buffer_km, Location) %>% summarise(FRP = length(Year)/sum(Value))

# Additional objects to transport to app
buffersize <- unique(d.emp$Buffer_km)
obs.years.range <- range(d.emp$Year)
mod.years.range <- range(alf.yrs)

# Assemble final data frames
rab.dat <- rbind(d, d.emp)
rm(d, d.emp)
dummy <- capture.output(gc())
frp.dat <- rbind(d2, d2.emp)
rm(d2, d2.emp)
dummy <- capture.output(gc())

rab.dat[, `:=`(Source, "Observed")]
rab.dat[Replicate != "Observed", `:=`(Source, "Modeled")]
frp.dat[, `:=`(Source, "Observed")]
frp.dat[Replicate != "Observed", `:=`(Source, "Modeled")]

# Make Fire Return Interval data frame this function could be improved using
# data tables and dplyr
friFun <- function(d) {
    d <- data.frame(filter(d, Value != 0))
    d$fac <- with(d, paste(Replicate, Buffer_km, Location))
    fri.list <- with(d, tapply(Year, paste(Replicate, Buffer_km, Location), 
        function(x) c(NA, diff(x))))
    d <- d[order(d$fac), ]
    d$FRI <- as.numeric(unlist(fri.list))
    d <- subset(d, !is.na(d$FRI), -which(names(d) %in% c("Year", "fac")))
    data.table(d)
}

fri.dat <- friFun(rab.dat)

# Load/save objects in a workspace file to be transported to app
dom <- if (substr(tolower(alf.domain), 1, 6) == "noatak") "Noatak" else if (substr(tolower(alf.domain), 
    1, 6) == "statew") "Statewide"
load(paste0(out, "/fsByVeg_df_", dom, ".RData"))  # assumed to have run fsByVeg.R
prefix <- ifelse(group.name == "none", "RAB_FRP", paste0(run.name, "_RAB_FRP"))
ws <- ifelse(group.name == "none", paste0(outDir, "/", prefix, "_Emp_", yrs.hist.all[1], 
    "_", tail(yrs.hist.all, 1), "_Alf_", alf.yrs[1], "_", tail(alf.yrs, 1), 
    ".RData"), paste0(outDir, "/", prefix, ".RData"))
save(d.fs, buffersize, obs.years.range, mod.years.range, rab.dat, frp.dat, fri.dat, 
    file = ws)
Sys.sleep(0.1)
```

### Save outputs


```r
# Create new app from template and copy data to app
app.name <- ifelse(group.name == "none", "alf_results", paste0(group.name, "_alf_results"))
alfDir <- "/var/www/shiny-server/shiny-apps/alfresco"  # Alfresco apps directory
templateDir <- file.path(alfDir, "alfout_template")  # Alfresco FRP results template app directory
appDir <- file.path(alfDir, app.name)  # App directory to use this run's data
system(paste("ssh eris.snap.uaf.edu mkdir -p", appDir))  # Make directory
system(paste("ssh eris.snap.uaf.edu chmod 2775", appDir))  # Set permissions
system(paste("ssh eris.snap.uaf.edu cp -r", file.path(templateDir, "*"), paste0(appDir, 
    "/")))  # Copy template app files to new directory
system(paste0("scp ", ws, " eris.snap.uaf.edu:", file.path(appDir, basename(ws))))  # Copy Alfresco run workspace file to new app directory

cat("Alfresco output results:\n\n")
app.url <- paste0("http://eris.snap.uaf.edu/shiny-apps/alfresco/", app.name, 
    "/")
cat(app.url)

sink()
Sys.sleep(0.1)
```
