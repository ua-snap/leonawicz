# ALFRESCO: RAB, FRP, FRI, FSV data sets, plots, app, and empirical vs. projected FRP maps
library(rgdal)
library(raster)
library(alfresco)

comArgs <- commandArgs(TRUE)
eval(parse(text = apply(prep_comArgs(comArgs), 1, paste, collapse="=")))
dom <- if(substr(tolower(alf.domain), 1, 6) == "noatak") "Noatak" else "Statewide"
prep_alf_frp(comArgs, out)
rasterOptions(tmpdir=alfdef()$atlas_shiny_dir, chunksize=10e10, maxmemory=10e11)
yr.start <- ifelse(period=="historical", 1950, as.numeric(baseline.year))
yrs <- yr.start:yr.end
yrs.hist.all <- ifelse(period == "historical", yrs, 1950:2013)
if(!exists("n.sims")) n.sims <- 32
n.cores <- min(n.sims, 32)
pts <- prep_points(read.csv(file.path(input, pts)), input, dom)
buffers <- eval(parse(text=buffers))
buffers.labels <- buffers/1000
buffers <- adjust_buffers(buffers)

if(emp.fire.cause == "All") fah <- shapefile(alf_defaults()$fire_cause_all)
if(emp.fire.cause == "Lightning") fah <- shapefile(alf_defaults()$fire_cause_lightning)
fah <- subset(fah, FireYear >= 1950) # do not use observed data prior to 1950
yrs.fah <- sort(as.numeric(unique(fah@data$FireYear)))
if(dom == "Noatak"){
	rs <- list(r=raster(alfdef()$noa_veg), shp=shapefile(alfdef()$noa_shp))
} else if(dom == "Statewide") {
	rs <- list(r=raster(alfdef()$sw_veg), shp=shapefile(alfdef()$sw_shp))
}

# Fire size by vegetation class
pp_fsv_save(1:n.sims, file.path(input, "Maps"), rs$r, rs$shp, yrs, yrs.hist.all, 
  result2, 1:raster::nlayers(b.fid), b.fid, out, dom, n.cores)
# Load or prepare empirical fire scar raster bricks
fire_scar_brick(fah, yrs.fah, yrs.hist.all, emp.fire.cause, dom, rs$shp, rs$r)
# Process empirical data
out.emp <- prep_fire_events_emp(b = result, pts = pts$pts, locs = pts$locs, replicates = c("Observed", reps.alf), 
  buffer_list = buffers, buffer_labels = buffers.labels, veg = rs$r, yrs = yrs)
# Process model fire scar outputs
reps.alf <- paste0("Rep_", c(paste0(0,0,0:9), paste0(0,10:99), 100:999))[1:n.sims]
out.alf <- mclapply(1:n.cores, prep_fire_events, pts = pts$pts, locs = pts$locs, replicates = reps.alf,
  buffer_list = buffers, buffer_labels = buffers.labels, veg = rs$r, 
  main_dir = mainDir, mc.cores = n.cores)
# Save stock FRP_maps
mclapply(1:length(out.alf), frp_maps_no_buffer, alf_data = out.alf, emp_data = result2,
 shp = rs$shp, fire_area_history = fah, out = out, domain = dom, emp_yrs = yrs.hist.all, 
 mc.cores = n.cores)
# Prepare fire events data frames, load and save objects for Shiny app
pp_fire_events(alf.out, emp.out, dom, group.name, run.name, yrs.hist.all, out)
