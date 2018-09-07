library(raster)
path <- "data-raw/shapefiles"
ids <- c("alaska", "canada", "ecoreg", "aklcc", "lcc", "cavm", "fmz", "tpa")
files <- file.path(
  path, c("Political/Alaska.shp", "Political/CanadianProvinces_NAD83AlaskaAlbers.shp",
          "AK_ecoregions/akecoregions.shp", "LCC/LCC_summarization_units_singlepartPolys.shp",
          "LCC_AKCAN_boreal/AK_LCC_boundaries_AKAlbersNAD83.shp", "CAVM/CAVM_complete.shp",
          "FireMgmtZones/FireManagementZones_simplified.shp", "NA_TPA/NA_TPA_simplified.shp"))
purrr::walk2(ids, files, ~assign(.x, shapefile(.y), envir = .GlobalEnv))
ecoreg <- spTransform(ecoreg, CRS(projection(alaska)))
aklcc <- spTransform(aklcc, CRS(projection(alaska)))
purrr::map(ids, ~(projection(get(.x)) == projection(alaska))) # check all AK Albers
usethis::use_data(alaska, canada, ecoreg, aklcc, lcc, cavm, fmz, tpa, compress="xz")
