library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
path <- "data-raw/shapefiles_simplified"
proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Full domain (Alaska / western Canada)
akcan <- readOGR(file.path(path, "Political/AK_CAN_PRISM_Extent.shp"), verbose = FALSE) %>% spTransform(proj4)
# State/Province subset
akcan2 <- readOGR(file.path(path, "Political/AK_CAN.shp"), verbose = FALSE) %>% spTransform(proj4)
akcan2_IDs <- c("Alaska", "Alberta", "Saskatchewan", "Manitoba", "Yukon Territory", "British Columbia")
akcan2 <- subset(akcan2, NAME %in% akcan2_IDs)
# Alaska ecoregions
ecoreg3 <- readOGR(file.path(path, "AK_Ecoregions/AK_Ecoregions_COMMONER.shp"), verbose = FALSE) %>% spTransform(proj4)
ecoreg2 <- readOGR(file.path(path, "AK_Ecoregions/AK_Ecoregions_LEVEL2.shp"), verbose = FALSE) %>% spTransform(proj4)
ecoreg1 <- readOGR(file.path(path, "AK_Ecoregions/AK_Ecoregions_LEVEL1.shp"), verbose = FALSE) %>% spTransform(proj4)
# Alaska LCC regions
aklcc <- readOGR(file.path(path, "LCC/LCC_regions.shp"), verbose = FALSE) %>% spTransform(proj4)
# Alaska/Canada LCC regions
lcc <- readOGR(file.path(path, "AKCAN_LCC/AKCAN_LCC_regions.shp"), verbose = FALSE) %>% spTransform(proj4)
# CAVM regions
cavm <- readOGR(file.path(path, "CAVM/CAVM_complete.shp"), verbose = FALSE) %>% spTransform(proj4)
# Alaska fire management zones
fmz <- readOGR(file.path(path, "FireMgmtZones/FireManagementZones_simplified.shp"), verbose = FALSE) %>% spTransform(proj4)
# Terrestrail Protected Areas
tpa <- readOGR(file.path(path, "NA_TPA/NA_TPA_simplified.shp"), verbose = FALSE) %>% spTransform(proj4)

usethis::use_data(akcan, akcan2, ecoreg1, ecoreg2, ecoreg3, aklcc, lcc, cavm, fmz, tpa, compress="xz")
