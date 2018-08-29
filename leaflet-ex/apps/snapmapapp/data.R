library(raster)
library(leaflet)
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
load("C:/github/snappoly/data/fmz.rda")
fmz <- spTransform(fmz, CRS(wgs84))
fmz <- rmapshaper::ms_simplify(fmz)

r <- readAll(raster("C:/Users/Matt/Desktop/ak771.tif"))
r <- projectRaster(r, res = 1/20, crs = wgs84)
r <- trim(crop(r, extent(xmin(r), -125, ymin(r), ymax(r))))

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")
pal_fmz <- ~colorQuantile("YlOrRd", 1:14)(1:14)

prov_tiles <- providers %>% purrr::keep(~ grepl('^Esri',.))

save(r, fmz, pal, pal_fmz, prov_tiles, file = "snapmapapp/data.RData")
