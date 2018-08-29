library(dplyr)
library(rtrek)

tiles <- st_tiles("galaxy1")
stGeo <- mutate(stGeo, color = c("#87CEFF", "#C0FF3E", "#FF3030", "#BF3EFF", "#7FFFD4", "#FF1493", "#FFB90F"))

eq_popup <- function(x, y){
  logo <- y$avatar[match(y$species, x$species)]
  logo <- paste0("<img src=\"", logo, "\" style=\"float:right;width:200px\"/>")
  info <- paste0("<div style=\"width:500px;height:270px;\">", logo, strong("Identification: "), x$label, "<br/>", strong("Geopolitical domain: "), x$zone, "<br/>", strong("Primary species: "), x$species,
         "<br/>", strong("Stellar body: "), x$body, "<br/>", strong("Classification: "), x$category, "</div>")
  info
}
pop_opts <- popupOptions(closeButton = FALSE)
label_opts <- list(
  "background-color" = info_colors[1],
  "font-size" = "16px", "font-style" = "bold", "color" = info_colors[3], "border-color" = info_colors[2],
  "border-size" = "10px", "border-radius" = "4px")

ras_coords <- function(data, width = 8000, height = 6445){
  wh_ratio <- width / height
  center <- c(200 * wh_ratio, -200)
  dplyr::mutate(data, x = center[1]*x/width, y = center[2]*y/height)
}

m <- leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.Simple"), minZoom = 0, maxZoom = 7, attributionControl = FALSE)) %>% 
  addTiles(urlTemplate = tiles) %>% setView(124, -100, 3) %>%
  addFullscreenControl() %>% addMiniMap(tiles = tiles, toggleDisplay = TRUE) %>%
  addPulseMarkers(data = ras_coords(stGeo), lng = ~x, lat = ~y, icon = makePulseIcon(heartbeat = 1, color = ~color, iconSize = 16), 
                  popup = eq_popup(stGeo, stSpecies), popupOptions = pop_opts,
                  label = ~label, labelOptions = labelOptions(style = label_opts))

server <- function(input, output, session) {
  
  output$Map <- renderLeaflet(m)
  # observe({
  #   dp <- filter(d, marker == "pulse")
  #   x <- leafletProxy("Map") %>% clearMarkers()
  #   if(nrow(dp) > 0) x <- addPulseMarkers(x, data = dp,
  #     lng = ~longitude, lat = ~latitude,
  #     popup = eq_popup(dp), popupOptions = pop_opts,
  #     label = ~gsub(", Alaska", "", place), labelOptions = labelOptions(style = label_opts),
  #     icon = makePulseIcon(color = ~icon_color, iconSize = ~icon_size, animate = ~!is.na(icon_rate), heartbeat = ~icon_rate))
  #   x
  # })

}

# Use 
# Use alternate gdal2tiles.py from https://github.com/commenthol/gdal2tiles-leaflet
# Rename as gdal2tiles2.py
# Place with other scripts and use:

# Map 1:
# python "C:/Program Files/QGIS 3.0/apps/Python36/Scripts/gdal2tiles2.py" --leaflet -p raster -z 0-7 -w none C:/github/tiles/src/st1/st1.png C:/github/tiles/data/st1

# Map 2:
# python "C:/Program Files/QGIS 3.0/apps/Python36/Scripts/gdal2tiles2.py" --leaflet -p raster -z 0-7 -w none C:/github/tiles/src/st2/st2.png C:/github/tiles/data/st2

# example: https://github.com/commenthol/leaflet-rastercoords

# map 2: http://sttff.net/AST_MAP.html
