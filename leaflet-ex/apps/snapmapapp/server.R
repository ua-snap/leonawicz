library(rgdal)
library(raster)
load("data.RData")
m <- leaflet() %>% setView(-160, 66, 4)
prov_tiles %>% purrr::walk(function(x) m <<- m %>% addProviderTiles(x, group = x))
m <- addRasterImage(m, r, colors = pal, opacity = 0.8, group = "raster", layerId = "raster1") %>%
  addLayersControl(baseGroups = names(prov_tiles), overlayGroups = c("raster", "polygons"), 
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addFullscreenControl()

server <- function(input, output, session) {

output$Map <- renderLeaflet(m)
observe({
  leafletProxy("Map") %>%
    addPolygons(
      data = fmz, label = fmz$REGION, labelOptions = labelOptions(direction = 'auto'),
      weight = 1, color = "#000000", fillColor = pal_fmz, fillOpacity=0.3,
      highlightOptions = highlightOptions(
        color='#ffffff', opacity = 1, weight = 2, fillOpacity = 0.6,
        bringToFront = TRUE, sendToBack = TRUE), group = "polygons")
})

}
