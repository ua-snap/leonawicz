lon <- -155
lat <- 65

shinyServer(function(input, output, session) {
  
  source("observers.R", local=TRUE) # observers related to region selection using map and selectInput
  
  # Initialize map and add flammability polygon layer
  mapSelect <- leaflet() %>% addTiles() %>% setView(lon, lat, 4) %>%
    addPolygons(data=flam, stroke=TRUE, fillOpacity=0.2, weight=1, color="red", group="flammable")
  
  # Add background polygon region outlines after map is created
  for(i in fmz$REGION) mapSelect <- mapSelect %>%
         addPolygons(data=subset(fmz, REGION==i), stroke=TRUE, fillOpacity=0, weight=1, color="black", group="not_selected", layerId=i, label=i,
                     highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, bringToFront=FALSE, sendToBack=FALSE))
  
  output$Map <- renderLeaflet(mapSelect)
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
})
