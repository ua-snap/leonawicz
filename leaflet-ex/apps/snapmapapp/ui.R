library(shinycssloaders)

ui <- fluidPage(
  withSpinner(leafletOutput("Map", width = "100%", height = "800px")),
  absolutePanel(id = "title", top = -10, left = 70, height = 20, width = 210, draggable = FALSE, style="z-index:500;",
                h3("Leaflet example", style="color: #ffffff; background-color: rgba(45, 45, 45, 0.5); padding: 20px;")),
  h4("Features:"),
  p("Rasterized data, polygons/aerial data, color palettes/semi-transparency, selectable base maps, full screen option.")
)
