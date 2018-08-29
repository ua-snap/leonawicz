library(shinycssloaders)

ui <- fluidPage(
  tags$head(tags$style(".leaflet-popup-content-wrapper { border-radius: 4px; background-color: rgba(152, 245, 255, 0.5); font-size: 16px; color: #ffffff; } .leaflet-popup { border-color: #98F5FF; border-size: 6px; }")),
  withSpinner(leafletOutput("Map", width = "100%", height = "1000px")),
  absolutePanel(id = "title", top = 10, left = 70, height = 20, width = 320, draggable = FALSE, style = "z-index:500;",
                div(h3("Leaflet example", style = "margin-top: 0px;"), h4("Alaska earthquakes past 24 hours"), style = "color: #ffffff; background-color: rgba(90, 90, 90, 0.5); padding: 20px;")),
  absolutePanel(id = "mag_mult", top = 770, left = 25, height = 90, width = 300, draggable = FALSE, style= "z-index:500; background-color: #ffffff; padding: 5px;",
                checkboxInput("mag_mult", "Square magnitudes", TRUE, width = "100%"),
                p("Artificially increase magnitudes for demonstration purposes when no large earthquakes present.", style = "text-align: justify; font-size: 10px; padding: 0px 5px 0px 5px;")),
  absolutePanel(id = "mag_slider", top = 870, left = 25, height = 120, width = 300, draggable = FALSE, style= "z-index:500; background-color: #ffffff; padding: 20px;",
                uiOutput("MinMag")),
  h4("Features:"),
  p("API connection to USGS earthquake data, point data, pulse markers."),
  p(em("Magnitudes have been increased by default for demonstration purposes. Typically, most magnitudes will be low (green/uneventful), some orange (magnitude from four up to six) may occur, and red (six and above) are often not present. Uncheck the box above for actual magnitudes."))
)
