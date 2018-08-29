library(shinycssloaders)

ui <- fluidPage(
  tags$head(tags$style(paste0(".leaflet-container { background: #000000; } .leaflet-popup-content-wrapper { border-radius: 4px; background-color: ", info_colors[1], "; width: 540px; font-size: 16px; color: ", info_colors[3], "; } .leaflet-popup { border-color: ", info_colors[2], "; border-size: 6px; }"))),
  withSpinner(leafletOutput("Map", width = "100%", height = "1000px")),
  absolutePanel(id = "title", top = 10, left = 70, height = 20, width = 320, draggable = FALSE, style = "z-index:500;",
                div(h3("Leaflet example", style = "margin-top: 0px;"), h4("Star Trek: Stellar Cartography"), style = "color: #ffffff; background-color: rgba(90, 90, 90, 0.5); padding: 20px;")),
  absolutePanel(id = "stlogo", top = 10, right = 25, height = 200, width = 130, draggable = FALSE, style = "z-index:500; background-color:rgba(255, 185, 15, 0.75); border-radius: 6px;",
                img(src = "stlogo-white.png", width = 130, height = 200, style = "padding:10px;")),
  h4("Features"),
  p("Non-geographic coordinate reference system, pulse markers, labels, popups."),
  h4("Credits"),
  p(em("Base map tiles generated from source map"), HTML('<a href="https://archerxx.deviantart.com/art/Star-Trek-Star-Chart-316982311">Star trek Star Chart</a> by <a href="https://archerxx.deviantart.com/">Rob Archer</a>.'))
)
