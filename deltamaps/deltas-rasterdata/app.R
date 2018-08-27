library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)
library(rgdal)
library(raster)
library(aws.s3)
source("aws_key.R")
#clrs <- c("#9ACD32", "#FFA500", "#FF3030")
#colpal <- colorBin(clrs, bins = c(0, 4, 6, 10))

aws <- "apps/deltamaps/data"
files <- file.path(aws, readRDS("srcfiles.rds"))
files_df <- as.data.frame(t(sapply(strsplit(basename(files), "_"), "[", 2:6)), stringsAsFactors = FALSE)
names(files_df) <- c("Var", "GCM", "RCP", "Season", "Yr1")

ui <- fluidPage(
   h3("Some maps"),
   fluidRow(
     column(3, 
            selectInput("varid", "Climate variable", "tas", width = "100%"),
            selectInput("gcm", "Climate model", "NCAR-CCSM4", width = "100%"),
            selectInput("rcp", "RCP", "rcp85", width = "100%"),
            selectInput("season", "Season", c(Winter = "12-2", Spring = "3-5", Summer = "6-8", Autumn = "9-11"), width = "100%"),
            selectInput("period", "Time period", c("2010 - 2039" = "2010", "2040 - 2069" = "2040", "2070 - 2099" = "2070"), width = "100%"),
            sliderInput("ras_op", "Raster opacity", 0.5, 1, 0.7, 0.1, width = "100%")
     ),
     column(9,
            withSpinner(leafletOutput("Map", width = "100%", height = "1000px"))
     )
   )
)

server <- function(input, output, session){
  m <- leaflet() %>% setView(-142, 61, 4) %>% addProviderTiles("CartoDB.Positron") %>% addFullscreenControl() %>%
    addScaleBar(position = c("bottomright"), options = scaleBarOptions())
  
  output$Map <- renderLeaflet(m)
  
  mapfile <- reactive({
    pat <- paste(input$varid, input$gcm, input$rcp, input$season, input$period, sep = "_")
    files[grep(pat, files)]
  })
  
  rv <- reactiveValues(tmp_raster = "")
  
  observeEvent(mapfile(), {
    if(rv$tmp_raster != mapfile()){
      rv$tmp_raster <- mapfile()
      x <- get_object(rv$tmp_raster, "leonawicz")
      writeBin(x, "tmp.tif")
    }
  })
  
  ras <- reactive({
    if(rv$tmp_raster == "") return()
    isolate(raster("tmp.tif"))
  })
  
  rasvals <- reactive({ ras()[] })
  pal <- reactive({
    colorNumeric(palette = "Spectral", domain = rasvals(), na.color = "transparent")
  })
  
  observe({
    leafletProxy("Map") %>% removeImage("raslayer") %>% removeControl("legend") %>%
      addRasterImage(ras(), colors = pal(), opacity = input$ras_op, layerId = "raslayer") %>%
      addLegend("bottomright", pal = pal(), title = "TEMP", values = rasvals(), opacity = input$ras_op, layerId = "legend")
  })
}

shinyApp(ui = ui, server = server)
