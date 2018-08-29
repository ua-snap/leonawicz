clrs <- c("#9ACD32", "#FFA500", "#FF3030")
colpal <- colorBin(clrs, bins = c(0, 4, 6, 10))
m <- leaflet() %>% setView(-154, 63, 5) %>% addProviderTiles("CartoDB.DarkMatter") %>% addFullscreenControl() %>%
  addScaleBar(position = c("bottomright"), options = scaleBarOptions(metric = TRUE)) %>%
  addLegend("bottomright", colors = clrs, title = "Magnitude", opacity = 1, labels = c("< 4", "[4 - 6)", ">= 6"))

mag_color <- function(x, colors = clrs) sapply(x, function(x) if(x < 4) colors[1] else if(x < 6) colors[2] else colors[3])
mag_size <- function(x) sapply(x, function(x) if(x < 4) 8 else if(x < 6) 12 else 16)
mag_rate <- function(x) sapply(x, function(x) if(x < 4) NA  else 1)
mag_marker <- function(x) sapply(x, function(x) if(x < 4) "circle" else "pulse")
mag_id <- function(x) sapply(x, function(x) if(x <= 4) "small" else if(x <= 6) "medium" else "large")

eq_popup <- function(x){
  paste0(strong("Time: "), x$time, "<br/>", strong("Place: "), x$place, "<br/>", strong("Coordinates: "), x$longitude, ", ", x$latitude, 
         "<br/>", strong("Magnitude: "), x$mag, "<br/>", strong("Depth: "), x$depth, " km<br/>", strong("RMS: "), x$rms, "<br/>",
         strong("Type: "), x$type, "<br/>", strong("Status: "), x$status)
}
pop_opts <- popupOptions(closeButton = FALSE)
label_opts <- list(
  "background-color" = "rgba(152, 245, 255, 0.5", "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
  "font-size" = "16px", "font-style" = "bold", "color" = "#ffffff", "border-color" = "#98F5FF",
  "border-size" = "6px", "border-radius" = "4px")

server <- function(input, output, session) {

  endtime <- Sys.Date()
  starttime <- endtime - 1
  eq_query <- paste0(usgs, "query?format=", fileformat, "&starttime=", starttime, "&endtime=", endtime, 
                     "&minmagnitude=", minmag, "&minlatitude=", bbox[3], "&maxlatitude=", bbox[4], 
                     "&minlongitude=", bbox[1], "&maxlongitude=", bbox[2])
  eq0 <- tbl_df(read.csv(eq_query)) %>% filter(locationSource == "ak") %>%
    select(-c(7:9, 11:13, 16:19, 21:22)) %>%
    mutate(time = format(as.POSIXct(substr(time, 1, 19), "GMT", format = "%Y-%m-%dT%H:%M:%S"), tz = "America/Anchorage", usetz = TRUE))
  eq1 <- reactive(if(input$mag_mult) mutate(eq0, mag = ifelse(mag^2 <= 10, mag^2, 10), mag = if(min(mag) < 1) mag + min(mag) - 1 else mag) else eq0)
  eq2 <- reactive(mutate(eq1(), icon_color = mag_color(mag), icon_size = mag_size(mag), icon_rate = mag_rate(mag), marker = mag_marker(mag)))
  max_min <- reactive(floor(max(eq2()$mag)))
  mag_seq <- reactive(seq(1, max_min(), by = 0.5))
  
  output$MinMag <- renderUI({
    if(length(mag_seq()) < 2) return()
    sliderInput("minmag", "Minimum magnitude", 1, max_min(), mag_seq()[1], 0.5, width = "100%")
  })
  d_eq <- reactive(if(is.null(input$minmag)) eq2() else filter(eq2(), mag >= input$minmag))
  
  output$Map <- renderLeaflet(m)
  observe({
    dc <- filter(d_eq(), marker == "circle")
    dp <- filter(d_eq(), marker == "pulse")
    x <- leafletProxy("Map") %>% clearMarkers()
    if(nrow(dc) > 0) x <- addCircleMarkers(x, data = dc, 
      lng = ~longitude, lat = ~latitude, radius = 5, weight = 2, color = clrs[1], fillColor = "#000000", opacity = 1, fillOpacity = 1,
      popup = eq_popup(dc), popupOptions = pop_opts,
      label = ~gsub(", Alaska", "", place), labelOptions = labelOptions(style = label_opts))
    if(nrow(dp) > 0) x <- addPulseMarkers(x, data = dp,
      lng = ~longitude, lat = ~latitude,
      popup = eq_popup(dp), popupOptions = pop_opts,
      label = ~gsub(", Alaska", "", place), labelOptions = labelOptions(style = label_opts),
      icon = makePulseIcon(color = ~icon_color, iconSize = ~icon_size, animate = ~!is.na(icon_rate), heartbeat = ~icon_rate))
    x
  })

}
