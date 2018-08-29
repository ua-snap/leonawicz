library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)

usgs <- "https://earthquake.usgs.gov/fdsnws/event/1/"
fileformat <- "csv"
bbox <- c(-180, -125, 50, 80)
minmag <- 1
