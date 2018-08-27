library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)

library(rgdal)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)
load("appData.RData")

action_btn_style <- "margin: 10px 15px 10px 15px; width: 200px"
enableBookmarking(store="server")
