library(shiny)
library(shinydashboard)
library(apputils)
library(snaputils)
library(rgl)

reg <- c("Arctic Tundra", "Bering Taiga", "Bering Tundra", "Intermontane Boreal")
names(reg) <- reg
fmz <- c(Delta = "DAS", Fairbanks = "FAS", Galena = "GAD", Military = "MID", Southwest = "SWS", Tanana = "TAD", Tok = "TAS", `Upper Yukon` = "UYD")
regions <- list("Statewide" = c('Alaska (ALFRESCO)' = "Alaska"), Ecoregions = reg, `Fire Management Zones` = fmz)
fmos <- c("Unmanaged", "Limited", "Modified", "Full", "Critical", "Other", "Total")
