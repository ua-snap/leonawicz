library(shiny)
load(url("https://s3.amazonaws.com/leonawicz/apps/snapapps/sea_ice_coverage/data.RData")) # public accessible file
mos <- month.abb
modnames <- c("ACCESS-1.0","CESM1-CAM5","CMCC-CM","HADGEM2-AO","MIROC-5","Composite model")
