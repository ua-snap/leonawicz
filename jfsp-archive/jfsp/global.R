ptm <- proc.time()
library(rintrojs)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinytoastr)
library(apputils)
library(snaputils)
library(DT)

library(sp)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpmisc)
library(knitr)
library(rmarkdown)
library(aws.s3)

cat("Total library load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
load("appData/appData.RData") # load any default local data sets
dataloc <- "s3://leonawicz/apps/jfsp" # specify location for any external data sets
source("aws_key.R") # authentication to AWS
cat("Total data load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
action_btn_style <- "color: black; margin: 10px 15px 10px 15px; width: 200px;"
groupby_vars <- c("", "RCP", "GCM"="Model", "Fire Mgmt Zone"="Region", "Vegetation")
pooled_options <- c("Average observations", "Unique observations")
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")
names(mapsets)[1] <- "Full AK simulation domain"

# load modules
source("modules/main/mod_utils.R")
source("modules/main/mod.R")
source("modules/plots/mod_plots.R")

#enableBookmarking(store="server") # not yet available on shinyapps.io
temptext <- p(em("This website was developed as part of project (#16-1-01-18) funded by the Joint Fire Science Program.
If you would be interested in participating in an interview to guide the direction of the management scenarios implemented
as part of this work, please contact the project PI, Courtney Schultz (courtney.schultz@colostate.edu)"))

cat("Remainder global.R time:\n")
print(proc.time() - ptm)
