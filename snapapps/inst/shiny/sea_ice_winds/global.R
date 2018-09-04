library(shiny)
load(url("https://s3.amazonaws.com/leonawicz/apps/snapapps/sea_ice_winds/wind_ice.RData")) # public accessible file

cuts <- rev(unique(w.beaufort.GFDL$Cut))
varlevels <- as.character(unique(w.beaufort.GFDL$Var))
years <- unique(w.beaufort.GFDL$Year)
decades <- years[years %% 10 == 0]
seas <- c("Beaufort", "Bering", "Chukchi")
models <- unique(sapply(strsplit(ls(pattern = "^w.*.c$", envir = .GlobalEnv), "\\."), "[[", 3))
