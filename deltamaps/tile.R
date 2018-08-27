library(tiler)
files <- list.files("data", full.names = TRUE)

clrs <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(100)
tile(files[1], file.path("tiles/1", gsub("\\.tif", "", basename(files[1]))), "3-7", col = clrs)

clrs <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(20)
tile(files[1], file.path("tiles/2", gsub("\\.tif", "", basename(files[1]))), "3-7", col = clrs)
