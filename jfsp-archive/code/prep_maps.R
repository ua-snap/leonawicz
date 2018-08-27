library(alfresco)
library(rasterVis)
library(dplyr)
set.seed(47)

in_dir <- "C:/github/jfsp-archive/data-raw/alf_maps"
x <- readAll(stack(file.path(in_dir, c("Veg_0_2000_tx0.tif", "Veg_0_2040_tx0.tif")))) %>%
  mask(snapgrid::swflam, updatevalue = -1)
x[x > 4] <- 1
x[x == 3] <- 2
clrs <- c("#eeeeee", "#bbbbbb", "yellowgreen", "burlywood4")
classes <- c("Outside domain", "Other vegetation", "Coniferous", "Deciduous")

ratby <- function(x, classes){
  purrr::map(as.list(x), ~({
    x <- raster::ratify(subset(.x, 1))
    rat <- raster::levels(x)[[1]]
    rat$class <- factor(classes, levels = classes)
    levels(x) <- rat
    x
  })) %>% stack
}

x <- ratby(x, classes)

pstrip <- list(cex=1.5, lines=1)
p1 <- levelplot(x, att = "class", col.regions = clrs,
  maxpixels = 1e6, main = "Forest succession 2000 - 2040",
  xlab = NULL, ylab = NULL, scales = list(draw = FALSE), names.attr = c("2000", "2040"), par.strip.text = pstrip,
  colorkey = list(space = "bottom", height = 1, labels = list(cex = 1.5)))

x1 <- subset(x, 1)
x2 <- subset(x, 2)
x0 <- x1
x0[x1 == 2 & x2 == 4] <- 30
x0[x1 == 4 & x2 == 2] <- 20
x0[x0 == 2 | x0 == 4] <- 10
clrs <- c("#eeeeee", "#bbbbbb", "burlywood", "darkgreen", "yellowgreen")
classes <- c("Outside domain", "Other vegetation", "Unchanged", "To coniferous", "To deciduous")
x0 <- ratby(x0, classes)

p2 <- levelplot(x0, att = "class", col.regions = clrs,
  maxpixels = 1e6, main = "Forest succession 2040",
  xlab = NULL, ylab = NULL, scales = list(draw = FALSE),
  colorkey = list(space = "bottom", height = 1, labels = list(cex = 0.75)))

out_dir <- "data-raw/presentation_plots"
png(file.path(out_dir, "ak_cdratio_2000-2040.png"), width = 3000, height = 2000, res = 300, type = "cairo")
p1
dev.off()
png(file.path(out_dir, "ak_cdratio_2040.png"), width = 1800, height = 2000, res = 300, type = "cairo")
p2
dev.off()

radius <- 75000
shift <- 0
xy <- tibble::as_data_frame(wgs2ak(data.frame(x = -147.7164, y = 64.8378))) # Fairbanks
if(shift != 0) xy <- dplyr::mutate(xy, y = y - shift)
veg <- snapgrid::swveg
cells <- extract(veg, xy, buffer = radius, cellnumbers = TRUE)[[1]][, 1]
path <- "data-raw/fbks_maps/fbks_"
suf <- "km of Fairbanks"
r <- radius / 1000

# Near-Fairbanks fire probability, 1950 - 2013 all replicates, cru tx0
m <- readAll(raster("C:/github/jfsp-archive/data-raw/alf_maps/cru_tx0_pfire.tif")) %>%
  mask(snapgrid::swflam)
file <- paste0(path, "sw_pfire.png")
title <- "Alaska status quo 1950-2013 all-replicate mean annual P(Fire)"
Cairo::CairoPNG(file, width = 1000, height = 1000)
levelplot(m, maxpixels = 1e+06, main = title, par.settings = YlOrRdTheme,
          xlab = NULL, ylab = NULL, scales = list(draw = FALSE), margin = FALSE,
          colorkey = list(space = "bottom", height = 1))
dev.off()
file <- paste0(path, r, "km_pfire.png")
title <- paste("Alaska status quo 1950-2013 all-replicate mean annual P(Fire)\nwithin", r, suf)
m[1:ncell(m)][-cells] <- NA
m <- trim(m)
Cairo::CairoPNG(file, width = 1000, height = 1000)
levelplot(m, maxpixels = 1e+06, main = title, par.settings = YlOrRdTheme,
          xlab = NULL, ylab = NULL, scales = list(draw = FALSE), margin = FALSE,
          colorkey = list(space = "bottom", height = 1))
dev.off()

# f <- function(year, dir){
#   files <- list.files(file.path(dir, year), "FireScar", full.names = TRUE)
#   s <- raster::stack(files, bands = 3)
#   s[s == 0] <- 1
#   s[is.na(s)] <- 0
#   raster::calc(s, mean, na.rm = TRUE)
# }

# Near-Fairbanks vegetation map, spinup
m <- veg
m[m == 3] <- 2
file <- paste0(path, r, "km_vegspinup.png")
title <- paste("Vegetation spinup (2005) composition within", r, suf)
classes <- c("No vegetation", "Alpine tundra", "Coniferous", "Deciduous")
clrs <- c("gray80", "yellow", "yellowgreen", "burlywood4")
plot_map_zoom(file, m, cells, radius, classes, clrs, title)

# Near-Fairbanks vegetation map, 2000 tx0 sample rep, cru run
m <- readAll(raster("C:/github/jfsp-archive/data-raw/alf_maps/Veg_0_2000_tx0.tif"))
m[m == 3] <- 2
file <- paste0(path, r, "km_vegsq2000sample.png")
title <- paste("Vegetation 2000 sample composition within", r, suf)
plot_map_zoom(file, m, cells, radius, classes, clrs, title)

# Near-Fairbanks vegetation map, 2040 tx0 sample rep/rcp/gcm
m <- readAll(raster("C:/github/jfsp-archive/data-raw/alf_maps/Veg_0_2040_tx0.tif"))
m[m == 3] <- 2
file <- paste0(path, r, "km_vegsq2040sample.png")
title <- paste("Status quo vegetation 2040 sample composition within", r, suf)
plot_map_zoom(file, m, cells, radius, classes, clrs, title)

# Near-Fairbanks vegetation map, 2040 tx1 sample rep/rcp/gcm
m <- readAll(raster("C:/github/jfsp-archive/data-raw/alf_maps/Veg_0_2040_tx1.tif"))
m[m == 3] <- 2
file <- paste0(path, r, "km_vegtx12040sample.png")
title <- paste("Treatment 1 vegetation 2040 sample composition within", r, suf)
plot_map_zoom(file, m, cells, radius, classes, clrs, title)

# Near-Fairbanks FMO classes map, status quo
m <- readAll(raster("C:/github/Flammability/data/fmo/fmo_standard.tif"))
file <- paste0(path, r, "km_fmosq.png")
title <- paste("Status quo FMO class composition within", r, suf)
classes <- c("No management", "Limited", "Modified", "Full", "Critical")
clrs <- c("gray80", "cornflowerblue", "orange", "firebrick1", "darkred")
plot_map_zoom(file, m, cells, radius, classes, clrs, title, c(1:3, 5, 4))

# Near-Fairbanks FMO classes map, Tx1/Tx2
m <- readAll(raster("C:/github/Flammability/data/fmo/fmo_buffer_Full5.tif"))
file <- paste0(path, r, "km_fmotx1.png")
title <- paste("Treatment 1 FMO class composition within", r, suf)
classes <- c("Limited", "Modified", "Full", "Critical")
clrs <- c("cornflowerblue", "orange", "firebrick1", "darkred")
plot_map_zoom(file, m, cells, radius, classes, clrs, title, c(1:2, 4, 3))
