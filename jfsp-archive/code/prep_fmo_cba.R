library(alfresco)
library(dplyr)

fmz_ids <- c(Delta = "DAS", Fairbanks = "FAS", Galena = "GAD", Military = "MID", Southwest = "SWS", Tanana = "TAD", Tok = "TAS",  `Upper Yukon` = "UYD")
mask_values <- c(list(NULL), as.list(c("Arctic Tundra", "Bering Taiga", "Bering Tundra", "Intermontane Boreal", fmz_ids)))
masks <- c(list(NULL), as.list(rep(c("ecoreg", "fmz"), times = c(4, 8))))

# historical observations
b <- readAll(brick("/big_scratch/shiny/firescarbrick_annual_observed_Statewide_lightning_1950_2013.tif"))

ba_fmo_region <- function(mask = NULL, mask_value = NULL, b){
  id <- 0:5
  labels <- c("Unmanaged", "Limited", "Modified", "Critical", "Full", "Other")
  r <- snapgrid::swfmo
  if(!is.null(mask) & !is.null(mask_value)){
    if(mask == "ecoreg") x <- subset(snappoly::ecoreg, LEVEL_2 == mask_value)
    if(mask == "fmz"){
      x <- subset(snappoly::fmz, REGION == mask_value)
    }
    cells <- raster::extract(r, x, cellnumbers = TRUE) %>% purrr::map(~.x[, 1]) %>% unlist() %>% sort()
    idx <- purrr::map(id, ~which(r[] == .x & seq_along(r) %in% cells))
  } else {
    idx <- purrr::map(id, ~which(r[] == .x))
  }
  f <- function(b, i, idx) purrr::map_dbl(idx, ~length(which(as.numeric(raster::extract(subset(b, i), .x)) > 0)))
  
  x <- purrr::map(1:64, ~f(b, .x, idx))
  x <- as_data_frame(do.call(rbind, x))
  x <- cbind(1950:2013, x)
  names(x) <- c("Year", labels)
  x <- tbl_df(x)
  x$Set <- "Observed"
  x$Tx <- "tx0"
  x <- select(x, c(8, 9, 1:7))
  if(is.null(mask)) prefix <- "sw" else if(mask == "ecoreg") prefix <- "eco" else if(mask == "fmz") prefix <- "fmz"
  suffix <- ifelse(is.null(mask_value), "alaska", mask_value)
  saveRDS(x, paste0("/workspace/UA/mfleonawicz/ba_fmo_", prefix, "_", suffix, ".rds"))
  invisible()
}

parallel::mclapply(seq_along(masks), function(i, m, mv, b) ba_fmo_region(m[[i]], mv[[i]], b), m = masks, mv = mask_values, b = b, mc.cores = length(masks))

# historical and projected modeled outputs
library(alfresco)
library(dplyr)

tx <- c("cru_none", "cru_tx0", rep("gcm_tx0", 15), rep("gcm_tx1", 15))
rcps <- paste0("rcp", c(45, 60, 85))
gcms <- c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
c1 <- c("00", 99)
c2 <- c("00", 95)
sets1 <- paste0("fmo", c1, "s", c2, "i.historical.CRU32")
sets2 <- paste0("fmo99s95i.", rep(rcps, each = 5), ".", rep(gcms, 3))
sets <- c(sets1, rep(sets2, 2))
in_dir <- file.path("/atlas_scratch/mfleonawicz/alfresco/JFSP/outputs", tx, sets, "/Maps")
years <- list(1950:2013, 2014:2099)[c(1, 1, rep(2, 30))]
fmo_tx1 <- readAll(raster("/workspace/UA/mfleonawicz/projects/Flammability/data/fmo/fmo_buffer_Full5.tif"))
fmo <- vector("list", length(tx))
fmo[tx == "gcm_tx1"] <- rep(as.list(fmo_tx1), 15)

ba_fmo <- function(in_dir, years, id = 0:5, labels = c("Unmanaged", "Limited", "Modified", "Critical", "Full", "Other"), 
                   fmo_layer = NULL, mask = NULL, mask_value = NULL){
  if(is.null(fmo_layer)) fmo_layer <- snapgrid::swfmo
  if(!is.null(mask) & !is.null(mask_value)){
    if(mask == "ecoreg") x <- snappoly::ecoreg[snappoly::ecoreg[["LEVEL_2"]] == mask_value, ]
    if(mask == "fmz") x <- snappoly::fmz[snappoly::fmz[["REGION"]] == mask_value, ]
    cells <- raster::extract(fmo_layer, x, cellnumbers = TRUE) %>%
      purrr::map(~.x[, 1]) %>% unlist() %>% sort()
    idx <- purrr::map(id, ~which(fmo_layer[] == .x & seq_along(fmo_layer) %in% cells))
  }
  else {
    idx <- purrr::map(id, ~which(fmo_layer[] == .x))
  }
  f <- function(year, id) {
    files <- list.files(file.path(in_dir, year), pattern = "^FireScar", full.names = TRUE)
    s <- raster::readAll(raster::stack(files, bands = 2))
    x <- purrr::map_dbl(idx, ~length(which(!is.na(as.numeric(raster::extract(s, .x)))))/raster::nlayers(s))
    x <- c(year, x)
    names(x) <- c("Year", labels[id + 1])
    do.call(tibble::data_frame, as.list(x))
  }
  purrr::map(years, ~f(.x, id)) %>% dplyr::bind_rows()
}

fmz_ids <- c(Delta = "DAS", Fairbanks = "FAS", Galena = "GAD", Military = "MID", Southwest = "SWS", Tanana = "TAD", Tok = "TAS",  `Upper Yukon` = "UYD")
mask_values <- c(list(NULL), as.list(c("Arctic Tundra", "Bering Taiga", "Bering Tundra", "Intermontane Boreal", fmz_ids)))
masks <- c(list(NULL), as.list(rep(c("ecoreg", "fmz"), times = c(4, 8))))

j <- 1 # 13 masking options, do one at a time: 
mask <- masks[[j]]
mask_value <- mask_values[[j]]
x <- vector("list", length(sets))
for(k in seq_along(sets)){
  print(k)
  x[[k]] <- parallel::mclapply(seq_along(years[[k]]),
                               function(i, years, fmo, mask, mask_value){
                                 ba_fmo(in_dir[k], years[[k]][i], fmo_layer = fmo[[k]], mask = mask, mask_value = mask_value)
                                 }, years = years, fmo = fmo, mask = mask, mask_value = mask_value, mc.cores = 64) %>%
    bind_rows() %>% mutate(Set = sets[k], Tx = strsplit(tx[k], "_")[[1]][2]) %>% select(c(8, 9, 1:7))
  gc()
}
x <- bind_rows(x)
prefix <- if(is.null(mask)) "sw" else mask
suffix <- if(is.null(mask_value)) "alaska" else mask_value
saveRDS(x, paste0("/workspace/UA/mfleonawicz/ba_fmo2_", prefix, "_", suffix, ".rds"))
