library(alfresco)
library(dplyr)
library(parallel)
rasterOptions(chunksize = 1e+10, maxmemory = 1e+11)
rcps <- paste0("rcp", c(45, 60, 85))
gcms <- c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
xy_fbks <- tibble::as_data_frame(wgs2ak(data.frame(x = -147.7164, y = 64.8378)))
#xy_fbks <- dplyr::mutate(xy_fbks, y = y - 30000)
xb <- seq(1000, 50000, length.out = 100)
reps <- 1:1

# tx none, tx0 and tx1, all years
tx <- c("cru_none", "cru_tx0", rep("gcm_tx0", 15), rep("gcm_tx1", 15))
c1 <- c("00", 99)
c2 <- c("00", 95)
sets1 <- paste0("fmo", c1, "s", c2, "i.historical.CRU32")
sets2 <- paste0("fmo99s95i.", rep(rcps, each = 5), ".", rep(gcms, 3))
sets <- c(sets1, rep(sets2, 2))
labs <- paste(sets, tx, sep = ".")
in_dir <- file.path("/atlas_scratch/mfleonawicz/alfresco/JFSP/outputs", tx, sets, "Maps")
years <- list(1950:2013, 2014:2099)[c(1, 1, rep(2, 30))]
path <- "/workspace/UA/mfleonawicz/fire_prob_allyears/fire_prob_fbks_allyears"
for(k in seq_along(sets)){
  print(k)
  ids <- strsplit(sets[k], "\\.")[[1]]
  x <- parallel::mclapply(reps, point_fire, base_path = in_dir[k], label = ids[2], sample_size = 2, sample_buffer = 1000,
                          center = xy_fbks, extraction_buffers = xb, years = years[[k]], mc.cores = length(reps))
  x2 <- bind_rows(x) %>% select(-run) %>% mutate(Set = ids[1], Tx = tx[k], RCP = ids[2], Model = ids[3])
  saveRDS(x2, paste0(path, c(paste0(0, 1:9), 10:length(sets))[k], ".rds"))
  gc()
}
x <- purrr::map(list.files(dirname(path), full.names = TRUE), readRDS) %>% bind_rows()
saveRDS(x, "/workspace/UA/mfleonawicz/fire_prob_fbks_allyears.rds")

# tx0 and tx1, 2014 - 2040
tx <- c(rep("gcm_tx0", 15), rep("gcm_tx1", 15))
sets <- rep(paste0("fmo99s95i.", rep(rcps, each = 5), ".", rep(gcms, 3)), 2)
labs <- paste(sets, tx, sep = ".")
in_dir <- file.path("/atlas_scratch/mfleonawicz/alfresco/JFSP/outputs", tx, sets, "Maps")
years <- list(2014:2040)[rep(1, 30)]
x <- vector("list", length(sets))
for(k in seq_along(sets)){
  print(k)
  ids <- strsplit(sets[k], "\\.")[[1]]
  x[[k]] <- parallel::mclapply(reps, point_fire, base_path = in_dir[k], label = ids[2], sample_size = 2, sample_buffer = 1000,
                               center = xy_fbks, extraction_buffers = xb, years = years[[k]], mc.cores = length(reps)) %>% 
  bind_rows() %>% select(-run) %>% mutate(Set = ids[1], Tx = tx[k], RCP = ids[2], Model = ids[3])
}
x <- bind_rows(x)
saveRDS(x, "/workspace/UA/mfleonawicz/fire_prob_fbks_2014-2040.rds")

# tx0 and tx1, 2041 - 2099
years <- list(2041:2099)[rep(1, 30)]
x <- vector("list", length(sets))
for(k in seq_along(sets)){
  print(k)
  ids <- strsplit(sets[k], "\\.")[[1]]
  x[[k]] <- parallel::mclapply(reps, point_fire, base_path = in_dir[k], label = ids[2], sample_size = 2, sample_buffer = 1000,
                               center = xy_fbks, extraction_buffers = xb, years = years[[k]], mc.cores = length(reps)) %>% 
  bind_rows() %>% select(-run) %>% mutate(Set = ids[1], Tx = tx[k], RCP = ids[2], Model = ids[3])
}
x <- bind_rows(x)
saveRDS(x, "/workspace/UA/mfleonawicz/fire_prob_fbks_2041-2099.rds")

# combine
library(dplyr)
files <- paste0("/workspace/UA/mfleonawicz/fire_prob_fbks_", c("2014-2040", "2041-2099", "allyears"), ".rds")
period_labels <- c("2014 - 2040", "2041 - 2099", "1950 - 2013 and 2014 - 2099")
d <- purrr::map2(files, period_labels, ~mutate(readRDS(.x), Years = .y)) %>% bind_rows()
d <- mutate(d, Tx = substring(Tx, 5))

saveRDS(d, "/workspace/UA/mfleonawicz/fire_prob_fbks_all.rds")
d <- filter(d, Set == "fmo99s95i") %>% 
  mutate(#value = as.numeric(value != 0), 
         Tx = ifelse(Tx == "tx0", "Status quo", ifelse(Tx == "tx1", "Treatment 1", "Treatment 2"))) %>%
  group_by(Tx, RCP, Years, buffer) %>% summarise(value = mean(value)) %>% ungroup %>% 
  arrange(Years, Tx, RCP, buffer) %>% mutate(Years = factor(Years), Tx = factor(Tx), RCP = factor(RCP))
saveRDS(d, "/workspace/UA/mfleonawicz/fire_prob_fbks_simplified.rds")
