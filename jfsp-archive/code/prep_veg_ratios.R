library(dplyr)
path <- "/atlas_scratch/mfleonawicz/alfresco/JFSP/extractions"
tx <- c("cru_tx0", rep("gcm_tx0", 15), rep("gcm_tx1", 15))
rcps <- c("historical", rep(rep(paste0("rcp", c(45, 60, 85)), each = 5), 2))
models <- c("CRU32", rep(c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"), 6))
varid <- "veg"
domain <- "Statewide__AK"
files <- file.path(path, tx, varid, paste(varid, domain, models, rcps, "fmo99s95i.rds", sep = "__"))
d <- parallel::mclapply(seq_along(files), function(i, files, tx) readRDS(files[i]) %>% mutate(Tx = tx[i]), files = files, tx = substring(tx, 5), mc.cores = 31) %>% 
  bind_rows() %>% ungroup() %>% select(-c(1, 4:6)) %>%
  filter(Vegetation %in% c("Black Spruce", "White Spruce", "Deciduous")) %>% 
  mutate(Scenario = factor(Scenario, levels = c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5"))) %>%
  rename(RCP = Scenario) %>% split(.$Vegetation == "Deciduous")
d <- group_by(d[[1]], Tx, RCP, Model, Year, Replicate, FMO) %>% summarise(Val = sum(Val)) %>% ungroup %>% 
  mutate(Vegetation = "Coniferous") %>% bind_rows(mutate(d[[2]], Vegetation = "Deciduous")) %>% select(-Replicate) %>%
  mutate(Vegetation = factor(Vegetation, levels = c("Coniferous", "Deciduous")))
d <- group_by(d, Tx, RCP, Model, Year, FMO, Vegetation) %>% summarise(Val = mean(Val)) %>% ungroup
saveRDS(d, "/workspace/UA/mfleonawicz/conif_decid_area.rds")
