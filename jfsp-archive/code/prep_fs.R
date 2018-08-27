library(dplyr)
path <- "/atlas_scratch/mfleonawicz/alfresco/JFSP/extractions"
tx <- c("cru_tx0", rep("gcm_tx0", 15), rep("gcm_tx1", 15))
rcps <- c("historical", rep(rep(paste0("rcp", c(45, 60, 85)), each = 5), 2))
models <- c("CRU32", rep(c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"), 6))
varid <- "fsv"
domain <- "Statewide__AK"
files <- file.path(path, tx, varid, paste(varid, domain, models, rcps, "fmo99s95i.rds", sep = "__"))
d <- parallel::mclapply(seq_along(files), function(i, files, tx) readRDS(files[i]) %>% mutate(Tx = tx[i]), files = files, tx = substring(tx, 5), mc.cores = 31) %>% 
  bind_rows() %>% ungroup() %>% select(-c(1, 4:6)) %>% rename(Set = FMO, RCP = Scenario)
d <- group_by(d, Set, Tx, RCP, Model, Year, Replicate, FID) %>%
  summarise(Val = sum(Val)) %>% group_by(Set, Tx, RCP, Model, Year, Val) %>% summarise(Freq = n()) %>%
  ungroup %>% mutate(RCP = factor(RCP, levels = c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")))
saveRDS(d, "/workspace/UA/mfleonawicz/fs.rds")
