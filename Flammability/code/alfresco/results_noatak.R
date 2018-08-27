set <- "Noatak"
setwd(paste0("/atlas_scratch/mfleonawicz/alfresco/CMIP5_", set, "/outputs"))

library(dplyr)

runs <- list.files(pattern="*_rcp*")
runtab <- data.frame(do.call(rbind, strsplit(runs, "_"))[, c(1,4,5)])
names(runtab) <- c("GBM", "RCP", "GCM")
runtab$GBM <- as.integer(substr(runtab$GBM, 2, 2))
runtab$dir <- runs

d <- vector("list", length(runs))
for(i in seq_along(runs)){
  load(paste0(runs[i], "/fsByVeg_df_", set, ".RData"))
  d.fs <- d.fs %>% filter(Domain=="Masked" & Source=="Modeled" & Year>=2014) %>%
    mutate(Replicate=as.integer(substr(Replicate, 5, nchar(Replicate)))) %>%
    group_by(Replicate, Year) %>% summarise(BA=sum(FS)) %>%
    mutate(CBA=cumsum(BA), GBM=runtab$GBM[i], RCP=runtab$RCP[i], GCM=runtab$GCM[i]) %>%
    select(GBM, RCP, GCM, Replicate, Year, BA, CBA)
  d[[i]] <- d.fs
}
d <- bind_rows(d)

saveRDS(d, paste0("/workspace/UA/mfleonawicz/ba_", set, ".rds")) # temp file, transfer from server to local

#################################################################

setwd("C:/github/Flammability/workspaces/Noatak_final")
d <- readRDS("ba_Noatak.rds")

library(dplyr)
library(ggplot2)

#d.sub <- filter(d, GCM=="CCSM4" & GBM==3)
clrs.rcp <- c("#2196f3", "#ff9800", "#795548")
clrs.gcm <- c("#2196f3", "#ff9800", "#e91e63", "#009688", "#795548")

d2 <- d %>% group_by(GBM, RCP, GCM, Year) %>% summarise(CBA_025=quantile(CBA, 0.025), CBA_mean=mean(CBA), CBA_975=quantile(CBA, 0.975))

ggplot(d, aes(Year, CBA, colour=RCP, group=interaction(RCP, Replicate))) +
  geom_step() + facet_grid(GBM~GCM, switch="both") + scale_color_manual(values=clrs.rcp)

ggplot(d2, aes(Year, CBA_mean, colour=RCP)) +
  geom_ribbon(aes(ymin=CBA_025, ymax=CBA_975, fill=RCP), alpha=0.2) + stat_smooth(se=FALSE, span=0.15) + facet_grid(GBM~GCM, switch="both") +
  scale_color_manual(values=clrs.rcp) + scale_fill_manual(values=clrs.rcp) + theme(legend.position="bottom")

ggplot(d2, aes(Year, CBA_mean, colour=GCM)) +
  geom_ribbon(aes(ymin=CBA_025, ymax=CBA_975, fill=GCM), alpha=0.2) +
  stat_smooth(se=FALSE, span=.15) + facet_grid(GBM~RCP, switch="both") +
  scale_color_manual(values=clrs.gcm) + scale_fill_manual(values=clrs.gcm) + theme(legend.position="bottom")

d3a <- d %>% group_by(RCP) %>%
  summarise(ABA_025=quantile(BA, 0.025), ABA_mean=mean(BA), ABA_975=quantile(BA, 0.975), ABA_sd=sd(BA)) %>%
  mutate_each(funs(round(., 1)), ABA_025, ABA_mean, ABA_975, ABA_sd) %>% arrange(ABA_mean) %>%
  rename(`Marginal Scenario`=RCP)
d3b <- d %>% group_by(GCM) %>%
  summarise(ABA_025=quantile(BA, 0.025), ABA_mean=mean(BA), ABA_975=quantile(BA, 0.975), ABA_sd=sd(BA)) %>%
  mutate_each(funs(round(., 1)), ABA_025, ABA_mean, ABA_975, ABA_sd) %>% arrange(ABA_mean) %>%
  rename(`Marginal Scenario`=GCM)
d3 <- bind_rows(d3a, d3b)


