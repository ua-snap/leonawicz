setwd("/atlas_scratch/mfleonawicz/projects/bird")
dir.create(outDir <- "plots", showWarnings=FALSE)
lapply(c("data.table", "dplyr", "ggplot2", "rvtable"), library, character.only=T)
vegtypes <- c("Black Spruce", "White Spruce", "Deciduous")
#lev <- c("Spruce", vegtypes[3:5])
#load("historical_vegage.RData")
load("projected_vegage.RData")
d <- filter(d.alf.vegage, Scenario=="SRES A1B" & Model %in% c("CCCMAcgcm31", "MPIecham5") & Vegetation %in% vegtypes) %>%
  rvtable #%>% group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year)

#d <- filter(d, Vegetation %in% vegtypes[1:2]) %>% marginalize("Vegetation") %>% mutate(Vegetation=factor("Spruce", levels=lev)) %>%
#    bind_rows(filter(d, Vegetation %in% vegtypes[3:5]) %>% mutate(Vegetation=factor(Vegetation, levels=lev))) %>% data.table %>%
#    group_by(Phase, Scenario, Model, Location, Var, Vegetation, Year) %>% disttable %>% uc_table

d <- marginalize(d, "Year", density.args=list(from=0)) %>% sample_rvtable

e <- element_blank()
tp_theme <- theme(panel.grid=e, strip.background=e,
                  panel.background=element_rect(fill="transparent"), plot.background=element_rect(fill="transparent"))

png("/workspace/UA/mfleonawicz/age_test.png", height=1600, width=4800, res=300)
#png("plots/age_test.png", height=2000, width=2000, res=300)
ggplot(d, aes(Val, colour=Model)) +
  geom_line(stat="density") +
  theme_bw() + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(),
                     strip.text=element_text(size=12)) +
  labs(title="2008-2100 AK-CAN Boreal LCC stand age", x=expression(Age~(years)), y="Density") +
  facet_wrap(~Vegetation, scales="free") +
  tp_theme
dev.off()
