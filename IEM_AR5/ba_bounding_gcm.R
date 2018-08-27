setwd("C:/github/IEM_AR5")

library(data.table)
library(dplyr)
library(rvtable)
library(ggplot2)

load("Sboreal_NoFMO_vegareaStats.RData")
area.Sboreal <- group_by(stats.alf.vegarea, Scenario, Model, Year) %>% summarise(Area=sum(Mean)) %>% group_by %>% summarise(Area=mean(Area))
load("Nboreal_NoFMO_vegareaStats.RData")
area.Nboreal <- group_by(stats.alf.vegarea, Scenario, Model, Year) %>% summarise(Area=sum(Mean)) %>% group_by %>% summarise(Area=mean(Area))
load("Tundra_NoFMO_vegareaStats.RData")
area.tundra <- group_by(stats.alf.vegarea, Scenario, Model, Year) %>% summarise(Area=sum(Mean)) %>% group_by %>% summarise(Area=mean(Area))
d.area <- bind_rows(area.Sboreal, area.Nboreal, area.tundra) %>% mutate(Region=c("Sboreal", "Nboreal", "Tundra"))

den.args <- list(n=10000, adjust=0.1, from=0)
samp.args <- list(n=10000, interp=TRUE, n.interp=100000, decimals=NULL)

load("Sboreal_StdFMO_ba.RData")
d <- filter(d.alf.ba, Vegetation=="All") %>% rvtable
d1 <- d %>% marginalize(c("Scenario", "Year"), density.args=den.args)

load("Nboreal_StdFMO_ba.RData")
d <- filter(d.alf.ba, Vegetation=="All") %>% rvtable
d2 <- d %>% marginalize(c("Scenario", "Year"), density.args=den.args)

load("Tundra_StdFMO_ba.RData")
d <- filter(d.alf.ba, Vegetation=="All") %>% rvtable
d3 <- d %>% marginalize(c("Scenario", "Year"), density.args=den.args)

d <- bind_rows(d1, d2, d3) %>% data.table %>% rvtable
d <- marginalize(d, "Location", d.area$Area, den.args, samp.args)

ggplot(sample_rvtable(d), aes(log(Val+1))) + geom_density()
d.stats <- sample_rvtable(d) %>% summarise(Min=min(Val), Pct05=quantile(Val, prob=0.05), Mean=mean(Val), Median=median(Val), Pct95=quantile(Val, prob=0.95), Max=max(Val))

d.prob <- purrr::map(sample(5000:40000, 100), ~inverse_pmf(d, c(.x, 100000), "Model", samp.args)) %>% bind_rows %>% group_by(Val) %>% summarise(Prob=mean(Prob)) %>% arrange(Prob)
d.prob
