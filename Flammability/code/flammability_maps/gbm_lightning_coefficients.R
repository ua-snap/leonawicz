# @knitr setup
setwd("C:/github/Flammability/workspaces")
dir.create(plotDir <- "../plots/lightning", showWarnings=FALSE)

load("gbm_lightning_preds_1950_2099.RData")

library(reshape2)
library(ggplot2)
library(data.table)
library(dplyr)

# @knitr support_functions
get_classes1 <- function(x, y=qtiles) cut(x, breaks=c(0, y, 99999), labels=F)
get_coefficients <- function(x, b=bounds, use.ecdf=TRUE, trim=FALSE){
    if(use.ecdf){
        y <- ecdf(x)(x) # x represents lightning values
        if(trim) { y[y < 0.1] <- 0.1; y[y>0.9] <- 0.9 }
        return(y)
    }
    b <- c(0.01, b, 1) # x represents bin integers
    #sapply(x, function(y) switch(y, '1'=0.05, '2'=0.5, '3'=0.95))
    sapply(x, function(y, b) runif(1, b[y], b[y+1]), b=b)
}
get_classes2 <- function(x) factor(x, labels=c("Low", "Medium", "High"))

# @knitr quantiles
bounds <- c(0.2, 0.8)
preds.cru32 <- filter(lightning.preds, Model=="CRU32")$LightPred
qtiles <- quantile(preds.cru32, bounds)
bins.cru <- get_classes1(preds.cru32)
coef.cru <- get_coefficients(preds.cru32)
bins.cru <- get_classes2(bins.cru)
d <- data.table(Scenario="historical", Model="CRU32", Year=1950:2013, LightPred=preds.cru32, Rank=rank(preds.cru32), Class=bins.cru, Coef=coef.cru, ECDF=ecdf(preds.cru32)(preds.cru32))

# @knitr plots_cru32
title.prefix <- "1950-2013 CRU 3.2 GBM-predicted summer lightning strikes: "
(g1 <- ggplot(data=d, aes(x=Rank, y=LightPred, label=Year)) +
    geom_hline(yintercept=qtiles, linetype=2) + geom_point() +
    geom_text(aes(colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=1, y=qtiles, label=paste("qunatile =", bounds), size=3, vjust=-0.5) +
    labs(x="Predicted rank", y="Predicted number of strikes", title=paste0(title.prefix, "ranked and ordered")))
    
(g2 <- ggplot(data=d, aes(x=Year, y=LightPred, label=Rank)) +
    geom_line() + geom_hline(yintercept=qtiles, linetype=2) + geom_point() +
    geom_text(aes(colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=1950, y=qtiles, label=paste("qunatile =", bounds), size=3, vjust=-0.5) +
    labs(x="Year", y="Predicted number of strikes", title=paste0(title.prefix, "time series")))
    
(g3 <- ggplot(data=d, aes(x=LightPred, label=Year)) +
    geom_vline(xintercept=qtiles, linetype=2) + stat_ecdf() + stat_ecdf(geom="point") +
    geom_text(aes(y=ECDF, colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=qtiles, y=0, label=paste("qunatile =", bounds), size=3, hjust=-0.1) +
    labs(x="Predicted number of strikes", y="CDF", title=paste0(title.prefix, "empirical CDF")))

# @knitr notrun1
png(file.path(plotDir, "gbm_pred_1950_2011_Rank.png"), width=3200, height=1600, res=200)
g1
dev.off()

png(file.path(plotDir, "gbm_pred_1950_2011_TS.png"), width=3200, height=1600, res=200)
g2
dev.off()

png(file.path(plotDir, "gbm_pred_1950_2011_CDF.png"), width=3200, height=1600, res=200)
g3
dev.off()

# @knitr plots_gcm_prep
lightning.preds %>% group_by(Scenario, Model) %>% filter(Model!="CRU32") %>%
    mutate(Rank=rank(LightPred), Class=get_classes1(LightPred), Coef=get_coefficients(LightPred), ECDF=ecdf(LightPred)(LightPred)) %>%
    mutate(Class=get_classes2(Class)) %>% setcolorder(names(d)) -> d2
d %>% rbind(d2) %>% mutate(Model=factor(Model, levels=unique(Model))) -> d.all
rcp.lab <- "RCP 6.0"
rcp <- tolower(gsub("[ .]", "", rcp.lab))
d2 %>% filter(Scenario %in% c("historical", rcp)) %>% mutate(Scenario=rcp) %>% rbind(d) %>% mutate(Model=factor(Model, levels=c("CRU32", unique(Model)[1:5]))) -> d.sub

# @knitr plots_gcm
title.prefix <- paste("1950-2013 CRU 3.2 and 1950-2005/2006-2099 GCM", rcp.lab, "\n")
(g4 <- ggplot(data=d.sub, aes(x=Rank, y=LightPred, label=Year)) +
    geom_hline(yintercept=qtiles, linetype=2) + geom_point() +
    geom_text(aes(colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=1, y=qtiles, label=paste("CRU qunatile =", bounds), size=3, hjust=0.25, vjust=-0.25) +
    labs(x="Predicted rank", y="Predicted number of strikes", title=paste0(title.prefix, "GBM-predicted summer lightning strikes: ranked and ordered")) +
    facet_wrap(~ Model, ncol=2))
    
(g5 <- ggplot(data=d.sub, aes(x=Year, y=LightPred, label=Rank)) +
    geom_line() + geom_hline(yintercept=qtiles, linetype=2) + geom_point() +
    geom_text(aes(colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=1950, y=qtiles, label=paste("CRU qunatile =", bounds), size=3, hjust=0.25, vjust=-0.25) +
    labs(x="Year", y="Predicted number of strikes", title=paste0(title.prefix, "GBM-predicted summer lightning strikes: time series")) +
    facet_wrap(~ Model, ncol=2))
    
(g6 <- ggplot(data=filter(d.all, Scenario=="historical"), aes(x=LightPred, label=Year)) +
    geom_vline(xintercept=qtiles, linetype=2) + stat_ecdf() + stat_ecdf(geom="point") +
    geom_text(aes(y=ECDF, colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    stat_ecdf(data=filter(d.all, Scenario=="rcp60")) + stat_ecdf(data=filter(d.all, Scenario=="rcp60"), geom="point") +
    geom_text(data=filter(d.all, Scenario=="rcp60"), aes(y=ECDF, colour=Class), hjust=0, vjust=0, size=3, show_guide=F) +
    annotate("text", x=qtiles, y=0, label=paste("CRU qunatile =", bounds), size=3, hjust=-0.1, vjust=-0.125) +
    labs(x="Predicted number of strikes", y="CDF", title=paste0(title.prefix, "GBM-predicted summer lightning strikes: empirical CDF")) +
    facet_wrap(~ Model, ncol=2))

# @knitr setup_preds_uncertainty
cbpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
acf(filter(lightning.preds, Model=="CRU32")$LightPred) # uncorrelated at lag 1

genSim <- function(i, data){
    data %>% filter(Model!="CRU32") %>% group_by(Year) %>% sample_n(1) %>% select(Scenario, Year, LightPred) -> d
    d[, Sim:=i]
    d
}

d.new <- select(d, Scenario, Year, LightPred)
d.new[, Sim:=-1]
d.emp <- data.table(Scenario="Empirical", Year=2003:2011, LightPred=lightning.obs, Sim=0)
set.seed(47)
d.sim <- rbindlist(lapply(1:100, genSim, data=d.sub))
d.sim.all <- rbind(d.emp, d.new, d.sim)
d.sim.all$Scenario = factor(d.sim.all$Scenario, levels=c("historical", rcp, "Empirical"))
d.sim.all %>% group_by(Scenario, Year) %>% summarise(Mean=mean(LightPred), Lb=min(LightPred), Ub=max(LightPred)) -> d.sim.cb

library(scales)
library(grid)
cols <- c("5-model uncertainty"=alpha("black", 0.2), "5-model mean estimated strikes"="black", "CRU 3.2 historical estimated strikes"=cbpal[6],"Observed strikes"=cbpal[7])

# @knitr plots_preds_uncertainty
(g7 <- ggplot(data=subset(d.sim.cb, Scenario=="rcp60"), aes(x=Year, y=Mean)) +
    geom_line(aes(colour="5-model mean estimated strikes"), size=1) +
    geom_ribbon(aes(ymin=Lb, ymax=Ub, fill="5-model uncertainty")) +
    geom_line(data=subset(d.sim.cb, Scenario=="historical"), aes(colour="CRU 3.2 historical estimated strikes"), size=1) +
    geom_line(data=subset(d.sim.cb, Scenario=="Empirical"), aes(colour="Observed strikes"), size=1, linetype=2) + 
    geom_point(data=subset(d.sim.cb, Scenario=="Empirical"), aes(colour="Observed strikes"), size=3) + 
    geom_smooth(data=d.sim.all, aes(x=Year, y=LightPred), method='lm',formula=y~x, colour=1, size=1) +
    geom_smooth(data=d.sim.all, aes(x=Year, y=LightPred),formula=y~x, colour=1, size=1) +
    labs(x="Year", y="Number of strikes", title="1950-2099 Observed and GBM-estimated summer lightning strikes") +
    scale_colour_manual(name="",values=cols) + scale_fill_manual(name="",values=cols) +
    theme_bw(base_size=16) + theme(legend.position="bottom", legend.box="horizontal") + guides(colour=guide_legend(reverse=T, order=1), fill=guide_legend(reverse=T, order=2)) +
    scale_x_continuous(breaks=seq(1950, 2100, by=10)))
    
# @knitr notrun2
png(file.path(plotDir, "gbm_pred_1950_2099_RankbyModel.png"), width=3200, height=1600, res=200)
g4
dev.off()

png(file.path(plotDir, "gbm_pred_1950_2099_TSbyModel.png"), width=3200, height=1600, res=200)
g5
dev.off()

png(file.path(plotDir, "gbm_pred_1950_2099_CDFbyModel.png"), width=3200, height=1600, res=200)
g6
dev.off()

png(file.path(plotDir, "gbm_pred_lightning_1950_2099_TSuncertainty.png"), width=5000, height=2500, res=300)
g7
dev.off()

# @knitr notrun3
save(d.all, file="./gbmFlammability/gbm_lightning_coefficients.RData")
