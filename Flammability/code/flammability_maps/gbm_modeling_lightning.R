# @knitr setup
library(data.table); library(reshape2); library(dplyr); library(gbm)

# climate data
setwd("C:/github/Flammability/workspaces/tpByVeg")
load("tpByVeg_means_CRU32_individual.RData")
X <- d
load("tpByVeg_means_CMIP5_individual.RData")
X <- rbind(X, d)
rm(d)

X %>% filter(Vegetation=="forest") %>% mutate(Month=factor(Month, levels=month.abb), Var=substr(Var,1,1)) %>%
    group_by(Scenario, Model, Year, Month, Var) %>% summarise(Val=mean(Val)) %>%
    dcast(Scenario + Model + Year ~ Month + Var, value.var="Val") %>% data.table() -> X
setnames(X, gsub("_", "", names(X)))

# fire and lightning data
load("../gbmFlammability/baByVeg_historical_1950_2013.RData") # burn area (km^2) by veg class, contains d.ba.pct.noatak and d.ba.pct.sw
light.yrs <- 2003:2011
lightning.current <- filter(X, Model=="CRU32" & Year %in% light.yrs)
lightning.current$Positive.Strokes <- lightning.obs <- c(9792, 37067, 28793, 11632, 32129, 15601, 14318, 15423, 7878) # 2003-2011 AK SW forest positive strikes
lightning.current$ba <- with(d.ba.pct.sw, total[Year %in% light.yrs])

# @knitr gbm
# gbm modeling
gbm.light <-
gbm(Positive.Strokes ~
    JunP +
    JulP +
    AugP +
    JunT +
    JulT +
    AugT,
    distribution="gaussian", bag.fraction=0.9, cv.folds=5,
    weights=sqrt(lightning.current$Positive.Strokes),
    data=lightning.current, verbose=F, interaction.depth=2, n.minobsinnode=2, n.trees=500, shrinkage=0.025)
    
# plots
#win.graph(height=6, width=8)
tmp.inf <- summary(gbm.light)
tmp.inf[order(tmp.inf$var),]

best.iter <- gbm.perf(gbm.light, method="cv")
print(best.iter)

X$LightPred <- predict(gbm.light, newdata=X, n.trees=best.iter)
X.cru32 <- filter(X, Model=="CRU32")

#win.graph()
plot(lightning.current$Year, lightning.current$Positive.Strokes, pch="", cex.lab=1.3, cex.axis=1.3,
     main="Positive Lightning Strikes", xlab="Year", ylab="",
    las=1, xlim=range(X$Year), ylim=c(7000,43000))
models <- unique(X$Model[X$Scenario!="historical"]) # GCM predictions
for(k in 1:5){
    dsub <- filter(X, Model==models[k] & Scenario %in% c("historical", "rcp60"))
    lines(dsub$Year, dsub$LightPred, col="gray", cex=1.7, lwd=2.5, lty=2)
}
lines(d.ba.pct.sw$Year, 200*sqrt(d.ba.pct.sw$total) + 9000, col=2, cex=1.7, lwd=2) # scaled observed fire
lines(lightning.current$Year, lightning.current$Positive.Strokes, col=3, lwd=2) # observed strikes
points(lightning.current$Year, lightning.current$Positive.Strokes, pch=21, bg=3, cex=1.7, col=1)
points(X.cru32$Year, X.cru32$LightPred, pch="*", col=1, cex=2, lty=2) # CRU 3.2 predictions
lines(X.cru32$Year, X.cru32$LightPred, col=1, cex=1.7, lwd=2.5, lty=2)
legend("bottomright", c("Obs.", "CRU", "GCM", "Fire"), col=c("green", "black", "gray", "red"), lwd=2, lty=c(1,2,2,1), horiz=T, bty="n")

# @knitr notrun1
#win.graph()
#gbm.light$sa <- plot.pd.gbm(object=gbm.light, nVar=4)
#sa.pd.plot(gbm.light,saData=X,nVar=4)

X %>% select(Scenario, Model, Year, LightPred) %>% group_by(Scenario, Model) -> lightning.preds

lightning.preds %>% summarise(
    Mean=mean(LightPred), SD=sd(LightPred), Min=min(LightPred), Pct25=quantile(LightPred, 0.25),
    Median=median(LightPred), Pct75=quantile(LightPred, 0.75), Max=max(LightPred)) %>%
    data.frame()

save(lightning.obs, lightning.preds, file="../gbm_lightning_preds_1950_2099.RData")
