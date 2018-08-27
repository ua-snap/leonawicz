


##
##
## gbm_modeling_lightning.R

The `gbm_modeling_lightning.R` script models backcast and forecast lightning strike frequency over Alaska from 1950 projected through 2099.
A lightning model is fitted using stochastic gradient boosting (GBM: generalized boosted regression models, or gradient boosting machine).
The script assembles data for modeling, performs the modeling, and saves results to a workspace file to be read and utilized by other scripts, e.g., `gbm_lightning_coefficients.R`.

The observed period is small (n = 9 years), due to limitations in comparable observations resulting from changes in the lightning detection network prior to 2003 and after 2011.
The sample size for the strike data over this time period is large. Some details can be found below.

## Lightning as a function of climate

*    Lightning: Response variable
*    All positive-polarized lightning strikes, sample size n = 172,633 strikes.
*    Annual strike frequency over the domain during the observational period ranges from 7,878 to 32,129 strikes per year.
*    Annual average is 19,181 strikes.
*    Estimated average annual strike frequency is 14,043 strikes during the 1950s and 33,903 strikes during the 2090s.

##### Climate: Predictor variables, monthly summer temperature and precipitation.

##### Spatial domain: statewide Alaska forested areas (spruce and deciduous).

##### Temporal domain: Summer (June + July + August total strike frequency), 2003 - 2011.

##### Notes:

*    Changes to the lightning detection network prior to 2003 and after 2011 limit the series of comparable samples.

*    A GBM model estimates lightning strike frequency over the domain as a function of climate.
*    Use of climate provides the opportunity to model annual lightning strike frequency backcasts and future projections over the domain using CRU 3.2 and CMIP5 GCM climate outputs.
*    Given the lightning sample size, much information is aggregated into each total statewide summer lightning frequency.
*    The model will likely be improved upon with additional data, climate scenarios, assessment of GBM model uncertainty (currently only GCM uncertainty is included), as well as deeper explorations of the nonlinear climate-lightning linkage itself.

##
##
## R code

### Setup


```r
library(data.table)
library(reshape2)
library(dplyr)
library(gbm)

# climate data
setwd("C:/github/Flammability/workspaces/tpByVeg")
load("tpByVeg_means_CRU32_individual.RData")
X <- d
load("tpByVeg_means_CMIP5_individual.RData")
X <- rbind(X, d)
rm(d)

X <- X %>% filter(Vegetation == "forest") %>% mutate(Month = factor(Month, levels = month.abb), 
    Var = substr(Var, 1, 1)) %>% group_by(Scenario, Model, Year, Month, Var) %>% 
    summarise(Val = mean(Val)) %>% dcast(Scenario + Model + Year ~ Month + Var, 
    value.var = "Val") %>% data.table()
setnames(X, gsub("_", "", names(X)))

# fire and lightning data
load("../gbmFlammability/baByVeg_historical_1950_2013.RData")  # burn area (km^2) by veg class, contains d.ba.pct.noatak and d.ba.pct.sw
light.yrs <- 2003:2011
lightning.current <- filter(X, Model == "CRU32" & Year %in% light.yrs)
lightning.current$Positive.Strokes <- lightning.obs <- c(9792, 37067, 28793, 
    11632, 32129, 15601, 14318, 15423, 7878)  # 2003-2011 AK SW forest positive strikes
lightning.current$ba <- with(d.ba.pct.sw, total[Year %in% light.yrs])
```

##
##
### GBM


```r
# gbm modeling
gbm.light <- gbm(Positive.Strokes ~ JunP + JulP + AugP + JunT + JulT + AugT, 
    distribution = "gaussian", bag.fraction = 0.9, cv.folds = 5, weights = sqrt(lightning.current$Positive.Strokes), 
    data = lightning.current, verbose = F, interaction.depth = 2, n.minobsinnode = 2, 
    n.trees = 500, shrinkage = 0.025)

# plots win.graph(height=6, width=8)
tmp.inf <- summary(gbm.light)
```

![](gbm_modeling_lightning_files/figure-html/gbm-1.png) 

```r
tmp.inf[order(tmp.inf$var), ]
```

```
##       var    rel.inf
## AugP AugP  7.6496813
## AugT AugT 50.5180437
## JulP JulP  0.7154789
## JulT JulT  3.2653273
## JunP JunP  3.9475620
## JunT JunT 33.9039069
```

```r
best.iter <- gbm.perf(gbm.light, method = "cv")
```

![](gbm_modeling_lightning_files/figure-html/gbm-2.png) 

```r
print(best.iter)
```

```
## [1] 499
```

```r
X$LightPred <- predict(gbm.light, newdata = X, n.trees = best.iter)
X.cru32 <- filter(X, Model == "CRU32")

# win.graph()
plot(lightning.current$Year, lightning.current$Positive.Strokes, pch = "", cex.lab = 1.3, 
    cex.axis = 1.3, main = "Positive Lightning Strikes", xlab = "Year", ylab = "", 
    las = 1, xlim = range(X$Year), ylim = c(7000, 43000))
models <- unique(X$Model[X$Scenario != "historical"])  # GCM predictions
for (k in 1:5) {
    dsub <- filter(X, Model == models[k] & Scenario %in% c("historical", "rcp60"))
    lines(dsub$Year, dsub$LightPred, col = "gray", cex = 1.7, lwd = 2.5, lty = 2)
}
lines(d.ba.pct.sw$Year, 200 * sqrt(d.ba.pct.sw$total) + 9000, col = 2, cex = 1.7, 
    lwd = 2)  # scaled observed fire
lines(lightning.current$Year, lightning.current$Positive.Strokes, col = 3, lwd = 2)  # observed strikes
points(lightning.current$Year, lightning.current$Positive.Strokes, pch = 21, 
    bg = 3, cex = 1.7, col = 1)
points(X.cru32$Year, X.cru32$LightPred, pch = "*", col = 1, cex = 2, lty = 2)  # CRU 3.2 predictions
lines(X.cru32$Year, X.cru32$LightPred, col = 1, cex = 1.7, lwd = 2.5, lty = 2)
legend("bottomright", c("Obs.", "CRU", "GCM", "Fire"), col = c("green", "black", 
    "gray", "red"), lwd = 2, lty = c(1, 2, 2, 1), horiz = T, bty = "n")
```

![](gbm_modeling_lightning_files/figure-html/gbm-3.png) 
