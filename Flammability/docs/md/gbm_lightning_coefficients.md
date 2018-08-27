


##
##
## gbm_lightning_coefficients.R

The `gbm_lightning_coefficients.R` script assembles GBM lightning strike point model historical fitted values and backcast and forecast predictions.
Historical and future values are loaded via workspace file.

The script also provides exploratory graphs of the assembled data.
It presents the discrete classification of annual lightning intensity with several visualizations
including time series, ranked and ordered values, and empirical CDF.

Plots are made of exclusively CRU 3.2-based fitted values and backcast predictions.
Similar multi-panel plots are then made comparing each of five CMIP5 GCMs' predicted values with CRU 3.2 as a comparative baseline.

The finalized compiled data table including discrete classified annual lightning intensity coefficients is saved to a new workspace to be sourced by other scripts.
For example, it is sourced by `FlammabilityMapMultipliers.R` for applying a scalar multiplier to GBM vegetation flammability maps used in ALFRESCO simulations.

## R code

### Setup


```r
setwd("C:/github/Flammability/workspaces")
dir.create(plotDir <- "../plots/lightning", showWarnings = FALSE)

load("gbm_lightning_preds_1950_2099.RData")

library(reshape2)
library(ggplot2)
library(data.table)
library(dplyr)
```

##
##
### Support functions


```r
get_classes1 <- function(x, y = qtiles) cut(x, breaks = c(0, y, 99999), labels = F)
get_coefficients <- function(x, b = bounds, use.ecdf = TRUE) {
    if (use.ecdf) 
        return(ecdf(x)(x))  # x represents lightning values
    b <- c(0.01, b, 1)  # x represents bin integers
    # sapply(x, function(y) switch(y, '1'=0.05, '2'=0.5, '3'=0.95))
    sapply(x, function(y, b) runif(1, b[y], b[y + 1]), b = b)
}
get_classes2 <- function(x) factor(x, labels = c("Low", "Medium", "High"))
```

##
##
### Quantiles and classification


```r
bounds <- c(0.2, 0.8)
preds.cru32 <- filter(lightning.preds, Model == "CRU32")$LightPred
qtiles <- quantile(preds.cru32, bounds)
bins.cru <- get_classes1(preds.cru32)
coef.cru <- get_coefficients(preds.cru32)
bins.cru <- get_classes2(bins.cru)
d <- data.table(Scenario = "historical", Model = "CRU32", Year = 1950:2013, 
    LightPred = preds.cru32, Rank = rank(preds.cru32), Class = bins.cru, Coef = coef.cru, 
    ECDF = ecdf(preds.cru32)(preds.cru32))
```

##
##
### CRU 3.2 plots


```r
title.prefix <- "1950-2013 CRU 3.2 GBM-predicted summer lightning strikes: "
(g1 <- ggplot(data = d, aes(x = Rank, y = LightPred, label = Year)) + geom_hline(yintercept = qtiles, 
    linetype = 2) + geom_point() + geom_text(aes(colour = Class), hjust = 0, 
    vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1, y = qtiles, 
    label = paste("qunatile =", bounds), size = 3, vjust = -0.5) + labs(x = "Predicted rank", 
    y = "Predicted number of strikes", title = paste0(title.prefix, "ranked and ordered")))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-1.png) 

```r
(g2 <- ggplot(data = d, aes(x = Year, y = LightPred, label = Rank)) + geom_line() + 
    geom_hline(yintercept = qtiles, linetype = 2) + geom_point() + geom_text(aes(colour = Class), 
    hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1950, 
    y = qtiles, label = paste("qunatile =", bounds), size = 3, vjust = -0.5) + 
    labs(x = "Year", y = "Predicted number of strikes", title = paste0(title.prefix, 
        "time series")))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-2.png) 

```r
(g3 <- ggplot(data = d, aes(x = LightPred, label = Year)) + geom_vline(xintercept = qtiles, 
    linetype = 2) + stat_ecdf() + stat_ecdf(geom = "point") + geom_text(aes(y = ECDF, 
    colour = Class), hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", 
    x = qtiles, y = 0, label = paste("qunatile =", bounds), size = 3, hjust = -0.1) + 
    labs(x = "Predicted number of strikes", y = "CDF", title = paste0(title.prefix, 
        "empirical CDF")))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-3.png) 

##
##
### RCP 6.0 CMIP5 GCM comparisons with CRU 3.2


```r
d2 <- lightning.preds %>% group_by(Scenario, Model) %>% filter(Model != "CRU32") %>% 
    mutate(Rank = rank(LightPred), Class = get_classes1(LightPred), Coef = get_coefficients(LightPred), 
        ECDF = ecdf(LightPred)(LightPred)) %>% mutate(Class = get_classes2(Class)) %>% 
    setcolorder(names(d))
d.all <- d %>% rbind(d2) %>% mutate(Model = factor(Model, levels = unique(Model)))
rcp.lab <- "RCP 6.0"
rcp <- tolower(gsub("[ .]", "", rcp.lab))
d.sub <- d2 %>% filter(Scenario %in% c("historical", rcp)) %>% mutate(Scenario = rcp) %>% 
    rbind(d) %>% mutate(Model = factor(Model, levels = c("CRU32", unique(Model)[1:5])))
```


```r
title.prefix <- paste("1950-2013 CRU 3.2 and 1950-2005/2006-2099 GCM", rcp.lab, 
    "\n")
(g4 <- ggplot(data = d.sub, aes(x = Rank, y = LightPred, label = Year)) + geom_hline(yintercept = qtiles, 
    linetype = 2) + geom_point() + geom_text(aes(colour = Class), hjust = 0, 
    vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1, y = qtiles, 
    label = paste("CRU qunatile =", bounds), size = 3, hjust = 0.25, vjust = -0.25) + 
    labs(x = "Predicted rank", y = "Predicted number of strikes", title = paste0(title.prefix, 
        "GBM-predicted summer lightning strikes: ranked and ordered")) + facet_wrap(~Model, 
    ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-1.png) 

```r
(g5 <- ggplot(data = d.sub, aes(x = Year, y = LightPred, label = Rank)) + geom_line() + 
    geom_hline(yintercept = qtiles, linetype = 2) + geom_point() + geom_text(aes(colour = Class), 
    hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1950, 
    y = qtiles, label = paste("CRU qunatile =", bounds), size = 3, hjust = 0.25, 
    vjust = -0.25) + labs(x = "Year", y = "Predicted number of strikes", title = paste0(title.prefix, 
    "GBM-predicted summer lightning strikes: time series")) + facet_wrap(~Model, 
    ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-2.png) 

```r
(g6 <- ggplot(data = filter(d.all, Scenario == "historical"), aes(x = LightPred, 
    label = Year)) + geom_vline(xintercept = qtiles, linetype = 2) + stat_ecdf() + 
    stat_ecdf(geom = "point") + geom_text(aes(y = ECDF, colour = Class), hjust = 0, 
    vjust = 0, size = 3, show_guide = F) + stat_ecdf(data = filter(d.all, Scenario == 
    "rcp60")) + stat_ecdf(data = filter(d.all, Scenario == "rcp60"), geom = "point") + 
    geom_text(data = filter(d.all, Scenario == "rcp60"), aes(y = ECDF, colour = Class), 
        hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", x = qtiles, 
    y = 0, label = paste("CRU qunatile =", bounds), size = 3, hjust = -0.1, 
    vjust = -0.125) + labs(x = "Predicted number of strikes", y = "CDF", title = paste0(title.prefix, 
    "GBM-predicted summer lightning strikes: empirical CDF")) + facet_wrap(~Model, 
    ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-3.png) 

##
##
### Observed and modeled lightning strikes as a function of climate

#### CRU 3.2- and RCP 6.0 CMIP5 GCM-based lightning trend backcast and projection

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



```r
cbpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
    "#D55E00", "#CC79A7")
acf(filter(lightning.preds, Model == "CRU32")$LightPred)  # uncorrelated at lag 1
```

![](gbm_lightning_coefficients_files/figure-html/setup_preds_uncertainty-1.png) 

```r
genSim <- function(i, data) {
    d <- data %>% filter(Model != "CRU32") %>% group_by(Year) %>% sample_n(1) %>% 
        select(Scenario, Year, LightPred)
    d[, `:=`(Sim, i)]
    d
}

d.new <- select(d, Scenario, Year, LightPred)
d.new[, `:=`(Sim, -1)]
d.emp <- data.table(Scenario = "Empirical", Year = 2003:2011, LightPred = lightning.obs, 
    Sim = 0)
set.seed(47)
d.sim <- rbindlist(lapply(1:100, genSim, data = d.sub))
d.sim.all <- rbind(d.emp, d.new, d.sim)
d.sim.all$Scenario = factor(d.sim.all$Scenario, levels = c("historical", rcp, 
    "Empirical"))
d.sim.cb <- d.sim.all %>% group_by(Scenario, Year) %>% summarise(Mean = mean(LightPred), 
    Lb = min(LightPred), Ub = max(LightPred))

library(scales)
library(grid)
cols <- c(`5-model uncertainty` = alpha("black", 0.2), `5-model mean estimated strikes` = "black", 
    `CRU 3.2 historical estimated strikes` = cbpal[6], `Observed strikes` = cbpal[7])
```


```r
(g7 <- ggplot(data = subset(d.sim.cb, Scenario == "rcp60"), aes(x = Year, y = Mean)) + 
    geom_line(aes(colour = "5-model mean estimated strikes"), size = 1) + geom_ribbon(aes(ymin = Lb, 
    ymax = Ub, fill = "5-model uncertainty")) + geom_line(data = subset(d.sim.cb, 
    Scenario == "historical"), aes(colour = "CRU 3.2 historical estimated strikes"), 
    size = 1) + geom_line(data = subset(d.sim.cb, Scenario == "Empirical"), 
    aes(colour = "Observed strikes"), size = 1, linetype = 2) + geom_point(data = subset(d.sim.cb, 
    Scenario == "Empirical"), aes(colour = "Observed strikes"), size = 3) + 
    geom_smooth(data = d.sim.all, aes(x = Year, y = LightPred), method = "lm", 
        formula = y ~ x, colour = 1, size = 1) + geom_smooth(data = d.sim.all, 
    aes(x = Year, y = LightPred), formula = y ~ x, colour = 1, size = 1) + labs(x = "Year", 
    y = "Number of strikes", title = "1950-2099 Observed and GBM-estimated summer lightning strikes") + 
    scale_colour_manual(name = "", values = cols) + scale_fill_manual(name = "", 
    values = cols) + theme_bw(base_size = 16) + theme(legend.position = "bottom", 
    legend.box = "horizontal") + guides(colour = guide_legend(reverse = T, order = 1), 
    fill = guide_legend(reverse = T, order = 2)) + scale_x_continuous(breaks = seq(1950, 
    2100, by = 10)))
```

![](gbm_lightning_coefficients_files/figure-html/plots_preds_uncertainty-1.png) 

*    Backcast 1950-2013 CRU 3.2-based lightning (blue) matches strongly in its overlap with empirical observations (orange) and is comparable in inter-annual variability to CMIP5-based estimates (gray) over the full 1950-2099 period.
*    An upward linear trend in lightning frequency cannot be inferred from the narrow nine-year observational window.
*    A linear trend is present during backcast years based on either CRU 3.2 or CMIP5 GCMs though occurring among relatively high inter-annual variability and uncertainty.
*    The nonlinear climate-lightning linkage is strong and the upward trend in lightning frequency based on GCMs (black) becomes increasingly prominent as climate is projected to change.
This is bounded by GCM-based uncertainty (gray).
*    The model estimates lightning to increase by 17% per decade on average.
*    The "flatlining" behavior of projected lightning in the latter decades of the century represent a restriction of the GBM model
against making large extrapolations of the climate-lightning linkage when projected climate is more extreme than the range of historical climate values used to fit the model.
To that degree the GBM model is conservative.
