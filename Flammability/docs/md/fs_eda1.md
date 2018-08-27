


##
##
## fsMLE.R EDA part 1
### Fire samples

The `fsMLE.R` script performs exploratory data analysis followed by maximum likelihood estimation of fire size distributions based on historical observations and ALFRESCO simulation outputs.
Currently under development.
Preliminary EDA plots available.

## R code and results

### Setup


```r
setwd("C:/github/Flammability/workspaces")
load("fseByVeg_df_Noatak.RData")
d.fse.veg$Domain <- "Noatak"
d <- d.fse.veg
load("fseByVeg_df_Statewide.RData")
d.fse.veg$Domain <- "Statewide"
d <- rbind(d, d.fse.veg)
rm(d.fse.veg)

d <- transform(d, Decade = Year - Year%%10)
d$Decade <- paste0(d$Decade, "s")
d <- subset(d, Year < 2010)
# veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
dec <- sort(unique(d$Decade))
n.dec <- length(dec)
doms <- c("Noatak", "Statewide")
d <- transform(d, Replicate = factor(Replicate, levels = unique(Replicate)), 
    Source = factor(Source, levels = c("Observed", "Modeled")))

library(dplyr)
library(ggplot2)
dir.create(plotDir <- "C:/github/Flammability/plots/fseMLE", showWarnings = FALSE)
cbpal <- c("gray40", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
    "#D55E00", "#CC79A7")
border <- TRUE  # Set TRUE for knitted documents

# Plot setup
g <- ggplot(data = d, aes(x = Replicate, fill = Source)) + theme_bw(base_size = 14) + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, 
        hjust = 1)) + scale_fill_manual(values = cbpal) + scale_colour_manual(values = cbpal) + 
    labs(x = NULL, y = "Fire count")
```

##
##
### Exploratory data analysis

#### Fire count by vegetation class: Noatak


```r
# Observed and modeled fire counts by vegetation class
(p01a <- g + geom_bar(data = subset(d, Domain == "Noatak")) + facet_wrap(~Vegetation, 
    ncol = 1, scales = "free_y") + labs(title = "Number of observed and modeled Noatak fire events 1950 - 2009"))
```

![](fs_eda1_files/figure-html/fc_noa_veg-1.png) 

#### Fire count by vegetation class: Statewide


```r
(p01b <- g + geom_bar(data = subset(d, Domain == "Statewide")) + facet_wrap(~Vegetation, 
    ncol = 1, scales = "free_y") + labs(title = "Number of observed and modeled statewide fire events 1950 - 2009"))
```

![](fs_eda1_files/figure-html/fc_sw_veg-1.png) 

#### Fire count by decade: Noatak shrub


```r
# Observed and modeled fire counts by decade given vegetation class
(p01c <- p01a + facet_wrap(~Decade, ncol = 1) + labs(title = "Number of observed and modeled Noatak shrub fire events"))
```

![](fs_eda1_files/figure-html/fc_noa_shrub_dec-1.png) 

#### Fire count by decade: Statewide forest


```r
(p01d <- p01b + facet_wrap(~Decade, ncol = 1) + labs(title = "Number of observed and modeled statewide forest fire events"))
```

![](fs_eda1_files/figure-html/fc_sw_forest_dec-1.png) 
