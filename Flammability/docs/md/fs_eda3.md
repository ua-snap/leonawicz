


##
##
## fsMLE.R EDA part 3
### Statewide forest fire size log-normality

Evaluate the degree of log-normality of forest fire size distributions using observed data and modeled outputs for the statewide region.
The forest class is an aggregation of black spruce, white spruce, and deciduous tree species.





#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
# Statewide forest observed and simulation replicate 1
d.sf <- subset(d, Domain == "Statewide" & Vegetation == "Forest" & Replicate %in% 
    c("Observed", "Rep 0"), select = c(2, 5, 7))
p03a <- check_lnorm(d.sf, border = border, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
p03a()
```

![](fs_eda3_files/figure-html/lnorm_sw_forest_all-1.png) 

#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
check_lnorm_dec("p03", d.sf, dec, border = border, col = "gray40", cex.lab = 1.3, 
    cex.axis = 1.3)
```

![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-1.png) ![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-2.png) ![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-3.png) ![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-4.png) ![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-5.png) ![](fs_eda3_files/figure-html/lnorm_sw_forest_decades-6.png) 
