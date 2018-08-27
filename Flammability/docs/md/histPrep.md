


##
##
## histPrep.R

`histPrep.R` stores the function `histPrep`, which prepares certain histogram-related data objects in the global **R** environment for reuse by the function, `fireSizePlot`.

This function is called in `AlfrescoCalibration.R`.

## R code


```r
histPrep <- function(x, a = NULL) {
    h <- hist(x, plot = F)
    d <- diff(h$breaks)[1]
    r <- range(c(h$breaks, a)) + c(-d, d)
    assign("s", seq(r[1], r[2], d), pos = 1)
    assign("h1", hist(x, breaks = s, plot = F), pos = 1)
    assign("x0s", c(a, mean(x, na.rm = T), quantile(x, c(0.025, 0.975), na.rm = T)), 
        pos = 1)
    assign("ymx", max(h1$counts), pos = 1)
}
```
