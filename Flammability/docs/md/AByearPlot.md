


##
##
## AByearPlot.R

`AByearPlot.R` stores the function `AByearPlot`, which generates a basic annual box plot time series of total burn area over the burnable ALFRESCO domain among simulation replicates.
It includes an overlay of points representing historical observations.

This plot is called in `AlfrescoCalibration.R` and currently is hardcoded to plot a 1950 - 2011 time series.

### R code


```r
AByearPlot <- function(ALF.FS, d.obs.fs, years, domain, domain.name, baseline, 
    period) {
    if (period == "historical") {
        abByYear.tmp <- tapply(d.obs.fs$FS, d.obs.fs$Year, sum)
        abByYear <- rep(0, length(years))
        abByYear[match(names(abByYear.tmp), years)] <- abByYear.tmp
        domain <- abByYear
    } else domain <- NULL
    ylm <- range(c(domain, c(ALF.FS[years - baseline + 1, ])))
    png(file.path(outDir, paste0("ABYear_", domain.name, "_", years[1], "to", 
        years[length(years)], ".png")), res = 75, width = round(1600 * (length(years)/length(1950:2013))), 
        height = 800)
    par(mar = c(5, 5, 4, 2) + 0.1, mfrow = c(1, 1))
    plot(0, type = "n", xlim = range(years), ylim = ylm, ylab = expression(paste0(plain("Area Burn   "), 
        ("km"^2))), xlab = "Year", main = paste(domain.name, " AB/Yr", sep = ""), 
        cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    boxplot(as.data.frame(t(ALF.FS[years - baseline + 1, ])), add = TRUE, at = years, 
        col = "gray", names = FALSE, axes = F)
    axis(1, years, labels = F)
    if (period == "historical") {
        points(years, domain, pch = 21, bg = "#FF0000", cex = 1.3)
        segments(x0 = years, y0 = domain, y1 = apply(ALF.FS[years - baseline + 
            1, ], 1, median), col = "#FF000085")
    }
    d <- diff(par()$yaxp)[1]/70
    p <- 0.95 * ylm[2]
    legend(years[4] - 0.5, p, yjust = 0.5, "ALFRESCO", pch = NA, col = NA, bty = "n", 
        cex = 1.3)
    if (period == "historical") 
        legend(years[10], p, yjust = 0.5, "Historical", pch = 21, pt.bg = "#FF0000", 
            bty = "n", cex = 1.3)
    rect(years[2], p - d, years[3], p + d, col = "gray")
    rect(years[2] + 0.45, p - d, years[3] - 0.45, p + d, col = 1)
    segments(x0 = years[c(1, 3)], x1 = years[c(2, 4)], y0 = par()$yaxp[2], lty = 2)
    segments(x0 = years[c(1, 4)], y0 = p - d/2, y1 = p + d/2)
    points(years[1] - 0.5, p)
    dev.off()
}
```
