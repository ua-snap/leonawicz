


##
##
## fireSizePlot.R

`fireSizePlot.R` stores the function `fireSizePlot`, which plots the distribution of maximum fire size events over the burnable ALFRESCO conditional on a fixed period of time.
The plot includes lines representing the historically observed maximum fire size and mean and 95% confidence interval among simulation replicates.

This plot is called in `AlfrescoCalibration.R` and currently is hardcoded to plot curves based on 1950 - 2011.

## R code


```r
fireSizePlot <- function(years, d.obs.fs, period) {
    max.alf.fse <- c()
    for (i in fire.reps) max.alf.fse[i] <- max(alf.fse[, 3][alf.fse[, 2] == 
        i & alf.fse[, 1] > years[1] & alf.fse[, 1] <= years[length(years)]])
    if (period == "historical") 
        histPrep(max.alf.fse, max(d.obs.fs$FS)) else histPrep(max.alf.fse)
    png(file.path(outDir, "HIST_MaxFS.png"), res = 100, width = 1000, height = 800)
    par(mar = c(5, 4, 1, 1) + 0.1)
    plot(h1, xlim = range(s), ylim = c(0, 1.2 * ymx), xlab = "Max Fire Size", 
        main = paste("Maximum Fire Size (", years[1], ":", years[length(years)], 
            ")", sep = ""), col = "gray")
    if (period == "historical") {
        segments(x0 = x0s, y0 = 0, y1 = ymx, lwd = c(3, 3, 2, 2), lty = c(1, 
            1, 2, 2), col = c(2, 1, 1, 1))
        legend("top", yjust = 0.5, horiz = T, c("Empirical Max FS", "ALFRESCO Mean Max FS", 
            "95% CI"), bty = "n", lwd = c(3, 3, 2), lty = c(1, 1, 2), col = c(2, 
            1, 1))
        legend(mean(s), 1.2 * ymx, xjust = 0.5, "ALFRESCO Max FS", bty = "n", 
            fill = "gray")
    } else {
        segments(x0 = x0s, y0 = 0, y1 = ymx, lwd = c(3, 2, 2), lty = c(1, 2, 
            2), col = c(1, 1, 1))
        legend("top", yjust = 0.5, horiz = T, c("ALFRESCO Mean Max FS", "95% CI"), 
            bty = "n", lwd = c(3, 2), lty = c(1, 2), col = c(1, 1))
        legend(mean(s), 1.2 * ymx, xjust = 0.5, "ALFRESCO Max FS", bty = "n", 
            fill = "gray")
    }
    dev.off()
}
```
