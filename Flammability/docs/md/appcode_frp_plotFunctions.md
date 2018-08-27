


##
##
## plotFunctions.R

### plotFRPbyBuffer


```r
# plot FRP ~ buffer radius, grouped/colored by modeled vs. observed
plotFRPbyBuffer <- function(data, min.buffer, colpal, subject, grp = "", fontsize = 16, 
    leg.pos = "top", maintitle = "", xlb = "", ylb = "", facet.by = NULL, facet.cols = 1) {
    d <- subset(data, Buffer_km >= as.numeric(min.buffer))
    g <- ggplot(d, aes_string(x = "Buffer_km", y = "FRP", group = subject, colour = "Source")) + 
        scale_color_manual(values = colpal) + scale_fill_manual(values = colpal)
    if (grp != "") {
        g <- g + geom_line(data = subset(d, Source == "Modeled"), colour = "gray")
        g <- g + geom_line(aes_string(group = grp, colour = grp), data = subset(d, 
            Source == "Observed"), size = 1) + scale_color_manual(values = colpal[-c(1, 
            2)]) + scale_fill_manual(values = colpal[-c(1, 2)])
    } else g <- g + geom_line() + geom_line(data = subset(d, Source == "Observed"), 
        size = 1)
    g <- g + theme_bw(base_size = fontsize) + theme(legend.position = tolower(lgd.pos)) + 
        ggtitle(bquote(paste(.(maintitle) >= .(paste(min.buffer, "km"))))) + 
        xlab(xlb) + ylab(ylb)
    if (!is.null(facet.by)) 
        g <- g + facet_wrap(as.formula(paste("~", facet.by)), ncol = as.numeric(facet.cols))
    print(g)
}
```

### plotRABbyTime


```r
# plot time series of cumulative or non-cumulative annual relative area
# burned for a given buffer size
plotRABbyTime <- function(data, buffersize, year.range, cumulative = F, subject, 
    grp = "", colpal, fontsize = 16, lgd.pos = "top", facet.by = NULL, facet.cols = 1, 
    ...) {
    d <- data.table(data)
    d <- subset(d, Buffer_km %in% as.numeric(buffersize) & Year >= year.range[1] & 
        Year <= year.range[2])
    xlb = "Year"
    if (cumulative) {
        d <- d %>% group_by(Replicate, Buffer_km, Location) %>% mutate(Value = cumsum(Value))
        maintitle <- paste(year.range[1], "-", year.range[2], "Cumulative Relative Area Burned ~ Time | Buffer")
        ylb <- "CRAB (%)"
    } else {
        maintitle <- paste(year.range[1], "-", year.range[2], "Relative Area Burned ~ Time | Buffer")
        ylb <- "RAB (%)"
    }
    g <- ggplot(d, aes_string(x = "Year", y = "Value", group = subject, colour = "Source")) + 
        scale_color_manual(values = colpal) + scale_fill_manual(values = colpal)
    if (cumulative) {
        if (grp != "") {
            g <- g + geom_step(data = subset(d, Source == "Modeled"), colour = "gray")
            g <- g + geom_step(aes_string(group = grp, colour = grp), data = subset(d, 
                Source == "Observed"), size = 1) + scale_color_manual(values = colpal[-c(1, 
                2)]) + scale_fill_manual(values = colpal[-c(1, 2)])
        } else g <- g + geom_step() + geom_step(data = subset(d, Source == "Observed"), 
            size = 1)
    } else {
        if (grp != "") {
            g <- g + geom_point(data = subset(d, Source == "Modeled"), colour = "gray")
            g <- g + geom_point(aes_string(group = grp, colour = grp), data = subset(d, 
                Source == "Observed"), size = 2.5) + scale_color_manual(values = colpal[-c(1, 
                2)]) + scale_fill_manual(values = colpal[-c(1, 2)])
        } else g <- g + geom_point() + geom_point(data = subset(d, Source == "Observed"), 
            size = 2.5)
    }
    g <- g + theme_bw(base_size = fontsize) + theme(legend.position = tolower(lgd.pos)) + 
        ggtitle(bquote(paste(.(maintitle) == .(paste(buffersize, "km"))))) + 
        xlab(xlb) + ylab(ylb)
    if (!is.null(facet.by)) 
        g <- g + facet_wrap(as.formula(paste("~", facet.by)), ncol = as.numeric(facet.cols))
    print(g)
}
```

### plotFRIboxplot


```r
# plot boxplots of fire return intervals for given buffer sizes, locations,
# replicates, source data
plotFRIboxplot <- function(d, x, y, grp = NULL, Log = FALSE, colpal, show.points = TRUE, 
    pts.alpha = 1, fontsize = 16, leg.pos = "top", facet.by = NULL, facet.cols = 1, 
    lgd.pos = "top") {
    d$Buffer_km <- factor(d$Buffer_km)
    if (Log) 
        d$FRI <- log(d$FRI + 1)
    dodge <- position_dodge(width = 0.9)
    if (is.null(pts.alpha)) 
        pts.alpha <- 1
    if (!length(grp) || grp == "") 
        grp <- 1
    if (is.character(grp)) {
        if (grp != "Source") 
            colpal <- colpal[-c(1:2)]  # Only used black and gray when coloring observed vs. modeled
        n.grp <- length(unique(d[, grp]))
    } else n.grp <- 1
    x.n <- length(unique(d[, x]))
    if (is.character(grp) & n.grp > 1) {
        if (is.null(facet.by)) {
            x.names <- sort(unique(as.character(d[, x])))
            x.num <- grp.n <- grp.num <- rep(NA, nrow(d))
            for (m in 1:length(x.names)) {
                ind <- which(as.character(d[, x]) == x.names[m])
                grp.n[ind] <- length(unique(d[ind, grp]))
                x.num[ind] <- m
                grp.num[ind] <- 0.9 * ((as.numeric(factor(d[ind, grp]))/grp.n[ind]) - 
                  (1/grp.n[ind] + ((grp.n[ind] - 1)/2)/(grp.n[ind])))
            }
            d$xdodge <- x.num + grp.num
        } else {
            x.names <- sort(unique(as.character(d[, x])))
            panel.names <- unique(as.character(d[, facet.by]))
            n.panels <- length(panel.names)
            x.num <- grp.n <- grp.num <- rep(NA, nrow(d))
            for (m in 1:n.panels) {
                for (mm in 1:length(x.names)) {
                  ind <- which(as.character(d[, facet.by]) == panel.names[m] & 
                    as.character(d[, x]) == x.names[mm])
                  grp.n[ind] <- length(unique(d[ind, grp]))
                  x.num[ind] <- mm - 1 + as.numeric(factor(d[ind, x]))
                  grp.num[ind] <- 0.9 * ((as.numeric(factor(d[ind, grp]))/grp.n[ind]) - 
                    (1/grp.n[ind] + ((grp.n[ind] - 1)/2)/(grp.n[ind])))
                }
            }
            d$xdodge <- x.num + grp.num
        }
        xdodge <- "xdodge"
    }
    
    # d <- subset(d, Buffer_km %in% as.numeric(buffersize) & Year >=
    # year.range[1] & Year <= year.range[2])
    xlb = x
    # maintitle <- paste(year.range[1], '-', year.range[2], 'Fire Return
    # Interval Distributions')
    maintitle <- "Fire Return Interval Distributions"
    ylb <- "Fire Return Interval (Years)"
    if (grp == 1) 
        basic.fill.clr <- NULL else basic.fill.clr <- grp
    g <- ggplot(d, aes_string(x = x, y = y, colour = basic.fill.clr)) + scale_color_manual(values = colpal) + 
        scale_fill_manual(values = colpal)
    g <- g + geom_boxplot(fill = "white", outlier.colour = NA, position = dodge)
    if (show.points) {
        if (is.character(grp) & n.grp > 1) {
            g <- g + geom_point(aes_string(x = xdodge, fill = basic.fill.clr), 
                pch = 21, size = 1, colour = "black", alpha = pts.alpha, position = position_jitter(width = 0.9/(x.n * 
                  grp.n)))
        } else {
            g <- g + geom_point(aes_string(fill = basic.fill.clr), pch = 21, 
                size = 1, colour = "black", fill = "red", alpha = pts.alpha, 
                position = position_jitter(width = 0.9/x.n))
        }
    }
    g <- g + theme_bw(base_size = fontsize) + theme(legend.position = lgd.pos) + 
        # ggtitle(bquote(paste(.(maintitle) == .(paste(buffersize,'km'))))) +
    xlab(xlb) + ylab(ylb)
    if (!is.null(facet.by)) 
        g <- g + facet_wrap(as.formula(paste("~", facet.by)), ncol = as.numeric(facet.cols))
    print(g)
}
```
