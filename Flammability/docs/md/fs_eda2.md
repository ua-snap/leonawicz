


##
##
## fsMLE.R EDA part 2
### Noatak shrub fire size log-normality

Evaluate the degree of log-normality of shrub fire size distributions using observed data and modeled outputs for the Noatak region.



#### Function for log-normality assessment


```r
# Functions to assess log-normality of fire size for observed data and a
# sample simulation replicate
check_lnorm <- function(d, nmax.ad.test = 100, verbose = FALSE, closure = TRUE, 
    period = "1950-2009", border = FALSE, ...) {
    f <- function() {
        require("nortest")
        dl <- split(d$FSE, as.character(d$Replicate))
        if (length(dl) == 2) 
            iters <- 1:2 else if (names(dl) == "Observed") 
            iters <- 1 else iters <- 2
        id <- paste(period, c("observations", "simulations"))[iters]
        if (length(iters) == 1) 
            iters <- 1
        layout(matrix(1:(3 * length(iters)), length(iters), byrow = T))
        if (border) 
            par(mar = c(5, 5, 4, 1))
        for (i in iters) {
            x <- dl[[i]]
            logx <- log(x + runif(length(x), -0.95, 0.95))  # Add uniform noise
            hist(x, main = paste("Histrogram of", id[i]), ...)
            hist(logx, main = paste0("Histrogram of log(", id[i], ")"), ...)
            qqnorm(logx, main = paste("Q-Q plot:", id[i]), ...)
            qqline(logx, main = paste("Q-Q plot:", id[i]), ...)
            if (verbose) {
                if (length(logx) > 7) 
                  print(ad.test(sample(logx, min(length(logx), nmax.ad.test)))) else print("Sample too small for Anderson-Darling normality test.")
            }
        }
        if (border) {
            par(xpd = NA)
            rect(grconvertX(0, from = "ndc"), grconvertY(0, from = "ndc"), grconvertX(1, 
                from = "ndc"), grconvertY(1, from = "ndc"))
            par(mar = c(5, 4, 4, 1) + 0.1, xpd = FALSE)
        }
    }
    if (closure) 
        return(f) else f()
}

check_lnorm_dec <- function(id, d, dec, i.offset = 1, border = FALSE, ...) {
    for (i in 1:length(dec)) {
        pid <- paste0(id, letters[i + i.offset])
        assign(pid, check_lnorm(subset(d, Decade == dec[i]), period = dec[i], 
            border = border, ...), pos = 1)
        get(pid)()
    }
}
```

#### Observed and sample replicate simulation fire sizes

The first replicate was selected.


```r
# Noatak shrub observed and simulation replicate 1
set.seed(8923)
d.sf <- subset(d, Domain == "Noatak" & Vegetation == "Shrub" & Replicate %in% 
    c("Observed", "Rep 0"), select = c(2, 5, 7))
p02a <- check_lnorm(d.sf, border = border, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
p02a()
```

![](fs_eda2_files/figure-html/lnorm_noa_shrub_all-1.png) 

#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
check_lnorm_dec("p02", d.sf, dec, border = border, col = "gray40", cex.lab = 1.3, 
    cex.axis = 1.3)
```

![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-1.png) ![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-2.png) ![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-3.png) ![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-4.png) ![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-5.png) ![](fs_eda2_files/figure-html/lnorm_noa_shrub_decades-6.png) 
