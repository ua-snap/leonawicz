


##
##
## fsMLE.R

Maximum likelihood estimation of fire size distributions functions are shown below.





### Functions

#### Perform MLE


```r
do_mle_fes <- function(d, parvec = c(meanlog = 1, sdlog = 1), dec = NULL, by.decade = FALSE) {
    n2loglik <- function(fun, x, params) -2 * sum(log(do.call(fun, list(x = x, 
        params))))
    l <- split(d$FES, d$Replicate)
    pars <- list(lapply(l, function(i) optim(fn = n2loglik, fun = dlnorm, x = i, 
        par = parvec)$par))
    if (by.decade) {
        for (k in 1:length(dec)) {
            d.sub <- subset(d, Decade == dec[k])
            l <- split(d.sub$FES, d.sub$Replicate)
            pars[[k + 1]] <- lapply(l, function(i) optim(fn = n2loglik, fun = dlnorm, 
                x = i, par = parvec)$par)
        }
    }
    pars
}
```

#### Plot data with ML-estimated log pdf overlay


```r
plot_mle_fes <- function(d, pars.list, dec = NULL, period = "1950 - 2009", facet.by = "Source") {
    if (facet.by != "Decade" & length(pars.list) > 1) 
        pars.list <- pars.list[1]
    if (facet.by == "Decade" & is.null(dec)) 
        stop("Must provide dec if faceting by dacades.")
    if (facet.by == "Decade" & length(pars.list) == length(dec) + 1) 
        pars.list <- pars.list[-1]
    if (!is.null(dec)) 
        period <- dec
    
    g <- ggplot(data = d, aes(x = logFES, fill = Source)) + theme_bw(base_size = 14) + 
        theme(legend.position = "bottom", legend.box = "horizontal") + scale_fill_manual(values = cbpal) + 
        scale_colour_manual(values = cbpal) + labs(title = paste(period, "MLE of observed and modeled fire event size distributions"), 
        x = expression("FES log(" ~ km^2 ~ ")"))
    g <- g + geom_histogram(aes(y = ..density..)) + geom_histogram(aes(y = ..density..), 
        colour = "white", show_guide = FALSE) + facet_wrap(as.formula(paste("~", 
        facet.by)), ncol = 2)
    
    src <- c("Observed", rep("Modeled", 32))
    alpha <- c(1, rep(0.2, 32))
    sq <- seq(min(d$logFES), max(d$logFES), length = 1000)
    d2.list <- list()
    for (k in 1:length(pars.list)) {
        pars <- pars.list[[k]]
        mle_text <- c(paste("MLE_obs: meanlog =", round(pars[[1]][1], 3), "sdlog =", 
            round(pars[[1]][2], 3)), paste("MLE_alf: meanlog =", round(mean(sapply(pars[-1], 
            "[[", 1)), 3), "sdlog =", round(mean(sapply(pars[-1], "[[", 2)), 
            3)))
        mle_text <- rep(mle_text, times = c(1, 32))
        d2.list[[k]] <- do.call(rbind, lapply(1:length(pars), function(i, sq, 
            pars, src, alpha, mle_text, dec = NULL) {
            d <- data.frame(Replicate = names(pars[i]), Source = src[i], x = sq, 
                logFES_MLE = dnorm(sq, mean = pars[[i]][1], sd = pars[[i]][2]), 
                alpha = alpha[i], mle_text = mle_text[i])
            if (!is.null(dec)) 
                d$Decade <- dec
            d
        }, sq = sq, pars = pars, src = src, alpha = alpha, mle_text = mle_text, 
            dec = dec[k]))
    }
    d2 <- do.call(rbind, d2.list)
    g <- g + geom_line(data = d2, aes(x = x, y = logFES_MLE, fill = NULL, group = Replicate, 
        alpha = alpha), colour = "black", size = 1) + scale_alpha_continuous(name = "MLE", 
        breaks = 1, labels = "")
    g <- g + geom_text(data = d2, x = Inf, y = Inf, aes(label = mle_text), hjust = 1.05, 
        vjust = 2, size = 4)
    g
}
```

### Log censored data


```r
set.seed(47)
d$FES <- d$FSE + runif(nrow(d), -0.95, 0.95)
d$logFES <- log(d$FES)
```
