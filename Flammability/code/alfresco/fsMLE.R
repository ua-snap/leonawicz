########################################################################
#### Maximum likelihood estimation of fire event size distributions ####
########################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   05/29/2015        ####

# @knitr setup
setwd("C:/github/Flammability/workspaces")
load("fseByVeg_df_Noatak.RData")
d.fse.veg$Domain <- "Noatak"
d <- d.fse.veg
load("fseByVeg_df_Statewide.RData")
d.fse.veg$Domain <- "Statewide"
d <- rbind(d, d.fse.veg)
rm(d.fse.veg)

d <- transform(d, Decade=Year - Year %% 10)
d$Decade <- paste0(d$Decade, "s")
d <- subset(d, Year < 2010)
#veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
dec <- sort(unique(d$Decade))
n.dec <- length(dec)
doms <- c("Noatak", "Statewide")
d <- transform(d, Replicate=factor(Replicate, levels=unique(Replicate)), Source=factor(Source, levels=c("Observed", "Modeled")))

library(dplyr)
library(ggplot2)
dir.create(plotDir <- "C:/github/Flammability/plots/fseMLE", showWarnings=FALSE)
cbpal <- c("gray40", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
border <- TRUE # Set TRUE for knitted documents

# Plot setup
g <- ggplot(data=d, aes(x=Replicate, fill=Source)) + theme_bw(base_size=14) + theme(legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1)) + 
	scale_fill_manual(values=cbpal) + scale_colour_manual(values=cbpal) + labs(x=NULL, y="Fire count")
	
# @knitr fc_noa_veg
# Observed and modeled fire counts by vegetation class
(p01a <- g + geom_bar(data=subset(d, Domain=="Noatak")) + facet_wrap(~ Vegetation, ncol=1, scales="free_y") + labs(title="Number of observed and modeled Noatak fire events 1950 - 2009"))

# @knitr fc_sw_veg
(p01b <- g + geom_bar(data=subset(d, Domain=="Statewide")) + facet_wrap(~ Vegetation, ncol=1, scales="free_y") + labs(title="Number of observed and modeled statewide fire events 1950 - 2009"))

# @knitr fc_noa_shrub_dec
# Observed and modeled fire counts by decade given vegetation class
(p01c <- p01a + facet_wrap(~ Decade, ncol=1) + labs(title="Number of observed and modeled Noatak shrub fire events"))

# @knitr fc_sw_forest_dec
(p01d <- p01b + facet_wrap(~ Decade, ncol=1) + labs(title="Number of observed and modeled statewide forest fire events"))

# @knitr func_check_lnorm
# Functions to assess log-normality of fire size for observed data and a sample simulation replicate
check_lnorm <- function(d, nmax.ad.test=100, verbose=FALSE, closure=TRUE, period="1950-2009", border=FALSE, ...){
	f <- function(){
		require("nortest")
		dl <- split(d$FSE, as.character(d$Replicate))
		if(length(dl)==2) iters <- 1:2 else if(names(dl)=="Observed") iters <- 1 else iters <- 2
		id <- paste(period, c("observations", "simulations"))[iters]
		if(length(iters)==1) iters <- 1
		layout(matrix(1:(3*length(iters)), length(iters), byrow=T))
		if(border) par(mar=c(5,5,4,1))
		for(i in iters){
			x <- dl[[i]]
			logx <- log(x + runif(length(x), -0.95, 0.95)) # Add uniform noise
			hist(x, main=paste("Histrogram of", id[i]), ...)
			hist(logx, main=paste0("Histrogram of log(", id[i], ")"), ...)
			qqnorm(logx, main=paste("Q-Q plot:", id[i]), ...)
			qqline(logx, main=paste("Q-Q plot:", id[i]), ...)
			if(verbose) { if(length(logx) > 7) print(ad.test(sample(logx, min(length(logx), nmax.ad.test)))) else print("Sample too small for Anderson-Darling normality test.") }
		}
		if(border){
			par(xpd=NA)
			rect(grconvertX(0, from='ndc'), grconvertY(0, from='ndc'), grconvertX(1, from='ndc'), grconvertY(1, from='ndc'))
			par(mar=c(5,4,4,1)+0.1, xpd=FALSE)
		}
	}
	if(closure) return(f) else f()
}

check_lnorm_dec <- function(id, d, dec, i.offset=1, border=FALSE, ...){
	for(i in 1:length(dec)){
		pid <- paste0(id, letters[i + i.offset])
		assign(pid, check_lnorm(subset(d, Decade==dec[i]), period=dec[i], border=border, ...), pos=1)
		get(pid)()
	}
}

# @knitr lnorm_noa_shrub_all
# Noatak shrub observed and simulation replicate 1
set.seed(8923)
d.sf <- subset(d, Domain=="Noatak" & Vegetation=="Shrub" & Replicate %in% c("Observed", "Rep 0"), select=c(2,5,7))
p02a <- check_lnorm(d.sf, border=border, col="gray40", cex.lab=1.3, cex.axis=1.3)
p02a()
# @knitr lnorm_noa_shrub_decades
check_lnorm_dec("p02", d.sf, dec, border=border, col="gray40", cex.lab=1.3, cex.axis=1.3)

# @knitr lnorm_sw_forest_all
# Statewide forest observed and simulation replicate 1
d.sf <- subset(d, Domain=="Statewide" & Vegetation=="Forest" & Replicate %in% c("Observed", "Rep 0"), select=c(2,5,7))
p03a <- check_lnorm(d.sf, border=border, col="gray40", cex.lab=1.3, cex.axis=1.3)
p03a()
# @knitr lnorm_sw_forest_decades
check_lnorm_dec("p03", d.sf, dec, border=border, col="gray40", cex.lab=1.3, cex.axis=1.3)

# @knitr func_eda_pngs
# This code is not run with kntir. It is for standalone session PNG generation.
dev.off()

savePNG <-  function(files, plots, ...){
	for(i in 1:length(files)){
		png(files[i], ...)
		p <- get(plots[i])
		if("ggplot" %in% class(p)) print(p) else if(class(p)=="function") p()
		dev.off()
	}
}

# @knitr eda_pngs_01
plots <- ls(pattern=paste0("^p01"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", rep(doms, 2), "_", c(rep("fcByVeg.png",2), "fcShrubByDec.png", "fcForestByDec.png"))
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr eda_pngs_02
plots <- ls(pattern=paste0("^p02"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", doms[1], "_shrub_fs", c("All", dec), "_lnormPlots.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr eda_pngs_03
plots <- ls(pattern=paste0("^p03"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", doms[2], "_forest_fs", c("All", dec), "_lnormPlots.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr mle_functions1
do_mle_fes <- function(d, parvec=c(meanlog=1, sdlog=1), dec=NULL, by.decade=FALSE){
	n2loglik <- function(fun, x, params) -2*sum(log(do.call(fun, list(x=x, params))))
	l <- split(d$FES, d$Replicate)
	pars <- list(lapply(l, function(i) optim(fn=n2loglik, fun=dlnorm, x=i, par=parvec)$par))
	if(by.decade){
		for(k in 1:length(dec)){
			d.sub <- subset(d, Decade==dec[k])
			l <- split(d.sub$FES, d.sub$Replicate)
			pars[[k+1]] <- lapply(l, function(i) optim(fn=n2loglik, fun=dlnorm, x=i, par=parvec)$par)
		}
	}
	pars
}

# @knitr mle_functions2
plot_mle_fes <- function(d, pars.list, dec=NULL, period="1950 - 2009", facet.by="Source"){
	if(facet.by!="Decade" & length(pars.list) > 1) pars.list <- pars.list[1]
	if(facet.by=="Decade" & is.null(dec)) stop("Must provide dec if faceting by dacades.")
	if(facet.by=="Decade" & length(pars.list)==length(dec)+1) pars.list <- pars.list[-1]
	if(!is.null(dec)) period <- dec
	
	g <- ggplot(data=d, aes(x=logFES, fill=Source)) + theme_bw(base_size=14) + theme(legend.position="bottom", legend.box="horizontal") +
		scale_fill_manual(values=cbpal) + scale_colour_manual(values=cbpal) +
		labs(title=paste(period, "MLE of observed and modeled fire event size distributions"), x=expression("FES log("~km^2~")"))
	g <- g + geom_histogram(aes(y=..density..)) + geom_histogram(aes(y=..density..), colour="white", show_guide=FALSE) + facet_wrap(as.formula(paste("~", facet.by)), ncol=2)
	
	src <- c("Observed", rep("Modeled", 32))
	alpha <- c(1, rep(0.2, 32))
	sq <- seq(min(d$logFES), max(d$logFES), length=1000)
	d2.list <- list()
	for(k in 1:length(pars.list)){
		pars <- pars.list[[k]]
		mle_text <- c(paste("MLE_obs: meanlog =", round(pars[[1]][1], 3), "sdlog =", round(pars[[1]][2], 3)), paste("MLE_alf: meanlog =", round(mean(sapply(pars[-1], "[[", 1)), 3), "sdlog =", round(mean(sapply(pars[-1], "[[", 2)), 3)))
		mle_text <- rep(mle_text, times=c(1, 32))
		d2.list[[k]] <- do.call(rbind, lapply(1:length(pars),
			function(i, sq, pars, src, alpha, mle_text, dec=NULL){
				d <- data.frame(Replicate=names(pars[i]), Source=src[i], x=sq, logFES_MLE=dnorm(sq, mean=pars[[i]][1], sd=pars[[i]][2]), alpha=alpha[i], mle_text=mle_text[i])
				if(!is.null(dec)) d$Decade <- dec
				d
			}, sq=sq, pars=pars, src=src, alpha=alpha, mle_text=mle_text, dec=dec[k]))
	}
	d2 <- do.call(rbind, d2.list)
	g <- g + geom_line(data=d2, aes(x=x, y=logFES_MLE, fill=NULL, group=Replicate, alpha=alpha), colour="black", size=1) + scale_alpha_continuous(name="MLE", breaks=1, labels="")
	g <- g + geom_text(data=d2, x=Inf, y=Inf, aes(label=mle_text), hjust=1.05, vjust=2, size=4)
	g
}

# @knitr mle_logdata
set.seed(47)
d$FES <- d$FSE + runif(nrow(d), -0.95, 0.95)
d$logFES <- log(d$FES)

# @knitr mle_noa_shrub_all
# Noatak shrub all years, all replicates
d.sub <- subset(d, Domain=="Noatak" & Vegetation=="Shrub")
d.sub %>% group_by(Replicate) %>% summarise(x_bar=mean(logFES), s=sd(logFES)) %>% mutate(Decade="All") %>% select(Decade, Replicate, x_bar, s) -> d.sub.stats1
d.sub %>% group_by(Decade, Replicate) %>% summarise(x_bar=mean(logFES), s=sd(logFES)) -> d.sub.stats2
d.sub.stats <- rbind(d.sub.stats1, d.sub.stats2)

pars <- do_mle_fes(d.sub, parvec=c(meanlog=1, sdlog=1), dec=dec, by.decade=TRUE)
pars.df <- lapply(1:length(pars),
	function(i, x, dec){
		x <- data.frame(do.call(rbind, args=x[[i]]))
		x$Replicate <- factor(rownames(x), levels=rownames(x))
		names(x)[1:2] <- c("mu_hat_mle", "sigma_hat_mle")
		x$Decade <- dec[i]
		rownames(x) <- NULL
		x[,c(4,3,1,2)]
	}, x=pars, dec=c("All", dec)
)
pars.df <- do.call(rbind, pars.df)

full_join(d.sub.stats, pars.df)

(p04a <- plot_mle_fes(d.sub, pars[1]))

# @knitr mle_noa_shrub_dec
for(i in 1:length(dec)) print(assign(paste0("p04", letters[i+1]), plot_mle_fes(subset(d.sub, Decade==dec[i]), pars[i+1])))

# @knitr mle_sw_forest_all
# statewide forest all years, all replicates
d.sub <- subset(d, Domain=="Statewide" & Vegetation=="Forest")
d.sub %>% group_by(Replicate) %>% summarise(x_bar=mean(logFES), s=sd(logFES)) %>% mutate(Decade="All") %>% select(Decade, Replicate, x_bar, s) -> d.sub.stats1
d.sub %>% group_by(Decade, Replicate) %>% summarise(x_bar=mean(logFES), s=sd(logFES)) -> d.sub.stats2
d.sub.stats <- rbind(d.sub.stats1, d.sub.stats2)

pars <- do_mle_fes(d.sub, parvec=c(sdlog=10, meanlog=10), dec=dec, by.decade=TRUE)
pars.df <- lapply(1:length(pars),
	function(i, x, dec){
		x <- data.frame(do.call(rbind, args=x[[i]]))
		x$Replicate <- factor(rownames(x), levels=rownames(x))
		names(x)[1:2] <- c("mu_hat_mle", "sigma_hat_mle")
		x$Decade <- dec[i]
		rownames(x) <- NULL
		x[,c(4,3,1,2)]
	}, x=pars, dec=c("All", dec)
)
pars.df <- do.call(rbind, pars.df)

full_join(d.sub.stats, pars.df)

(p05a <- plot_mle_fes(d.sub, pars[1]))

# @knitr mle_sw_forest_dec
for(i in 1:length(dec)) print(assign(paste0("p05", letters[i+1]), plot_mle_fes(subset(d.sub, Decade==dec[i]), pars[i+1])))

# @knitr mle_pngs_04
dev.off()
plots <- ls(pattern=paste0("^p04"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_Noatak_shrub_logfs", c("All", dec), "_MLEdist.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr mle_pngs_05
plots <- ls(pattern=paste0("^p05"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_Statewide_forest_logfs", c("All", dec), "_MLEdist.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)
