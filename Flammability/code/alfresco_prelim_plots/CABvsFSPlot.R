# @knitr CABvsFSPlot
CABvsFSPlot <- function(years, d.obs.fs, period, max.years=length(1950:2013)){
    if(length(years) > max.years) years <- years[1:max.years]
	png(file.path(outDir,"CABvsFireSize.png"),res=120,width=1000,height=800)
	par(mar=c(5,5,3,2)+0.1)
	sort.tmp <- vector("list", length(fire.reps))
    akfire.trun1km <- sort(round(d.obs.fs$FS, 0))
    m <- max(akfire.trun1km)
    m1 <- sum(akfire.trun1km)
    for (i in 1:length(fire.reps)){
		ind <- alf.fse[,2]==fire.reps[i] & alf.fse[,1]>=years[1] & alf.fse[,1]<=years[length(years)]
		sort.tmp[[i]] <- sort(alf.fse[,3][ind])
	}
    m2 <- max(unlist(lapply(sort.tmp, sum)))
	xlm <- c(0, max(c(m, unlist(sort.tmp))))
    ylm <- c(0, 1.1*max(m1, m2))
	plot(0,0,type="n",xlim=xlm, ylim=ylm, cex.axis=1.2, cex.lab=1.2, main="Cumulative Burn vs. Fire Size",
        xlab=expression(paste(plain("Fire Size   "), ("km"^2), sep="")), ylab=expression(paste(plain("Cumalitive Area Burn   "), ("km"^2), sep="")))
	for(i in 1:length(sort.tmp)) lines(sort.tmp[[i]], cumsum(sort.tmp[[i]]), lty=2, lwd=1, col="darkgray")
    lines(sort(akfire.trun1km),cumsum(sort(akfire.trun1km)),col=1,lty=1,lwd=3)
	if(period=="historical"){
        legend("topleft", c(paste0("Empirical (", years[1], ":", years[length(years)], "); Truncated at 1km"), "ALFRESCO"), lty=c(1,2), lwd=c(4,1), cex=1.2, bty='n', col=c(1,"darkgray"))
    } else legend("topleft", c(paste0("Empirical (1950:2013); Truncated at 1km"), paste0("ALFRESCO (", years[1], ":", years[length(years)], ")")), lty=c(1,2), lwd=c(4,1), cex=1.2, bty='n', col=c(1,"darkgray"))
	dev.off()
}
