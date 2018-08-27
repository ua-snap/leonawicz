# @knitr CABvsTimePlot
CABvsTimePlot <- function(years, baseline.year, d.obs.fs, period){
	if(period=="historical"){
        abByYear <- tapply(d.obs.fs$FS, d.obs.fs$Year, sum)
        indices <- match(names(abByYear), years)
        cab.emp.all <- rep(0, length(years))
        cab.emp.all[indices] <- abByYear
        cab.emp.all <- cumsum(cab.emp.all)
    } else cab.emp.all <- NULL
	cab.alf.all <- apply(alf.fs[years - baseline.year + 1,], 2, cumsum)
	ylm <- range(c(cab.emp.all, c(cab.alf.all) ))
	png(file.path(outDir, paste0("CAB_", years[1], "to", years[length(years)], ".png")), res=120, width=1000, height=800)
	par(mar=c(5,5,4,2)+0.1, mfrow=c(1,1))
	plot(0, type="n", xlim=range(years), ylim=ylm, ylab=expression(paste(plain("Cumalitive Area Burn   "), ("km"^2), sep="")), xlab='Year',
		main=paste("CAB ", years[1],"-", years[length(years)], sep=""), cex.axis=1.1, cex.lab=1.2)
	for (j in 1:ncol(cab.alf.all)) lines(years, cab.alf.all[,j], lwd=1, lty=2, col="darkgray")
	if(period=="historical"){
        lines(years, cab.emp.all, lwd=4, col=1)
        legend("topleft", c("Historical", "ALFRESCO Rep"), col=c(1,"darkgray"), lty=c(1,2), bty="n", cex=1.2, lwd=c(4,1))
    } else legend("topleft", c("ALFRESCO Rep"), col="darkgray", lty=2, bty="n", cex=1.2, lwd=1)
	dev.off()
}
