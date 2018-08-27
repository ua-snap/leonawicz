##################################################################################################################
#### This R script tables fire sizes (FS) by vegetation class and year for observed data and ALFRESCO outputs ####
##################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   12/11/2015        ####

# @knitr setup
comArgs <- commandArgs(TRUE)
if(length(comArgs>0)){
        arg.mat <- do.call("rbind",strsplit(comArgs,"="))
        options(warn=-1); arg.char <- which(is.na(as.numeric(arg.mat[,2]))); options(warn=0)
        if(length(arg.char>0)) arg.mat[arg.char,2] <- paste("'",arg.mat[arg.char,2],"'",sep="")
        eval(parse(text=apply(arg.mat,1,paste,collapse="=")))
}
cat(comArgs)

if(!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if(period=="historical") yr.start <- 1950 else yr.start <- baseline.year
if(exists("yr.end")) yrs <- yr.start:yr.end else stop("must provide 'baseline.year' and 'yr.end'")
yrs <- yr.start:yr.end
if(!exists("n.sims")) n.sims <- 32
n.cores <- min(n.sims, 32)

library(raster)
library(data.table)
library(parallel)
library(dplyr)

rasterOptions(tmpdir="/big_scratch/shiny", chunksize=10e10, maxmemory=10e11)
mainDir <- file.path(input, "Maps")

# @knitr func_fsByVeg
fsByVeg <- function(i, v, f, obs=NULL){
	v[v!=i] <- NA
	x <- f[!is.na(v) & !is.na(f)]
	d <- d2 <- NULL
    if(length(x)) d <- data.table(Vegetation=i, FS=sort(as.numeric(tapply(x, x, length))), Domain=factor("Full", levels=c("Full", "Masked")))
    if(!is.null(obs)){
        x <- f[!is.na(v) & !is.na(f) & !is.na(obs)]
        if(length(x)) d2 <- data.table(Vegetation=i, FS=sort(as.numeric(tapply(x, x, length))), Domain=factor("Masked", levels=c("Full", "Masked")))
        if(is.data.table(d2)) d <- rbindlist(list(d, d2))
    }
    d
}

# @knitr func_fsByRep
fsByRep <- function(d, mainDir, vid, v.veg, years, obs=NULL){
	reps <- paste0("_",d-1,"_")
	files <- list.files(mainDir, pattern=gsub("expression","",paste(bquote(expression("FireSc.*.",.(reps),".*.tif$")),collapse="")), recur=T, full=T)
	yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
	ord <- order(yrs)
	files <- files[ord]
	yrs <- yrs[ord]
	ind <- which(yrs %in% years)
	files <- files[ind]
	yrs <- yrs[ind]
	n <- length(yrs)
	dlist <- vector("list", n)
	for(k in 1:n){
		v.fid <- getValues(raster(files[k], band=2))
		if(!all(is.na(v.fid))){
			dl <- lapply(vid, fsByVeg, v=v.veg, f=v.fid, obs=obs)
			dlist[[k]] <- as.data.frame(rbindlist(dl))
			dlist[[k]]$Year <- yrs[k]
		}
	}
	d <- rbindlist(dlist)
    if(nrow(d) > 0){
        d[, Source:="Modeled"]
        d[, Replicate:=paste("Rep", gsub("_", "", reps))]
        setcolorder(d, names(d)[c(3,5,6,1,4,2)])
    } else d <- NULL
	d
}

# @knitr func_fsByRepEmp
fsByRepEmp <- function(i, b, vid, v.veg, yrs, obs=NULL){
	v.fid <- getValues(subset(b, i))
	if(all(is.na(v.fid))) return(NULL)
	dl <- lapply(vid, fsByVeg, v=v.veg, f=v.fid, obs=obs)
	d <- rbindlist(dl)
	d[, Year:=yrs[i]]
	d[, Source:="Observed"]
    d[, Replicate:="Observed"]
	setcolorder(d, names(d)[c(3,5,6,1,4,2)])
	d
}

# @knitr empirical_data_setup
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(mask(r, shp))
totalba <- length(v.veg[!is.na(v.veg) & v.veg > 0])
v.veg[v.veg==3 | v.veg==4] <- 2 # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")

if(!exists("result2")) stop("empirical fire scar total fires map layer not loaded.")
empba <- extract(result2, shp)[[1]]
empba <- length(empba[!is.na(empba) & empba > 0])
empba.totalba.ratio <- empba/totalba
print(empba)
print(totalba)
print(empba.totalba.ratio)
r.obs <- result2
r.obs[r.obs==0] <- NA
r.obs[r.obs>0] <- 1
v.obs <- getValues(r.obs)

# @knitr run
# Process empirical data
fs.emp <- mclapply(1:nlayers(b.fid), fsByRepEmp, b=b.fid, vid=vid, v.veg=v.veg, yrs=yrs.hist.all, obs=v.obs, mc.cores=n.cores)
fs.emp <- rbindlist(fs.emp)
# Process modeled data
fs.alf.list <- mclapply(1:n.sims, fsByRep, mainDir=mainDir, vid=vid, v.veg=v.veg, years=yrs, obs=v.obs, mc.cores=n.cores)
fs.alf <- rbindlist(fs.alf.list)
d.fs <- rbind(fs.emp, fs.alf)
d.fs[, Vegetation:=v.names[Vegetation]]
d.fs <- tidyr::complete(d.fs, Domain, tidyr::nesting(Source, Replicate), Vegetation, Year=tidyr::full_seq(Year, 1L), fill=list(FS=0))
dom <- if(substr(tolower(alf.domain),1,6)=="noatak") "Noatak" else if(substr(tolower(alf.domain),1,6)=="statew") "Statewide"
save(d.fs, file=paste0(out, "/fsByVeg_df_", dom, ".RData"))

# plot time series of regional cumulative or non-cumulative annual total area burned for a given vegetation class or collection of vegetation classes
cbpalette <- c("#000000", "gray", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(ggplot2)

plotRegionalTABbyTime <- function(data, year.range, cumulative=F, subject, grp="", colpal, fontsize=16, lgd.pos="top", facet.by=NULL, facet.cols=1, facet.scales=NULL, ...){
    d <- filter(data, Year >= year.range[1] & Year <= year.range[2]) %>% group_by(Domain, Source, Replicate, Vegetation, Year)
    given.veg <- "| Vegetation"
	xlb="Year"
	if(cumulative){
		d %>% summarise(Value=sum(FS)) %>% mutate(Value=cumsum(Value)) -> d
		maintitle <- paste(year.range[1], "-", year.range[2], "Regional Cumulative Total Area Burned ~ Time", given.veg)
		ylb <- expression("CTAB ("~km^2~")")
	} else {
        d <- summarise(d, Value=sum(FS))
		maintitle <- paste(year.range[1], "-", year.range[2], "Regional Total Area Burned ~ Time", given.veg)
		ylb <- expression("TAB ("~km^2~")")
	}
	g <- ggplot(d, aes_string(x="Year", y="Value", group=subject, colour="Source"))
	if(cumulative){
		if(grp!=""){
			g <- g + geom_step(data=filter(d, Source=="Modeled"), colour="gray")
			g <- g + geom_step(aes_string(colour=grp), data=filter(d, Source=="Observed"), size=1) +
				scale_color_manual(values=colpal[-c(1,2)]) + scale_fill_manual(values=colpal[-c(1,2)])
		} else g <- g + geom_step() + geom_step(data=filter(d, Source=="Observed"), size=1) + scale_color_manual(values=colpal) + scale_fill_manual(values=colpal)
	} else {
		if(grp!=""){
			g <- g + geom_point(data=filter(d, Source=="Modeled"), colour="gray")
			g <- g + geom_point(aes_string(colour=grp), data=filter(d, Source=="Observed"), size=2.5) +
				scale_color_manual(values=colpal[-c(1,2)]) + scale_fill_manual(values=colpal[-c(1,2)])
		} else g <- g + geom_point() + geom_point(data=filter(d, Source=="Observed"), size=2.5) + scale_color_manual(values=colpal) + scale_fill_manual(values=colpal)
	}
    ttl <- maintitle
	g <- g + theme_bw(base_size=fontsize) + theme(legend.position=tolower(lgd.pos)) + ggtitle(ttl) + xlab(xlb) + ylab(ylb)
	if(!is.null(facet.by)){
        string <- if(length(facet.by)==1) paste("~", facet.by) else paste(facet.by[1], "~", facet.by[2])
        g <- g + facet_wrap(as.formula(string), ncol=as.numeric(facet.cols), scales=facet.scales)
    }
	print(g)
}

subjects <- sprintf("interaction(%s)", paste0(c("Replicate", "Vegetation"), collapse = ", "))
png(paste0(out, "/CABvsTimeByVeg_fullDomain_", yr.start, "_", yr.end, ".png"), width=3200, height=2400, res=200)
plotRegionalTABbyTime(d.fs %>% filter(Domain=="Full"), c(yr.start, yr.end), cumulative=TRUE, subject=subjects, grp="Vegetation", colpal=cbpalette, facet.by="~ Vegetation", facet.cols=3, facet.scales="free")
dev.off()
png(paste0(out, "/CABvsTimeByVeg_maskedToObsFire_", yr.start, "_", yr.end, ".png"), width=3200, height=2400, res=200)
plotRegionalTABbyTime(d.fs %>% filter(Domain=="Masked"), c(yr.start, yr.end), cumulative=TRUE, subject=subjects, grp="Vegetation", colpal=cbpalette, facet.by="~ Vegetation", facet.cols=3, facet.scales="free")
dev.off()

veg.lev <- c("Combined Area", unique(d.fs$Vegetation))
d.fs2 <- d.fs %>% filter(Domain=="Full") %>% mutate(FS=ifelse(Source=="Modeled", FS*empba.totalba.ratio, FS)) %>% group_by(Domain, Source, Replicate, Year)
d.fs2 <- d.fs2 %>% rbind(d.fs2 %>% summarise(FS=sum(FS), Vegetation=veg.lev[1])) %>% group_by(Domain, Source, Replicate, Vegetation) %>%
    arrange(FS) %>% mutate(CAB=cumsum(FS)) %>% group_by %>% mutate(Vegetation=factor(Vegetation, levels=veg.lev), Source=factor(Source, levels=c("Observed", "Modeled")))

png(paste0(out, "/CABvsFSByVeg_ObsFire_to_AlfArea_Ratio_scaledDomain_", yr.start, "_", yr.end, ".png"), width=3200, height=2400, res=200)
ggplot(d.fs2, aes(x=FS, y=CAB, group=Replicate, Vegetation, colour=Source)) +
    geom_step(data=filter(d.fs2, Source=="Modeled"), colour="gray") + geom_step(data=filter(d.fs2, Source=="Observed"), aes(group=Source, colour=Source), size=1) +
    facet_wrap(~ Vegetation) + theme(legend.position="bottom") +
    labs(title=paste(yr.start, "-", yr.end, "cumulative area burned vs. fire size by vegetation and combined area"), x=expression("Fire size "~(km^2)~""), y=expression("Cumulative burn area "~(km^2)~"")) +
    scale_colour_manual(values=cbpalette)
dev.off()

png(paste0(out, "/CABvsFSByVeg_ObsFire_to_AlfArea_Ratio_scaledDomain_", yr.start, "_", yr.end, "_freeXY.png"), width=3200, height=2400, res=200)
ggplot(d.fs2, aes(x=FS, y=CAB, group=Replicate, Vegetation, colour=Source)) +
    geom_step(data=filter(d.fs2, Source=="Modeled"), colour="gray") + geom_step(data=filter(d.fs2, Source=="Observed"), aes(group=Source, colour=Source), size=1) +
    facet_wrap(~ Vegetation, scales="free") + theme(legend.position="bottom") +
    labs(title=paste(yr.start, "-", yr.end, "cumulative area burned vs. fire size by vegetation and combined area"), x=expression("Fire size "~(km^2)~""), y=expression("Cumulative burn area "~(km^2)~"")) +
    scale_colour_manual(values=cbpalette)
dev.off()

sink(file=file.path(out, "message.txt"), append=TRUE)
cat("An R workspace file containing fire event sizes partitioned by vegetation class is attached.\n")
sink()
