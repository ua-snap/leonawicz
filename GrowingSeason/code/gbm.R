setwd("/atlas_scratch/mfleonawicz/projects/GrowingSeason/workspaces")
pkgs <- list("rgdal", "raster", "maptools", "ggplot2", "data.table", "dplyr", "tidyr", "parallel", "gbm")
lapply(pkgs, function(x) library(x, character.only=T))

rasterOptions(tmpdir="/atlas_scratch/mfleonawicz/raster_tmp", chunksize=10e10, maxmemory=10e11)
load("data.RData") # d, d.stats, d.stats2, d.hm, sos, ecomask, yrs, cbpal
source("../code/functions.R")
sos <- readAll(brick("../data/sos_1982_2010.tif"))
r <- calc(sos, mean)

d <- dcast(d, Region + Year + Obs + x + y + SOS ~ Threshold, value.var="TDD")
d <- mutate(d, Cell=cellFromXY(sos, cbind(x,y))) %>% select(-x, -y)
setnames(d, c("Region", "Year", "Obs", "SOS", paste0("DOY_TDD", c("05", 10, 15, 20)), "Cell"))
d$Region <- "Alaska"
cells <- (d %>% group_by(Cell) %>% summarise(Region="Alaska", n=n()) %>% filter(n==29))$Cell
d <- filter(d, Cell %in% cells)

set.seed(564)
n.trees <- 100
shrink <- 0.5
frac <- 0.1

# build gbm models
gbm_explore <- function(i, data, n.trees, shrinkage, frac, years=sort(unique(data$Year))){
  d <- filter(data, Year %in% years)
  rm(data); gc()
  if(length(frac)==1) frac <- rep(frac, length(unique(d$Region)))
  d <- d %>% split(.$Region) %>% purrr::map2(frac, ~group_by(.x, Year) %>% sample_frac(.y)) %>% bind_rows %>% data.table %>% group_by(Region)
  d.train <- sample_frac(d, 0.1)
  d.test <- setdiff(d, d.train)
  d.gbm <- d.train %>% split(.$Region) %>% purrr::map2(shrinkage, ~gbm(SOS ~ DOY_TDD10, data=.x,
    distribution="gaussian", bag.fraction=0.5, cv.folds=5, train.fraction=1,
    interaction.depth=1, n.minobsinnode=5, n.trees=n.trees, shrinkage=.y, verbose=FALSE, keep.data=TRUE, n.cores=1))
  d.gbm <- d.train %>% group_by %>% select(Region) %>% distinct(Region) %>% mutate(GBM1=d.gbm) %>% group_by(Region)
  regions <- d.gbm$Region
  d.tdd <- select(d.train, -Obs, -Year, -Cell) %>% melt(id.vars=c("Region", "SOS"), variable.name="Var", value.name="Val") %>% group_by(Region, Var)

  rm(d.train); gc()
  d.bi <- d.gbm %>% do(BI=get_bi(., model=.$GBM1, plotDir, saveplot=F))
  d.gbm <- data.table(suppressMessages(left_join(d.gbm, d.bi))) %>% group_by(Region)
  d.gbm <- d.gbm %>% mutate(CV=purrr::map(BI, ~.x$CV)) %>% group_by(Region)
  d.ri <- d.gbm %>% do(RI=get_ri(., model=.$GBM1, n.trees=.$BI, plotDir, saveplot=F))
  d.gbm <- data.table(suppressMessages(left_join(d.gbm, d.ri))) %>% group_by(Region)
  d.preds <- d.gbm %>%
    do(Predicted=get_preds(., model=.$GBM1, newdata=d.test, n.trees=.$BI, type.err="cv", grp=as.character(groups(d.gbm)))) %>% group_by(Region)
  d.err <- d.gbm %>% group_by(Region) %>% do(Error=get_cv_err(.))
  d.gbm <- suppressMessages(left_join(d.gbm, d.err)) %>% group_by(Region)
  rm(d.bi, d.ri, d.err); gc()

  d.pd <- d.gbm %>% do(PD=get_pd(., source_data=d.tdd, x="Val", y="SOS", outDir=NULL, model=.$GBM1, vars=1, order.by.ri=TRUE,
                                 suffix=.$Region, saveplot=FALSE, grp=as.character(groups(d.gbm)))) %>% group_by(Region)
  d.gbm <- data.table(left_join(d.gbm, d.pd)) %>% group_by(Region)
  rm(d.tdd); gc()

  d.test$Predicted <- unnest(d.preds)$Predicted
  d.test$Run <- i
  d.test <- d.test %>% select(Region, Year, Run, SOS, Predicted) %>% setnames(c("Region", "Year", "Run", "Observed", "Predicted")) %>%
    melt(id.vars=c("Region", "Year", "Run"), value.name="SOS") %>% data.table %>% setnames(c("Region", "Year", "Run", "Source", "SOS")) %>%
    group_by(Region, Year, Run, Source) %>% summarise(SOS=round(mean(SOS)))
  list(GBM=d.gbm, data=d.test)
}

# Run
system.time( dlist <- mclapply(1:32, gbm_explore, d, n.trees=n.trees, shrinkage=shrink, frac=frac, mc.cores=32) )

# Extract tables of models, CV optimal trees, predictions, corrections, etc.
gbm.out <- lapply(dlist, "[[", 1)
ri.out <-  purrr::map2(gbm.out, seq_along(gbm.out), ~select(.x, Region, RI) %>% mutate(Run=.y) %>% unnest) %>% bind_rows %>% filter(Method=="CV")
cv.out <-  purrr::map(gbm.out, ~select(.x, Region, CV)) %>% bind_rows
err.out <-  purrr::map2(gbm.out, seq_along(gbm.out), ~select(.x, Region, Error) %>% mutate(Run=.y)) %>% bind_rows %>% unnest
pd.out <-  purrr::map2(gbm.out, seq_along(gbm.out), ~group_by(.x) %>% select(PD) %>% mutate(Run=.y)) %>% bind_rows %>% unnest
d.out <- rbindlist(lapply(dlist, "[[", 2))
d.out <- group_by(d.out, Region, Year, Source) %>% summarise(SOS=mean(SOS)) %>% bind_rows(filter(d.out))

dir.create("akOnly_singlePred_outputs", showWarnings=FALSE)
save(ri.out, cv.out, pd.out, d.out, file="akOnly_singlePred_outputs/final_gbm_summary_tables_akOnly.RData")

gbm_prediction_maps <- function(newdata, d, r, output="raster", year.method="match", simplify=TRUE, n.cores=32){
  yrs <- sort(unique(newdata$Year))
  newdata <- arrange(newdata, Region, Year, Obs)
  if(output=="table") return(list(d=d, newdata=newdata)) # hack

  d <- mclapply(seq_along(d), function(i, x, y) x[[i]] %>% group_by(Region) %>%
                  do(Predicted=as.integer(round(predict(.$GBM1[[1]], newdata=y, n.trees=.$BI[[1]]$CV)))) %>%
                  mutate(Run=i), x=d, y=newdata, mc.cores=n.cores)
  d <- d %>% purrr::map(~unnest(.x) %>% mutate(Cell=newdata$Cell, Year=as.integer(newdata$Year)) %>%
                          nest(Cell, Predicted) %>% group_by(Region, Year, Run))
  gc()
  setPred <- function(d, r){
    d <- d %>% ungroup %>% unnest %>% select(Predicted, Cell)
    r[d$Cell] <- d$Predicted
    r
  }
  r <- raster(r)
  r <- setValues(r, NA)
  mclapply(d, function(x) brick(purrr::map(x %>% split(.$Year), ~setPred(.x, r))), mc.cores=n.cores)
}

# Run
load("tdd_table.RData")
d.tdd <- mutate(d.tdd, Region="Alaska")
rcps <- unique(d.tdd$RCP)[-1]
models <- unique(d.tdd$Model)[-1]
pred.maps <- purrr::map(vector("list", length(rcps)), ~vector("list", length(models)) %>% setNames(models)) %>% setNames(rcps)
set.seed(1)
system.time({
for(i in seq_along(rcps)){
  for(j in seq_along(models)){
    d.gcm <- d.tdd %>% data.table %>% filter(RCP %in% c("Historical", rcps[i]) & Model==models[j]) %>% select(-RCP, -Model, -x, -y, -Elev, -SOS)
    pred.maps[[i]][[j]] <- gbm_prediction_maps(d.gcm, gbm.out[1:10], r, n.cores=10)
    print(j)
  }
}
})
saveRDS(pred.maps, file="akOnly_singlePred_outputs/gbm_pred_rasters_akOnly.rds")

rcps <- c("RCP 6.0", "RCP 8.5")
gcms <- c("GFDL-CM3", "IPSL-CM5A-LR", "MRI-CGCM3")

extract_to_dt <- function(x, rcp, gcm, run){
  yrs <- as.integer(substr(names(x), 2, 5))
  idx <- which(!is.na(subset(x, 1)[]))
  x <- raster::extract(x, idx)
  data.frame(RCP=factor(rcp, levels=c("RCP 6.0", "RCP 8.5")), Model=gcm, Region="Alaska",
    Year=yrs, Source="Projected", SOS=as.integer(round(as.numeric(colMeans(x, na.rm=TRUE)))), Run=run,
    stringsAsFactors=FALSE) %>%
    tbl_df()
}

d.proj <- vector("list", length(rcps)*length(gcms)*length(pred.maps[[1]][[1]]))
idx <- 1
for(i in seq_along(rcps)){
  for(j in seq_along(gcms)){
    for(k in seq_along(pred.maps[[i]][[j]])){
      d.proj[[idx]] <- extract_to_dt(pred.maps[[i]][[j]][[k]], rcps[i], gcms[j], run=k)
      idx <- idx + 1
      print(paste("j =", j))
    }
  }
}
d.proj <- bind_rows(d.proj) %>% mutate(Model=factor(Model, levels=gcms))
saveRDS(d.proj, "akOnly_singlePred_outputs/sos_projections_akOnly.rds")
