setwd("C:/github/SNAPQAQC/data/final/alfresco/IEM_Bird/samples/TPA Regions")

library(data.table)
library(dplyr)
library(purrr)
library(ggplot2)
library(rvtable)

reg <- list.files()

loadData <- function(region, variable){
  y <- switch(variable, "age"="vegage", "veg"="vegarea")
  load(paste0(region, "/historical_", y, ".RData"), envir=environment())
  if(variable=="age") x <- d.alf.vegage
  if(variable=="veg") x <- d.alf.vegarea
  load(paste0(region, "/projected_", y, ".RData"), envir=environment())
  if(variable=="age") x <- bind_rows(x, d.alf.vegage)
  if(variable=="veg") x <- bind_rows(x, d.alf.vegarea)
  select(x, -Phase, -Scenario, -Var) %>%
    mutate(Decade=Year - Year %% 10) %>% rvtable
}

prepAge <- function(region, vegetation, breaks, labels, size=10000,
                    density.args=list(adjust=0.1, n=1000), out="both",
                    period.breaks=NULL, period.labels=NULL){
  x <- loadData(region, "age")
  x <- filter(x, Location==region & Vegetation==vegetation)
  if(!is.null(period.breaks)){ # only do this if periods requested; ignore all else
    p <- filter(x, Year > period.breaks[1] & Year <= tail(period.breaks, 1)) %>%
      mutate(Period=cut(Year, breaks=period.breaks, labels=period.labels)) %>% rvtable
    p <- marginalize(p, c("Model", "Decade", "Year"), density.args=density.args)
    p <- sample_rvtable(p, n=1000, density.args=density.args)
    return(p)
  }
  if(out!="decadal"){
    a <- marginalize(x, "Model", density.args=density.args)
    a <- sample_rvtable(a, density.args=density.args)
    a <- mutate(a, Age=cut(Val, breaks=breaks, labels=labels)) %>%
      group_by(Age, add=TRUE) %>% summarise(n=n()/size)
  }
  if(out!="annual"){
    d <- marginalize(x, c("Model", "Year"), density.args=density.args)
    d <- sample_rvtable(d, density.args=density.args)
    d <- mutate(d, Age=cut(Val, breaks=breaks, labels=labels)) %>%
      group_by(Age, add=TRUE) %>% summarise(n=n()/size)
  }
  if(out=="annual") return(list(annual=a))
  if(out=="decadal") return(list(decadal=d))
  list(annual=a, decadal=d)
}

prepVeg <- function(region, vegetation, lb=0.025, ub=0.975, size=10000,
                    density.args=list(adjust=0.1, n=1000), out="both"){
  x <- loadData(region, "veg")
  x <- filter(x, Location==region & Vegetation==vegetation)
  if(out!="decadal"){
    a <- marginalize(x, "Model", density.args=density.args)
    a <- sample_rvtable(a, density.args=density.args)
    a <- summarise(a, LB=quantile(Val, lb), UB=quantile(Val, ub))
  }
  if(out!="annual"){
    d <- marginalize(x, c("Model", "Year"), density.args=density.args)
    d <- sample_rvtable(d, density.args=density.args)
    d <- summarise(d, LB=quantile(Val, lb), UB=quantile(Val, ub))
  }
  if(out=="annual") return(list(annual=a))
  if(out=="decadal") return(list(decadal=d))
  list(annual=a, decadal=d)
}

prepData <- function(region, vegetation, breaks, labels, size=10000, lb=0.025,
                     ub=0.975, density.args=list(adjust=0.1, n=1000), out="both"){
  cat("\nPreparing age data...\n")
  age <- prepAge(region, vegetation, breaks, labels, size, density.args, out)
  cat("\nPreparing veg data...\n")
  veg <- prepVeg(region, vegetation, lb, ub, size, density.args, out)
  cat("\nCombining age and veg data...\n")
  if(out!="decadal"){
    a <- left_join(age$annual, veg$annual,
                   by=c("Location", "Vegetation", "Decade",  "Year"))
    a <- mutate(a, Mean=n*(LB+UB)/2, LB=n*LB, UB=n*UB)
  }
  if(out!="annual"){
    d <- left_join(age$decadal, veg$decadal,
                   by=c("Location", "Vegetation", "Decade"))
    d <- mutate(d, Mean=n*(LB+UB)/2, LB=n*LB, UB=n*UB)
  }
  if(out=="annual") return(list(annual=a))
  if(out=="decadal") return(list(decadal=d))
  list(annual=a, decadal=d)
}

veg <- c("Black Spruce", "White Spruce", "Deciduous")
brk <- list(c(-25, 200, 10000), c(-25, 200, 10000), c(-25, 25, 50, 100, 10000))
lab <- list(c("0 - 200", "> 200"), c("0 - 200", "> 200"), c("0 - 25", "26 - 50", "51 - 100", "> 100"))

d <- vector("list", length(reg))
for(i in seq_along(reg)){
  cat(paste0("\nProcessing ", reg[i], "...\n"))
  x <- map(seq_along(veg), ~prepData(reg[i], veg[.x], brk[[.x]], lab[[.x]]))
  x <- map(transpose(x), ~bind_rows(.x))$annual
  x <- map2(veg, lab, ~filter(x, Vegetation==.x) %>%
              mutate(Age=factor(Age, levels=.y)) %>% group_by(Location, Vegetation, Age))
  names(x) <- veg
  d[[i]] <- x
  rm(x)
}

d <- map(transpose(d), ~bind_rows(.x))

pbrk <- c(2009, 2039, 2069, 2099)
plab <- c("2010 - 2039", "2040 - 2069", "2070 - 2099")

p <- vector("list", length(reg))
for(i in seq_along(reg)){
  cat(paste0("\nProcessing ", reg[i], "...\n"))
  x <- map(seq_along(veg), ~prepAge(reg[i], veg[.x], NULL, NULL, period.breaks=pbrk, period.labels=plab))
  p[[i]] <- bind_rows(x)
  rm(x)
}

p <- bind_rows(p)

dir.create(outDir <- "C:/github/SNAPQAQC/apps/standage", recursive=TRUE, showWarnings=FALSE)
saveRDS(d, file=file.path(outDir, "age.rds"))
saveRDS(p, file=file.path(outDir, "distributions.rds"))
