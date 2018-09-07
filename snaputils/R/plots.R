#' Divide sample size by a factor based on the number of RCP-GCM combinations
#'
#' This function reduces sample size of point sample overlays in plots to improve aesthetics and/or save time
#' when there are too many points to plot.
#'
#' @param x special data frame.
#' @param baseline character, name of baseline data set.
#'
#' @return a number.
#' @export
#'
#' @examples
#' #not run
samplesize_factor <- function(x, baseline){
  if("RCP" %in% names(x)){
    n.rcps <- nlevels(x$RCP)
    if("Historical" %in% levels(x$RCP) && n.rcps > 1) n.rcps <- n.rcps - 1
  } else n.rcps <- 1
  if("Model" %in% names(x)){
    n.models <- nlevels(x$Model)
    if(baseline %in% levels(x$Model) && n.models > 1) n.models <- n.models - 1
  } else n.models <- 1
  n.rcps * n.models
}
