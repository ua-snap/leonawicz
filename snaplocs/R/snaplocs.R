globalVariables(".data")

#' snaplocs: Functions for working with SNAP point location data.
#'
#' The \code{snaplocs} package provides functions for working with point location data used in various applications and projects by the
#' Scenarios Network for Alaska and Arctic Planning at the University of Alaska Fairbanks. Point location data is included.
#' Locations include major and minor cities, towns, villages, and some other locations such as mines.
#' This package is part of the SNAPverse collection of R packages.
#'
#' @docType package
#' @name snaplocs
NULL

#' @importFrom magrittr %>%
NULL

#' Point locations of interest in Alaska and western Canada associated with various SNAP projects and applications.
#'
#' This data set contains point locations and minimal associated metadata for 3,914 communities
#' and similar units in Alaska and western Canada. Locations include major and minor cities, towns, villages, and some other locations such as mines.
#'
#' @format A data frame with 3914 rows and 4 variables:
#' \describe{
#'   \item{Location}{point location name}
#'   \item{Group}{region group/set name, a state, privince or territory}
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#' }
#' @source For a visual demonstration of where these locations lie and their availability for use in a live application,
#' see \url{https://uasnap.shinyapps.io/cc4liteFinal/}
"locs"
