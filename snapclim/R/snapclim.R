globalVariables(".data")

#' snapclim: Curated SNAP climate data sets.
#'
#' \code{snapclim} offers curated collections of public climate data sets.
#'
#' The \code{snapclim} package provides access to curated collections of public climate data sets
#' offered by Scenarios Network for Alaska and Arctic Planning (SNAP) at the University of Alaska Fairbanks.
#' \code{snapclim} interfaces with SNAP's Amazon Web Services cloud storage to retrieve specific climate data.
#' Available data includes historical observation-based climate data and historical and projected climate model outputs.
#' Available climate variables include total precipitation and minimum, mean and maximum temperature.
#'
#' Regional climate summaries and climate data at point locations are available,
#' stretching over Alaska and western Canada.
#' Regions include the state of Alaska and several Canadian provinces, ecological regions, fire management zones,
#' terrestrial protected areas under jurisdiction and management of various governmental agencies and more.
#' Point locations include cities, towns, villages and other municipal units and locations of interest.
#'
#' Daily, monthly, seasonal, annual and decadal temporal resolutions are available.
#' However, not all combinations of temporal and spatial resolution exist, e.g.,
#' daily point location climate projections.
#'
#' \code{snapclim} is a member package in the data sector of the SNAPverse.
#' Data packages typically include raw data sets in support of other R packages.
#' \code{snapclim} is technically more like a typical R package. Instead of storing local copies of data sets,
#' it contains functions for accessing external data sets that would be too large to store conveniently even in an
#' explicit data package, especially considering that any given user session would likely utilize only a small fraction
#' of the total available data. However, \code{snapclim} does not offer functionality beyond accessing data and is
#' therefore still best conceptualized as a data package.
#' Functions for statistical analysis and modeling of SNAP data are already encompassed in packages like \code{snapstat}.
#'
#' @docType package
#' @name snapclim
NULL

#' @importFrom magrittr %>%
NULL
