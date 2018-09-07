globalVariables(c(".", ".data"))

#' snaputils: SNAP-specific utilities for Shiny apps.
#'
#' The \code{snaputils} package contains utilities for Shiny app development.
#' The package inherits primarily from `apputils`, which contains common utility functions, settings and references for use across multiple Shiny apps.
#' It also imports \code{maputils}. As the name suggests, \code{snaputils} is specific to a SNAP context. So is \code{maaputils} though separate.
#' Only \code{apputils} has a more general non-SNAP context.
#' All three packages are satellite members of the SNAPverse collection of R packages.
#' 
#' @docType package
#' @name snaputils
NULL

#' @importFrom magrittr %>%
NULL

#' Get a snaputils resource path
#'
#' Get a resource path in an app to local package resources.
#'
#' This function is called in the UI code of an app that relies \code{snaputils} package resources, typically image logos.
#'
#' @param type character, resource type. \code{"images"} is the only currently available resource type.
#'
#' @export
#'
#' @examples
#' \dontrun{snap_res()}
snap_res <- function(type = "images"){
  system.file(file.path("res", type), package = "snaputils")
}
