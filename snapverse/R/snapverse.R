#' snapverse: The primary sector package for the SNAPverse.
#'
#' The snapverse package loads a subset of individual SNAPverse member packages.
#'
#' \describe{
#'   \item{snapstat}{functions for working with popular, curated SNAP data from SNAPverse data packages.}
#'   \item{snapplot}{functions for graphing SNAP data.}
#'   \item{snaplocs}{functions for working with SNAP point locations data.}
#'   \item{snapprep}{functions for SNAP source data extraction, manipulation and curation for various projects.}
#'   \item{alfresco}{functions for working with and analyzing ALFRESCO wildfire model inputs and outputs.}
#'   \item{snapclim}{SNAP climate data.}
#'   \item{snapfire}{SNAP fire data.}
#'   \item{snappoly}{collection of popular polygon shapefile data sets used at SNAP.}
#'   \item{snapgrid}{collection of popular gridded maps used at SNAP.}
#'   \item{snapdist}{SNAP spatial distributions examples data set.}
#'   \item{snapapps}{curated collection of SNAP R Shiny apps loadable via R package.}
#'   \item{snapdash}{selected SNAP Shiny Dashboard apps.}
#'   \item{snapflex}{selected SNAP Flex Dashboard apps.}
#'   \item{snapdocs}{other SNAP interactive documents.}
#' }
#'
#' @docType package
#' @name snapverse
NULL

#' @importFrom magrittr %>%
NULL

# code modified from tidyverse: https://github.com/tidyverse

.onAttach <- function(...) {
  # nolint start
  core <- c("snapplot", "snaplocs", "snapprep", "alfresco", "snapclim", "snappoly", "snapgrid", "snapapps", "snapflex") # temporary override
  #core <- dplyr::filter(
  #  snapmeta::sv_pkgs(), .data[["type"]] %in% c("functions", "data", "apps"))$pkg
  # nolint end
  needed <- core[!snapmeta::is_attached(core)]
  if (length(needed) == 0) return()
  snapmeta::verse_attach(core)
}

#' Update \code{snapverse}
#'
#' Update \code{snapverse} and its member packages.
#'
#' This function updates packages from GitHub. See \url{https://leonawicz.github.io/sv}.
#'
#' @param force If \code{TRUE}, force member install even if unchanged.
#' @param quiet logical.
#'
#' @export
#' @examples
#' \dontrun{
#' snapverse_update()
#' }
snapverse_update <- function(force = FALSE, quiet = TRUE){
  snapmeta::sector_update(pkg = "snapverse", force = force, quiet = quiet)
}
