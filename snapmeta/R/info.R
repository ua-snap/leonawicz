#' List local SNAPverse packages
#'
#' Generate a list of existing local SNAPverse packages.
#'
#' This function looks for packages beginning with the expression \code{^snap*} and sharing the same parent
#' directory as the current package/working directory. If \code{self = TRUE} (default),
#' the current working directory will be included in the list if applicable.
#'
#' @param base_path defaults to the parent directory of the working directory.
#' @param self logical, include the currently loaded package (working directory) if in the SNAPverse.
#'
#' @return a character vector.
#' @export
#'
#' @examples
#' sv_local_pkgs()
sv_local_pkgs <- function(base_path="../", self = TRUE){
  x <- list.files(base_path, pattern = "^snap*|^alfresco$|^jfsp$|^rvtable$|^apputils$|^maputils$")
  if(!self) x <- x[x != basename(getwd())]
  x
}

#' Location in the SNAPverse
#'
#' List the packages in and around the SNAPverse and their section of the verse.
#'
#' This function returns a 3-column data frame of all SNAPverse package names, their part of the verse,
#' and whether local git repositories/R projects sharing the same parent directory are found for each.
#' The list includes SNAPverse satellite packages, which are related to and often used in conjunction with,
#' but not a strict part of the SNAPverse.
#'
#' The other packages are considered core packages of the SNAPverse and fall into of three sectors of the verse:
#' functions, data or apps. The three packages listed as \code{sector} packages load different subsets of SNAPverse packages
#' for convenience. \code{snapverse} loads functions, data and apps packages. \code{snaplite} loads only the functions packages.
#' \code{snapdata} loads only the data packages. For most users, the packages most likely needed or of interest
#' are \code{snapstat}, \code{snapapps} and any of the data packages.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' sv_pkgs
sv_pkgs <- function(){
  typenames <- c("sector", "functions", "data", "apps", "satellite")
  type <- list(
    sector = c("snapverse", "snaplite", "snapdata", "snapwebs"),
    functions = c("snapstat", "snapplot", "snaplocs", "snapprep", "alfresco"),
    data = c("snapclim", "snapfire", "snappoly", "snapgrid", "snapdist"),
    apps = c("snapapps", "snapdash", "snapflex", "snapdocs"),
    satellite = c("jfsp", "rvtable", "apputils", "maputils", "snaputils", "snapmeta", "snapsite")

  )
  d <- tibble::data_frame(
    pkg = unlist(type), type = rep(typenames, times=sapply(type, length)), local = FALSE
  )
  x <- sv_local_pkgs()
  if(length(x)) d$local[d$pkg %in% x] <- TRUE
  d
}

pkg_metadata <- list(
  labels = sv_pkgs()$pkg,
  titles = c(
    "Functions, data, apps", "Load functions packages", "Load data packages", "All apps and docs",
    "Analyze SNAP data", "Graphical support", "Work with point data", "Mung source data", "Work with ALFRESCO",
    "SNAP climate data", "SNAP fire data", "SNAP vector maps",
    "SNAP raster maps", "Spatial distributions",
    "SNAP Shiny apps", "SNAP Shiny apps", "SNAP dashboards", "SNAP docs",
    "Joint Fire Science Program", "Random variables", "Shiny app support", "Shiny and Leaflet", "SNAP app support",
    "SNAPverse integration", "SNAPverse integration"
    ),
  subtitles = c(
    "Traverse the verse...", "A lightweight collection", "All data in one call", "Wrapped in neat packages",
    "Functions for users", "Functions for developers", "Functions for users", rep("Functions for developers", 2),
    "Modeled and observational", "Modeled and observational", "Common spatial polygons",
    "Common raster layers", "Example data sets",
    "Various examples", "Shiny dashboards", "Flex dashboard examples", "Interactive documents",
    "Alaska wildfire", "Distributions tables", "Common utilities", "Mapsets utilities", "SNAP-specific utilities",
    "For developers", "For developers"
    )
)

#' Get heading information related to SNAPverse R packages
#'
#' This function returns a list of headings related to SNAPverse R packages for \code{apputils::app_showcase} display purposes.
#'
#' Labels are generally shown above an app showcase widget. Titles are generally the top line heading over the image link inside the widget.
#' Subtitles are for smaller text below.
#'
#' @param type character, \code{"label"}, \code{"title"} or \code{"sub"}.
#'
#' @return a vector of headings.
#' @export
#'
#' @examples
#' pkg_titles("label")
pkg_titles <- function(type){
  switch(type,
         "label" = pkg_metadata$labels,
         "title" = pkg_metadata$titles,
         "sub" = pkg_metadata$subtitles)
}

#' Get members of a sector
#'
#' Get a list of the collection of member packages that are installed and loaded via a SNAPverse sector package.
#'
#' The available sector packages are \code{"snapverse"}, \code{"snaplite"}, \code{"snapdata"} and \code{"snapwebs"}.
#'
#' @param pkg character, a sector package. See details.
#'
#' @return a character vector.
#' @export
#'
#' @examples
#' sector_members("snapverse")
sector_members <- function(pkg){
  pkgs <- snapmeta::sv_pkgs()
  if(!pkg %in% dplyr::filter(pkgs, .data[["type"]] == "sector")$pkg)
    stop(paste(pkg, "is not a SNAPverse sector package."))
  switch(
    pkg,
    "snapverse" = pkgs$pkg[pkgs$type %in% c("functions", "data", "apps")],
    "snaplite" = pkgs$pkg[pkgs$type == "functions"],
    "snapdata" = pkgs$pkg[pkgs$type == "data"],
    "snapwebs" = pkgs$pkg[pkgs$type == "apps"])
}
