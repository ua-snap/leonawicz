#' snapapps: Collection of SNAP Shiny apps.
#'
#' The \code{snapapps} package contains a collection of open source Shiny apps by Matthew Leonawicz at the
#' Scenarios Network for Alaska and Arctic Planning, University of Alaska Fairbanks.
#' It is a member package in the apps and docs sector of the SNAPverse collection of R packages.
#' Many of the apps in this package originate from \url{https://github.com/ua-snap/shiny-apps}.
#' Additional apps can be found in other SNAPverse apps and docs sector packages.
#' Related sector packages include \code{snapdash}, \code{snapflex} and \code{snapdocs}.
#'
#' @docType package
#' @name snapapps
NULL

#' Run SNAP Shiny app examples
#'
#' Launch a Shiny app in your browser.
#'
#' The \code{source} defaults to \code{"local"} for package apps. If set to \code{"remote"},
#' \code{snapp} will launch the app in your browser using the canonical url rather than the package app.
#' Options for \code{local_mode} are \code{"normal"} (default) or \code{"showcase"}
#' for tandem reactive code highlighting, and metadata if applicable. \code{local_mode} applies to \code{source = "local"} package apps.
#'
#' @param id character, the name of the application. See \link{snapps} for available IDs.
#' @param source character, source of app. See details.
#' @param local_mode character, display mode.
#'
#' @export
#'
#' @examples
#' \dontrun{snapp("rv4")}
snapp <- function(id, source = "local", local_mode = "normal") {
  if(!local_mode %in% c("normal", "showcase"))
    stop("`local_mode` must be 'normal' or 'showcase'.")
  x <- snapps()
  if(!id %in% x$id) stop("Invalid app `id`. See `snapps` for available apps.")
  url <- x$url[x$id == id]
  id2 <- basename(url)
  if(source == "remote"){
    name <- x$name[x$id == id]
    utils::browseURL(url)
    cat(paste(name, "launched remotely.\n"))
    return(invisible())
  }
  shiny::runApp(system.file("shiny", id2, package = "snapapps"), display.mode = local_mode)
}

#' Basic metadata for all apps in snapapps
#'
#' This function returns a data frame with basic meta data for all Shiny apps in \code{snapapps}.
#' This includes the following information:
#'
#' \itemize{
#'   \item the app ID used for launching an app via \code{snapp}.
#'   \item the app name.
#'   \item a short description.
#'   \item date originally published.
#'   \item date most recently revised.
#'   \item app complexity rating: \code{beginner}, \code{intermediate}, \code{advanced} or \code{developer}.
#'   \item a status indicator of whether the app is \code{complete} or \code{partial} with respect to the canonical hosted app.
#'   For example, did the local app require available data sets to be reduced or removed the minimize its size in \code{snapapps}?
#'   \item an indicator of whether the app relies on publicly accessible data stored on Amazon Web Services in order to minimize package size.
#'   When possible, AWS is used to ensure a packaged app can be offered as \code{complete}.
#'   \item the canonical remote url.
#'   \item the redirect url.
#' }
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' snapps()
snapps <- function(){
  urls <- .snapp_url()
  tibble::data_frame(id = .snapp_id, name = .snapp_name, description = .snapp_desc,
                     published = .snapp_pubdate, revised = .snapp_revdate, rating = .snapp_rating,
                     status = .snapp_status, aws = .snapp_aws, url = urls$url, redirect = urls$redirect)
}

.snapp_id <- c("rv", "twe", "sic", "siw", "rv1", "rv2", "rv3", "rv4", "tring")
.snapp_name <- c("RV distributions (official)", "Temperature and wind", "Sea ice coverage", "Sea ice and wind",
                 paste0("RV distributions (legacy) v", 1:4), "Tree rings")
.snapp_desc <- c("Distributions of random variables",
                 "Coastal Alaska extreme temperatures and winds", "Alaska sea ice coverage totals",
                 "Coastal Alaska sea ice and winds",
                 rep("Distributions of random variables", 4), "Climate and tree growth correlations")
.snapp_basename <- c("rvdist", "temp_wind_events", "sea_ice_coverage", "sea_ice_winds",
                     paste0("RV_distributions", c("", "V2", "V3", "V4")), "tree_rings")
.snapp_pubdate <- c(2017, rep(2013, 8))
.snapp_revdate <- rep(2017, 9)
.snapp_status <- rep("complete", 9)
.snapp_aws <- c(FALSE, rep(TRUE, 3), rep(FALSE, 5))
.snapp_levels <- c("Beginner", "Intermediate", "Advanced", "Developer")
.snapp_rating <- factor(
  c(.snapp_levels[1], rep(.snapp_levels[2], 3), rep(.snapp_levels[1], 5)),
  levels = .snapp_levels)
.snapp_url <- function(canonical = "https://uasnap.shinyapps.io/",
                        redirect = "http://shiny.snap.uaf.edu/"){
  list(url = paste0(canonical, .snapp_basename), redirect = paste0(redirect, .snapp_basename))
}
