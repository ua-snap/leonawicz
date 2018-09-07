#' Use apps from shiny-apps repo in snapapps package
#'
#' Include Shiny apps from a local clone of the SNAP shiny-apps repository in the local clone of the snapapps package.
#'
#' This function will check (assuming default arguments) to see if local git repositories exist
#' for both the \code{snapapps} package and the SNAP \code{shiny-apps} repository.
#' This is assuming it is called with the \code{snapapps} directory as the working directory, that \code{source_path} is unchanged, and that they are
#' adjacent to each other.
#' For general use, change \code{base_path} to your source package root directory and
#' change \code{source_path} to whatever directory contains your existing apps.
#' By default, \code{use_apps} also assumes the package installed resources location for apps is be \code{"inst/shiny"}.
#'
#' If \code{id} is a valid app directory in the latter, it will be copied to the former. \code{id} may be a vector.
#' This function should be used once per included app. It should only be reused (to update an app) if changes have not been made
#' to the copy previously placed in \code{snapapps} or you will lose those changes.
#'
#' At this time, the canonical source is the \code{shiny-apps} repository, though this may swap to \code{snapapps} in the future
#' when critical mass favors the package, as well as more apps being includes from sources other than \code{shiny-apps}.
#'
#' This function should be called for copying apps into the \code{snapapps} top level directory. It will place apps in \code{inst/shiny}.
#' Another base path should only be used if apps are to be copied to some other location that \code{snapapps} for some reason.
#'
#' @param id character, app directory name.
#' @param base_path base file path, defaults to working directory.
#' @param source_path character, path to source directory containing apps to be copied. See details.
#' @param res_path character, path to installed resources where apps will be copied to. See details.
#' @param description logical, add a \code{DESCRIPTION} file template for showcase mode.
#' @param readme logical, add a \code{Readme.md} template for showcase mode.
#' @param overwrite logical, overwrite previously added apps.
#'
#' @export
#'
#' @examples
#' \dontrun{use_apps(id = "rvdist")}
use_apps <- function(id, base_path = ".", source_path = "../shiny-apps", res_path = "inst/shiny",
                     description = TRUE, readme = TRUE, overwrite = FALSE){
  if(!file.exists("../shiny-apps"))
    stop("`shiny-apps` directory does not exist adjacent to parent directory.")
  if(any(!id %in% list.files(source_path)))
    stop("`id` must refer to an app in the local `shiny-apps` repository.")
  path <- file.path(base_path, res_path)
  if(!file.exists(path)){
    message(paste0("`", res_path, "` does not exist. Creating it now."))
    dir.create(res_path, recursive = TRUE, showWarnings = FALSE)
  }
  cur_files <- list.files(path)
  if(!length(cur_files)) cur_files <- NULL
  purrr::walk(id, ~(if(.x %in% cur_files & overwrite)
    unlink(file.path(path, .x), recursive = TRUE, force = TRUE)))
  purrr::walk(id, ~(if(!.x %in% cur_files || overwrite)
    dir.create(file.path(path, .x), recursive = TRUE, showWarnings = FALSE)))
  purrr::walk(id, ~(if(!.x %in% cur_files || overwrite){
    cat(paste("Copying app to:", file.path(path, .x), "\n"))
    file.copy(file.path(source_path, .x), path, recursive = TRUE)
    if(description) use_appdesc(file.path(path, .x))
    if(readme) use_appreadme(file.path(path, .x))
    })
  )
  purrr::walk(id, ~(if(!.x %in% cur_files || overwrite){
    if("rsconnect" %in% list.files(file.path(path, .x)))
      unlink(file.path(path, .x, "rsconnect"), recursive = TRUE, force = TRUE)
    })
  )
  invisible()
}

#' Create an app DESCRIPTION file
#'
#' Add a DESCRIPTION template to a Shiny app.
#'
#' This file is used with Shiny app showcase mode.
#' Since \code{snapmeta} is a developer package used by the SNAPverse author/maintainer,
#' most arguments to \code{use_appdesc} have contextual defaults that you must otherwise override.
#' \code{title} and \code{tags} do not have defaults. If missing, they should be updated directly in the generated DESCRIPTION file.
#' Tags passed in the form \code{tags = c("random variables", "plotmath")} will result in \code{random-variables, plotmath}.
#'
#' @param base_path output directory.
#' @param title app title.
#' @param author author name.
#' @param url author url.
#' @param license license type.
#' @param mode Shiny display mode.
#' @param tags optional tags. May be a vector. See details.
#'
#' @export
#'
#' @examples
#' \dontrun{use_appdesc()}
use_appdesc <- function(base_path = ".", title, author = "Matthew Leonawicz",
                        url = "https://leonawicz.github.io/", license = "MIT",
                        mode = "Showcase", tags){
  x <- readLines(file.path(system.file(package = "snapmeta"), "resources/apps/DESCRIPTION"))
  if(!missing(title)) x <- gsub("__title__", title, x)
  if(!missing(tags)){
    if(!inherits(tags, "character")) stop("`tags` must be a character vector.")
    tags <- paste(gsub(" ", "-", tags), collapse = ", ")
    x <- gsub("__tags__", tags, x)
  }
  x <- gsub("__author__", author, x)
  x <- gsub("__authorurl__", url, x)
  x <- gsub("__license__", license, x)
  x <- gsub("__displaymode__", mode, x)
  x <- paste0(paste(x, collapse = "\n"), "\n")
  sink(file.path(base_path, "DESCRIPTION"))
  cat(x)
  sink()
  cat("Added DESCRIPTION file template.\n")
  invisible()
}

#' Create an app Readme.md file
#'
#' Add a Readme.md template to a Shiny app.
#'
#' This file is used to add a text description below a Shiny app when run in showcase mode.
#'
#' @param base_path output directory.
#'
#' @export
#'
#' @examples
#' \dontrun{use_appreadme()}
use_appreadme <- function(base_path = "."){
  sink(file.path(base_path, "Readme.md"))
  cat("This Shiny app...\n")
  sink()
  cat("Added Readme.md template.\n")
  invisible()
}
