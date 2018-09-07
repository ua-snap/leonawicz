#' Attach SNAPverse member packages
#'
#' Attach SNAPverse member packages for different sectors of the verse.
#' There are three sector packages: \code{snapverse}, \code{snaplite} and \code{snapdata}.
#' Each passes a list of packages via \code{core} to \code{verse_attach}.
#' See the respective packages for details.
#'
#' Verse attachment code snippets are borrowed and modified from tidyverse: https://github.com/tidyverse
#' but are slightly more general to handle different SNAPverse package subsets (sectors of verse).
#' The functionality in \code{snapmeta} only loads sector packages tidily; it does not check for conflicts.
#'
#' @param core character, vector of packages that defines part of the SNAPverse.
#'
#' @export
verse_attach <- function(core) {
  versions <- vapply(core, package_version, character(1))
  packages <- paste0(
    crayon::black("+ "), crayon::blue(format(core)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  info <- platform_info()
  info_name <- paste0(format(names(info), justify = "right"), ": ")
  info <- paste0(style_grey(0.6, info_name), style_grey(0.4, info))

  n <- max(length(packages), length(info))
  info <- c(info, rep("", n - length(info)))

  info <- paste0(packages, "      ", info, collapse = "\n")

  startup_message(info)
  suppressPackageStartupMessages(
    lapply(core, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

#' Check if package is attached
#'
#' Checks if a package is attached.
#'
#' @param x character, package name.
#'
#' @export
#' @rdname verse_attach
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::bgRed(crayon::white(as.character(version[4:length(version)])))
  }
  crayon::black(paste0(version, collapse = "."))
}


platform_info <- function() {
  if (rstudioapi::isAvailable()) {
    ver <- rstudioapi::getVersion()
    ui <- paste0("RStudio ", ver, "")
  } else {
    ui <- .Platform$GUI
  }

  ver <- R.version

  c(
    Date = format(Sys.Date()),
    R = paste0(ver$major, ".", ver$minor),
    OS = os(),
    GUI = ui,
    Locale = Sys.getlocale("LC_COLLATE"),
    TZ = Sys.timezone()
  )
}

os <- function() {
  x <- utils::sessionInfo()$running

  # Regexps to clean up long windows strings generated at
  # https://github.com/wch/r-source/blob/af7f52f70101960861e5d995d3a4bec010bc89e6/src/library/utils/src/windows/util.c

  x <- gsub("Service Pack", "SP", x)
  x <- gsub(" [(]build \\d+[)]", "", x)

  x
}

startup_message <- function(...) {
  if (isTRUE(getOption("snapverse.quiet")))
    return()

  packageStartupMessage(...)
}

style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}

#' Update SNAPverse sector packages
#'
#' Update a SNAPverse sector package and its member packages.
#'
#' @param pkg character, the sector package covering all member packages.
#' @param force If \code{TRUE}, force member install even if unchanged.
#' @param quiet logical.
#'
#' @export
#' @examples
#' \dontrun{
#' sector_update("snapverse")
#' }
sector_update <- function(pkg, force = FALSE, quiet = TRUE) {
  pkgs <- snapmeta::sv_pkgs() %>% dplyr::filter(.data[["type"]] == "sector")
  if(!pkg %in% pkgs$pkg) stop(paste(pkg, "is not a SNAPverse sector package."))
  members <- sector_members(pkg)
  devtools::install_github(paste0("leonawicz/", c(pkg, members)), quiet = quiet, force = force)
  invisible()
}
