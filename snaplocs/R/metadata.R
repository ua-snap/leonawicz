#' Get point location metadata.
#'
#' These functions return metadata associate with a point location.
#'
#' \code{get_state} returns the state, province or territory of a point location.
#' \code{get_country} returns the name of the country (US or Canada).
#' \code{get_coords} returns the longitutde and latitude of a point location as a data frame.
#'
#' @param location character, a name of a location in the \code{locs} data frame. May be a vector.
#' @param group character, the region group/set the location belongs to.
#' If supplied, the \code{locs} data set is first filtered to \code{group}. May be a vector.
#' @param keep_cols logical, keep all columns with \code{get_coords}. Defaults to \code{FALSE}, returning only the \code{lon} and \code{lat} columns.
#' @name metadata
#'
#' @return a character string, or a data frame for \code{get_coords}.
#'
#' @examples
#' x <- "Calgary"
#' get_state(x)
#' get_country(x)
#' get_coords(x)
NULL

#' @export
#' @rdname metadata
get_state <- function(location, group){
  if(missing(group)) group <- NULL
  locs <- .prep_locs(group = group)
  idx <- which(locs$Location %in% location)
  .no_loc(location, idx)
  idx <- idx[match(location, locs$Location[idx])]
  as.character(locs$Group[idx])
}

#' @export
#' @rdname metadata
get_province <- function(location, group){
  get_state(location, group)
}

#' @export
#' @rdname metadata
get_country <- function(location, group){
  get_state(location, group) %>%
    purrr::map_chr(~ifelse(.x == "Alaska", "United States", "Canada"))
}

#' @export
#' @rdname metadata
get_coords <- function(location, group, keep_cols = FALSE){
  if(missing(group)) group <- NULL
  locs <- .prep_locs(group = group)
  idx <- which(locs$Location %in% location)
  .no_loc(location, idx)
  idx <- match(location, locs$Location[idx])
  x <- dplyr::filter(locs, .data[["Location"]] %in% location) %>% dplyr::slice(idx)
  if(keep_cols) x else dplyr::select(x, .data[["lon"]], .data[["lat"]])
}

.no_loc <- function(location, idx){
  noloc <- length(location) > length(idx)
  location <- if(length(idx)) "At least one location" else paste0("'", location, "'")
  if(noloc) stop(paste0(location, " is not an available location in `locs`."))
}

.prep_locs <- function(group){
  if(is.null(group)) return(snaplocs::locs)
  if(!group %in% levels(snaplocs::locs$Group)) stop("Invalid `group`.")
  y <- group
  dplyr::filter(snaplocs::locs, .data[["Group"]] %in% y)
}

#' Common SNAP regions
#'
#' Commonly used political and ecological polygon regions at SNAP.
#'
#' A data frame of available political and ecological regions and their respective region groups/sets
#' commonly used with SNAP data spatial distributions and regional summary statistics.
#'
#' @format A data frame with 86 rows and 2 columns giving the region name and the set/group it belongs to.
"regions"
