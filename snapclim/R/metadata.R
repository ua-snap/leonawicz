.ids <- c("ar5stats")

#' Basic metadata for all data sets available via snapclim
#'
#' This function returns a data frame with basic meta data for all of the curated collections of SNAP climate data
#' available via \code{snapclim}.
#' This includes ID, short description, and spatial and temporal information.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' climate_collections()
climate_collections <- function(){
  id <- .ids
  desc <- c("AR5/CMIP5 climate statistics")
  regions <- TRUE
  points <- TRUE
  start <- 1901
  end <- 2100
  daily <- FALSE
  monthly <- TRUE
  seasonal <- TRUE
  annual <- TRUE
  decadal <- TRUE
  tibble::data_frame(id = id, description = desc, regions = regions, points = points, start = start, end = end,
                     daily = daily, monthly = monthly, seasonal = seasonal, annual = annual, decadal = decadal)
}

region_groups <- unique(snaplocs::regions$Group)

point_groups <- levels(snaplocs::locs$Group)[-5]

#' List the available location sets
#'
#' List the available location sets/groups for regional or point location data.
#'
#' @param type character, \code{"region"} or \code{"point"}.
#'
#' @return a character vector.
#' @export
#'
#' @examples
#' location_sets("region")
#' location_sets("point")
location_sets <- function(type){
  switch(type, region = region_groups, point = point_groups)
}

#' Display available locations
#'
#' Display a data frame of available locations and their respective location groups/sets.
#'
#' @param type character, \code{"all"}, \code{"region"} or \code{"point"}.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' climate_locations()
climate_locations <- function(type = "all"){
  cr <- snaplocs::regions
  if(type == "region") return(dplyr::rename(cr, Location = .data[["Region"]]))
  pts <- snaplocs::locs %>% dplyr::select(-.data[["lon"]], -.data[["lat"]]) %>%
    dplyr::filter(.data[["Group"]] != "Northwest Territories") %>%
    dplyr::mutate(Group = as.character(.data[["Group"]]))
  if(type == "point") return(pts)
  if(type == "all")
    return(dplyr::bind_rows(dplyr::rename(cr, Location = .data[["Region"]]), pts))
}
