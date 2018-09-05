#' Obtain SNAP climate data sets
#'
#' Download climate data sets from SNAP.
#'
#' This function downloads climate data sets from Scenarios Network for Alaska and Arctic Planning based on the data set \code{id}
#' and specification of the region or point location of interest.
#'
#' For regional climate data, annual statistics with a temporal resolution of monthly, 3-month seasonal and full annual period
#' are based on regional spatial climate variable distribution samples at the respective resolution.
#'
#' Statistics include:
#' \itemize{
#'   \item mean
#'   \item standard deviation
#'   \item minimum
#'   \item maximum
#'   \item quantiles: 0.025, 0.05, 0.10, 0.25, 0.50 (median), 0.75, 0.90, 0.95 and 0.975.
#' }
#'
#' Comparatively, point location data sets do not include statistics regarding the surrounding spatial distribution of values.
#' Only the \code{Mean} column is retained in these tables.
#'
#' Seasonal and annual aggregate statistics represent means for temperature variables and totals for precipitation.
#' Note that decadal averages are strictly as their name suggests: decadal means of each statistic at the respective intra-annual time steps.
#' E.g., \code{decavg = TRUE} returns a data frame including the mean of ten annual 95th percentile values,
#' not the 95th percentile of the spatial climate variable distribution at a decadal resolution.
#' For something like the latter, this can be computed from raw distribution data.
#'
#' Data sets are retrieved for specific locations. For the smaller, decadal resolution data sets, there is an option to download
#' all point locations in one table with \code{area = "points"} (requires \code{decavg = TRUE}),
#' but in this case it must be requested for a specific climate variable to further reduce the download size to no more than 24
#' MB compressed for all 3,867 point locations.
#' In general, if \code{variable = NULL}, all available climate variables are retained. In the case of \code{area = "points"},
#' a specific climate variable must be provided or it will default to temperature (\code{tas}).
#'
#' @param id character, data set ID. See \code{\link{climate_collections}} for available data sets.
#' @param area character, region or point location of interest. See \code{\link{climate_locations}}.
#' @param set character or \code{NULL}, The set/group that \code{area} belongs to. Can be ignored,
#' but should be provided in rare cases where \code{area} name is not unique (a warning is thrown). See \code{\link{climate_locations}}.
#' @param time_scale character, \code{"monthly"}, \code{"seasonal"}, \code{"annual"}.
#' @param decavg logical, if \code{TRUE}, return decadal means of annual statistics. See details.
#' @param variable \code{NULL} or character: \code{"tas"}, \code{"tasmin"}, \code{"tasmax"} or \code{"pr"}. See details.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' climdata("ar5stats", "AK-CAN")
climdata <- function(id, area, set = NULL, time_scale = "monthly", decavg = FALSE, variable = NULL){
  .check_clim_id(id)
  intra_annual <- switch(time_scale,
                         monthly = month.abb,
                         seasonal = c("Winter", "Spring", "Summer", "Autumn"),
                         annual = "Annual")
  sub_dir <- ifelse(time_scale == "monthly", "monthly", "seasonal")
  intra_var <- paste0(toupper(substr(sub_dir, 1, 1)), substr(sub_dir, 2, nchar(sub_dir) - 2))

  if(area == "points"){
    if(!decavg) stop("Using area = 'points' requires decavg = TRUE.")
    if(is.null(variable)){
      variable <- "tas"
      message("`variable` is NULL. Returning mean temperature: `tas`.")
    }
    sub_dir <- file.path("point/decavg/all_locs", sub_dir)
    file <- file.path(.clim_dir, sub_dir, paste0(variable, "_clim_stats.rds"))
    x <- readRDS(url(file)) %>% dplyr::filter(.data[[intra_var]] %in% intra_annual) %>%
      droplevels %>% dplyr::rename(Group = .data[["LocGroup"]], Model = .data[["GCM"]]) %>%
      dplyr::mutate(Group = as.character(.data[["Group"]]))
    return(x)
  }
  set_ <- .check_area(area, set)
  if(is.null(set)) set <- set_
  .check_set(set)
  if(decavg) sub_dir <- file.path("decavg", sub_dir)
  loc_type <- ifelse(set %in% region_groups, "regional", "point")
  file <- file.path(.clim_dir, loc_type, sub_dir, gsub(" ", "%20", set),
                    paste0(gsub(" ", "%20", gsub("/", "--", area)), "_clim_stats.rds"))
  x <- readRDS(url(file)) %>% dplyr::filter(.data[[intra_var]] %in% intra_annual) %>%
    droplevels %>% dplyr::rename(Model = .data[["GCM"]])
  if("LocGroup" %in% names(x)){
    x <- dplyr::rename(x, Group = .data[["LocGroup"]]) %>%
      dplyr::mutate(Group = as.character(.data[["Group"]]))
  }
  x
}

.clim_dir <- "https://s3.amazonaws.com/leonawicz/clim/stat/ar5_2km"

.check_clim_id <- function(x){
  if(!x %in% .ids) stop("Invalid `id`. See `climate_collections`.")
}

.check_area <- function(x, s = NULL){
  cl <- climate_locations()
  if(!x %in% cl$Location)
    stop("Invalid `area`. See `climate_locations` for available regions and point locations.")
  y <- dplyr::filter(cl, .data[["Location"]] == x)
  set <- y$Group[1]
  if(nrow(y) > 1 & is.null(s))
    warning(paste0("`area` not unique and `set` not provided. Assuming '", set, "'. Please provide `set`."))
  set
}

.check_set <- function(x){
  if(!x %in% c(region_groups, point_groups))
    stop("Invalid `set`. See `location_sets` for available region and point location groups/sets.")
}
