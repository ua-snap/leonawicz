#' snappoly: Common SNAP geographic polygon units in SpatialPolygonsDataFrame format.
#'
#' The snappoly package contains a collection of spatial polygons maps focused on
#' Alaska and Canada, including political boundaries, ecological regions, fire management zones,
#' terrestrial protected areas under jurisdiction and management
#' of various governmental agencies and more.
#'
#' snappoly is a member package in the data sector of the SNAPverse.
#' All maps are in the NAD83 Alaska Albers Equal Area Conic projection for convenience and conformity,
#' but due to the disparate sources of the maps other attributes are not intended to conform.
#'
#' There are eight available maps:
#' \describe{
#' \item{\code{alaska}}{State of Alaska}
#' \item{\code{canada}}{Canadian provinces}
#' \item{\code{ecoreg}}{Alaska ecological regions (ecoregions)}
#' \item{\code{aklcc}}{Alaska Landscape Conservation Cooperative}
#' \item{\code{lcc}}{Alaska and Canada Landscape Conservation Cooperative}
#' \item{\code{cavm}}{Circumpolar Arctic Vegetation (Alaska)}
#' \item{\code{fmz}}{Alaska Fire Service fire management zones}
#' \item{\code{tpa}}{Terrestrial protected areas}
#' }
#'
#' @docType package
#' @name snappoly
NULL

#' State of Alaska.
#'
#' A vector map of the state of Alaska boundary.
#'
#' @format A SpatialPolygonsDataFrame with a single feature.
"alaska"

#' Canadian provinces.
#'
#' A vector map of Canadian provinces.
#'
#' @format A SpatialPolygonsDataFrame with 13 features. See \code{canada$NAME}.
"canada"

#' Alaska ecological regions (ecoregions).
#'
#' A vector map of Alaska ecoregions.
#'
#' @format A SpatialPolygonsDataFrame with 32 features. See the \code{COMMONER}, \code{LEVEL_2}
#' and \code{LEVEL_1} attributes, e.g.: \code{ecoreg$Level_2}.
"ecoreg"

#' Alaska Landscape Conservation Cooperative.
#'
#' A vector map of Alaska Landscape Conservation Cooperative areas.
#' For further details, see \url{https://lccnetwork.org}
#'
#' @format A SpatialPolygonsDataFrame with 5 features. See \code{aklcc$LCC_Name}.
"aklcc"

#' Alaska and Canada Landscape Conservation Cooperative.
#'
#' A vector map of Alaska and Canada Landscape Conservation Cooperative.
#' For further details, see \url{https://lccnetwork.org}
#'
#' @format A SpatialPolygonsDataFrame with 5 features. See \code{lcc$LCC_Name}.
"lcc"

#' Circumpolar Arctic Vegetation (Alaska).
#'
#' A vector map of SNAP's derived version of the Alaska region of the Circumpolar Arctic Vegetation map layer.
#' For further details on the source data, see \url{http://www.geobotany.uaf.edu/cavm}.
#'
#' @format A SpatialPolygonsDataFrame with 3 features. See \code{cavm$Name}.
"cavm"

#' Alaska Fire Service fire management zones.
#'
#' A vector map of Alaska Fire Service fire management zones.
#' For further details, see \url{https://afs.ak.blm.gov/fire-management/zones.php}.
#'
#' @format A SpatialPolygonsDataFrame with 14 features. See \code{fmz$REGION}.
"fmz"

#' Terrestrial protected areas.
#'
#' A vector map of Alaskan and Canadian terrestrial protected areas.
#' Management agencies include the Government of British Columbia, Parks Canada Agency,
#' Government of Yukon Department of Environment, US Bureau of Land Management,
#' US Department of Defense and US Department of Energy, US Fish and Wildlife Service, US National Park Service,
#' and Alaska state Department of Natural Resources.
#'
#' @format A SpatialPolygonsDataFrame with 8 features. See \code{tpa$MGT_AGENCY}.
"tpa"

#' Basic metadata for all data sets in snappoly
#'
#' This function returns a data frame with basic meta data for all data sets in \code{snappoly}.
#' This includes data object names, spatial domain, number of features, number of variables, ID column name,
#' whether or not the SpatialPolygonsDataFrame has multilevel/overlapping features, and a short description.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' snappolys()
snappolys <- function(){
  objects <- c("alaska", "canada", "ecoreg", "aklcc", "lcc", "cavm", "fmz", "tpa")
  id <- c(NA, "NAME", "COMMONER", rep("LCC_Name", 2), "Name", "REGION", "MGT_AGENCY")
  dom <- c("ak", "akcan")[c(1, 2, 1, 1, 2, 1, 1, 2)]
  dims <- purrr::map(objects, ~dim(get(.x)))
  feat <- purrr::map_int(dims, ~.x[1])
  vars <- purrr::map_int(dims, ~.x[2])
  multi <- c(rep(FALSE, 2), TRUE, rep(FALSE, 2), TRUE, rep(FALSE, 2))
  desc <- c(rep("Domain mask", 2), "Alaska ecological regions (levels 1 - 3)",
            "Landscape Conservation Cooperative (Alaska)",
            "Landscape Conservation Cooperative (AK/Can)",
            "Alaska circumpolar artic vegetation",
            "Alaska Fire Service fire management zones",
            "Alaska/Canada terrestrial protected areas"
            )
  tibble::data_frame(data = objects, domain = dom, features = feat, variables = vars,
                     id = id, multilevel = multi, description = desc)
}
