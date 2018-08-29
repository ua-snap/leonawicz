#' Alaska and western Canada.
#'
#' A vector map of full SNAP PRISM domain, a single union polygon of the State of Alaska and the five SNAP-relevant western Canadian provinces: Alberta, British Columbia, Manitoba, Saskatchewan and Yukon Territory.
#'
#' @format A SpatialPolygonsDataFrame with a single feature.
"akcan"

#' Alaska and western Canadian provinces.
#'
#' A vector map like \code{\link{akcan}}, but retaining the six distinct encompassing state/province polygons.
#'
#' @format A SpatialPolygonsDataFrame with 6 features. See \code{akcan2$NAME}.
"akcan2"

#' Alaska level 1 ecological regions (ecoregions).
#'
#' A vector map of Alaska level 1 ecoregions.
#'
#' @format A SpatialPolygonsDataFrame with 3 features. See \code{ecoreg1$NAME}.
"ecoreg1"

#' Alaska level 2 ecological regions (ecoregions).
#'
#' A vector map of Alaska level 2 ecoregions.
#'
#' @format A SpatialPolygonsDataFrame with 9 features. See \code{ecoreg2$NAME}.
#'
"ecoreg2"

#' Alaska level 3 ecological regions (ecoregions).
#'
#' A vector map of Alaska level 3 ecoregions.
#'
#' @format A SpatialPolygonsDataFrame with 32 features. See \code{ecoreg3$NAME}.
"ecoreg3"

#' Alaska Landscape Conservation Cooperative.
#'
#' A vector map of Alaska Landscape Conservation Cooperative areas.
#' For further details, see \url{https://lccnetwork.org}
#'
#' @format A SpatialPolygonsDataFrame with 5 features. See \code{aklcc$NAME}.
"aklcc"

#' Alaska and Canada Landscape Conservation Cooperative.
#'
#' A vector map of Alaska and Canada Landscape Conservation Cooperative.
#' For further details, see \url{https://lccnetwork.org}
#'
#' @format A SpatialPolygonsDataFrame with 5 features. See \code{lcc$NAME}.
"lcc"

#' Circumpolar Arctic Vegetation (Alaska).
#'
#' A vector map of SNAP's derived version of the Alaska region of the Circumpolar Arctic Vegetation map layer.
#' For further details on the source data, see \url{http://www.geobotany.uaf.edu/cavm}.
#'
#' @format A SpatialPolygonsDataFrame with 3 features. See \code{cavm$NAME}.
"cavm"

#' Alaska Fire Service fire management zones.
#'
#' A vector map of Alaska Fire Service fire management zones.
#' For further details, see \url{https://afs.ak.blm.gov/fire-management/zones.php}.
#'
#' @format A SpatialPolygonsDataFrame with 14 features. See \code{fmz$NAME}.
"fmz"

#' Terrestrial protected areas.
#'
#' A vector map of Alaskan and Canadian terrestrial protected areas.
#' Management agencies include the Government of British Columbia, Parks Canada Agency,
#' Government of Yukon Department of Environment, US Bureau of Land Management,
#' US Department of Defense and US Department of Energy, US Fish and Wildlife Service, US National Park Service,
#' and Alaska state Department of Natural Resources.
#'
#' @format A SpatialPolygonsDataFrame with 8 features. See \code{tpa$NAME}.
"tpa"

#' Basic metadata for all data sets in maputils
#'
#' This function returns a data frame with basic meta data for all data sets in \code{maputils}.
#' This includes data object names, spatial domain, number of features, number of variables, ID column name,
#' whether or not the SpatialPolygonsDataFrame has multilevel/overlapping features, and a short description.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' mapdata()
mapdata <- function(){
  objects <- c("akcan", "akcan2", "ecoreg1", "ecoreg2", "ecoreg3", "aklcc", "lcc", "cavm", "fmz", "tpa")
  id <- c(NA, rep("NAME", 9))
  dom <- c("ak", "akcan")[c(2, 2, 1, 1, 1, 1, 2, 1, 1, 2)]
  dims <- purrr::map(objects, ~dim(get(.x)))
  feat <- purrr::map_int(dims, ~.x[1])
  vars <- purrr::map_int(dims, ~.x[2])
  multi <- rep(FALSE, 10)
  desc <- c("PRISM domain mask", "PRISM domain state/provinces", paste("Alaska level", 1:3, "ecoregions"),
            "Landscape Conservation Cooperative (Alaska)",
            "Landscape Conservation Cooperative (AK/Can)",
            "Alaska circumpolar artic vegetation",
            "Alaska Fire Service fire management zones",
            "Alaska/Canada terrestrial protected areas"
  )
  tibble::data_frame(data = objects, domain = dom, features = feat, variables = vars,
                     id = id, multilevel = multi, description = desc)
}
