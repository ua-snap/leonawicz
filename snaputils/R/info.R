snapp_metadata <- list(
  labels = c(
    "RV distributions", "Climate distributions", "Alaska wildfire projections", "Climate model analysis",
    "Community climate outlooks", "Leaflet and Shiny observers", "Northwest Territories",
    "Alaska sea ice maps", "CMIP3/CMPI5 climate", "Alaska daily precipitation",
    "Alaska & Canada climate", "GBM example", "Monty Hall", "plot3D examples", "Random Forest example",
    "ALFRESCO launcher", "Alaska weather stations", "Random variables v1", "Random variables v2",
    "Random variables v3", "Random variables v4", "Sea ice coverage", "Sea ice and winds",
    "Temperature and winds", "Tree rings"),
  titles = c(
    "Random variables", "CMIP5 regional climate", "Wildfire projections", "CMIP5 climate models",
    "Localized projections", "Interactive documents", "NT climate projections",
    "Sea ice edge maps",
    "Downscaled climate",
    "Historical dailies",
    "Climate projections",
    "GBM in Shiny",
    "Generalized Monty Hall",
    "plot3D and rgl examples",
    "Random Forest in Shiny",
    "Shiny ALFRESCO launcher",
    "Weather and climate",
    rep("RV distributions", 4),
    "Sea ice extent",
    "Sea ice and winds",
    "Temperature and winds",
    "Tree rings data sample"),
  subtitles = c(
    "Probability distributions", "Full spatial distributions", "ALFRESCO model output", "GCM evaluation results",
    "Alaska & western Canada", "Leaflet + Shiny observers", "Regional + community level",
    "Monthly and decadal extents",
    "CMIP3/CMPI5 comparisons",
    "Alaska weather stations",
    "Alaska & western Canada",
    "Minimal example",
    "Minimal example",
    "Explore 3D plots in Shiny",
    "Minimal example",
    "Inactive but explorable",
    "Compare station and CRU",
    "Version 1",
    "Version 2",
    "Version 3",
    "Version 4",
    "Temporal and spatial data",
    "Sea ice and winds",
    "Compare extreme thresholds",
    "Simple app example"),
  app_urls = paste0(
    "https://uasnap.shinyapps.io/",
    c("rvdist", "climdist", "jfsp-v10", "ar5eval", "cc4liteFinal", "ex_leaflet", "nwtapp", "ak_ice_edge",
      "#",
      "ak_daily_precipitation", "akcan_climate", "gbm_example", "monty_hall",
      "plot3D", "random_forest_example",
      "#",
      "ak_station_cru_eda", "RV_distributions", "RV_distributionsV2", "RV_distributionsV3",
      "RV_distributionsV4", "sea_ice_coverage", "sea_ice_winds", "temp_wind_events", "tree_rings")),
  img_urls = c(
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/rvdist.jpg",
    "https://raw.githubusercontent.com/leonawicz/dash/master/images/_climdist_small.png",
    "https://raw.githubusercontent.com/leonawicz/jfsp-archive/master/_jfsp_small.png",
    "https://raw.githubusercontent.com/leonawicz/ar5eval/master/_ar5eval_small.png",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/cc4liteFinal.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ex_leaflet.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/nwtapp.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ak_ice_edge.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ar4ar5.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ak_daily_precipitation.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/akcan_climate.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/gbm_example.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/monty_hall.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/plot3D.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/random_forest_example.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/run_alfresco.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ak_station_cru_eda.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/RV_distributions.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/RV_distributionsV2.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/RV_distributionsV3.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/RV_distributionsV4.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/sea_ice_coverage.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/sea_ice_winds.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/temp_wind_events.jpg",
    "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/tree_rings.jpg"),
  gh_urls = c(
    "https://github.com/ua-snap/shiny-apps/tree/master/rvdist",
    "https://github.com/leonawicz/dash/tree/master/climdist",
    "https://github.com/leonawicz/jfsp",
    "https://github.com/leonawicz/ar5eval/tree/master/ar5eval",
    "https://github.com/ua-snap/shiny-apps/tree/master/cc4liteFinal",
    "https://github.com/ua-snap/shiny-apps/tree/master/idocs/ex_leaflet",
    "https://github.com/leonawicz/nwtapp",
    "https://github.com/ua-snap/shiny-apps/tree/master/ak_ice_edge",
    "https://github.com/ua-snap/shiny-apps/tree/master/ar4ar5",
    "https://github.com/ua-snap/shiny-apps/tree/master/ak_daily_precipitation",
    "https://github.com/ua-snap/shiny-apps/tree/master/akcan_climate",
    "https://github.com/ua-snap/shiny-apps/tree/master/gbm_example",
    "https://github.com/ua-snap/shiny-apps/tree/master/monty_hall",
    "https://github.com/ua-snap/shiny-apps/tree/master/plot3D",
    "https://github.com/ua-snap/shiny-apps/tree/master/random_forest_example",
    "https://github.com/ua-snap/shiny-apps/tree/master/run_alfresco",
    "https://github.com/ua-snap/shiny-apps/tree/master/ak_station_cru_eda",
    "https://github.com/ua-snap/shiny-apps/tree/master/RV_distributions",
    "https://github.com/ua-snap/shiny-apps/tree/master/RV_distributionsV2",
    "https://github.com/ua-snap/shiny-apps/tree/master/RV_distributionsV3",
    "https://github.com/ua-snap/shiny-apps/tree/master/RV_distributionsV4",
    "https://github.com/ua-snap/shiny-apps/tree/master/sea_ice_coverage",
    "https://github.com/ua-snap/shiny-apps/tree/master/sea_ice_winds",
    "https://github.com/ua-snap/shiny-apps/tree/master/temp_wind_events",
    "https://github.com/ua-snap/shiny-apps/tree/master/tree_rings")
)

#' Get urls related to SNAP Shiny apps
#'
#' This function returns a list of urls related to SNAP Shiny apps.
#'
#' App urls are for the hosted apps. Image urls are to thumbnail images representing each app.
#' GitHub urls are to the code repositories related to each app.
#'
#' @param type character, \code{"app"}, \code{"img"} or \code{"gh"}.
#'
#' @return a vector of url strings.
#' @export
#'
#' @examples
#' snapp_urls()
snapp_urls <- function(type = "app"){
  switch(type,
         "app" = snapp_metadata$app_urls,
         "img" = snapp_metadata$img_urls,
         "gh" = snapp_metadata$gh_urls)
}

#' Get heading information related to SNAP Shiny apps
#'
#' This function returns a list of headings related to SNAP Shiny apps.
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
#' snapp_titles("label")
snapp_titles <- function(type){
  switch(type,
         "label" = snapp_metadata$labels,
         "title" = snapp_metadata$titles,
         "sub" = snapp_metadata$subtitles)
}

#' Genrate SNAP apps showcase content
#'
#' This is a wrapper around \code{apputils::app_showcase}.
#'
#' @param drop character vector of any apps to exclude from showcase.
#'
#' @return a shiny fluidRow containing organized and stylized app image links for reference.
#' @export
#'
#' @examples
#' # not run
snapp_showcase <- function(drop = NULL){
  args <- list(
    app_url = c(
      paste0("https://uasnap.shinyapps.io/",
             c("rvdist", "climdist", "jfsp-v10", "ar5eval", "cc4liteFinal", "ex_leaflet", "nwtapp", "standage"))
    ),
    img_url = c(
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/rvdist.jpg",
      "https://raw.githubusercontent.com/leonawicz/dash/master/images/_climdist_small.png",
      "https://raw.githubusercontent.com/leonawicz/jfsp-archive/master/_jfsp_small.png",
      "https://raw.githubusercontent.com/leonawicz/ar5eval/master/_ar5eval_small.png",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/cc4liteFinal.jpg",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ex_leaflet.jpg",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/nwtapp.jpg",
      "https://raw.githubusercontent.com/leonawicz/agedist/master/_agedist_small.png"
    ),
    title = c(
      "Random variables", "CMIP5 Regional Climate", "Alaska Wildfire Projections", "Climate Model Analysis",
      "Communities & Climate", "Interactive Documents", "Northwest Territories",
      "Alaska Vegetation Changes"
    ),
    subtitle = c(
      "Probability distributions", "Full distributions", "ALFRESCO model output", "CMIP5 GCM evaluation",
      "Alaska & western Canada", "Leaflet + Shiny observers", "Climate projections",
      "Tree stand age projections"
    )
  )
  do.call(apputils::app_showcase, c(args, drop = list(drop)))
}

#' Wrapper around \code{apputils::contactinfo}
#'
#' This function makes a call to \code{apputils::contactinfo} using stored author contact info templates.
#' This override requires that \code{snaputils} be loaded after \code{apputils}, but this is the meaningful
#' order for loading these packages by their nature.
#' Otherwise, \code{snapputils::contactinfo} would have to be called explicitly.
#'
#' The user may optionally provide paths to SNAP, IARC and UAF logo images for branding.
#' The logos may be contained in the \code{www/} directory or could be at a web url.
#'
#' @param id character, name of author template to use. Available templates: \code{"leonawicz"}.
#' @param snap image url.
#' @param iarc image url.
#' @param uaf image url.
#'
#' @return a tag list.
#' @export
#'
#' @examples
#' \dontrun{contactinfo()}
contactinfo <- function(id = "leonawicz", snap, iarc, uaf){
  logo <- href <- NULL
  if(!missing(uaf)){
    logo <- c(logo, uaf)
    href <- c(href, "http://www.uaf.edu")
  }
  if(!missing(iarc)){
    logo <- c(logo, iarc)
    href <- c(href, "https://web.iarc.uaf.edu")
  }
  if(!missing(snap)){
    logo <- c(logo, snap)
    href <- c(href, "https://www.snap.uaf.edu")
  }

  if(id %in% c("leonawicz", "leonawicz-teach")){
    urls <- c(
      "https://leonawicz.github.io", "https://twitter.com/leonawicz",
      "http://www.linkedin.com/in/leonawicz", "https://www.youtube.com/user/StatisticsWithR",
      "https://plus.google.com/+StatisticsWithR/posts", "https://leonawicz.github.io/blog"
    )
    icons <- c("github", "twitter", "linkedin", "youtube", "google-plus", "rss-square")
    links <- list(paste0(
      '<a href="', urls,
      '" style="margin-right: 8px;" target="_blank"><i class="fa fa-',
      icons, ' fa-lg"></i></a>', collapse = ""))
    photo_leo <- "https://www.gravatar.com/avatar/5ab20ebc3829054f8af7b1ea4a317269?s=128"
    role_leo <- "Statistician | R Developer"
  }

  if(id == "leonawicz"){
    info <- apputils::contactinfo(
      name = "Matthew Leonawicz",
      role = role_leo, photo = photo_leo, logo = logo, href = href, links = links,
      heading = "Contact information",
      footnote = "For questions about this application, please email mfleonawicz@alaska.edu"
    )
  }

  if(id == "leonawicz-teach"){
    info <- apputils::contactinfo(
      name = "Matthew Leonawicz, MS",
      role = role_leo, photo = photo_leo, logo = logo, href = href, links = links,
      heading = "Instructor",
      footnote = c("Email: mfleonawicz@alaska.edu", "Phone: (XXX) XXX - XXXX")
    )
  }
  info
}
