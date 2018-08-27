function(request){
  dashboardPage(
    dashboardHeader(title="Polygons selectable?"),
    dashboardSidebar(
      actionButton("btn_modal_data", "Data filter", icon("sliders"),
        style=action_btn_style, class="btn-flat action-button btn-block")
    ),
    dashboardBody(
      bsModal("modal_data", "Data filter", "btn_modal_data", size="large",
              leafletOutput("Map", width="100%"),
              selectInput("regions", "Region", choices=regions, selected="", multiple=TRUE, width="100%"),
              checkboxInput("flammable", "Show flammable region", TRUE, width="100%")
      )
    ),
    title="Leaflet polygons"
  )
}
