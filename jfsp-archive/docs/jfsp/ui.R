library(shinycssloaders)
addResourcePath("res", snap_res())
ht <- "512px"

dashboardPage(skin = "red",
  dashboardHeader(
    title = "JFSP",
    tags$li(class = "dropdown",
            tags$a(href = "http://snap.uaf.edu", target="_blank",
                   tags$img(src = "res/snap_acronym_white.png", width="100%", height="30px"), style = "padding: 10px; margin: 0px;"))
  ),
  dashboardSidebar(
    use_apputils(),
    sidebarMenu(
      id = "tabs",
      menuItem("Management Cost", icon = icon("dollar"), tabName = "mc"),
      menuItem("Burn Area", icon = icon("fire", lib = "glyphicon"), tabName = "ba"),
      menuItem("Fire Size", icon = icon("fire", lib = "glyphicon"), tabName = "fs"),
      menuItem("Vegetation", icon = icon("tree-conifer", lib = "glyphicon"), tabName = "veg"),
      menuItem("Information", icon = icon("info-circle"), tabName = "info")
    ),
    div(hr(), h4("Global options"), style = "margin:10px;"),
    checkboxInput("by_rcp", "Condition on RCPs", TRUE),
    checkboxInput("by_tx", "Include treatment", FALSE),
    dashboard_footer("http://snap.uaf.edu/", "res/snap_white.svg", "SNAP Dashboards")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mc",
              uiOutput("mc_box"),
              fluidRow(
                column(3, div(selectInput("mc_domain", "Spatial domain", regions, width = "100%"), style = "height:260px;")),
                column(3, checkboxGroupInput("fmo", "Fire management options", fmos[2:5], inline = TRUE, width = "100%")),
                conditionalPanel("input.mc_domain === 'Alaska' && input.fmo == ''", column(3, checkboxInput("mc_obs", "Overlay mean historical cost", FALSE)))
              )
      ),
      tabItem(tabName = "ba",
              fluidRow(
                tabBox(
                  tabPanel("Variability", plotOutput("plot_bavar", height = "512px")),
                  tabPanel("Totals", plotOutput("plot_babox", height = ht)),
                  selected = "Totals", title = "Burn area totals and inter-annual variability", side = "right", width = 12, id = "tb_ba"
                )
              ),
              fluidRow(
                column(3, selectInput("ba_domain", "Spatial domain", c("Alaska (ALFRESCO)", "Full vs. Critical FMO"), width = "100%")),
                column(3, checkboxInput("log_bp", "Log scale box plots", TRUE)),
                conditionalPanel("input.ba_domain === 'Alaska (ALFRESCO)' && input.tb_ba === 'Variability'", 
                                 column(3, checkboxInput("basd_obs", "Overlay mean historical SD", FALSE)))
              )
      ),
      tabItem(tabName = "fs",
              fluidRow(
                box(plotOutput("plot_fs", height = ht), 
                    title = "Fire size distributions", width = 8),
                box(withSpinner(rglwidgetOutput("plot_fsrgl", width = "100%")), 
                    title = "Mean Fire size (Interactive 3D)", width = 4)
              ),
              fluidRow(
                column(3, checkboxInput("log_fs", "Log scale", TRUE))
              )
      ),
      tabItem(tabName = "veg",
              fluidRow(
                tabBox(
                  tabPanel("Veg map", img(src = "https://github.com/leonawicz/jfsp-archive/blob/master/plots/ak_cdratio_2000-2040.png?raw=true", width = "100%", height = "auto")),
                  tabPanel("Veg ratio", plotOutput("plot_cdratio", height = "512px")),
                  tabPanel("Burn area", plotOutput("plot_cdba", height = "512px")),
                  selected = "Burn area", title = "Alaska coniferous:deciduous ratios and burn area", side = "right", width = 12
                )
              )
      ),
      tabItem(tabName = "info",
              h2("About this application"),
              "Use the info tab as the landing tab???",
              #about_app,
              h2("Frequently asked questions"),
              h4("Placeholder"),
              h5("This is where"),
              h6("FAQ information goes..."),
              "Other widgets available but not shown.",
              #faq(faqs, bscollapse_args = list(id = "faq", open = "apps"), showcase_args = list(drop = "climdist")),
              contactinfo(snap = "res/snap_color.svg", iarc = "res/iarc.jpg", uaf = "res/uaf.png"), br()
      )
    )
  ),
  title = "JFSP"
)
