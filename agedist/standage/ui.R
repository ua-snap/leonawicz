library(shiny)
library(shinydashboard)
library(shinyBS)
library(rintrojs)
library(leaflet)

action_btn_style <- "margin: 10px 15px 10px 15px; width: 200px"
reg <- c(
  "DNR"="State Department of Natural Resources",
  "BLM"="Bureau of Land Management (BLM)",
  "DOD/DOE"="DOD and DOE",
  "FWS"="Fish and Wildlife Service (FWS)",
  "NPS"="National Park Service (NPS)",
  "CA, BC"="Government of British Columbia",
  "CA, PCA"="Government of Canada, Parks Canada Agency",
  "Yukon, DOE"="Government of Yukon, Department of Environment"
)
veg <- c("Deciduous", "Black Spruce", "White Spruce")

text_mean <- "Total area for each mean stand age bin is the percentage of the total vegetation cover area distribution mean
where stand age falls within each age bin given the corresponding stand age probability distribition."

text_ci <- "Stand age confidence bands are based on age variability in space as well as uncertainty in total vegetation cover area across Alfresco simulations.
Confidence limits are computed for the total vegetation cover area probability distribution and are used to bound mean stand age."

text_dist1 <- "Three example aggregate periods (2010-2039, 2040-2069, and 2070-2099) show nonparametric estimated probability density functions
for stand age.
Stand age distributions are estimated across space and simulation replicates at an annual temporal resolution for each vegetation class and spatial domain.
Total vegetation cover area probability distributions are similarly derived across simulations but not across space 
(total cover area is already spatially aggregated by definition)."

text_dist2 <- "These estimated annual probability distributions can be integrated over levels of other variables such as time to obtain marginal distributions.
For each period here, 30 annual distributions are integrated to show stand age densities at a coarser time scale.
These density plots are included to provide context regarding where mean stand age estimates and uncertainty in the other two plots are derived from."

css <- "https://gist.githubusercontent.com/leonawicz/24ed656f63d4a889ad7043bc5436a641/raw/050538f0c78616ac53a03ebebe9c256d33f9053f/shiny_app_styles.css"
.app_img_link <- function(app_url, img_url, title, subtitle, height=200){
  HTML(paste0(
    '<div class="img_link_wrap">
    <img class="img_app" src="', img_url, '" width="100%" height="', height, '"/>
    <a href="', app_url, '" style="color:white;" target="_blank"
    <div class="img_hover_layer">
    <div class="img_hover">
    <h3><p>', title, '</p></h3>
    <h4><p class="img_hover">', subtitle, '</p></h4>
    </div>
    </div>
    </a>')) # contextual, must remove a closing div with mutliple inline calls
}

app_img_links <- function(app_url, img_url, title, subtitle, height=200, min.width=300, max.width=400, col.width=4){
  x <- purrr::map(seq_along(app_url),
                  ~column(col.width, .app_img_link(app_url[.x], img_url[.x], title[.x], subtitle[.x], height),
                          style=paste0("min-width: ", min.width, "px; max-width: ", max.width, "px; padding:5px;")))
  fluidRow(x, style="padding: 10px;")
}

other_apps <- source("otherapps.R", local=TRUE)[[1]]
faq <- source("faq.R", local=TRUE)[[1]]

dashboardPage(
  dashboardHeader(
    title="Terrestrial protected areas stand age summary",
    titleWidth=450,
    tags$li(class="dropdown",
            tags$a(href="http://snap.uaf.edu", target="_blank",
                   tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="margin: 10px; padding: 0px;")
    )
  ),
  dashboardSidebar(
    introjsUI(),
    sidebarMenu(
      id="tabs",
      menuItem("Stand age", icon=icon("sliders"), tabName="age"),
      menuItem("About", icon=icon("info-circle"), tabName="about")
    ),
    actionButton("help", "Take tour", style=action_btn_style, class="btn-flat action-button btn-block", icon=icon("question-circle")),
    actionButton("map", "View map", style=action_btn_style, class="btn-flat action-button btn-block", icon=icon("globe"))
  ),
  dashboardBody(
    includeCSS(css),
    tabItems(
      tabItem(tabName="age",
        tabBox(
          tabPanel("Example distributions", plotOutput("plotdist", height="auto")),
          tabPanel("95% confidence", plotOutput("plotci", height="auto")),
          tabPanel("Mean", plotOutput("plotmean", height="auto")),
          id="tb", selected="Mean", title="Stand age summaries", width=12, side="right"
        ),
        fluidRow(
          column(2, selectInput("reg", "Area", reg, reg[1], multiple=TRUE, width="100%")),
          column(2, selectInput("veg", "Vegetation", veg, width="100%")),
          conditionalPanel("input.tb == 'Example distributions'",
            column(2, selectInput("fctby", "Facet by", c("Region"="Location", "Period"), width="100%"))),
          conditionalPanel("input.tb != 'Example distributions'",
            column(2, selectInput("x", "X-axis", c("Year", "Decade"), width="100%"))),
          column(2, selectInput("fctscales", "Axis scales", c("Fixed"="fixed", "Free"="free_y"), width="100%")),
          conditionalPanel("input.tb == 'Example distributions'",
            column(2, sliderInput("adj", "Density smoothing", 0.2, 2, 1, 0.2, width="100%"))),
          conditionalPanel("input.tb != 'Example distributions'",
            conditionalPanel("input.tb == 'Mean'",
              column(2, selectInput("pos", "Bars", c("Stacked"="stack", "Proportions"="fill"), width="100%"))
            ),
            column(2, sliderInput("span", "Series smoothing", 0, 1, 0, 0.1, width="100%"))
          )
        ),
        fluidRow(
          column(12,
            conditionalPanel("input.tb == 'Mean'", p(em(text_mean))),
            conditionalPanel("input.tb == '95% confidence'", p(em(text_ci))),
            conditionalPanel("input.tb == 'Example distributions'", p(em(text_dist1)), p(em(text_dist2)))
          )
        )
      ),
      tabItem(tabName="about",
        h2("About this application"),
        p("This app shows projected stand age for tree species in Alaska based on ALFRESCO model outputs.
          In addition to averages, confidence bands and probability distributions of stand age 
          across space and multiple model simulations are available.
          This provides insight into future uncertainty caputred by the model.", style="text-align:justify"),
        h2("Frequently asked questions"),
        faq,
        h2("Contact information"),
        HTML('
             <div style="clear: left;"><img src="https://www.gravatar.com/avatar/5ab20ebc3829054f8af7b1ea4a317269?s=128"
             alt="" style="float: left; margin-right:5px" /></div>
             <p>Matthew Leonawicz<br/>
             Statistician | useR<br/>
             <a href="https://leonawicz.github.io" target="_blank">Github.io</a> |
             <a href="http://blog.snap.uaf.edu" target="_blank">Blog</a> |
             <a href="https://twitter.com/leonawicz" target="_blank">Twitter</a> |
             <a href="http://www.linkedin.com/in/leonawicz" target="_blank">Linkedin</a> <br/>
             <a href="http://www.snap.uaf.edu/", target="_blank">Scenarios Network for Alaska and Arctic Planning</a>
             </p>'
        ),
        p("For questions about this application, please email mfleonawicz@alaska.edu")
      )
    )
  ),
  title="TPA stand age"
)
