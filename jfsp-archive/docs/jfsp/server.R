library(jfsp)
library(dplyr)
library(tidyr)
fs <- filter(firesize, Tx == "Status quo") %>% group_by(Decade, FS) %>% summarise(Freq = mean(Freq)) %>% ungroup() %>% complete(Decade, FS)
x <- unique(fs$Decade)
y <-  unique(fs$FS)
ylog <- log(y)
f01 <- function(x) (x - min(x, na.rm = TRUE))/max(x - min(x, na.rm = TRUE), na.rm = TRUE)
m <- matrix(f01(fs$Freq), nrow = length(x), byrow = TRUE) 
clrs <- colorRampPalette(c("cornflowerblue", "orange", "firebrick1"))(30)

costdec <- readRDS(url("https://s3.amazonaws.com/leonawicz/fire/jfsp/mgmtcost/costdec_2020-2099.rds")) %>%
  select(-Group) %>% group_by(Region, Tx, RCP, FMO, Decade) %>% 
  summarise(`5th percentile` = mean(`5th percentile`),
            Mean = mean(Mean), `95th percentile` = mean(`95th percentile`)) %>% ungroup()

pt <- 4
txt <- 26

shinyServer(function(input, output){
  
  by_rcp <- reactive(input$by_rcp)
  by_tx <- reactive(input$by_tx)
  alaska <- reactive(input$ba_domain == "Alaska (ALFRESCO)")
  log_bp <- reactive(input$log_bp)
  log_fs <- reactive(input$log_fs)
  fs_3d_y <- reactive(if(log_fs()) ylog else y)
  fs_3d_ylab <- reactive(ifelse(log_fs(), "Fire size Log(acres)", "Fire size (acres)"))
  dcost <- reactive({ 
    x <- filter(costdec, Region == input$mc_domain) 
    if(is.null(input$fmo)) filter(x, FMO == "Total") %>% select(-FMO) else filter(x, FMO %in% input$fmo)
  })
  costrows <- reactive(ifelse(length(input$fmo) < 3, 1, 2))
  obs_cost <- reactive(input$mc_obs && input$mc_domain == "Alaska" && (is.null(input$fmo) || input$fmo == ""))
  obs_basd <- reactive(input$basd_obs && input$ba_domain == "Alaska (ALFRESCO)" && input$tb_ba == "Variability")

  output$plot_cost <- renderPlot({
    p <- jfsp_plot("cost_dec", 2020:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt, 
                   obs = obs_cost(), dashtype = c("ff", "44"), x = dcost())
    suppressMessages( p + ggplot2::scale_x_discrete(labels = paste0(seq(2020, 2090, by = 10))) )
  }, height = function(){ ifelse(costrows() == 1, 512, 800) })

  output$mc_box <- renderUI({
    ht <- ifelse(costrows() == 1, "512px", "800px")
    x <- input$mc_domain
    if(x %in% fmz) x <- names(fmz)[match(x, fmz)]
    box(plotOutput("plot_cost", height = ht), 
        title = paste(x, "management cost"), width = 12)
  })
  
  output$plot_babox <- renderPlot({
    jfsp_plot("ba_box", 1950:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt, 
              alaska = alaska(), log = input$log_bp)
  })
  
  output$plot_bavar <- renderPlot({
    jfsp_plot("ba_sd", 1950:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt,
              breaks = seq(1960, 2090, by = 10), alaska = alaska(), n = 30, 
              continuous = TRUE, dashtype = c("ff", "44"), obs = obs_basd())
  })
  
  output$plot_fs <- renderPlot({
    p <- jfsp_plot("fs_box", 2020:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt, log = log_fs())
    suppressMessages( p + ggplot2::scale_x_discrete(labels = paste0(seq(2020, 2090, by = 10))) )
  })
  
  output$plot_fsrgl <- renderRglwidget({
    try(rgl.close())
    rgl.viewpoint(theta = 210, phi = 90)
    persp3d(x, fs_3d_y(), m, col = clrs[cut(m, 30)], ticktype = "detailed", xlab = "Decade", ylab = fs_3d_ylab(), zlab = "Probability", lit = FALSE, axes = FALSE)
    axes3d(edges = c("x+-", "y--", "z"))
    rglwidget()
  })
  
  output$plot_cdba <- renderPlot({
    jfsp_plot("cdba", 1950:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt,
              breaks = c(seq(1950, 2090, by = 25), 2099))
  })
  
  output$plot_cdratio <- renderPlot({
    jfsp_plot("cdratio", 1950:2099, by_rcp = by_rcp(), by_tx = by_tx(), text_size = txt, pt_size = pt,
              breaks = c(seq(1950, 2090, by = 10), 2099))
  })
  
})
