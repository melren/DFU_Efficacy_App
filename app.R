#.libPaths(c(normalizePath("./libs"), .libPaths()))
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(haven)
library(shinydashboard)
library(stringr)
library(DT)
library(Hmisc)
library(survminer)
library(effiplot)

#########################################################,
#### Application Components
#########################################################,
ui <- dashboardPage(
  dashboardHeader(
    title = "Study Visualizations",
    titleWidth = 270
  ),
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "home", icon = icon("home")),
      menuItem("Demographic Table", tabName = "demo", icon = icon("table")),
      menuItem("Efficacy Plots", icon = icon("area-chart"),
               menuSubItem("Time to Event Plots", tabName = "tte"),
               menuSubItem("Longitudinal Ulcer Area Plots", tabName = "long"),
               menuSubItem("Box Plots", tabName = "box"),
               menuSubItem("Additional Plots", tabName = "add")
      ),
      menuItem("Biomarker Exploration", tabName = "bm", icon = icon("medkit")),
      menuItem("Filter/Plot Options", icon = icon("gear"),
               startExpanded = TRUE,
               
               
               # Longitudinal plot options
               uiOutput("long_statShow"),
               uiOutput("long_stat_yvalShow"),
               uiOutput("long_sp_yvalShow"),
               
               # Box plot options
               uiOutput("box_xShow"),
               
               # BM plot options
               uiOutput("bm_anltShow"),
               uiOutput("bm_statShow"),
               
               # General plot options
               uiOutput("cohortCheck"),
               uiOutput("pooledChrtCheck"),
               uiOutput("uaStratCheck"),
               uiOutput("regionCheck"),
               uiOutput("popCheck"),
               br(),
               br(),
               br(),
               br()
      ),
      menuItem("Questions/Need Assistance?", icon = icon("question-circle"),
               br(),
               div(span(icon("user-circle"), "Melanie Ren (melanie.ren@gmail.com)", style = "padding-left:10px;"), style = "height:14px"),
               br()
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$script(type="text/javascript",'$(document).ready(function(){
                             $(".main-sidebar").css("height","100%");
                  $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow": "auto"})
                  })'),
      tags$style(
        # Fix widths of infoboxes with html since the function call is bugged
        '
        #sfchk, #rbchk, #dupchk, #scrnfchk {width:25%}
        .shiny-plot-output{height:80vh !important;}
        .downloadBtn{margin-left:15px;}
        ',
        HTML('
             /* logo */
             .skin-blue .main-header .logo {
             background: linear-gradient(to right, rgb(32, 193, 237), rgb(64,142,186));
             font-weight: bold;
             position: fixed;
             width: 270px;
             }
             
             /* logo when hovered */
             .skin-blue .main-header .logo:hover {
             background: linear-gradient(to right, rgb(32, 193, 237), rgb(64,142,186));
             }
             
             /* navbar (rest of the header) */
             .skin-blue .main-header .navbar {
             background: linear-gradient(to right, rgb(64,142,186),rgb(74, 109, 130));
             width: 100%;
             margin-top: -0.5px;
             position: fixed;
             }        
             
             .content {
             padding-top: 60px;
             }
             
             /* navbar when hovered */
             .skin-blue .main-header .navbar:hover {
             background: linear-gradient(to right, rgb(64,142,186),rgb(32, 193, 237));
             }    
             
             /* collapse button when hovered */
             .skin-blue .main-header .navbar .sidebar-toggle:hover {
             background-color: rgb(64,142,186);
             }
             
             /* main sidebar */
             .skin-blue .main-sidebar {
             background: linear-gradient(217deg, rgba(10,10,10, 0.9), rgba(100,100,100, 0) 80%),
             linear-gradient(127deg, rgba(64,142,186,0.9), rgba(64,142,186,0) 70.71%),
             linear-gradient(336deg, rgba(32, 193, 237,0.9), rgba(32, 193, 237,0) 70.71%);
             position: fixed;
             white-space: nowrap;
             overflow: visible;
             }
             
             
             /* active selected tab in the sidebarmenu */
             .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
             background-color: rgba(0,0,0,0.3);
             border-left-color: rgb(32, 193, 237);
             }
             
             
             /* other links in the sidebarmenu when hovered */
             .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
             background-color: rgba(0,0,0,0.3);
             border-left-color: rgb(32, 193, 237);
             color: #fffff;
             }
             
             .skin-blue .sidebar-menu>li>.treeview-menu {
             background-color: rgba(0,0,0,0.3);
             }
             
             /* Change backgroud color*/
             .content-wrapper, .right-side {
             background-color: #def5fc; /*(18,34,38);*/
             }
             ')
        )
        ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(status = "primary",
                    width = 3,
                    h4("Data Dependencies"),
                    code('none'), br(),
                    "The original data depends on snapshot data from the official study database. 
                    For this sample app, all data displayed are from a randomly generated dummy data set."
                ),
                box(width = 9,
                    status = "primary",
                    h4("About This App"),
                    p("This app was created as part of my internship projects at Genentech for the exploration of efficacy endpoints 
                      of a randomized phase Ib study. The indication for this study is Diabetic Foot Ulcers (DFU) and the 
                      endpoints here are related to amount of ulcer healing (size) compared to baseline."),
                    tags$li("This modified version of the app is only intended to be a sample to showcase the types of functionality implemented in the project."),
                    tags$li("The plots generated in this app use the ", tags$strong(code("effiplot")), 
                            " R package that I developed. More detail about this package can be found here:", tags$a(href = "https://github.com/melren/effiplot", "https://github.com/melren/effiplot")),
                    tags$li(tags$strong("Due to confidentiality agreements, none of the data is real except the cohort grouping.")),
                    tags$li("If errors for the plots appear, please open the Filter/Plot Options panel to refresh the input."),
                    p(),
                    p(span("Note:", style = "color: red"), "If the download plot button is not working, the plots can be downloaded from the right-click menu.")
                )
              )
      ),
      tabItem(tabName = "demo",
              fluidRow(
                box(status = "primary", width = 12,
                    uiOutput("demo_tbl")
                )
              )
      ),
      tabItem(tabName = "tte",
              fluidRow(
                tabBox(
                  id="tteTabs",
                  width = 12,
                  tabPanel(h4("Time to First Ulcer Closure"), value = "TFUC",
                           plotOutput("TFUC")
                  ),
                  tabPanel(h4("Time to Confirmed Ulcer Closure"), value = "TCUC",
                           plotOutput("TCUC")
                  )
                )
              ),
              fluidRow(downloadButton("downloadTTEPlot", label = "Download Plot", class="downloadBtn")),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Associated Data Records",
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    solidHeader = TRUE,
                    DT::dataTableOutput("tteTbl")
                )
              )
      ),
      tabItem(tabName = "long",
              fluidRow(
                tabBox(
                  id = "longtabs",
                  width = 12,
                  tabPanel(h4("Mean/Median"),value = "meanmed",
                           plotOutput("mmplot")),
                  tabPanel(h4("Spaghetti Plots"),value = "spaghetti",
                           plotOutput("spplot"))
                )
              ),
              fluidRow(downloadButton("downloadLongPlot", label = "Download Plot", class="downloadBtn")),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Associated Data Records",
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DT::dataTableOutput("longTbl")
                    
                )
              )
      ),
      tabItem(tabName = "box",
              fluidRow(
                tabBox(
                  id = "boxtabs",
                  width = 12,
                  tabPanel(h4("Ulcer Area at Week 6 and Week 12"),value = "AVAL",
                           plotOutput("uaplot")
                  ),
                  tabPanel(h4("% Decrease in Ulcer Area at Week 6 & Week 12"),value = "PDCR",
                           plotOutput("pdcrPlot")
                  )
                )
              ),
              fluidRow(downloadButton("downloadBoxPlot", label = "Download Plot", class="downloadBtn")),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Associated Data Records",
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DT::dataTableOutput("boxTbl")
                )
              )
              
      ),
      tabItem(tabName = "add",
              fluidRow(
                tabBox(
                  id = "addtabs",
                  width = 12,
                  tabPanel(h4("Heat Map"), value = "heat",
                           ## Outputs
                           plotOutput("heatplot")
                  ),
                  tabPanel(h4("Bubble Plot"), value = "bubble",
                           plotOutput("bubbleplot")
                  ),
                  tabPanel(h4("Waterfall Plot"), value = "wf",
                           "There is usually a waterfall plot here. However, since this plot was too study-specific, 
                           I decided not to create dummy data for it and have left this tab blank."
                           #plotOutput("wfplot")
                  )
                )
              ),
              fluidRow(downloadButton("downloadAddPlot", label = "Download Plot", class="downloadBtn")),
              br(),
              fluidRow(
                uiOutput("gradienttbl")
              )
      ),
      tabItem(tabName = "bm",
              fluidRow(
                tabBox(
                  id = "bmtabs",
                  width = 12,
                  tabPanel(h4("Subject-level"),value = "subj",
                           plotOutput("bmSubPlot")
                  ),
                  tabPanel(h4("Cohort-level"),value = "cohort",
                           plotOutput("bmChtPlot")
                  )
                )
              ),
              fluidRow(downloadButton("downloadBmPlot", label = "Download Plot", class="downloadBtn")),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Associated Data Records",
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DT::dataTableOutput("bmTbl")
                )
              )
              
      )
    )
        )
    )

server <- function(input, output) {
  #########################################################,
  #### Sidebar
  #########################################################,
  
  ## Outputs
  output$cohortCheck <- renderUI({
    if(input$tabs != "add" | (input$tabs == "add" & input$addtabs!= "wf")){
      checkboxGroupInput("cohortSelection",
                         h5("Included Cohorts"),
                         choices = list("Non-Infected Placebo A+B+E" = "NON-INFECTED PLACEBO A+B+E",
                                        "Cohort A" = "COHORT A",
                                        "Cohort B" = "COHORT B",
                                        "Cohort E" = "COHORT E",
                                        "Infected Placebo C+D" = "INFECTED PLACEBO C+D",
                                        "Cohort C+D" = "COHORT C+D"),
                         selected = c("NON-INFECTED PLACEBO A+B+E","COHORT A","COHORT B","COHORT E","INFECTED PLACEBO C+D","COHORT C+D"))
    }
    
  })
  
  output$pooledChrtCheck <- renderUI({
    if(!("INFECTED PLACEBO C+D" %in% input$cohortSelection) & !("COHORT C+D" %in% input$cohortSelection)){
      if(input$tabs != "add" | (input$tabs == "add" & input$addtabs!= "wf")){
        checkboxInput("pooledSelection", "Pooled Cohort A+B+E Grouping", value = FALSE)
      }
    }
  })
  
  output$uaStratCheck <- renderUI({
    if(input$tabs %in% c("home","tte","long","box", "add","bm")){
      if(input$tabs != "add" | (input$tabs == "add" & input$addtabs!= "wf")){
        selectInput("uaStratSelection",
                    h5("Baseline Ulcer Area"),
                    choices = list("All" = 1,"<= 2cm^2" = "<= 2 (cm^2)", "> 2cm^2"= "> 2 (cm^2)"),
                    selected = 1)
      }
    }
  })
  
  output$regionCheck <- renderUI({
    if(input$tabs %in% c("home","tte","long","box","add")){
      if(input$tabs != "add" | (input$tabs == "add" & input$addtabs!= "wf")){
        selectInput("regionSelection",
                    h5("Filter by Region"),
                    choices = list("All" = 1,"Europe" = "EU", "United States"= "US"),
                    selected = 1)
      }
    }
  })
  
  output$popCheck <- renderUI({
    if(input$tabs == "demo"){
      selectInput("popSelection",
                  h5("Filter Population"),
                  choices = list("All Subjects" = 1,"Safety" = "SAFFL", "Intent-to-Treat"= "ITTFL"),
                  selected = 1)
    }
  })
  
  ## Logic
  getDataSet <- reactive({
    if(input$tabs == "demo") {
      return(effiplot::ASL)
    } else if (input$tabs == "tte") {
      return(effiplot::ATE)
    } else if (input$tabs %in% c("long","box","add")) {
      return(effiplot::AZA)
    } else if(input$tabs == "bm") {
      return(effiplot::BM)
    } else {
      return(NULL)
    }
  })
  
  subsetData <- reactive({
    data <- getDataSet() %>%
      dplyr::mutate(CHRT = CHRT) %>%
      dplyr::filter(CHRT %in% input$cohortSelection) 
    
    if(!is.null(input$pooledSelection)){
      if(input$pooledSelection) {
        data <- data %>%
          dplyr::mutate(CHRT = CHRTGRP)
      }
    }
    
    if(!is.null(input$uaStratSelection)){
      if(input$uaStratSelection !=1) {
        data <- data %>%
          dplyr::filter(BASEUA == input$uaStratSelection)
      }
    }
    
    if(!is.null(input$regionSelection)){
      if(input$regionSelection != 1){
        data <- data %>%
          dplyr::filter(REGION == input$regionSelection)
      }
    }
    
    if(!is.null(input$popSelection)){
      if(input$popSelection != 1){
        data <- data %>%
          dplyr::filter(data[[input$popSelection]]=="Y")
      }
    }
    
    return(data)
  })
  
  ################################,
  #### Demographics
  ################################,
  output$demo_tbl <- renderUI({
    data <- subsetData() %>%
      dplyr::mutate(CHRT = factor(CHRT))
    p("This table is currently unavailable as it depends on an internal package with many other dependencies. 
      I will update the sample app at a later date once I solve this package dependency issue.")
    #as_html(
    #  t_summary(x=data[,c("BAGE", "AGE65", "SEX", "RACE", "WEIGHT", "BASE", "BASEUA","HBA1C")], 
    #            col_by = data$CHRT, total = "TOTAL")
    #)
  })
  
  ################################,
  #### Time to Event Plots
  ################################,
  
  ## Outputs
  output$downloadTTEPlot <- downloadHandler(
    filename = function() {paste(input$tteTabs, '.png', sep='')},
    content = function(file) {
      ggsave(file, plot = print(tteplot()), device = "png", width = 12, height = 8)
    }
  )
  
  output$TFUC <- renderPlot({
    tteplot()
  })
  
  output$TCUC <- renderPlot({
    tteplot()
  })
  
  output$tteTbl <- DT::renderDataTable({
    datatable(ttetbl(),
              rownames=FALSE,
              extensions = c('Buttons', 'ColReorder'),
              options = list(scrollX = TRUE, dom = 'Bfrtip', pageLength = 15, buttons = list('colvis'), colReorder = TRUE), filter = "top")
  })
  
  ## Logic
  tteplot <- reactive({
    event_param = input$tteTabs
    chrt = "CHRT"
    input_data <- subsetData() %>%
      dplyr::mutate(CHRT = factor(CHRT))
    
    #define dynamic title
    event = ifelse(event_param=="TFUC", "First", "Confirmed")
    title = paste("Time to", event, "Ulcer Closure")
    
    effiplot::tteplot(data = input_data, group_by = "CHRT", param = input$tteTabs,
                      reverse = TRUE, title = title,
                      xlab = "Time (Days)", ylab = "Proportion of patients \nwith Ulcer Closure", size = "large")
  })
  
  ttetbl <- reactive({
    data <- subsetData() %>%
      dplyr::filter(PARAMCD == input$tteTabs) %>%
      dplyr::select(PARAM, USUBJID, REGION, CHRT, CHRTGRP, AVAL, CNSR, BASE, BASEUA)
    
    return(data)
  })
  
  ################################,
  #### Longitudinal Plots
  ################################,
  
  ## Outputs
  output$downloadLongPlot <- downloadHandler(
    filename = function() {paste(input$longtabs, '.png', sep='')},
    content = function(file) {
      ggsave(file, plot = print(longCurrPlot()), device = "png", width = 12, height = 8)
    }
  )
  
  output$long_statShow <- renderUI({
    if(input$tabs == "long" & input$longtabs == "meanmed"){
      selectInput("long_statSelection",
                  h5("Mean or Median"),
                  choices = list("Mean" = "mean", "Median" = "median"),
                  selected = "mean")
    }
  })
  
  output$long_stat_yvalShow <- renderUI({
    if(input$tabs == "long" & input$longtabs == "meanmed"){
      selectInput("long_stat_yvalSelection",
                  h5("Choose y-variable"),
                  choices = list("Ulcer Area" = "AVAL", "% Decrease" = "PDCR"),
                  selected = "AVAL")
    }
  })
  
  output$long_sp_yvalShow <- renderUI({
    if(input$tabs == "long" & input$longtabs == "spaghetti"){
      selectInput("long_sp_yvalSelection",
                  h5("Choose y-variable"),
                  choices = list("Individual Ulcer Area" = "ua", "Absolute Change in Ulcer Area" = "abs", "Percent Change in Ulcer Area" = "pchg"),
                  selected = "ua")
    }
  })
  
  output$mmplot <- renderPlot({
    longplot()
  })
  
  output$longTbl <- DT::renderDataTable({
    datatable(longdata(),
              rownames=FALSE,
              extensions = c('Buttons', 'ColReorder'),
              options = list(scrollX = TRUE, dom = 'Bfrtip', pageLength = 15, buttons = list('colvis'), colReorder = TRUE), filter = "top")
  })
  
  output$spplot <- renderPlot({
    spPlot()
  })
  
  ## Logic
  longplot <- reactive({
    input_data <- subsetData()
    y_val=ifelse(is.null(input$long_stat_yvalSelection),"AVAL",input$long_stat_yvalSelection)
    stat= ifelse(is.null(input$long_statSelection),"mean",input$long_statSelection)
    
    # Define dynamic title and labels
    title = ""
    if(y_val == "PDCR"){
      title = paste(capitalize(stat), "of %Decrease in Ulcer Area")
    } else {
      units = ifelse(y_val=="AVAL","(cm^2)", "AUC (cm^2 x day)")
      title = paste(capitalize(stat), "of Ulcer Area", units)
    }
    
    ylab = ""
    if(y_val == "PDCR"){
      ylab = "%Decrease in Ulcer Area"
    } else {
      units = ifelse(y_val=="AVAL","(cm^2)", "AUC (cm^2 x day)")
      ylab = paste("Ulcer Area", units)
    }
    
    pdcr_cap = ifelse(y_val =="PDCR","%Decrease=max(0, -%change).","")
    cap = paste(pdcr_cap,"Missing value is imputed by carrying last non-missing observation forward.")
    
    effiplot::lineplot(data = input_data, x = "ADY", y = y_val, group_by = "CHRT",
                       stat = stat, markers = c(43,85), ylab = ylab, title = title, caption = cap, size = "large")
    
  })
  
  longdata <- reactive({
    data <- subsetData() %>%
      dplyr::select(USUBJID, CHRT, CHRTGRP, REGION, AVAL, BASE, BASEUA, CHG, PCHG, PDCR,ADY)
    colnames(data) <- c("Subject ID", "Cohort", "Cohort Group", "Region", "Ulcer Area (cm^2)",
                        "Baseline UA (cm^2)", "Baseline UA Category",
                        "Change in UA", "% Change in UA", "% Decrease in UA", "Analysis Day")
    return(data)
  })
  
  spPlot <- reactive({
    input_data = subsetData()
    type = ifelse(is.null(input$long_sp_yvalSelection),"ua",input$long_sp_yvalSelection)
    
    plotdata<-input_data %>%
      dplyr::mutate(EPOCH=factor(EPOCH,
                                 levels=c("SCREENING","TREATMENT","OBSERVATION PERIOD")))
    if(type == "ua"){
      plotdata <- plotdata %>%
        dplyr::mutate(logval = AVAL + 0.01)
    }
    if(type == "abs"){
      plotdata <- plotdata %>%
        dplyr::mutate(logval = abs(CHG) + 0.01)
    }
    if(type == "pchg") {
      plotdata <- plotdata %>%
        dplyr::mutate(logval = PCHG + 100)
    }
    
    # Define dynamic plot labels based on type
    title = "Individual Ulcer Area (cm^2)"
    ylab = "Log scale Ulcer Area (cm^2)"
    if(type=="abs"){
      title = "Absolute Change in Ulcer Area (cm^2)"
      ylab = "Log scale Change from Baseline Ulcer Area (cm^2)"
    }
    if(type=="pchg"){
      title = "Percent Change in Ulcer Area"
      ylab = "Log scale %Change from Baseline Ulcer Area"
    }
    
    effiplot::lineplot(data = plotdata, x = "ADY", y = "logval", group_by = "USUBJID", facet = "CHRT",
                       log_y = TRUE, symb = "EPOCH", ylab = ylab, title = title,
                       caption = "Missing value is imputed by carrying last non-missing observation forward.", size = "large")
  })
  
  longCurrPlot <- reactive({
    if(input$longtabs == "meanmed"){
      return(longplot())
    }else{
      return(spPlot())
    }
  })
  
  ################################,
  #### Box Plots
  ################################,
  
  ## Outputs
  output$downloadBoxPlot <- downloadHandler(
    filename = function() {paste(input$boxtabs, '.png', sep='')},
    content = function(file) {
      ggsave(file, plot = print(boxPlot()), device = "png", width = 12, height = 8)
    }
  )
  
  output$box_xShow <- renderUI({
    if(input$tabs == "box") {
      selectInput("box_xSplitSelection",
                  h5("Choose x-variable"),
                  choices = list("Cohort" = "CHRT", "Region" = "REGION", "Baseline Ulcer Area"= "BASEUA"),
                  selected = "CHRT")
    }
    
  })
  
  output$uaplot <- renderPlot({
    boxPlot()
  })
  
  output$pdcrPlot <- renderPlot({
    boxPlot()
  })
  
  output$boxTbl <- DT::renderDataTable({
    datatable(boxdata(),
              rownames=FALSE,
              extensions = c('Buttons', 'ColReorder'),
              options = list(scrollX = TRUE, dom = 'Bfrtip', pageLength = 15, buttons = list('colvis'), colReorder = TRUE), filter = "top")
  })
  
  ## Logic
  boxPlot <- reactive({
    input_data = subsetData()
    
    y_lab = ifelse(input$boxtabs=="PDCR","%Decrease in ","")
    units = ifelse(input$boxtabs=="PDCR",""," (cm^2)")
    title=paste0("Boxplot of ", y_lab,"Ulcer Area at Week 6 and Week 12 (eCRF)")
    ylab = paste0(y_lab,"Ulcer Area", units)
    
    plotdata<-input_data %>%
      dplyr::filter(ADY %in% c(43,85)) %>%
      dplyr::mutate(daylabel=ifelse(ADY==43,"Week 6 (Day = 43)","Week 12 (Day = 85)")) %>%
      dplyr::mutate(daylabel=factor(daylabel,levels=c("Week 6 (Day = 43)","Week 12 (Day = 85)"))) %>%
      dplyr::mutate(uabase_ordered=factor(BASEUA,levels=c("<= 2 (cm^2)","> 2 (cm^2)")))
    
    effiplot::boxplot(
      data = plotdata, group_by = input$box_xSplitSelection, y = input$boxtabs, color_by = "uabase_ordered",
      facet = "daylabel", title = title, xlab = "", ylab = ylab, size = "large", show_n = FALSE
    )
  })
  
  boxdata <- reactive({
    data <- subsetData() %>%
      dplyr::select(USUBJID, CHRT, CHRTGRP, REGION, EPOCH, AVAL, BASE, BASEUA, CHG, PCHG, PDCR, ADY)
    colnames(data) <- c("Subject ID", "Cohort", "Cohort Group",
                        "Region", "Epoch", "Ulcer Area (cm^2)","Baseline UA (cm^2)", "Baseline UA Category",
                        "Change in UA", "% Change in UA", "% Decrease in UA", "Analysis Day")
    return(data)
  })
  
  ################################,
  #### Additional Plots
  ################################,
  
  ## Outputs
  output$downloadAddPlot <- downloadHandler(
    filename = function() {paste(input$addtabs, '.png', sep='')},
    content = function(file) {
      ggsave(file, plot = print(addPlot()), device = "png", width = 12, height = 8)
    }
  )
  
  output$heatplot <- renderPlot(
    gradientPlot()
  )
  
  output$bubbleplot <- renderPlot(
    gradientPlot()
  )
  
  output$gradientTbl <- DT::renderDataTable(
    datatable(gradientdata(),
              rownames=FALSE,
              extensions = c('Buttons', 'ColReorder'),
              options = list(scrollX = TRUE, dom = 'Bfrtip', pageLength = 15, buttons = list('colvis'), colReorder = TRUE), filter = "top")
    
  )
  
  output$wfplot <- renderPlot(
    wfPlot()
  )
  
  output$gradienttbl <- renderUI({
    if(input$addtabs != "wf"){
      box(width = 12,
          title = "Associated Data Records",
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          DT::dataTableOutput("gradientTbl")
      )
    }
  })
  
  ## Logic
  gradientPlot <- reactive({
    type = ifelse(is.null(input$addtabs),"heat",input$addtabs)
    input_data <- subsetData()
    
    uniqueid_d85 <- input_data %>% 
      dplyr::filter(ADY==85) %>% 
      dplyr::arrange(CHRT,PDCR) %>% 
      dplyr::group_by(CHRT) %>%
      dplyr::mutate(xn=row_number()) %>% 
      dplyr::select(USUBJID,CHRT,PDCR,xn)
    
    uniqueid_d43_d85 <- input_data %>% 
      dplyr::filter(ADY==43) %>% 
      dplyr::arrange(CHRT,PDCR) %>% 
      dplyr::rename(day43pdcr=PDCR) %>% 
      dplyr::full_join(uniqueid_d85,by=c("USUBJID","CHRT")) %>% 
      dplyr::arrange(CHRT,PDCR,day43pdcr) %>%  #sort by d85 pdcr then d43 pdcr
      dplyr::group_by(CHRT) %>%
      dplyr::mutate(xn=as.factor(row_number()))
    
    plot_data <-input_data %>%
      dplyr::left_join(uniqueid_d43_d85 %>% 
                         dplyr::select(USUBJID,xn),
                       by=c("USUBJID","CHRT"))  %>%
      dplyr::mutate(patnum=USUBJID) %>%
      dplyr::mutate(RPDCR = -PDCR)
    
    effiplot::grdplot(
      type = type, data = plot_data, x = "xn", y = "ADY", facet = "CHRT", ord = "PDCR",
      ord_lbl = "patnum", reverse=TRUE, ncol = 3, xlab = "Patients", leglab = "Ulcer Area % Decr from BL",
      title = "% Decrease in Ulcer Area", size = "large"
    )
  })
  
  gradientdata <- reactive({
    data <- subsetData() %>%
      dplyr::select(USUBJID, CHRT, CHRTGRP, REGION, EPOCH, AVAL, BASE, BASEUA, CHG, PCHG, PDCR,ADY)
    colnames(data) <- c("Subject ID", "Cohort", "Cohort Group",
                        "Region", "Epoch", "Ulcer Area (cm^2)","Baseline UA (cm^2)", "Baseline UA Category",
                        "Change in UA", "% Change in UA", "% Decrease in UA", "Analysis Day")
    return(data)
  })
  
  wfPlot <- reactive({
    input_data <- AZA %>%
      dplyr::filter(CHRTINFT %in% c("NON-INFECTED PLACEBO A+B+E", "COHORT E")) %>%
      dplyr::mutate(pat=paste("pat_",as.integer(substring(USUBJID,16,19)),sep="")) %>%
      dplyr::group_by(USUBJID) %>%
      dplyr::ungroup() %>%
      dplyr::select(pat,ADY,AVAL) %>%
      dplyr::group_by(ADY) %>%
      tidyr::spread(pat,AVAL) %>%
      dplyr::mutate(pat_5022=ifelse(ADY==85,0,pat_5022),
                    pat_5027=ifelse(ADY==85,0,pat_5027),
                    pat_5030=ifelse(ADY==85,3.72,pat_5030),
                    pat_5026=ifelse(ADY==85,1.68,pat_5026),
                    pat_5023=ifelse(ADY==85,0,pat_5023),
                    pat_5019=ifelse(ADY==85,0,pat_5019),
                    pat_5024=ifelse(ADY==85,0.74,pat_5024),
                    pat_5021=ifelse(ADY==85,0,pat_5021)) %>%
      dplyr::filter(ADY==85) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ADY) %>%
      tidyr::gather(pat,val) %>%
      dplyr::mutate(impute_flag=ifelse(grepl("5022|5027|5030|5026|5023|5019|5024|5028|5021",pat),1,0)) %>%
      dplyr::right_join(AZA %>%
                          dplyr::filter(ADY<=85 & CHRTINFT %in% c("NON-INFECTED PLACEBO A+B+E", "COHORT E")) %>%
                          dplyr::mutate(pat=paste("pat_",as.integer(substring(USUBJID,16,19)),sep="")) %>%
                          dplyr::group_by(USUBJID,pat,CHRTINFT) %>%
                          dplyr::mutate(pchg=-round(100*CHG/BASEUA,2)) %>%
                          dplyr::mutate(pchg=ifelse(row_number()==1 & CHG==0,NA,pchg)) %>%
                          dplyr::summarise(maxpchg=max(pchg,na.rm=T)) %>%
                          dplyr::select(USUBJID,pat,CHRTINFT,maxpchg),
                        by="pat") %>%
      dplyr::mutate(maxpchg=ifelse(grepl("5022|5027|5023|5019|5021",pat),100,maxpchg)) %>%
      dplyr::mutate(CHRT_impute=ifelse(impute_flag==1 , paste("Imputed",CHRTINFT),paste(CHRTINFT)))
    
    
    ggplot(input_data, aes(x=reorder(pat, -maxpchg,median), y=maxpchg, fill=CHRT_impute)) +
      facet_wrap(~CHRTINFT,scale="free_x",ncol=2)+
      scale_fill_manual(values = c("grey","red1",  "blue1", "grey"),name="Cohort") +
      labs(x = "Subject ID", y = "Best % Change from Baseline Ulcer Area") +
      theme_bw() %+replace%
      
      theme(strip.text = element_text(size=14),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(angle=90, hjust = 1),
            axis.ticks.x=element_blank(),
            axis.text = element_text(size=12),
            axis.title.x = element_text(face="bold", angle=0, size=14),
            axis.title.y = element_text(face="bold",angle=90, size=14),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom",
            legend.key = element_rect(fill = "white", 0.1)) +
      scale_y_continuous(limits=c(-40, 100), breaks=seq(-40, 100, by=10)) +
      coord_cartesian(ylim = c(-40, 100)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))
    
  })
  
  addPlot <- reactive({
    if(input$addtabs == "wf"){
      return(wfPlot())
    }else{
      return(gradientPlot())
    }
  })
  
  ################################,
  #### Biomarker Exploration
  ################################,
  
  ## Outputs
  output$downloadBmPlot <- downloadHandler(
    filename = function() {paste(input$bmtabs, '.png', sep='')},
    content = function(file) {
      ggsave(plot = print(bmPlot()), filename = file, device = "png", width = 12, height = 8)
    }
  )
  
  output$bm_anltShow <- renderUI({
    if(input$tabs == "bm" ){
      selectInput("bm_anltSelection",
                  h5("Analyte"),
                  choices = list("S100A12" = "S100A12", "Calprotectin" = "Calprotectin", "HSA"= "HSA"),
                  selected = "CHRT")
    }
  })
  
  output$bm_statShow <- renderUI({
    if(input$tabs =="bm" & input$bmtabs == "cohort"){
      selectInput("bm_statSelection",
                  h5("Mean or Median"),
                  choices = list("Mean" = "Mean", "Median" = "Median"),
                  selected = "mean")
    }
  })
  
  output$bmTbl <- DT::renderDataTable(
    datatable(bmData(),
              rownames=FALSE,
              extensions = c('Buttons', 'ColReorder'),
              options = list(scrollX = TRUE, dom = 'Bfrtip', pageLength = 15, buttons = list('colvis'), colReorder = TRUE), filter = "top")
    
  ) 
  
  output$bmSubPlot <- renderPlot({
    bmSub()
  })
  
  output$bmChtPlot <- renderPlot({
    bmChrt()
  })
  
  ## Logic
  bmData <- reactive({
    data <- subsetData() %>%
      dplyr::filter(ANALYTE == input$bm_anltSelection)
    
    if(input$bmtabs == "subj"){
      data <- data %>%
        dplyr::select(USUBJID, ANALYTE, CHRT, ADY, AVAL, BASE, CHG, PCHG, BASEUA)
      colnames(data) <- c("Subject ID", "Analyte", "Cohort", "Analysis Day", "Lab Value", "Baseline Value",
                          "Change from Baseline", "% Change", "Baseline Ulcer Area")
    } else {
      data <- data %>%
        dplyr::select(USUBJID, ANALYTE, CHRT, ADY, PCHG) %>%
        dplyr::group_by(ANALYTE, CHRT, ADY) %>%
        dplyr::mutate(meanPCHG = mean(PCHG, na.rm = TRUE),
                      medPCHG = median(PCHG, na.rm = TRUE)) %>%
        dplyr::arrange(ANALYTE, CHRT, ADY) %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-PCHG)
      colnames(data) <- c("Subject ID", "Analyte","Cohort", "Analysis Day", "Mean % Change",
                          "Median % Change")
    }
    return(data)
  })
  
  bmSub <- reactive({
    type = input$bm_anltSelection
    input_data <- subsetData() %>%
      dplyr::filter(ANALYTE == type)
    
    title = paste("Percent Change in", type)
    ylab = "%Change from Baseline Measurement"
    
    effiplot::lineplot(data = input_data, x = "ADY", y = "AVAL", group_by = "USUBJID", aggr = "USUBJID", facet = "CHRT",
                       ylab = ylab, title = title, size = "large")
  })
  
  bmChrt <- reactive({
    type = input$bm_anltSelection
    stat = ifelse(is.null(input$bm_statSelection), "Mean", input$bm_statSelection)
    input_data <- subsetData() %>%
      dplyr::filter(ANALYTE == type)
  
    title = paste(stat, "Percent Change in", type, "by Cohort")
    ylab = paste(stat, "%Change from Baseline Measurement")
    
    effiplot::lineplot(data = input_data, x = "ADY", y = "PCHG", stat = tolower(stat), group_by = "CHRT", aggr = "USUBJID",
                       ylab = ylab, title = title, size = "large")
    
  })
  
  bmPlot <- reactive({
    if(input$bmtabs == "subj"){
      return(bmSub())
    }else{
      return(bmChrt())
    }
  })
}

shinyApp(ui, server)