# LL-FP-ShinyAnalysisAPP
# Author: Shana Terai Snarrenberg
# Dec 2020

# TO RUN:
#
#     Click "Run App" in the upper R corner of this window
#
#

vzn = "1.3.3"
library(shiny)
library(shinyjs)
library(openxlsx)
# library(reticulate)
# source_python("loadTDdata.py")
source(paste("LLFPSA-fxns_v.", vzn, ".R", sep=""))
source(paste("LLFPSA-plot_v.", vzn, ".R", sep=""))


# Define UI ----
ui <- fluidPage(theme="style.css",  useShinyjs(),
                titlePanel("LL-FP-ShinyAnalysis"),
                sidebarLayout(
                  sidebarPanel(width=4,
                               fileInput("file1", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv",
                                                    ".Tdk")),
                               checkboxInput("rmvNaNs", "Remove missing data", TRUE),
                               checkboxInput("twof", "Two fluorophores", FALSE),
                               tags$hr(),
                               h4("Processing Options"),
                               fluidRow(
                                 column(width=6, 
                                        selectInput("downsamp", "Down sampling method", 
                                                    choices = list("Decimation" = "deci", "Binning" = "bin", "None" = FALSE), 
                                                    selected = 1)),
                                 column(width=6, 
                                        hidden(numericInput("ds_val", "Down sampling factor", value=100)))
                               ),
                               fluidRow(
                                 column(width=6, 
                                        selectInput("rec_type", "Recording type",
                                                    choices = list("Interval" = "int", "Continuous" = "cont"), 
                                                    selected = 1)),
                                 column(width=6, 
                                        hidden(numericInput("edge_trim", "Interval edge trimming (ms)", value=1000, step=100)))
                               ),
                               fluidRow(
                                 column(width=6, 
                                        selectInput("SMA", "Moving average filter",
                                                    choices = list("Apply" = TRUE, "None" = FALSE), 
                                                    selected = 1)),
                                 column(width=6, 
                                        hidden(numericInput("SMA_val", "Moving average window", value = 3, step = 1)))
                               ),
                               h4("Signal Correction"),
                               br(),
                               actionButton("runButton", h4("Run!")),
                               h4("Plot Options"),
                               checkboxInput("stim", "Plot stimulus info from FP_DataInfo.xlsx", TRUE)
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Data Analysis", 
                                         br(),
                                         h4(textOutput("rawdata_title")),
                                         tableOutput("rawdata_table"),
                                         htmlOutput("rawdata_text"),
                                         tags$b(textOutput("load_errtext")),
                                         br(),
                                         h4(textOutput("rundata_title")),
                                         tableOutput("rundata_table"),
                                         htmlOutput("rundata_text"),
                                         tags$b(textOutput("run_errtext")),
                                         tags$b(textOutput("twof_errtext")),
                                         tags$b(textOutput("FPinfo_errtext")),
                                         br(),
                                         hidden(downloadButton("excelDL","Download processed results"))
                                ),
                                tabPanel("Save Plots",
                                         br(),
                                         tags$b(textOutput("plot_errtext")),
                                         selectInput("plot_type", "Select plot",
                                                     width = "100%",
                                                     choices = list("Raw 465 & 405" = "raw",
                                                                    "Isosbestic fit" = "fit", 
                                                                    "Isosbestic fit removed" = "dFF",
                                                                    "Isosbestic fit removed & mean centered" = "dFF.c",
                                                                    "Isosbestic fit removed & exponential baseline fit removed" = "dFF.blr",
                                                                    "Isosbestic fit removed & exponential baseline fit removed & mean centered " = "dFF.blr.c",
                                                                    "465 mean centered" = "f465.c",
                                                                    "465 exponential baseline fit removed" = "f465.blr",
                                                                    "465 exponential baseline fit removed & mean centered" = "f465.blr.c",
                                                                    "Red - raw" = "fred",
                                                                    "Red mean centered" = "fred.c",
                                                                    "Red exponential baseline fit removed" = "fred.blr",
                                                                    "Red exponential baseline fit removed & mean centered" = "fred.blr.c"
                                                     ), 
                                                     selected = 1),
                                         plotOutput("selected_plot", height = 300),
                                         hidden(downloadButton("selected_plotDL","Download selected plot")),
                                         br(), br()
                                )
                    )
                  )
                )
)

# Define server logic ----
server <- function(input, output) {
  options(shiny.maxRequestSize=5000*1024^2)
  
  # Load data
  FPdata <- reactive({
    req(input$file1)
    if(tools::file_ext(input$file1)[1] == "csv"){
      if(input$twof){
        loadData_2f(input$file1$datapath, input$rmvNaNs)
      } else if(length(read.csv(input$file1$datapath, nrows=1)[1]) <= 7){
        loadData(input$file1$datapath, input$rmvNaNs)
      } else{
        FPdata <- NULL
      }
    } else{
      loadTDdata_py(input$file1$datapath)
    }
  })
  
  # Load FPdata_info
  FPdata_info <- reactive({
    req(input$file1, input$runButton)
    FPdata_info <- loadDatainfo("../FP_DataInfo.xlsx")
  })
  
  # Run data analysis
  FPdata_run <- reactive({
    req(input$file1, input$runButton)
    if(input$file1$name %in% FPdata_info()$Filename){
      if(input$twof){
        if(length(read.csv(input$file1$datapath, nrows=1))[1] > 7){
          try(Run_FPAnalysis(FPdata(), FPdata_info(), input$file1$name, input$downsamp, input$ds_val, 
                             input$rec_type, input$edge_trim, input$SMA, input$SMA_val, input$isos, 
                             input$center, input$blr, input$twof)
          )
        } else{
          FPdata_run <- NULL
        }
      } else{
        try(Run_FPAnalysis(FPdata(), FPdata_info(), input$file1$name, input$downsamp, input$ds_val, 
                           input$rec_type, input$edge_trim, input$SMA, input$SMA_val, input$isos, 
                           input$center, input$blr, input$twof)
        )
      }
    } else{
      FPdata_run <- NULL
    }
    
  })
  
  # Show/hide options
  observe({
    if(input$downsamp != FALSE){ shinyjs::show("ds_val") }
    else{ shinyjs::hide("ds_val") }
    if(input$rec_type == "int"){ shinyjs::show("edge_trim") }
    else{ shinyjs::hide("edge_trim") }
    if(input$SMA){ shinyjs::show("SMA_val") }
    else{ shinyjs::hide("SMA_val") }
    if(input$runButton){ 
      shinyjs::show("raw_plotDL")
      shinyjs::show("selected_plotDL")
      shinyjs::show("excelDL")
    } else{ 
      shinyjs::hide("raw_plotDL")
      shinyjs::hide("selected_plotDL")
      shinyjs::hide("excelDL")
    }
  })
  
  # Check for errors
  errtext1 <- reactive({
    req(FPdata())
    if(input$rec_type == "int" && length(get_recEnds(FPdata())) <= 1){
      errtext1 <-  "Are you sure this is an interval recording?"
    } else {
      errtext1 <- " "
    }
    return(errtext1)
  })
  errtext2 <- reactive({
    req(FPdata())
    if(input$stim && !(input$file1$name %in% FPdata_info()$Filename)){
      errtext2 <-  "Plot stimulus info from FP_DataInfo.xlsx: 
      Stimulus information not found. Please check record in FP_DataInfo.xlsx or uncheck this option."
    } else {
      errtext2 <- " "
    }
    return(errtext2)
  })
  errtext3 <- reactive({
    req(input$file1)
    if(!input$twof && (length(read.csv(input$file1$datapath, nrows=1))[1] > 7)){
      errtext3 <- "Is this a two fluorophore recording? Please check the two fluorophore option or
      check the file formatting."
    } else {
      errtext3 <- " "
    }
    return(errtext3)
  })
  errtext4 <- reactive({
    req(input$file1)
    if(input$twof && (length(read.csv(input$file1$datapath, nrows=1))[1] <= 7)){
      errtext4 <- "Are you sure this is this a two fluorophore recording? Please uncheck the two fluorophore option or
      check the file formatting."
    } else {
      errtext4 <- " "
    }
    return(errtext4)
  })
  errtext5 <- reactive({
    req(input$file1)
    if(!(input$file1$name %in% FPdata_info()$Filename)){
      errtext5 <- "Data cannot be processed without file information in FP_DataInfo.xlsx. Please double check the
      excel file."
    } else {
      errtext5 <- " "
    }
    return(errtext5)
  })
  
  # Output errors
  output$run_errtext <- renderText({
    errtext1()
  })
  output$plot_errtext <- renderText({
    errtext2()
  })
  output$load_errtext <- renderText({
    errtext3()
  })
  output$twof_errtext <- renderText({
    errtext4()
  })
  output$FPinfo_errtext <- renderText({
    errtext5()
  })
  
  # Display raw data
  output$rawdata_title <- renderText({
    req(input$file1)
    req(!is.null(FPdata()))
    "Raw Data"
  })
  output$rawdata_table <- renderTable({
    req(input$file1)
    req(!is.null(FPdata()))
    head(FPdata())},
    width = "50%", spacing = "xs", align = 'c', digits = 6
  )
  output$rawdata_text <- renderUI({
    req(input$file1)
    req(!is.null(FPdata()))
    Fs <- signif(getFs(FPdata()),2)
    Len <- signif(FPdata()$time[dim(FPdata())[1]]/60,2)
    str1 <- paste("Total length of data:", Len, "min")
    str2 <- paste("Sampling frequency:", Fs, " Hz")
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  # Display processed data
  output$rundata_title <- renderText({
    req(input$file1, input$runButton)
    "Data Processing"
  })
  output$rundata_table <- renderTable({
    req(!is.null(FPdata_run()))
    head(FPdata_run())
  }, width = "50%", spacing = "xs", align = 'c', digits = 6 
  )
  output$rundata_text <- renderUI({
    req(!is.null(FPdata_run()))
    Fs <- signif(getFs(FPdata_run()),2)
    Len <- signif(FPdata_run()$time[dim(FPdata_run())[1]]/60,2)
    str1 <- paste("Total length of data:", Len, "min")
    str2 <- paste("Sampling frequency:", Fs, " Hz")
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  # Generate plots
  selected_plot <- reactive({
    req(input$file1, input$runButton)
    req(!is.null(FPdata_run()))
    plotFPdata(FPdata_run(), FPdata_info(), input$file1$name, input$plot_type, input$stim)
  })
  
  # Display plots
  output$selected_plot <- renderPlot({
    req(!is.null(FPdata_run()))
    selected_plot()
  })
  
  # Plot download
  output$selected_plotDL <- downloadHandler(
    filename = function() {savefilename(
      FPdata_run(), input$rec_type, input$edge_trim, input$SMA, input$SMA_val, input$file1$name, 
      paste(input$plot_type, ".png", sep=""))},
    content = function(file) {
      ggsave(file, plot = selected_plot(), device="png", dpi = 300, width = 10, height = 4)
    }
  )
  
  # Excel download
  output$excelDL <- downloadHandler(
    filename <- function() {savefilename(
      FPdata_run(), input$rec_type, input$edge_trim, input$SMA, input$SMA_val, input$file1$name, "_proc.xlsx")
    },
    content <- function(file) {
      write.xlsx(FPdata_run(), file)
    }
  )
  
  }

# Run the app ----
shinyApp(ui = ui, server = server)

