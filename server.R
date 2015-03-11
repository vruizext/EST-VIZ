shinyServer(function(input, output, session) {
  
   # load files ----
  flist <- sourceDirectory(path      = 'Functions/', 
                           encoding  = "UTF-8",
                           recursive = TRUE)
  
  # user interface ----
  output$settingsUI <- renderUI({
    
    if(is.null(input))
      return()
    
    estData <- collectData()
    if (is.null(estData)) 
      return()
    
    # AR 
    if (input$typeOfReport == "AR") {
      nam <- estData$Event.ID
      names(nam) <- estData$firms
      tagList(
        fluidRow(
          box(title  = "", 
              status = "primary", 
              width  = 3, 
              radioButtons(inputId = "arOrT", 
                           label   = "What do you want to plot?", 
                           choices = c("Abnormal Returns" = "AR", 
                                       "t-Statistics"     = "tstat")),
              hr(),
              selectizeInput(inputId  = "ARFirms", 
                             label    = "Firms", 
                             choices  = nam, 
                             multiple = TRUE)),
          box(title = "Visualization", 
              width = 9, 
              plotOutput("VisualizeAR"))
        ),
        fluidRow(
          box(title = "Data", 
              width = 12, 
              dataTableOutput("dtAR"))
        )
      )
    } else if (input$typeOfReport == "AAR") {
      idnam <- which(str_detect(estData$Grouping.Variable.N, "N[(]"))
      nam <- estData$Grouping.Variable.N[idnam - 1]
      tagList(
        fluidRow(
          box(title  = "", 
              status = "primary", 
              width  = 3, 
              radioButtons(inputId = "aarOrT", 
                           label   = "What do you want to plot?", 
                           choices = c("Abnormal Returns" = "AR", 
                                       "t-Statistics"     = "tstat")),
              hr(),
              selectizeInput(inputId  = "ARGroups", 
                             labels   = "Groups", 
                             choices  = nam,
                             multiple = TRUE)),
          box(title = "Visualization", 
              width = 9, 
              plotOutput("VisualizeAAR"))
        )
      )
    } else if (input$typeOfReport == "CAR") {
      
    }
  })
  
  # loader for Analysis report.csv
  getReport <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return()
    
    read.csv(inFile$datapath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  })
  
  # Loader for AR results.csv; AAR results.csv; CAAR results.csv; CAR results.csv
  getAR <- reactive({
    inFile <- input$file2
    
    if (is.null(inFile))
      return()
    
    read.csv(inFile$datapath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  })
  
  # 
  collectData <- reactive({
    report <- getReport()
    ar <- getAR()
    
    if (is.null(report) | is.null(ar)) 
      return()
    
    if (input$typeOfReport == "AR") {
      firms <- report$Firm[-1]
      names(firms) <- as.character(report$Event.ID[-1])
      ar <- cbind(firms = firms[as.character(ar$Event.ID)], ar)
      return(ar)
    } else if (input$typeOfReport == "AAR") {
      return(ar)
    }
  })
})


