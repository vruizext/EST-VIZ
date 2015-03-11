dashboardPage(
  dashboardHeader(title = "Est Viz"),
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem(text    = "Load Data", 
               icon    = icon("home"), 
               tabName = "tab_start"),
      menuItem(text    = "Visualization", 
               icon    = icon("line-chart"), 
               tabName = "tab_visualization"),
      menuItem(text    = "About", 
               icon    = icon("book"), 
               tabName = "tab_about")
    )
  ),
  # body
  dashboardBody(
    tabItems(
      # tab data
      tabItem(tabName = "tab_start",
              fluidRow(
                column(width = 3,
                       tabBox(width = NULL,
                              title = NULL,
                              tabPanel("Load Data", 
                                       tagList(fileInput('file1', 'Upload Report File',
                                                         accept = c(
                                                           'text/csv',
                                                           'text/comma-separated-values',
                                                           'text/tab-separated-values',
                                                           'text/plain',
                                                           '.csv',
                                                           '.tsv'
                                                         )
                                       ),
                                       radioButtons("typeOfReport", "Type of Result File", c("AR", "AAR"), inline=TRUE),
                                       fileInput('file2', 'Upload Result File',
                                                 accept = c(
                                                   'text/csv',
                                                   'text/comma-separated-values',
                                                   'text/tab-separated-values',
                                                   'text/plain',
                                                   '.csv',
                                                   '.tsv')
                                                 )
                                       )
                              )
                       )
                )
              )
      ),
      # Tab process capability
      tabItem(tabName = "tab_visualization",
              uiOutput("settingsUI")
      ),
      # about
      tabItem(tabName = "tab_about",
              helpText("Entwickelt von TTI GmbH - Abteilung TGU MUON-STAT; Version 0.1")
      )
    )
  )
)

