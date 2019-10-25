
GetAnalysisDataVisualitzationSidebar<- sidebarPanel("",
          p("STEP1. Load the file (ExpressionGeneData)"),
          fileInput('file1',"Select main file:",accept=c(".xlsx"),buttonLabel = "Load File"),
          p("Example file (TFGShinyApp/www/data/)"),
          downloadButton("downloadData", "Download example file"),    
          hr(),
          uiOutput("NAs")
          )
          
GetAnalysisAnalizeSidebar <- sidebarPanel("",
          p("STEP2. Setting and configuration."),
          uiOutput("factorSelection"),
          uiOutput("covariableSelection"),
          uiOutput("IDSelection"),
          uiOutput("TissueSelection"),
          uiOutput("TissueCategory"),
          checkboxInput('use_logs', 'Use logs on values', value=TRUE),
          sliderInput("alpha","Significance Levels for ANOVA(alpha):",value=0.05,min=0.01,max=0.4,step=0.01),
          # sliderInput("noNaMin","Minimum number of valid treatments. If -1, apply formula", value=-1, min=-1, max=10, step=1),
          numericInput("noNaMin", label = "Minimum number of valid treatments. If -1, apply formula", value = -1, min=-1, step=1), 
          checkboxInput('del.badRows', 'Delete Bad Rows', value=TRUE),
          selectInput('language', 'Language', c('Catalan', 'English'), 'Catalan'),
          actionButton("Start", "Start Analysis"),
          hr()
          # conditionalPanel("input.Start>0",downloadButton("ExcelButton","Download Tables in .xlsx"))
           )


  



