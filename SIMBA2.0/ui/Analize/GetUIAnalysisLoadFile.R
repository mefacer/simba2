
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
          sliderInput("alpha","Significance Levels for ANOVA(alpha):",value=0.05,min=0.05,max=0.2,step=0.05),
          sliderInput("alphaFDR","FDR's alpha:",value=0.1,min=0.05,max=0.2,step=0.05),
          sliderInput("alphaTukey","Tukey's alpha",value=0.05,min=0.05,max=0.2,step=0.05),
          sliderInput("NAInput","Percentage of missing values to remove by treatment in each gene.",value=0.5,min=0,max=0.95,step=0.05),
          actionButton("Start", "Start Analysis"),
          hr(),
          conditionalPanel("input.Start>0",downloadButton("ExcelButton","Download Tables in .xlsx"))
           )


  



