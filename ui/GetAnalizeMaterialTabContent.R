source("ui/Analize/GetUIAnalysisLoadFile.R")

GetMaterialAnalizeTabContent <- tabPanel("Analize", GetAnalysisAnalizeSidebar,
    #Carreguem el sidebar del load file                 
    # conditionalPanel("input.Start>0",mainPanel("",
    conditionalPanel("output.show_panel",mainPanel("",
              p(HTML(paste("<b>","Gene Expression Matrix (gene x samples: first 6 rows and 6 columns):","</b>"))),
              checkboxInput("checkbox", label = "View all Data", value = F),
              withSpinner(tableOutput("tableHead"),color="#0b295b"),
              p(HTML(paste("<b>","ANOVA F-test","</b>"))),
              withSpinner(DT::dataTableOutput("tableMCA"),color="#0b295b"),
              br(),
              fluidRow(
                downloadButton("downloadMCA", "Download"), 
                actionButton("show1", "Theory of ANOVA",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
                actionButton("show2", "Theory of FDR",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
                actionButton("showi2", "Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ),
              hr(),
              textOutput("tukey_min"),
              p(HTML(paste("<b>","Table Tukey 1: Tukey-Kramer p-values","</b>"))),
              withSpinner(DT::dataTableOutput("tableTukey"),color="#0b295b"),
              downloadButton("downloadTukey", "Download"), 
              hr(),
              p(HTML(paste("<b>","Tukey Table 2: HSD","</b>"))),
              withSpinner(DT::dataTableOutput("tableTreatments"),color="#0b295b"),
              downloadButton("downloadTreatments", "Download"), 
              br(),
              fluidRow(
              actionButton("show3", "Theory of Tukey",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
              actionButton("showi3", "Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              # p(HTML(paste("<b>","Tukey: Post-hoc plot","</b>"))),
              # withSpinner(plotOutput("tukeyplot",height = 800,width="auto")),
              hr(),
              p(HTML(paste("<b>","Names and Legends","</b>"))),
              uiOutput('treatment_names'),
              withSpinner(plotOutput('treatment_legend'),color="#0b295b"),
              downloadButton('download_legend_treat','Download'),
              downloadButton('download_legend_functions','Download'),
              hr(),
              radioButtons("defCol", label = h4("ColourPicker"),
                           choices = list("Default colors" = 1, "Customize colors" = 2), 
                           selected = 1),
              conditionalPanel("input.defCol==2",uiOutput('colorSelector')),
              hr(),
              p(HTML(paste("<b>","PCA type I: variables=treatments means; cases=genes","</b>"))),
              withSpinner(plotOutput("pca"),color="#0b295b"),
              fluidRow(
              downloadButton("downloadPCA_a", "Download"),
              downloadButton("downloadPCA_b", "Download"),
              actionButton("show4", "Theory of PCA",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
              actionButton("showi4","Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              hr(),
              p(HTML(paste("<b>","PCA type II: variables=genes; cases=samples","</b>"))),
              withSpinner(plotOutput("pca2"),color="#0b295b"),
              fluidRow(
                downloadButton("downloadPCA2_a", '"Download'),
                downloadButton("downloadPCA2_b", '"Download'),
                actionButton("show4b", "Theory of PCA",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
                actionButton("showi4b","Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              hr(),
              p(HTML(paste("<b>","LinePlot: Mean gene expression by covariable","</b>"))),
              radioButtons("orderLine", label = h5("Order By (decreasing):"),
                           choices = list("Treatment" = 1, "Functions" = 2, "Both"= 3), 
                           selected = 1),
              conditionalPanel("input.orderLine!=2",uiOutput('treatSelector')),
              withSpinner(plotOutput("lineplot",height = 800, width = 800),color="#0b295b"),
                        actionButton("showi5", "Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              downloadButton("downloadLine", '"Download'),
              p(HTML(paste("<b>","HeatMap and Double Clustering"),"</b>")),
              hr(),
              withSpinner(plotOutput("heatmap",height = "850px"),color="#0b295b"),
              downloadButton("downloadHeatmap", "Download"),
              fluidRow(
                actionButton("show6", "Theory of HeatMap",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
                actionButton("showi6", "Interpretation Hint",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              value=2)
              ))

