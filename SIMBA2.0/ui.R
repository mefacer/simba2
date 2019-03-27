source(paste0(script.dirname,"/ui/GetUsageMaterialTabContent.R"))
source(paste0(script.dirname,"/ui/GetAboutMaterialTabContent.R"))
source(paste0(script.dirname,"/ui/GetAnalizeMaterialTabContent.R"))


ui <- fluidPage(theme=shinytheme("flatly"), 
   title = "SIMBA: Statistical Inference Methods for BioGeneExpression Analysis",
   tags$style(type="text/css", "body {padding-top: 70px;}"),
   navbarPage(title = "SIMBA",
              collapsible = T,
              position = "fixed-top",
    # tabPanel("Usage",uiOutput('markdown')),
    tabPanel("DataVisualitzation",GetAnalysisDataVisualitzationSidebar,
             mainPanel("",
                       h2("Main Database"),
                       hr(),
                       dataTableOutput('table'),
                       h2("Functions Database"),
                       hr(),
                       dataTableOutput('table2')
             )),
    GetMaterialAnalizeTabContent,
    tabPanel("Usage",
             includeMarkdown(paste0(script.dirname,"Usage.md")))
    #tabPanel("About",GetMaterialAboutTabContent())
  ))
  #Hauré de fer una funció apply per definir el entorn de les pagines
  #Inicialment només seria necesari fer 3 tabs
  ## About: Descripció del frontend, objectius i autor
  ## Parallax (para hacer bonito)
  ## .rmd
  ## Usage: Tutorial
  ## Pasos a seguir i protocol d'anàlisi
  ## .rmd
  ## Analisi: Resultats i obtenció del pdf.
  ## Load csv
  ## Plots, taules i cards
  ## Download pdf analisis
  ## input.Rnw (plantilla generica)

  

