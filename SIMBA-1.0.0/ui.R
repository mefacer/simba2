source("ui/GetUsageMaterialTabContent.R")
source("ui/GetAboutMaterialTabContent.R")
source("ui/GetAnalizeMaterialTabContent.R")


ui <- fluidPage(theme=shinytheme("flatly"), 
   title = "T3LP: Statistical software analysis for GeneExpression",
   tags$style(type="text/css", "body {padding-top: 70px;}"),
   navbarPage(title = div("T3LP", img(src = "https://github.com/djangosee/TFGShinyApp/blob/UserInterface/www/logo.png?raw=true", height = "70px", 
              style = "position: fixed; right: 10px;  top: -5px;")),
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
             HTML('<div style="width: 100%;">
                    <div style="position: relative; padding-bottom: 56.25%; padding-top: 0; height: 0;">
                    <iframe frameborder="0" width="1200" height="675" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;" src="https://view.genial.ly/5b794b55c900496607622291" type="text/html" allowscriptaccess="always" allowfullscreen="true" scrolling="yes" allownetworking="all"></iframe>
                    </div>
                    </div>'),
             hr(),
             includeMarkdown("Usage.md"))
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

  

