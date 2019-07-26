#Arxiu per carregar tots els paquets

#Llistat de paquets

list.of.packages <- c("plotly","codetools","writexl","openxlsx",
                      "FactoMineR","knitr","zoo","colourpicker",
                      "RColorBrewer","shinycssloaders","shinyjs",
                      "ggplot2","heatmaply","gplots","Biobase","RCurl",
                      "genefilter","shinythemes","DT","tools","readxl",
                      "shiny","shinymaterial","stringi", "data.table", 
                      "agricolae", 'magrittr', 'tidyr', 'dplyr', 'missMDA', 
                      'RColorBrewer', 'glue', 'httpuv', 'markdown')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages())]
if(length(new.packages) > 0){
  install.packages(new.packages)
}

if(!"genefilter" %in% installed.packages()){
  source("https://bioconductor.org/biocLite.R")
  biocLite("genefilter")
}

lapply(list.of.packages, require, character.only = TRUE, quietly = T)

install_all_packages <- function(){
  for(package in list.of.packages){
    install.packages(package)
  }
  source("https://bioconductor.org/biocLite.R")
  biocLite("genefilter")
}
