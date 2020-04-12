list.of.packages <- c("caTools", "hexbin", 'dplyr', "plotly","codetools","writexl","openxlsx",
                      "FactoMineR","knitr","zoo","colourpicker",
                      "RColorBrewer","shinycssloaders","shinyjs",
                      "ggplot2","heatmaply","gplots","RCurl", "Biobase",
                      "shinythemes","DT", "readxl",
                      "shiny","shinymaterial","stringi", "data.table",
                      "agricolae", 'magrittr', 'tidyr', 'missMDA', 
                      'RColorBrewer', 'glue', 'httpuv', 'markdown', 'htmltools', 'htmlwidgets')


packs_load <- c(
  list.of.packages,
  "genefilter",
  "tools"
)


lapply(packs_load, require, character.only = TRUE, quietly = T)