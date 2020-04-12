
# remove all non base packages
# base_packs <- installed.packages( priority = "base" )[,1]
# packs_remove <- na.omit(sapply(installed.packages()[,1], function(pack){
#   if(!(pack %in% base_packs)){
#     return(pack)
#   }else{
#     NA
#   }
# }))
# remove.packages( packs_remove )

# View(installed.packages())

# Export versions
# write.table(installed.packages()[, c(1, 3)], 'versions.csv', quote=FALSE, sep=";", row.names=FALSE)

list.of.packages <- c("caTools", "hexbin", 'dplyr', "plotly","codetools","writexl","openxlsx",
                      "FactoMineR","knitr","zoo","colourpicker",
                      "RColorBrewer","shinycssloaders","shinyjs",
                      "ggplot2","heatmaply","gplots","RCurl", "Biobase",
                      "shinythemes","DT", "readxl",
                      "shiny","shinymaterial","stringi", "data.table",
                      "agricolae", 'magrittr', 'tidyr', 'missMDA', 
                      'RColorBrewer', 'glue', 'httpuv', 'markdown', 'htmltools', 'htmlwidgets')



install_all_packages <- function(){
  if(!('devtools' %in% installed.packages())){
    install.packages('devtools', type = "source")
  }
  library(devtools)
  
  install_if_missing <- TRUE
  
  pack_versions_list <- read.csv('versions.csv', sep=";")
  pack_versions <- list()
  for(i in 1:nrow(pack_versions_list)){
    pack_versions[[ as.character(pack_versions_list$Package[i]) ]] = as.character(pack_versions_list$Version[i])
  }
  
  ### Check current versions
  to_update <- c()
  failed_packages <- c()
  for(pack in list.of.packages){
    installed <- as.data.frame(installed.packages())
    pack_version <- as.character(pack_versions[[pack]])
    ind <- which(installed$Package == pack)
    needs_update <- FALSE
    if(length(ind) == 0 | length(pack_version) == 0){
      print(paste('Missing:', pack))
      cat('\n')
      needs_update <- TRUE
    }else{
      current_version <- as.character(installed$Version[ind])
      if(current_version != pack_version){
        print(paste0('Wrong Version:', pack))
        print(paste0('Current:', current_version))
        print(paste0('Necessary Version:', pack_version))
        cat('\n')
        needs_update <- TRUE
      }
    }
    if(needs_update == TRUE){
      to_update <- c(to_update, pack)
    }
    if(needs_update == TRUE & install_if_missing){
      print(paste0('### Installing ', pack))
      if(length(pack_version) > 0){
        res <- try(
          remotes::install_version(
            pack, 
            version=pack_version, 
            repos = "http://cran.us.r-project.org",
            upgrade="never"
          )
        )
        # if(class(res) == "try-error"){
        #   res <- try(
        #     install.packages(pack)
        #   )
        # }
      }else{
        res <- try(
          install.packages(pack)
        )
      }
      if(class(res) == "try-error"){
        failed_packages <- c(failed_packages, pack)
        stop()
      }  
    }
  }
  
  if( length(failed_packages) > 0 ){
    print('Failed Packages')
    print(failed_packages)
  }
  
  if(!"genefilter" %in% installed.packages()){
    source("https://bioconductor.org/biocLite.R")
    biocLite("genefilter")
  }
  
}

install_all_packages()

packs_load <- c(
  list.of.packages,
  "genefilter",
  "tools"
)

