#Aixo es per treballar desde el Rstudio

##script.dirname <- "/home/anrodriguez/simba/SIMBA-1.0.0/"

script.dirname <- "D:/TFGiTFM/TFGiM1718/tfgARodriguez/10O18_simba/simba/SIMBA-1.0.0/"

source(paste0(script.dirname,"init.R")) # Script per obrir tots els paquets necesaris
source(paste0(script.dirname,"ui.R")) # User Interface
source(paste0(script.dirname,"server.R")) #Server

options(warn=-1)

shinyApp(ui = ui, server = server, options = c(launch.browser=T))


