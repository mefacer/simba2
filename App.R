#Aixo es per treballar desde el Rstudio

source('global.R') # Script per obrir tots els paquets necesaris
source("ui.R") # User Interface
source("server.R") #Server


options(warn=-1)

shinyApp(ui = ui, server = server, options = c(launch.browser=T))




