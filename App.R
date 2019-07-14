#Aixo es per treballar desde el Rstudio

# install_all_packages()

source("init.R") # Script per obrir tots els paquets necesaris
source("ui.R") # User Interface
source("server.R") #Server

options(warn=-1)

shinyApp(ui = ui, server = server, options = c(launch.browser=T))



