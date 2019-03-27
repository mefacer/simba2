library(shiny)

script.dirname <- "/home/toni/TFGShinyApp/" #posa la ruta del repo

options(shiny.host = "192.168.1.15") #posa la teva ip
options(shiny.port = 1234) # numero para que cualquiera se acuerde
options(shiny.sanitize.errors="TRUE")
runApp("~/TFGShinyApp/App.R") #posa la ruta del App.R


