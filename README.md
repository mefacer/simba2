# T3LP: Protocolo de análisis de datos genéticos en Shiny.

Repositorio oficial del trabajo de final de grado.

### Descargar repositorio desde el navegador
Para descargar el repositorio, hay una opción en verde "Clone or download" para descargarlo en ".ZIP".

### Descargar repositorio desde la terminal
Para clonar el repositorio localmente, es necesario ejecutar la siguiente linea en la terminal de Linux:

```
git clone https://github.com/djangosee/TFGShinyApp.git
```

### Versión de R

*Importante:* Utilizar con la versión de R 3.5.3

##### Windows:

- Se puede descargar la versión de R [aquí](https://cran.r-project.org/bin/windows/base/old/)
- Se puede cambiar la versión de R des de RStudio en el menú Tools > Global Options > General

#### MacOS:

- Se puede descargar la vesión de R [aquí](https://cran.r-project.org/bin/macosx/el-capitan/base/)
- Para cambiar la versión se puede utilizar la aplicación [RSwitch](https://rud.is/rswitch/)

Más información [aquí](https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop)




### Cómo abrir la aplicación con Rstudio


1. Extraer el ".ZIP" en una carpeta local.
2. Ejecutar el proyecto `simba2.Rproj`


### Instalar packages

Ejecutar el código en el script `install_packages.R`. Esto puede tardar bastante (entre 1h y 2h).

*Nota Importante*: la función `load_versions` instala y carga los paquetes. La primera vez, puede tardar bastante rato en instalarlos todos. A la pregunta "Update all/some/none? [a/s/n]:" responder `n`. Si pregunta (posiblemente varias veces), 'Do you want to install from sources the package which needs compilation? (Yes/no/cancel)' responder `Yes`. 

### Lanzar aplicación

Ejecutar todas las lineas de código que encontramos en App.r. La aplicación se abrirá automáticamente.


## GIF **ExampleFile.xlsx**

<img src="GifReactionj.gif" />

Autor: Antonio Rodríguez Gómez
