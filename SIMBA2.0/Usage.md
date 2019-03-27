## File format
This section details the load file requirements for the respective file type (**xlsx**).

- Load file requires header names.
- Treatment variable has to be integer ( T1 = 1 , T2 = 2 ,..., Tn = n ).
- Tissue variable has to exist.
- Don't transform NA to '0'.
- This shiny app doesn't load csv or txt formats.
- The first sheet is for main data.
- The second sheet is for genes functions. There are only 2 columns. The columns' names must be: "Gens" (1st column), "Funcions" (2nd column).

In www folder you can find a good formated file. If load files doesn't follow these requirements, the application won't work.
