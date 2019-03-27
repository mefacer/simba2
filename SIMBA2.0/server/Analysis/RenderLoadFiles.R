
RenderLoadFiles <- function(input1){ # Get the upload file
    inFile <- input1

    if(is.null(inFile)) {
      return(NULL) 
    }else{
      
      validate(
        need(file_ext(inFile$name) %in% c(
          'xlsx'
        ), "Wrong File Format try again!"))

      read_xlsx(inFile$datapath,
                col_names = TRUE,sheet = 1)
    }
    

}