source("server/Analysis/RenderLoadFiles.R")
source("server/Analysis/GetTukeyPlot.R")
source("server/Analysis/PCAStats.R")

server <- function(input, output,session) {
  ##### 
  #Provador
  ####
  provador=FALSE
  if(provador){
    input <- list()
    newData <-  read_xlsx("/Users/aleixrvr/Google Drive/AKC/Entities/UAB/simba_docs/data/negCtrolComparison_Francesc.xlsx",col_names = TRUE,sheet = 1)
    functions <- read_xlsx("/Users/aleixrvr/Google Drive/AKC/Entities/UAB/simba_docs/data/negCtrolComparison_Francesc.xlsx",col_names = TRUE,sheet = 2)
    functions <- functions[,1:2]
    input$factors <- c("Sample ID","Treatment","Tissue")
    input$covariables <- c("Treatment")
    input$Tissue <- c("Tissue")
    input$tissuecat <- c("lleum")
    #input$alphaTukey <- 0.1
    input$NAInput <- 0.5
    input$id <- "Sample ID"
  }
  
  
  ######
  #
  # Leer los datos como primera acción
  #
  ######
  
  read_data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    if(file_ext(inFile$name)=="csv"){
      validate(
        need(file_ext(input$file1$name) %in% c(
          'csv'
        ), "Wrong File Format try again!"))
      table1 <- read.csv2(inFile$datapath,header = T,sep = ",")
      return(table1)
    }else{
      validate(
        need(file_ext(input$file1$name) %in% c(
          'xlsx'
        ), "Wrong File Format try again!"))
      table1 <- read_xlsx(inFile$datapath,col_names = TRUE,sheet = 1)
      return(table1)
    }
  })
  
 
  
  #####
  #
  # Leer las funciones de los genes
  #
  ####
  get_gene_functions <- reactive({
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    if(file_ext(inFile$name)=="xlsx"){
      validate(
        need(file_ext(input$file1$name) %in% c(
          'xlsx'
        ), "Wrong File Format try again!"))
      table1 <- read_xlsx(inFile$datapath,col_names = TRUE,sheet = 2)
      table1 <- table1[,1:2]
      return(table1)
    }
  })
  
  ####
  #
  # Filtrar NA's, SELECTOR NA'S
  #
  ####
  
  output$NAs <- renderUI({
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    div(p(paste0("Total rows: ",nrow(read_data()))),
        p(paste0("Total columns: ", ncol(read_data()))),
        p(paste0("Number of NA in database: ",sum(apply(read_data(),1,function(x) sum(is.na(x)))))))
  })
  
  
  
  ####
  #
  # Selectores (Uno para los factores, uno para el subset, uno para la covariable); alfa slectors 
  #
  ####
  output$NAinput <- renderPrint({ input$NAinput })
  output$factorSelection<- renderUI({
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    selectizeInput("factors","Select Factors:",
                   choices = c(colnames(read_data())),selected=NULL, multiple = TRUE)})
  output$TissueSelection <- renderUI({
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    selectizeInput("Tissue","Select Tissue variable:",choices = c(input$factors),selected=F, multiple = FALSE)})
  output$IDSelection <- renderUI({
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    selectizeInput("id","Select id variable:",choices = c(input$factors),selected=F, multiple = FALSE)})
  output$TissueCategory <- renderUI({
    datTiss<- read_data()
    conditionalPanel(condition = "input.Tissue",
                     validate(need(input$factors,"Select Tissue")),
                     selectizeInput("tissuecat","Select Tissue's category:",choices = datTiss[[input$Tissue]],selected=F, multiple = FALSE))})
  output$covariableSelection<- renderUI({
    validate(need(input$factors,"Select factors of dataset"))
    selectizeInput("covariables","Select Treatment:",choices = c(input$factors),selected=F, multiple = FALSE)})
  output$alpha <- renderPrint({ input$alpha })
  
  
  
  ####
  #
  # Tabla global
  #
  ####
  gestioNA <- function( new_data, treatment_column, remove0=TRUE, del.badRows=TRUE, noNaMin=-1){
    ## columns containing factors
    factor_columns <- input$factors
    treatment_column <- input$covariables
    ## change 0 to NA, if exist
    if (remove0==TRUE){
      new_data[!is.na(new_data)&new_data==0] <- NA  ## .............!!!
    } 
    
    ## delete columns all NA's!
    nacol <- apply(new_data,2,function(x) sum(is.na(x)))
    if( sum( nacol==nrow(new_data) ) >0 ){ 
      columnes_eliminar <- which(nacol==nrow(new_data), useNames=TRUE) 
      new_data <- new_data[, -columnes_eliminar]
    }  
    
    new_data %>% 
      select(-factor_columns) ->
      new_data_no_factor 
    
    ## delete rows all NA's!
    narow <- apply(new_data_no_factor, 1, function(x) sum(is.na(x)))  # suma de NA per files
    if( sum( narow == ncol( new_data_no_factor ) )>0 ){ 
      files_eliminar <- which( narow == ncol( new_data_no_factor ), useNames=TRUE ) 
      new_data <- new_data[-files_eliminar, ]
    }  
    
    ## delete rows with 50% or more NA's!  (opcional: del.badRows==TRUE)
    if( del.badRows==TRUE ){ 
      condition  <- narow >= floor(0.50*ncol(new_data_no_factor))   # narrow >= numgens/2
      whichcond  <- which(condition==TRUE,useNames=TRUE)  
      if(length(whichcond)>0) new_data<-subset(new_data,condition==FALSE)   
    }
    
    ## delete columns (gens) with a too small number of valid replicates
    ### in one or more treatments
    ## default threshold: integer part (number of replicates /2)
    if(noNaMin < 0){
      noNaMin <- floor(max(table(new_data[, treatment_column]))/2)        
    } 
    
    ## delete cols s.t. number of valid replicates in some treatment is < noNaMin
    llista <- split(new_data, new_data[, treatment_column])
    noNASxTrac <- sapply(llista, function(df){
      apply(df, 2, function(v){
        sum(!is.na(v))
      })
    })
    noNASxTrac <- t(noNASxTrac)
    
    eliminar <- which( apply(noNASxTrac< noNaMin, 2, sum) > 0, useNames=TRUE)
    if(length(eliminar) > 0){
      new_data <- new_data[-eliminar]      
    }
    return(new_data)
  }
  
  
  get_new_data <- eventReactive(input$Start,{
    new_data <- read_data() %>% as.data.frame() 
    new_data <- subset(new_data, new_data[,input$Tissue]==input$tissuecat) 
    new_data <- gestioNA(new_data, input$covariables, input$del.badRows, input$noNaMin) 
    
    colnames(new_data) %>% 
      setdiff(input$factors) %>% 
      setdiff(input$covariables) ->
      gene_cols
    
    for( col in gene_cols ){
      new_data[[col]] <- log10(new_data[[col]])
    }
    
    rownames(new_data) <- new_data[,input$id] 
    new_data
  })
  
  data_expression <- eventReactive(input$Start,{
    newDat <- get_new_data()
    if(provador==T){newDat <- newData}
    newDataFact <- newDat[,input$factors]
    colnames(newDataFact) <- input$factors
    df <- data.frame(newDataFact[,input$covariables],
                     row.names=rownames(newDataFact))
    colnames(df) <- as.character(input$covariables)
    idx <- match(input$factors, names(newDat))
    idx <- sort(c(idx-1, idx))
    data.idx <- newDat[,-idx]
    data.idx2 <- apply(data.idx,2,function(x) as.numeric(x))
    nw <- data.idx2
    rownames(nw) <- rownames(data.idx)
    colnames(nw) <- colnames(data.idx)
    #nw <- subset(newData, colnames(newData) %in% input$factors)
    Expression <- ExpressionSet(as.matrix(t(nw)),phenoData = AnnotatedDataFrame(data=df))
    Expression
  })
  
  data_expression_new <- eventReactive(input$Start,{
    data_set <- get_new_data()
    if(provador==T){newDat <- newData}
    data_covars <- data_set[,input$covariables, drop=FALSE]
    
    sel_cols <- setdiff(colnames(data_set), input$factors)
    
    data_set[, sel_cols] %>%
      t %>% 
      as.matrix %>%
      ExpressionSet(phenoData = AnnotatedDataFrame(data=data_covars))
  })
  
 
  #Load file function server
  output$table <-renderDataTable({read_data()})
  output$table2 <- renderDataTable({get_gene_functions()})
  ####
  #
  # Tabla gene x samples
  #
  ####
  
  
  output$tableHead <- renderTable({    
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select all factors of dataset"))
    validate(need(input$covariables,"Select covariable"))
    if(input$checkbox==F){
      exprs(data_expression())[1:6,1:6]
    }else if(input$checkbox){
      exprs(data_expression())
    }
  },rownames=T)
  
  ####
  #
  # Tabla Ftests
  #
  ####
  
  significatius <- reactive({
    
    new_data <- get_new_data()
    gene_cols <- setdiff(colnames(new_data), input$factors)
    new_data_gene <- new_data[, gene_cols]
    treatment <- as.factor(new_data[, input$covariables])
    
    statistic<-apply(new_data_gene, 2, function(x){ 
      summary(aov(x~treatment))[[1]]['F value'][1,1]
    })
    p.value<-apply(new_data_gene,2, function(x){
      summary(aov(x~treatment))[[1]]['Pr(>F)'][1,1]
    })
    sem_error<-apply(new_data_gene, 2, function(x){ 
      summary(aov(x~treatment))[[1]]['Mean Sq'][2,1]
    })
    sample_size <- apply(new_data_gene, 2, function(x){ 
      cbind(treatment, x) %>% 
        na.omit %>% 
        .[, 'treatment'] %>% 
        table %>% 
        max
    })
    sem <- sqrt(sem_error/sample_size)
    
    data.frame(Genes=colnames(new_data_gene), statistic, p.value, sem, stringsAsFactors = FALSE) %>% 
      mutate(p.BH=p.adjust(p.value, "BH" )) %>% 
      combine_genes_with_funcions ->
      result_data
    
    return(result_data)
  })
    
  significatius_old <- reactive({
    functions <- get_gene_functions()
    data_expressions <- data_expression_new() 
    if(provador==T){data_expressions <- Expression}
    tt=rowFtests(data_expressions,as.factor(pData(data_expressions)[,input$covariables]))
    p.BH = p.adjust(tt[,"p.value"], "BH" )
    tt <- cbind(tt,p.BH)
    tt <- na.omit(tt)
    func<- functions[functions$Gens %in% rownames(tt),]
    rownames(tt) <- paste0(func$Funcions,"_",func$Gens)
    tt
  })
  
  output$tableMCA<- DT::renderDataTable({
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select all factors of dataset"))
    validate(need(input$covariables,"Select covariable"))
    
    pvaluesTable <- significatius() %>% select(-sem)
    colnames(pvaluesTable) <- c("Genes", "Contrast Statistic", "P-value", "P-value(FDR)")
    
    pvaluesTable <- datatable(pvaluesTable) %>%
      formatStyle(
        colnames(pvaluesTable), 
        valueColumns = "P-value",
        backgroundColor = styleInterval(input$alpha,c("#b5f2b6","white")))%>%
      formatRound(columns=colnames(pvaluesTable), digits=4)
    pvaluesTable
  })
  
  output$downloadMCA <- downloadHandler(
    filename = function() {
      paste("table_multiple_comparisons_analysis.csv", sep = "")
    },
    content = function(file) {
      dataMCA <- significatius()
      dataMCA$p.value <- format(dataMCA$p.value,4)
      dataMCA$p.BH <- format(dataMCA$p.BH,4)
      colnames(dataMCA) <- c("Contrast Statistic", "P-value", "P-value(FDR)")
      
      dataMCA$Names <- row.names(dataMCA)
      dataMCA <- dataMCA[, c("Names", "Contrast Statistic", "P-value", "P-value(FDR)")]
      write.csv(dataMCA, file, row.names = FALSE, quote = FALSE)
    }
  )
  
  
  ####
  #
  # Tabla Tukey Post-hoc
  #
  ####
  Tukey_test<- function(data_expression){
    Tuk <- list()
    expressions <- exprs(data_expression)
    treatments <- as.factor(pData(data_expression)[, 1])
    
    for(i in 1:nrow(expressions)){
      Tuk[[i]] <- TukeyHSD(aov(expressions[i,]~ treatments))
      names(Tuk)[[i]] <- rownames(expressions)[i]
    }
    return(na.omit(Tuk))
  }
 
  combine_genes_with_funcions <- function(data_set, gene_column_name = 'Genes'){
    gene_functions <- get_gene_functions()
    colnames(gene_functions)[1] <- gene_column_name
    
    data_set %<>% left_join(gene_functions, by=gene_column_name)
    data_set[, gene_column_name] <- paste(data_set$Funcions, data_set[, gene_column_name], sep='_')
    data_set$Funcions <- NULL
    data_set
  }
  
  calculate_table_Tukey <- reactive({
    data_expression_new() %>% 
      Tukey_test() %>% 
      lapply(function(x) as.data.frame(t(x$treatments[,4, drop=FALSE]))) -> 
      tukey_table
    
    tukey_table %>% 
      rbindlist(fill=TRUE) %>% 
      as.data.frame()  ->
      treats_pvalues
    
    treats_pvalues <- cbind(Genes = names(tukey_table), treats_pvalues)
    row.names(treats_pvalues) <- NULL
    
    treats_pvalues <- combine_genes_with_funcions(treats_pvalues)
    
    significatius() %>% 
      filter(p.value < input$alpha) %>% 
      select(Genes) %>% 
      left_join(treats_pvalues, by='Genes')
    
  })
  
  
  output$tableTukey <- DT::renderDataTable({
    treats_pvalues <- calculate_table_Tukey()
    datatable(treats_pvalues) %>%
      formatRound(columns=colnames(treats_pvalues), digits=4)
  })
  
  output$downloadTukey <- downloadHandler(
    filename = function() {
      paste("table_tukey.csv", sep = "")
    },
    content = function(file) {
      treats_pvalues <- calculate_table_Tukey()
      write.csv(treats_pvalues, file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ######
  #
  # Tukey table: Significant Groups
  #
  #####
  
  Tukey_letters<- reactive({
    data_expression <- data_expression_new()
    Tukey_comparisons <- list()
    gene_expressions <- exprs(data_expression)
    treatments <- as.factor(pData(data_expression)[,1])
    
    for(row_n in 1:nrow(gene_expressions)){
      valid_data = sum(is.na(gene_expressions[row_n,])) < length(gene_expressions[row_n,])-2
      if( valid_data ){
        aov_model <- aov(gene_expressions[row_n,]~ treatments)
        comparisons <- HSD.test(aov_model, trt ='treatments', alpha=input$alpha)$groups
        comparisons$treatment <- rownames(comparisons)
        rownames(comparisons) <- NULL
        comparisons <- comparisons[, c('treatment', 'groups')]
        Tukey_comparisons[[row_n]] <- comparisons
        names(Tukey_comparisons)[[row_n]] <- rownames(gene_expressions)[row_n]
      }
    }
    return(Tukey_comparisons)
  })
  
  calc_treatment_means <- reactive({
    new_data <- get_new_data()
    gene_cols <- setdiff(names(new_data), input$factors)
    sel_cols <- c(input$covariables, gene_cols)
    new_data <- as.data.table(new_data[, sel_cols])
    
    new_data %>% 
      melt(id.vars=input$covariables, measure.vars = gene_cols) ->
      melted_data
    setnames(melted_data, input$covariables, 'treatment')
    
    melted_data %>% 
      .[, .(mean_treatment = mean(na.omit(value))), by= .(variable, treatment)] %>% 
      dcast(variable~treatment, value.var='mean_treatment') ->
      treatment_means
    setnames(treatment_means, 'variable', 'Genes')
    
    treatment_means
  })
  
  output$tableTreatments <- DT::renderDataTable({
    treatment_means <- calc_treatment_means()
    tukey_letters <- Tukey_letters()
    
    significatius() %>% 
      select(Genes, p.value, sem) %>% 
      mutate(sem = round(sem, digits=4)) %>% 
      mutate(p.value = round(p.value, digits=4)) ->
      genes_sign
    
    format_tukey_letters(tukey_letters, treatment_means) %>% 
      combine_genes_with_funcions %>% 
      left_join(genes_sign) %>% 
      datatable(escape = FALSE)
  })
  
  remove_subs <- function(values){
    values %>% 
      gsub('<sup>', '', .) %>% 
      gsub('</sup>', '', .) 
  }
  
  output$downloadTreatments <- downloadHandler(
    filename = function() {
      paste("table_tukey.csv", sep = "")
    },
    content = function(file) {
      treatment_means <- calc_treatment_means()
      tukey_letters <- Tukey_letters()

      format_tukey_letters(tukey_letters, treatment_means) %>%
        combine_genes_with_funcions %>% 
        sapply(remove_subs) %>% 
        write.csv(file, row.names = FALSE, quote = FALSE)
    }
  )

  
  format_tukey_letters <- function(tukey_letters, treatment_means, digits=4){
    tukey_letters_table <- do.call(rbind, tukey_letters)
    rownames(tukey_letters_table) %>% 
      strsplit(".", fixed = TRUE) %>% 
      do.call(rbind, .) %>% 
      .[, 1] ->
      tukey_letters_table$Genes  
    
    treatment_means %>% 
      gather('treatment', 'value', -Genes) %>% 
      left_join(tukey_letters_table, by=c('Genes', 'treatment')) %>% 
      mutate(groups = as.character(groups)) %>% 
      mutate(groups = replace_na(groups, '')) ->
      table_res
    
    table_res %>% 
      mutate(value_int=paste(round(value, digits=digits), "<sup>", groups, "</sup>", sep="")) %>% 
      mutate(value=ifelse(groups=='', round(value, digits=digits), value_int)) %>% 
      mutate(value_int=NULL) %>% 
      mutate(groups=NULL) %>% 
      spread(treatment, value) 
  }
  
  
  
    
  ######
  #
  # Tukey plots
  #
  #####
  
  output$tukeyplot<- renderPlot({NULL
    # newData <- read_data()
    # l.dat.pre.data <- GetTukeyList() 
    # functions <- get_gene_functions()
    # # ff.plot(funcg=functions$Funcions,genes = functions$Funcions, l.dat.pre=l.dat.pre.data, 
    # #                         treat=input$covariables, grup=input$Tissue, alf=input$alphaTukey, 
    # #                         a=0.3,     # desplaçament de l'inici dels rectangles
    # #                         eps=0.2,   # y del plot a nivell x=0
    # #                         incy=0.4,  # alçada rectanglets
    # #                         nomsgrups=c("Il","Je"))
    # ff.plot(data=newData,funcg=functions$Funcions,genes = functions$Gens, l.dat.pre=l.dat.pre.data, 
    #         treat=input$covariables, grup=input$Tissue, alf=input$alphaTukey, 
    #         a=0.3,     # desplaçament de l'inici dels rectangles
    #         eps=0.2,   # y del plot a nivell x=0
    #         incy=0.4,  # alçada rectanglets
    #         nomsgrups=c("Il","Je"), mida=1)
  })
  
  
  ####
  #
  # LinePlots
  #
  #### 
  # 
  # Color picker
  #   
  ####  
  get_treatments_number <- reactive({
    get_new_data()[, input$covariables] %>% uniqueN()
  })
  
  cols <- reactive({
    colins <- get_default_colors()
    
    lapply(1:get_treatments_number(), function(i) {
      div(style="display: inline-block;vertical-align:top; width: 150px;",colourpicker::colourInput(paste("col", i, sep="_"), paste0("Treatment",i), colins[i],returnName = TRUE, allowTransparent = TRUE))
    })
  })
  output$colorSelector <- renderUI({cols()})
  
  treat <- reactive({
    dat<- read_data()
    selectizeInput("treatcat","Select treat category:",choices = dat[[input$covariables]],selected=T, multiple = FALSE)
  })
  
  get_default_colors <- function(){
    c("black","green","blue","red","yellow","orange","royalblue") 
  }
  
  get_function_colors <- reactive({
    gene_functions <-  pull(get_gene_functions(), 'Funcions') %>% unique
    gene_functions_n <- length(gene_functions)
    colors <- rev(get_default_colors())[1:gene_functions_n]
    
    return( data.frame(gene_functions, colors, stringsAsFactors = FALSE) )
  })
  
  get_treatment_colors <- reactive({
    treatments <- get_new_data()[, input$covariables] %>% unique
    treatments_n <- length(treatments)
    if(as.numeric(input$defCol) == 1 ) {
      colorins <- get_default_colors()[1:treatments_n]
    } else {
      colorins <- sapply(1:treatments_n, function(i) {
        input[[paste("col", i, sep="_")]]
      })
    }
    return( data.frame(treatments, colors=colorins, stringsAsFactors = FALSE) )
  })
  
  assign_class_colors <- function(x, colors, column_name){
    df <- data.frame(x)
    colnames(df) <- column_name
    df %>% 
      left_join(colors, by=column_name) %>% 
      .[, 'colors'] %>% 
      as.character
  }
  
  output$treatSelector <- renderUI({treat()})
  
  output$lineplot <- renderPlot({
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    newDat <- get_new_data()
    functions <- get_gene_functions()
    if(provador==T){ newDat <- newData;    input$treatcat <- 1;input$defCol=1;input$orderLine=1}
    idx <- match(input$factors, names(newDat))
    idx <- sort(c(idx-1, idx))
    data.idx <- newDat[,-idx]
    data.idx2 <- apply(data.idx,2,function(x) as.numeric(x))
    nw <- as.data.frame(data.idx2)
    rownames(nw) <- rownames(data.idx)
    colnames(nw) <- colnames(data.idx)
    nw[[input$covariables]] <- newDat[,input$covariables]
    maxim <- list()
    minim <- list()
    for(i in 1:length(levels(as.factor(nw[,input$covariables])))){
      
      maxim[[i]] <- as.numeric(max(colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)))
      minim[[i]] <- as.numeric(min(colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)))
      
    }
    
    colorins <- get_treatment_colors()$colors
    
    mitjanes <- list()
    if(as.numeric(input$orderLine)==1){
      for(i in 1:length(levels(as.factor(newDat[,input$covariables])))){
        mitjanes[[i]] <- colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)
      }
      mitjanes <- as.data.frame(matrix(unlist(mitjanes),nrow=length(mitjanes[[1]]),byrow=F))
      colnames(mitjanes) <- levels(as.factor(newDat[,input$covariables]))
      rownames(mitjanes) <- colnames(nw)[-ncol(nw)]
      funcions<- functions[unlist(functions[,1]) %in% colnames(nw),]
      noms <- paste0(funcions$Funcions,"_",funcions$Gens)
      mitjanes<- cbind(noms,mitjanes)
      mitjanes <- mitjanes[order(mitjanes[,input$treatcat],decreasing = T),]
      nomsfinals <- mitjanes[,"noms"]
      mitjanes <- mitjanes[,-1]
      
    }else if(as.numeric(input$orderLine)==2){
      for(i in 1:length(levels(as.factor(newDat[,input$covariables])))){
        mitjanes[[i]] <- colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)
      }
      mitjanes <- as.data.frame(matrix(unlist(mitjanes),nrow=length(mitjanes[[1]]),byrow=F))
      colnames(mitjanes) <- levels(as.factor(newDat[,input$covariables]))
      rownames(mitjanes) <- colnames(nw)[-ncol(nw)]
      funcions<- functions[unlist(functions[,1]) %in% colnames(nw),]
      noms <- paste0(funcions$Funcions,"_",funcions$Gens)
      mitjanes<- cbind(noms,mitjanes)
      mitjanes<-mitjanes[order(mitjanes[,"noms"]),]
      nomsfinals <- mitjanes[,"noms"]
      mitjanes <- mitjanes[,-1]
    }else{
      for(i in 1:length(levels(as.factor(newDat[,input$covariables])))){
        mitjanes[[i]] <- colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)
      }
      mitjanes <- as.data.frame(matrix(unlist(mitjanes),nrow=length(mitjanes[[1]]),byrow=F))
      colnames(mitjanes) <- levels(as.factor(newDat[,input$covariables]))
      rownames(mitjanes) <- colnames(nw)[-ncol(nw)]
      funcions<- functions[unlist(functions[,1]) %in% colnames(nw),]
      noms <- paste0(funcions$Funcions,"_",funcions$Gens)
      mitjanes<- cbind(noms,funcions,mitjanes)
      mitjanes<-mitjanes[order(mitjanes[,"Funcions"],-mitjanes[,paste0(input$treatcat)]),]
      nomsfinals <- mitjanes[,1]
      mitjanes <- mitjanes[,-c(1:3)]
    }
    par(mar=c(14, 3, 1, 1))
    for(i in 1:get_treatments_number()){
      if(i == 1){
        plot(mitjanes[,i],
             col=colorins[i],
             ylim=c(min(as.numeric(na.omit(unlist(minim))))-0.1,
                    max(as.numeric(na.omit(unlist(maxim))))+0.1),
             type="o",
             pch=19,
             xaxt='n',
             xlab=NA,
             ylab=NA)
        axis(1, at=1:(ncol(nw)-1), labels=nomsfinals,las=2, cex.axis=0.8)
      }else{
        lines(mitjanes[,i],col=colorins[i],type="o",pch=19)
      }
    }
    
    # a<- significatius()
    # g <- which( a[,3] <= input$alpha)
    # validate(need(g,"No hi ha cap valor significatiu"))
    significatius() %>% 
      filter(p.value < input$alpha) %>% 
      select(Genes) %>% 
      .[, 1] ->
      sign_genes
    
    ind_sign <- which(sapply(nomsfinals, function(nom) nom %in% sign_genes))
    if( length(ind_sign)>0 ){
      abline(v=ind_sign,col="black",lty="dotted")
    }
    legend("topright",paste0("T",1:length(levels(as.factor(newDat[,input$covariables])))), cex=0.8, col=colorins,lty=1, title=input$covariables)
    
  })
  # ####
  #
  # Heatmap
  #
  #### 
  
  output$heatmap <-renderPlotly({
    if(provador==FALSE) validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    newDat <- get_new_data()
    if(provador==T){
      newDat <- newData;
      Covariable<- as.factor(pData(Expression)[,input$covariables])
      nomscols <- functions[functions$Gens %in% rownames(Expression),"Funcions"]
      Covariable <- as.data.frame(Covariable)
    }
    functions<- get_gene_functions()
    nomscols <- data.frame("Funcions"=functions[functions$Gens %in% rownames(exprs(data_expression())),"Funcions"])
    rownames(nomscols) <- unlist(functions[functions$Gens %in% rownames(exprs(data_expression())),"Gens"])
    Covariable<- as.factor(pData(data_expression())[,input$covariables])
    Covariable <- as.data.frame(Covariable)
    colnames(Covariable) <- input$covariables
    rownames(Covariable) <- colnames(exprs(data_expression()))
    divergent_viridis_magma <- c(viridis(10, begin = 0.3), rev(magma(10, begin = 0.3)))
    rwb <- colorRampPalette(colors = c("darkred", "white", "darkgreen"))
    BrBG <- colorRampPalette(brewer.pal(11, "BrBG"))
    Spectral <- colorRampPalette(rev(brewer.pal(40, "Spectral")))
    heatmaply(exprs(data_expression()),colors=Spectral,na.value = "grey50",na.rm=F,col_side_colors=Covariable,row_side_colors = nomscols,margins = c(120,120,20,120),seriate = "OLO") %>%
      colorbar(tickfont = list(size = 10), titlefont = list(size = 10), which = 1) %>%
      colorbar(tickfont = list(size = 10), titlefont = list(size = 10), which = 2)
    
  })
  
  ####
  #
  # Components principals
  #
  #### 
  
  plot_pca <- reactive({
    mitjanes <- calc_treatment_means()
    
    # saveRDS(mitjanes, 'mitjanes.RDS')
    # saveRDS(colors, 'colors.RDS')
    # saveRDS(colorize_functions, 'color_funs.RDS')
    # 
    gene_functions <- get_gene_functions()
    colnames(gene_functions)[1] <- 'Genes'
    mitjanes %>% 
      select(Genes) %>% 
      left_join(gene_functions) ->
      mitjanes_funcions
    colnames(mitjanes_funcions)[2] <- 'gene_functions'
    
    colors = get_function_colors()
    colorize_functions <- assign_class_colors(x = mitjanes_funcions$gene_functions, colors, 'gene_functions')
    
    
    pcajetr<-PCA(mitjanes,quali.sup=1,graph=F)
    par(mfrow = c(1,2),
        oma = c(0,0,0,0) + 0.5,
        mar = c(4,4,4,4) + 0.5)
    plot(pcajetr,choix="var",col.var="blue",cex.main=0.7)
    plot.PCA(pcajetr,choix="ind",col.ind=colorize_functions, invisible="quali",label="none")
    legend("topright",colors$gene_functions, cex=0.6, col=colors$colors,lty=1,bg="white")
    
    
  })
  
  output$pca <- renderPlot({
    plot_pca()
  })
  
  output$downloadPCA <- downloadHandler(
    filename = "PCA.png",
    contentType = "image/png",
    content = function(file) {
      # png(file=file)
      # plot_pca()
      # dev.off()
    }
  )
  
  
  ####
  #
  # Components principals II
  #
  #### 
  
  color_treatment <- reactive({
    get_new_data() %>% 
      select(input$covariables) %>% 
      .[, 1] %>% 
      unique -> 
      treatments
    
    col_pal <- brewer.pal(length(treatments), 'Set3')
    col_pal <- col_pal[1:min(length(treatments), length(col_pal))]
    data.frame(treatment = treatments, color = col_pal)
  })
  
  # 
  output$pca2 <- renderPlot({
    funcpca()
  })
  
  funcpca<-reactive({
    eixos=c(1,2)
    # coltract=c("black","orange","red")
    gruix=1.7
    gris=4
    limcos2=0.5
    cextit=.7
    cexleg=.6
    cexlab=.6
    cexax=.6
    cexlletra=.7
    # nomstr=nomstracs,gsignif=tab1, nivellsfunc=levels_func,
    # data=datJejunal[,-c(1:3)]
    # gsignif=tab1  
    # eixos=c(1,3)
    
    new_data <- get_new_data()
    
    new_data %>% 
      colnames %>% 
      setdiff(input$factors) ->
      selected_columns
    treatment <- new_data %>% select(input$covariables)
    new_data %<>% select(selected_columns) 
    
    treatment_color <- color_treatment()
    
    ## impute NAs with EM-algorithm to improve default imputation by mean value
    if (sum(is.na(new_data))!=0){
      new_data<-imputePCA(new_data)$completeObs
      # sum(is.na(new_data.i_1)) ## must be 0
    }
    new_data.i<-cbind(treatment, new_data)
    
    pcaout<-PCA(new_data,quali.sup=1,graph=F)
    
    par(mfrow = c(1,2),
        oma = c(0,0,0,0) + 0.5,
        mar = c(4,4,4,4) + 0.5)
    
    legend_opts <- list(
      x="topleft", 
      legend=get_treatment_colors()[, 'treatments'], 
      cex=cexleg, 
      col=get_treatment_colors()[, 'colors'],
      # lty=1, 
      title=input$covariables,
      bg="white")

    ## individuals graph
    colorize_treatments <- assign_class_colors(treatment[, 1], get_treatment_colors(), 'treatments')
    
    plot(pcaout,
         choix="ind",
         axes=eixos,
         invisible="quali",
         habillage=1,
         col.hab=colorize_treatments %>% as.character(),
         lwd=gruix,
         cex.main=cextit,
         cex=cexlletra,
         cex.lab=cexlab,
         cex.axis=cexax,
         legend=legend_opts )


    ## variables graph
    ## only genes with a quality over threshold in limcos2
    ###  and being significant, that is, belong to the table gsignif
    
    pcaux<-pcaout
    pcaqual2<-apply(pcaux$var$cos2[,eixos],1,sum)
    ## subset of pcaout
    ### significant genes and quality over threshold
    
    significatius() %>% 
      subset(p.value < input$alpha) %>% 
      select(Genes) %>% 
      .[, 1] ->
      sign_genes
    ind_limcos <- pcaqual2 > limcos2
    
    rownames(pcaux$var$coord) %>% 
      data.frame(Genes=., stringsAsFactors = FALSE) %>% 
      combine_genes_with_funcions() ->
      genes
    ind_sign <- sapply(genes, function(x) x %in% sign_genes)
    pcaux$var$coord <- pcaux$var$coord[ind_sign & ind_limcos, ]
    
    rownames(pcaux$var$cos2) %>% 
      data.frame(Genes=., stringsAsFactors = FALSE) %>% 
      combine_genes_with_funcions() ->
      genes
    ind_sign <- sapply(genes, function(x) x %in% sign_genes)
    pcaux$var$cos2 <- pcaux$var$cos2[ind_sign & ind_limcos, ]
    
    
    gene_functions <- get_gene_functions()
    colnames(gene_functions)[1] <- 'Genes'
    rownames(pcaux$var$cos2) %>%
      data.frame(Genes=.) %>% 
      left_join(gene_functions) ->
      rows_funcions
    colnames(rows_funcions)[2] <- 'gene_functions'
    
    colors = get_function_colors()
    colorize_functions <- assign_class_colors(x = rows_funcions$gene_functions, colors, 'gene_functions')
    
    
    # saveRDS(pcaux, 'pca.RDS')
    # saveRDS(colors, 'colors.RDS')
    # saveRDS(colorize_functions, 'color_funs.RDS')
    
    plot(pcaux, axes=eixos, choix="var", col.var=colorize_functions,
         lwd=gruix, cex.main=cextit, cex=cexlletra*.8,  cex.lab=cexlab,cex.axis=cexax)
    legend("topleft",colors$gene_functions, cex=0.6, col=colors$colors,lty=1,bg="white")
    
    # gray color is the baseline color for variables, overprinted with functions colors afterwards
    # colorbase<-gray.colors(1,0.3+gris*0.05,0.3+gris*0.1)
    # plot(pcaux, axes=eixos, choix="var", col.var=colorbase,
    #      lwd=gruix, cex.main=cextit, cex=cexlletra*.8,  cex.lab=cexlab,cex.axis=cexax)

    # same colors as in heatmap
    # ng<-nrow(pcaux$var$coord)
    # nomgenaux<-rownames(pcaux$var$coord)
    # nomfuncaux<-asig.func(nomgenaux,nivellsfunc)
    # colaux<-character()
    # for (i in 1:ng){## i<-1
    #   fila<-which(rownames(levels_func)==nomgenaux[i])
    #   colaux[i]<-levels_func[fila,3]
    # }
    # for (i in 1:ng)
    #   arrows(x0=0,y0=0,x1=pcaux$var$coord[i,eixos[1]],
    #          y1=pcaux$var$coord[i,eixos[2]],col=colaux[i], angle = 14,length=.1)
    # # legend
    # colors<-unique(colaux)
    # funcaux<-unique(nomfuncaux)
    # legend("topleft",legend=funcaux, col=colors,cex=cexleg,lty=1,title="Functions")
  })
  
  
  ####
  #
  # Codi per tancar automaticament l'aplicacio web
  #
  ####
  
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML('Usage.md'))
  # })
  # 
  # 
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  
  ######
  # Codi per generar multiple xlsx summary
  ######
  llistaTables <- reactive({
    llista <- list()
    llista[[1]] <- read_data() #guardem la taula principal
    llista[[2]] <- get_gene_functions() #guardem les funcions
    # llista[[3]] <- as.data.frame(exprs(data_expression()))#guardem la matriu d'expressió
    # tt <- significatius()
    # tt <- na.omit(tt)
    # tt$p.value <- format(tt$p.value,4)
    # tt$p.BH <- format(tt$p.BH,4)
    # colnames(tt) <- c("Contrast Statistic", "P-value", "P-value(FDR)")
    # aligned <- 'rrr'
    # g <- which( tt[,2] <= input$alpha)
    # pvaluesTable<- tt[g,]
    # pvaluesTable <- datatable(pvaluesTable) %>%
    #   formatStyle("P-value(FDR)",backgroundColor = styleInterval(c(input$alphaFDR),c("#b5f2b6","white")))%>%
    #   formatRound(columns=colnames(pvaluesTable), digits=4)
    # llista[[4]] <- pvaluesTable #anova
    # a<- significatius()
    # g <- which( a[,3] <= input$alpha)
    # validate(need(g,"No hi ha cap valor significatiu"))
    # Tukey_test<- function(data_expression){
    #   Tuk <- list()
    #   for(i in 1:nrow(exprs(data_expression))){
    #     if( sum(is.na(exprs(data_expression)[i,])) < length(exprs(data_expression)[i,])-2){
    #       Tuk[[i]] <- TukeyHSD(aov(exprs(data_expression)[i,]~ as.factor(pData(data_expression)[,1])))
    #       names(Tuk)[[i]] <- rownames(exprs(data_expression))[i]
    #     }
    #   }
    #   return(na.omit(Tuk))
    # }
    # Tuk <- Tukey_test(data_expression())
    # tt <- significatius()
    # names(Tuk) <- rownames(tt)
    # dataTuk <- as.data.frame(na.omit(t(sapply(Tuk,function(x) x$`as.factor(pData(data_expression)[, 1])`[,4], USE.NAMES = F))))
    # dataTuk <- dataTuk[rowSums(dataTuk <= input$alphaTukey)>=1,]
    # dataTuk <- datatable(dataTuk) %>%
    #   formatStyle(colnames(dataTuk),backgroundColor = styleInterval(c(input$alphaTukey),c("#b5f2b6","white"))) %>%
    #   formatRound(columns=colnames(dataTuk), digits=4)
    # llista[[5]] <- dataTuk #tukey
    names(llista) <- c("MainData","Functions")
    llista
  })
  
  output$ExcelButton <- downloadHandler(
    filename = "Results.xlsx",
    content = function(file) {
      dat <- llistaTables()
      # wb <- createWorkbook()
      # for (i in 1:2) {
      #   addWorksheet(wb,sheetName=names(dat)[i])
      #   writeData(wb, sheet=i,dat[[i]])
      #   saveWorkbook(wb,file,overwrite = T)
      # }
      write_xlsx(dat)
    }
  )
  
  #THEORY
  
  observeEvent(input$show1, {
    showModal(modalDialog(
      title = "Theory of ANOVA (Catalan version)",
      withMathJax(includeMarkdown("shows/show1.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show2, {
    showModal(modalDialog(
      title = "Theory of FDR (Catalan version)",
      withMathJax(includeMarkdown("shows/show2.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show3, {
    showModal(modalDialog(
      title = "Theory of Tukey (Catalan version)",
      withMathJax(includeMarkdown("shows/show3.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show4, {
    showModal(modalDialog(
      title = "Theory of PCA (Catalan version)",
      withMathJax(includeMarkdown("shows/show4.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show6, {
    showModal(modalDialog(
      title = "Theory of Heatmap (Catalan version)",
      withMathJax(includeMarkdown("shows/show5.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  #HINTS
  observeEvent(input$showi2, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown("hints/hint1.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi3, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown("hints/hint2.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi4, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown("hints/hint3.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi5, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown("hints/hint4.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi6, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown("hints/hint5.md")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Examplefile", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/data/Resultados_INT_44_2017.xlsx", file)
    }
  )
  
}
