source(paste0(script.dirname,"server/Analysis/RenderLoadFiles.R"))
source(paste0(script.dirname,"server/Analysis/GetTukeyPlot.R"))
server <- function(input, output,session) {
  ##### 
  #Provador
  ####
  provador=F
  if(provador){
    input <- list()
    newData <-  read_xlsx("~/Descargas/Database_AGL_H3_d2.xlsx",col_names = TRUE,sheet = 1)
    functions <- read_xlsx("~/Descargas/Database_AGL_H3_d2.xlsx",col_names = TRUE,sheet = 2)
    functions <- functions[,1:2]
    input$factors <- c("Sample ID","Group","Treatment","Tissue")
    input$covariables <- c("Treatment")
    input$Tissue <- c("Tissue")
    input$tissuecat <- c("Jejunum")
    #input$alphaTukey <- 0.1
    input$NAInput <- 0.5
    input$id <- "Sample ID"
  }
  
  
  ######
  #
  # Leer los datos como primera acción
  #
  ######
  
  dt <- reactive({
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
    }
  })
  
  
  #####
  #
  # Leer las funciones de los genes
  #
  ####
  Functions <- reactive({
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
    }
  })
  
  ####
  #
  # Filtrar NA's, SELECTOR NA'S
  #
  ####
  
  output$NAs <- renderUI({
    validate(need(input$file1,"Insert File!"))
    div(p(paste0("Total rows: ",nrow(dt()))),
        p(paste0("Total columns: ", ncol(dt()))),
        p(paste0("Number of NA in database: ",sum(apply(dt(),1,function(x) sum(is.na(x)))))))
  })
  
  
  
  ####
  #
  # Selectores (Uno para los factores, uno para el subset, uno para la covariable); alfa slectors 
  #
  ####
  output$NAinput <- renderPrint({ input$NAinput })
  output$factorSelection<- renderUI({
    validate(need(input$file1,"Insert File!"))
    selectizeInput("factors","Select Factors:",
                   choices = c(colnames(dt())),selected=NULL, multiple = TRUE)})
  output$TissueSelection <- renderUI({
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    selectizeInput("Tissue","Select Tissue variable:",choices = c(input$factors),selected=F, multiple = FALSE)})
  output$IDSelection <- renderUI({
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    selectizeInput("id","Select id variable:",choices = c(input$factors),selected=F, multiple = FALSE)})
  output$TissueCategory <- renderUI({
    datTiss<- dt()
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
  newData <- eventReactive(input$Start,{
    dat <- dt()
    if(provador==T){dat <- newData}  
    rowbye <- list()
    for(i in 1:nrow(dat)){
      if(sum(is.na(dat[i,]))==(ncol(dat)-length(input$factors))){
        rowbye[[i]] <- i
      }else{
        rowbye[[i]] <- NA
      }
    }
    if(length(na.omit(unlist(rowbye)))>0){
      dat <- dat[-na.omit(unlist(rowbye)),]
    }
    bad.sample <- list()
    ##Bye bye NA's
    for(i in 1:length(levels(as.factor(unlist(dat[,input$covariables]))))){
      datCat <- subset(dat, dat[,input$covariables]==levels(as.factor(unlist(dat[,input$covariables])[i])))
      bad.sample[[i]]<- colMeans(is.na(datCat)) > input$NAInput
    }
    newData <- as.data.frame(dat[,!colnames(dat) %in% unique(names(which(unlist(bad.sample)==T)))])
    newData <- subset(newData, newData[,input$Tissue]==input$tissuecat)
    rownames(newData) <- newData[,input$id] 
    newData
  })
  
  dataExpression <- eventReactive(input$Start,{
    newDat <- newData()
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
    nw <- log10(data.idx2)
    rownames(nw) <- rownames(data.idx)
    colnames(nw) <- colnames(data.idx)
    #nw <- subset(newData, colnames(newData) %in% input$factors)
    Expression <- ExpressionSet(as.matrix(t(nw)),phenoData = AnnotatedDataFrame(data=df))
    Expression
  })
  
  dataExpression_new <- eventReactive(input$Start,{
    data_set <- newData()
    if(provador==T){newDat <- newData}
    data_covars <- data_set[,input$covariables, drop=FALSE]
    
    sel_cols <- setdiff(colnames(data_set), input$factors)
    
    data_set[, sel_cols] %>%
      t %>% 
      as.matrix %>%
      ExpressionSet(phenoData = AnnotatedDataFrame(data=data_covars))
  })
  
  # GetTukeyList <- eventReactive(input$Start,{
  #   dat <- dt()
  #   if(provador==T){dat <- newData}    
  #   bad.sample <- list()
  #   ##Bye bye NA's
  #   for(i in 1:length(levels(as.factor(unlist(dat[,input$covariables]))))){
  #     datCat <- subset(dat, dat[,input$covariables]==levels(as.factor(unlist(dat[,input$covariables])[i])))
  #     bad.sample[[i]]<- colMeans(is.na(datCat)) > input$NAInput
  #   }
  #   newData <- as.data.frame(dat[,!colnames(dat) %in% unique(names(which(unlist(bad.sample)==T)))])
  #   newData1 <- subset(newData, newData[,input$Tissue]==input$tissuecat)
  #   tratnewData <- newData1[,input$covariables]
  #   idx <- match(input$factors, names(newData1))
  #   idx <- sort(c(idx-1, idx))
  #   newData1 <- log10(newData1[,-idx])
  #   newData1<- cbind(newData1,tratnewData)
  #   colnames(newData1)[ncol(newData1)] <- input$covariables
  #   newData2 <- subset(newData, newData[,input$Tissue]!=input$tissuecat)
  #   tratnewData <- newData2[,input$covariables]
  #   idx <- match(input$factors, names(newData2))
  #   idx <- sort(c(idx-1, idx))
  #   newData2 <- log10(newData2[,-idx])
  #   newData2<- cbind(newData2,tratnewData)
  #   colnames(newData2)[ncol(newData2)] <- input$covariables
  #   llista <- list(newData1,newData2)
  #   llista
  # })
  
  #Load file function server
  output$table <-renderDataTable({dt()})
  output$table2 <- renderDataTable({Functions()})
  ####
  #
  # Tabla gene x samples
  #
  ####
  
  
  output$tableHead <- renderTable({    
    validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select all factors of dataset"))
    validate(need(input$covariables,"Select covariable"))
    if(input$checkbox==F){
      exprs(dataExpression())[1:6,1:6]
    }else if(input$checkbox){
      exprs(dataExpression())
    }
  },rownames=T)
  ####
  #
  # Tabla Ftests
  #
  ####
  significatius <- reactive({
    functions <- Functions()
    Express <- dataExpression() 
    if(provador==T){Express <- Expression}
    tt=rowFtests(Express,as.factor(pData(Express)[,input$covariables]))
    p.BH = p.adjust(tt[,"p.value"], "BH" )
    tt <- cbind(tt,p.BH)
    tt <- na.omit(tt)
    func<- functions[functions$Gens %in% rownames(tt),]
    rownames(tt) <- paste0(func$Funcions,"_",func$Gens)
    tt
  })
  
  output$tableMCA<- DT::renderDataTable({
    validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select all factors of dataset"))
    validate(need(input$covariables,"Select covariable"))
    
    pvaluesTable <- significatius()
    pvaluesTable$p.value <- format(pvaluesTable$p.value,4)
    pvaluesTable$p.BH <- format(pvaluesTable$p.BH,4)
    colnames(pvaluesTable) <- c("Contrast Statistic", "P-value", "P-value(FDR)")
    
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
  # Tabla Tukey
  #
  ####
  Tukey_test<- function(dataExpression){
    Tuk <- list()
    for(i in 1:nrow(exprs(dataExpression))){
      if( sum(is.na(exprs(dataExpression)[i,])) < length(exprs(dataExpression)[i,])-2){
        Tuk[[i]] <- TukeyHSD(aov(exprs(dataExpression)[i,]~ as.factor(pData(dataExpression)[,1])))
        names(Tuk)[[i]] <- rownames(exprs(dataExpression))[i]
      }
    }
    return(na.omit(Tuk))
  }
  
  calculate_table_Tukey <- reactive({
    a<- significatius()
    g <- which( a[,3] <= input$alpha)
    validate(need(g,"No hi ha cap valor significatiu"))
    functions <- Functions()
    Tuk <- Tukey_test(dataExpression_new())
    tt <- significatius()
    func2<- functions[functions$Gens %in% names(Tuk),]
    names(Tuk) <- paste0(func2$Funcions,"_",func2$Gens)
    g <- which(tt[,3]<=input$alphaFDR)
    noms <- as.numeric(names(Tuk) %in% rownames(tt[g,]))
    cac <- as.numeric((t(noms)*(1:length(noms))))
    Tuk <- Tuk[which(cac>0)]
    
    treats_pvalues <- lapply(Tuk,function(x) t(x$`as.factor(pData(dataExpression)[, 1])`[,4, drop=FALSE]))
    treats_pvalues <- do.call(rbind, treats_pvalues)
    treats_pvalues <- as.data.frame(treats_pvalues)
    rownames(treats_pvalues) <- names(Tuk)
    treats_pvalues
  })
  
  calculate_table_Tukey_new <- reactive({
    # a<- significatius()
    # g <- which( a[,3] <= input$alpha)
    # validate(need(g,"No hi ha cap valor significatiu"))
    
    # functions <- Functions()
    tukey_table <- Tukey_test(dataExpression_new())
    
    treats_pvalues <- lapply(tukey_table,function(x) t(x$`as.factor(pData(dataExpression)[, 1])`[,4, drop=FALSE]))
    treats_pvalues <- do.call(rbind, treats_pvalues)
    treats_pvalues <- as.data.frame(treats_pvalues)
    rownames(treats_pvalues) <- names(tukey_table)
    treats_pvalues
  })
  
  
  output$tableTukey <- DT::renderDataTable({
    treats_pvalues <- calculate_table_Tukey()
    datatable(treats_pvalues) %>%
      #formatStyle(colnames(treats_pvalues),backgroundColor = styleInterval(c(input$alphaTukey),c("#b5f2b6","white"))) %>%
      formatRound(columns=colnames(treats_pvalues), digits=4)
  })
  
  ######
  #
  # Tukey table: Significant Groups
  #
  #####
  
  get_treats_names_from_comparisons <- function(treats_comp){
    treats_comp %>% 
      strsplit('-') %>% 
      unlist %>% 
      unique %>% 
      sort
  }
  
  calculate_letters <- function(p_values, alpha_value){
    p_values %>% 
      names %>% 
      get_treats_names_from_comparisons ->
      treats
    
    significant_inds <- p_values < alpha_value
    
    significant_treats_comp <- colnames(p_values)[significant_inds]
    significant_treats <- get_treats_names_from_comparisons(significant_treats_comp)
    significant_treats_comp %>% 
      strsplit('-') %>% 
      lapply(sort) ->
      significant_treats_comp
    
    for( signif_treat in significant_treats ){
      
    }
    
    
    
  }
  
  pvalues_to_letters <- reactive({
    alpha_value <- input$alpha
    treats_pvalues <- calculate_table_Tukey_new()
    
  })
  
  
  Tukey_letters<- function(dataExpression){
    Tuk <- list()
    for(i in 1:nrow(exprs(dataExpression))){
      if( sum(is.na(exprs(dataExpression)[i,])) < length(exprs(dataExpression)[i,])-2){
        aov_model <- aov(exprs(dataExpression)[i,]~ as.factor(pData(dataExpression)[,1]))
        Tuk[[i]] <- HSD.test(aov_model, trt =input$covariables)
        names(Tuk)[[i]] <- rownames(exprs(dataExpression))[i]
      }
    }
    return(na.omit(Tuk))
  }
  
  output$tableTreatments <- DT::renderDataTable({
    new_data <- newData()
    gene_cols <- setdiff(names(new_data), input$factors)
    sel_cols <- c(input$covariables, gene_cols)
    new_data <- as.data.table(new_data[, sel_cols])
    
    new_data %>% 
      melt(id.vars=input$covariables, measure.vars = gene_cols) ->
      melted_data
    setnames(melted_data, input$covariables, 'treatment')
    
    melted_data %>% 
      .[, .(mean_treatment = mean(log10(value))), by= .(variable, treatment)] %>% 
      dcast(variable~treatment, value.var='mean_treatment') ->
      treatment_means
    setnames(treatment_means, 'variable', 'Gene')
    
    # browser()
    
    # tuket_letters <- Tukey_letters(dataExpression_new())
    
   
    treatment_means %>% 
      na.omit %>% 
      datatable %>%
      formatRound(columns=colnames(treatment_means), digits=4)
  })
  
  
  ######
  #
  # Tukey plots
  #
  #####
  
  output$tukeyplot<- renderPlot({NULL
    # newData <- dt()
    # l.dat.pre.data <- GetTukeyList() 
    # functions <- Functions()
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
  cols <- reactive({
    newDataCol <- newData()
    colins <- c("black","green","blue","red","yellow","orange","royalblue") 
    
    lapply(1:length(levels(as.factor(newDataCol[[input$covariables]]))), function(i) {
      div(style="display: inline-block;vertical-align:top; width: 150px;",colourpicker::colourInput(paste("col", i, sep="_"), paste0("Treatment",i), colins[i],returnName = TRUE, allowTransparent = TRUE))
    })
  })
  output$colorSelector <- renderUI({cols()})
  
  
  colors <- reactive({
    newDat <- newData()
    lapply(1:length(levels(as.factor(newDat[[input$covariables]]))), function(i) {
      input[[paste("col", i, sep="_")]]
    })
  })
  
  treat <- reactive({
    dat<- dt()
    selectizeInput("treatcat","Select treat category:",choices = dat[[input$covariables]],selected=T, multiple = FALSE)
  })
  
  output$treatSelector <- renderUI({treat()})
  
  output$lineplot <- renderPlot({
    validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    newDat <- newData()
    functions <- Functions()
    if(provador==T){ newDat <- newData;    input$treatcat <- 1;input$defCol=1;input$orderLine=1}
    idx <- match(input$factors, names(newDat))
    idx <- sort(c(idx-1, idx))
    data.idx <- newDat[,-idx]
    data.idx2 <- apply(data.idx,2,function(x) as.numeric(x))
    nw <- as.data.frame(log10(data.idx2))
    rownames(nw) <- rownames(data.idx)
    colnames(nw) <- colnames(data.idx)
    nw[[input$covariables]] <- newDat[,input$covariables]
    maxim <- list()
    minim <- list()
    for(i in 1:length(levels(as.factor(nw[,input$covariables])))){
      
      maxim[[i]] <- as.numeric(max(colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)))
      minim[[i]] <- as.numeric(min(colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)))
      
    }
    
    if(as.numeric(input$defCol) == 1 ) {
      colorins <- 1:length(levels(as.factor(newDat[[input$covariables]])))
    } else {
      colorins <- unlist(colors())
    }
    
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
    for(i in 1:length(levels(as.factor(newDat[,input$covariables])))){
      if(i == 1){
        plot(mitjanes[,i],col=colorins[i],ylim=c(min(as.numeric(na.omit(unlist(minim))))-0.1,max(as.numeric(na.omit(unlist(maxim))))+0.1),type="o",pch=19,xaxt='n',xlab=NA,ylab=NA)
        axis(1, at=1:(ncol(nw)-1), labels=nomsfinals,las=2, cex.axis=0.8)
      }else{
        lines(mitjanes[,i],col=colorins[i],type="o",pch=19)
      }
    }
    
    # a<- significatius()
    # g <- which( a[,3] <= input$alpha)
    # validate(need(g,"No hi ha cap valor significatiu"))
    sign<- significatius()
    if(provador==T){sign <- tt}
    sign <- na.omit(sign)
    if(length(sign)>0){
      signAblines<- rownames(sign[sign$p.BH<=input$alphaFDR,])
      signAblines<- which(nomsfinals %in% signAblines)
      for(i in 1:length(signAblines)) abline(v=signAblines[i],col="black",lty="dotted")
    }
    legend("topright",paste0("T",1:length(levels(as.factor(newDat[,input$covariables])))), cex=0.8, col=colorins,lty=1, title=input$covariables)
    
  })
  # ####
  #
  # Heatmap
  #
  #### 
  
  output$heatmap <-renderPlotly({
    validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    newDat <- newData()
    if(provador==T){
      newDat <- newData;
      Covariable<- as.factor(pData(Expression)[,input$covariables])
      nomscols <- functions[functions$Gens %in% rownames(Expression),"Funcions"]
      Covariable <- as.data.frame(Covariable)
    }
    functions<- Functions()
    nomscols <- data.frame("Funcions"=functions[functions$Gens %in% rownames(exprs(dataExpression())),"Funcions"])
    rownames(nomscols) <- unlist(functions[functions$Gens %in% rownames(exprs(dataExpression())),"Gens"])
    Covariable<- as.factor(pData(dataExpression())[,input$covariables])
    Covariable <- as.data.frame(Covariable)
    colnames(Covariable) <- input$covariables
    rownames(Covariable) <- colnames(exprs(dataExpression()))
    divergent_viridis_magma <- c(viridis(10, begin = 0.3), rev(magma(10, begin = 0.3)))
    rwb <- colorRampPalette(colors = c("darkred", "white", "darkgreen"))
    BrBG <- colorRampPalette(brewer.pal(11, "BrBG"))
    Spectral <- colorRampPalette(rev(brewer.pal(40, "Spectral")))
    heatmaply(exprs(dataExpression()),colors=Spectral,na.value = "grey50",na.rm=F,col_side_colors=Covariable,row_side_colors = nomscols,margins = c(120,120,20,120),seriate = "OLO") %>%
      colorbar(tickfont = list(size = 10), titlefont = list(size = 10), which = 1) %>%
      colorbar(tickfont = list(size = 10), titlefont = list(size = 10), which = 2)
    
  })
  
  ####
  #
  # Components principals
  #
  #### 
  output$pca <- renderPlot({
    validate(need(input$file1,"Insert File!"))
    validate(need(input$factors,"Select factors of dataset"))
    validate(need(input$covariables,"Select covariable of dataset"))
    newDat <- newData()
    functions <- Functions()
    if(provador==T){ newDat <- newData;input$defCol=1;input$orderLine=1}
    idx <- match(input$factors, names(newDat))
    idx <- sort(c(idx-1, idx))
    nw <- log10(newDat[,-idx])
    nw[[input$covariables]] <- newDat[,input$covariables]
    mitjanes <- list()
    for(i in 1:length(levels(as.factor(newDat[,input$covariables])))){
      mitjanes[[i]] <- colMeans(subset(nw,nw[[input$covariables]]==i)[,-ncol(nw)],na.rm = T)
    }
    mitjanes <- as.data.frame(matrix(unlist(mitjanes),nrow=length(mitjanes[[1]]),byrow=F))
    colnames(mitjanes) <- levels(as.factor(newDat[,input$covariables]))
    rownames(mitjanes) <- colnames(nw)[-ncol(nw)]
    funcions<- functions[unlist(functions[,1]) %in% colnames(nw),2]
    mitjanes<- cbind(funcions,mitjanes)
    
    pcajetr<-PCA(mitjanes,quali.sup=1,graph=F)
    par(mfrow = c(1,2),
        oma = c(0,0,0,0) + 0.5,
        mar = c(4,4,4,4) + 0.5)
    plot(pcajetr,choix="var",col.var="blue",cex.main=0.7)
    plot(pcajetr,choix="ind",habillage=1,label="quali",cex.main=0.7)
    
  })
  ####
  #
  # Codi per tancar automaticament l'aplicacio web
  #
  ####
  
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML(paste0(script.dirname,'Usage.md')))
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
    llista[[1]] <- dt() #guardem la taula principal
    llista[[2]] <- Functions() #guardem les funcions
    # llista[[3]] <- as.data.frame(exprs(dataExpression()))#guardem la matriu d'expressió
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
    # Tukey_test<- function(dataExpression){
    #   Tuk <- list()
    #   for(i in 1:nrow(exprs(dataExpression))){
    #     if( sum(is.na(exprs(dataExpression)[i,])) < length(exprs(dataExpression)[i,])-2){
    #       Tuk[[i]] <- TukeyHSD(aov(exprs(dataExpression)[i,]~ as.factor(pData(dataExpression)[,1])))
    #       names(Tuk)[[i]] <- rownames(exprs(dataExpression))[i]
    #     }
    #   }
    #   return(na.omit(Tuk))
    # }
    # Tuk <- Tukey_test(dataExpression())
    # tt <- significatius()
    # names(Tuk) <- rownames(tt)
    # dataTuk <- as.data.frame(na.omit(t(sapply(Tuk,function(x) x$`as.factor(pData(dataExpression)[, 1])`[,4], USE.NAMES = F))))
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
      withMathJax(includeMarkdown(paste0(script.dirname,"shows/show1.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show2, {
    showModal(modalDialog(
      title = "Theory of FDR (Catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"shows/show2.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show3, {
    showModal(modalDialog(
      title = "Theory of Tukey (Catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"shows/show3.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show4, {
    showModal(modalDialog(
      title = "Theory of PCA (Catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"shows/show4.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$show6, {
    showModal(modalDialog(
      title = "Theory of Heatmap (Catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"shows/show5.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  #HINTS
  observeEvent(input$showi2, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"hints/hint1.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi3, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"hints/hint2.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi4, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"hints/hint3.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi5, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"hints/hint4.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showi6, {
    showModal(modalDialog(
      title = "Interpretation Hint (catalan version)",
      withMathJax(includeMarkdown(paste0(script.dirname,"hints/hint5.md"))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Examplefile", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy(paste0(script.dirname,"www/data/Resultados_INT_44_2017.xlsx"), file)
    }
  )
  
}
