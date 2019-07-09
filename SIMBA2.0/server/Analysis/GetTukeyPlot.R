
#######################   funció recttext()  ((obtinguda del web))

#### funció per dibuixar text dins de rectangles (extret del web)
recttext <- function(xl, yb, xr, yt, text, rectArgs = NULL, textArgs = NULL) {
  center <- c(mean(c(xl, xr)), mean(c(yb, yt)))
  do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt), rectArgs))
  do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}

#### funció que dibuixa els rectangles a partir d'un vector amb 6 components: 4 coord, pvalor i signe
## v[1]=xl (x left  o inferior) i v[2]=yb  (y bottom o inferior)
## v[3]=xr (x right o superior) i v[4]=yt  (y top o superior)
## v[5]  (p-valor) i  v[6]  (signe=0,1)
plot.grup<-function(v,nom="A",cexrec){ ## v<-AI[2,3,]; v<-AJ[10,1,]
  verblau<-ifelse( v[6]==1, "red", "blue")  # red=overexpressed, blue=underexpressed
  if ( v[5]<=0.05& !is.na(v[5]))  recttext(v[1], v[3], v[2], v[4], 
                                           nom,rectArgs = list(col = verblau),
                                           textArgs = list(col = 'white', cex = cexrec))
  if ( v[5]>0.05 | is.na(v[5]))   points(v[1],v[3],cex=.3)}


#######################   funció    GetDataExpresion()
## amb el paquet BioBase, creem un objecte de clase ExpressionSet, per poder implementar les funcions del paquet:
GetDataExpression <- function(database, sepVar){
  #Funcio que retorna un objecte clase expressionset
  #database: data on tenim els gens
  #sepVar: variable factor que volem estudiar "trat"
  df <- data.frame(trat=database[,sepVar],row.names=paste("Sample", 1:nrow(database), sep=""))
  caca<- colnames(database) %in% sepVar
  caca2 <- as.matrix(t(database[,!caca]))
  colnames(caca2) <- paste("Sample", 1:ncol(caca2),sep="")
  dataExpression <- ExpressionSet(caca2,
                                  phenoData = AnnotatedDataFrame(data=df))
   return(dataExpression)
}

#######################   funció    GetPvalues()
GetPvalues <- function(ExpressionSets, sepVar, alpha){
  # Funcio que calcula el pvalor del test F ANOVA per a comparacions de tractaments
  # Retorna un data.frame amb 3 columnes, 
  ### l'estadistic F, el pvalor ANOVA i el pvalor corregit Benjamin-H FDR
  tt = genefilter::rowFtests(ExpressionSets,as.factor(pData(ExpressionSets)[,sepVar]))
  p.BH = p.adjust(tt[,"p.value"], "BH" )
  tt <- cbind(tt,p.BH)
  g <- which( tt[,2] < alpha)
  if( length(g) > 0){
    tt[which( tt[,2] < alpha),] 
  }else{
    print("No hi ha cap significatiu")
  }
}

#######################   funció    Tukey_test()
Tukey_test<- function(dataExpression){  ## dataexpression <- datJejuExpression
  Tuk <- list()
  for(i in 1:nrow(exprs(dataExpression))){
    if( sum(is.na(exprs(dataExpression)[i,])) < length(exprs(dataExpression)[i,])-2){
      Tuk[[i]] <- TukeyHSD(aov(exprs(dataExpression)[i,]~ as.factor(pData(dataExpression)[,1])))
      names(Tuk)[[i]] <- rownames(exprs(dataExpression))[i]
    }
  }
  return(na.omit(Tuk))
}





 
#####################################################################################################
###  -----------------------------     funció específica     ------------------------------------ ###
#####################################################################################################

##  "l.dat.pre" ha deser una llista de data frames, un per grup=teixit, 
### Atenció: el nombre de grups poden ser com a molt 3 (1 o 2 poden ser nuls)
##  "treat" té el factor d'interès (tractament)
##  "grup"  és la variable de segmentació de l'arxiu (p.e., teixits ili i jejú)

ff.plot<-function(data,funcg,genes,
                  l.dat.pre, 
                  #l.dat.pre=l.dat.pre.prova,  ## 3 tractaments 
                  treat, 
                  grup,
                  alf, 
              a,     # a+dx/2  per començar plotar "2-1"
              eps,   # y del plot a nivell x=0
              incy,  # alçada rectanglets
              nomsgrups,
              mida   # mida text en els rectangles
              #nomsgrups=c("Il","Je","N")
              ){ 
  # data=newData
  # funcg=functions$Funcions
  # genes = functions$Gens
  # l.dat.pre=llista
  # treat=input$covariables
  # grup="Teixit"
  # alf=input$alphaTukey 
  # a=0.3     # desplaçament de l'inici dels rectangles
  # eps=0.2   # y del plot a nivell x=0
  # incy=0.4  # alçada rectanglets
  # nomsgrups=c("Il","Je")
  # mida=1
  
  newData <- data
  levels_func<- data.frame(geneid=genes,funcio=funcg, colors=0)

  asig.funcgen<-function(nomgens) 
    {  
    n<-length(nomgens)
    nomfuncio<-character(n)  # i=1
    for (i in 1:n) {
      num<-which(levels_func$geneid==nomgens[i])
      nom<-as.character(levels_func$funcio[num])
      nomfuncio[i]<-nom}
      nomfuncio }
  
  # ngr<-length(l.dat.pre)   ## num teixits
  # ntr<-length(levels(as.factor(unlist(newData[,treat]))))
  # # ntr <- 3# num tractaments

  ngr<-length(l.dat.pre)   ## num teixits
  ntr<-length(levels(as.factor(l.dat.pre[[1]][,treat])))  # num tractaments
  noms.gr<-rev(levels(as.factor(unlist(newData[,grup]))))

## APLICACIÓ MASSIVA A TOTS ELS GENS  
  
 ## llista de dades d'expressió gènica, tots els grups 
  l.expr <- lapply(l.dat.pre, FUN="GetDataExpression",sepVar=treat)   
  ## això és una llista, conserva els noms
 ## taules anova  ## tot en classe 'list' 
  ## llista: output anova (stat, pval, p.BH) # amb alpha se seleccionen els significatius
  l.anov  <-lapply(l.expr,"GetPvalues",sepVar=treat,alpha=alf)  
 ## llista: noms dels gens significatius a cada grup ((teixit))
  l.gnames<-lapply(l.anov,function(ldf)rownames(ldf))
 ## llista: funcionalitats dels gens significatius a cada grup ((teixit))
  l.fnames<-lapply(l.gnames,function(ldf)asig.funcgen(ldf)) 
 ## aplicació de les comparacions múltiples amb correcció de Tukey, per grups 
  
## ELIMINAR GENS NO SIGNIFICATIUS  
  ########## -----------------eliminar gens no significatius, per a cada grup, abans de Tukey
  ## llista de dades, per grups, només amb els gens significatius de cada grup
  
  l.dat.signif<-list()
  length(l.dat.signif)<-ngr  
  for(i in 1:ngr){# i=1
    aa<-l.dat.pre[[i]]
    nomssig<-c(treat,l.gnames[[i]])
    aa.sig <- subset(aa, select=c(nomssig))
    l.dat.signif[[i]]<-aa.sig
    rownames(l.dat.signif[[i]])<-rownames(aa)
    }
   names(l.dat.signif)<-noms.gr 
   
  l.expr.signif <- lapply(l.dat.signif, FUN="GetDataExpression",sepVar=treat)   # expres gens signif 
  l.tukey<-lapply(l.expr.signif,FUN="Tukey_test") ## Tukey gens significatius

## TAULES output de TUKEY:p-valors, diferències i signes de ls diferències
  ## els objectes de les llistes són dataframes
  ## es generen a partir de "l.tukey"
  
  l.tuk.pval<-list()  ## llista de p-val Tukey gens significatius
  l.tuk.dif<-list()
  l.tuk.signe<-list()
  noms<-character()
  for(i in 1:ngr){ 
    l.tuk.pval[[i]]<-as.data.frame(t(sapply(l.tukey[[i]],function(x) 
     x$`as.factor(pData(dataExpression)[, 1])`[,4], USE.NAMES = F))) 
    l.tuk.dif[[i]]<-as.data.frame(t(sapply(l.tukey[[i]],function(x) 
     x$`as.factor(pData(dataExpression)[, 1])`[,1], USE.NAMES = F)))
    l.tuk.signe[[i]]<- as.data.frame(apply(as.matrix(l.tuk.dif[[i]]),c(1,2),function(x)as.numeric(x>0)))
    noms<-c(noms,rownames(l.tuk.pval[[i]]))
    }
  names(l.tuk.pval)  <-noms.gr     ## llista 2 objectes: A i B que són dataframes
  names(l.tuk.dif)   <-noms.gr     ## idem
  names(l.tuk.signe) <-noms.gr     ## idem
  noms.norep<-as.character(unique(noms))  # gens significatius no-repetits
  
## CREACIÓ DATAFRAME "df"  AMB ELS GENS SIGNIFICATIUS NO REPETITS I ORDENATS  
  
  ndf<-length(noms.norep)
  func<-asig.funcgen(noms.norep)              
  df<-data.frame(noms=noms.norep,func)
  df$noms<-as.character(df$noms)
  df<-df[order(df$func,df$noms),]     ## df ordenat per func i noms
  ### asigno número de símbol a la funció del gen
  df$simbfunc<-numeric(ndf)
  unifun<-unique(df$func)
  
##  AFEGIM "símbfun" és la pch que apareix segons el nom de la funció
  for (i in 1:ndf){df$simbfunc[df$func==unifun[i]]<-i-1}
  
## AFEGIM "grups": combinacions de lletres si el gen és significatiu en més de 1 grup
  df$grups<-character(ndf)
  for(i in 1:ngr){ ## i=1
      for (j in 1:ndf) {## j=1
      if (df$noms[j] %in% l.gnames[[i]] )  df$grups[j]<-paste(df$grups[j],names(l.tuk.pval)[i],sep="")}}
  ## df conté ara, per a cada gen no-repetit: nom, funcio, símbol-gràfic i grup: ordenats
 

## CREACIÓ DE LA LLISTA "l.df" per tenir els subsets  A, B,.. separats  
  
  ## separem els caràcters de la combinació de grup
  aux<-apply(df,1,function(v){strsplit(v[4],"1")}) ## llista de caràcters del nom de la variable grups
  aux<-lapply(aux,function(x)x[[1]])
  ## creem la llista de subsets de df per grup
  l.df<-list()
  length(l.df)<-ngr
  names(l.df)<-noms.gr
  for (i in 1:ngr){ ## i=1
    cond<-unlist(lapply(aux,function(v)noms.gr[i] %in% v))
    l.df[[i]] <-subset(df,cond)}
  
## CREACIÓ DE "l.df.complet" I DE "df.complet" (per incorporar p-valor, signe ...)   
  
  l.df.complet<-l.df
  for (i in 1:ngr) {# i=1
  pv<-l.tuk.pval[[i]]
  pv$noms<-rownames(pv)
  pv$func<-asig.funcgen(pv$noms) 
  signe<-l.tuk.signe[[i]]
  signe$noms<-rownames(signe)
  signe$func<-asig.funcgen(signe$noms)  ## signe binari (0 neg, 1 pos)
  l.df.complet[[i]]<- merge(l.df[[i]], pv,by=c("noms","func") )
  l.df.complet[[i]]<- merge(l.df.complet[[i]], signe,by=c("noms","func") )
  # ordenem cada dataframe de lallista
  l.df.complet[[i]]<-l.df.complet[[i]][order(l.df.complet[[i]]$func,l.df.complet[[i]]$noms),] 
  colnames(l.df.complet[[i]])[-c(1:4)]<-
        c(paste(names(l.df.complet[i]),".pval.",colnames(l.tuk.pval[[i]]),sep=""),
          paste(names(l.df.complet[i]),".signe.",colnames(l.tuk.signe[[i]]),sep=""))}
  ##  "l.df.complet" llista d'objectes: "A" amb gens signif. en A i potser altres gens, etc
  ##  cada objecte és ún dataframe amb columnes: 
  ### noms, func, simbfunc, grups, p-valors i signes (tants com parelles de comparacions )
  
 
  auxi<-df
  # ndf: num rows de df, definit abans
  ## columnes que afegirem a df: les dels objectes de "l.df.complet" 
  ### (menys les 4 primeres) multiplicades pel nombre de gupss
  ncol.grup<- ncol(l.df.complet[[1]])-4  ## dependrà del nombre de tractaments
  ncol.add.df<-ncol.grup*ngr 
  auxi<-cbind(auxi,matrix(NA,nrow=ndf,ncol=ncol.add.df))  
  cn.aux<-character() ## colnames auxi, llevat de les 4 primeres columnes
  for (i in 1:ngr) cn.aux<-c(cn.aux,colnames(l.df.complet[[i]])[-c(1:4)])
  colnames(auxi)[-c(1:4)]<-cn.aux    # hem inicialitzat auxi per crear df.complet
  
  # nombre de gens per grup
  ngens.grup<-lapply(l.df.complet,FUN=function(fitxer)nrow(fitxer))
  i.grup<-rep(1,ngr) ## (auxiliar per al for que ve seguidament) 
  cols<-numeric(ngr) ## vector amb els nums de les columnes que omplirem 
  cols<-seq(5,ncol(auxi),by=ncol.grup)
  
  for (i in 1:ngr) {# i=1  # primer grup
    col.omplir<-cols[i]:(cols[i]+ncol.grup-1) 
    for (j in 1:ndf){### j=1
      if (names(l.tuk.pval)[i] %in% strsplit(auxi$grup[j],"1")[[1]]) {
        auxi[j,col.omplir]<- l.df.complet[[i]][i.grup[i],-c(1:4)]  ## l.df.complet[[i]] és ordenat
        i.grup[i]<-i.grup[i]+1 }}}
  df.complet<-auxi 
  ##ncol(df.complet)-4
  
 
 ###################################  preparant el plot
  
  ##  comparacions 2 a 2
  comb<-combn(1:ntr,2)    
  ## nom de les comparacions
  compar<-paste(as.character(comb[2,]),as.character(comb[1,]),sep="-")
  ncompar<-length(compar)  ## num de comparacions
  ###   nc<-4 + ncompar          ## num de columnes del gràfic  crec que no cal !!!!!!!!!
 
 ### rangs de x's!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  mal calculat
  
  nrecmax<- 3*ncompar     ## num max de rec: suposant que hi poden haver tres grups, no més
  rgx    <- 4+ncompar   ## xlim serà entre 0 i rgx+1 
  dx.text<-(rgx+1)/rgx    
   ## posició eix horitzontal on dibuixar les quatre primeres columnes
  seqx.text<-c(0,dx.text/2,dx.text/2+dx.text,dx.text/2+2*dx.text) ## fixat ???
  ## posició eix horitzontal on dibuixar la resta de columnes
  ## a<-0.3 default
  dx<-(rgx+1-seqx.text[4]-a)/ncompar   ##  dx= distància max entre punts successius en x
                                          ### depén del num de comparacions
  
 
  sequ <- 0:(ncompar-1)
  ## a<-0.3 default
  seqx <- a+ seqx.text[4]+dx/2 +dx*sequ ## a: per centrar els tractaments
  
  #############  vell: seqx<-seq(seqx.text[4]+k*dx,rgx,by=dx)  ## k=cex (<1) on dibuixar

## LLISTES DE LES COORDENADES x INFERIOR I SUPERIOR DELS RECTANGLES: "l.xi"  i  "l.xs"  
  l.xi<-list()  ## limits inferiors coord x dels rectangles
  l.xs<-list()  ## limits inferiors coord x dels rectangles
  length(l.xi)<-ngr
  length(l.xs)<-ngr
  if (ngr==1){ ## ngr2=1
         kdx<-1/3 # fixem aquest kdx, si 1 grup
         del<-kdx*dx/2 
         l.xi[[1]]<- seqx-del  
         l.xs[[1]]<- seqx+del}  
  if (ngr==2){ ## ngr2=1
    kdx<-1/2  # fixem aquest kdx, si 2 grups
    del<-kdx*dx/2
    l.xi[[1]]<-seqx-del
    l.xs[[1]]<-seqx 
    l.xi[[2]]<-seqx
    l.xs[[2]]<-seqx+del}
  if (ngr==3){ ## ngr2=1
    kdx<-2/3  # ficem aquest kdx, si 3 grups
    del<-kdx*dx/3
    l.xi[[1]]<-seqx - del/2- 1*del
    l.xs[[1]]<-seqx - del/2
    l.xi[[2]]<-seqx - del/2
    l.xs[[2]]<-seqx + del/2 
    l.xi[[3]]<-seqx + del/2
    l.xs[[3]]<-seqx + del/2+1*del }      
         
  names(l.xi)<-noms.gr
  names(l.xs)<-noms.gr

## idem en llistes de matrius (repetint valors per fer quadrícula): "l.Axi"  i  "l.Axs" 
 l.Axi<-list()
 length(l.Axi)<-ngr
 l.Axs<-list()
 length(l.Axs)<-ngr
 for (i in 1:ngr) l.Axi[[i]]<-matrix(rep(l.xi[[i]],ndf),ncol=ncompar,byrow=T)  # coord x inferiors repetides 
 for (i in 1:ngr) l.Axs[[i]]<-matrix(rep(l.xs[[i]],ndf),ncol=ncompar,byrow=T)  # idem superiors
 names(l.Axi)<-noms.gr
 names(l.Axs)<-noms.gr

 ## MATRIUS DE LES COORDENADES y INFERIOR I SUPERIOR DELS RECTANGLES: "Bi"  i  "Bs"  
 y1<- (ndf:1)-eps
 y2<- y1+incy

 Bi<-matrix(rep(y1,length(compar)),ncol=ncompar,byrow=F)  # coord y1 rectangle
 Bs<-matrix(rep(y2,length(compar)),ncol=ncompar,byrow=F)  # coord y2 rectangle


 ## llista de MATRIUS DE p-valors i signes: "l.pval.comple"  i  "l.signe.comple" 
 l.pval.comple<-list()
 length(l.pval.comple)<-ngr
 c=0; ## i=1
 for (i in 1:ngr) {l.pval.comple[[i]]<-df.complet[,(5+c*ncompar):(5+(c+1)*ncompar-1)]; c<-c+2}
 l.signe.comple<-list()
 length(l.signe.comple)<-ngr
 c=1; ## i=1
 for (i in 1:ngr) {l.signe.comple[[i]]<-df.complet[,(5+c*ncompar):(5+(c+1)*ncompar-1)]; c<-c+2} 

 
 
 row.names <- c(ndf:1)
 l.array<-list()
 length(l.array)<-ngr
 for (i in 1:ngr){ # i=1
   column.names <- paste(names(l.Axi[i]),compar,sep="")
   matrix.names<-paste(names(l.Axi[i]), c("xinf","xsup","yinf","ysup","pval","sig"),sep="")
   l.array[[i]]<- array(c(as.vector(l.Axi[[i]]),as.vector(l.Axs[[i]]),as.vector(Bi),as.vector(Bs),
          as.vector(as.matrix(l.pval.comple[[i]])),as.vector(as.matrix(l.signe.comple[[i]]))), 
          dim = c(ndf,ncompar,6),
          dimnames = list(row.names ,column.names, matrix.names)) }
 names(l.array)<-noms.gr
##############   

 rgy<- nrow(df)+1
 delty<-(rgy)/(rgy-1)
 seqy<-seq(1,rgy,by=delty)
 nc <- 4+ncol(comb)

# start plotting text columns
 plot(1:ndf,rep(nc,ndf),type="n", xlab="", ylab="Gens significatius",
     xlim=c(0,rgx+1),ylim=c(1,rgy),xaxt="n",yaxt="n")

 points(rep(seqx.text[1],ndf),ndf:1,pch=df$simbfun,cex=1) # símbol funció
 text(rep(seqx.text[2],ndf),ndf:1,df$fun,cex=1)           # nom funció
 text(rep(seqx.text[3],ndf),ndf:1,df$noms,cex=1)          # nom gen
 text(rep(seqx.text[4],ndf),ndf:1,df$grup,cex=1)          # teixit
 
# start plotting significant comparisons
 ly<-length(seqy)
 ytop<-seqy[ly]
 lx<-length(seqx)
 text(seqx,rep(ytop,lx),compar,cex=1)   # nom comparació parella
 ## points(seqx,rep(ytop-0.3,lx),pch=21,cex=.7)
 
### PLOT DELS RECTANGLES AMB LA FUNCIÓ "plot.grup"
 if (is.null(nomsgrups)) nomrec<-names(l.array) else nomrec<-rev(nomsgrups)
 ##################for (i in (1:ngr)) apply(l.array[[i]],1:2,"plot.grup",nom=names(l.array[i])) 
 for (i in (1:ngr)) apply(l.array[[i]],1:2,"plot.grup",nom=nomrec[i],cexrec=1) 
 coords<-"topleft"
 legend(coords,  horiz = T,xpd=T,legend = c("und.expr","over.expr"),
        col = c("blue","red"),lty= 1,lwd =5,cex= 0.7 )
} 


##################  not run  #-------------------------------- proves -----------------------------------# 

## Executant

#ff.plot()   # alçada rectanglets)  
## default is below  
#ff.plot(funcg=funcg.INT, l.dat.pre=l.dat.pre.INT, 
#                  treat="trat", grup="Teixit", alf=0.10, 
#                  a=0.3,  # desplaçament de l'inici dels rectangles
#                  eps=0.3,   # y del plot a nivell x=0
#                  incy=0.4,   # alçada rectanglets
#                  nomsgrups=c("Il","Je"))
#ff.plot(eps=0.2)  ## millor  


# afegim un nou grup C, que sigui com A
#l.dat.pre.prova<-l.dat.pre.INT
#l.dat.pre.prova$C<-l.dat.pre.INT$A
# ff.plot(l.dat.pre=l.dat.pre.prova, a=0.5, eps=0.2,nomsgrups=c("Il","Je","N"))

### -------------------------------------------------------------------------------------------------------#

