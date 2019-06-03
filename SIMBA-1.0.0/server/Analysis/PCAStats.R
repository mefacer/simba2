require(FactoMineR)

# levels_func <- data.frame(geneid=functions$Gens,funcio=functions$Funcions, colors=0)
# rownames(levels_func)<-functions$Gens
# 
# for(i in 1:nrow(levels_func)){
#   if(levels_func[i,2]=="BF"){
#     levels_func[i,3]="dodgerblue"
#   }else if(levels_func[i,2]=="OX"){
#     levels_func[i,3]="firebrick1"
#   }else if(levels_func[i,2]=="IR"){
#     levels_func[i,3]="chartreuse1"
#   }else if(levels_func[i,2]=="NT"){
#     levels_func[i,3]="orange1"
#   }else if(levels_func[i,2]=="EH"){
#     levels_func[i,3]="gray48"
#   }else{
#     levels_func[i,3]="aquamarine1"   ## funció "S"
#   }
# }

plot_pca_mitjanes <- function(dades){
  pcailtr<-PCA(dades,quali.sup=4,graph=F)
  
  ## setwd("C:/Users/farre/Desktop/PINSO/ExprGENETICA/DOCS.Finals/bioiberica/fitxers14maig2019/aovNA")
  ## tiff("PCAIli_ambNA.tiff",height =3.8, width = 7.5, units = 'in', type="cairo", res=300)
  
  par(mfrow = c(1,2),
      oma = c(0,0,0,0) + 0.7,
      mar = c(4,4,3,2) + 0.2,
      lwd=1.9, cex.main=.9, cex=.7,  cex.lab=.8,cex.axis=.8) 
  
  ## variables plot (treatments means plot)
  plot(pcailtr,choix="var",col.var="blue") 
  
  nomgenaux<-rownames(pcailtr$ind$coord)
  nomsquali<-rownames(pcailtr$quali.sup$coord)
  ## individuals plot
  plot.PCA(pcailtr,choix="ind",invisible="quali",label="none") 

    # funcColInd <- as.character(levels_func[nomgenaux,3])
  # cols<-asig.col(nomsquali,levels_func) 
  # plot.PCA(pcailtr,choix="ind",col.ind=funcColInd,invisible="quali",label="none") 
  # legend("topright",nomsquali, cex=0.6, col=cols,lty=1,bg="white")  
}


funcpca<-function(data,eixos=c(1,2),nomstr=nomstracs,gsignif=tab1, nivellsfunc=levels_func,
                  coltract=c("black","orange","red"), gruix=1.7, gris=4, limcos2=0.5,
                  cextit=.7,cexleg=.6,cexlab=.6,cexax=.6,cexlletra=.7)
{ # data=datJejunal[,-c(1:3)]
  # gsignif=tab1
  # eixos=c(1,3)
  datas<-data
  colnames(datas)[1] <- "Treatment"

  require(FactoMineR)
  require(missMDA)
  ## impute NAs with EM-algorithm to improve default imputation by mean value

  if (sum(is.na(datas))!=0){
    datas.i_1<-imputePCA(datas[-1])$completeObs
    # sum(is.na(datas.i_1)) ## must be 0
    datas.i<-data.frame(datas[1],datas.i_1)
  }
  else datas.i<-datas

  pcaout<-PCA(datas.i,quali.sup=1,graph=F)

  nomgen<-colnames(datas.i[-1])  ## only genes
  nomfun<-asig.func(genes=nomgen,funs=nivellsfunc)

  par(oma = c(0,0,0,0) + 0.5,
      mar = c(4,4,1,2) + 0.2)

  ## individuals graph
  nlev<-length(nomstr)
  plot(pcaout,choix="ind",axes=eixos, invisible="quali", habillage=1, col.hab=coltract,
       lwd=gruix, cex.main=cextit, cex=cexlletra, cex.lab=cexlab, cex.axis=cexax)
  legend("topleft", nomstr, cex=cexleg, col=coltract,lty=1, title="Treatment",bg="white")

  ## variables graph
  ## only genes with a quality over threshold in limcos2
  ###  and being significant, that is, belong to the table gsignif

  pcaux<-pcaout
  pcaqual2<-apply(pcaux$var$cos2[,eixos],1,sum)
  ## subset of pcaout
  ### significant genes and quality over threshold
  pcaux$var$coord<-subset(pcaux$var$coord,rownames(pcaux$var$coord)%in% rownames(gsignif)& pcaqual2 > limcos2)
  pcaux$var$cos2 <-subset(pcaux$var$cos2,rownames(pcaux$var$cos2)%in% rownames(gsignif)& pcaqual2 > limcos2)

  # gray color is the baseline color for variables, overprinted with functions colors afterwards
  colorbase<-gray.colors(1,0.3+gris*0.05,0.3+gris*0.1)
  plot(pcaux, axes=eixos, choix="var", col.var=colorbase,
       lwd=gruix, cex.main=cextit, cex=cexlletra*.8,  cex.lab=cexlab,cex.axis=cexax)

  # same colors as in heatmap
  ng<-nrow(pcaux$var$coord)
  nomgenaux<-rownames(pcaux$var$coord)
  nomfuncaux<-asig.func(nomgenaux,nivellsfunc)
  colaux<-character()
  for (i in 1:ng){## i<-1
    fila<-which(rownames(levels_func)==nomgenaux[i])
    colaux[i]<-levels_func[fila,3]
  }
  for (i in 1:ng)
    arrows(x0=0,y0=0,x1=pcaux$var$coord[i,eixos[1]],
           y1=pcaux$var$coord[i,eixos[2]],col=colaux[i], angle = 14,length=.1)
  # legend
  colors<-unique(colaux)
  funcaux<-unique(nomfuncaux)
  legend("topleft",legend=funcaux, col=colors,cex=cexleg,lty=1,title="Functions")
}
