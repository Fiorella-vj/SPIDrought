#' @import SCI
#' @import xlsx
#' @title SPI and Drought Events Characteristics
#' @description This function allows to compute the Standarized Precipitation Index (SPI) based on an established. climatological period Esta es una funcion de ejemplo para multiplicar un valor o vector x por cuatro.
#' @param data a zoo object with time ordered values of monthly precipitation.
#' @param scale an integer value representing the time scale at which the SPI will be computed.
#' @param ini.clim optional, character date of starting point of the reference period used for computing the index (p.e. c("1981-01-01")).Defaults to NULL
#' @param fin.clim optional, character date of ending point of the reference period used for computing the index (p.e. c("2010-12-01")). Defaults to NULL
#' @param threshold optional, a negative integer value defining the threashold to use for drought events
#' based on SPI series. Defaults to -1.
#' @param distr distribution function to standarize data, could be: "gamma","gev" or "genlog". Default "gamma"
#' @details The fitSCI function allow computing the SPI index. Basically, the fitSCI function standardize the precipitation following a Gamma distribution function (i.e., they transform it to a standard Gaussian variate with zero mean and standard deviation of one).
#' @examples
#' SPIF(data,scale,ini.clim=NULL,fin.clim=NULL,threshold=NULL,distr="gamma")
#' @export

SPIF<-function(data,scale,ini.clim=NULL,fin.clim=NULL,threshold=NULL,distr="gamma"){
  scale<-as.numeric(scale)
  if ({
    is.null(ini.clim)
  } | {
    is.null(fin.clim)
  }) {
    ini.clim <- index(data)[1]
    fin.clim <- index(data)[nrow(data)]
  }

  if (!is.null(distr)) {
    distr <- "gamma"
  } else {
    distr <- distr
  }

  #SPI computing
  if (is.null(dim(data))){
    dataset<-data[,1]
    n.stations <- 1
    dist_zoo_spi<-dataset #par ala salida
    ini.clim <- ini.clim
    fin.clim <- fin.clim

    wind.dataset <- window(dataset[, i], start = ini.clim, end = fin.clim)
    spi.ref.para <- SCI::fitSCI(wind.dataset, first.mon = 1, distr = distr, time.scale = scale, p0 = TRUE)
    spi.ref <- SCI::transformSCI(coredata(dataset[, i]) , first.mon = 1, obj = spi.ref.para)
    dist_zoo_spi[]<- as.numeric(spi.ref)
    spi<-dist_zoo_spi

    #Remove -Inf and fill na values for all computed series
    a<-coredata(spi[][spi[]==-Inf])
    coredata(spi[][spi[]==-Inf])<-rep(0.1,length(a))
    #b<-coredata(spi[,1][is.na(spi[,1])])
    #coredata(spi[][is.na(spi[])])<-rep(0.1,length(b))

  } else {
    dataset<-data
    n.stations <- dim(dataset)[2]
    dist_zoo_spi<-dataset #par ala salida
    ini.clim <- ini.clim
    fin.clim <- fin.clim

    for(i in 1:n.stations){
      wind.dataset <- window(dataset[, i], start = ini.clim, end = fin.clim)
      spi.ref.para <- SCI::fitSCI(wind.dataset, first.mon = 1, distr = distr, time.scale = scale, p0 = TRUE)

      spi.ref <- SCI::transformSCI(coredata(dataset[, i]) , first.mon = 1, obj = spi.ref.para)
      dist_zoo_spi[,i]<- as.numeric(spi.ref)
    }
      spi<-dist_zoo_spi

      #Remove -Inf and fill na values for all computed series
      for (i in 1:ncol(spi)) {
        a<-coredata(spi[,i][spi[,i]==-Inf])
        coredata(spi[,i][spi[,i]==-Inf])<-rep(0.1,length(a))
        #b<-coredata(spi[,i][is.na(spi[,i])])
        #coredata(spi[,i][is.na(spi[,i])])<-rep(0.1,length(b))
      }
  }

  #Saving
  xlsx::write.xlsx(spi, file=paste0("SPI",scale,"series.xlsx"), sheetName="SPI",row.names=T)

  spi_df<-as.data.frame(spi)

  #Characterizing Events
  if (is.null(threshold)) {
    threshold <- -1
  }  else {
    threshold <- threshold
  }

  #FUNCION PARA EL CALCULO DE DURACION, SEVERIDAD, INTENSIDAD E INTERARRIVAL
  drought.index<-function(SPI,spi_escala,umbral){
    #Calculo de parametros de sequia: Intensidad, Duraci?n, Severidad e Interarrival.
    #SPI: serie de tiempo
    #spi_escala: escala a en la que se calculo el spi
    #umbral: valor minimo para calcular los parametros
    #
    #Adrian Huerta
    SPI<-SPI[spi_escala:length(SPI)]
    #------------------------ severidad, duracion e intensidad ------------------------
    sdi<-SPI
    sdi[sdi >= umbral]<-NA
    idx<- 1 + cumsum(is.na(sdi))
    no.NA<-!is.na(sdi)
    D.S<-split(sdi[no.NA],idx[no.NA])

    D<-matrix(nrow=length(D.S),ncol=1)
    S<-matrix(nrow=length(D.S),ncol=1)
    for (i in 1:length(D.S)){
      D[i]<- length(D.S[[i]])
      S[i]<- -1*sum(D.S[[i]])
    }
    I<-S/D
    rm(idx,no.NA)

    #---------------------------------- Interarrival ----------------------------------
    int<-SPI
    int[int < umbral]<-NA
    idx<- 1 + cumsum(is.na(int))
    no.NA<-!is.na(int)
    I.n<-split(int[no.NA],idx[no.NA])

    intera.raw<-matrix(nrow=length(I.n),ncol=1)
    for (i in 1:length(I.n)){
      intera.raw[i]<- length(I.n[[i]])
    }

    #1era condici?n.
    if (is.na(int[1])==FALSE){
      intera.y<-intera.raw[c(-1),]} else
      {intera.y<-intera.raw }

    #2da condicion
    if (is.na(int[length(int)])==FALSE){
      interarrival<-intera.y+D} else
      { n<-c(intera.y,0)+D
      interarrival<-matrix(n[-length(n)],ncol=1)}

    return(list(Duracion=D,Severidad=S,Intensidad=I,Interarrival=interarrival))
  }

  caract<-list()
  for (i in 1:ncol(spi_df)){#ncol(spi_peru3.df)#hay problemas con la columna 17
    SD<-drought.index(spi_df[,i],scale,threshold)
    caract[["Duracion"]][[i]]<-SD$Duracion
    caract[["Severidad"]][[i]]<-SD$Severidad
    caract[["Intensidad"]][[i]]<-SD$Intensidad
    caract[["Interarrival"]][[i]]<-SD$Interarrival
  }

  caract1<-list()
  for (j in 1:length(caract)) {
    mat<-matrix(data = NA,nrow = max(as.numeric(summary(caract[[j]])[,1])),ncol = ncol(spi_df))
    for (i in 1:ncol(mat)) {
      mat[1:length(caract[[j]][[i]]),i]<-caract[[j]][[i]]
      }
    caract1[[j]]<-mat
  }

  #Saving
  for (i in 1:length(caract1)) {
    name<-c("Duracion","Severidad","Intensidad","Interarrival")
    xlsx::write.xlsx(caract1[[i]], file=paste0("SPI",scale,"_Caract.xlsx"), sheetName=name[i], append=TRUE,row.names=FALSE)
  }

}


