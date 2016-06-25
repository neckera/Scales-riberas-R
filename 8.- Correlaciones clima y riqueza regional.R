setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(dplyr)
library(rgdal)
library(rgeos)

#### Create ClimPoints ####

cc <- read.csv("/Users/marialeomontes/Dropbox/LocalRegional/Datos/CoordClim.csv", header=T, sep=";")
ccc <- cc[complete.cases(cc),]
ccc <- dplyr::filter(ccc, ccc$Lon < 1)
Climpoints <- SpatialPointsDataFrame(coords = ccc[,2:1], data = ccc[,10:13], proj4string = CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"))

#### Load cuencas.shp and calculate areas ####
sc100<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_100km")
sc200<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_200km")
sc300<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_300km")
sc400<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_400km")
sc500<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_500km")
sc600<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_600km")
sc700<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_700km")
sc800<-readOGR(dsn = "/Users/marialeomontes/Downloads/zip capas corr alberto", layer="Subcuenca_c_800")

# calcular las áreas de cada polígono para todas las capas y después la mediana
# la idea es conseguir al final una tabla de dos columnas, en la primera la escala y en
# la segunda la mediana de tamaño de áreas de cuenca para cada escala. 
# INDRA: Esto es lo que no me sale

aa <- sapply(sc800@polygons, function(x) x@Polygons[[1]]@area)
med <- median(aa)

#### Buffers ####
# los buffers tendrán distinto tamaño una vez calculemos las cuencas. Hay que aplicar el 5
# w <- 4500000000
# ws <- sqrt(w/pi*1:8)

buffers <- lapply(ws, function(w){
  apply(DISTANCEMATRIX,2,function(x){
    x<w})
  })

#### Unir riqueza regional ####
# no es un leftjoin. 
# se ha borrado el códigooooo
Climpoints <- left_join(Climpoints, estrichL[[1:8]][,1] )


#### Calcular medianas de las áreas ####
# si consigo cargar las areas en qgis no problem, script 5. Si no, hay que calcular las areas aquí

#### Calcular riqueza regional ####

rib1 <- read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/Inventario_LibroSurPI_coord.csv", header=T, sep=",")
rib1 <- rib1[,c("Tesela","TAXON..revisado.en.azul.")]
rib1 <- unique(rib1)
ribcont <- table(rib1$Tesela,rib1$TAXON..revisado.en.azul. )

regrich <- function (x) {
  pool <- specpool(ribcont[x,])
  Species <- pool$Species
  
  return(c("Species"=Species))
}

estrichL <- lapply(buffers, function(buffer){
  apply(buffer,2,regrich)})

#### Calcular medias climáticas ####

medclim <- function(y) {
  
  AMTm <- mean(Climpoints$AMT[y,])
  MeanTColdm <- mean(Climpoints$MeanTCold[y,])
  APm <- mean(Climpoints$AP[y,])
  PColdQm <- mean(Climpoints$PColdQ[y,])
  
  return(c("AMTm"=AMTm, "MeanTColdm"=MeanTColdm,"APm"=APm,"PColdQm"=PColdQm))
}

#### Unir riqueza regional y variables climáticas ####


#### Calcular correlaciones ####

corc <- function(x){
  x <- as.data.frame(apply(x,2,as.numeric))
  corAMT <- cor(x$Species, x$AMTm, use="complete")
  corMeanTColdQ <- cor(x$Species, x$MeanTColdQ, use="complete")
  corAP <- cor(x$Species, x$AP, use="complete")
  corPColdQ <- cor(x$Species, x$PColdQ, use="complete")
  
  return(c("corAMT"=corAMT, "corMeanTColdQ"=corMeanTColdQ, "corAP"=corAP, "corPColdQ"=corPColdQ))
}

# falta meter la lista en el apply!
res <- lapply(list, corc)