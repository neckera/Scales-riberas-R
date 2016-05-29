setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(raster)
library(gdistance)
library(rgdal)
library(dplyr)
library(sp)
library(rgeos)
library(vegan)
library(betapart)


#load layers
mdt<-raster("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS/mdt_100m_surPI.tif")
coord<-readOGR(dsn = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS", layer="Inventarios_PI")

#crear capa de transición. TransitionFunction es el valor de las dos celdas
#que están siendo conectadas. Directions es el nº de vecinos con el que conecta
tr1<-transition(x = mdt, transitionFunction = mean, directions = 4)

#Convertir cooordenadas en spatial points
coord1<- SpatialPoints(coords = coord)

#cost distance
cost<-costDistance(x = tr1, fromCoords = coord1, toCoords = coord1)

#open data Calatayud (results.Rdata)
load("/Users/marialeomontes/Downloads/results.RData")

#calcular el tamaño de los buffers de coste. De lo que hemos hecho en el archivo 5.-Area
#de los buffers, tenemos que aumentar un 1.7% del coste máximo cada vez, es decir
w<-(1.7*max(cost))/100


#### Buffers ####


i<-1
j<-1
w<-(1.7*max(cost))/100

res<-list()
for (j in 1:8){
  rek<-data.frame(matrix(0,ncol=662,nrow=662)) #crea un df para almacenar 1 y 0 de los inventarios que caen en cada buffer
  rownames(rek)<-as.character(puntos@data$Tesela) 
  
  for (i in 1:662){
    p <- SpatialPoints(puntos[i,c(2,3)])          #cada punto
    proj4string(p) <- CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")
    bf<-gBuffer(spgeom = p, width = sqrt(j*w/pi) , byid = T) #buffer para un punto
    a<-puntos[bf,]     #puntos que caen en el buffer 
    rek[rownames(rek)%in%as.character(a@data$Tesela),i]<-1   #da valor 1 a los inventarios que caen en el buffer
  }
  res[[j]]<-rek     #res es una lista que contiene 8 rek, uno por escala
  print(c(j,i))     #cuenta los pasos que lleva
}


