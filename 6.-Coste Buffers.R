setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(raster)
library(gdistance)
library(rgdal)

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
