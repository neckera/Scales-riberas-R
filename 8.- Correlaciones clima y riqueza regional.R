setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(dplyr)
library(rgdal)
#### Create ClimPoints ####

cc <- read.csv("/Users/marialeomontes/Dropbox/LocalRegional/Datos/CoordClim.csv", header=T, sep=";")
ccc <- cc[complete.cases(cc),]
ccc <- dplyr::filter(ccc, ccc$Lon < 1)
Climpoints <- SpatialPointsDataFrame(coords = ccc[,2:1], data = ccc[,10:13], proj4string = CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"))

#### Buffers ####

w <- 4500000000
ws <- sqrt(w/pi*1:8)

buffers <- lapply(ws, function(w){
  apply(cost,2,function(x){
    x<w})
  })

#### Unir riqueza regional ####
Climpoints <- left_join(Climpoints, estrichL[[1:8]][,1] )
