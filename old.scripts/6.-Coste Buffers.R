setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(raster)
library(gdistance)
library(rgdal)
library(dplyr)
library(sp)
library(rgeos)
library(vegan)
library(betapart)

#### Preparar datos de coste ####
#load layers
mdt <- raster("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS/mdt_100m_surPI.tif")
coord <- readOGR(dsn = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS", layer="Inventarios_PI")

#crear capa de transición. TransitionFunction es el valor de las dos celdas
#que están siendo conectadas. Directions es el nº de vecinos con el que conecta
tr1 <- transition(x = mdt, transitionFunction = mean, directions = 4)

#Convertir cooordenadas en spatial points
coord1 <- SpatialPoints(coords = coord)

#cost distance
cost <- costDistance(x = tr1, fromCoords = coord1, toCoords = coord1)

#open data Calatayud (results.Rdata)
#load("/Users/marialeomontes/Downloads/results.RData")
#save.image("results2.RData")

#### Resultados matriz de coste y cost distance ####

load("results2.RData")

#calcular el tamaño de los buffers de coste. De lo que hemos hecho en el archivo 5.-Area
#de los buffers, tenemos que aumentar un 1.7% del coste máximo cada vez, es decir
# w <- (1.7*max(cost))/100
#w es para definir el umbral de los buffers
w <- (1.7 * max(cost)) / 100
ws <- w*1:8


#### Buffers ####
# hay que cargar lrich para poder poner los nombres antes del apply largo. Ten cuidado.
lrich <- read.csv("/Users/marialeomontes/Dropbox/LocalRegional/Datos/Estrich/localrich_PI.csv", sep=";")
colunames<-lrich[,1]
colnames(cost) <- colunames
rownames(cost) <- colunames

##ex para hacero una vez y ver como va ####
#seleccionando los puntos de la columna 2 menores a w
# i<- cost[,2] < w
# table(i) #contar TF

####vectorizacion. ####
#calcular todos los buffers (puntos menores a w, es decir, i) de cada columna
#buffer es un ejemplo para un caso, buffers para muchos casos
# buffer <- apply(cost,2,function(x){x<w})

buffers <- lapply(ws, function(w){
          apply(cost,2,function(x){
                x<w})
  })

#### Riqueza regional ####

#tabla de contingencias
#especies por inventarios (tabla original, antes de registros únicos)
rib1 <- read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/Inventario_LibroSurPI_coord.csv", header=T, sep=",")
#eliminamos las especies que salen dos veces (a veces hay una especie duplicada porque tiene dos biotipos)
rib1 <- rib1[,c("Tesela","TAXON..revisado.en.azul.")]
rib1 <- unique(rib1)
#tabla de contingencias
ribcont <- table(rib1$Tesela,rib1$TAXON..revisado.en.azul. )

#function para calcular las riquezas regionales para cada buffer
indexes <- function (x) {
  pool <- specpool(ribcont[x,])
  Species <- pool$Species
  Chao <- pool$chao
  Chao.se <- pool$chao.se
  Boot <- pool$boot
  
  return(c("Species"=Species, "Chao"=Chao, "Chao.se"=Chao.se, "Boot"=Boot))
}

#aplicar indexes
# estrich<-apply(buffer,2,indexes)

#aplicar indexes muchas veces
estrichL<- lapply(buffers, function(buffer){
          apply(buffer,2,indexes)})

#boxplot de como cambia la riqueza regional con la escala
boxplot(as.numeric(estrichL[[1]][,1]),as.numeric(estrichL[[2]][,1]),as.numeric(estrichL[[3]][,1]),
        as.numeric(estrichL[[4]][,1]),as.numeric(estrichL[[5]][,1]),as.numeric(estrichL[[6]][,1]),
        as.numeric(estrichL[[7]][,1]),as.numeric(estrichL[[8]][,1]), main="Riqueza regional con costes")

#### Juntar riqueza local ####

#fusionar lrich con la lista
lrich <- read.csv("/Users/marialeomontes/Dropbox/LocalRegional/Datos/Estrich/localrich_PI.csv", sep=";")
#transponer lrich usando la 1a columna como colnames para que tenga las mismas dimensiones
#que los elementos de la lista
tlrich <- setNames(data.frame(t(lrich[,-1])), lrich[,1])
# colunames<-lrich[,1]
#que coincidan los nombres de lrich y estrichL[[i]]
# estrichL <- lapply(estrichL1,setNames,colunames)

#pegar lrich a cada estrichl por nombre
allrich2 <- lapply(estrichL, function(c){
            rbind(c, "LRich" = tlrich)
})

allrich <- lapply(allrich2, t)

#### Correlaciones riqueza local y regional ####

#function para calcular las correlaciones para un elemento de la lista
# revisar allrich porque hay malas estimaciones regionales
corr <- function(x){
    x <- as.data.frame(apply(x,2,as.numeric))
    corS <- cor(x$Species, x$LRich, use="complete")
    corC <- cor(x$Chao, x$LRich, use="complete")
    corB <- cor(x$Boot, x$LRich, use="complete")
    
    return(c("corS"=corS, "corC"=corC, "corB"=corB))
}

#con esto aplicamos correl a 1 elemento de la lista
correl <- lapply(allrich, corr)
co <- t(as.data.frame(correl))
rownames(co) <- paste0("Scale_", 1:8)
plot.ts(co, plot.type = "single", col=c("black", "red", "blue"), lwd=2, ylim=c(-0.2, 0.4), main="Correl riqueza local y regional en costes")

#### Beta ####

betaL <- lapply(buffers, function(buffer){
  apply(buffer,2,function(x){beta.multi(ribcont[x,])})})

resbeta <- lapply(X = betaL, function(x){do.call(rbind, x)})

beta.SIM <- mapply(function(x){do.call(rbind, (x[,1]))}, resbeta)
beta.SNE <- mapply(function(x){do.call(rbind, (x[,2]))}, resbeta)
beta.SOR <- mapply(function(x){do.call(rbind, (x[,3]))}, resbeta)

boxplot(beta.SIM, main="Beta.SIM en buffers de coste")
boxplot(beta.SNE, main="Beta.SNE en buffers de coste")
boxplot(beta.SOR, main="Beta.SOR en buffers de coste")
