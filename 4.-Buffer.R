setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R")

library(dplyr)
library(sp)
library(rgeos)
library(rgdal)
library(vegan)
library(betapart)

#load data
puntos<-readOGR(dsn = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS", layer="Inventarios_PI")
puntos.df<-as.data.frame(puntos@data)
sc<-readOGR(dsn = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/GIS", layer="Subcuenca_c_800")
  
#### Buffers ####
#W viene del script "5.-Area de los buffers": con un redondeo, 4339374683 pasa a 4500000000
i<-1
j<-1
w<-4500000000

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

#### Riqueza regional ####
#especies por inventarios (tabla original, antes de registros únicos)
rib1<-read.csv("/Users/marialeomontes/Dropbox/Riqueza vegetación riparia/ARTÍCULO/Datos/Inventarios Libro Sur 2015 18122015.csv", header = T, sep = ";")

#eliminamos las especies que salen dos veces (a veces hay una especie duplicada porque tiene dos biotipos)
rib1<-rib1[,c("Tesela","TAXON..revisado.en.azul.")]
rib1 <- unique(rib1)

#tabla de contingencias
ribcont<-table(rib1$Tesela,rib1$TAXON..revisado.en.azul. )

#ni es el número de buffers que es igual al número de puntos o longitud de la matriz res
ni<-662
#estrichL es la lista que almacena las 8 matrices estrich, que contienen los resultados de
#las estimas de las riquezas regionales en los buffers (1/escala)
estrichL<-list()
estrich<-matrix(0,ni,4)
colnames(estrich)<-c("species","chao","chao.se","boot")

i<-1
j<-1

for (j in 1:8){ 
  for (i in 1:ni){
    #df tiene aquellos inventarios que se encuentran dentro del buffer
  df<-ribcont[res[[j]][,i]==1,]
  
      if (class(df)=="integer") {       #con estas dos lineas hacemos que si hay un único
      estrich[i,1:4]<-"NA"} else        #punto por buffer no lo calcule (no se puede)
       {pool <- specpool(df)
        estrich[i,1]<-pool$Species
        estrich[i,2]<-pool$chao
        estrich[i,3]<-pool$chao.se
        estrich[i,4]<-pool$boot
       }
    }
      estrichL[[j]]<-estrich
      print(c(j,i))
}

#boxplot de como cambia la riqueza regional con la escala
boxplot(as.numeric(estrichL[[1]][,1]),as.numeric(estrichL[[2]][,1]),as.numeric(estrichL[[3]][,1]),
        as.numeric(estrichL[[4]][,1]),as.numeric(estrichL[[5]][,1]),as.numeric(estrichL[[6]][,1]),
        as.numeric(estrichL[[7]][,1]),as.numeric(estrichL[[8]][,1]), main="Riqueza regional en buffers")


#### Juntar riqueza local ####

lrich<-read.csv("/Users/marialeomontes/Dropbox/LocalRegional/Datos/Estrich/localrich_PI.csv", sep=";")
allrich<-list()

i<-1
for (i in 1:8){
  allrich[[i]]<-cbind(x=estrichL[[i]], y=lrich)
}

#marranada con las riquezas locales y regionales (tarda mucho el loop de riqueza regional)
write.csv(as.data.frame(allrich[[1]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich1.csv")
write.csv(as.data.frame(allrich[[2]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich2.csv")
write.csv(as.data.frame(allrich[[3]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich3.csv")
write.csv(as.data.frame(allrich[[4]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich4.csv")
write.csv(as.data.frame(allrich[[5]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich5.csv")
write.csv(as.data.frame(allrich[[6]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich6.csv")
write.csv(as.data.frame(allrich[[7]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich7.csv")
write.csv(as.data.frame(allrich[[8]]), file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Analisis_R/allrich8.csv")

#### Correlaciones riqueza local y regional ####
correl<-matrix(NA,nrow = 8,ncol = 3) #matriz almacenaje de resultados: nrow=escalas, ncol=nestimadores
colnames(correl)<-c("Chao","Boot","Species")

#loop por num estimadores
i<-1
j<-1
for (j in 1:3) { 
  #loop por escalas  
  for (i in 1:8){ 
    c <- c(1,2,4) #la columna 3 tiene la sd del Chao que no nos interesa para correlacionar
    z=as.numeric(allrich[[i]][,c[j]]) 
    m=allrich[[i]]$y.LocalRichness_l
    correl[i,j]<-cor(x=m, y=z, use="complete")
    }
}

#ploteamos los 3 estimadores en un solo grafico
plot(c(100,200,300,400,500,600,700,800),correl[,1],ylim=c(-0.15,0.4), 
     xlab = "Scale", ylab = "Correlation", main="Correlation en buffers")
points(c(100,200,300,400,500,600,700,800),correl[,2],col="red")
points(c(100,200,300,400,500,600,700,800),correl[,3],col="blue")
legend("topleft", pch=21 , col = c("black", "red", "blue"), legend=c("Chao", "Boot", "Species"))


#### Beta ####
resbeta<-list()
resbetaL<-matrix(0,662,3)
colnames(resbetaL)<-c("beta.SIM", "beta.SNE", "beta.SOR")

i<-1
j<-1
for (j in 1:8){ 
  for (i in 1:662){
    #df tiene aquellos inventarios que se encuentran dentro del buffer
    df<-ribcont[res[[j]][,i]==1,]
    beta<-beta.multi(x = df, index.family = "sorensen")
    resbetaL[i,] <- matrix(unlist(beta), ncol = 3, byrow = TRUE)
    
  }
  resbeta[[j]]<-resbetaL
  print(c(j,i))
}

#boxplots Beta
boxplot(resbeta[[1]][,1],resbeta[[2]][,1],resbeta[[3]][,1],resbeta[[4]][,1],resbeta[[5]][,1],
        resbeta[[6]][,1],resbeta[[7]][,1],resbeta[[8]][,1],main="Beta.SIM en buffers", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )
boxplot(resbeta[[1]][,2],resbeta[[2]][,2],resbeta[[3]][,2],resbeta[[4]][,2],resbeta[[5]][,2],
        resbeta[[6]][,2],resbeta[[7]][,2],resbeta[[8]][,2],main="Beta.SNE en buffers", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )
boxplot(resbeta[[1]][,3],resbeta[[2]][,3],resbeta[[3]][,3],resbeta[[4]][,3],resbeta[[5]][,3],
        resbeta[[6]][,3],resbeta[[7]][,3],resbeta[[8]][,3],main="Beta.SOR en buffers", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )

