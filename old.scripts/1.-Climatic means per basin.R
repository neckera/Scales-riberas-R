setwd("/Users/marialeomontes/Dropbox/LocalRegional/Datos")
cc<-read.csv("CoordClim.csv", header=T, sep=";")
library(dplyr)
library(ggplot2)
#quitamos los NA
ccc<-cc[complete.cases(cc),]

#calculamos las medias (funs(mean)) por SC para cada variable####
seq<-seq(from=200, to=800,by=100)
names<-paste("SC", seq(from=200, to=800,by=100), sep="")

means<-list()
for (i in 1:7){
  ccc2<-select(ccc,num_range("SC", seq[i]),AMT, MeanTColdQ, AP, PColdQ)
  means[[i]]<-
    ccc2 %>% group_by_(names[i]) %>% summarize_each(funs(mean))
}

#forma sucia de guardar cada elemento de la lista####
sc1<-means[[1]]
sc2<-means[[2]]
sc3<-means[[3]]
sc4<-means[[4]]
sc5<-means[[5]]
sc6<-means[[6]]
sc7<-means[[7]]

#vemos cuantos puntos hay por cuenca (a cada escala) para ver si hay cuencas
#con demasiados pocos puntos. ####

ccc_long <- gather(ccc, escalas, SC, SC200:SC800)
ccc_count<- 
  ccc_long %>% group_by(escalas,SC) %>% 
 
    summarise(length(SC))
#guarrada mientras el loop no funciona
a2<-count(ccc, SC200)
a3<-count(ccc, SC300)
a4<-count(ccc, SC400)
a5<-count(ccc, SC500)
a6<-count(ccc, SC600)
a7<-count(ccc, SC700)
a8<-count(ccc, SC800)

#####
#juntamos la riqueza regional con todos estos parametros. 
#estrich es la matriz que viene de 1.-Accumulation curves que es previa al leftjoin

c200<-left_join(x=sc1, y=sc200, by="SC200")
c200<-c200[complete.cases(c200),]

c300<-left_join(x=sc2, y=sc300, by="SC300")
c300<-c300[complete.cases(c300),]
c400<-left_join(x=sc3, y=sc400, by="SC400")
c400<-c400[complete.cases(c400),]

c500<-left_join(x=sc4, y=sc500, by="SC500")
c500<-c500[complete.cases(c500),]

c600<-left_join(x=sc5, y=sc600, by="SC600")
c600<-c600[complete.cases(c600),]

c700<-left_join(x=sc6, y=sc700, by="SC700")
c700<-c700[complete.cases(c700),]

c800<-left_join(x=sc7, y=sc800, by="SC800")
c800<-c800[complete.cases(c800),]

#Loop para correl ####
clim<-c("AMT", "MeanTColdQ", "AP", "PColdQ") #vector que contenga los nombres de los par clim
names<-paste("c", seq(from=200, to=800,by=100), sep="")
#concatena nuestros vectores del mismo nombre+numero
correl<-matrix(NA,nrow = 7,ncol = 4) #matriz almacenaje de resultados: nrow=escalas, ncol=nestimadores
for (j in 1:4){ #loop por datos clim
  for (i in 1:7){ #loop por escalas
    z=get(names[i])[,clim[j]]
    m=get(names[i])$Chao
    correl[i,j]<-cor(x=m, y=z, use="complete")
  }}

plot(c(200,300,400,500,600,700,800),correl[,1],ylim=c(-0.25,0.30),
     xlab = "Scale", ylab = "Correlation")
points(c(200,300,400,500,600,700,800),correl[,2],col="red")
points(c(200,300,400,500,600,700,800),correl[,3],col="blue")
points(c(200,300,400,500,600,700,800),correl[,4],col="green")
legend("topleft", cex=0.6, pch=21 ,col = c("black", "red", "blue", "green"), legend=c("AMT", "MeanTColdQ", "AP", "PColdQ"))


