setwd("/Users/marialeomontes/Dropbox/LocalRegional/Datos/Estrich")
library(dplyr)
library(ggplot2)

#load data
sc200<-read.csv("rib_estrich1.csv", sep=";", header = T)
sc300<-read.csv("rib_estrich2.csv", sep=";", header = T)
sc400<-read.csv("rib_estrich3.csv", sep=";", header = T)
sc500<-read.csv("rib_estrich4.csv", sep=";", header = T)
sc600<-read.csv("rib_estrich5.csv", sep=";", header = T)
sc700<-read.csv("rib_estrich6.csv", sep=";", header = T)
sc800<-read.csv("rib_estrich7.csv", sep=";", header = T)

#remove na
sc200<-na.omit(sc200)
sc300<-na.omit(sc300)
sc400<-na.omit(sc400)
sc500<-na.omit(sc500)
sc600<-na.omit(sc600)
sc700<-na.omit(sc700)
sc800<-na.omit(sc800)

#cargar teselas y añadirles la riqueza local. lo hacemos para cada SC ####
teselas<-read.csv("rib_curvasacumulacion.csv", sep=";",header=T)
riq<-read.csv("localrich.csv", sep=";", header=T)

teselas2<-left_join(x=teselas, y=riq, by="Tesela")
teselas2<-left_join(x=teselas2, y=sc200, by="SC200")

teselas3<-left_join(x=teselas, y=riq, by="Tesela")
teselas3<-left_join(x=teselas3, y=sc300, by="SC300")

teselas4<-left_join(x=teselas, y=riq, by="Tesela")
teselas4<-left_join(x=teselas4, y=sc400, by="SC400")

teselas5<-left_join(x=teselas, y=riq, by="Tesela")
teselas5<-left_join(x=teselas5, y=sc500, by="SC500")

teselas6<-left_join(x=teselas, y=riq, by="Tesela")
teselas6<-left_join(x=teselas6, y=sc600, by="SC600")

teselas7<-left_join(x=teselas, y=riq, by="Tesela")
teselas7<-left_join(x=teselas7, y=sc700, by="SC700")

teselas8<-left_join(x=teselas, y=riq, by="Tesela")
teselas8<-left_join(x=teselas8, y=sc800, by="SC800")

ggplot(data=teselas2, aes(y=teselas2$Chao, x=teselas2$LocalRichness_l)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

#hacemos un plot con las correlaciones entre riqueza local y regional en ####
#el eje y y las distancias en el eje x

est<-c("Chao","Boot", "Species") #vector que contenga los nombres de los estimadores
names<-paste("teselas",c(2:8),sep = "") #concatena nuestros vectores del mismo nombre+numero
correl<-matrix(NA,nrow = 7,ncol = 3) #matriz almacenaje de resultados: nrow=escalas, ncol=nestimadores
for (j in 1:3){ #loop por num estimadores
for (i in 1:7){ #loop por escalas
z=get(names[i])[,est[j]]
m=get(names[i])$LocalRichness_l
correl[i,j]<-cor(x=m, y=z, use="complete")
}}

#ploteamos los 3 estimadores en un solo grafico
plot(c(200,300,400,500,600,700,800),correl[,1],ylim=c(0.1,.35), 
     xlab = "Scale", ylab = "Correlation")
points(c(200,300,400,500,600,700,800),correl[,2],col="red")
points(c(200,300,400,500,600,700,800),correl[,3],col="blue")
legend("topright", pch=21 ,col = c("black", "red", "blue"), legend=c("Chao", "Boot", "Species"))

cambio esn el branch
gitpush