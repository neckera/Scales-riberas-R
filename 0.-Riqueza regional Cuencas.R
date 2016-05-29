setwd("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/")

library(dplyr)
library(vegan)
library(betapart)

#### Preparar datos ####
#load data
#Están todos los inventarios de la PI salvo "Higueron-1"
rib.ls<-read.csv("Inventarios_LibroSurPI.csv", header = T, sep = ";") 
rib.coordgis<-read.csv("Coord_Inv_PI.csv", header = T, sep = ";")

#unir coordenadas con los datos de sp por inventario
rib1<-tbl_df(left_join(rib.ls,rib.coordgis,by="Tesela"))

#codigo guardado para todas las sp de la bd
write.csv(rib1, file = "Inventario_LibroSurPI_coord.csv", row.names = FALSE)

#### Riqueza regional ####

#Calcular curvas de acumulación
#load data
rib1<-read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/Inventario_LibroSurPI_coord.csv", header=T, sep=",")
invdn<-read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/Inv_DNCuencas.csv", header=T, sep=";")

rib1T<-unique(rib1)
rib1<-rib1T

#Calcular una tabla de contingencias
ribcont<-table(rib1$Tesela,rib1$TAXON..revisado.en.azul. )

#Ahora le pegamos las columnas de las subcuencas a distintas escalas
ribcont1<-cbind(ribcont,  invdn$DN_100)
ribcont2<-cbind(ribcont1, invdn$DN_200)
ribcont3<-cbind(ribcont2, invdn$DN_300)
ribcont4<-cbind(ribcont3, invdn$DN_400)
ribcont5<-cbind(ribcont4, invdn$DN_500)
ribcont6<-cbind(ribcont5, invdn$DN_600)
ribcont7<-cbind(ribcont6, invdn$DN_700)
ribcont8<-cbind(ribcont7, invdn$DN_800)
ribcont<-as.data.frame(ribcont8)
colnames(ribcont)[1910:1917] <- c("DN_100", "DN_200", "DN_300", "DN_400", "DN_500", "DN_600", "DN_700", "DN_800")

#seleccionamos las columnas que tienen las sp
cont<-ribcont[,1:1909]
cont[cont>0]<-1
cont[is.na(cont)]<-0

#Creamos una lista para almacenar los resultados de las curvas de acumulación
estrichL<-list()
m<-colnames(ribcont[,1910:1917])

j<-1
i<-1

#hacemos un loop para evitar iterar por cada subcuenca manualmente
for (j in 1:8){
    cont$DN_100 <-ribcont[,m[j]]

    #Nos aseguramos de que la columna de SC/localidades/loquesea es un factor y hacemos un 
    #objeto que contiene los niveles del factor
    cont$DN_100<-factor(cont$DN_100)
    cont.lev<-levels(cont$DN_100)

    ni<-length(cont.lev)
    estrich<-data.frame()
    
    require(vegan)
        #loop para calcular curvas de acumulación
        for (i in 1:ni){
        df<-cont[cont$DN_100==cont.lev[i],]
        if (dim(df)[1]==1) {                     #con estas dos lineas hacemos que si hay un único
              estrich[i,1:4]<-"NA"} else        #punto por subcuenca no lo calcule (no se puede)
       {pool <- specpool(df)
        estrich[i,1]<-pool$Species
        estrich[i,2]<-pool$chao
        estrich[i,3]<-pool$chao.se
        estrich[i,4]<-pool$boot
        estrich[i,5]<-cont.lev[i]
        }
      }

    #aquí almacenamos los resultados 
    estrichL[[j]]<-estrich
    print(c(j,i))
}

boxplot(as.numeric(estrichL[[1]][,1]),as.numeric(estrichL[[2]][,1]),as.numeric(estrichL[[3]][,1]),
        as.numeric(estrichL[[4]][,1]),as.numeric(estrichL[[5]][,1]),as.numeric(estrichL[[6]][,1]),
        as.numeric(estrichL[[7]][,1]),as.numeric(estrichL[[8]][,1]), main="Riqueza regional en cuencas")

#### Guardar estrich ####
colnames(estrichL[[1]])<-c("species","chao","chao.se","boot","DN_100")
colnames(estrichL[[2]])<-c("species","chao","chao.se","boot","DN_200")
colnames(estrichL[[3]])<-c("species","chao","chao.se","boot","DN_300")
colnames(estrichL[[4]])<-c("species","chao","chao.se","boot","DN_400")
colnames(estrichL[[5]])<-c("species","chao","chao.se","boot","DN_500")
colnames(estrichL[[6]])<-c("species","chao","chao.se","boot","DN_600")
colnames(estrichL[[7]])<-c("species","chao","chao.se","boot","DN_700")
colnames(estrichL[[8]])<-c("species","chao","chao.se","boot","DN_800")

ribestrich1<-as.data.frame(estrichL[[1]])
ribestrich2<-as.data.frame(estrichL[[2]])
ribestrich3<-as.data.frame(estrichL[[3]])
ribestrich4<-as.data.frame(estrichL[[4]])
ribestrich5<-as.data.frame(estrichL[[5]])
ribestrich6<-as.data.frame(estrichL[[6]])
ribestrich7<-as.data.frame(estrichL[[7]])
ribestrich8<-as.data.frame(estrichL[[8]])

write.csv(ribestrich1, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich100.csv", row.names = FALSE)
write.csv(ribestrich2, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich200.csv", row.names = FALSE)
write.csv(ribestrich3, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich300.csv", row.names = FALSE)
write.csv(ribestrich4, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich400.csv", row.names = FALSE)
write.csv(ribestrich5, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich500.csv", row.names = FALSE)
write.csv(ribestrich6, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich600.csv", row.names = FALSE)
write.csv(ribestrich7, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich700.csv", row.names = FALSE)
write.csv(ribestrich8, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/EstrichCuencas/estrich800.csv", row.names = FALSE)

#### Unir riqueza local con los identificadores de las cuencas ####
lrich<-read.csv("localrich_PI.csv", header=T, sep=";")
curvac<-read.csv("rib_curvasacumulacion.csv", header=T, sep=";")

#comprobar que los nombres de los inventarios son iguales
setdiff(levels(lrich$Tesela), levels(invdn$Tesela))

base<-left_join(lrich, invdn, by="Tesela")
write.csv(base, file = "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/lrich_DNInv.csv", row.names = FALSE)
base<-read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/lrich_DNInv.csv", head=T, sep=",")

#### Unir riquezas locales y regionales en una sola tabla ####

base2<-merge(x=base, y=ribestrich1[,c(1,2,4,5)],"DN_100",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_100"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_100"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_100"
base2<-merge(x=base2, y=ribestrich2[,c(1,2,4,5)],"DN_200",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_200"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_200"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_200"
base2<-merge(x=base2, y=ribestrich3[,c(1,2,4,5)],"DN_300",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_300"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_300"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_300"
base2<-merge(x=base2, y=ribestrich4[,c(1,2,4,5)],"DN_400",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_400"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_400"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_400"
base2<-merge(x=base2, y=ribestrich5[,c(1,2,4,5)],"DN_500",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_500"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_500"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_500"
base2<-merge(x=base2, y=ribestrich6[,c(1,2,4,5)],"DN_600",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_600"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_600"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_600"
base2<-merge(x=base2, y=ribestrich7[,c(1,2,4,5)],"DN_700",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_700"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_700"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_700"
base2<-merge(x=base2, y=ribestrich8[,c(1,2,4,5)],"DN_800",all=T)
colnames(base2)[which(names(base2) == "species")] <- "RegRich_800"
colnames(base2)[which(names(base2) == "chao")] <- "Chao_800"
colnames(base2)[which(names(base2) == "boot")] <- "Boot_800"
base2<-base2[-c(663:1292),]
base2<-base2[,c(9,11,12,10,13:36,8:1)]

#guardar df 
write.csv(base2, "/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/TRich_Cuencas.csv", row.names = FALSE)

#### Correlaciones riqueza local y regional ####
i<-1
j<-1
correl<-matrix(NA,nrow = 8,ncol = 3) #matriz almacenaje de resultados: nrow=escalas, ncol=nestimadores
colnames(correl)<-c("Chao", "Boot", "RegRich")
est<-c("Chao_", "Boot_", "RegRich_")

for (j in 1:3){ #loop por num estimadores
  a<-dplyr::select(base2, num_range(est[j], c(100,200,300,400,500,600,700,800)))
  for (i in 1:8){ #loop por escalas
    z=as.numeric(a[,i])
    n=as.numeric(base2$LocalRichness_l)
    correl[i,j]<-cor(x=n, y=z, use="complete")
  }
  print(j,i)
}

#plot correlaciones
plot(c(100,200,300,400,500,600,700,800),correl[,1],ylim=c(-0.15, 0.4), 
     xlab = "Scale", ylab = "Correlation", main="Correlation en Cuencas")
points(c(100,200,300,400,500,600,700,800),correl[,2],col="red")
points(c(100,200,300,400,500,600,700,800),correl[,3],col="blue")
legend("bottomleft", pch=21 ,col = c("black", "red", "blue"), legend=c("Chao", "Boot", "Species"))


#### Beta ####
resbeta<-list()
colnames(resbetaL)<-c("beta.SIM", "beta.SNE", "beta.SOR")
m<-colnames(ribcont[,1910:1917])

i<-1
j<-1
for (j in 1:8){ 
  
  cont$DN_100 <-ribcont[,m[j]]
  cont$DN_100<-factor(cont$DN_100)
  cont.lev<-levels(cont$DN_100)
  ni<-length(cont.lev)
  resbetaL<-matrix(0,ni,3)
  
  for (i in 1:ni) {
    #df tiene los inventarios de cada cuenca
    df<-cont[cont$DN_100==cont.lev[i],]
    beta<-beta.multi(x = df[,-1910], index.family = "sorensen")
   
    resbetaL[i,] <- matrix(unlist(beta), ncol = 3, byrow = TRUE)
    
  }
  resbeta[[j]]<-resbetaL
  print(c(j,i))
}

#boxplots para cada Beta
boxplot(resbeta[[1]][,1],resbeta[[2]][,1],resbeta[[3]][,1],resbeta[[4]][,1],resbeta[[5]][,1],
        resbeta[[6]][,1],resbeta[[7]][,1],resbeta[[8]][,1],main="Beta.SIM en cuencas", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )
boxplot(resbeta[[1]][,2],resbeta[[2]][,2],resbeta[[3]][,2],resbeta[[4]][,2],resbeta[[5]][,2],
        resbeta[[6]][,2],resbeta[[7]][,2],resbeta[[8]][,2],main="Beta.SNE en cuencas", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )
boxplot(resbeta[[1]][,3],resbeta[[2]][,3],resbeta[[3]][,3],resbeta[[4]][,3],resbeta[[5]][,3],
        resbeta[[6]][,3],resbeta[[7]][,3],resbeta[[8]][,3],main="Beta.SOR en cuencas", 
        names=c("1","2","3","4","5","6","7","8"), ylim=c(0,1) )


