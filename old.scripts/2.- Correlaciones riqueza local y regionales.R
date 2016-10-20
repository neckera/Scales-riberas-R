setwd("/Users/marialeomontes/Dropbox/LocalRegional/Datos/Estrich")
library(dplyr)
library(ggplot2)

#cargamos y juntamos los datos de riqueza local y SC por teselas####
lrich<-read.csv("localrich.csv", header=T, sep=";")
curvac<-read.csv("rib_curvasacumulacion.csv", header=T, sep=";")

b1<-left_join(lrich, curvac, by="Tesela")
b1<-na.omit(b1)
#cargamos y juntamos la riqueza regional en la misma tabla####

regrich<-left_join(b1, sc200[,c("Chao","SC200")], by="SC200", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao200"

regrich<-left_join(regrich, sc300[,c("Chao","SC300")], by="SC300", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao300"

regrich<-left_join(regrich, sc400[,c("Chao","SC400")], by="SC400", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao400"

regrich<-left_join(regrich, sc500[,c("Chao","SC500")], by="SC500", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao500"

regrich<-left_join(regrich, sc600[,c("Chao","SC600")], by="SC600", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao600"

regrich<-left_join(regrich, sc700[,c("Chao","SC700")], by="SC700", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao700"

regrich<-left_join(regrich, sc800[,c("Chao", "SC800")], by="SC800", copy=T)
colnames(regrich)[which(colnames(regrich)=="Chao")] <- "Chao800"

#Calculamos la correacion entre cada riqueza local y las riquezas regionales#####

names<-paste("Chao", seq(from=200, to=800,by=100), sep="")
correl<-matrix(NA,nrow = 7,ncol = 1)
for (i in 1:7){ #loop por escalas
  z=regrich[names[i]]
  m=regrich$LocalRichness_l
  correl[i]<-cor(x=m, y=z, use="complete")
}
plot(c(200,300,400,500,600,700,800),correl[,1],ylim=c(0.1,.35))

i<-1
j<-1
div<-natrix(NA, nrow=672, ncol=7)
for (j in 1:7){
  for (i in 1:672){
  div<-regrich[i,2]/regrich[i,j]
}
}

#Forma sucia de guardar una bd que sea regrich+varclim de 200 a 800####
bd<-left_join(regrich, sc1, by="SC200")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT200"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ200"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP200"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ200"

bd<-left_join(bd, sc2, by="SC300")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT300"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ300"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP300"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ300"

bd<-left_join(bd, sc3, by="SC400")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT400"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ400"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP400"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ400"

bd<-left_join(bd, sc4, by="SC500")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT500"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ500"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP500"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ500"

bd<-left_join(bd, sc5, by="SC600")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT600"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ600"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP600"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ600"

bd<-left_join(bd, sc6, by="SC700")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT700"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ700"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP700"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ700"

bd<-left_join(bd, sc7, by="SC800")
colnames(bd)[which(colnames(bd)=="AMT")] <- "AMT800"
colnames(bd)[which(colnames(bd)=="MeanTColdQ")] <- "MeanTColdQ800"
colnames(bd)[which(colnames(bd)=="AP")] <- "AP800"
colnames(bd)[which(colnames(bd)=="PColdQ")] <- "PColdQ800"

bdb<-bd[complete.cases(bd),]
write.csv(bdb, "/Users/marialeomontes/Dropbox/LocalRegional/Datos/BDsem.csv")
