setwd("/Users/marialeomontes/Dropbox/LocalRegional/Datos")
library(dplyr)
library(ggplot2)
BD <- read.csv("BDsem.csv", header = T, sep=";")

seq<-seq(from=200, to=800,by=100)
result<-list(NA)

BD2<-BD[,c(2,10:44)]
for (i in 1:7){
  BD.temp<-select(BD2, contains(as.character(seq[i])), LocalRichness_l)
  lm[[i]]<- lm(LocalRichness_l ~ .,data=BD.temp)
  result<-summary(lm[[i]])
plot(lm[[i]])
}