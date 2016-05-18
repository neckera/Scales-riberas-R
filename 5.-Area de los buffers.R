areas<-read.csv("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/areas_sc200800.csv", sep=";")
ar200<-areas[,4]
ar800<-areas[,2]
ar800<-na.omit(ar800)

#rm duplicated values
ar200<-unique(ar200)
ar800<-unique(a800)

#object to save
area<-matrix(0,nrow=4,ncol=2)
colnames(area)<-c("200","800")
row.names(area)<-c("mean", "mediana", "min", "max")
area<-as.data.frame(area)
area[1,1]<-mean(ar200)
area[2,1]<-median(ar200)
area[3,1]<-min(ar200)
area[4,1]<-max(ar200)

area[1,2]<-mean(ar800)
area[2,2]<-median(ar800)
area[3,2]<-min(ar800)
area[4,2]<-max(ar800)

#10 buffers para un punto con min area de cuenca 200km y max area de cuenca 800km
w<-(area[4,2]-area[3,1])/10
i<-1
for (i in 1:10){
    bf<-gBuffer(spgeom = p, width = sqrt(i*w/pi) , byid = T)
    plot(bf, add=T)
}

#10 buffer para un punto con mean area de cuenca 200 km y 800 km
z<-(area[1,2]-area[1,1])/10
i<-1
for (i in 1:10){
  bf<-gBuffer(spgeom = p, width = sqrt(i*z/pi) , byid = T)
  plot(bf, add=T)
}
