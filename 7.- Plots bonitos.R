library(ggplot2)
library(reshape2)
library(dplyr)

#hay que correr antes el script correspondiente para sacar los datos (4.-Buffers 
#para los plots de buffers y 0.-Riqueza regional Cuencas para los plots de cuencas)

#### BUFFERS ####
#subset datos riqueza regional para hacer el plot
# no está bien hecho este primer plot porque acumula riquezas de hasta 11000. el problema viene de.
# melt pero no consigo solucionarlo
meltes<-melt(estrichL)
meltes$value <- as.numeric(meltes$value)
ssmelt<-subset(meltes, variable %in% "species") 
ssmelt[ssmelt == -9999] <- NA
ssmelt2<-dplyr::select(ssmelt, value:L1)
head(ssmelt2)

##### Plot Riqueza Regional Buffers ####
ggplot(data = ssmelt2, aes(x= as.character(ssmelt2$L1), y=as.numeric(ssmelt2$value), group=ssmelt2$L1)) + 
  geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Buffer Area (km2)", breaks=c("1","2","3","4","5","6","7","8")
            #, labels=c(37846.99) HAY QUE REVISAR LAS ÁREAS
            ) +
  scale_y_continuous(name="Regional Richness", limits=c(0, 1200))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

##### Plot Correlaciones Buffers ####
correl<-as.data.frame(correl)
correl[,4]<-c("4500", "9000", "13500", "18000", "22500", "27000", "31500", "36000")
ggplot(correl, aes(x=V4, y=Chao)) +
  #add points by variable
  geom_point(shape=1, aes(x=V4, y=Chao, colour="Chao")) +
  geom_point(shape=2, aes(x=V4, y=Boot, colour="Boot")) +
  geom_point(shape=3, aes(x=V4, y=Species, colour="Species")) +
  scale_x_discrete(name="Buffer Area (km2)" )+
  scale_y_continuous(name="Regional Richness", limits=c(-0.16, 0.4)) +
  #add legend and colours by variable
  theme(legend.background = element_rect( size=.5, linetype="dotted")) +
  scale_colour_manual("Legend", 
                      breaks = c("Chao", "Boot", "Species"),
                      values = c("Chao"="red", "Boot"="blue", 
                                 "Species"="black"))

#### Plot Beta Buffers ####
meltbet<-melt(resbeta)
betsim<-subset(meltbet, Var2 %in% "beta.SIM") 
betnes<-subset(meltbet, Var2 %in% "beta.SNE") 
bettot<-subset(meltbet, Var2 %in% "beta.SOR") 

betsim2<-dplyr::select(betsim, value:L1)
head(betsim2)

betnes2<-dplyr::select(betnes, value:L1)
head(betnes2)

bettot2<-dplyr::select(bettot, value:L1)
head(bettot2)


#plot betaSIM
ggplot(data = betsim2, aes(x= as.character(betsim2$L1), y=as.numeric(betsim2$value), 
      group=betsim2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Buffer Area (km2)", breaks=c("1","2","3","4","5","6","7","8"),
                   labels=c("4500", "9000", "13500", "18000", "22500", "27000", "31500", "36000")) +
  scale_y_continuous(name="Turnover Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

#plot betaNES
ggplot(data = betnes2, aes(x= as.character(betnes2$L1), y=as.numeric(betnes2$value), 
                           group=betnes2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Buffer Area (km2)", breaks=c("1","2","3","4","5","6","7","8"),
                   labels=c("4500", "9000", "13500", "18000", "22500", "27000", "31500", "36000")) +
  scale_y_continuous(name="Nestedness Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

#plot betaSOR
ggplot(data = bettot2, aes(x= as.character(bettot2$L1), y=as.numeric(bettot2$value), 
                           group=bettot2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Buffer Area (km2)", breaks=c("1","2","3","4","5","6","7","8"),
                   labels=c("4500", "9000", "13500", "18000", "22500", "27000", "31500", "36000")) +
  scale_y_continuous(name="Total Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

#### CUENCAS ####

##### Plot Riqueza Regional Cuencas ####
i<-1
for (i in 1:8){
  colnames(estrichL[[i]])<-c("Species","Chao","Chao.se","Boot","id")
}
cmeltes<-melt(estrichL, id.vars="id")
cssmelt<-subset(cmeltes, variable %in% "Species") 
cssmelt2<-dplyr::select(cssmelt, value:L1)
head(cssmelt2)

ggplot(data = cssmelt2, aes(x= as.character(L1), y=as.numeric(value), group=L1, na.omit=T)) + 
  geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Basin scale", breaks=c("1","2","3","4","5","6","7","8")) +
  scale_y_continuous(name="Regional Richness", limits=c(0, 1000))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

##### Plot Correlaciones Cuencas ####
correl<-as.data.frame(correl)
correl[,4]<-c("1","2","3","4","5","6","7","8")
ggplot(correl, aes(x=V4, y=Chao)) +
  #add points by variable
  geom_point(shape=1, aes(x=V4, y=Chao, colour="Chao")) +
  geom_point(shape=2, aes(x=V4, y=Boot, colour="Boot")) +
  geom_point(shape=3, aes(x=V4, y=RegRich, colour="RegRich")) +
  scale_x_discrete(name="Basin Scale" )+
  scale_y_continuous(name="Regional Richness", limits=c(-0.16, 0.4)) +
  #add legend and colours by variable
  theme(legend.background = element_rect( size=.5, linetype="dotted")) +
  scale_colour_manual("Legend", 
                      breaks = c("Chao", "Boot", "RegRich"),
                      values = c("Chao"="red", "Boot"="blue", 
                                 "RegRich"="black"))

#### Plot Beta Cuencas ####
i<-1
for (i in 1:8){
  colnames(resbeta[[i]])<-c("beta.SIM","beta.SNE","beta.SOR")
}
meltbet<-melt(resbeta)
betsim<-subset(meltbet, Var2 %in% "beta.SIM") 
betnes<-subset(meltbet, Var2 %in% "beta.SNE") 
bettot<-subset(meltbet, Var2 %in% "beta.SOR") 

betsim2<-dplyr::select(betsim, value:L1)
head(betsim2)

betnes2<-dplyr::select(betnes, value:L1)
head(betnes2)

bettot2<-dplyr::select(bettot, value:L1)
head(bettot2)


#plot betaSIM
ggplot(data = betsim2, aes(x= as.character(betsim2$L1), y=as.numeric(betsim2$value), 
                           group=betsim2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Basin Scale", breaks=c("1","2","3","4","5","6","7","8"))+
  scale_y_continuous(name="Turnover Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect(size=.5, linetype="dotted")) 

#plot betaNES
ggplot(data = betnes2, aes(x= as.character(betnes2$L1), y=as.numeric(betnes2$value), 
                           group=betnes2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Basin Scale", breaks=c("1","2","3","4","5","6","7","8"))+
  scale_y_continuous(name="Nestedness Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

#plot betaSOR
ggplot(data = bettot2, aes(x= as.character(bettot2$L1), y=as.numeric(bettot2$value), 
                           group=bettot2$L1)) + geom_boxplot() + 
  #add texts and y range
  scale_x_discrete(name="Basin Scale", breaks=c("1","2","3","4","5","6","7","8"))+
  scale_y_continuous(name="Total Beta", limits=c(0, 1))+
  #legend
  theme(legend.background = element_rect( size=.5, linetype="dotted")) 

