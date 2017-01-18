#load data
load("/Users/marialeomontes/Documents/BIOLOGIA/ARTICULOS/Local_Regional/Datos/loc.bas.Rdata")
loc.bas.indra <- loc.bas

### Scale 1
bd <- loc.bas.indra[[1]]
bdscaled <- scale(bd[3:9])

lm1 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm1)
summary(lm1)


### Scale 2
bd <- loc.bas.indra[[2]]
bdscaled <- scale(bd[3:9])

lm2 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm2)
summary(lm2)


### Scale 3
bd <- loc.bas.indra[[3]]
bdscaled <- scale(bd[3:9])

lm3 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm3)
summary(lm3)


### Scale 4
bd <- loc.bas.indra[[4]]
bdscaled <- scale(bd[3:9])

lm4 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm4)
summary(lm4)


### Scale 5
bd <- loc.bas.indra[[5]]
bdscaled <- scale(bd[3:9])

lm5 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm5)
summary(lm5)


### Scale 6
bd <- loc.bas.indra[[6]]
bdscaled <- scale(bd[3:9])

lm6 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm6)
summary(lm6)


### Scale 7
bd <- loc.bas.indra[[7]]
bdscaled <- scale(bd[3:9])

lm7 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm7)
summary(lm7)


### Scale 8
bd <- loc.bas.indra[[8]]
bdscaled <- scale(bd[3:9])

lm8 <- lm(data = as.data.frame(bdscaled), formula = locrich~pa+pcq+tcqm+tam)
plot(lm8)
summary(lm8)

