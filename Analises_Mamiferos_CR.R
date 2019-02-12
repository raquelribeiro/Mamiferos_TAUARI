library(iNEXT)
library(vegan)
library(rareNMtests)
## Importando dados
library(readr)
Pequenos <- read_delim("Desktop/Pequenos.csv",";", escape_double = FALSE, trim_ws = TRUE)
View(Pequenos)

Grandes <- read_delim("Desktop/Grandes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(Grandes)

####Curvas de rarefa????o
CR<-rarefaction.individual(Grandes$N,method="sample-size", q=0, powerfun=1)
resultado<-rarefaction.individual(Grandes$N,method="sample-size", q=0, powerfun=1)
riqueza<-resultado$`Hill (q=0)`
amostra<-resultado$`sample-size`
ymin<-min(riqueza)
ymax<-max(riqueza)
ybaixo<-ymin*0.8
yalto=ymax+(ymin*0.2)
limites<-c(ybaixo, yalto)
plot(riqueza~amostra, type="l", ylim=limites, las=1, xlab="Numero de Individuos", ylab="Riqueza de Especies")
points(small, las=1, xlab="Numero de individuos", ylab="Riqueza de especies")
points(riqueza~amostra, pch=18, bg="gray")
legend("bottomright",c("Medios e Grandes mamiferos", "Pequenos mamiferos"), pch=c(18,21))

## Indices de Diversidade
invsimpson<-diversity(Pequenos_BSCR_AnalisesR$N, "inv")
shannon<-diversity(Pequenos_BSCR_AnalisesR$N, "shannon")
mlinvsimp<-diversity(Grandes$N, "inv")
> mlshan<-diversity(Grandes$N, "shannon")


