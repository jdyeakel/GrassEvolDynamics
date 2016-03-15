setwd("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso")

library(siar)
library(dplyr)




#Import Data
mdata <- read.csv("data/CerlingD1.csv",header=TRUE)
fdata <- read.csv("data/CerlingD2.csv",header=TRUE)

md13C <- mdata$d13C.enamel...equivalent
hist(md13C,breaks=50,col="gray")


#Original values are adjusted to date 1750 Seuss
#Means and SDs from Sponheimer et al. 2005
#Enamel-diet adj from Cerling 2015
diet_enamel <- 14.1
C4_mean <- -12.2 + diet_enamel
C4_sd <- 1.1
C3_mean <- -26.4 + diet_enamel
C3_sd <- 1.8
sources <- data.frame(c("C4","C3"),c(C4_mean,C3_mean),c(C4_sd,C3_sd))
colnames(sources) <- c("Source","Meand13C","SDd13C")
tef <- data.frame(c("C4","C3"),c(0,0),c(0,0))
colnames(tef) <- c("Source","MeanTDF","SDTDF")

cons <- data.frame(mdata$Taxon.used,mdata$d13C.enamel...equivalent)
colnames(cons) <- c("Taxon","d13C")
levels(cons$Taxon) <- seq(1:22)
##
model <- list()
for (i in 1:22) {
  taxonsel <- i
  values <- filter(cons,Taxon==taxonsel)$d13C
  values_rev <- values[!is.na(values)]
  selcons <- cbind(rep(1,length(values_rev)),values_rev)
  model[[i]]<-siarmcmcdirichletv4(selcons,sources,tef,concdep=0,500000,50000)
}
save.image("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")

## Analysis
load("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")
siarmatrixplot(model[[3]])
