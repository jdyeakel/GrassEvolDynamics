setwd("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso")

library(siar)
library(dplyr)
library(RColorBrewer)



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

#By Taxon
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
#save.image("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")

## Analysis
#load("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")
#siarmatrixplot(model[[3]])

#By Ecosystem type
cons_eco <- data.frame(mdata$ecosystem...classified,mdata$d13C.enamel...equivalent)
colnames(cons_eco) <- c("Ecosystem","d13C")
lgt_eco <- length(levels(mdata$ecosystem...classified))
#levels(cons_eco$Ecosystem) <- seq(1:lgt_eco)
##
eco_levels <- levels(cons_eco$Ecosystem)
eco_model <- list()
for (i in 1:lgt_eco) {
  ecosel <- eco_levels[i]
  values <- filter(cons_eco,Ecosystem==ecosel)$d13C
  values_rev <- values[!is.na(values)]
  selcons <- cbind(rep(1,length(values_rev)),values_rev)
  eco_model[[i]]<-siarmcmcdirichletv4(selcons,sources,tef,concdep=0,500000,50000)
}
ecotype <- c("F","M","M","M","F","M","M","M","F","F","F","M","F","M","M","F","M","M","M","M","F","G","M","O","M","M","F","M","G","M","M")
z <- 6
eco_levels[z]
siarmatrixplot(eco_model[[z]])

colors <- brewer.pal(3,"Set1")
norm_dens <- cbind(density(eco_model[[1]]$output[,1])$x,density(eco_model[[1]]$output[,1])$y/max(density(eco_model[[1]]$output[,1])$y))


save.image("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")
load("/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/Moderniso.Rdata")

pdf(width=10,height=4,file="/Users/justinyeakel/Dropbox/PostDoc/2014_GrasslandEvol/HerbIso/EcoProb.pdf")
plot(norm_dens,xlim=c(0,1),col=colors[3],lwd=3,type="l",xlab="Percent C4 in Diet",ylab="Probability")
for (i in 2:lgt_eco) {
  if (ecotype[i] != "O") {
    if (ecotype[i] == "G") colorpick <- 1; if (ecotype[i] == "M") colorpick <- 2; if (ecotype[i] == "F") colorpick <- 3
    norm_dens <- cbind(density(eco_model[[i]]$output[,1])$x,density(eco_model[[i]]$output[,1])$y/max(density(eco_model[[i]]$output[,1])$y))
    lines(norm_dens,col=colors[colorpick],lwd=3)
  }
}
dev.off()

plot(mean(eco_model[[1]]$output[,1]),sd(eco_model[[i]]$output[,1]),xlim=c(0,1),ylim=c(0,0.15),col=colors[3],lwd=3,pch=16,xlab="C4 input means",ylab="C4 input Variability")
for (i in 2:lgt_eco) {
  if (ecotype[i] != "O") {
    if (ecotype[i] == "G") colorpick <- 1; if (ecotype[i] == "M") colorpick <- 2; if (ecotype[i] == "F") colorpick <- 3
    points(mean(eco_model[[i]]$output[,1]),sd(eco_model[[i]]$output[,1]),col=colors[colorpick],pch=16)
  }
}



ecosd <- numeric(lgt_eco)
for (i in 1:lgt_eco) {
  ecosd[i] <- sd((eco_model[[i]]$output[,1]))
}
hist(ecosd,breaks=15,xlim=c(0,0.15),col="gray")
plot(density(ecosd),xlim=c(0,0.15),col="black")

#Fossil Data
timebins <- levels(fdata$Actual.range)
formations <- levels(fdata$Formation)
t_early <- c("3.4-3.0",    "3.6-3.4",    "4.0-3.4",    "4.0-3.6",    "4.0-4.3",    "4.2-4.0")
t_mid <- "3.0-2.5"   
t_late <- c("1.3 to 1.0", "1.3-0.75",   "1.5-1.3",    "1.55-1.4",   "1.9-1.5",    "1.9-1.55",   "2.1-1.9",    "2.35-1.9",   "2.35-2.1",   "2.5-2.35")

lgt_form <- length(formations)
lgt_timebins <- length(timebins)
for (i in 1:3) {
  fdata_form <- 
  for (i in 1:)
}
for (i in 1:lgt_timebins) {
  filter()
}
  

