library(deSolve)
library(RColorBrewer)

#Simulate Grassland-Sapling-Tree-Woodland dynamics
source("R/GSTF_func.R")

state <- c(
  G = 0,            #Grass
  S = 0.5,          #Saplings
  Tr = 0.25,        #Savanna Trees
  Fo = 0.25)        #Forest Trees

parameters <- c(
  alpha = 0.2,      #Forest tree birth rate
  beta = 0.3,       #Savanna sapling birth rate
  omega0 = 0.9,     #Omega is the savanna sapling-to-adult recruitment rate
  omega1 = 0.4,
  omegamid = 0.5,
  mu = 0.1,         #Savanna sapling mortality rate
  v = 0.05,         #Adult savanna tree mortality rate
  phi0 = 0.1,       #Phi is the forest tree mortality rate
  phi1 = 0.9,
  phimid = 0.5
  )

time <- seq(0,1000, by = 0.1)

out <- ode(y = state, times = time, func = GSTF_func, parms = parameters)

#Plot ALL
colors <- brewer.pal(4,"Set1")
plot(out[,1],out[,2], xlab = "time", ylab = "-",type="l",lwd=2,col=colors[2],ylim=c(0,1)) #Blue
lines(out[,1],out[,3],col=colors[4],type="l",lwd=2) #Purple
lines(out[,1],out[,4],col=colors[1],type="l",lwd=2) #Red
lines(out[,1],out[,5],col=colors[3],type="l",lwd=2) #Green

#Plot Grass vs. Trees+Forests
plot(out[,1],out[,2], xlab = "time", ylab = "-",type="l",col=colors[4],ylim=c(0,1)) #Purple
lines(out[,1],(out[,4]+out[,5]),col=colors[3],type="l") #Blue



#Simulation with Herbivory
source("R/GSTFH_func.R")

state <- c(
  G = 0,            #Grass
  S = 0.5,          #Saplings
  Tr = 0.25,        #Savanna Trees
  Fo = 0.25)        #Forest Trees
  #H = 0.1)           #Herbivores

parameters <- c(
  alpha = 0.2,      #Forest tree birth rate
  beta = 0.3,       #Savanna sapling birth rate
  omega0 = 0.9,     #Omega is the savanna sapling-to-adult recruitment rate
  omega1 = 0.4,
  omegamid = 0.5,
  mu = 0.1,         #Savanna sapling mortality rate
  v = 0.05,         #Adult savanna tree mortality rate
  phi0 = 0.1,       #Phi is the forest tree mortality rate
  phi1 = 0.9,
  phimid = 0.5,
  #Herbivory parameters
  #eg = 0.1,
  #eb = 0.1,
  ag = 0.01,           #Grazer attack rate
  ab = 0.04         #Browser attack rate
  #dg = 0.05,
  #db = 0.05
)

time <- seq(0,5000, by = 0.1)

out <- ode(y = state, times = time, func = GSTFH_func, parms = parameters)

exp_out <- apply(out,2,mean)

#Plot ALL
colors <- brewer.pal(5,"Set1")
plot(out[,1],out[,2], xlab = "time", ylab = "-",type="l",lwd = 2,col=colors[2],ylim=c(0,1)) #Blue
lines(out[,1],out[,3],col=colors[4],type="l",lwd=2) #Purple
lines(out[,1],out[,4],col=colors[1],type="l",lwd=2) #Red
lines(out[,1],out[,5],col=colors[3],type="l",lwd=2) #Green

plot(out[,1],out[,6],col=colors[5],type="l")
