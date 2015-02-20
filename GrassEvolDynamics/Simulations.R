library(deSolve)
library(RColorBrewer)

#Grassland-Sapling-Tree-Woodland dynamics
source("R/GSTF_func.R")

state <- c(
  G = 0,
  S = 0.5,
  Tr = 0.25,
  Fo = 0.25)

parameters <- c(
  alpha = 0.2,
  beta = 0.3,
  omega0 = 0.9,
  omega1 = 0.4,
  omegamid = 0.5,
  mu = 0.1,
  v = 0.05,
  phi0 = 0.1,
  phi1 = 0.9,
  phimid = 0.5
  )

time <- seq(0,5000, by = 0.1)

out <- ode(y = state, times = time, func = GSTF_func, parms = parameters)

colors <- brewer.pal(4,"Set1")
plot(out[,1],out[,2], xlab = "time", ylab = "-",type="l",col=colors[1],ylim=c(0,1))
lines(out[,1],out[,3],col=colors[2],type="l")
lines(out[,1],out[,4],col=colors[3],type="l")
lines(out[,1],out[,5],col=colors[4],type="l")

plot(out[,"G"],out[,"Fo"],pch=".",col=colors[1])