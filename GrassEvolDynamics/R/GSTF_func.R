GSTF_func <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    
    #Rate of change
    dG <- mu*S + v*Tr + (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid)))))*Fo - beta*G*Tr - alpha*G*Fo
    dS <- beta*G*Tr - (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - mu*S - alpha*S*Fo
    dTr <- (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - v*Tr - alpha*Tr*Fo
    dFo <- (alpha*(1 - Fo) - (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid))))) )*Fo
    
    #Return rate of change
    list(c(dG,dS,dTr,dFo))
  }) #end with statement
  
}