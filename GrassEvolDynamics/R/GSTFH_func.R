#Now with more herbivory flavor
GSTFH_func <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    
    #Rate of change
    #Grassland <-> Sapling <-> Savanna <-> Woodland
    dG <- mu*S + v*Tr + (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid)))))*Fo - beta*G*Tr - alpha*G*Fo + ag*G #*H
    dS <- beta*G*Tr - (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - mu*S - alpha*S*Fo - ag*G + ab*(Tr+Fo) #*H
    dTr <- (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - v*Tr - alpha*Tr*Fo - ab*Tr #*H
    dFo <- (alpha*(1 - Fo) - (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid))))) )*Fo - ab*Fo #*H
    
    #Herbivory
    #dH <- ((eg*ag*G - dg) + (eb*ab*(Tr+Fo) - db))*H
    
    #Return rate of change
    list(c(dG,dS,dTr,dFo))
  }) #end with statement
  
}