#Now with more herbivory flavor
GSTFH_func <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    
    #The current dynamics account for herbivory mortality exactly as natural mortality
    #Grazed saplings -> grass
    #Browsed trees -> grass
    #Browsed forest -> grass
    
    #Might consider allowing browsed forest -> (grass,saplings)
    
    #Rate of change
    #Grassland <-> Sapling <-> Savanna <-> Woodland
    dG <- mu*S + v*Tr + (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid)))))*Fo - beta*G*Tr - alpha*G*Fo + ag*S +ab*(Tr + Fo)
    dS <- beta*G*Tr - (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - mu*S - alpha*S*Fo - ag*S
    dTr <- (omega0 + ((omega1-omega0)/(1 + exp(-1000*(G-omegamid)))))*S - v*Tr - alpha*Tr*Fo - ab*Tr
    dFo <- (alpha*(1 - Fo) - (phi0 + ((phi1-phi0)/(1 + exp(-1000*(G-phimid))))) )*Fo - ab*Fo
    
    #Herbivory
    #dH <- ((eg*ag*G - dg) + (eb*ab*(Tr+Fo) - db))*H
    
    #Return rate of change
    list(c(dG,dS,dTr,dFo))
  }) #end with statement
  
}