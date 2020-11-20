
###############################################################################
# Parâmetros gerais  # (recebido como parâmetros, mas comuns a todos os tipos)
# tau15
# kc15
# ko15
# cimax
# co2conc


###############################################################################
# Parâmetros específicos relacionados ao tipos de planta (c3 / c4 / crop / veg)

# alpha  - intrinsic quantum efficiency
# beta   - photosynthesis coupling coefficient
# theta  - photosynthesis coupling coefficient
# gamma  - leaf respiration coefficients 
# coefm  - 'm' coefficients for stomatal conductance relationship
# coefb  - 'b' coefficients for stomatal conductance relationship
# gsmin  - absolute minimum stomatal conductances

###################
# Outros parâmetros

# vmax_pft - nominal vmax of top leaf at 15 C (mol-co2/m**2/s) 
# lotemp   - low temperature threshold in tempvm equation 
# f1       - constant used in tempvm equations 
# f2       - constant used in tempvm equations 


#############################################################################################  
# Variáveis do modelo ( cada tipo de planta terá individualmente esse conjunto de variáveis )

# ci      - Intercellular co2 concentration (mol_co2/mol_air)
# cs      - Leaf boundary layer co2 concentration (mol_co2/mol_air)
# gs      - Lower canopy stomatal conductance (mol_co2 m-2 s-1)
# ag      - Canopy average gross photosynthesis rate (mol_co2 m-2 s-1)
# an     - Canopy average net photosynthesis rate (mol_co2 m-2 s-1)
# totcond 

# greenfrac

############################################################################# 
# Inputs Lower - Upper (são os inputs que mudam se a planta é lower ou upper)

# t34      /  t12
# q34      /  q12
# tl       /  tu
# sl       /  su
# topparl  /  topparu
# stresstl /  stresstu
# fwetl    /  fwetu

###############
# Outros inputs

# psurf
# stressn 

# #######
# SCALING

# terml         /  termu 
# a10scalparaml /  a10scalparamu 
# a10daylightl  /  a10daylightu 
# scalcoefl     /  scalcoefu 


StomataC3Crops <- function(i) {
  
  canopy <- plantList[[i]]$canopy
  
  if(canopy == UPPER) {
    
    airTemp     <- t12
    airHumidity <- q12
    canopyTemp  <- tu
    airVegCoef  <- su
    toppar      <- topparu
    stresst     <- stresstu
    fwet        <- fwetu
    l_lai       <- lai[2]
    l_sai       <- sai[2]
    term        <- termu
    scalcoef    <- scalcoefu
    a10scalparam <- a10scalparamu
    a10daylight  <- a10daylightu
    
  } else {
    
    airTemp     <- t34
    airHumidity <- q34
    canopyTemp  <- tl
    airVegCoef  <- sl
    toppar      <- topparl
    stresst     <- stresstl
    fwet        <- fwetl
    l_lai       <- lai[1]
    l_sai       <- sai[1]
    term        <- terml
    scalcoef    <- scalcoefl
    a10scalparam <- a10scalparaml
    a10daylight  <- a10daylightl
    
  }
  
  rwork  <- 3.47e-03 - 1 / canopyTemp
  tau    <- tau15 * exp( - 5000 * rwork)
  kc     <- kc15 * exp( 6000 * rwork)
  ko     <- ko15 * exp( 1400 * rwork)
  tleaf  <- canopyTemp - 273.16
  tempvm <- exp(3500 * rwork ) / ((1 + exp(0.40 * (  5 - tleaf))) * (1 + exp(0.40 * (tleaf - 50))))
  gamstar <- o2conc / (2 * tau)
  
  ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))  
  
  gbco2l <- min (10, max (0.1, airVegCoef * 25.5))
  esat34 <- esat (airTemp)
  qsat34 <- qsat (esat34, psurf)
  rh34 <- max (0.30, airHumidity / qsat34)
  
  # modelo começa a partir daqui
  rdarkc3 <- 0
  
  if(croplive[i] == 1) {
    
    rwork <- 3.47e-03 - 1 / canopyTemp # recalcula aqui
    tleaf <- canopyTemp - 273.16       # recalcula aqui
    
    q10 <- 2
    
    tempvm <- q10 ** ((tleaf - 8) / 10) / ((1 + exp(f1[i] * (lotemp[i] - tleaf))) * (1 + exp(f2[i] * (tleaf - hitemp[i]))))
    
    stressc3c <- min(1, stresst)
#    vmax <- max(0, vmax_pft[i] * tempvm * min(stressc3c, stressn[i], croplive[i]))
    vmax <- min(max(0, vmax_pft[i] * tempvm * min(stressc3c, stressn[i], croplive[i])),150) # maxim value of 150: Michel
    
    rdarkc3 <- gamma[i] * vmax_pft[i] * tempvm * croplive[i]
    
    je <- toppar * 4.59e-06 * alpha[i] * (ci[i] - gamstar) /(ci[i] + 2 * gamstar)
    jc <- vmax * (ci[i] - gamstar) / (ci[i] + kc * (1 + o2conc / ko))
    
    duma <- theta[i]
    
    dumb <- je  + jc
    dumc <- je  * jc
    
    dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
    dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
    
    jp <- min (dumq / duma, dumc / dumq)
    js <- vmax / 4.2#2.2
    
    duma <- beta[i]
    dumb <- jp + js
    dumc <- jp * js
    
    dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
    dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
    
    agc3 <- min (dumq / duma, dumc / dumq)
    anc3 <- agc3 - rdarkc3
    # anc3 <- anc3 * stresst  
    
    cs[i]   <- 0.5 * (cs[i] + co2conc - anc3 / gbco2l)
    
    cs[i] <- max (1.05 * gamstar, cs[i])
    
    # gs[i] <- 0.5 * (gs[i] + coefm[i] * anc3 * rh34 / cs[i] + coefb[i] * stressc3c)
    #CSVC BBL
    D0    <- 1.5#3.0
    gs[i] <- 0.5 * (gs[i] + coefm[i] * anc3 / ((cs[i]-gamstar)*(1+rh34/D0)) + coefb[i] * stressc3c) #Ball (1988) & Berry (1991) model
    
    #CSVC Modified BBL
    
    #  VPDSLP = -0.32  
    #  VPDMIN = 0.5    
    
    #  if (rh34 >= VPDMIN) { VPDFACTOR=MAX(0.3,(1+VPDSLP*(rh34-VPDMIN))) }else{VPDFACTOR=1.0}
    #  
    #  gs[i] <- 0.5 * (gs[i] +  (coefm[i] * anc3*VPDFACTOR) / (cs[i]-gamstar) + coefb[i] * stressc3c) #Ball (1988) & Berry (1991) model
    
    gs[i] <- max (gsmin[i], coefb[i] * stressc3c, gs[i])
    
    ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * anc3 / gs[i])
    
    ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
    
    
  } else {
    
    cs[i] <- 0
    gs[i] <- 0
    ci[i] <- 0
    agc3  <- 0
    anc3  <- 0
    
  }
  # # ---------------------------------------------------------------------
  # # lower canopy scaling
  # # ---------------------------------------------------------------------
  # calculate the approximate extinction coefficient
  extpar  <- (term[6] * scalcoef[1] + term[7] * scalcoef[2] - term[7] * scalcoef[3]) / max (scalcoef[4], epsilon)
  extpar  <- max (1e-1, min (1e+1, extpar))
  pxail   <- extpar * (l_lai + l_sai)  
  plail   <- extpar * l_lai
  zweight <- exp( - 1 / (10 * 86400 / dtime))
  
  if(plail > 0) {
    if(toppar > 10) {
      scale         <- (1 - exp( - pxail)) / plail
      a10scalparam <- zweight * a10scalparam + (1 - zweight) * scale  * toppar
      a10daylight  <- zweight * a10daylight +  (1 - zweight) * toppar
    } else {
      scale <- a10scalparam / a10daylight
    }
  } else {
    scale <- 0
  }
  
  # Usa a escala nas variáveis especificas
  
  ag[i] <- agc3 * scale
  an[i] <- anc3 * scale
  
  if(croplive[i] == 1) {
    
    cscc3 <- max (1.05 * gamstar, co2conc - an[i] / gbco2l)

    gscc3 <- coefm[i] * an[i] * rh34 / cscc3 + coefb[i] * stressc3c
  
    gscc3 <- max (gsmin[i], coefb[i] * stressc3c, gscc3)
    
    gscc3 <- gscc3 * greenfrac[i]
    
  } else {
    cscc3 <- 0
    gscc3 <- 0
  }
  
  rwork <- 1 / airVegCoef   
  dump  <- 1 / 0.029   
  
  if(gscc3 > 0)
    totcond[i] <- 1 / ( rwork + dump / gscc3 )
  
  rwork <- 1 - fwet
  
  ag[i] <- rwork * ag[i]
  
  an[i] <- rwork * an[i]
  
  
  assign("stressn", stressn, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("ag", ag, envir = env)
  assign("an", an, envir = env)
  assign("totcond", totcond, envir = env)
  assign ("rdarkc3", rdarkc3, envir = env)
  
}

