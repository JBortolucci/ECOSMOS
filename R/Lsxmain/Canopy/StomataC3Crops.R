
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
# an      - Canopy average net photosynthesis rate (mol_co2 m-2 s-1)
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
    
    airTemp      <- t12
    airHumidity  <- q12
    canopyTemp   <- tu
    airVegCoef   <- su
    toppar       <- topparu
    stresst      <- stresstu
    fwet         <- fwetu
    canopy_lai   <- lai[2]
    canopy_sai   <- sai[2]
    term         <- termu
    scalcoef     <- scalcoefu
    a10scalparam <- a10scalparamu
    a10daylight  <- a10daylightu
    
  } else {
    
    airTemp       <- t34
    airHumidity   <- q34
    canopyTemp    <- tl
    airVegCoef    <- sl
    toppar        <- topparl
    stresst       <- stresstl
    fwet          <- fwetl
    canopy_lai    <- lai[1]
    canopy_sai    <- sai[1]
    term          <- terml
    scalcoef      <- scalcoefl
    a10scalparam  <- a10scalparaml
    a10daylight   <- a10daylightl
    
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
  esatdossel <- esat (airTemp)
  qsatdossel <- qsat (esatdossel, psurf)
  rhdossel <- max (0.05, airHumidity / qsatdossel)
  
  
  
  #(1) Ascertain the saturated vapour pressure (SVP) for a given temperature (see list below)
  #       Temperature (degC) - SVP (Pa)
  VPSAT = 610.78 * exp(17.269*(airTemp - 273.16)/((airTemp- 273.16)+237.30))/1000
  
  #2 - (2) As VPD is the saturated vapour pressure minus the actual vapour pressure (SVP - VPactual), 
  # and VPactual = (RH*SVP)/100
  VPDSL = VPSAT*(1- (rhdossel ))
  
  
  
  # modelo começa a partir daqui
  rdarkc3 <- 0
  
  if(croplive[i] == 1) {
    
    rwork <- 3.47e-03 - 1 / canopyTemp # recalcula aqui
    tleaf <- canopyTemp - 273.16       # recalcula aqui
    
    tempvm <- q10 ** ((tleaf - 15) / 10) / ((1 + exp(f1[i] * (lotemp[i] - tleaf))) * (1 + exp(f2[i] * (tleaf - hitemp[i]))))
    
    stressc3c <-  min(1, stresst)
  
    #    vmax <- max(0, vmax_pft[i] * tempvm * min(stressc3c, stressn[i], croplive[i]))
    vmax <- min(max(0, vmax_pft[i] * tempvm * min(stressc3c, stressn[i], croplive[i])), 200) # maxim value of 150: Michel
    
    
    rdarkc3 <- gamma[i] * vmax_pft[i] * tempvm * croplive[i]
    
    je <- toppar * 4.59e-06 * alpha[i] * (ci[i] - gamstar) /(ci[i] + 2 * gamstar)
    jc <- vmax * (ci[i] - gamstar) / (ci[i] + kc * (1 + o2conc / ko))
    
    duma <- theta[i]
    
    dumb <- je  + jc
    dumc <- je  * jc
    
    dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
    dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
    
    jp <- min (dumq / duma, dumc / dumq)
    
    js <- vmax / 2.2
    
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
    

    # Stomatal conductance models [2020-11-18]
    {
      
      gsmodel <- "BBC" # BBO | BBL | USO | BBC
      
      D0 = 1.5 # BBL
      VPDSLP = -0.7  # BBC slope       [Soybean = -0.32; Eucalyptus = -0.9]
      VPDMIN = 1.5   # BBC - Start
      
      # Ball (1988) & Berry (1991) model [BBO] 'O' means original
      if (gsmodel=="BBO") {
        gs[i] <- 0.5 * (gs[i] + coefm[i] * anc3 * rhdossel / cs[i] + coefb[i] * stressc3c)
      }
      
      # BB after Leuning (1995) [BBL]
      if (gsmodel=="BBL") {
        gs[i] <- 0.5*gs[i] + 0.5*(coefm[i] * anc3 / ((cs[i]-gamstar)*(1+VPDSL/D0)) + coefb[i] * stressc3c)
      }
      
      # BB with the optimal stomatal control model of Cowan and Farquhar (1977) was proposed by Medlyn et al. (2011) [USO]
      if (gsmodel=="USO") {
        gs[i] <- 0.5 * (gs[i] + 1.6 * (1 + coefm[i] / sqrt(rhdossel)) * (anc3 / cs[i]) ) 
        # check if rhdossel = D & where to include stressc3c
      }
      
      # BB after Cuadra et al. (2021) [BBC]
      if (gsmodel=="BBC") {
        if (VPDSL >= VPDMIN) {
          VPDFACTOR=1+VPDSLP*(VPDSL-VPDMIN)
         # print(paste(rhdossel,VPDSL,VPDFACTOR),sep=" / ")
        } else {
          VPDFACTOR=1.0
        }
        VPDFACTOR=max(min(VPDFACTOR,1),0)
        gs[i] <- 0.5*gs[i] + 0.5 * ((coefm[i]*anc3*VPDFACTOR)/(cs[i]-gamstar) + coefb[i] * stressc3c)
      }
      
    }
    
    gs[i] <- max (gsmin[i], coefb[i] * stressc3c, gs[i])
    
    ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * anc3 / gs[i])
    
    ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
    
  
#________________________________________________________________________
# # Canopy scaling

# calculate the approximate extinction coefficient
    extpar  <- (term[6] * scalcoef[1] + term[7] * scalcoef[2] - term[7] * scalcoef[3]) / max (scalcoef[4], epsilon)
    extpar  <- max (1e-1, min (1e+1, extpar))
    
    # calculate canopy average photosynthesis (per unit leaf area):
    pxail   <- extpar * (canopy_lai + canopy_sai)  
    plail   <- extpar * canopy_lai
    
    # scale is the parameter that scales from leaf-level photosynthesis to
    # canopy average photosynthesis
    
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
    
    #  perform scaling on all carbon fluxes from lower canopy
    ag[i] <- agc3 * scale
    an[i] <- anc3 * scale
    
    
    #________________________________________________________    
    #c calculate canopy average surface co2 concentration
    
    csc <- max (1.05 * gamstar, co2conc - an[i] / gbco2l)
    
    {
      
      # Ball (1988) & Berry (1991) model [BBO] 'O' means original
      if (gsmodel=="BBO") {
        gsc <- coefm[i] * an[i] * rhdossel / csc + coefb[i] * stressc3c
      }
      
      # BB after Leuning (1995) [BBL]
      if (gsmodel=="BBL") {
        gsc <-coefm[i] * an[i] / ((csc-gamstar)*(1+VPDSL/D0)) + coefb[i] * stressc3c
      }
      
      # BB with the optimal stomatal control model of Cowan and Farquhar (1977) was proposed by Medlyn et al. (2011) [USO]
      if (gsmodel=="USO") {
        gsc <-  1.6 * (1 + coefm[i] / sqrt(rhdossel)) * (an[i] / csc)  
        # check if rhdossel = D & where to include stressc3c
      }
      
      # BB after Cuadra et al. (2021) [BBC]
      if (gsmodel=="BBC") {
        if (VPDSL >= VPDMIN) {
          VPDFACTOR=1+VPDSLP*(VPDSL-VPDMIN)
          # print(paste(rhdossel,VPDSL,VPDFACTOR),sep=" / ")
        } else {
          VPDFACTOR=1.0
        }
        VPDFACTOR=max(min(VPDFACTOR,1),0)
        gsc <- (coefm[i]*an[i]*VPDFACTOR)/(csc-gamstar) + coefb[i] * stressc3c
      }
      
    }
    
    gsc <- max (gsmin[i], coefb[i] * stressc3c, gsc)
    
    
    
    
    # calculate canopy and boundary-layer total conductance for water vapor diffusion
    
    rwork <- 1 / airVegCoef   
    dump  <- 1 / 0.029   
    
    if(gsc > 0) {  totcond[i] <- 1 / ( rwork + dump / gsc )}else{ totcond[i] = 0}
    
    
    
    # fwet effect on totcond is applyed at turvap       
    rwork <- 1 - fwet
    ag[i] <- rwork * ag[i]
    an[i] <- rwork * an[i]
    
      
  } else {
    
    ag[i] <- 0
    an[i] <- 0
    cs[i] <- 0
    gs[i] <- 0
    ci[i] <- 0
    totcond[i] <- 0

    
  }
  
 
  
  assign("stressn", stressn, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("ag", ag, envir = env)
  assign("an", an, envir = env)
  assign("totcond", totcond, envir = env)
  
  assign("rdarkc3", rdarkc3, envir = env)
  assign("tempvm", tempvm, envir = env)
  
}