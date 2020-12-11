
StomataC4Crops <- function(i) {
  
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
  
  ci[i] <- max (0           , min (cimax, ci[i]))
  
  gbco2l <- min (10, max (0.1, airVegCoef * 25.5))
  
  esat34 <- esat (airTemp)
  qsat34 <- qsat (esat34, psurf)
  rh34 <- max (0.30, airHumidity / qsat34)
  
  #q10 <- 2.5  
  
  rwork <- 3.47e-03 - 1 / canopyTemp
  tleaf <- canopyTemp - 273.16
  
  tempvm <- q10 ** ((tleaf - 15) / 10) / ((1 + exp(f1[i] * (lotemp[i] - tleaf))) * (1 + exp(f2[i] * (tleaf - hitemp[i]))))
  
  stressc4c <- 1  
  
  # TODO: Retirar isso daqui! Setar para cana em outro local.
  # if(i == 1) stressn[i] <- 1
  
  vmax      <- max(0, vmax_pft[i] * tempvm * min(stressc4c, stressn[i], croplive[i]))
  
  rdarkc4 <- gamma[i] * vmax_pft[i] * tempvm * croplive[i]
  
  je <- toppar * 4.59e-06 * 0.067 
  jc <- vmax
  
  kco2 <- 4e+03 * vmax    
  
  #duma <- thetac4
  duma <- theta[i]
  dumb <- je  + jc
  dumc <- je  * jc
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  jp <- min (dumq / duma, dumc / dumq)
  ji <- kco2 * cic4
  
  # duma <- betac4
  duma <- beta[i]
  dumb <- jp + ji
  dumc <- jp * ji
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  agc4 <- min (dumq / duma, dumc / dumq)
  anc4 <- agc4 - rdarkc4
  
  anc4 <- anc4 * max(0, stresst)
  
  # csc4 <- 0.5 * (csc4 + co2conc - anc4 / gbco2l)  
  cs[i] <- 0.5 * (cs[i] + co2conc - anc4 / gbco2l)
  
  # csc4 <- max (0, csc4)
  cs[i] <- max (0, cs[i])
  
  # gsc4 <- 0.5 * (gsc4 + coefmc4 * anc4 * rh34 / csc4 + coefbc4 * stressc4c)  
  gs[i] <- 0.5 * (gs[i] + coefm[i] * anc4 * rh34 / cs[i] + coefb[i] * stressc4c)
  
  # gsc4 <- max (gsc4min, coefbc4 * stressc4c, gsc4) 
  gs[i] <- max (gsmin[i], coefb[i] * stressc4c, gs[i])
  
  # cic4 <- (1 / 3) * (cic4 * 2 + csc4 - 1.6 * anc4 / gsc4)
  ci[i] <- (1 / 3) * (ci[i] * 2 + cs[i] - 1.6 * anc4 / gs[i])
  # cic4 <- max (0, min (cimax, cic4))  
  ci[i] <- max (0, min (cimax, ci[i]))
  
  # ---------------------------------------------------------------------
  # lower canopy scaling
  # ---------------------------------------------------------------------
  # calculate the approximate extinction coefficient
  
  # PFT_UPDATE: Escala é comum para todos os tipos de planta, mudando apenas as variáveis upper ou lower
  # Transformar numa função?
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
  
  
  
  # agcc4 <- agc4 * scale
  ag[i] <- agc4 * scale
  
  # ancc4 <- anc4 * scale
  an[i] <- anc4 * scale
  
  # cscc4 <- max (1e-08       , co2conc - ancc4 / gbco2l)
  cscc4 <- max (1e-08       , co2conc - an[i] / gbco2l)
  
  # gscc4 <- coefmc4 * ancc4 * rh34 / cscc4 + coefbc4 * stressc4c
  gscc4 <- coefm[i] * an[i] * rh34 / cscc4 + coefb[i] * stressc4c
  
  # ancc4 <- ancc4 * grnfraccrop[idc]
  an[i] <- an[i] * greenfrac[i]
  # agcc4 <- agcc4 * grnfraccrop[idc]
  ag[i] <- ag[i] * greenfrac[i]
  # gscc4 <- max (gsc4min, coefbc4 * stressc4c, gscc4)
  gscc4 <- max (gsmin[i], coefb[i] * stressc4c, gscc4)
  # gscc4 <- gscc4 * grnfraccrop[idc]
  gscc4 <- gscc4 * greenfrac[i]
  
  
  rwork <- 1 / airVegCoef
  dump <- 1 / 0.029   
  
  # totcondc4 <- 1 / ( rwork + dump / gscc4 )
  totcond[i] <- 1 / ( rwork + dump / gscc4 )
  
  rwork <- 1 - fwet
  
  # agcc4 <- rwork * agcc4
  ag[i] <- rwork * ag[i]
  
  # ancc4 <- rwork * ancc4
  an[i] <- rwork * an[i]
  
  assign("stressn", stressn, envir = env)
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("ag", ag, envir = env)
  assign("an", an, envir = env)
  assign("totcond", totcond, envir = env)
  
}