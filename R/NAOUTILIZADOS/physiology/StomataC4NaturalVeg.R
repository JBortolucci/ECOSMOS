StomataC4Grass <- function(i) {
 
  
  if(plantList[[i]]$canopy == UPPER) {
    
    airTemp     <- t12
    airHumidity <- q12
    canopyTemp  <- tu
    airVegCoef  <- su
    toppar      <- topparu
    stresst     <- stresstu
    fwet        <- fwetu
    l_lai       <- lai[2]
    l_sai       <- sai[2]
    
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
    
  }
  
  # ---------------------------------------------------------------------
  # *  *  * lower canopy physiology calculations *  * *
  # ---------------------------------------------------------------------
  
  # calculate physiological parameter values which are a function of temperature
  
  rwork <- 3.47e-03 - 1 / canopyTemp 
  
  tau <- tau15 * exp( - 5000 * rwork)
  kc <- kc15 * exp( 6000 * rwork)
  ko <- ko15 * exp( 1400 * rwork)
  
  tleaf <- canopyTemp - 273.16
  
  tempvm <- exp(3500 * rwork ) / ((1 + exp(0.40 * (  5 - tleaf))) * (1 + exp(0.40 * (tleaf - 50))))
  
  # lower canopy gamma - star values (mol / mol)
  
  gamstar <- o2conc / (2 * tau)
  
  # constrain ci values to acceptable bounds -- to help ensure numerical stability
  
  ci[i] <- max (0, min (cimax, ci[i]))
  
  # calculate boundary layer parameters (mol / m**2 / s) <- sl / 0.029 * 1.35
  
  #sant - original -  
  gbco2l <- min (10, max (0.1, airVegCoef * 25.5))
  
  #sant - I think should be-> m.s - 1*kg / m3 <- kg / m2s to (mol / m**2 / s) <- sl * (1 / 0.029) - > (1 / M) <- (mol / kg)
  #        gbco2l <- min (10, max (0.1, sl[i] * (1 / 0.029)))
  # 
  # calculate the relative humidity in the canopy air space
  # with a minimum value of 0.30 to avoid errors in the 
  # physiological calculations
  
  esat34 <- esat (airTemp)
  qsat34 <- qsat (esat34, psurf)
  rh34 <- max (0.30, airHumidity / qsat34)
  
  # only perform calculations below if crops are not planted
  
  #      if (cropsums[i] == 0) then
  # ---------------------------------------------------------------------
  # c4 grass physiology
  # ---------------------------------------------------------------------
  
  # nominal values for vmax of top leaf at 15 C (mol - co2 / m**2 / s)
  
  # vmaxl4 <- vmax_pft[11]
  
  # calculate the parameter values which are a function of temperature
  
  rwork <- 3.47e-03 - 1 / canopyTemp
  
  tleaf <- canopyTemp - 273.16
  
  tempvm <- exp(3500 * rwork ) /((1 + exp(0.40 * ( 10 - tleaf))) *(1 + exp(0.40 * (tleaf - 50))))
  
  # vmax and dark respiration for current conditions
  
  # PFT_UPDATE:
  # vmax <- vmaxl4 * tempvm * stresstl
  # rdarkl4 <- gammal4 * vmaxl4 * tempvm
  vmax <- vmax_pft[i] * tempvm * stresst
  rdarkl4 <- gamma[i] * vmax_pft[i] * tempvm
  
  # initial c4 co2 efficiency (mol / m**2 / s)
  
  kco2 <- 18e+03 * vmax
  
  # 'light limited' rate of photosynthesis (mol / m**2 / s)
  

  je <- toppar * 4.59e-06 * alpha[i]
  
  # 'rubisco limited' rate of photosynthesis
  
  jc <- vmax
  
  # solve for intermediate photosynthesis rate
  
  # PFT_UPDATE:
  # duma <- theta4
  duma <- theta[i]
  dumb <- je  + jc
  dumc <- je  * jc
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  jp <- min (dumq / duma, dumc / dumq)
  
  # 'carbon dioxide limited' rate of photosynthesis (mol / m**2 / s)
  
  # PFT_UPDATE:
  # ji <- kco2 * cil4
  ji <- kco2 * ci[i]
  
  # solution to quadratic equation
  
  # PFT_UPDATE:
  # duma <- beta4
  duma <- beta[i]
  dumb <- jp + ji
  dumc <- jp * ji
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  # calculate the net photosynthesis rate (mol / m**2 / s)
  
  # PFT_UPDATE:
  # agl4 <- min (dumq / duma, dumc / dumq)
  # anl4 <- agl4 - rdarkl4
  agl <- min (dumq / duma, dumc / dumq)
  anl4 <- agl - rdarkl4
  
  # calculate co2 concentrations and stomatal condutance values
  # using simple iterative procedure
  
  # weight results with the previous iteration's values -- this
  # improves convergence by avoiding flip - flop between diffusion
  # into and out of the stomatal cavities
  
  # calculate new value of cs using implicit scheme
  # CD: For numerical stability (to avoid division by zero in gsl4), 
  # csl4 is limited to 1e-8 mol_co2 / mol_air.
  
  # PFT_UPDATE: 
  # csl4 <- 0.5 * (csl4 + co2conc - anl4 / gbco2l)
  # csl4 <- max (1e-8, csl4)]
  cs[i] <- 0.5 * (cs[i] + co2conc - anl4 / gbco2l)
  cs[i] <- max (1e-8, cs[i])
  
  # calculate new value of gs using implicit scheme
  
  # PFT_UPDATE:
  # gsl4 <- 0.5 * (gsl4 + coefml4 * anl4 * rh34 / csl4 + coefbl4 * stresstl)
  # gsl4 <- max (gsl4min, coefbl4 * stresstl, gsl4)
  gs[i] <- 0.5 * (gs[i] + coefm[i] * anl4 * rh34 / cs[i] + coefb[i] * stresst)
  gs[i] <- max (gsmin[i], coefb[i] * stresst, gs[i])
  
  # calculate new value of ci using implicit scheme
  
  # PFT_UPDATE:
  # cil4 <- 0.5 * (cil4 + csl4 - 1.6 * anl4 / gsl4)
  # cil4 <- max (0, min (cimax, cil4))
  ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * anl4 / gs[i])
  ci[i] <- max (0, min (cimax, ci[i]))
  
  # ---------------------------------------------------------------------
  # lower canopy scaling
  # ---------------------------------------------------------------------
  
  # calculate the approximate extinction coefficient
  
  extpar <- (terml[6] * scalcoefl[1] + terml[7] * scalcoefl[2] - terml[7] * scalcoefl[3]) / max (scalcoefl[4], epsilon)
  
  extpar <- max (1e-1, min (1e+1, extpar))
  
  # calculate canopy average photosynthesis (per unit leaf area):
  
  pxail <- extpar * (l_lai + l_sai)  #csant - lai[i,1] <- avglail / fl[i]
  plail <- extpar * l_lai
  
  
  # scale is the parameter that scales from leaf - level photosynthesis to
  # canopy average photosynthesis
  # CD : replaced 24 (hours) by 86400 / dtime for use with other timestep
  
  zweight <- exp( - 1 / (10 * 86400 / dtime))
  
  # for non - zero lai
  
  if(plail > 0) {
    
    # day - time conditions, use current scaling coefficient
    
    if(toppar > 10) {
      
      scale <- (1 - exp( - pxail)) / plail  #csant - for example  - if lai <- 4, scale is +- 0.2
      
      #sant	if(i == 1) print(paste0('sai[i,1],lai[i,1],extpar',sai[i,1],lai[i,1],scale ))
      
      # update 10 - day running mean of scale, weighted by light levels
      
      a10scalparaml <- zweight * a10scalparaml + (1 - zweight) * scale  * toppar
      
      a10daylightl <- zweight * a10daylightl +  (1 - zweight) * toppar
      
      # night - time conditions, use long - term day - time average scaling coefficient
      
    } else {
      
      scale <- a10scalparaml / a10daylightl
      
    }
    
    # if no lai present
    
  } else {
    
    scale <- 0
    
  }
  
  # perform scaling on all carbon fluxes from lower canopy
  
  # PFT_UPDATE:
  # agcl4 <- agl4 * scale
  # ancl4 <- anl4 * scale
  ag[i] <- agl * scale
  an[i] <- anl * scale
  
  # calculate canopy average surface co2 concentration
  # CD: For numerical stability (to avoid division by zero in gscl4),
  # cscl4 is limited to 1e-8 mol_co2 / mol_air.
  
  # PFT_UPDATE:
  # cscl4 <- max (1e-08       , co2conc - ancl4 / gbco2l)
  cscl4 <- max (1e-08       , co2conc - an[i] / gbco2l)
  
  # calculate canopy average stomatal conductance
  
  # PFT_UPDATE:
  # gscl4 <- coefml4 * ancl4 * rh34 / cscl4 + coefbl4 * stresstl
  # gscl4 <- max (gsl4min, coefbl4 * stresstl, gscl4)
  gscl4 <- coefm[i] * an[i] * rh34 / cscl4 + coefb[i] * stresst
  gscl4 <- max (gsmin[i], coefb[i] * stresst, gscl4)
  
  # The following adjusts the above calculated values of ancl3, ancl4,
  # agcl3, agcl4, gscl3, and gscl4 according to what percentage of the
  # lower canopy is green by weighting the above calculations by greenfrac
  # terms. Only the green portion of the canopy performs photosynthesis.
  # Shrubs that have leaves have only green leaves since they are allowed
  # to drop their leaves in the fall. C3 and C4 grasses may be either green
  # or brown so they are the only terms that are adjusted.
  
  # Scale value of ancl3, ancl4, gscl3, and gscl4 according to what fraction
  # of the canopy is green
  
  # PFT_UPDATE:
  # ancl4 <- ancl4 * greenfracl4
  # agcl4 <- agcl4 * greenfracl4
  # gscl4 <- gscl4 * greenfracl4
  an[i] <- an[i] * greenfrac[i]
  ag[i] <- ag[i] * greenfrac[i]
  gscl4 <- gscl4 * greenfrac[i]
  
  # calculate canopy and boundary - layer total conductance for water vapor diffusion
  
  rwork <- 1 / airVegCoef   #csant - rb <- 1 / gb (gb is the leaf boundary - layer conduct) but sl <- gb * rhoa[m s - 1  * kg m - 3]
  dump <- 1 / 0.029   
  
  # Make sure that the calculation does not divide by zero if gscl3 or
  # gscl4 are equal to zero

  totcond[i] <- 1 / ( rwork + dump / gscl4 )

  
  # multiply canopy photosynthesis by wet fraction -- this calculation is
  # done here and not earlier to avoid using within canopy conductance
  
  #	print(paste0(fwetl[i]))
  rwork <- 1 - fwet
  
  # PFT_UPDATE:
  # agcl4 <- rwork * agcl4
  # ancl4 <- rwork * ancl4
  ag[i] <- rwork * ag[i]
  an[i] <- rwork * an[i]
  
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("totcond", totcond, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("ag", ag, envir = env)
  assign("an", an, envir = env)

}