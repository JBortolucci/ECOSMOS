StomataShrub <- function(i){
  # ---------------------------------------------------------------------
  # *  *  * lower canopy physiology calculations *  * *
  # ---------------------------------------------------------------------
  
  # calculate physiological parameter values which are a function of temperature
  
  rwork <- 3.47e-03 - 1 / tl
  
  tau <- tau15 * exp( - 5000 * rwork)
  kc <- kc15 * exp( 6000 * rwork)
  ko <- ko15 * exp( 1400 * rwork)
  
  tleaf <- tl - 273.16
  
  tempvm <- exp(3500 * rwork ) / ((1 + exp(0.40 * (  5 - tleaf))) * (1 + exp(0.40 * (tleaf - 50))))
  
  # lower canopy gamma - star values (mol / mol)
  
  gamstar <- o2conc / (2 * tau)
  
  # constrain ci values to acceptable bounds -- to help ensure numerical stability
  
  # PFT_UPDATE:
  # cils <- max (1.05 * gamstar, min (cimax, cils))
  ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  # calculate boundary layer parameters (mol / m**2 / s) <- sl / 0.029 * 1.35
  
  #sant - original -  
  gbco2l <- min (10, max (0.1, sl * 25.5))
  
  #sant - I think should be-> m.s - 1*kg / m3 <- kg / m2s to (mol / m**2 / s) <- sl * (1 / 0.029) - > (1 / M) <- (mol / kg)
  #        gbco2l <- min (10, max (0.1, sl[i] * (1 / 0.029)))
  # 
  # calculate the relative humidity in the canopy air space
  # with a minimum value of 0.30 to avoid errors in the 
  # physiological calculations
  
  esat34 <- esat (t34)
  qsat34 <- qsat (esat34, psurf)
  rh34 <- max (0.30, q34 / qsat34)
  
  # only perform calculations below if crops are not planted
  
  #      if (cropsums[i] == 0) then
  
  # ---------------------------------------------------------------------
  # shrub physiology
  # ---------------------------------------------------------------------
  # 
  # nominal values for vmax of top leaf at 15 C (mol - co2 / m**2 / s)
  
  # PFT_IPDATE:
  # vmaxls <- vmax_pft[9] 
  
  # vmax and dark respiration for current conditions
  
  # PFT_IPDATE:
  # vmax <- vmaxls * tempvm * stresstl
  # rdarkls <- gammals * vmaxls * tempvm
  vmax <- vmax_pft[i] * tempvm * stresstl
  rdark <- gamma[i] * vmax_pft[i] * tempvm
  
  # 'light limited' rate of photosynthesis (mol / m**2 / s)
  
  #PFT_UPDATE:
  # je <- topparl * 4.59e-06 * alpha3 * (cils - gamstar) / (cils + 2 * gamstar)
  je <- topparl * 4.59e-06 * alpha[i] * (ci[i] - gamstar) / (ci[i] + 2 * gamstar)
  
  # 'rubisco limited' rate of photosynthesis (mol / m**2 / s)
  
  # PFT_UPDATE:
  # jc <- vmax * (cils - gamstar) / (cils + kc * (1 + o2conc / ko))
  jc <- vmax * (ci[i] - gamstar) / (ci[i] + kc * (1 + o2conc / ko))
  
  # solution to quadratic equation
  
  # PFT_UPDATE:
  # duma <- theta3
  duma <- theta[i]
  
  dumb <- je  + jc
  dumc <- je  * jc
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  # calculate the intermediate photosynthesis rate (mol / m**2 / s)
  
  jp <- min (dumq / duma, dumc / dumq)
  #       
  # 'sucrose synthesis limited' rate of photosynthesis (mol / m**2 / s)
  #       
  js <- vmax / 2.2
  # 
  # solution to quadratic equation
  
  # PFT_UPDATE:
  # duma <- beta3
  duma <- beta[i]
  
  dumb <- jp + js
  dumc <- jp * js
  #       
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  # calculate the net photosynthesis rate (mol / m**2 / s)
  
  # PFT_UPDATE:
  # agls <- min (dumq / duma, dumc / dumq)
  # anls <- agls - rdarkls
  agl <- min (dumq / duma, dumc / dumq)
  an <- agl - rdark
  
  # calculate co2 concentrations and stomatal condutance values
  # using simple iterative procedure
  
  # weight results with the previous iteration's values -- this
  # improves convergence by avoiding flip - flop between diffusion
  # into and out of the stomatal cavities
  
  # calculate new value of cs using implicit scheme
  
  # PFT_UPDATE:
  # csls <- 0.5 * (csls + co2conc - anls / gbco2l)
  # csls <- max (1.05 * gamstar, csls)
  cs[i] <- 0.5 * (cs[i] + co2conc - anl / gbco2l)
  cs[i] <- max (1.05 * gamstar, cs[i])
  
  # calculate new value of gs using implicit scheme
  
  # PFT_UPDATE:
  # gsls <- 0.5 * (gsls + coefmls * anls * rh34 / csls + coefbls * stresstl)
  # gsls <- max (gslsmin, coefbls * stresstl, gsls)
  gs[i] <- 0.5 * (gs[i] + coefm[i] * anl * rh34 / cs[i] + coefb[i] * stresstl)
  gs[i] <- max (gsmin[i], coefb[i] * stresstl, gs[i])
  
  # calculate new value of ci using implicit scheme
  
  # PFT_UPDATE:
  # cils <- 0.5 * (cils + csls - 1.6 * anls / gsls)
  # cils <- max (1.05 * gamstar, min (cimax, cils))
  ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * anl / gs[i])
  ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  # ---------------------------------------------------------------------
  # lower canopy scaling
  # ---------------------------------------------------------------------
  
  # calculate the approximate extinction coefficient
  
  extpar <- (terml[6] * scalcoefl[1] + terml[7] * scalcoefl[2] - terml[7] * scalcoefl[3]) / max (scalcoefl[4], epsilon)
  
  extpar <- max (1e-1, min (1e+1, extpar))
  
  # calculate canopy average photosynthesis (per unit leaf area):
  
  pxail <- extpar * (lai[1] + sai[1])  #csant - lai[i,1] <- avglail / fl[i]
  plail <- extpar * lai[1]
  
  
  # scale is the parameter that scales from leaf - level photosynthesis to
  # canopy average photosynthesis
  # CD : replaced 24 (hours) by 86400 / dtime for use with other timestep
  
  zweight <- exp( - 1 / (10 * 86400 / dtime))
  
  # for non - zero lai
  
  if(plail > 0) {
    
    # day - time conditions, use current scaling coefficient
    
    if(topparl > 10) {
      
      scale <- (1 - exp( - pxail)) / plail  #csant - for example  - if lai <- 4, scale is +- 0.2
      
      # update 10 - day running mean of scale, weighted by light levels
      
      a10scalparaml <- zweight * a10scalparaml + (1 - zweight) * scale  * topparl
      
      a10daylightl <- zweight * a10daylightl +  (1 - zweight) * topparl
      
      # night - time conditions, use long - term day - time average scaling coefficient
      
    } else {
      
      scale <- a10scalparaml / a10daylightl
      
    }
    
    # if no lai present
    
  } else {
    
    scale <- 0
    
  }
  
  # perform scaling on all carbon fluxes from lower canopy
  

  agc[i] <- agl * scale
  anc[i] <- anl* scale
  
  
  # calculate canopy average surface co2 concentration
  # CD: For numerical stability (to avoid division by zero in gscl4),
  # cscl4 is limited to 1e-8 mol_co2 / mol_air.
  

  cscls <- max (1.05 * gamstar, co2conc - anc[i] / gbco2l)
  # calculate canopy average stomatal conductance
  gscls <- coefm[i] * anc[i] * rh34 / cscls + coefb[i] * stresstl
  gscls <- max (gsmin[i], coefb[i] * stresstl, gscls)
  
  
  anc[i] <- anc[i] * greenfrac[i]
  agc[i] <- agc[i] * greenfrac[i]
  gscls  <- gscls * greenfrac[i]
  
  # The following adjusts the above calculated values of ancl3, ancl4,
  # agcl3, agcl4, gscl3, and gscl4 according to what percentage of the
  # lower canopy is green by weighting the above calculations by greenfrac
  # terms. Only the green portion of the canopy performs photosynthesis.
  # Shrubs that have leaves have only green leaves since they are allowed
  # to drop their leaves in the fall. C3 and C4 grasses may be either green
  # or brown so they are the only terms that are adjusted.
  
  # Scale value of ancl3, ancl4, gscl3, and gscl4 according to what fraction
  # of the canopy is green
  
  # calculate canopy and boundary - layer total conductance for water vapor diffusion
  
  rwork <- 1 / sl   #csant - rb <- 1 / gb (gb is the leaf boundary - layer conduct) but sl <- gb * rhoa[m s - 1  * kg m - 3]
  dump <- 1 / 0.029   
  

  totcond[i] <- 1 / (rwork + dump / gscls)
  
  # Make sure that the calculation does not divide by zero if gscl3 or
  # gscl4 are equal to zero
  
  # multiply canopy photosynthesis by wet fraction -- this calculation is
  # done here and not earlier to avoid using within canopy conductance
  
  rwork <- 1 - fwetl
  
  agc[i] <- rwork * agc[i]
  anc[i] <- rwork * anc[i]
  
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("agc", agc, envir = env)
  assign("anc", anc, envir = env)
  assign("totcond", totcond, envir = env)
}