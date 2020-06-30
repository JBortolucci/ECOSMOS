
stomataBroadleaf <- function() {
  
  # ---------------------------------------------------------------------
  # *  *  * upper canopy physiology calculations *  * *
  # ---------------------------------------------------------------------
  
  # only perform calculations if crops are not planted
  
  # calculate physiological parameter values which are a function of temperature
  
  rwork <- 3.47e-03 - 1 / tu
  
  tau <- tau15 * exp( - 4500 * rwork)
  kc  <- kc15 * exp( 6000 * rwork)
  ko  <- ko15 * exp( 1500 * rwork)
  
  tleaf   <- tu - 273.16
  
  tempvm  <- exp(3500 * rwork ) / ((1 + exp(0.40 * (  5 - tleaf))) * (1 + exp(0.40 * (tleaf - 50))))
  
  # upper canopy gamma - star values (mol / mol)
  
  gamstar <- o2conc / (2 * tau)
  
  # constrain ci values to acceptable bounds -- to help ensure numerical stability
  
  # PFT_UPDATE:
  # ciub    <- max (1.05 * gamstar, min (cimax, ciub))
  ci[i]    <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  # calculate boundary layer parameters (mol / m**2 / s) <- su / 0.029 * 1.35
  
  gbco2u  <- min (10, max (0.1, su * 25.5))
  
  # calculate the relative humidity in the canopy air space
  # with a minimum value of 0.30 to avoid errors in the 
  # physiological calculations

  esat12  <- esat (t12)
  qsat12  <- qsat (esat12, psurf)
  rh12    <- max (0.30, q12 / qsat12)
  
  # ---------------------------------------------------------------------
  # broadleaf (evergreen & deciduous) tree physiology 
  # ---------------------------------------------------------------------
  # 
  # nominal values for vmax of top leaf at 15 C (mol - co2 / m**2 / s)
  
  # tropical broadleaf trees          60 e-06 mol / m**2 / sec
  # warm - temperate broadleaf trees    40 e-06 mol / m**2 / sec
  # temperate broadleaf trees         25 e-06 mol / m**2 / sec
  # boreal broadleaf trees            25 e-06 mol / m**2 / sec
  
  
  # PFT_UPDATE: Pensar em como isso vai ficar
  #             Parece um parâmetro especifico para o tipo 1 e 3
  
  if(exist[1] > 0.5) {
    vmaxub <- vmax_pft[1]     
  } else if(exist[3] > 0.5) {
    vmaxub <- vmax_pft[3]     
  } else { 
    vmaxub <- vmax_pft[5] 
  }
  
  # vmax and dark respiration for current conditions
  
  # PFT_UPDATE:
  # vmax    <- vmaxub * tempvm * stresstu
  # rdarkub <- gammaub * vmaxub * tempvm
  vmax    <- vmax_pft[i] * tempvm * stresstu
  rdark   <- gamma[i] * vmax_pft[i] * tempvm
  
  # 'light limited' rate of photosynthesis (mol / m**2 / s)
  
  # PFT_UPDATE:
  # je <- topparu * 4.59e-06 * alpha3 * (ciub - gamstar) /  (ciub + 2 * gamstar)
  # 'rubisco limited' rate of photosynthesis (mol / m**2 / s)
  # jc <- vmax * (ciub - gamstar) /  (ciub + kc * (1 + o2conc / ko))
  je <- topparu * 4.59e-06 * alpha[i] * (ci[i] - gamstar) / (ci[i] + 2 * gamstar)
  
  # 'rubisco limited' rate of photosynthesis (mol / m**2 / s)
  
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
  
  # 'sucrose synthesis limited' rate of photosynthesis (mol / m**2 / s)
  
  js <- vmax / 2.2
  
  # solution to quadratic equation
  
  # PFT_UPDATE:
  # duma <- beta3
  duma <- beta[i]
  
  dumb <- jp + js
  dumc <- jp * js
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  # calculate the net photosynthesis rate (mol / m**2 / s)
  
  # PFT_UPDATE:
  # agub <- min (dumq / duma, dumc / dumq)
  # anub <- agub - rdarkub
  ag <- min (dumq / duma, dumc / dumq)
  an <- ag - rdark
  
  # calculate co2 concentrations and stomatal condutance values
  # using simple iterative procedure
  
  # weight results with the previous iteration's values -- this
  # improves convergence by avoiding flip - flop between diffusion
  # into and out of the stomatal cavities
  
  # calculate new value of cs using implicit scheme
  
  # PFT_UPDATE:
  # csub <- 0.5 * (csub + co2conc - anub / gbco2u)
  # csub <- max (1.05 * gamstar, csub)
  cs[i] <- 0.5 * (cs[i] + co2conc - an / gbco2u)
  cs[i] <- max (1.05 * gamstar, cs[i])
  
  # calculate new value of gs using implicit scheme
  
  # PFT_UPDATE:
  # gsub <- 0.5 * (gsub + (coefmub * anub * rh12 / csub + coefbub * stresstu))
  gs[i] <- 0.5 * (gs[i] + (coefm[i] * an * rh12 / cs[i] + coefb[i] * stresstu))
  
  # PFT_UPDATE:
  # gsub <- max (gsubmin, coefbub * stresstu, gsub)
  gs[i] <- max (gsmin[i], coefb[i] * stresstu, gs[i])
  
  # calculate new value of ci using implicit scheme
  
  # PFT_UPDATE:
  # ciub <- 0.5 * (ciub + csub - 1.6 * anub / gsub)
  # ciub <- max (1.05 * gamstar, min (cimax, ciub))
  ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * an / gs[i])
  ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  
  # ---------------------------------------------------------------------
  # upper canopy scaling
  # ---------------------------------------------------------------------
  
  # the canopy scaling algorithm assumes that the net photosynthesis
  # is proportional to absored par (apar) during the daytime. during night,
  # the respiration is scaled using a 10 - day running - average daytime canopy
  # scaling parameter.
  
  # apar(x) <- A exp( - k x) + B exp( - h x) + C exp(h x)
  # an(x) is proportional to apar(x)
  
  # therefore, an(x) <- an(0) * apar(x) / apar(0)
  # an(x) <- an(0) * (A exp( - k x) + B exp( - h x) + C exp(h x)) /  
  #                 (A + B  + C)
  
  # this equation is further simplified to
  # an(x) <- an(0) * exp ( - extpar * x)
  
  # an(0) is calculated for a sunlit leaf at the top of the canopy using
  # the full - blown plant physiology model (Farquhar / Ball&Berry, Collatz).
  # then the approximate par extinction coefficient (extpar) is calculated
  # using parameters obtained from the two - stream radiation calculation.
  
  # an,canopy avg. <- integral (an(x), from 0 to xai) / lai
  # <- an(0) * (1 - exp ( - extpar * xai )) / (extpar * lai)
  
  # the term '(1 - exp ( - extpar * xai )) / lai)' scales photosynthesis from leaf
  # to canopy level (canopy average) at day time. A 10 - day running mean of this
  # scaling parameter (weighted by light) is then used to scale the respiration
  # during night time.
  
  # once canopy average photosynthesis is calculated, then the canopy average
  # stomatal conductance is calculated using the 'big leaf approach',i.e. 
  # assuming that the canopy is a big leaf and applying the leaf - level stomatal
  # conductance equations to the whole canopy.
  
  # calculate the approximate par extinction coefficient:
  
  # extpar <- (k * A  + h  * B  - h  * C) / (A + B  + C)
  
  extpar <- (termu[6] * scalcoefu[1] + termu[7] * scalcoefu[2] - termu[7] * scalcoefu[3]) /max (scalcoefu[4], epsilon)
  
  extpar <- max (1e-1, min (1e+1, extpar))
  
  # calculate canopy average photosynthesis (per unit leaf area):
  
  pxaiu <- extpar * (lai[2] + sai[2])
  plaiu <- extpar * lai[2]
  
  # scale is the parameter that scales from leaf - level photosynthesis to
  # canopy average photosynthesis
  # CD : replaced 24 (hours) by 86400 / dtime for use with other timestep
  
  zweight <- exp( - 1 / (10 * 86400 / dtime))
  
  # for non - zero lai
  
  if(plaiu > 0) {
    
    # day - time conditions, use current scaling coefficient
    
    if(topparu > 10) {
      
      scale <- (1 - exp( - pxaiu)) / plaiu
      
      # update 10 - day running mean of scale, weighted by light levels
      
      a10scalparamu <- zweight * a10scalparamu + (1 - zweight) * scale * topparu
      
      a10daylightu  <- zweight * a10daylightu + (1 - zweight) * topparu
      
      # night - time conditions, use long - term day - time average scaling coefficient
      
    } else {
      
      scale <- a10scalparamu / a10daylightu
      
    }
    
    # if no lai present
    
  } else {
    
    scale <- 0
    
  }
  
  # perform scaling on all carbon fluxes from upper canopy
  
  # PFT_UPDATE:
  # agcub <- agub * scale
  # ancub <- anub * scale
  agc[i] <- ag * scale
  anc[i] <- an * scale
  
  # calculate diagnostic canopy average surface co2 concentration 
  # (big leaf approach)
  
  # PFT_UPDATE:
  # cscub <- max (1.05 * gamstar, co2conc - ancub / gbco2u)
  csc <- max (1.05 * gamstar, co2conc - anc[i] / gbco2u)
  
  # calculate diagnostic canopy average stomatal conductance (big leaf approach)
  
  # PFT_UPDATE:
  # gscub <- coefmub * ancub * rh12 / cscub + coefbub * stresstu
  # gscub <- max (gsubmin, coefbub * stresstu, gscub)
  gsc <- coefm[i] * anc[i] * rh12 / csc + coefb[i] * stresstu
  gsc <- max (gsmin[i], coefb[i] * stresstu, gsc)
  
  # calculate total canopy and boundary - layer total conductance for 
  # water vapor diffusion
  
  rwork     <- 1 / su
  dump      <- 1 / 0.029
  
  # PFT_UPDATE:
  # totcondub <- 1 / (rwork + dump / gscub)
  totcond[i] <- 1 / (rwork + dump / gsc)
  
  # multiply canopy photosynthesis by wet fraction - this calculation is
  # done here and not earlier to avoid using within canopy conductance
  
  rwork <- 1 - fwetu
  
  # PFT_UPDATE:
  # agcub <- rwork * agcub
  # ancub <- rwork * ancub
  agc[i] <- rwork * agc[i]
  anc[i] <- rwork * anc[i]
  
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("a10scalparamu", a10scalparamu, envir = env)
  assign("a10daylightu", a10daylightu, envir = env)
  assign("agc", agc, envir = env)
  assign("anc", anc, envir = env)
  assign("totcond", totcond, envir = env)
  
}