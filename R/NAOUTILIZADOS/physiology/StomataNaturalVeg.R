# stomataUpper & StomataShrub  --> StomataNaturalVeg

StomataNaturalVeg <- function(i) {
  
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
  
  # calculate physiological parameter values which are a function of temperature
  
  rwork <- 3.47e-03 - 1 / canopyTemp
  
  
  tau <- tau15 * exp( - 4500 * rwork)   # tau <- tau15 * exp( - 5000 * rwork)   # Lower
  kc  <- kc15 * exp( 6000 * rwork)      # kc <- kc15 * exp( 6000 * rwork)       # Lower
  ko  <- ko15 * exp( 1500 * rwork)      # ko <- ko15 * exp( 1400 * rwork)       # Lower
  
  tleaf   <- canopyTemp - 273.16
  
  tempvm  <- exp(3500 * rwork ) / ((1 + exp(0.40 * (  5 - tleaf))) * (1 + exp(0.40 * (tleaf - 50))))
  
  # upper canopy gamma - star values (mol / mol)
  
  gamstar <- o2conc / (2 * tau)
  
  # constrain ci values to acceptable bounds -- to help ensure numerical stability
  
  ci[i]    <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  # calculate boundary layer parameters (mol / m**2 / s) <- airVegCoef / 0.029 * 1.35
  
  gbco2u  <- min (10, max (0.1, airVegCoef * 25.5))
  
  # calculate the relative humidity in the canopy air space
  # with a minimum value of 0.30 to avoid errors in the 
  # physiological calculations
  
  esat12  <- esat (airTemp)
  qsat12  <- qsat (esat12, psurf)
  rh12    <- max (0.30, airHumidity / qsat12)
  
  # vmax and dark respiration for current conditions
  
  vmax    <- vmax_pft[i] * tempvm * stresst
  rdark   <- gamma[i] * vmax_pft[i] * tempvm
  
  # 'light limited' rate of photosynthesis (mol / m**2 / s)
  
  je <- toppar * 4.59e-06 * alpha[i] * (ci[i] - gamstar) / (ci[i] + 2 * gamstar)
  
  # 'rubisco limited' rate of photosynthesis (mol / m**2 / s)
  
  jc <- vmax * (ci[i] - gamstar) / (ci[i] + kc * (1 + o2conc / ko))
  
  # solution to quadratic equation 
  
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
  
  duma <- beta[i]
  
  dumb <- jp + js
  dumc <- jp * js
  
  dume <- max (dumb ** 2  - 4 * duma * dumc, 0)
  dumq <- 0.5 * (dumb + sqrt(dume)) + 1e-15
  
  # calculate the net photosynthesis rate (mol / m**2 / s)
  
  ag <- min (dumq / duma, dumc / dumq) 
  an <- ag - rdark
  
  # calculate co2 concentrations and stomatal condutance values
  # using simple iterative procedure
  
  # weight results with the previous iteration's values -- this
  # improves convergence by avoiding flip - flop between diffusion
  # into and out of the stomatal cavities
  
  # calculate new value of cs using implicit scheme
  
  cs[i] <- 0.5 * (cs[i] + co2conc - an / gbco2u)
  cs[i] <- max (1.05 * gamstar, cs[i])
  
  # calculate new value of gs using implicit scheme
  
  gs[i] <- 0.5 * (gs[i] + (coefm[i] * an * rh12 / cs[i] + coefb[i] * stresst))
  
  gs[i] <- max (gsmin[i], coefb[i] * stresst, gs[i])
  
  # calculate new value of ci using implicit scheme
  
  ci[i] <- 0.5 * (ci[i] + cs[i] - 1.6 * an / gs[i])
  ci[i] <- max (1.05 * gamstar, min (cimax, ci[i]))
  
  extpar <- (term[6] * scalcoef[1] + term[7] * scalcoef[2] - term[7] * scalcoef[3]) /max (scalcoef[4], epsilon)
  
  extpar <- max (1e-1, min (1e+1, extpar))
  
  # calculate canopy average photosynthesis (per unit leaf area):
  
  pxaiu <- extpar * (l_lai + l_sai)
  plaiu <- extpar * l_lai
  
  # scale is the parameter that scales from leaf - level photosynthesis to
  # canopy average photosynthesis
  # CD : replaced 24 (hours) by 86400 / dtime for use with other timestep
  zweight <- exp( - 1 / (10 * 86400 / dtime))
  # for non - zero lai
  
  if(plaiu > 0) {
    # day - time conditions, use current scaling coefficient
    if(toppar > 10) {
      scale <- (1 - exp( - pxaiu)) / plaiu
      # update 10 - day running mean of scale, weighted by light levels
      a10scalparam <- zweight * a10scalparam + (1 - zweight) * scale * toppar
      a10daylight  <- zweight * a10daylight + (1 - zweight) * toppar
      # night - time conditions, use long - term day - time average scaling coefficient
    } else {
      scale <- a10scalparam / a10daylight 
    }
    # if no lai present
  } else {
    scale <- 0
  }
  
  # perform scaling on all carbon fluxes from upper canopy
  
  agc[i] <- ag * scale
  anc[i] <- an * scale
  
  # calculate diagnostic canopy average surface co2 concentration 
  # (big leaf approach)
  
  csc <- max (1.05 * gamstar, co2conc - anc[i] / gbco2u)
  # calculate diagnostic canopy average stomatal conductance (big leaf approach)
  gsc <- coefm[i] * anc[i] * rh12 / csc + coefb[i] * stresst
  gsc <- max (gsmin[i], coefb[i] * stresst, gsc)
  
  # calculate total canopy and boundary - layer total conductance for 
  # water vapor diffusion
  
  rwork     <- 1 / airVegCoef
  dump      <- 1 / 0.029
  
  totcond[i] <- 1 / (rwork + dump / gsc)
  
  # multiply canopy photosynthesis by wet fraction - this calculation is
  # done here and not earlier to avoid using within canopy conductance
  
  rwork <- 1 - fwet
  
  agc[i] <- rwork * agc[i]
  anc[i] <- rwork * anc[i]
  
  assign("ci", ci, envir = env)
  assign("cs", cs, envir = env)
  assign("gs", gs, envir = env)
  assign("a10scalparam", a10scalparam, envir = env)
  assign("a10daylight", a10daylight, envir = env)
  assign("agc", agc, envir = env)
  assign("anc", anc, envir = env)
  assign("totcond", totcond, envir = env)
}