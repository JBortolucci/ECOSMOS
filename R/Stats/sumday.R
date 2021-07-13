# Global Vars:
# adaet       # daily average aet (mm/day)
# adco2mic    # daily accumulated co2 respiration from microbes (kg_C m-2 /day)
# adco2ratio  # ratio of root to total co2 respiration
# adco2root   # daily accumulated co2 respiration from roots (kg_C m-2 /day)
# adco2soi    # daily accumulated co2 respiration from soil(total) (kg_C m-2 /day)
# adcsoln     # daily average nitrate concentration in solution (mg/liter)
# addrainage  # daily average drainage (mm/day)
# adevap      # daily average evaporation (mm/day)
# adnmintot   # daily accumulated net nitrogen mineralization (kg_N m-2 /day)
# adnpp       # daily total npp for each plant type (kg-C/m**2/day) 
# adgpp       # daily total gpp for each plant type (kg-C/m**2/day) 
# adan        # daily total an for each plant type (kg-C/m**2/day) 
# adrain      # daily average rainfall rate (mm/day)
# adrh        # daily average rh (percent)
# adsnod      # daily average snow depth (m)
# adsnof      # daily average snow fraction (fraction)
# adsnow      # daily average snowfall rate (mm/day)
# adsrunoff   # daily average surface runoff (mm/day)
# adtlaysoi   # daily average soil temperature (c) of top layer
# adtrans     # daily average transpiration (mm/day)
# adtratio    # daily average transpiration : evaporation ratio 
# adtrunoff   # daily average total runoff (mm/day)
# adtsoi      # daily average soil temperature (c)
# adtsoic     # daily average soil temperature (c) using profile weighting
# adtsoilay   # daily average soil temperature for each soil layer  
# adud        # daily average windspeed
# adupsoil    # daily total soil water uptake from lower canopy (kg m-2/day)
# adwisoi     # daily average soil ice (fraction)
# adwisoilay  # daily average soil ice content for each soil layer
# adwlaysoi   # daily average soil moisture of top layer(fraction)
# adwsoi      # daily average soil moisture (fraction)
# adwsoic     # daily average soil moisture using root profile weighting (fraction)
# adwsoilay   # daily average soil moisture for each soil layer
# csoln       # current timestep solute concentration in solution (mg/liter)
# decompl     # litter decomposition factor                  (dimensionless)
# decomps     # soil organic matter decomposition factor     (dimensionless)
# dmetend     # ending day of reading in hourly met data
# dmetyear    # day to start using hourly met data for climate 
# dtime       # model timestep (seconds)
# fi          # fractional snow cover
# froot       # fraction of root in soil layer 
# fvapa       # downward h2o vapor flux between za & z12 at za (kg m-2 s-1)
# gdrain      # drainage rate out of bottom of lowest soil layer (kg_h2o m-2 s-1)
# grunof      # surface runoff rate (kg_h2o m-2 s-1)
# gtransl     # transpiration from lower canopy (kg_h2o m-2 s-1)
# gtransu     # transpiration from upper canopy (kg_h2o m-2 s-1)
# hsno        # thickness of snow layers (m)
# hsoi        # soil layer thickness (m)
# imetend     # ending year of reading in hourly met data
# imetyear    # year to start using hourly met data for climate 
# ndtimes     # counter for daily average calculations
# precip      # daily precitation (mm/day)
# raina       # rainfall rate (mm/s or kg m-2 s-1)
# rh          # relative humidity(%)
# snowa       # snowfall rate (mm/s or kg m-2 s-1 of water)
# tco2mic     # instantaneous microbial co2 flux from soil (mol-CO2 / m-2 / second)
# tco2root    # instantaneous fine co2 flux from soil (mol-CO2 / m-2 / second)
# tnmin       # instantaneous nitrogen mineralization (kg_N m-2/timestep)
# tnpp        # instantaneous NPP for each pft (mol-CO2 / m-2 / second)
# tsoi        # soil temperature for each layer (K)
# ud          # daily average wind speed (m/sec)
# upsoil      # soil water uptake from transpiration (kg_h2o m-2 s-1)
# wisoi       # fraction of soil pore space containing ice
# wsoi        # fraction of soil pore space containing liquid water

sumday <- function (istep, plens, iyear, jday) {
  # ---------------------------------------------------------------------
  # *  *  * update counters and working variables *  * *
  # ---------------------------------------------------------------------
  #
  # reset sumday if the first timestep of the day 
  if (istep == 1) ndtimes <- 0
  
  # accumulate daily output (at this point for soil decomposition)
  ndtimes <- ndtimes + 1
  
  # working variables
  rwork <- 1 / ndtimes
  rwork2 <- 86400
  rwork3 <- 86400 * 12e-3
  rwork4 <- 86400 * 14e-3
  
  # constants used in temperature function for c decomposition
  # (arrhenius function constant) 
  tconst <- 344  # constant for Lloyd and Taylor (1994) function
  btemp <- 288.16  # base temperature used for carbon decomposition
  
  bconst <- 10    # maximum value of decomposition factor
  
  # soil weighting factors
  rdepth <- 1 / (hsoi[1] + hsoi[2] + hsoi[3] + hsoi[4])
  rdepth2 <- 1 / (hsoi[1] + hsoi[2])
  
  # calculation of daily npp values for crops
  for(i in seq(1,npft)) {
    # if(!plantList[[i]]$active) next
    # if(plantList[[i]]$type == CROPS)
    #     adnpp[i] <- ((ndtimes - 1) * adnpp[i] + tnpp[i] * rwork3) * rwork
    adnpp[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$type == CROPS, ((ndtimes - 1) * adnpp[i] + tnpp[i] * rwork3) * rwork, adnpp[i])
    adgpp[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$type == CROPS, ((ndtimes - 1) * adgpp[i] + tgpp[i] * rwork3) * rwork, adgpp[i])
    adan[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$type == CROPS, ((ndtimes - 1) * adan[i] + tan[i] * rwork3) * rwork, adan[i])
    
  }
  
  # ---------------------------------------------------------------------
  # *  *  * daily water budget terms *  * *
  # ---------------------------------------------------------------------
  
  adrain <- ((ndtimes - 1) * adrain + raina * 86400) * rwork
  adsnow <- ((ndtimes - 1) * adsnow + snowa * 86400) * rwork
  
  # Verification of the weather generator algorithm
  niter <- as.integer(86400 / dtime)
  
  if(istep == niter) {
    
    precipfac <- precip - adrain - adsnow
    
    if ( ((precipfac <  -0.1) || (precipfac > 0.1)) &&  
         ( iyear < imetyear || iyear > imetend ||  
           (iyear == imetyear && jday < dmetyear) ||  
           (iyear == imetend && jday > dmetend) ) ) print(paste0('ERROR in sumday:', 1, jday, adrain + adsnow, precip))
  }
  
  # End of verification
  adtrans <- ((ndtimes - 1) * adtrans + (gtransl + gtransu) * 86400) * rwork
  
  adaet <- ((ndtimes - 1) * adaet - fvapa * 86400) * rwork
  
  adevap <- adaet - adtrans 
  adtratio <- max(0, min(1, adtrans / adaet)) 
  
  adtrunoff <- ((ndtimes - 1) * adtrunoff  + (grunof + gdrain) * 86400) * rwork
  
  adsrunoff <- ((ndtimes - 1) * adsrunoff  + grunof * 86400) * rwork
  addrainage <- ((ndtimes - 1) * addrainage + gdrain * 86400) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * daily atmospheric terms *  * *
  # ---------------------------------------------------------------------
  adrh <- ((ndtimes - 1) * adrh + rh) * rwork
  adud <- ((ndtimes - 1) * adud + ud) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * daily snow parameters *  * *
  # ---------------------------------------------------------------------
  snodpth <- hsno[1] + hsno[2] + hsno[3]
  
  adsnod <- ((ndtimes - 1) * adsnod + snodpth) * rwork
  adsnof <- ((ndtimes - 1) * adsnof + fi) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * soil parameters *  * *
  # ---------------------------------------------------------------------
  #
  # initialize average soil parameters
  soiltemp <- 0
  soilmois <- 0
  soilice <- 0
  
  soitempc <- 0
  soimoisc <- 0
  
  awc <- 0
  wup <- 0
  
  # averages for first 2 layers of soil
  for(k in 1:2) { 
    soiltemp <- soiltemp + tsoi[k] * hsoi[k]
    soilmois <- soilmois + wsoi[k] * hsoi[k]
    soilice <- soilice  + wisoi[k] * hsoi[k]
  }
  
  # weighting on just thickness of each layer
  soilmois <- soilmois * rdepth2
  soilice <- soilice  * rdepth2
  soiltemp <- soiltemp * rdepth2
  
  # calculate average root temperature, soil temperature and moisture and 
  # ice content based on rooting profiles (weighted) from jackson et al
  # 1996
  #
  # these soil moisture and temperatures are used in biogeochem.f 
  # we assume that the rooting profiles approximate
  # where carbon resides in the soil
  for(k in seq(1,nsoilay)) { 
    soitempc <- soitempc + tsoi[k] * 0.5 * (froot[k,1] + froot[k,2])
    soimoisc <- soimoisc + wsoi[k] * 0.5 * (froot[k,1] + froot[k,2])
  }
  
  # calculate daily average soil moisture and soil ice
  # using thickness of each layer as weighting function
  adwsoi <- ((ndtimes - 1) * adwsoi + soilmois) * rwork
  adtsoi <- ((ndtimes - 1) * adtsoi + soiltemp) * rwork
  adwisoi <- ((ndtimes - 1) * adwisoi + soilice) * rwork
  
  # calculate daily average soil moisture, ice, and temperature for
  # each soil layer
  #
  # also calculate daily uptake of water by plant (mm / day)
  # calculate daily average nitrate concentration in solution (mg / liter)
  for(k in seq(1,nsoilay)) { 
    adwsoilay[k] <- ((ndtimes - 1) * adwsoilay[k] +  wsoi[k]) * rwork
    adwisoilay[k] <- ((ndtimes - 1) * adwisoilay[k] + wisoi[k]) * rwork
    adtsoilay[k] <- ((ndtimes - 1) * adtsoilay[k] + tsoi[k]) * rwork
    
    adupsoil[k] <- ((ndtimes - 1) * adupsoil[k] + upsoil[k] * 86400) * rwork 
    
    adcsoln[k] <- ((ndtimes - 1) * adcsoln[k] + csoln[k]) * rwork
  }
  
  adtlaysoi <- ((ndtimes - 1) * adtlaysoi + tsoi[1]) * rwork
  adwlaysoi <- ((ndtimes - 1) * adwlaysoi + wsoi[1]) * rwork
  
  # calculate separate variables to keep track of weighting using 
  # rooting profile information
  #
  # note that these variables are only used for diagnostic purposes
  # and that they are not needed in the biogeochemistry code
  adwsoic <- ((ndtimes - 1) * adwsoic + soimoisc) * rwork
  adtsoic <- ((ndtimes - 1) * adtsoic + soitempc) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * calculate daily soil co2 fluxes *  * *
  # ---------------------------------------------------------------------
  
  # increment daily total co2 respiration from microbes
  # tco2mic is instantaneous value of co2 flux calculated in biogeochem.f
  adco2mic <- ((ndtimes - 1) * adco2mic + tco2mic * rwork3) * rwork
  
  # increment daily total co2 respiration from fine roots
  # tco2root is instantaneous value of co2 flux calculated in stats.f
  adco2root <- ((ndtimes - 1) * adco2root + tco2root * rwork3) * rwork
  
  # calculate daily total co2 respiration from soil
  adco2soi <- adco2root + adco2mic

  # calculate daily ratio of total root to total co2 respiration
  if(adco2soi > 0) {
    adco2ratio <- adco2root/ adco2soi
  } else {
    adco2ratio <- -999.99
  }
  
  # ---------------------------------------------------------------------
  # *  *  * calculate daily litter decomposition parameters *  * *
  # ---------------------------------------------------------------------
  #
  # calculate litter carbon decomposition factors
  # using soil temp, moisture and ice for top soil layer
  #
  # calculation of soil biogeochemistry decomposition factors 
  # based on moisture and temperature affects on microbial
  # biomass dynamics
  #
  # moisture function based on water - filled pore space (wfps)  
  # williams et al., 1992 and friend et al., 1997 used in the
  # hybrid 4 model; this is based on linn and doran, 1984
  #
  # temperature functions are derived from arrhenius function
  # found in lloyd and taylor, 1994 with a 15 c base 
  #
  # calculate temperature decomposition factor
  # CD impose lower limit to avoid division by zero at tsoi <- 227.13
  if(tsoi[1] > 237.13) {
    factor <- min (exp(tconst * ((1 / (btemp - 227.13)) - (1 /(tsoi[1] - 227.13)))), bconst)
  } else {
    factor <- exp(tconst * ((1 / (btemp - 227.13)) - (1 /(237.13 - 227.13))))
  }
  
  # calculate water - filled pore space (in percent)
  #
  # wsoi is relative to pore space not occupied by ice and water
  # thus must include the ice fraction in the calculation
  wfps <- (1 - wisoi[1]) * wsoi[1] * 100	
  
  # calculate moisture decomposition factor
  if(wfps >= 60) {
    moist <- 0.000371 * (wfps ** 2) - (0.0748 * wfps) + 4.13
  } else {
    moist <- exp((wfps - 60) ** 2  / (-800))	
  }
  
  # calculate combined temperature  / moisture decomposition factor
  factor <- max (0.001, min (bconst, factor * moist))
  
  # calculate daily average litter decomposition factor
  decompl <- ((ndtimes - 1) * decompl + factor) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * calculate daily soil carbon decomposition parameters *  * *
  # ---------------------------------------------------------------------
  #
  # calculate soil carbon decomposition factors
  # using soil temp, moisture and ice weighted by rooting profile scheme 
  #
  # calculation of soil biogeochemistry decomposition factors 
  # based on moisture and temperature affects on microbial
  # biomass dynamics
  #
  # moisture function based on water - filled pore space (wfps)  
  # williams et al., 1992 and friend et al., 1997 used in the
  # hybrid 4 model; this is based on linn and doran, 1984
  #
  # temperature functions are derived from arrhenius function
  # found in lloyd and taylor, 1994 with a 15 c base 
  #
  # calculate temperature decomposition factor
  # CD: impose lower limit to avoid division by zero at tsoi <- 227.13
  if(soiltemp > 237.13) {
    factor <- min (exp(tconst * ((1 / (btemp - 227.13)) - (1 / (soiltemp - 227.13)))), bconst)
  } else {
    factor <- exp(tconst * ((1 / (btemp - 227.13)) - (1 / (237.13 - 227.13))))
  }
  
  # calculate water - filled pore space (in percent)
  #
  # wsoi is relative to pore space not occupied by ice and water
  # thus must include the ice fraction in the calculation
  wfps <- (1 - soilice) * soilmois * 100	
  
  # calculate moisture decomposition factor
  if(wfps >= 60) {
    moist <- 0.000371 * (wfps ** 2) - (0.0748 * wfps) + 4.13
  } else {
    moist <- exp((wfps - 60) ** 2  / (-800))
  }
  
  # calculate combined temperature  / moisture decomposition factor
  factor <- max (0.001, min (bconst, factor * moist))
  
  # calculate daily average soil decomposition factor
  decomps <- ((ndtimes - 1) * decomps + factor) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * calculate other daily biogeochemical parameters *  * *
  # ---------------------------------------------------------------------
  #
  # increment daily total of net nitrogen mineralization
  # value for tnmin is calculated in biogeochem.f
  adnmintot <- ((ndtimes - 1) * adnmintot + tnmin * rwork4) * rwork
  
  
  adpar <- ((ndtimes - 1) * adpar + solad[1] + solai[1] ) * rwork
  
  
  
  tl_h[istep]<-tl 
  tu_h[istep]<-tu 
  ta_h[istep]<-ta 
  
  assign("ndtimes", ndtimes, envir = env)
  assign("adnpp", adnpp, envir = env)
  assign("adan", adan, envir = env)
  assign("adgpp", adgpp, envir = env)
  assign("adrain", adrain, envir = env)
  assign("adsnow", adsnow, envir = env)
  assign("adtrans", adtrans, envir = env)
  assign("adaet", adaet, envir = env)
  assign("adevap", adevap, envir = env)
  assign("adtratio", adtratio, envir = env)
  assign("adtrunoff", adtrunoff, envir = env)
  assign("adsrunoff", adsrunoff, envir = env)
  assign("addrainage", addrainage, envir = env)
  assign("adrh", adrh, envir = env)
  assign("adud", adud, envir = env)
  assign("adsnod", adsnod, envir = env)
  assign("adsnof", adsnof, envir = env)
  assign("adwsoi", adwsoi, envir = env)
  assign("adtsoi", adtsoi, envir = env)
  assign("adwisoi", adwisoi, envir = env)
  assign("adwsoilay", adwsoilay, envir = env)
  assign("adwisoilay", adwisoilay, envir = env)
  assign("adtsoilay", adtsoilay, envir = env)
  assign("adupsoil", adupsoil, envir = env)
  assign("adcsoln", adcsoln, envir = env)
  assign("adtlaysoi", adtlaysoi, envir = env)
  assign("adwlaysoi", adwlaysoi, envir = env)
  assign("adwsoic", adwsoic, envir = env)
  assign("adtsoic", adtsoic, envir = env)
  assign("adco2mic", adco2mic, envir = env)
  assign("adco2root", adco2root, envir = env)
  assign("adco2soi", adco2soi, envir = env)
  assign("adco2ratio", adco2ratio, envir = env)
  assign("decompl", decompl, envir = env)
  assign("decomps", decomps, envir = env)
  assign("adnmintot", adnmintot, envir = env)
  assign("tl_h", tl_h, envir = env)
  assign("tu_h", tu_h, envir = env)
  assign("ta_h", ta_h, envir = env)

  assign("adpar", adpar, envir = env)
  
  
  # return to main program
  return()
}