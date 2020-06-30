# Global Vars:
# cloud      # cloud fraction
# coszen     # cosine of solar zenith angle
# daylength  # length of day (minutes)
# dtime      # model timestep (seconds)
# fira       # incoming ir flux (W m-2)
# latindex   # j index of nth point in land+sea array (i,j)
# latscale   # latitude of nth point in degrees morth
# precip     # daily precitation (mm/day)
# psurf      # surface pressure (Pa)
# qa         # specific humidity (kg_h2o/kg_air)
# qd         # daily average specific humidity (kg_h2o/kg_air)
# raina      # rainfall rate (mm/s or kg m-2 s-1)
# rh         # relative humidity(%)
# snowa      # snowfall rate (mm/s or kg m-2 s-1 of water)
# solad      # direct downward solar flux (W m-2)
# solai      # diffuse downward solar flux (W m-2)
# stef       # stefan-boltzmann constant (W m-2 K-4)
# ta         # air temperature (K)
# totirrig   # annual total irrigation applied (mm/yr)
# xirrig     # irrigated water application rate (mm/day) to crops
# xirriga    # irrigated application rate per timestep

diurnalmetR <- function (envi, time, jday, plens, startp, endp,
                        irrigate, ilens, starti, endi) {
  # ---------------------------------------------------------------------- 
  # *  *  * calendar and orbital calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # calculate time in hours
  rtime <- time  / 3600
  
  # calculate the earths orbital angle[around the sun] in radians
  orbit <- 2 * pi * jday / 365.2425
  
  # calculate the solar hour angle in radians
  angle <- 2 * pi * (rtime  - 12) / 24
  
  # calculate the current solar declination angle
  # ref: global physical climatology, hartmann, appendix a
  xdecl <- 0.006918                    -
    0.399912 * cos(orbit) +  
    0.070257 * sin(orbit)       -
    0.006758 * cos(2 * orbit) +
    0.000907 * sin(2 * orbit) -
    0.002697 * cos(3 * orbit) +
    0.001480 * sin(3 * orbit)
  
  # calculate the effective solar constant, including effects of eccentricity
  # ref: global physical climatology, hartmann, appendix a
  sw <- 1370 * (1.000110 +
                  0.034221 * cos(orbit) +  
                  0.001280 * sin(orbit) +  
                  0.000719 * cos(2 * orbit)  +
                  0.000077 * sin(2 * orbit)) 
  

    
  # ---------------------------------------------------------------------- 
  # *  *  * solar calculations *  * *
  # ---------------------------------------------------------------------- 
  
  # calculate the latitude in radians
  jj <- latindex
  
  xlat <- latscale * pi / 180
  
  # calculate the cosine of the solar zenith angle
  coszen <- max (0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos(angle)))
  
  # find daylength to be used in pheno subroutine
  daylength <- (180 / pi) * ((2 * 60) / 15) * (acos((coszen - (sin(xlat) * sin(xdecl))) / (cos(xlat) * cos(xdecl))))
  
  # calculate the solar transmission through the atmosphere
  # using simple linear function of tranmission and cloud cover
  #
  # note that the 'cloud cover' data is typically obtained from
  # sunshine hours -- not direct cloud observations
  #
  # where, cloud cover <- 1 - sunshine fraction 
  #
  # different authors present different values for the slope and 
  # intercept terms of this equation
  #
  # Friend, A: Parameterization of a global daily weather generator for
  # terrestrial ecosystem and biogeochemical modelling, Ecological 
  # Modelling
  #
  # Spitters et al., 1986: Separating the diffuse and direct component
  # of global radiation and its implications for modeling canopy
  # photosynthesis, Part I: Components of incoming radiation,
  # Agricultural and Forest Meteorology, 38, 217 - 229
  #
  # A. Friend       : trans <- 0.251 + 0.509 * (1 - cloud[i])
  # Spitters et al. : trans <- 0.200 + 0.560 * (1 - cloud[i])
  #
  # we are using the values from A. Friend

  if(cloud>0){
    trans <- cloud / (sw * coszen)
  } else {
    trans <- 0
  } #cloud = incidente solar radiation
  trans <- max(0,min(1,trans))
  
  # calculate the fraction of indirect (diffuse) solar radiation
  # based upon the cloud cover
  #
  # note that these relationships typically are measured for either
  # monthly or daily timescales, and may not be exactly appropriate
  # for hourly calculations -- however, in ibis, cloud cover is fixed
  # through the entire day so it may not make much difference
  #
  # method i --
  #
  # we use a simple empirical relationships from Nikolov and Zeller (1992)
  #
  # Nikolov, N. and K.F. Zeller, 1992:  A solar radiation algorithm for ecosystem
  # dynamics models, Ecological Modelling, 61, 149 - 168
  fdiffuse <- 1.0045 + 0.0435 * trans -  
    3.5227 * trans ** 2+ 
    2.6313 * trans ** 3

    if (trans > 0.75) fdiffuse <- 0.166
  
  
  # do for each waveband
  for(ib in 1: nband) { 
    # calculate the fraction in each waveband
    wfrac <- 0.46 + 0.08 * (ib - 1)  #visible 0.46 and NIR 0.54
    
    # calculate the direct and indirect solar radiation
    solad[ib] <- wfrac * cloud * (1 - fdiffuse)
    solai[ib] <- wfrac * cloud * fdiffuse
  }
  
  # ---------------------------------------------------------------------- 
  # *  *  * temperature calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # assign hourly temperatures using tmax and tmin 
  # following Environmental Biophysics, by Campbell and Norman, p.23
  #
  # this function fits a fourier series to the diurnal temperature cycle
  # note that the maximum temperature occurs at 2:00 pm local solar time
  #
  # note that the daily mean value of gamma is 0.44, 
  # so td <- 0.44 * tmax + 0.56 * tmin,  instead of
  #    td <- 0.50 * tmax + 0.50 * tmin
  
  gamma <- 0.44 - 0.46 * sin (      pi / 12 * rtime  + 0.9) +  
    0.11 * sin (2 * pi / 12 * rtime  + 0.9)
  
  # ---------------------------------------------------------------------- 
  # *  *  * humidity calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # adjust specific humidity against daily minimum temperatures
  #
  # To do this, qa is written as an approximate sine function (same as ta)
  # to preserve the daily mean specific humidity, while also preventing rh
  # from exceeding 99% at night
  #
  # Note that the daily mean RH is * not * preserved, and therefore the
  # output RH will be slightly different from what was read in.
  #
  # first adjust so that maximum RH cannot exceed 99% at night
  
  # if needed, adjust again to 99% at other times of the day (in which
  # case the daily mean * specific * humidity is also not preserved)
  
  qsa <- 0.99 * qsat(esat(ta), psurf)
  
  # calculate the hourly specific humidity, using the above adjustments
  qa <- min (qsa, qd)
  
  # calculate the hourly relative humidity 
  rh <- 100 * qa / qsat(esat(ta), psurf)
  
  # ---------------------------------------------------------------------- 
  # *  *  * ir flux calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # clear - sky emissivity as a function of water vapor pressure
  # and atmospheric temperature
  #
  # calculate the ir emissivity of the clear sky
  # using equation from idso (1981) water resources res., 17, 295 - 304
  emb <- 0.01 * (psurf * qa / (0.622 + qa))
  ea <- 0.70 + 5.95e-5 * emb * exp (1500 / ta)
  
  # assign the ir emissivity of clouds[assume they are ~black in the ir]
  ec <- 0.950
  
  # assign the temperature difference of emissions (air + cloud) from
  # the surface air temperature
  dtair <- 2
  dtcloud <- 2
  
  # total downward ir is equal to the sum of:
  #
  # (1) clear sky contribution to downward ir radiation flux
  # (2) cloud contribution to downward ir radiation flux
  truecloud <- 1 - ((trans - 0.251) / 0.509) 
  fira <- (1 - truecloud) * ea * stef * (ta - dtair  ) ** 4 + truecloud * ec * stef * (ta - dtcloud) ** 4
  
  # ---------------------------------------------------------------------- 
  # *  *  * snow and rain calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # reset snow and rain to zero
  snowa <- 0
  raina <- 0
  
  # determine the number of timesteps per day
  niter <- as.integer (86400 / dtime)
  
  if(ta - 273.15 > 2.5) {
    raina <- precip / plens
  } else {
    snowa <- precip / plens
  }
  
  # ---------------------------------------------------------------------- 
  # *  *  * irrigation calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # reset rate of irrigation application per timestep 
  xirriga <- 0
  
  # if precipitation event - then no irrigation that day 
  if(time  >= starti && time  < endi &&  
     irrigate  == 1&& 
     precip == 0) {  
    
    xirriga <- xirrig / ilens
    
    # update annual total - totirrig
    # rate of irrigation multiplied by length of timestep (mm / s  * s) <- total applied
    # for this timestep 
    
    totirrig <- totirrig + (xirriga * dtime)
    
  } 
    
  
  assign("coszen", coszen, envir = env)
  assign("daylength", daylength, envir = env)
  assign("solad", solad, envir = env)
  assign("solai", solai, envir = env)
  assign("qa", qa, envir = env)
  assign("rh", rh, envir = env)
  assign("fira", fira, envir = env)
  assign("snowa", snowa, envir = env)
  assign("raina", raina, envir = env)
  assign("xirriga", xirriga, envir = env)
  assign("totirrig", totirrig, envir = env)
  
  return()
}
