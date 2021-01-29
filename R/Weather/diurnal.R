# Global Vars:
# cloud      # cloud fraction
# cloud      # monthly average cloudiness (percent)
# coszen     # cosine of solar zenith angle
# daylength  # length of day (minutes)
# dtime      # model timestep (seconds)
# fira       # incoming ir flux (W m-2)
# latindex   # j index of nth point in land+sea array (i,j)
# latscale   # latitude of nth point in degrees morth
# precip     # daily precitation (mm/day)
# psurf      # surface pressure (Pa)
# qa         # monthly average specific humidity (kg-h2o/kg-air)
# qa         # specific humidity (kg_h2o/kg_air)
# qd         # daily average specific humidity (kg_h2o/kg_air)
# qd         # daily climatological relative humidity (%) +
# raina      # rainfall rate (mm/s or kg m-2 s-1)
# rh         # daily average rh (percent)
# rh         # relative humidity(%)
# snowa      # snowfall rate (mm/s or kg m-2 s-1 of water)
# solad      # direct downward solar flux (W m-2)
# solai      # diffuse downward solar flux (W m-2)
# stef       # stefan-boltzmann constant (W m-2 K-4)
# stinrad    # daily Station Solar Radiation (MJ/m2/day) 
# ta         # air temperature (K)
# tmax       # daily NCEP/NCAR 2m temp maximum (C)  
# tmax       # maximum daily temperature (K)
# tmin       # 5-day average minimum air temperature (K)
# tmin       # daily NCEP/NCAR 2m temp minimum (C)  
# tmin       # minimum daily temperature (K)
# tmin       # total mineralized nitrogen in timestep (kg_n m-2 timestep-1) 
# totirrig   # annual total irrigation applied (mm/yr)
# ua         # wind speed (m s-1)
# ud         # cloud fraction
# ud         # daily average windspeed
# ud         # liquid content of puddles per soil area (kg m-2)
# xirrig     # irrigated water application rate (mm/day) to crops
# xirriga    # irrigated application rate per timestep

diurnalR <- function (envi, time, jday, plens, startp, endp,
                     irrigate, ilens, starti, endi) {
  # ---------------------------------------------------------------------- 
  # *  *  * calendar and orbital calculations *  * *
  # ---------------------------------------------------------------------- 
  
  # TODO: IMPORTANTE, dailyrad não é inicializada no fortran (contém lixo)
  # Init dailyrad
  dailyrad <- 0
  
  # calculate time in hours
  rtime <- time  / 3600
  
  # calculate the earth's orbital angle[around the sun] in radians
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
  xlat <- latscale * pi / 180
  
  # calculate the cosine of the solar zenith angle
  coszen <- max (0, (sin(xlat) * sin(xdecl) + cos(xlat) * cos(xdecl) * cos(angle)))
  

  # calculate the solar transmission through the atmosphere

  if(time == 0) {
    ij <- ((24 * 3600 / dtime) - 1)
    
    for(j in 0: ij	) { #integral for 1 day
      # calculate the cosine of the solar zenith angle
      cosz <- max (0, (sin(xlat) * sin(xdecl) +
                         cos(xlat) * cos(xdecl) *  
                         cos( (2 * pi * (j - 12) / 24) )))
      
      trans <- 1. #SVC, trans <- 1 to calculate the maximum hipotetical radiation.
      
      dailyrad <- dailyrad + (sw * cosz * trans) * (3600 / 10 ** 6)   
    }

    trans <- max(0.251,min(stinrad/dailyrad,0.75)) # 0.251 + 0.509 * (1 - cloud)  
    
    # find daylength to be used in pheno subroutine
    
    #Valores estavam errados
    #SVC daylength <- (180 / pi) * ((2 * 60) / 15) * (acos((coszen -  (sin(xlat) * sin(xdecl))) / (cos(xlat) * cos(xdecl))))
    
    # SVC Solar Declination in degrees  
    SD <- 23.45*sin((pi/180)*(360/365)*(284+jday))
    H <- acos(-tan(xlat)*tan(SD*(pi/180)))
    daylength <- (2/15)*( 0.83+(H*180/pi ) )
    
    if(is.nan(daylength)) { print("daylength = 0.0")
      stop()  }
    
  }
  
  
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
  
  fdiffuse <- 1.0045 + 0.0435 * trans - 3.5227 * trans ** 2 + 2.6313 * trans ** 3
  
  fdiffuse <- max(0.166,min(fdiffuse,0.95))
  
  # do for each waveband
  for(ib in 1: nband) { 
    # calculate the fraction in each waveband
    fracw <- 0.46 + 0.08 * (ib - 1)
    
    # calculate the direct and indirect solar radiation
    solad[ib] <- sw * coszen * fracw * trans * (1 - fdiffuse)  
    
    solai[ib] <- sw * coszen * fracw * trans * fdiffuse
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
  
  ta <- tmax * gamma + tmin * (1 - gamma)
  

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
  qmin <- min (qd, 0.99 * qsat(esat(tmin), psurf))
  qmax <- (qd - 0.56 * qmin) / 0.44
  
  # if needed, adjust again to 99% at other times of the day (in which
  # case the daily mean * specific * humidity is also not preserved)
  qsa <- 0.99 * qsat(esat(ta), psurf)
  
  # calculate the hourly specific humidity, using the above adjustments
  qa <- min (qsa, qmax * gamma + qmin * (1 - gamma))
  
  # calculate the hourly relative humidity 
  rh <- 100 * qa / qsat(esat(ta), psurf)
  
  # ---------------------------------------------------------------------- 
  # *  *  * wind speed calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # following logic of the EPIC weather generator
  # select random wind speed following this equation
  # ua[i] <- 1.13989 * ud[i] * (-log(runif(1))) ** 0.30 
  ua <- 1.13989 * ud * (-log(0.5)) ** 0.30 
  
  ua <- max (0.2, min (10, ua))
  
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
  
  #SCV - Invert c Friend, A: Parameterization of a global daily weather generator for
  #SCV - Invert c terrestrial ecosystem and biogeochemical modelling, Ecological 
  #SCV - Invert c Modelling
  #SCV - Invert c
  #SCV - Invert c Spitters et al., 1986: Separating the diffuse and direct component
  #SCV - Invert c of global radiation and its implications for modeling canopy
  #SCV - Invert c photosynthesis, Part I: Components of incoming radiation,
  #SCV - Invert c Agricultural and Forest Meteorology, 38, 217-229.
  #SCV - Invert c
  #SCV - Invert c A. Friend       : trans = 0.251 + 0.509 * (1.0 - cloud(i))
  #SCV - Invert c Spitters et al. : trans = 0.200 + 0.560 * (1.0 - cloud(i))
  #SCV - Invert c
  #SCV - Invert c we are using the values from A. Friend
  #SCV - Invert c
  #SCV - Invert c trans = 0.251 + 0.509 * (1.0 - cloud(i))
  
  
  truecloud <- max(0, min(1 - ((trans - 0.251) / 0.509),1)) 
  fira <- (1 - truecloud) * ea * stef * (ta - dtair  ) ** 4 + truecloud * ec * stef * (ta - dtcloud) ** 4
  
  # ---------------------------------------------------------------------- 
  # *  *  * snow and rain calculations *  * *
  # ---------------------------------------------------------------------- 
  
  repeat {
    
    # reset snow and rain to zero
    snowa <- 0
    raina <- 0
    
    # determine the number of timesteps per day
    niter <- as.integer (86400 / dtime)
    
    # change the rain length when the amount of rainfall / timestep is
    # too high (at the first time step)
    
    if(time  < dtime) {
      plen <- plens / dtime
      plenmin <- 1 + as.integer ((4 * 3600 - 1) / dtime)
      plenmax <- max (as.integer (24 * 3600 / dtime), plenmin)
      checkP <- 0
      
      while(precip / plen > 95 && plen < plenmax) {
        plen <- plen + 1
        checkP <- 1
      }
      
      if(checkP == 1) {
        plens <- dtime  * plen
        # startp <- dtime  * min (niter - plen, (runif(1) * (niter - plen + 1)))
        startp <- dtime  * min (niter - plen, (0.5 * (niter - plen + 1)))
        endp <- startp + plen * dtime
      } else {
        break
      }
    } else {
      break
    }
  }
  
  
  
  # if precipitation event then calculate
  if(time  >= startp && time  < endp) {  
    # for rain / snow partitioning, make it all rain if 
    # ta > 2.5 and all snow if ta <= 2.5
    #
    # reference:
    #
    # Auer, A. H., 1974: The rain versus snow threshold temperatures,
    # Weatherwise, 27, 67
    if(ta - 273.15 > 2.5) {
      raina <- precip / plens
    } else {
      snowa <- precip / plens
    }
  }
  
  # ---------------------------------------------------------------------- 
  # *  *  * irrigation calculations *  * *
  # ---------------------------------------------------------------------- 
  #
  # reset rate of irrigation application per timestep 
  xirriga <- 0
  
  # if precipitation event - then no irrigation that day 
  if(time  >= starti && time  < endi &&  
     irrigate  == 1 && 
     precip == 0) {  
    
    xirriga <- xirrig / ilens
    
    # update annual total - totirrig
    # rate of irrigation multiplied by length of timestep (mm / s  * s) <- total applied
    # for this timestep 
    totirrig <- totirrig + (xirriga * dtime)
  } 
  
  
  daylength[is.nan(daylength)] <- 0
  assign("trans", trans, envir = env)
  assign("coszen", coszen, envir = env)
  assign("daylength", daylength, envir = env)
  assign("solad", solad, envir = env)
  assign("solai", solai, envir = env)
  assign("ta", ta, envir = env)
  assign("qa", qa, envir = env)
  assign("rh", rh, envir = env)
  assign("ua", ua, envir = env)
  assign("fira", fira, envir = env)
  assign("snowa", snowa, envir = env)
  assign("raina", raina, envir = env)
  assign("xirriga", xirriga, envir = env)
  assign("totirrig", totirrig, envir = env)
  
  return()
}


