# steps intercepted h2o due to drip, precip, and min/max limits
# 
# calls steph2o for upper leaves, upper stems and lower veg in
# iurn, adjusting precips at each level
cascadeR <- function(envi){
  
  environment(mix) <- env
  environment(steph2o) <- env
  environment(twet3) <- env
  
  # adjust rainfall and snowfall rates at above-tree level
  # 
  # set wliqmin, wsnomin -- unlike wliq*max, wsno*max, these are
  # part of the lsx numerical method and not from the vegetation
  # database, and they are the same for all veg components
  # 
  # the value 0.0010 should be small compared to typical precip rates
  # times dtime to allow any intercepted h2o to be initiated, but
  # not too small to allow evap rates to reduce wliq*, wsno* to
  # that value in a reasonable number of time steps
  
  wliqmin <- 0.0010 * (dtime/3600.) * (wliqumax / 0.2)
  wsnomin <- 0.0010 * (dtime/3600.) * (wsnoumax / 2.0)
  
  rainu <- raina
  
  # add amount for irrigation  - C. Kucharik 04/11/01
  
  rainu <- rainu + xirriga
  
  # set rain temperature to the wet bulb temperature
  
  if (ta > tmelt) {
    twetbulb <- twet3( ta, qa, psurf )
  }else{
    twetbulb <- tmelt
  }
  trainu <- max (twetbulb, tmelt)
  x1 <- 0.0
  x2 <- max (t12, tmelt)
  
  retorno <- mix (rainu,trainu, rainu,trainu, x1,x2, vzero,vzero)
  
  rainu <- retorno$xm
  trainu <- retorno$tm
  
  snowu <- snowa
  tsnowu <- min (ta, tmelt)
  x1 <- 0.0
  x2 <- min (t12, tmelt)
  
  retorno <-mix (snowu,tsnowu, snowu,tsnowu, x1,x2, vzero,vzero)
  snowu <- retorno$xm
  tsnowu <- retorno$tm
  
  # set up for upper leaves
  
  xai <- 2.0 * lai[2]
  rain <- rainu
  train <- trainu
  snow <- snowu
  tsnow <- tsnowu
  
  # step upper leaves
  
  retorno <- steph2o (tu,  wliqu,  wsnou,  xai,  pfluxu,  rain, train, snow, tsnow, tdripu, tblowu, wliqumax, wsnoumax, wliqmin, wsnomin)
  
  wliqu <- retorno$wliq
  wsnou <- retorno$wsno
  pfluxu <- retorno$pflux
  rain <- retorno$rain
  snow <- retorno$snow
  train <- retorno$train
  tsnow <- retorno$tsnow
  
  # set up for upper stems
  # the upper stems get precip as modified by the upper leaves
  
  xai <- 2.0 * sai[2]
  
  # step upper stems
  
  retorno <- steph2o (ts,  wliqs,  wsnos,  xai,  pfluxs,  rain, train, snow, tsnow, tdrips, tblows, wliqsmax, wsnosmax, wliqmin, wsnomin)
  
  wliqs <- retorno$wliq
  wsnos <- retorno$wsno
  pfluxs <- retorno$pflux
  rain <- retorno$rain
  snow <- retorno$snow
  train <- retorno$train
  tsnow <- retorno$tsnow
  
  # adjust rainfall and snowfall rates at below-tree level
  # allowing for upper-veg interception/drip/belowoff
  
  x1 <- fu*rain
  x2 <- (1.-fu)*rainu
  x3 <- 0.0
  x4 <- max (t34, tmelt)
  
  retorno <- mix (rainl,trainl, x1,train, x2,trainu, x3,x4)
  rainl <- retorno$xm
  trainl <- retorno$tm
  
  x1 <- fu*snow
  x2 <- (1.-fu)*snowu
  x3 <- 0.0
  x4 <- min (t34, tmelt)
  
  retorno <- mix (snowl,tsnowl, x1,tsnow, x2,tsnowu, x3,x4)
  snowl <- retorno$xm
  tsnowl <- retorno$tm
  
  # set up for lower veg
  
  xai <- 2.0 * (lai[1] + sai[1])
  rain <- rainl
  train <- trainl
  snow <- snowl
  tsnow <- tsnowl
  
  # step lower veg
  
  retorno <- steph2o (tl,  wliql,  wsnol,  xai,  pfluxl,  rain, train, snow, tsnow, tdripl, tblowl, wliqlmax, wsnolmax, wliqmin, wsnomin)
  
  wliql <- retorno$wliq
  wsnol <- retorno$wsno
  pfluxl <- retorno$pflux
  rain <- retorno$rain
  snow <- retorno$snow
  train <- retorno$train
  tsnow <- retorno$tsnow
  
  # adjust rainfall and  snowfall rates at soil level,
  # allowing for lower-veg interception/drip/blowoff
  
  x1 <- fl * rain
  x2 <- (1.-fl) * rainl
  
  retorno <- mix (raing,traing, x1,train, x2,trainl, vzero,vzero)
  raing <- retorno$xm
  traing <- retorno$tm
  
  x1 <- fl * snow
  x2 <- (1.-fl) * snowl
  
  retorno <- mix (snowg,tsnowg, x1,tsnow, x2,tsnowl, vzero,vzero)
  snowg <- retorno$xm
  tsnowg <- retorno$tm
  
  
  assign("wliqmin", wliqmin, envir = env)
  assign("wsnomin", wsnomin, envir = env)
  assign("rainu", rainu, envir = env)
  assign("trainu", trainu, envir = env)
  assign("snowu", snowu, envir = env)
  assign("tsnowu", tsnowu, envir = env)
  assign("wliqu", wliqu, envir = env)
  assign("wsnou", wsnou, envir = env)
  assign("pfluxu", pfluxu, envir = env)
  assign("wliqs", wliqs, envir = env)
  assign("wsnos", wsnos, envir = env)
  assign("pfluxs", pfluxs, envir = env)
  assign("rainl", rainl, envir = env)
  assign("trainl", trainl, envir = env)
  assign("snowl", snowl, envir = env)
  assign("tsnowl", tsnowl, envir = env)
  assign("wliql", wliql, envir = env)
  assign("wsnol", wsnol, envir = env)
  assign("pfluxl", pfluxl, envir = env)
  assign("raing", raing, envir = env)
  assign("traing", traing, envir = env)
  assign("snowg", snowg, envir = env)
  assign("tsnowg", tsnowg, envir = env)
}


# steps intercepted h2o for one canopy component (upper leaves, 
# upper stems, or lower veg) through one lsx time step, adjusting
# for h2o sensible heat and phase changes. also modifies precip
# due to interception and drip,blowoff

steph2o <- function(tveg,  wliq,  wsno,  xai,  pflux,  rain, train, snow, tsnow, tdrip, tblow, wliqmax, wsnomax, wliqmin, wsnomin) {
  
  environment(mix) <- env
  
  # calculate fint, the intercepted precip fraction per unit
  # leaf/stem area -- note 0.5 * lai or sai (similar to irrad)
  
  if (xai >= epsilon) {
    fint <- ( 1.-exp(-0.5*xai) )/ xai
  }else{
    fint <- 0.5
  }
  
  # step intercepted liquid and snow amounts due to drip/blow,
  # intercepted rainfall/snowfall, and min/max limits. also 
  # adjust temperature of intercepted precip to current veg temp,
  # storing the heat needed to do this in pflux for use in turvap
  #  
  # without these pfluxes, the implicit turvap calcs could not
  # account for the heat flux associated with precip adjustments,
  # especially changes of phase (see below), and so could not
  # handle equilibrium situations such as intercepted snowfall
  # being continuously melted by warm atmos fluxes, with the veg 
  # temp somewhat lower than the equil atmos temp to supply heat
  # that melts the incoming snow; (turvap would just change veg 
  # temp to atmos equil, with little sensible heat storage...then
  # final phase adjustment would return veg temp to melt point)
  # 
  # the use of the current (ie, previous timestep's) veg temp 
  # gives the best estimate of what this timestep's final temp
  # will be, at least for steady conditions
  
  rwork <- 1. / dtime
  
  # liquid
  
  drip <- xai*wliq/tdrip
  wliq <- wliq * (1.-dtime/tdrip)
  
  wliq <- wliq + dtime*rain*fint
  pflux <- rain*fint * (tveg-train)*ch2o
  rain <- rain*(1.-xai*fint)
  
  x <- wliq
  wliq <- min (wliq, wliqmax)
  if (wliq < wliqmin) wliq  <- 0.
  drip <- drip + xai*(x-wliq)*rwork
  
  # snow
  
  blow <- xai*wsno/tblow
  wsno <- wsno * (1.-dtime/tblow)
  
  wsno <- wsno + dtime*snow*fint
  pflux <- pflux + snow*fint * (tveg-tsnow)*cice
  snow <- snow*(1.-xai*fint)
  
  x <- wsno
  wsno <- min (wsno, wsnomax)
  if (wsno < wsnomin) wsno <- 0. 
  blow <- blow + xai*(x-wsno)*rwork
  
  # change phase of liquid/snow below/above melt point, and add
  # required heat to pflux (see comments above). this will only
  # affect the precip intercepted in this timestep, since original
  # wliq, wsno must have been ge/le melt point (ensured in later
  # call to cascad2/steph2o2)
  
  rwork2 = ch2o - cice
  
  # liquid below freezing
  
  dw <- 0.
  if (tveg < tmelt)  dw <- wliq
  
  pflux <- pflux + dw * (rwork2*(tmelt-tveg) - hfus) * rwork
  wliq <- wliq - dw
  wsno <- wsno + dw
  
  # snow above freezing
  
  dw <- 0.
  if (tveg > tmelt)  dw <- wsno
  
  pflux <- pflux + dw * (rwork2*(tveg-tmelt) + hfus) * rwork
  wsno <- wsno - dw
  wliq <- wliq + dw
  
  # adjust rainfall, snowfall below veg for interception 
  # and drip, blowoff
  
  retorno <- mix (rain,train, rain,train, drip,tveg, vzero,vzero)
  rain <- retorno$xm
  train <- retorno$tm
  
  retorno <- mix (snow,tsnow, snow,tsnow, blow,tveg, vzero,vzero)
  snow <- retorno$xm
  tsnow <- retorno$tm
  
  return(list(wliq=wliq, pflux=pflux, rain=rain, wsno=wsno, snow=snow, train=train, tsnow=tsnow))
}


# calorimetrically mixes masses x1,x2,x3 with temperatures
# t1,t2,t3 into combined mass xm with temperature tm
# 
# xm,tm may be returned into same location as one of x1,t1,..,
# so hold result temporarily in xtmp,ttmp below
# 
# will work if some of x1,x2,x3 have opposite signs, but may 
# give unphysical tm's
mix <- function(xm,tm, x1,t1, x2,t2, x3,t3) {
  
  xtmp <- x1 + x2 + x3
  
  ytmp <- (max (abs(xtmp), epsilon) * sign(xtmp))
  # ytmp <- sign (max (abs(xtmp), epsilon), xtmp)
  
  if (abs(xtmp) >= epsilon) {
    ttmp <- (t1*x1 + t2*x2 + t3*x3) / ytmp
  }else{
    ttmp <- 0.
    xtmp <- 0.
  }
  
  xm <- xtmp
  tm <- ttmp
  
  return(list(xm=xm,tm=tm))
}


# twet3.f last update 8/30/2000 C Molling
# 
# This function calculates the wet bulb temperature given
# air temp, specific humidity, and air pressure.  It needs the function esat
# in order to work (in comsat.h).  The function is an approximation to
# the actual wet bulb temperature relationship.  It agrees well with the
# formula in the Smithsonian Met. Tables for moderate humidities, but differs
# by as much as 1 K in extremely dry or moist environments.
# 
# INPUT
#      tak - air temp in K
#      q - specific humidity in kg/kg
#      p - air pressure in Pa (Pa = 100 * mb)
# 
# OUTPUT
#      twet3 - wet bulb temp in K, accuracy?
twet3 <- function(tak, q, p) {
  
  environment(hvapf) <- env
  environment(qsat) <- env
  
  # temperatures in twet3 equation must be in C
  # pressure in qsat function must be in Pa
  # temperatures in esat,hvapf functions must be in K
  # 
  #      Air temp in C
  
  ta <- tak - 273.16
  
  # First guess for wet bulb temp in C, K
  # -------------------------------------
  twet3 <- ta * q / qsat(esat(tak),p)
  twk <- twet3 + 273.16
  
  # Iterate to converge
  # -------------------
  for (i in 1:20) {
    twold <- twk - 273.16
    twet3 <- ta - (hvapf(twk,tak)/cair) * ( qsat( esat(twk),p )-q )
    diff <- twet3 - twold
    
    # below, the 0.2 is the relaxation parameter that works up to 40C (at least)
    
    twk <- twold + 0.2 * diff + 273.16
    if (abs(twk-273.16-twold) < 0.02) {
      twet3 <- twk
      return(twet3)
    }
  }
  
  # print *, 'Warning, twet3 failed to converge after 20 iterations!'
  # print *, 'twet3, twetold: ', twk, twold+273.16
  # print *, 'twetbulb is being set to the air temperature'
  
  twet3 = tak
  
  # Return wet bulb temperature in K
  # --------------------------------
  
  twet3 = twk
  return(twet3)
  
}
