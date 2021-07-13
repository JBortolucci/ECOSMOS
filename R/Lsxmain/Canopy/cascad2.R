# at end of timestep, removes evaporation from intercepted h2o,
# and does final heat-conserving adjustment for any liquid/snow 
# below/above melt point. calls steph2o2 for upper leaves, 
# upper stems and lower veg in turn.
cascad2R <- function(envi){
  
  environment(steph2o2) <- env
  
  # set up for upper leaves
  
  fveg <- fu
  xai <- 2.0 * lai[2]
  
  # step upper leaves
  
  retorno <- steph2o2(tu,wliqu,wsnou,fveg,xai,rliqu,fvapuw,chu)
  tu <- retorno$tveg
  wliqu <- retorno$wliq
  wsnou <- retorno$wsno
  
  # set up for upper stems
  
  fveg <- fu
  xai <- 2.0 * sai[2]
  
  # step upper stems
  
  retorno <- steph2o2 (ts,wliqs,wsnos,fveg,xai,rliqs,fvaps,chs)
  ts <- retorno$tveg
  wliqs <- retorno$wliq
  wsnos <- retorno$wsno
  
  # set up for lower veg
  
  fveg <- (1.-fi)*fl
  xai <- 2.0 * (lai[1] + sai[1])
  
  # step lower veg
  
  retorno <- steph2o2 (tl,wliql,wsnol,fveg,xai,rliql,fvaplw,chl)
  tl <- retorno$tveg
  wliql <- retorno$wliq
  wsnol <- retorno$wsno
  
  assign("tu", tu, envir = env)
  assign("wliqu", wliqu, envir = env)
  assign("wsnou", wsnou, envir = env)
  assign("ts", ts, envir = env)
  assign("wliqs", wliqs, envir = env)
  assign("wsnos", wsnos, envir = env)
  assign("tl", tl, envir = env)
  assign("wliql", wliql, envir = env)
  assign("wsnol", wsnol, envir = env)
}

# removes evaporation from intercepted h2o, and does final
# heat-conserving adjustment for any liquid/snow below/above
# melt point, for one veg component
steph2o2 <- function(tveg, wliq, wsno, fveg, xai, rliq, fvapw, cveg) {
  
  environment(hvapf) <- env
  environment(hsubf) <- env
  
  # step intercepted h2o due to evaporation/sublimation.
  # (fvapw already has been multiplied by fwet factor in turvap,
  # so it is per unit leaf/stem area.)
  # 
  # due to linear fwet factors (see comments in fwetcal) and
  # the cap on suw,ssw,slw in turvap, evaporation in one timestep
  # should hardly ever make wliq or wsno negative -- but if this
  # happens, compensate by increasing vapor flux from atmosphere, 
  # and decreasing sensib heat flux from atmos (the former is
  # dangerous since it could suck moisture out of a dry atmos,
  # and both are unphysical but do fix the budget) tveg in hvapf
  # and hsubf should be pre-turvap-timestep values, but are not
  
  wliq <- wliq - dtime * rliq * fvapw
  wsno <- wsno - dtime * (1.-rliq) * fvapw
  
  # check to see if predicted wliq or wsno are less than zero
  
  if ((wliq < 0. | wsno < 0.) & fveg*xai > 0. )  {
    
    # write (*,9999) i, wliq(i), wsno(i)
    # 9999     format(' ***warning: wliq<0 or wsno<0 -- steph2o2 9999',
    #     >           ' i, wliq, wsno:',i4, 2f12.6)
    # 
    # calculate corrective fluxes
    
    zm <- max (-wliq, 0.) * fveg * xai / dtime
    fvapa <- fvapa + zm
    fsena <- fsena - zm * hvapf(tveg,ta)
    wliq <- max (wliq, 0.)
    
    zm <- max (-wsno, 0.) * fveg * xai / dtime
    fvapa <- fvapa + zm
    fsena <- fsena - zm * hsubf(tveg,ta)
    wsno <- max (wsno, 0.)
    
    assign("fvapa", fvapa, envir = env)
    assign("fsena", fsena, envir = env)
  }
  
  # final heat-conserving correction for liquid/snow below/above
  # melting point
  
  rwork <- 1. / hfus
  
  chav <- cveg + ch2o*wliq + cice*wsno
  
  # correct for liquid below melt point
  
  # (nb: if tveg > tmelt or wliq = 0, nothing changes.)
  
  if (tveg < tmelt & wliq > 0.0) {
    dh <- chav*(tmelt - tveg)
    dw <- min (wliq, max (0., dh*rwork))
    wliq <- wliq - dw
    wsno <- wsno + dw 
    chav <- cveg + ch2o*wliq + cice*wsno
    tveg <- tmelt - (dh-hfus*dw)/chav
  }
  
  # correct for snow above melt point
  # 
  # (nb: if tveg < tmelt or wsno = 0, nothing changes.)
  
  if (tveg > tmelt & wsno > 0.0) {
    dh <- chav*(tveg - tmelt)
    dw <- min (wsno, max (0., dh*rwork))
    wsno <- wsno - dw
    wliq <- wliq + dw
    chav <- cveg + ch2o*wliq + cice*wsno
    tveg <- tmelt + (dh-hfus*dw)/chav
  }
  return(list(tveg = tveg, wliq = wliq, wsno = wsno, fveg = fveg, xai = xai, rliq = rliq, fvapw = fvapw, cveg = cveg))
} 