# Global Vars:
# grav  # gravitational acceleration (m s-2)
# vonk  # von karman constant (dimensionless)

fstrat <- function (tb, tt, ttfac, qb, qt, zb, zt, 
                    albm, albh, alt, u, rich, stram, strah, iter) {
  # ---------------------------------------------------------------------
  
  # computes mixing - length stratification correction factors
  # for momentum and heat / vapor, for current 1d strip, using
  # parameterizations in louis (1979),blm,17,187 first computes
  # richardson numbers. sets an upper limit to richardson numbers
  # so lower - veg winds don't become vanishingly small in very
  # stable conditions (cf, carson and richards,1978,blm,14,68)
  
  # system (i) is as in louis(1979). system (vi) is improved as
  # described in louis(1982), ecmwf workshop on planetary boundary
  # layer parameterizations,november 1981,59 - 79 (qc880.4 b65w619)
  
  # common blocks
  
  
  
  # input variables
  
  # integer iter   # current iteration number
  # 
  # real ttfac     # pot. temp factor for ttop (relative to bottom,supplied)
  # 
  # real
  # tb <- array(0, 1)     # bottom temperature (supplied)
  # tt <- array(0, 1)     # top temperature (supplied)
  # qb <- array(0, 1)     # bottom specific humidity (supplied)
  # qt <- array(0, 1)     # top specific humidity (supplied)
  # zb <- array(0, 1)     # height of bottom (supplied)
  # zt <- array(0, 1),     # height of top (supplied)
  # albm <- array(0, 1),   # log (bottom roughness length) for momentum (supplied)
  # albh <- array(0, 1),   # log (bottom roughness length) for heat / h2o (supplied)
  # alt <- array(0, 1),    # log (z at top) (supplied)
  # u <- array(0, 1),      # wind speed at top (supplied)
  # rich <- array(0, 1),   # richardson number (returned)
  # stram <- array(0, 1),  # stratification factor for momentum (returned)
  # strah <- array(0, 1),  # stratification factor for heat / vap (returned)
  
  stramx <- 0
  strahx <- 0
  
  # local variables
  
  #integer 
  indp <- 0
  indq <- 0  
  
  # integer i, j, np, nq
  
  # real zht, zhb, xm, xh, rwork, ym, yh, z, w
  # ---------------------------------------------------------------------
  
  np <- 0
  nq <- 0
  
  # do for all points
  

    
    # calculate richardson numbers
    
    zht <- tt * ttfac * (1 + .622 * qt)
    zhb <- tb * (1 + .622 * qb)
    
    rich <- grav * max (zt - zb, 0) * (zht - zhb) / (0.5 * (zht + zhb) * u ** 2)
    
    # bound richardson number between - 2 (unstable) to 1 (stable)
    
    rich <- max ( - 2, min (rich, 1))
    
  
  
  # set up indices for points with negative or positive ri
  

    if(rich <= 0) {
      np <- np + 1
    } else {
      nq <- nq + 1
    }
    

  
  # calculate momentum and heat / vapor factors for negative ri
  
  if(np > 0) {
    
    for(j in 1: np) { 
      

      xm <- max (alt - albm, .5)
      xh <- max (alt - albh, .5)
      
      rwork <- sqrt( - rich)
      
      ym <- (vonk / xm) ** 2  * exp (0.5 * xm) * rwork
      yh <- (vonk / xh) ** 2  * exp (0.5 * xh) * rwork
      
      # system (vi)
      
      stramx <- 1 - 2*5 * rich / (1 + 75 * ym)
      strahx <- 1 - 3*5 * rich / (1 + 75 * yh)
      
    }
    
  }
  
  # calculate momentum and heat / vapor factors for positive ri
  
  if(nq > 0) {
    
    for(j in 1:nq) { 
      
      # system (vi)
      
      z <- sqrt(1 + 5  * rich)
      
      stramx <- 1 / (1 + 2*5 * rich / z)
      strahx <- 1 / (1 + 3*5 * rich * z)
      
    }
    
  }
  
  # except for the first iteration, weight results with the
  # previous iteration's values. this improves convergence by
  # avoiding flip - flop between stable/unstable stratif, eg,
  # with cold upper air and the lower surface being heated by
  # solar radiation
  
  if(iter == 1) {
    

      stram <- stramx
      strah <- strahx
      
   
    
  } else {
    
    w <- 0.5
    
      stram <- w * stramx + (1 - w) * stram
      strah <- w * strahx + (1 - w) * strah
      
    
    
  }
  
  return(list(rich = rich, stram = stram, strah = strah))
    
}