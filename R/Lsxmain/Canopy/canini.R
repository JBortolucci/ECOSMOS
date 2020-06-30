# initializes aerodynamic quantities that remain constant 
# through one timestep
# 
# note that some quantities actually are
# constant as long as the vegetation amounts and fractional
# coverage remain unchanged, so could re-arrange code for
# efficiency - currently all arrays initialized here are in
# com1d which can be overwritten elsewhere
# 
# rwork is used throughout as a scratch variable to reduce number of
# computations

caniniR <- function(envi, jday) {
  
  # define sigma level of atmospheric data
  # 
  # currently, the value of siga is set to 0.999. This is roughly 10 meters
  # above ground, which is the typical height for the CRU05 input wind speed data
  
  siga <- 0.997
  
  tfac <- 1.0 / (siga^cappa)
  
  # atmospheric conditions at za
  # za is variable, although siga = p/ps is constant
  
  pa <- psurf * siga
  
  rhoa <- pa / ( rair * ta * (1.0 + (rvap / rair - 1.0) * qa) )
  
  cp <- cair * (1.0 + (cvap / cair - 1.0) * qa)
  
  za <- (psurf - pa) / (rhoa * grav)
  
  # make sure that atmospheric level is higher than canopy top
  za <- max (za, ztop[2] + 1.0)
  
  
  # aerodynamic coefficients for the lower story
  # 
  # cvegl (drag coeff for momentum) is proportional, and dvegl
  # (diffusion coeff for momentum) inversely proportional,
  # to x = density of vegetation (without distinction between
  # lai,sai and fl*(1-fi)) - x is not allowed to be exactly
  # zero to avoid divide-by-zeros, and for x>1 dvegl is 
  # proportional to 1/x**2 so that roughness length tends to
  # zero as x tends to infinity
  # 
  # also the top, bottom and displacement heights z3(i),z4(i),
  # displ(i) tend to particular values as the density tends to
  # zero, to give same results as equations for no veg at all.
  
  x <- fl * (1.0 - fi) * 2.0 * (lai[1] + sai[1]) / alaiml
  
  x <- min (x, 3.0)
  x1 <- min (x, 1.0)
  
  rwork <- max(ztop[1]-zbot[1],0.01)
  cvegl <- (0.4 / rwork) * max(1.e-5, x)
  
  dvegl <- (0.1 * rwork) / max(1.e-5, x, x^2)
  
  # e-folding depth in canopy
  
  bvegl <- sqrt (2.0 * cvegl / dvegl )
  
  # [(tau/rho)/u**2] for inf canopy
  
  bdl <- 0.5 * bvegl * dvegl
  
  # 1 / diffusion coefficient
  
  dil <- 1. / dvegl
  
  rwork <- (1.0 - x1) * (max (z0soi,z0sno) + 0.01) 
  
  z3 <- x1 * ztop[1] + rwork
  
  z4 <- x1 * zbot[1] + rwork
  
  z34 <- 0.5 * (z3 + z4)
  
  exphl <- exp (0.5 * bvegl * (z3-z4))
  expl <- exphl^2
  
  displ <- x1 * 0.7 * z3
  
  # sant	if(i.eq.1)print*,jday,ztop(i,1),x1,z3(i),displ(i),0.1*(z3(i)-z4(i))
  
  # aerodynamic coefficients for the upper story
  # same comments as for lower story
  
  x <- fu * 2.0 * (lai[2]+sai[2]) / alaimu
  
  x <- min (x, 3.0)
  x1 <- min (x, 1.0)
  
  rwork <- max (ztop[2]-zbot[2],.01)
  cvegu <- (0.4 / rwork) * max(1.e-5,x)
  
  dvegu <- (0.1 * rwork) / max(1.e-5,x,x^2)
  
  rwork <- 1. / dvegu
  bvegu <- sqrt (2.0 * cvegu * rwork)
  bdu <- 0.5 * bvegu * dvegu
  diu <- rwork
  
  rwork <- (1.0 - x1) * (z3 + 0.01)
  z1 <- x1 * ztop[2] + rwork
  z2 <- x1 * zbot[2] + rwork
  
  z12 <- 0.5 * (z1 + z2)
  
  exphu <- exp (0.5 * bvegu * (z1 - z2))
  expu <- exphu^2
  
  dispu <- x1 * 0.7 * z1 + (1.0 - x1) * displ 
  
  # mixing-length logarithms
  
  alogg <- log (z0soi)
  alogi <- log (z0sno)
  alogav <- (1.0 - fi) * alogg + fi * alogi
  
  # alog4 must be > z0soi, z0sno to avoid possible problems later 
  
  alog4 <- log ( max (z4, 1.1*z0soi, 1.1*z0sno) )
  alog3 <- log (z3-displ)
  alog2 <- log (z2-displ)
  alog1 <- log (z1-dispu)
  aloga <- log (za-dispu)
  
  # initialize u2, alogu, alogl for first iteration's fstrat
  u2 <- ua/exphu
  alogu <- log (max(.01, .1*(z1-z2)))
  alogl <- log (max(.01, .1*(z3-z4)))
  
  assign("tfac", tfac, envir = env)
  assign("rhoa", rhoa, envir = env)
  assign("cp", cp, envir = env)
  #assign("za", za, envir = env)  # SVC change for not update za
  assign("bdl", bdl, envir = env)
  assign("dil", dil, envir = env)
  assign("z3", z3, envir = env)
  assign("z4", z4, envir = env)
  assign("z34", z34, envir = env)
  assign("exphl", exphl, envir = env)
  assign("expl", expl, envir = env)
  assign("displ", displ, envir = env)
  assign("bdu", bdu, envir = env)
  assign("diu", diu, envir = env)
  assign("z1", z1, envir = env)
  assign("z2", z2, envir = env)
  assign("z12", z12, envir = env)
  assign("exphu", exphu, envir = env)
  assign("expu", expu, envir = env)
  assign("dispu", dispu, envir = env)
  assign("alogg", alogg, envir = env)
  assign("alogi", alogi, envir = env)
  assign("alogav", alogav, envir = env)
  assign("alog4", alog4, envir = env)
  assign("alog3", alog3, envir = env)
  assign("alog2", alog2, envir = env)
  assign("alog1", alog1, envir = env)
  assign("aloga", aloga, envir = env)
  assign("u2", u2, envir = env)
  assign("alogu", alogu, envir = env)
  assign("alogl", alogl, envir = env)
  
}