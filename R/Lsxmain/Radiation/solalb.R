
# calculates effective albedos of the surface system,
# separately for unit incoming direct and diffuse flux -- the 
# incoming direct zenith angles are supplied in comatm array 
# coszen, and the effective albedos are returned in comatm
# arrays asurd, asuri -- also detailed absorbed and reflected flux
# info is stored in com1d arrays, for later use by solarf
# 
# the procedure is first to calculate the grass+soil albedos,
# then the tree + (grass+soil+snow) albedos. the labels
# (a) to (d) correspond to those in the description doc
solalbR <- function(envi, ib) {
  
  environment(twostr) <- env
  
  # do nothing if all points in current strip have coszen le 0
  if (nsol == 0) 
            return()
  
  # (a) obtain albedos, etc, for two-stream lower veg + soil
  #     system, for direct and diffuse incoming unit flux
  
  for (j in 1:nsol) {
    
    i <- indsol[j]
    
    asurd[i,ib] <- albsod[i]
    asuri[i,ib] <- albsoi[i]
  }

  
  retorno <- twostr(ablod, abloi, relod, reloi,  flodd,  dummy, flodi, floii,  asurd,  asuri,  1,  coszen, ib)
  
  ablod <- retorno[1]
  abloi <- retorno[2]
  relod <- retorno[3]
  reloi <- retorno[4]
  flodd <- retorno[5]
  dummy <- retorno[6]
  flodi <- retorno[7]
  floii <- retorno[8]
  
  # (b) areally average surface albedos (lower veg, soil, snow)
  for (j in 1:nsol) {
    i <- indsol[j]
    
    asurd[i,ib] <- fl[i]*(1.-fi[i])*relod[i] + (1.-fl[i])*(1.-fi[i])*albsod[i] + fi[i]*albsnd[i]  
    asuri[i,ib] <- fl[i]*(1.-fi[i])*reloi[i] + (1.-fl[i])*(1.-fi[i])*albsoi[i] + fi[i]*albsni[i]
  } 
  
  # (c) obtain albedos, etc, for two-stream upper veg + surface
  # system, for direct and diffuse incoming unit flux
  
  retorno <- twostr(abupd, abupi,  reupd, reupi,  fupdd,  dummy, fupdi, fupii,  asurd,  asuri,  2, coszen, ib)
  
  abupd <- retorno[1]
  abupi <- retorno[2]
  reupd <- retorno[3]
  reupi <- retorno[4]
  fupdd <- retorno[5]
  dummy <- retorno[6]
  fupdi <- retorno[7]
  fupii <- retorno[8]
  
  # (d) calculate average overall albedos 
  
  for (j in 1:nsol) {
    
    i <- indsol[j]
    
    asurd[i,ib] <- fu[i]*reupd[i] + (1.-fu[i])*asurd[i,ib]
    asuri[i,ib] <- fu[i]*reupi[i] + (1.-fu[i])*asuri[i,ib]
  }
  
  assign("asurd", asurd, envir = env)
  assign("asuri", asuri, envir = env)
  assign("ablod", ablod, envir = env)
  assign("abloi", abloi, envir = env)
  assign("relod", relod, envir = env)
  assign("reloi", reloi, envir = env)
  assign("flodd", flodd, envir = env)
  assign("flodi", flodi, envir = env)
  assign("floii", floii, envir = env)
  assign("abupd", abupd, envir = env)
  assign("abupi", abupi, envir = env)
  assign("reupd", reupd, envir = env)
  assign("reupi", reupi, envir = env)
  assign("fupdd", fupdd, envir = env)
  assign("dummy", dummy, envir = env)
  assign("fupdi", fupdi, envir = env)
  assign("fupii", fupii, envir = env)
  
}


# solves canonical radiative transfer problem of two-stream veg
# layer + underlying surface of known albedo, for unit incoming
# direct or diffuse flux. returns flux absorbed within layer,
# reflected flux, and downward fluxes below layer. note that all
# direct fluxes are per unit horizontal zrea, ie, already 
# including a factor cos (zenith angle)
# 
# the solutions for the twostream approximation follow Sellers (1985),
# and Bonan (1996) (the latter being the LSM documentation)
twostr <- function(abvegd, abvegi, refld, refli, fbeldd, fbeldi, fbelid, fbelii, asurd, asuri, iv, coszen, ib) {
  
  environment(twoset) <- env
  
  # do nothing if all points in current strip have coszen le 0
  
  if (nsol == 0) 
          return()
  
  # calculate two-stream parameters omega, betad, betai, avmu, gdir
  
  omega <- array(data = 0, dim = nsol)
  betad <- array(data = 0, dim = nsol)
  betai <- array(data = 0, dim = nsol)
  avmu <- array(data = 0, dim = nsol)
  gdir <- array(data = 0, dim = nsol)
  tmp0 <- array(data = 0, dim = nsol)
  
  retorno <- twoset (omega, betad, betai, avmu, gdir, coszen, iv, ib)

  omega <- retorno$omega
  betad <- retorno$betad
  betai <- retorno$betai
  avmu  <-retorno$avmu
  gdir  <- retorno$gdir
  
  
  for (j in 1:nsol) {
    
    i <- indsol[j]
    
    # the notations used here are taken from page 21 of Bonan's LSM documentation:
    # Bonan, 1996: A Land Surface Model (LSM version 1.0) for ecological, hydrological,
    # and atmospheric studies: Technical description and user's guide. NCAR Technical
    # Note. NCAR/TN-417+STR, January 1996.
    # 
    # some temporary variables are also introduced, which are from the original
    # lsx model.
    
    b <- 1. - omega[i] * (1.-betai[i])
    c <- omega[i] * betai[i]
    
    tmp0[i] <- b*b-c*c
    
    q <- sqrt ( max(0.0, tmp0[i]) )
    k <- gdir[i] / max(coszen[i], 0.01)
    p <- avmu[i] * k
    
    # next line perturbs p if p = q
    
    if((abs(p-q)) < (.001*p)) {
      p <- (1.+ (.001 * sign(p-q))) * p
    }
    
    c0 <- omega[i] * p
    d <- c0 * betad[i]
    f <- c0 * (1.-betad[i])
    h <- q / avmu[i]
    
    sign
    
    sigma <- p*p - tmp0[i]
    
    # direct & diffuse parameters are separately calculated
    
    ud1 <- b - c/asurd[i,ib]
    ui1 <- b - c/asuri[i,ib]
    ud2 <- b - c*asurd[i,ib]
    ui2 <- b - c*asuri[i,ib]
    ud3 <- f + c*asurd[i,ib]
    
    xai <- max (lai[i,iv] + sai[i,iv], epsilon)
    
    s1 <- exp(-1.*h*xai)
    s2 <- exp(-1.*k*xai)
    
    p1 <- b + q
    p2 <- b - q
    p3 <- b + p
    p4 <- b - p
    rwork <- 1./s1
    
    # direct & diffuse parameters are separately calculated
    
    dd1 <- p1*(ud1-q)*rwork - p2*(ud1+q)*s1
    di1 <- p1*(ui1-q)*rwork - p2*(ui1+q)*s1
    dd2 <- (ud2+q)*rwork - (ud2-q)*s1
    di2 <- (ui2+q)*rwork - (ui2-q)*s1
    h1 <- -1.*d*p4 - c*f
    rwork <- s2*(d-c-h1*(ud1+p)/sigma)
    h2 <- 1./dd1*( (d-h1*p3/sigma)*(ud1-q)/s1 - p2*rwork )
    h3 <- -1./dd1*( (d-h1*p3/sigma)*(ud1+q)*s1 -p1*rwork )
    h4 <- -1.*f*p3 - c*d
    rwork <- s2*(ud3-h4*(ud2-p)/sigma)
    h5 <- -1./dd2*( h4*(ud2+q)/(sigma*s1) + rwork )
    h6 <- 1./dd2*( h4*s1*(ud2-q)/sigma + rwork )
    h7 <- c*(ui1-q)/(di1*s1)
    h8 <- -1.*c*s1*(ui1+q)/di1
    h9 <- (ui2+q)/(di2*s1)
    h10 <- -1.*s1*(ui2-q)/di2
    
    # save downward direct, diffuse fluxes below two-stream layer
    
    fbeldd[i] <- s2
    fbeldi[i] <- 0.
    fbelid[i] <- h4/sigma*s2 + h5*s1 + h6/s1
    fbelii[i] <- h9*s1 + h10/s1
    
    # save reflected flux, and flux absorbed by two-stream layer
    
    refld[i] <- h1/sigma + h2 + h3
    
    refli[i] <- h7 + h8
    absurd <- (1.-asurd[i,ib]) * fbeldd[i] + (1.-asuri[i,ib]) * fbelid[i]
    absuri <- (1.-asuri[i,ib]) * fbelii[i]
    
    
    
    abvegd[i] <- max (0, 1 - refld[i] - absurd)
    abvegi[i] <- max (0, 1 - refli[i] - absuri)
    
    # if no veg, make sure abveg (flux absorbed by veg) is exactly zero
    # if this is not done, roundoff error causes small (+/-)
    # sols, soll values in solarf and subsequent problems in turvap
    # via stomata
    
    if (xai < epsilon) abvegd[i] <- 0.0
    if (xai < epsilon) abvegi[i] <- 0.0
    
    # some terms needed in canopy scaling
    # the canopy scaling algorithm assumes that the net photosynthesis
    # is proportional to absored par (apar) during the daytime. during night,
    # the respiration is scaled using a 10-day running-average daytime canopy
    # scaling parameter.
    # 
    # apar(x) = A exp(-k x) + B exp(-h x) + C exp(h x)
    # 
    # in the equations below, 
    # 
    #    k = term[u,l](i,6)
    #    h = term[u,l](i,7)
    # 
    #    A = term[u,l](i,1) * ipardir(0)
    #    B = term[u,l](i,2) * ipardir(0) + term[u,l](i,3) * ipardif(0)
    #    C = term[u,l](i,4) * ipardir(0) + term[u,l](i,5) * ipardif(0)
    # 
    # calculations performed only for visible (ib=1)
    
    if (ib == 1) {
      
      if (iv == 1) {
        terml[1] <- k * (1. + (h4-h1) / sigma)
        terml[2] <- h * (h5 - h2)
        terml[3] <- h * (h9 - h7)
        terml[4] <- h * (h3 - h6)
        terml[5] <- h * (h8 - h10)
        terml[6] <- k
        terml[7] <- h
        
        # terml[i,1] <- k * (1. + (h4-h1) / sigma)
        # terml[i,2] <- h * (h5 - h2)
        # terml[i,3] <- h * (h9 - h7)
        # terml[i,4] <- h * (h3 - h6)
        # terml[i,5] <- h * (h8 - h10)
        # terml[i,6] <- k
        # terml[i,7] <- h
      }else{
        
        termu[1] <- k * (1. + (h4-h1) / sigma)
        termu[2] <- h * (h5 - h2)
        termu[3] <- h * (h9 - h7)
        termu[4] <- h * (h3 - h6)
        termu[5] <- h * (h8 - h10)
        termu[6] <- k
        termu[7] <- h
        
        # termu[i,1] <- k * (1. + (h4-h1) / sigma)
        # termu[i,2] <- h * (h5 - h2)
        # termu[i,3] <- h * (h9 - h7)
        # termu[i,4] <- h * (h3 - h6)
        # termu[i,5] <- h * (h8 - h10)
        # termu[i,6] <- k
        # termu[i,7] <- h
      }
      
      assign("terml", terml, envir = env)
      assign("termu", termu, envir = env)
      
    }
  }

  return(c(abvegd, abvegi, refld, refli, fbeldd, fbeldi, fbelid, fbelii))
}


# sets two-stream parameters, given single-element transmittance
# and reflectance, leaf orientation weights, and cosine of the
# zenith angle, then adjusts for amounts of intercepted snow
# 
# the two-stream parameters omega,betad,betai are weighted 
# combinations of the "exact" values for the 3 orientations:
# all vertical, all horizontal, or all random (ie, spherical)
# 
# the vertical, horizontal weights are in oriev,orieh (comveg)
# 
# the "exact" expressions are as derived in my notes(8/6/91,p.6).
# note that values for omega*betad and omega*betai are calculated
# and then divided by the new omega, since those products are 
# actually used in twostr. also those depend *linearly* on the
# single-element transmittances and reflectances tauveg, rhoveg,
# which are themselves linear weights of leaf and stem values 
# 
# for random orientation, omega*betad depends on coszen according
# to the function in array tablemu
# 
# the procedure is approximate since omega*beta[d,i] and gdir
# should depend non-linearly on the complete leaf-angle
# distribution. then we should also treat leaf and stem angle
# distributions separately, and allow for the cylindrical
# shape of stems (norman and jarvis, app.b; the expressions 
# below are appropriate for flat leaves)

twoset <- function(omega, betad, betai, avmu, gdir, coszen, iv, ib) {
  
  
  
  ntmu <- 100
  otmp <- numeric(nsol)
  
  tablemu <- c(0.5000, 0.4967, 0.4933, 0.4900, 0.4867, 0.4833, 0.4800, 0.4767, 0.4733, 0.4700, 0.4667, 0.4633, 0.4600, 0.4567, 0.4533, 0.4500, 0.4467, 0.4433, 0.4400, 0.4367, 0.4333, 0.4300, 0.4267, 0.4233, 0.4200, 0.4167, 0.4133, 0.4100, 0.4067, 0.4033, 0.4000, 0.3967, 0.3933, 0.3900, 0.3867, 0.3833, 0.3800, 0.3767, 0.3733, 0.3700, 0.3667, 0.3633, 0.3600, 0.3567, 0.3533, 0.3500, 0.3467, 0.3433, 0.3400, 0.3367, 0.3333, 0.3300, 0.3267, 0.3233, 0.3200, 0.3167, 0.3133, 0.3100, 0.3067, 0.3033, 0.3000, 0.2967, 0.2933, 0.2900, 0.2867, 0.2833, 0.2800, 0.2767, 0.2733, 0.2700, 0.2667, 0.2633, 0.2600, 0.2567, 0.2533, 0.2500, 0.2467, 0.2433, 0.2400, 0.2367, 0.2333, 0.2300, 0.2267, 0.2233, 0.2200, 0.2167, 0.2133, 0.2100, 0.2067, 0.2033, 0.2000, 0.1967, 0.1933, 0.1900, 0.1867, 0.1833, 0.1800, 0.1767, 0.1733, 0.1700, 0.1667 )
  omegasno <- c(0.9, 0.7)
  betadsno <- 0.5
  betaisno <- 0.5
  
  # Assign leaf optical properties (taken from Sellers et al., 1996
  # and Bonan, 1995)
  # These are reflectance and transmission parameters depending on what part
  # of the spectrum is used, what part of the canopy is used (lower or upper),
  # and whether the leaves are green or brown
  
  #SVC 2021 (global params) - rhovegvlg <- 0.10      # vis leaf reflectance, lower story, green leaves
  #SVC 2021 (global params) - rhovegvlb <- 0.36      # vis leaf reflectance, lower story, brown leaves
  #SVC 2021 (global params) - rhovegvu  <- 0.10       # vis leaf reflectance, upper story, green leaves
  #SVC 2021 (global params) - 
  #SVC 2021 (global params) - rhovegirlg <- 0.48     # nir leaf reflectance, lower story, green leaves
  #SVC 2021 (global params) - rhovegirlb <- 0.58     # nir leaf reflectance, lower story, brown leaves
  #SVC 2021 (global params) - rhovegiru  <- 0.40      # nir leaf reflectance, upper story, green leaves
  #SVC 2021 (global params) - 
  #SVC 2021 (global params) - tauvegvlg <- 0.07      # vis leaf transmittance, lower story, green leaves
  #SVC 2021 (global params) - tauvegvlb <- 0.22      # vis leaf transmittance, lower story, brown leaves
  #SVC 2021 (global params) - tauvegvu  <- 0.05       # vis leaf transmittance, upper story, green leaves
  #SVC 2021 (global params) - 
  #SVC 2021 (global params) - tauvegirlg <- 0.25     # nir leaf transmittance, lower story, green leaves
  #SVC 2021 (global params) - tauvegirlb <- 0.38     # nir leaf transmittance, lower story, brown leaves
  #SVC 2021 (global params) - tauvegiru  <- 0.20      # nir leaf transmittance, upper story, green leaves
  
  
  # set two-stream parameters omega, betad, betai, gdir and avmu
  # as weights of those for 100% vert,horiz,random orientations
  
  for (j in 1:nsol) {
    i <- indsol[j]
    
    # The following determines zrho (reflectance of an average leaf) and
    # ztau (transmittance of an average leaf) for location i.
    # rhoveg and tauveg are given above for both upper and lower
    # canopies and for visible and near infrared wavebands. This new
    # routine adjusts those initialized values for the lower canopy
    # depending on how much of the canopy is green.
    # zrho and ztau will be
    # weighted by greenfracl (the fraction of lower canopy that is green)
    # to allow values to go from full green values to full brown values.
    # Note that zrho for near infrared is the same for both green and
    # brown leaves but the calculation is given for consistency.
    # 
    # iv is 1 for lower canopy and 2 for upper canopy
    # ib is 1 for visible wavebands and 2 for near infrared wavebands
    
    if (iv == 2) {
      if (ib == 1) {
        
        # visible values for the upper canopy
        
        zrho <- rhovegvu
        ztau <- tauvegvu
      }else{
        
        # ir values for the upper canopy
        
        zrho <- rhovegiru
        ztau <- tauvegiru
      }
    }else{
      if (ib == 1) {
        
        # visible values for the lower canopy, weighted by how much of
        # canopy is green
        
        zrho <- greenfracl[i] * rhovegvlg + rhovegvlb * (1. - greenfracl[i])
        ztau <- greenfracl[i] * tauvegvlg + tauvegvlb * (1. - greenfracl[i])
        
      }else{
        
        # ir values for the lower canopy, weighted by how much of
        # canopy is green
        
        zrho <- greenfracl[i] * rhovegirlg + rhovegirlb * (1. - greenfracl[i])
        
        ztau <- greenfracl[i] * tauvegirlg + tauvegirlb * (1. - greenfracl[i])
      }
    }
    
    # weight for random orientation is 1 - those for vert and horiz
    
    orand <- 1. - oriev[iv] - orieh[iv]
    
    omega[i] <- zrho + ztau
    
    # ztab is transmittance coeff - for random-orientation omega*betad,
    # given by tablemu as a function of coszen
    
    itab <- round (coszen[i]*ntmu + 1)
    ztab <- tablemu[itab]
    rwork <- 1./omega[i]
    
    betad[i] <- (oriev[iv] * 0.5*(zrho + ztau) + orieh[iv] * zrho + orand * ((1.-ztab)*zrho + ztab*ztau)) * rwork
    
    betai[i] <- (oriev[iv] * 0.5*(zrho + ztau) + orieh[iv] * zrho + orand * ((2./3.)*zrho + (1./3.)*ztau)) * rwork
    
    gdir[i] <- oriev[iv] * (2./pi) * sqrt (max (0., 1.-coszen[i]*coszen[i])) + orieh[iv] * coszen[i] + orand * 0.5
    
    avmu[i] <- 1.
    
  }
  
  # adjust omega, betad and betai for amounts of intercepted snow
  # (omegasno decreases to .6 of cold values within 1 deg of tmelt)
  
  if (iv == 1) {
    
    # lower story
    
    for (j in 1:nsol) {
      i <- indsol[j]
      y <- fwetl[i]*(1.-rliql[i])
      o <- omegasno[ib]*(.6 + .4*max(0.,min(1.,(tmelt-tl[i])/1.0)))
      otmp[i]  <- omega[i]
      rwork <- y * o
      omega[i] <-  (1-y)*otmp[i] + rwork
      betad[i] <- ((1-y)*otmp[i]*betad[i] + rwork*betadsno) / omega[i]  
      betai[i] <- ((1-y)*otmp[i]*betai[i] + rwork*betaisno) / omega[i]  
    }
  }else{
    
    # upper story
    
    for (j in 1:nsol) {
      i <- indsol[j]
      x <- lai[i,iv] / max (lai[i,iv]+sai[i,iv], epsilon)
      y <- x * fwetu[i]*(1.-rliqu[i]) + (1-x) *fwets[i]*(1.-rliqs[i])
      o <- (x * min (1., max (.6, (tmelt-tu[i])/0.1)) + (1-x) * min (1., max (.6, (tmelt-ts[i])/0.1))) *  omegasno[ib] 
      
      otmp[i]  <- omega[i]
      rwork <- y * o
      
      omega[i] <- (1-y)*otmp[i] + rwork
      
      betad[i] <- ((1-y)*otmp[i]*betad[i] + rwork*betadsno) / omega[i]
      betai[i] <- ((1-y)*otmp[i]*betai[i] + rwork*betaisno) / omega[i]
    }
  }
  
  return(list(omega=omega, betad=betad, betai=betai, avmu=avmu, gdir=gdir))
  
}