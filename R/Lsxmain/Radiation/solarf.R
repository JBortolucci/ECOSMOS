# calculates solar fluxes absorbed by upper and lower stories,
# soil and snow
# 
# zenith angles are in comatm array coszen, and must be the same
# as supplied earlier to solalb
# 
# solarf uses the results obtained earlier by solalb and 
# stored in com1d arrays. the absorbed fluxes are returned in
# com1d arrays sol[u,s,l,g,i]
# 
# the procedure is first to calculate the upper-story absorbed
# fluxes and fluxes below the upper story, then the lower-story
# absorbed fluxes and fluxes below the lower story, then fluxes
# absorbed by the soil and snow
# 
# ib = waveband number

solarfR <- function(envi, ib) {

  # do nothing if all points in current strip have coszen le 0
  
  if (nsol == 0)
    return()
  
  # (f) calculate fluxes absorbed by upper leaves and stems,
  # and downward fluxes below upper veg, using unit-flux
  # results of solalb(c) (apportion absorbed flux between
  # leaves and stems in proportion to their lai and sai)
  for (j in 1:nsol) {
    i <- indsol[j]
    x <- solad[ib]*abupd[i] + solai[ib]*abupi[i]
    # x <- solad[i,ib]*abupd[i] + solai[i,ib]*abupi[i]
    y <- lai[i,2] / max (lai[i,2]+sai[i,2], epsilon)
    solu[i] <- solu[i] + x * y
    sols[i] <- sols[i] + x * (1.-y)
    sol2d[i] <- solad[ib]*fupdd[i]
    sol2i[i] <- solad[ib]*fupdi[i] + solai[ib]*fupii[i]
  }
  
  # (g) areally average fluxes to lower veg, soil, snow
  
  for (j in 1:nsol) {
    i <- indsol[j]
    sol3d[i] <- fu[i]*sol2d[i] + (1.-fu[i])*solad[ib]
    sol3i[i] <- fu[i]*sol2i[i] + (1.-fu[i])*solai[ib]
  }
  
  # (h,i) calculate fluxes absorbed by lower veg, snow-free soil
  # and snow, using results of (g) and unit-flux results
  # of solalb(a)
  
  for (j in 1:nsol) {
    
    i <- indsol[j]
    soll[i] <- soll[i] + sol3d[i]*ablod[i] + sol3i[i]*abloi[i]
    
    xd <- (fl[i]*flodd[i] + 1.-fl[i]) * sol3d[i]
    
    xi <- fl[i]*(sol3d[i]*flodi[i] + sol3i[i]*floii[i]) + (1.-fl[i]) * sol3i[i]
    
    solg[i] <- solg[i] + (1.-albsod[i])*xd + (1.-albsoi[i])*xi
    
    solsoi[i] <- solsoi[i]+ xd+xi
    
    soli[i] <- soli[i] + (1.-albsnd[i])*sol3d[i] + (1.-albsni[i])*sol3i[i]
  }
  
  # estimate absorbed pars at top of canopy, toppar[u,l] and
  # some canopy scaling parameters
  # 
  # this neglects complications due to differing values of dead vs 
  # live elements, averaged into rhoveg, tauveg in vegdat, and 
  # modifications of omega due to intercepted snow in twoset
  # 
  # do only for visible band (ib=1)
  
  if (ib == 1) {
    
    for (j in 1:nsol) {
      
      i <- indsol[j]
      
      # the canopy scaling algorithm assumes that the net photosynthesis
      # is proportional to absored par (apar) during the daytime. during night,
      # the respiration is scaled using a 10-day running-average daytime canopy
      # scaling parameter.
      # 
      # apar(x) = A exp(-k x) + B exp(-h x) + C exp(h x)
      # 
      # some of the required terms (i.e. term[u,l] are calculated in the subroutine 'twostr'.
      # in the equations below, 
      # 
      # A = scalcoefu(i,1) = term[u,l](i,1) * ipardir(0)
      # B = scalcoefu(i,2) = term[u,l](i,2) * ipardir(0) + term[u,l](i,3) * ipardif(0)
      # C = scalcoefu(i,3) = term[u,l](i,4) * ipardir(0) + term[u,l](i,5) * ipardif(0)
      # A + B + C = scalcoefu(i,4) = also absorbed par at canopy of canopy by leaves & stems
      # 
      # upper canopy:
      # 
      # total single-sided lai+sai
      
      xaiu <- max (lai[i,2]+sai[i,2], epsilon)
      
      # some terms required for use in canopy scaling:
      
      scalcoefu[1] <- termu[1] * solad[ib]
      
      scalcoefu[2] <- termu[2] * solad[ib] + termu[3] * solai[ib]
      
      scalcoefu[3] <- termu[4] * solad[ib] + termu[5] * solai[ib]
      
      scalcoefu[4] <- scalcoefu[1] + scalcoefu[2] + scalcoefu[3]
      
      # apar of the "top" leaves of the canopy
      
      topparu[i] <- scalcoefu[4] * lai[i,2] / xaiu
      
      # lower canopy:
      # 
      # total single-sided lai+sai
      # 
      xail <- max (lai[i,1]+sai[i,1], epsilon)
      
      # some terms required for use in canopy scaling:
      
      scalcoefl[1] <- terml[1] * sol3d[i]
      
      scalcoefl[2] <- terml[2] * sol3d[i] + terml[3] * sol3i[i]
      
      scalcoefl[3] <- terml[4] * sol3d[i] + terml[5] * sol3i[i]
      
      scalcoefl[4] <- scalcoefl[1] + scalcoefl[2] + scalcoefl[3]
      
      # apar of the "top" leaves of the canopy
      
      topparl[i] <- scalcoefl[4] * lai[i,1] / xail
    
      
    }
  }
  
  assign("solu", solu, envir = env)
  assign("sols", sols, envir = env)
  assign("sol2d", sol2d, envir = env)
  assign("sol2i", sol2i, envir = env)
  assign("sol3d", sol3d, envir = env)
  assign("sol3i", sol3i, envir = env)
  assign("soll", soll, envir = env)
  assign("solg", solg, envir = env)
  assign("solsoi", solsoi, envir = env)
  assign("soli", soli, envir = env)
  assign("scalcoefu", scalcoefu, envir = env)
  assign("topparu", topparu, envir = env)
  assign("scalcoefl", scalcoefl, envir = env)
  assign("topparl", topparl, envir = env)
}