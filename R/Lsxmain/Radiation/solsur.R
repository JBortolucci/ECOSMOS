# sets surface albedos for soil and snow, prior to other
# solar calculations

# ib = waveband number

solsurR <- function(envi, ib) {                
  
  # set the "standard" snow values:
  
  a7svlo <- 0.90 
  a7svhi <- 0.70
  a7snlo <- 0.60
  a7snhi <- 0.40
  
  #     t7shi ... high threshold temperature for snow albedo
  #     t7slo ... low  threshold temperature for snow albedo
  
  t7shi <- tmelt
  t7slo <- tmelt - 15.0
  
  # do nothing if all points in current strip have coszen le 0
  
  if (nsol == 0) 
    return()
  if (ib == 1) {
    # soil albedos (visible waveband)
    for (j in 1:nsol) {
      i <- indsol[j]
      # change the soil albedo as a function of soil moisture
      zw <- wsoi[i,1] * (1.-wisoi[i,1])
      
      dinc <- 1.0 + 1.0 * min(1., max (0.0, 1. - (zw /.50) ))
      
      albsod[i] <- min(albsav[i] * dinc, .80)
      albsoi[i] <- albsod[i]
      
    }
    # snow albedos (visible waveband)
    for (j in 1:nsol) {
      i <- indsol[j]
      x <- (a7svhi*(tsno[i,1]-t7slo) + a7svlo*(t7shi-tsno[i,1]))/ (t7shi-t7slo)   #TODO x(i)? real x(npoi), zfac(npoi)
      
      x <- min (a7svlo, max (a7svhi, x))                                       #TODO x(i)? real x(npoi), zfac(npoi)
      
      zfac  <- max ( 0., 1.5 / (1.0 + 4.*coszen[i]) - 0.5 )                       #TODO zfac(i)? real x(npoi), zfac(npoi)
      albsnd[i] <- min (0.99, x + (1.-x)*zfac)
      albsni[i] <- min (1., x)
    }
  }else{
    # soil albedos (near-ir waveband)
    for (j in 1:nsol) {
      i <- indsol[j]
      # lsx.2 formulation (different from lsx.1)
      
      zw <- wsoi[i,1] * (1. - wisoi[i,1])
      
      dinc <- 1.0 + 1.0 * min (1., max (0.0, 1.0 - (zw / .50)  ))
      
      albsod[i] <- min (albsan[i] * dinc, .80)
      albsoi[i] <- albsod[i]
    }
    # snow albedos (near-ir waveband)
    for (j in 1:nsol) {
      i <- indsol[j]
      
      x <- (a7snhi*(tsno[i,1]-t7slo) + a7snlo*(t7shi-tsno[i,1])) / (t7shi-t7slo) 
      x <- min (a7snlo, max (a7snhi, x))                                          
      
      zfac <- max ( 0., 1.5/(1.+4.*coszen[i]) - 0.5 )                            
      
      albsnd[i] <- min (0.99, x + (1.-x)*zfac)
      albsni[i] <- min (1., x)                                                
    }
  }
  assign("albsod", albsod, envir = env)
  assign("albsoi", albsoi, envir = env)
  assign("albsnd", albsnd, envir = env)
  assign("albsni", albsni, envir = env)
}