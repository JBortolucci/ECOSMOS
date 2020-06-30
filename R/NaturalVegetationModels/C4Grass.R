
C4GrassPheno <- function(jday, i) {
  
  ddays <- 15.0
  ddfac <- 1.0 / ddays
  
  sthreshold = 273.16
  
  # initialize this year's values of stsumu, stsuml, precipsum
  
  if (jday == 1) {
    
    stsumu    <- 0
    stsuml    <- 0
    precipsum <- 0
    
    # PFT_UPDATE:  locais
    onflagl4  <- F #as.logical(array(F, 1))
    offflagl4 <- T #as.logical(array(T, 1))
    
  }
  
  
  if (offflagl4 & (jday < 243)) {
    # accumulate precipitation summation
    precipsum <- precipsum + precip
    PPTsumcrit <- 0.15 * PPTavgann
    
    # determine if soil temperature summation is initiated
    # if so, calculate onset summation
    if (a11soiltd < sthreshold) {
      stsuml <- stsuml
    }else{
      stsuml <- stsuml + a11soiltd - sthreshold
    }
    # for warm grasses, summation threshold is 1320
    # for cool grasses, summation threshold is 420
    # print(paste("stsuml ", stsuml, sep = ""))
    # print(paste("precipsum ", precipsum, sep = ""))
    # print(paste("PPTsumcrit ", PPTsumcrit, sep = ""))
    # print(paste("precip ", precip, sep = ""))
    if (precipsum >= PPTsumcrit) {
      if (stsuml >= 1320.) {
        onflagl4  <- T 
        offflagl4 <- F
      }
    }
  }
  
  # if onset has occured then determine leaf color
  # templs is retained so that deciduous shrubs may lose their leaves
  # 
  if(onflagl4) {
    
    # PFT_UPDATE:
    # greenfracl4 <- min (1., greenfracl4 + ddfac)
    greenfrac[i] <- min (1., greenfrac[i] + ddfac)
    
  }
  
  # This is tuned White et al. version that looks at the stress of the plants
  # and determines if 'cold' conditions are met
  if (onflagl4 & jday >= 243) {
    if ((stresstl < 0.27) | ((a3tdmin-273.16) <= tminavgann)) {
      offflagl4 <- T
      onflagl4  <- F
    }
  }
  
  # if offset has occured then determine leaf display
  if (offflagl4) {
    # PFT_UPDATE:
    # greenfracl4 <- max (0., greenfracl4 - ddfac)
    greenfrac[i] <- max (0., greenfrac[i] - ddfac)
    # print(greenfrac)
  }
  assign("stsuml", stsuml, envir = env)
  assign("precipsum", precipsum, envir = env)
  assign("greenfrac", greenfrac, envir = env)
  assign("onflagl4", onflagl4, envir = env)
  assign("offflagl4", offflagl4, envir = env)
  
}
