
UpperPheno <- function(jday, i) {
  
  ddays <- 15.0
  ddfac <- 1.0 / ddays
  
  sthreshold = 273.16
  
  # initialize this year's values of stsumu, stsuml, precipsum
  
  if (jday == 1) {
    
    stsumu    <- 0
    stsuml    <- 0
    precipsum <- 0
    
    # PFT_UPDATE:  locais
    onflagu <- F #as.logical(array(F, 1))
    offflagu <- T #as.logical(array(T, 1))
  }

  if (offflagu & (jday < 243)) {
    
    # sumcom is the threshold for upper canopy onset
    sumcom <- exp (4.395 + 0.129 * Tavgann)
    
    # determine if soil temperature summation is initiated
    # if so, calculate onset summation stsumu and stsuml
    
    if (a11soiltd < sthreshold) {
      stsumu <- stsumu
    }else{
      stsumu <- stsumu + a11soiltd - sthreshold
    }
    
    # determine if onset has occured
    
    if (stsumu >= sumcom) {
      onflagu <- T 
      offflagu <- F
    }
  }
  
  # if onset has occured then determine leaf display
  if (onflagu) {
    # tempu <- min (1., tempu + ddfac)
    temp[i] <- min (1., temp[i] + ddfac)
  }
  
  if (onflagu & jday >= 243) {
      if (((daylength <= 685.) & (a11soiltd <= (17.15+273.16))) | (a11soiltd <= (2.+273.16))) {
        offflagu <- T
        onflagu <- F
      }
  }
  
  # if offset has occured then determine leaf display
  if (offflagu) {
    # tempu <- max (0., tempu - ddfac)
    temp[i] <- max (0., temp[í] - ddfac)
  }
  
  assign("temp", temp, envir = env)
}


