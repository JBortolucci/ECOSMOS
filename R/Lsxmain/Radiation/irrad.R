# calculates overall emitted ir flux, and net absorbed minus
# emitted ir fluxes for upper leaves, upper stems, lower story,
# soil and snow. assumes upper leaves, upper stems and lower
# story each form a semi-transparent plane, with the upper-leaf
# plane just above the upper-stem plane. the soil and snow
# surfaces have emissivities of 0.95.
# 
# the incoming flux is supplied in comatm array fira
# 
# the emitted ir flux by overall surface system is returned in
# com1d array firb - the ir fluxes absorbed by upper leaves,
# upper stems, lower veg, soil and snow are returned in com1d
# arrays firu, firs, firl, firg and firi
# 
# other com1d arrays used are:
# 
# emu, ems, eml  = emissivities of the vegetation planes
# fup, fdown     = upward and downward fluxes below tree level

irradR <- function(envi){
  # set emissivities of soil and snow
  
  emisoil <- 0.95 
  emisnow <- 0.95
  
  # use uniform value 1.0 for average diffuse optical depth
  # (although an array for solar, all values are set to 1 in twoset).
  
  avmuir <- 1.
  
  
  emu <- 1. - exp ( -lai[2] / avmuir )
  ems <- 1. - exp ( -sai[2] / avmuir )
  eml <- 1. - exp ( -(lai[1]+sai[1]) / avmuir )
  
  emg <- emisoil
  emi <- emisnow
  
  
  fdown <-  (1.-fu) * fira + fu * ( (1.-emu)*(1.-ems)*fira + emu* (1.-ems)*stef*(tu^4) + ems*stef*(ts^4) )
  
  fdowng <- (1.-eml)*fdown + eml*stef*(tl^4)
  
  fupg <- (1.-emg)*fdowng + emg*stef*(tg^4)
  
  fupgb <- (1.-emg)*fdown + emg*stef*(tg^4)
  
  fupi <- (1.-emi)*fdown + emi*stef*(ti^4)
  
  fup <- (1.-fi)*(fl*(eml *stef*(tl^4) + (1.-eml)*fupg) +(1.-fl)*fupgb) + fi * fupi
  
  firb <- (1.-fu) * fup + fu * ((1.-emu)*(1.-ems)*fup + emu*stef*(tu^4) + ems*(1.-emu)*stef*(ts^4))
  
  firu <- emu*ems*stef*(ts^4) + emu*(1.-ems)*fup + emu*fira - 2*emu*stef*(tu^4)
  
  firs <- ems*emu*stef*(tu^4) + ems*fup + ems*(1.-emu)*fira - 2*ems*stef*(ts^4)
  
  firl <- eml*fdown + eml*fupg - 2*eml*stef*(tl^4)
  
  firg <- fl * (fdowng - fupg) + (1.-fl) * (fdown  - fupgb)
  
  firi <- fdown - fupi
  
  assign("firb", firb, envir = env)
  assign("firu", firu, envir = env)
  assign("firs", firs, envir = env)
  assign("firl", firl, envir = env)
  assign("firg", firg, envir = env)
  assign("firi", firi, envir = env)
}