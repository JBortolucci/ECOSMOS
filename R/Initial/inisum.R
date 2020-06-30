# ---------------------------------------------------------------------
#  subroutine inisurf  
# ---------------------------------------------------------------------
# Does initialization for time averaging
# ---------------------------------------------------------------------
# Globals:
#
# adaet
# adco2mic
# adco2root
# addrainage
# adevap
# adnmintot
# adrain
# adrh
# adsnod
# adsnof
# adsnow
# adsrunoff
# adtlaysoi
# adtrans
# adtratio
# adtrunoff
# adtsoi
# adtsoic
# adud
# adwisoi
# adwlaysoi
# adwsoi
# adwsoic
# amaet
# amawc
# amcloud
# amco2mic
# amco2root
# amdrainage
# amirdown
# amirup
# amlail
# amlaiu
# amlatent
# amnmintot
# amno3leach
# amnpp
# amqa
# amrain
# amrh
# amsens
# amsnod
# amsnof
# amsnow
# amsolar
# amsrunoff
# amtemp
# amtotnleach
# amtrunoff
# amtsoi
# amvwc
# amwisoi
# amwsoi
# ayaet
# ayalit
# ayanlit
# ayawc
# ayblit
# aybnlit
# aycmic
# ayco2mic
# ayco2root
# aycsoi
# aydrainage
# aygpp
# ayirdown
# ayirup
# aylatent
# aynmintot
# aynsoi
# ayprcp
# ayrootbio
# aysens
# aysolar
# aysrunoff
# aystresstl
# aystresstu
# aytrans
# aytrunoff
# aytsoi
# ayvwc
# aywisoi
# aywsoi
# decompl
# decomps
# fi
# firefac
# fl
# fu
# hsno
# hsoi
# lai
# npft
# npoi
# nsnolay
# nsoilay
# poros
# rhos
# rhow
# rnet
# sai
# wipud
# wisoi
# wliql
# wliqs
# wliqu
# wpud
# wsnol
# wsnos
# wsnou
# wsoi
# wtot
# ---------------------------------------------------------------------

inisum <- function() {
  
  wtot <- 0
  
  # initialize total water content in soil + snow + vegetation (for mass conservation check)
  
  wtot <- (wliqu + wsnou) * fu * 2.0 * lai[2] + (wliqs + wsnos) * fu * 2.0 * sai[2] +  (wliql + wsnol) * fl * 2.0 *  (lai[1] + sai[1]) * (1. - fi) + wpud + wipud
  for(k in 1:nsoilay) {
    wtot <- wtot + poros[k] * wsoi[k] * (1 - wisoi[k]) * 
      hsoi[k] * rhow + poros[k] * wisoi[k] * hsoi[k] * rhow
  }
  
  for(k in 1:nsnolay) {
    wtot = wtot + fi * rhos * hsno[k]
  }
  assign("wtot", wtot, envir = env)
  
  
  # Daily means
  assign("adrain", array(0, 1) , envir = env)
  assign("adsnow", array(0, 1) , envir = env)
  assign("adaet", array(0, 1) , envir = env)
  assign("adtrans", array(0, 1) , envir = env)
  assign("adevap", array(0, 1) , envir = env)
  assign("adtratio", array(0, 1) , envir = env)
  assign("adtrunoff", array(0, 1) , envir = env)
  assign("adsrunoff", array(0, 1) , envir = env)
  assign("addrainage", array(0, 1) , envir = env)
  assign("adrh", array(0, 1) , envir = env)
  assign("adud", array(0, 1) , envir = env)
  assign("adsnod", array(0, 1) , envir = env)
  assign("adsnof", array(0, 1) , envir = env)
  assign("adwsoi", array(0, 1) , envir = env)
  assign("adtsoi", array(0, 1) , envir = env)
  assign("adwisoi", array(0, 1) , envir = env)
  assign("adtlaysoi", array(0, 1) , envir = env)
  assign("adwlaysoi", array(0, 1) , envir = env)
  assign("adwsoic", array(0, 1) , envir = env)
  assign("adtsoic", array(0, 1) , envir = env)
  assign("adco2mic", array(0, 1) , envir = env)
  assign("adco2root", array(0, 1) , envir = env)
  assign("decompl", array(0, 1) , envir = env)
  assign("decomps", array(0, 1) , envir = env)
  assign("adnmintot", array(0, 1) , envir = env)
  
  # monthly mean quanties
  assign("amtemp", array(0, 1) , envir = env)
  assign("amrain", array(0, 1) , envir = env)
  assign("amsnow", array(0, 1) , envir = env)
  assign("amaet", array(0, 1) , envir = env)
  assign("amtrunoff", array(0, 1) , envir = env)
  assign("amsrunoff", array(0, 1) , envir = env)
  assign("amdrainage", array(0, 1) , envir = env)
  assign("amtotnleach", array(0, 1) , envir = env)
  assign("amno3leach", array(0, 1) , envir = env)
  assign("amcloud", array(0, 1) , envir = env)
  assign("amqa", array(0, 1) , envir = env)
  assign("amrh", array(0, 1) , envir = env)
  assign("amsolar", array(0, 1) , envir = env)
  assign("rnet", array(0, 1) , envir = env)
  assign("amirup", array(0, 1) , envir = env)
  assign("amirdown", array(0, 1) , envir = env)
  assign("amsens", array(0, 1) , envir = env)
  assign("amlatent", array(0, 1) , envir = env)
  assign("amlaiu", array(0, 1) , envir = env)
  assign("amlail", array(0, 1) , envir = env)
  assign("amtsoi", array(0, 1) , envir = env)
  assign("amwsoi", array(0, 1) , envir = env)
  assign("amwisoi", array(0, 1) , envir = env)
  assign("amvwc", array(0, 1) , envir = env)
  assign("amawc", array(0, 1) , envir = env)
  assign("amsnod", array(0, 1) , envir = env)
  assign("amsnof", array(0, 1) , envir = env)
  assign("amco2mic", array(0, 1) , envir = env)
  assign("amco2root", array(0, 1) , envir = env)
  assign("amnmintot", array(0, 1) , envir = env)
  assign("amnpp", matrix(0, 1, npft) , envir = env)
  
  
  # Annual mean quantities
  assign("aysolar", array(0, 1), envir = env)
  assign("ayirup", array(0, 1), envir = env)
  assign("ayirdown", array(0, 1), envir = env)
  assign("aysens", array(0, 1), envir = env)
  assign("aylatent", array(0, 1), envir = env)
  assign("ayprcp", array(0, 1), envir = env)
  assign("ayaet", array(0, 1), envir = env)
  assign("aytrans", array(0, 1), envir = env)
  assign("aytrunoff", array(0, 1), envir = env)
  assign("aysrunoff", array(0, 1), envir = env)
  assign("aydrainage", array(0, 1), envir = env)
  assign("aywsoi", array(0, 1), envir = env)
  assign("aywisoi", array(0, 1), envir = env)
  assign("aytsoi", array(0, 1), envir = env)
  assign("ayvwc", array(0, 1), envir = env)
  assign("ayawc", array(0, 1), envir = env)
  assign("aystresstu", array(0, 1), envir = env)
  assign("aystresstl", array(0, 1), envir = env)
  assign("firefac", array(0, 1), envir = env)
  assign("ayco2mic", array(0, 1), envir = env)
  assign("ayco2root", array(0, 1), envir = env)
  assign("ayrootbio", array(0, 1), envir = env)
  assign("aynmintot", array(0, 1), envir = env)
  assign("ayalit", array(0, 1), envir = env)
  assign("ayblit", array(0, 1), envir = env)
  assign("aycsoi", array(0, 1), envir = env)
  assign("aycmic", array(0, 1), envir = env)
  assign("ayanlit", array(0, 1), envir = env)
  assign("aybnlit", array(0, 1), envir = env)
  assign("aynsoi", array(0, 1), envir = env)
  
  
  assign("aygpp", matrix(0, 1, npft) , envir = env)
  
}


