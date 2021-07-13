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
  # CSVC - Remover -> assign("amtemp", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amrain", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsnow", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amaet", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amtrunoff", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsrunoff", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amdrainage", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amtotnleach", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amno3leach", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amcloud", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amqa", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amrh", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsolar", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("rnet", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amirup", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amirdown", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsens", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amlatent", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amlaiu", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amlail", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amtsoi", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amwsoi", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amwisoi", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amvwc", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amawc", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsnod", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amsnof", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amco2mic", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amco2root", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amnmintot", array(0, 1) , envir = env)
  # CSVC - Remover -> assign("amnpp", matrix(0, 1, npft) , envir = env)
  
  
  # Annual mean quantities
  # CSVC - Remover -> assign("aysolar", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayirup", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayirdown", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aysens", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aylatent", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayprcp", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayaet", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aytrans", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aytrunoff", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aysrunoff", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aydrainage", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aywsoi", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aywisoi", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aytsoi", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayvwc", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayawc", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aystresstu", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aystresstl", array(0, 1), envir = env)
  # CSVC - Remover -> assign("firefac", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayco2mic", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayco2root", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayrootbio", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aynmintot", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayalit", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayblit", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aycsoi", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aycmic", array(0, 1), envir = env)
  # CSVC - Remover -> assign("ayanlit", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aybnlit", array(0, 1), envir = env)
  # CSVC - Remover -> assign("aynsoi", array(0, 1), envir = env)
  # CSVC - Remover ->  assign("aygpp", matrix(0, 1, npft) , envir = env)
  
}


