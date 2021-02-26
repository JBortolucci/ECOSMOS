
C4GrassPheno <- function(jday, i) {
  
  if (jday == 1) {
    
    if (falll != 0) falll <- 0.0
    if (fallr != 0) fallr <- 0.0
    if (fallw != 0) fallw <- 0.0
    
    stsuml    <- 0.0
    precipsum <- 0.0
    
    flagl4$on  <- F
    flagl4$off <- T
    
  }
  
  ddays <- 15.0
  ddfac <- 1.0 / ddays
  
  sthreshold <- 273.16
  
# ---------------------------------------------------------------------
# * * * lower canopy onset phenology * * *
# ---------------------------------------------------------------------
#
# C3 and C4 grasses in the lower canopy act a little different than
# the trees found above. These grasses keep their leaves all year so
# their LAI is constant and they do not have a temp factor in the following
# algorithm. Instead, leaves are either green (living during the growing
# season) or brown (dead during the winter). Greenness is determined by
# the terms greenfracl3, for C3 grasses, or greenfracl4, for C4 grasses.
# The greenfrac terms act similarly to the temp factors. When onset occurs,
# greenfrac increases from 0 to 1 (all brown to all green leaves). When
# offset occurs, greenfrac decreases from 1 to 0.
# Greenfracl3 and greenfracl4 are then passed to stomata (physiology.f)
# and twoset (radiation.f) where they affect photosynthesis and leaf optical
# properties, respectively
  
  if (flagl4$off & jday < 243) {
#
# accumulate precipitation summation
#
    precipsum <- precipsum + precip
    PPTsumcrit <- 0.15 * PPTavgann
#
# determine if soil temperature summation is initiated
# if so, calculate onset summation
#
    stsuml <- ifelse(test = a11soiltd < sthreshold,
                     yes  = stsuml,
                     no   = stsuml + a11soiltd - sthreshold)
    
#
#
# for warm grasses, summation threshold is 1320
#
    if (precipsum >= PPTsumcrit & stsuml >= 1320) {
      flagl4$on  <- TRUE
      flagl4$off <- FALSE
    }
  }
#
# if onset has occured then determine leaf color
# templs is retained so that deciduous shrubs may lose their leaves
#
  if (flagl4$on) greenfrac[i] = min(1.0, greenfrac[i] + ddfac)
#
# ---------------------------------------------------------------------
# * * * lower canopy offset phenology * * *
# ---------------------------------------------------------------------
#
# This is tuned White et al. version that looks at the stress of the plants
# and determines if 'cold' conditions are met
#
  if (flagl4$on & jday >= 243 & (stresstl < 0.27 | (a3tdmin - 273.16) <= tminavgann)) {
    
    flagl4$off <- TRUE
    flagl4$on  <- FALSE
    
  }
#
# if offset has occured then determine leaf display
#
  if (flagl4$off) greenfrac[i] = max(0.0, greenfrac[i] - ddfac)
  
  if(isimveg != 0) {
    
    Deadleaves$C4Grass <- cbiol[i] / (tauleaf[i] * 365)
    Deadfroots$C4Grass <- cbior[i] / (tauroot[i] * 365)
  
    cbiol[i] <- cbiol[i] + aleaf[i] * max(0, adnpp[i]) - Deadleaves$C4Grass
    cbior[i] <- cbior[i] + aroot[i] * max(0, adnpp[i]) - Deadfroots$C4Grass
    
    plai[i] <- cbiol[i] * specla[i]
    biomass[i] <- cbiol[i] + cbior[i]
    
    falll <- falll + Deadleaves$C4Grass
    fallr <- fallr + Deadfroots$C4Grass
    fallw <- 0.0
    
    print(c('cbiorC4' = cbior[i], 'plaiC4' = plai[i], 'adnppC4' = adnpp[i]))
  
  }
  
  assign('flagl4'    , flagl4    , envir = env)
  assign('greenfrac' , greenfrac , envir = env)
  assign('stsuml'    , stsuml    , envir = env)
  assign('precipsum' , precipsum , envir = env)
  assign('cbiol'     , cbiol     , envir = env)
  assign('cbior'     , cbior     , envir = env)
  assign('plai'      , plai      , envir = env)
  assign('biomass'   , biomass   , envir = env)
  assign('Deadleaves', Deadleaves, envir = env)
  assign('Deadfroots', Deadfroots, envir = env)
  assign('falll'     , falll     , envir = env)
  assign('fallr'     , fallr     , envir = env)
  assign('fallw'     , fallw     , envir = env)

}