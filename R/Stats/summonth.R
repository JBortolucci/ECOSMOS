# Global Vars:
# amaet        # monthly average aet (mm/day)
# amawc        # monthly average 1m plant-available water content (fraction)
# amcloud      # monthly average cloudiness (percent)
# amco2mic     # monthly total CO2 flux from microbial respiration (kg-C/m**2/month)
# amco2ratio   # monthly ratio of root to total co2 flux
# amco2root    # monthly total CO2 flux from soil due to root respiration (kg-C/m**2/month)
# amco2soi     # monthly total soil CO2 flux from microbial and root respiration (kg-C/m**2/month)
# amdrainage   # monthly average drainage (mm/day)
# amirdown     # monthly average downward ir radiation (W/m**2)
# amirup       # monthly average upward ir radiation (W/m**2)
# amlail       # monthly average lai for lower canopy (m**2/m**2)
# amlaiu       # monthly average lai for upper canopy (m**2/m**2)
# amlatent     # monthly average latent heat flux (W/m**2)
# amneetot     # monthly total net ecosystem exchange of CO2 (kg-C/m**2/month)
# amnmintot    # monthly total N mineralization from microbes (kg-N/m**2/month)
# amno3leach   # monthly average nitrate leaching (kg/ha/day)
# amnpp        # monthly total npp for each plant type (kg-C/m**2/month)
# amnpptot     # monthly total npp for ecosystem (kg-C/m**2/month)
# amqa         # monthly average specific humidity (kg-h2o/kg-air)
# amrain       # monthly average rainfall rate (mm/day)
# amrh         # monthly average rh (percent)
# amsens       # monthly average sensible heat flux (W/m**2)
# amsnod       # monthly average snow depth (m)
# amsnof       # monthly average snow fraction (fraction)
# amsnow       # monthly average snowfall rate (mm/day)
# amsolar      # monthly average incident solar radiation (W/m**2)
# amsrunoff    # monthly average surface runoff (mm/day)
# amtemp       # monthly average air temperature (C)
# amtotnleach  # monthly average total nitrogen leaching (kg/ha/day)
# amtrans      # monthly average transpiration (mm/day)
# amtratio     # monthly average transpiration:ET ratio (fraction)
# amtrunoff    # monthly average total runoff (mm/day)
# amtsoi       # monthly average 1m soil temperature (C)
# amvwc        # monthly average 1m volumetric water content (fraction)
# amwisoi      # monthly average 1m soil ice (fraction)
# amwsoi       # monthly average 1m soil moisture (fraction)
# asurd        # direct albedo of surface system
# asuri        # diffuse albedo of surface system 
# cloud        # cloud fraction
# dtime        # model timestep (seconds)
# fi           # fractional snow cover
# fira         # incoming ir flux (W m-2)
# firb         # net upward ir radiation at reference atmospheric level za (W m-2)
# fl           # fraction of snow-free area covered by lower  canopy
# fout         # current timestep total nitrogen leaching (flux)  (kg_solute/m2)
# fsena        # downward sensible heat flux between za & z12 at za (W m-2)
# fu           # fraction of overall area covered by upper canopy
# fvapa        # downward h2o vapor flux between za & z12 at za (kg m-2 s-1)
# gdrain       # drainage rate out of bottom of lowest soil layer (kg_h2o m-2 s-1)
# grunof       # surface runoff rate (kg_h2o m-2 s-1)
# gtransl      # transpiration from lower canopy (kg_h2o m-2 s-1)
# gtransu      # transpiration from upper canopy (kg_h2o m-2 s-1)
# hsno         # thickness of snow layers (m)
# hsoi         # soil layer thickness (m)
# hvap         # latent heat of vaporization of water (J kg-1)
# lai          # canopy single-sided leaf area index (area leaf/area veg)
# ndaypm       # number of days per month
# nmtimes      # counter for monthly average calculations
# nout         # current timestep nitrate leaching flux  (kg_solute/m2)
# poros        # porosity (mass of h2o per unit vol at sat / rhow)
# qa           # specific humidity (kg_h2o/kg_air)
# raina        # rainfall rate (mm/s or kg m-2 s-1)
# rh           # relative humidity(%)
# rnet         # monthly average net radiation (W/m**2)
# snowa        # snowfall rate (mm/s or kg m-2 s-1 of water)
# solad        # direct downward solar flux (W m-2)
# solai        # diffuse downward solar flux (W m-2)
# swilt        # wilting soil moisture value (fraction of pore space)
# ta           # air temperature (K)
# tco2mic      # instantaneous microbial co2 flux from soil (mol-CO2 / m-2 / second)
# tco2root     # instantaneous fine co2 flux from soil (mol-CO2 / m-2 / second)
# tnmin        # instantaneous nitrogen mineralization (kg_N m-2/timestep)
# tnpp         # instantaneous NPP for each pft (mol-CO2 / m-2 / second)
# tsoi         # soil temperature for each layer (K)
# wisoi        # fraction of soil pore space containing ice
# wsoi         # fraction of soil pore space containing liquid water

summonth <- function (istep, iday, imonth) {
  
  # first convert to units that make sense for output
  #
  # - convert all temperatures to deg c
  # - convert all liquid or vapor fluxes to mm / day
  # - redefine upwd directed heat fluxes as positive
  
  # ---------------------------------------------------------------------
  # *  *  * update counters and working variables *  * *
  # ---------------------------------------------------------------------
  # 
  # if the first timestep of the month then reset averages
  
  if ((istep == 1) && (iday == 1)) nmtimes <- 0
  
  # accumulate terms
  nmtimes <- nmtimes + 1
  
  # working variables
  
  # rwork4 for conversion of nitrogen mineralization (moles)
  rwork <- 1 / nmtimes
  rwork2 <- ndaypm[imonth] * 86400
  rwork3 <- ndaypm[imonth] * 86400 * 12e-3
  rwork4 <- ndaypm[imonth] * 86400 * 14e-3
  

  
  rdepth <- 1 / (hsoi[1] + hsoi[2] + hsoi[3] + hsoi[4])
  
  # ---------------------------------------------------------------------
  # *  *  * monthly water budget terms *  * *
  # ---------------------------------------------------------------------
  
  amrain <- ((nmtimes - 1) * amrain + raina * 86400) * rwork
  amsnow <- ((nmtimes - 1) * amsnow + snowa * 86400) * rwork
  amaet <- ((nmtimes - 1) * amaet  - fvapa * 86400) * rwork
  amtrans <- ((nmtimes - 1) * amtrans + (gtransl + gtransu) * 86400) * rwork
  amtratio <- max(0, min(1, amtrans / amaet)) 
  
  amtrunoff <- ((nmtimes - 1) * amtrunoff  +(grunof + gdrain) * 86400) * rwork
  amsrunoff <- ((nmtimes - 1) * amsrunoff  + grunof * 86400) * rwork
  amdrainage <- ((nmtimes - 1) * amdrainage + gdrain * 86400) * rwork
  
  # nitrogen variables
  # converted from kg / m2 / s rate to kg / ha / day average
  amtotnleach <- ((nmtimes - 1) * amtotnleach + fout[5] / dtime  * 1e+04 * 86400) * rwork
  amno3leach <- ((nmtimes - 1) * amno3leach + nout[5] / dtime  * 1e+04 * 86400) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * monthly atmospheric terms *  * *
  # ---------------------------------------------------------------------
  
  amtemp <- ((nmtimes - 1) * amtemp + ta - 273.16) * rwork
  amcloud <- ((nmtimes - 1) * amcloud + cloud * 100) * rwork
  amqa <- ((nmtimes - 1) * amqa + qa) * rwork
  amrh <- ((nmtimes - 1) * amrh + rh) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * energy budget terms *  * *
  # ---------------------------------------------------------------------
  
  solartot <- solad[1] + solad[2] + solai[1] + solai[2]
  
  amsolar <- ((nmtimes - 1) * amsolar  + solartot) * rwork
  amirup <- ((nmtimes - 1) * amirup + firb) * rwork
  amirdown <- ((nmtimes - 1) * amirdown + fira) * rwork
  amsens <- ((nmtimes - 1) * amsens - fsena) * rwork
  amlatent <- ((nmtimes - 1) * amlatent - fvapa * hvap) * rwork
  
  
  rnet <- ( (nmtimes - 1) * rnet + ( solad[1] * (1 - asurd[1]) +  
                                       solad[2] * (1 - asurd[2]) + solai[1] * (1 - asuri[1]) +
                                       solai[2] * (1 - asuri[2]) + fira - firb )   ) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * monthly vegetation parameters *  * *
  # ---------------------------------------------------------------------
  
  amlaiu <- ((nmtimes - 1) * amlaiu + fu * lai[2]) * rwork
  amlail <- ((nmtimes - 1) * amlail + fl * lai[1]) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * monthly soil parameters *  * *
  # ---------------------------------------------------------------------
  
  soiltemp <- 0
  soilmois <- 0
  soilice <- 0
  
  vwc <- 0
  awc <- 0
  
  # averages for first 4 layers of soil (assumed to add to 1 meter depth)
  for(k in seq(1,4)) { 
    
    soiltemp <- soiltemp + tsoi[k] * hsoi[k]
    soilmois <- soilmois + wsoi[k] * hsoi[k]
    soilice <- soilice  + wisoi[k] * hsoi[k]
    
    vwc <- vwc + (wisoi[k] + (1 - wisoi[k]) * wsoi[k]) * hsoi[k] * poros[k]
    
    awc <- awc + max (0, (wisoi[k] + (1 - wisoi[k]) * wsoi[k]) - swilt[k]) *hsoi[k] * poros[k] * 100
    
  }
  
  soiltemp <- soiltemp * rdepth - 273.16
  soilmois <- soilmois * rdepth
  soilice <- soilice  * rdepth
  
  vwc <- vwc * rdepth
  awc <- awc * rdepth
  
  amtsoi <- ((nmtimes - 1) * amtsoi + soiltemp) * rwork
  amwsoi <- ((nmtimes - 1) * amwsoi + soilmois) * rwork
  amwisoi <- ((nmtimes - 1) * amwisoi + soilice) * rwork
  amvwc <- ((nmtimes - 1) * amvwc + vwc) * rwork
  amawc <- ((nmtimes - 1) * amawc + awc) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * snow parameters *  * *
  # ---------------------------------------------------------------------
  
  snodpth <- hsno[1] + hsno[2] + hsno[3]
  
  amsnod <- ((nmtimes - 1) * amsnod + snodpth) * rwork
  amsnof <- ((nmtimes - 1) * amsnof + fi) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * determine monthly npp *  * *
  # ---------------------------------------------------------------------
  
  amnpp <- ((nmtimes - 1) * amnpp + tnpp * rwork3) * rwork
  
  # amnpp[1] <- ((nmtimes - 1) * amnpp[1] + tnpp[1] * rwork3) * rwork
  # amnpp[2] <- ((nmtimes - 1) * amnpp[2] + tnpp[2] * rwork3) * rwork
  # amnpp[3] <- ((nmtimes - 1) * amnpp[3] + tnpp[3] * rwork3) * rwork
  # amnpp[4]  <- ((nmtimes - 1) * amnpp[4] + tnpp[4] * rwork3) * rwork
  # amnpp[5]  <- ((nmtimes - 1) * amnpp[5] + tnpp[5] * rwork3) * rwork
  # amnpp[6]  <- ((nmtimes - 1) * amnpp[6] + tnpp[6] * rwork3) * rwork
  # amnpp[7]  <- ((nmtimes - 1) * amnpp[7] + tnpp[7] * rwork3) * rwork
  # amnpp[8]  <- ((nmtimes - 1) * amnpp[8] + tnpp[8] * rwork3) * rwork
  # amnpp[9]  <- ((nmtimes - 1) * amnpp[9] + tnpp[9] * rwork3) * rwork
  # amnpp[10] <- ((nmtimes - 1) * amnpp[10] + tnpp[10] * rwork3) * rwork
  # amnpp[11] <- ((nmtimes - 1) * amnpp[11] + tnpp[11] * rwork3) * rwork
  # amnpp[12] <- ((nmtimes - 1) * amnpp[12] + tnpp[12] * rwork3) * rwork
  # amnpp[13] <- ((nmtimes - 1) * amnpp[13] + tnpp[13] * rwork3) * rwork
  # amnpp[14] <- ((nmtimes - 1) * amnpp[14] + tnpp[14] * rwork3) * rwork
  # amnpp[15] <- ((nmtimes - 1) * amnpp[15] + tnpp[15] * rwork3) * rwork
  # amnpp[16] <- ((nmtimes - 1) * amnpp[16] + tnpp[16] * rwork3) * rwork
  
  amnpptot <- sum(amnpp)
  
  # amnpptot <- amnpp[1] + amnpp[2] + amnpp[3] + amnpp[4] + amnpp[5] + amnpp[6] +  
  #   amnpp[7] + amnpp[8] + amnpp[9] +
  #   amnpp[10] + amnpp[11] + amnpp[12] +
  #   amnpp[13] + amnpp[14] + amnpp[15] + amnpp[16]
  
  # ---------------------------------------------------------------------
  # *  *  * monthly biogeochemistry parameters *  * *
  # ---------------------------------------------------------------------
  
  # increment monthly total co2 respiration from microbes
  # tco2mic is instantaneous value of co2 flux calculated in biogeochem.f
  amco2mic <- ((nmtimes - 1) * amco2mic + tco2mic * rwork3) * rwork
 
  # increment monthly total co2 respiration from roots
  # tco2root is instantaneous value of co2 flux calculated in stats.f
  amco2root <- ((nmtimes - 1) * amco2root + tco2root * rwork3) * rwork
  
  # calculate average total co2 respiration from soil
  amco2soi <- amco2root + amco2mic
  
  #  calculate ratio of root to total co2 respiration

  if(amco2soi > 0) {
    amco2ratio <- amco2root / amco2soi
  } else {
    amco2ratio <-  - 999.99
  }
  
  #  monthly net ecosystem co2 flux -- npp total minus microbial respiration 
  #  the npp total includes losses from root respiration
  amneetot <- amnpptot - amco2mic 
  
  # increment monthly total of net nitrogen mineralization
  # value for tnmin is calculated in biogeochem.f
  amnmintot <- ((nmtimes - 1) * amnmintot + tnmin * rwork4) * rwork
  
  assign("nmtimes", nmtimes, envir = env)
  assign("amrain", amrain, envir = env)
  assign("amsnow", amsnow, envir = env)
  assign("amaet", amaet, envir = env)
  assign("amtrans", amtrans, envir = env)
  assign("amtratio", amtratio, envir = env)
  assign("amtrunoff", amtrunoff, envir = env)
  assign("amsrunoff", amsrunoff, envir = env)
  assign("amdrainage", amdrainage, envir = env)
  assign("amtotnleach", amtotnleach, envir = env)
  assign("amno3leach", amno3leach, envir = env)
  assign("amtemp", amtemp, envir = env)
  assign("amcloud", amcloud, envir = env)
  assign("amqa", amqa, envir = env)
  assign("amrh", amrh, envir = env)
  assign("amsolar", amsolar, envir = env)
  assign("amirup", amirup, envir = env)
  assign("amirdown", amirdown, envir = env)
  assign("amsens", amsens, envir = env)
  assign("amlatent", amlatent, envir = env)
  assign("rnet", rnet, envir = env)
  assign("amlaiu", amlaiu, envir = env)
  assign("amlail", amlail, envir = env)
  assign("amtsoi", amtsoi, envir = env)
  assign("amwsoi", amwsoi, envir = env)
  assign("amwisoi", amwisoi, envir = env)
  assign("amvwc", amvwc, envir = env)
  assign("amawc", amawc, envir = env)
  assign("amsnod", amsnod, envir = env)
  assign("amsnof", amsnof, envir = env)
  assign("amnpp", amnpp, envir = env)
  assign("amnpptot", amnpptot, envir = env)
  assign("amco2mic", amco2mic, envir = env)
  assign("amco2root", amco2root, envir = env)
  assign("amco2soi", amco2soi, envir = env)
  assign("amco2ratio", amco2ratio, envir = env)
  assign("amneetot", amneetot, envir = env)
  assign("amnmintot", amnmintot, envir = env)
  
  # return to main program
  return()
}