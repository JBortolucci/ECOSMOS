# Global Vars:
# ayaet       # annual average aet (mm/yr)
# ayalit      # aboveground litter (kg-c/m**2)
# ayanlit     # aboveground litter nitrogen (kg-N/m**2)
# ayawc       # annual average 1m plant-available water content (fraction)
# ayblit      # belowground litter (kg-c/m**2)
# aybnlit     # belowground litter nitrogen (kg-N/m**2)
# aycmic      # total soil carbon in microbial biomass (kg-c/m**2)
# ayco2mic    # annual total CO2 flux from microbial respiration (kg-C/m**2/yr)
# ayco2root   # annual total CO2 flux from soil due to root respiration (kg-C/m**2/yr)
# ayco2soi    # annual total soil CO2 flux from microbial and root respiration (kg-C/m**2/yr)
# aycsoi      # total soil carbon (kg-c/m**2)
# aydrainage  # annual average drainage (mm/yr)
# aydwtot     # annual average soil+vegetation+snow water recharge (mm/yr or kg_h2o/m**2/yr)
# aygpp       # annual gross npp for each plant type(kg-c/m**2/yr)
# aygpptot    # annual total gpp for ecosystem (kg-c/m**2/yr)
# ayimmtot    # annual total gross nitrogen immobilization (kg-N/m**2/yr) 
# ayirdown    # annual average downward ir radiation (w/m**2)
# ayirup      # annual average upward ir radiation (w/m**2)
# aylatent    # annual average latent heat flux (w/m**2)
# aymintot    # annual total gross nitrogen mineralization (kg-N/m**2/yr) 
# ayneetot    # annual total NEE for ecosystem (kg-C/m**2/yr)
# aynmintot   # annual total net nitrogen mineralization (kg-N/m**2/yr)
# aynpp       # annual total npp for each plant type(kg-c/m**2/yr)
# aynpptot    # annual total npp for ecosystem (kg-c/m**2/yr)
# aynreltot   # annual total non-microbial nitrogen mineral/immobilization (kg-N/m**2/yr) 
# aynsoi      # total soil nitrogen (kg-N/m**2)
# ayprcp      # annual average precipitation (mm/yr)
# ayrootbio   # annual average live root biomass (kg-C / m**2)
# aysens      # annual average sensible heat flux (w/m**2)
# aysolar     # annual average incident solar radiation (w/m**2)
# aysrunoff   # annual average surface runoff (mm/yr)
# aystresstl  # annual average soil moisture stress parameter for lower canopy (dimensionless)
# aystresstu  # annual average soil moisture stress parameter for upper canopy (dimensionless)
# aytrans     # annual average transpiration (mm/yr)
# aytrunoff   # annual average total runoff (mm/yr)
# aytsoi      # annual average 1m soil temperature (C)
# ayvwc       # annual average 1m volumetric water content (fraction)
# aywisoi     # annual average 1m soil ice (fraction)
# aywsoi      # annual average 1m soil moisture (fraction)
# cbior       # carbon in fine root biomass pool (kg_C m-2)
# fi          # fractional snow cover
# fira        # incoming ir flux (W m-2)
# firb        # net upward ir radiation at reference atmospheric level za (W m-2)
# firefac     # factor that respresents the annual average fuel dryness of a grid cell, and hence characterizes the readiness to burn
# fl          # fraction of snow-free area covered by lower  canopy
# fsena       # downward sensible heat flux between za & z12 at za (W m-2)
# fu          # fraction of overall area covered by upper canopy
# fvapa       # downward h2o vapor flux between za & z12 at za (kg m-2 s-1)
# gdrain      # drainage rate out of bottom of lowest soil layer (kg_h2o m-2 s-1)
# grunof      # surface runoff rate (kg_h2o m-2 s-1)
# gtrans      # total transpiration rate from all vegetation canopies (kg_h2o m-2 s-1)
# hsno        # thickness of snow layers (m)
# hsoi        # soil layer thickness (m)
# hvap        # latent heat of vaporization of water (J kg-1)
# lai         # canopy single-sided leaf area index (area leaf/area veg)
# ndaypy      # number of days per year
# nytimes     # counter for yearly average calculations
# poros       # porosity (mass of h2o per unit vol at sat / rhow)
# raina       # rainfall rate (mm/s or kg m-2 s-1)
# rhos        # density of snow (kg m-3)
# rhow        # density of liquid water (all types) (kg m-3)
# sai         # current single-sided stem area index
# snowa       # snowfall rate (mm/s or kg m-2 s-1 of water)
# solad       # direct downward solar flux (W m-2)
# solai       # diffuse downward solar flux (W m-2)
# stresstl    # sum of stressl over all 6 soil layers (dimensionless)
# stresstu    # sum of stressu over all 6 soil layers (dimensionless)
# swilt       # wilting soil moisture value (fraction of pore space)
# tco2mic     # instantaneous microbial co2 flux from soil (mol-CO2 / m-2 / second)
# tco2root    # instantaneous fine co2 flux from soil (mol-CO2 / m-2 / second)
# tgpp        # instantaneous GPP for each pft (mol-CO2 / m-2 / second)
# tnmin       # instantaneous nitrogen mineralization (kg_N m-2/timestep)
# tnpp        # instantaneous NPP for each pft (mol-CO2 / m-2 / second)
# totalit     # total standing aboveground litter (kg_C m-2)
# totanlit    # total standing aboveground nitrogen in litter (kg_N m-2)
# totcmic     # total carbon residing in microbial pools (kg_C m-2)
# totcsoi     # total carbon in all soil pools (kg_C m-2)
# totimm      # total immobilized nitrogen in timestep (kg_n m-2 timestep-1) 
# totmin      # total mineralized nitrogen in timestep (kg_n m-2 timestep-1) 
# totnrel     # total mineralized/immobilized nitrogen in timestep (non-microbial) (kg_n m-2 timestep-1) 
# totnsoi     # total nitrogen in soil (kg_N m-2)
# totrlit     # total root litter carbon belowground (kg_C m-2)
# totrnlit    # total root litter nitrogen belowground (kg_N m-2)
# tsoi        # soil temperature for each layer (K)
# wipud       # ice content of puddles per soil area (kg m-2)
# wisoi       # fraction of soil pore space containing ice
# wliql       # intercepted liquid h2o on lower canopy leaf and stem area (kg m-2)
# wliqs       # intercepted liquid h2o on upper canopy stem area (kg m-2)
# wliqu       # intercepted liquid h2o on upper canopy leaf area (kg m-2)
# wpud        # liquid content of puddles per soil area (kg m-2)
# wsnol       # intercepted frozen h2o (snow) on lower canopy leaf & stem area (kg m-2)
# wsnos       # intercepted frozen h2o (snow) on upper canopy stem area (kg m-2)
# wsnou       # intercepted frozen h2o (snow) on upper canopy leaf area (kg m-2)
# wsoi        # fraction of soil pore space containing liquid water
# wtot        # total amount of water stored in snow, soil, puddles, and on vegetation (kg_h2o)

sumyear <- function (istep, iday, imonth) {
  # ---------------------------------------------------------------------
  # *  *  * update counters and working variables *  * *
  # ---------------------------------------------------------------------
  #
  # reset sumyear if the first timestep of the year
  
  if ((istep == 1) && (iday == 1) && (imonth == 1)) nytimes <- 0
  
  # accumulate yearly output
  nytimes <- nytimes + 1
  
  # working variables
  #
  # rwork4 is for nitrogen mineralization conversion
  
  rwork <- 1 / nytimes
  rwork2 <- ndaypy * 86400
  rwork3 <- ndaypy * 86400 * 12e-3
  rwork4 <- ndaypy * 86400 * 14e-3
  
  rdepth <- 1 / (hsoi[1] + hsoi[2] + hsoi[3] + hsoi[4])
  
  # ---------------------------------------------------------------------
  # *  *  * annual energy budget terms *  * *
  # ---------------------------------------------------------------------
  
  solartot <- solad[1] + solad[2] + solai[1] + solai[2]
  
  aysolar  <- ((nytimes - 1) * aysolar + solartot) * rwork
  ayirup   <- ((nytimes - 1) * ayirup + firb) * rwork
  ayirdown <- ((nytimes - 1) * ayirdown + fira) * rwork
  aysens   <- ((nytimes - 1) * aysens- fsena) * rwork
  aylatent <- ((nytimes - 1) * aylatent - fvapa * hvap) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * annual water budget terms *  * *
  # ---------------------------------------------------------------------
  
  ayprcp <- ((nytimes - 1) * ayprcp + (raina + snowa) * rwork2) * rwork
  ayaet <- ((nytimes - 1) * ayaet - fvapa * rwork2) * rwork
  aytrans <- ((nytimes - 1) * aytrans + gtrans * rwork2) * rwork
  aytrunoff <- ((nytimes - 1) * aytrunoff + (grunof + gdrain) * rwork2) * rwork
  aysrunoff <- ((nytimes - 1) * aysrunoff + grunof * rwork2) * rwork
  aydrainage <- ((nytimes - 1) * aydrainage + gdrain * rwork2) * rwork
  
  #---------------------------------------------------------------------
  # CD
  # estimate the change in soil - vegetation water content. Used to check 
  # mass conservation
  #---------------------------------------------------------------------
  
  wtotp <- wtot
  
  wtot <- (wliqu + wsnou) * fu * 2 * lai[2] + (wliqs + wsnos) * fu * 2 * sai[2] + (wliql + wsnol) * fl * 2 *
    (lai[1] + sai[1]) * (1 - fi)
  
  wtot <- wtot + wpud + wipud
  
  for(k in seq(1,nsoilay)) { 
    wtot <- wtot + poros[k] * wsoi[k] * (1 - wisoi[k]) * hsoi[k] * rhow + poros[k] * wisoi[k] * hsoi[k] * rhow
  }
  
  for(k in seq(1,nsoilay)) { 
    wtot <- wtot + fi * rhos * hsno[k]
  }
  
  aydwtot <- ((nytimes - 1) * aydwtot + wtot - wtotp) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * annual soil parameters *  * *
  # ---------------------------------------------------------------------
  
  soiltemp <- 0
  soilmois <- 0
  soilice <- 0
  
  vwc <- 0
  awc <- 0
  
  # averages for first 4 layers of soil
  for(k in seq(1,4)) { 
    
    soiltemp <- soiltemp + tsoi[k] * hsoi[k]
    soilmois <- soilmois + wsoi[k] * hsoi[k]
    soilice <- soilice  + wisoi[k] * hsoi[k]
    
    vwc <- vwc + (wisoi[k] + (1 - wisoi[k]) * wsoi[k]) * hsoi[k] * poros[k]
    
    awc <- awc + max (0, (wisoi[k] +(1 - wisoi[k]) * wsoi[k]) - swilt[k]) * hsoi[k] * poros[k] * 100
    
  }
  
  # average soil and air temperatures
  soiltemp <- soiltemp * rdepth - 273.16
  soilmois <- soilmois * rdepth
  soilice <- soilice  * rdepth
  
  vwc <- vwc * rdepth
  awc <- awc * rdepth
  
  # annual average soil moisture and soil ice
  aywsoi <- ((nytimes - 1) * aywsoi + soilmois) * rwork
  aywisoi <- ((nytimes - 1) * aywisoi + soilice) * rwork
  aytsoi <- ((nytimes - 1) * aytsoi + soiltemp) * rwork
  ayvwc <- ((nytimes - 1) * ayvwc + vwc) * rwork
  ayawc <- ((nytimes - 1) * ayawc + awc) * rwork
  
  # soil moisture stress
  aystresstu <- rwork * ((nytimes - 1) * aystresstu + stresstu)
  
  aystresstl <- rwork * ((nytimes - 1) * aystresstl + stresstl)
  
  # ---------------------------------------------------------------------
  # *  *  * determine annual gpp *  * *
  # ---------------------------------------------------------------------
  #
  # gross primary production of each plant type
  
  aygpp <- ((nytimes - 1) * aygpp  + tgpp * rwork3) * rwork
  

  # gross primary production of the entire gridcell
  aygpptot <- sum(aygpp)
  

  
  # ---------------------------------------------------------------------
  # *  *  * determine annual npp *  * *
  # ---------------------------------------------------------------------
  
  # net primary production of each plant type
  aynpp <- ((nytimes - 1) * aynpp + tnpp * rwork3) * rwork
  

  # net primary production of the entire gridcell
  aynpptot <- sum(aynpp)
  
 
  
  # ---------------------------------------------------------------------
  # *  *  * annual carbon budget terms *  * *
  # ---------------------------------------------------------------------
  #
  # fire factor used in vegetation dynamics calculations
  water <- wisoi[1] + (1 - wisoi[1]) * wsoi[1]
  waterfrac <- (water - swilt[1]) / (1 - swilt[1])
  
  fueldry <- max (0, min (1, - 2 * (waterfrac - 0.5)))
  
  firefac <- ((nytimes - 1) * firefac + fueldry) * rwork
  
  # increment annual total co2 respiration from microbes
  # tco2mic is instantaneous value of co2 flux calculated in biogeochem.f
  ayco2mic <- ((nytimes - 1) * ayco2mic + tco2mic * rwork3) * rwork
  
  # increment annual total co2 respiration from roots
  ayco2root <- ((nytimes - 1) * ayco2root + tco2root * rwork3) * rwork
  
  # calculate annual total co2 respiration from soil
  ayco2soi <- ayco2root + ayco2mic
  
  # annual net ecosystem co2 flux -- npp total minus microbial respiration 
  # the npp total includes losses from root respiration
  
  ayneetot <- aynpptot - ayco2mic
  
  # annual average root biomass
  allroots <- sum(cbior)
  
  ayrootbio <- ((nytimes - 1) * ayrootbio + allroots) * rwork
  
  # ---------------------------------------------------------------------
  # *  *  * annual biogeochemistry terms *  * *
  # ---------------------------------------------------------------------
  #
  # increment annual total of net nitrogen mineralization
  # value for tnmin is calculated in biogeochem.f
  
  aynmintot <- ((nytimes - 1) * aynmintot + tnmin * rwork4) * rwork
  
  # mineralization (gross)
  aymintot <- ((nytimes - 1) * aymintot + totmin * rwork4) * rwork
  
  # immobilization (gross)
  ayimmtot <- ((nytimes - 1) * ayimmtot + totimm * rwork4) * rwork 
  
  # other mineralization / immobilization
  # from non - microbial transformations
  aynreltot <- ((nytimes - 1) * aynreltot + totnrel * rwork4) * rwork 
  
  # other biogeochemistry variables
  ayalit <- ((nytimes - 1) * ayalit + totalit) * rwork
  ayblit <- ((nytimes - 1) * ayblit + totrlit) * rwork
  aycsoi <- ((nytimes - 1) * aycsoi + totcsoi) * rwork
  aycmic <- ((nytimes - 1) * aycmic + totcmic) * rwork
  ayanlit <- ((nytimes - 1) * ayanlit + totanlit) * rwork
  aybnlit <- ((nytimes - 1) * aybnlit + totrnlit) * rwork
  aynsoi <- ((nytimes - 1) * aynsoi + totnsoi) * rwork
  
  assign("nytimes", nytimes, envir = env)
  assign("aysolar", aysolar, envir = env)
  assign("ayirup", ayirup, envir = env)
  assign("ayirdown", ayirdown, envir = env)
  assign("aysens", aysens, envir = env)
  assign("aylatent", aylatent, envir = env)
  assign("ayprcp", ayprcp, envir = env)
  assign("ayaet", ayaet, envir = env)
  assign("aytrans", aytrans, envir = env)
  assign("aytrunoff", aytrunoff, envir = env)
  assign("aysrunoff", aysrunoff, envir = env)
  assign("aydrainage", aydrainage, envir = env)
  assign("wtot", wtot, envir = env)
  assign("aydwtot", aydwtot, envir = env)
  assign("aywsoi", aywsoi, envir = env)
  assign("aywisoi", aywisoi, envir = env)
  assign("aytsoi", aytsoi, envir = env)
  assign("ayvwc", ayvwc, envir = env)
  assign("ayawc", ayawc, envir = env)
  assign("aystresstu", aystresstu, envir = env)
  assign("aystresstl", aystresstl, envir = env)
  assign("aygpp", aygpp, envir = env)
  assign("aygpptot", aygpptot, envir = env)
  assign("aynpp", aynpp, envir = env)
  assign("aynpptot", aynpptot, envir = env)
  assign("firefac", firefac, envir = env)
  assign("ayco2mic", ayco2mic, envir = env)
  assign("ayco2root", ayco2root, envir = env)
  assign("ayco2soi", ayco2soi, envir = env)
  assign("ayneetot", ayneetot, envir = env)
  assign("ayrootbio", ayrootbio, envir = env)
  assign("aynmintot", aynmintot, envir = env)
  assign("aymintot", aymintot, envir = env)
  assign("ayimmtot", ayimmtot, envir = env)
  assign("aynreltot", aynreltot, envir = env)
  assign("ayalit", ayalit, envir = env)
  assign("ayblit", ayblit, envir = env)
  assign("aycsoi", aycsoi, envir = env)
  assign("aycmic", aycmic, envir = env)
  assign("ayanlit", ayanlit, envir = env)
  assign("aybnlit", aybnlit, envir = env)
  assign("aynsoi", aynsoi, envir = env)
  
  return()
}