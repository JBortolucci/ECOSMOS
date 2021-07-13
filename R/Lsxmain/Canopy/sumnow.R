# Global Vars:
# a10td      # 10-day average daily air temperature (K)
# a10tmin    # 10-day average minimum air temperature (K)
# a10ts      # 10-day average daily soil temperature in top layer (K)
# a11soiltd  # 11-day average daily soil temperature (K)
# a3tdmin    # 3-day running average minimum air temperature (deg C)
# a5td       # 5-day average daily air temperature (K)
# a5tmin     # 5-day average minimum air temperature (K)
# agcc3      # canopy average gross photosynthesis rate - c3 crops (mol_co2 m-2 s-1)
# agcc4      # canopy average gross photosynthesis rate - c4 crops (mol_co2 m-2 s-1)
# agcl3      # canopy average gross photosynthesis rate - c3 grasses (mol_co2 m-2 s-1)
# agcl4      # canopy average gross photosynthesis rate - c4 grasses (mol_co2 m-2 s-1)
# agcls      # canopy average gross photosynthesis rate - shrubs     (mol_co2 m-2 s-1)
# agcub      # canopy average gross photosynthesis rate - broadleaf  (mol_co2 m-2 s-1)
# agcuc      # canopy average gross photosynthesis rate - conifer    (mol_co2 m-2 s-1)
# ancc3      # canopy average net photosynthesis rate - c3 crops (mol_co2 m-2 s-1)
# ancc4      # canopy average net photosynthesis rate - c4 crops (mol_co2 m-2 s-1)
# ancl3      # canopy average net photosynthesis rate - c3 grasses   (mol_co2 m-2 s-1)
# ancl4      # canopy average net photosynthesis rate - c4 grasses   (mol_co2 m-2 s-1)
# ancls      # canopy average net photosynthesis rate - shrubs       (mol_co2 m-2 s-1)
# ancub      # canopy average net photosynthesis rate - broadleaf    (mol_co2 m-2 s-1)
# ancuc      # canopy average net photosynthesis rate - conifer      (mol_co2 m-2 s-1)
# cbior      # carbon in fine root biomass pool (kg_C m-2)
# cbiow      # carbon in woody biomass pool (kg_C m-2)
# dtime      # model timestep (seconds)
# fi         # fractional snow cover
# fl         # fraction of snow-free area covered by lower  canopy
# frac       # fraction of canopy occupied by each plant functional type
# froot      # fraction of root in soil layer 
# fu         # fraction of overall area covered by upper canopy
# lai        # canopy single-sided leaf area index (area leaf/area veg)
# ndaypy     # number of days per year
# nppdummy   # dummy NPP used in stats.f
# rootr      # root respiration
# sapfrac    # fraction of woody biomass that is in sapwood
# tco2mic    # instantaneous microbial co2 flux from soil (mol-CO2 / m-2 / second)
# tco2root   # instantaneous fine co2 flux from soil (mol-CO2 / m-2 / second)
# td         # daily average temperature (K)
# tgpp       # instantaneous GPP for each pft (mol-CO2 / m-2 / second)
# tgpptot    # instantaneous gpp (mol-CO2 / m-2 / second)
# tmin       # minimum daily temperature (K)
# tneetot    # instantaneous net ecosystem exchange of co2 per timestep (kg_C m-2/timestep) # check units [Henrique, 2021-02-12]
# tnpp       # instantaneous NPP for each pft (mol-CO2 / m-2 / second)
# tnpptot    # instantaneous npp (mol-CO2 / m-2 / second)
# ts         # temperature of upper canopy stems (K)
# tsoi       # soil temperature for each layer (K)
# tsoiavg    # average of top two soil layers soil temperature (K)

sumnow <- function() {
  

  
  # ---------------------------------------------------------------------
  # *  *  * define working variables *  *  *
  # ---------------------------------------------------------------------

  # maintenance respiration coefficients (per second)
  #
  # initially, we pick values for respiration coefficients that
  # defined in units of / year
  #
  #   rwood ~ 0.0125
  #   rroot ~ 1.2500
  #
  # however, we convert the unitsconvert to have resulting respiration
  # fluxes in units of mol - C  / m**2 / second
  #
  # this requires we convert the time unit to seconds and add an additional
  # factor to convert biomass units from kilograms to moles
  # rwood <- 0.0125 / (ndaypy * 86400) * (1000 / 12)
  # rroot <- 2.500 / (ndaypy * 86400) * (1000 / 12)
  # rroot <- 1.250 / (ndaypy * 86400) * (1000 / 12)
  # growth respiration coefficient (fraction)
  #
  #     rgrowthc <- 0.25  ! crop growth respiration from Amthor, 1984
  # rgrowthc <- 0.242  # crop growth respiration for sugarcane from Singles,2002
  # rgrowthc <- 0.17
  rgrowth <- 0.30 # To do: Jair ou Leandro enviar para planilha plantparams
  #TODO: rgrowth sem efeito nas culturas (crops); rgrowthc no plantparams é oq manda, certo? [Henrique, 2020-01-12]

  # 10 - day time averaging factor
  zweight <- exp( - 1 / (10 * 86400 / dtime))

  # 11 - day time averaging factor
  zweight11 <- exp( - 1 / (11 * 86400 / dtime))

  # 3 - day time averaging factor
  zweight3 <- exp( - 1 / (3 * 86400 / dtime))

  # 5 - day time averaging factor
  zweight5 <- exp( - 1 / (5 * 86400 / dtime))

  
  # calculate instantaneous carbon flux parameters, including
  # npp (net primary production) and nee (net ecosystem exchange)
  #
  # in this routine, all of the fluxes are calculated in the units
  # of mol - C  / m**2 / sec
  #
  # ---------------------------------------------------------------------
  # *  *  * calculate instantaneous GPP *  * *
  # ---------------------------------------------------------------------
  #
  # snow masking for lower canopy vegetation
  smask <- 1 - fi
  
  # note that the following plants types follow different physiological paths
  #
  # - broadleaf trees   :  types 1, 2, 3, 5, 7, 8
  # - conifer   trees   :  types 4, 6
  # - shrubs            :  types 9, 10
  # - c4 grasses        :  type 11
  # - c3 grasses        :  type 12
  # - c3 crops          :  soybean, wheat (type 13, 15)
  # - c4 crops          :  maize and sugarcane (type 14, 16)
  #
  # note that plant type 8 is actually a deciduous conifer (e.g., Larix), but
  # we are assuming that it's physiological behavior is like a broadleaf tree
  #
  # nppdummy is canopy npp before accounting for stem & root respirtation
  # Navin Sept 02
  
  # PFT_UPDATE: upper canopy
  for(i in seq(1,npft)) {
    nppdummy[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$canopy == UPPER,  frac[i] * an[i] * lai[2] * fu, nppdummy[i])
  }
 
  # PFT_UPDATE: lower canopy
  for(i in seq(1,npft)) {
    nppdummy[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$canopy == LOWER,  frac[i] * an[i] * lai[1] * fl * smask, nppdummy[i])
  }

  # PFT_UPDATE: upper canopy
  for(i in seq(1,npft)) {
    tgpp[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$canopy == UPPER,  frac[i] * ag[i] * lai[2] * fu, tgpp[i])
  }
  
  # PFT_UPDATE: lower canopy
  for(i in seq(1,npft)) {
    tgpp[i] <- ifelse(plantList[[i]]$active && plantList[[i]]$canopy == LOWER,   frac[i] * ag[i] * lai[1] * fl * smask, tgpp[i])
  }

  
  # PFT_UPDATE
  # calculate total gridcell gpp
  tgpptot <- sum(tgpp)
  
  
  # sapfrac <- Sapwood / (Sapwood + Heartwood)
  
  # ---------------------------------------------------------------------
  # *  *  * calculate temperature functions for respiration *  *  *
  # ---------------------------------------------------------------------
  #
  # calculate the stem temperature
  # stemtemp <- ts
  # 
  # # calculate average root temperature (average of all roots)
  # roottemp <- sum( tsoi * 0.5 * (froot[,1] + froot[,2]) )
  # 
  # 
  # # calculate respiration terms on a 15 degree base
  # # following respiration parameterization of Lloyd and Taylor
  # funca <- exp(3500 * (1 / 288.16 - 1 / stemtemp))
  # funcb <- exp(3500 * (1 / 288.16 - 1 / roottemp))
  #sant - these functions can be set to 0 to compare productivity
  #sant - (have to adjust these functios, they have a great impact in biomass production)
  
  # ---------------------------------------------------------------------
  # *  *  * calculate instantaneous NPP *  *  *
  # ---------------------------------------------------------------------
  #
  # the basic equation for npp is
  #
  #   npp <- (1 - growth respiration term) * (gpp - maintenance respiration terms)
  #
  # here the respiration terms are simulated as
  #
  #   growth respiration <- rgrowth * (gpp - maintenance respiration terms)
  #
  # where
  #
  #   rgrowth is the construction cost of new tissues
  #
  # and
  #
  #   root respiration <- rroot * cbior[i,k] * funcb
  #   wood respiration <- rwood * cbiow[i,k] * funca * sapwood fraction
  #
  # where
  #
  #   funca <- temperature function for aboveground biomass[stems]
  #   funcb <- temperature function for belowground biomass[roots]
  #
  # note that we assume the sapwood fraction for shrubs is 1
  #
  # also note that we apply growth respiration, (1 - rgrowth),
  # throughout the year; this may cause problems when comparing
  # these npp values with flux tower measurements
  #
  # also note that we need to convert the mass units of wood and
  # root biomass from kilograms of carbon to moles of carbon
  # to maintain consistent units (done in rwood, rroot)
  #
  # finally, note that growth respiration is only applied to
  # positive carbon gains (i.e., when gpp - rmaint is positive)
  
  rwood <- 0.0125 / (ndaypy * 86400) * (1000 / 12)
  rroot <- 2.50 / (ndaypy * 86400) * (1000 / 12)

  stemtemp <- ts

  roottemp <- sum( tsoi * 0.5 * (froot[,1] + froot[,2]) )

  funca <- exp(3500 * (1 / 288.16 - 1 / stemtemp))
  funcb <- exp(3500 * (1 / 288.16 - 1 / roottemp))
  
  for(i in seq(1,npft)) {
    if(!plantList[[i]]$active) next

    mcbior[i]  <- cbior[i]  * (funcb*rroot) #TODO checar [Henrique; 2021-02-11]
    mcbiow[i]  <- cbiow[i]  * (sapfrac*rwood*funca)
    mcbiob[i]  <- cbiob[i]  * (rwood*funca)
    mcbios[i]  <- cbios[i]  * (rwood*funca) 
#SVC: Bellow Maintenance respiration are applied in each crop model    
    mcbiog[i]  <- cbiog[i]  *  0.0
    mcbiop[i]  <- cbiop[i]  *  0.0 

  }
  # JAIR: Já alterei as variáveis apenas no modelo eucalipto. Nas outras partes do código que elas aparecem foi mantido,
  #       caso de algum problema é preciso verificar se nesses outros locais também é necessário mudar.
  #       As cbiog, cbios e arepr estava resetando na função "ResetCropsAfterHarvest". astem não.
  
  # OK: trocar cbiog por cbiocr (modelo eucalipto)
  # OK: trocar cbios por cbiob (modelo eucalipto)
  # OK: trocar arepr por acroot (modelo eucalipto)
  # OK: trocar astem por abranch (modelo eucalipto)
  
  # cbiocr -> coarse root
  # cbiob  -> branch 
  
  # NOVA versão
  for(i in seq(1,npft)) {
    # if(!plantList[[i]]$active) next
    tan[i] <- ifelse(plantList[[i]]$active, nppdummy[i] , tan[i]) #Aqui NAO e' necessario ifelse
    
    tnpp[i] <- ifelse(plantList[[i]]$active,                        #Aqui E' necessario ifelse, pois se tnpp = 0, segue 0 e não um valor negativo
                      nppdummy[i] - mcbior[i] - mcbiog[i] - mcbiop[i] - mcbiow[i] - mcbios[i] - mcbiocr[i] - mcbiob[i], tnpp[i])
    
    tan[i] <- max(tan[i],0.0)  
    tnpp[i] <- max(tnpp[i],0.0)  
  }

  
  # apply growth respiration and calculate total gridcell npp
  tnpptot <- 0
  
  # TODO (verificar): Tirei de dentro do loop, senão iria zerar pra cada planta (é somatória de todas as plantas?)
  fracgrowresp <- 0 
  for(k in seq(1,npft)) {
    
    if(!plantList[[k]]$active) next
    
    if(is.nan(tnpp[k])) tnpp[k] <- 0
    
    if(tnpp[k] > 0 && plantList[[k]]$type == NATURAL_VEG) {
      tnpp[k] <- tnpp[k] * (1 - rgrowth)
    }
    
    # TODO: Criar variável (criar acroot e abranch)
    
    if(tnpp[k] > 0 && plantList[[k]]$type == CROPS) {
      # fracgrowresp <- tnpp[k] * rgrowthc[k] * (aroot[k] + acroot[k])
      tnpp[k]      <- tnpp[k] * (1 - rgrowthc[k])
    }
    
    tnpptot <- tnpptot + tnpp[k]
  }
  
  # ---------------------------------------------------------------------
  # *  *  * calculate total fine root respiration *  *  *
  # ---------------------------------------------------------------------
  # tco2root <- sum(rroot * cbior * funcb)
  for(k in seq(1,npft)) {
    # if(!plantList[[k]]$active) next
    # if(plantList[[k]]$type == CROPS) {
    #     tco2root <- sum(mcbior[k] +  mcbiocr[k] + fracgrowresp)
    # }
    # 
    # tco2root <- ifelse(plantList[[k]]$active && plantList[[k]]$type == CROPS, sum(mcbior[k] +  mcbiocr[k] + fracgrowresp), tco2root)
    tco2root <- ifelse(plantList[[k]]$active && plantList[[k]]$type == CROPS, sum(mcbior[k] +  mcbiocr[k]), tco2root)
  }
  # ---------------------------------------------------------------------
  # *  *  * calculate instantaneous NEE *  * *
  # ---------------------------------------------------------------------
  #
  # microbial respiration is calculated in biogeochem.f
  tneetot <- tnpptot - tco2mic
  
  # ---------------------------------------------------------------------
  # *  *  * update 10 - day running - mean parameters *  *  *
  # ---------------------------------------------------------------------
  #
  # 10 - day daily air temperature
  
  a10td <- zweight * a10td + (1 - zweight) * td
  a10ts <- zweight * a10ts + (1 - zweight) * tsoi[1]
  a5td <- zweight5 * a5td + (1 - zweight5) * td
  
  # 3 - day daily minimum air temperature
  a3tdmin <- zweight3 * a3tdmin + (1 - zweight3) * tmin
  
  # 11 - day running mean daily soil temperature
  # Found after an estimate of soil temperature at 10cm from top two
  # IBIS soil layers (0 - 10 and 10 - 25cm) is determined
  tsoiavg <- 0.6 * tsoi[1] + 0.4 * tsoi[2]
  a11soiltd <- zweight11 * a11soiltd + (1 - zweight11) * tsoiavg
  

  # 10 - day minimimum daily temperature average -- used to help determine
  # planting dates of crops.  If both the daily average temperature
  # (10 - day mean) > 8 C and the 10 - day minimum temperature average
  # is > 0 C, then soybean and maize crops are allowed to be planted.
  a5tmin <- zweight5 * a5tmin + (1 - zweight5) * tmin
  a10tmin <- zweight * a10tmin + (1 - zweight) * tmin
  
  assign("fracgrowresp", fracgrowresp, envir = env)
  
  assign("mcbior", mcbior, envir = env)
  assign("mcbiog", mcbiog, envir = env)
  assign("mcbiop", mcbiop, envir = env)
  assign("mcbiow", mcbiow, envir = env)
  assign("mcbios", mcbios, envir = env)
  assign("mcbiocr", mcbiocr, envir = env)
  assign("mcbiob", mcbiob, envir = env)
  
  assign("nppdummy", nppdummy, envir = env)
  assign("tgpp", tgpp, envir = env)
  assign("tgpptot", tgpptot, envir = env)
  assign("tnpp", tnpp, envir = env)
  assign("tan", tan, envir = env)
  assign("rootr", rootr, envir = env)
  assign("tnpptot", tnpptot, envir = env)
  assign("tco2root", tco2root, envir = env)
  assign("tneetot", tneetot, envir = env)
  assign("a10td", a10td, envir = env)
  assign("a10ts", a10ts, envir = env)
  assign("a5td", a5td, envir = env)
  assign("a3tdmin", a3tdmin, envir = env)
  assign("tsoiavg", tsoiavg, envir = env)
  assign("a11soiltd", a11soiltd, envir = env)

  assign("a5tmin", a5tmin, envir = env)
  assign("a10tmin", a10tmin, envir = env)
  
  assign("smask", smask, envir = env)
  
  # return to main program
  return()
}
