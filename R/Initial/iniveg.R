# Global Vars:
# a10ancl3       # 10-day average canopy photosynthesis rate - c3 grasses (mol_co2 m-2 s-1)
# a10ancl4       # 10-day average canopy photosynthesis rate - c4 grasses (mol_co2 m-2 s-1)
# a10ancls       # 10-day average canopy photosynthesis rate - shrubs (mol_co2 m-2 s-1)
# a10ancub       # 10-day average canopy photosynthesis rate - broadleaf (mol_co2 m-2 s-1)
# a10ancuc       # 10-day average canopy photosynthesis rate - conifer (mol_co2 m-2 s-1)
# a10daylightl   # 10-day average day-time PAR - lower canopy (micro-Ein m-2 s-1)
# a10daylightu   # 10-day average day-time PAR - upper canopy (micro-Ein m-2 s-1)
# a10scalparaml  # 10-day average day-time scaling parameter - lower canopy (dimensionless)
# a10scalparamu  # 10-day average day-time scaling parameter - upper canopy (dimensionless)
# a10td          # 10-day average daily air temperature (K)
# a11soiltd      # 11-day average daily soil temperature (K)
# a3tdmin        # 3-day running average minimum air temperature (deg C)
# ancl3          # canopy average net photosynthesis rate - c3 grasses   (mol_co2 m-2 s-1)
# ancl4          # canopy average net photosynthesis rate - c4 grasses   (mol_co2 m-2 s-1)
# ancls          # canopy average net photosynthesis rate - shrubs       (mol_co2 m-2 s-1)
# ancub          # canopy average net photosynthesis rate - broadleaf    (mol_co2 m-2 s-1)
# ancuc          # canopy average net photosynthesis rate - conifer      (mol_co2 m-2 s-1)
# beta1          # parameter for Jackson rooting profile (lower canopy)
# beta2          # parameter for Jackson rooting profile (upper canopy)
# biomass        # total biomass of each plant functional type  (kg_C m-2)
# cbiog          # carbon in grain biomass pool (kg_C m-2)
# cbiol          # carbon in leaf biomass pool (kg_C m-2)
# cbior          # carbon in fine root biomass pool (kg_C m-2)
# cbios          # carbon in stem biomass pool (kg_C m-2)
# cbiow          # carbon in woody biomass pool (kg_C m-2)
# cnroot         # cn ratio of plant roots 
# cntops         # cn ratio of plant residue
# croplive       # 0 crops have been planted and living : 1 crops not living  
# daylength      # length of day (minutes)
# exist          # probability of existence of each plant functional type in a gridcell
# falll          # annual leaf litter fall                      (kg_C m-2/year)
# fallr          # annual root litter input                     (kg_C m-2/year)
# fallw          # annual wood litter fall                      (kg_C m-2/year)
# fl             # fraction of snow-free area covered by lower  canopy
# froot          # fraction of root in soil layer 
# fu             # fraction of overall area covered by upper canopy
# harvdate       # day of year that crop pft was harvested
# hsoi           # soil layer thickness (m)
# icropsum       # index - number of crop types planted in each grid cell
# iwheat         # 0: wheat not planted 1: spring wheat planted 2: winter wheat (ibis.infile)
# lai            # canopy single-sided leaf area index (area leaf/area veg)
# orieh          # fraction of leaf/stems with horizontal orientation
# oriev          # fraction of leaf/stems with vertical
# plai           # total leaf area index of each plant functional type
# plai_init      # initial total LAI for each vegtype (used in iniveg)
# plailower      # Potental LAI of lower canopy (uniform initial vegetation)
# plaiupper      # Potental LAI of upper canopy (uniform initial vegetation)
# precipsum      # precipitation summation for lower canopy onset (cm)
# sai            # current single-sided stem area index
# sapfrac        # fraction of woody biomass that is in sapwood
# sapfrac_init   # Initial value of sapwood fraction used for all woody PFTs
# specla         # specific leaf area (m**2/kg)
# stresstl       # sum of stressl over all 6 soil layers (dimensionless)
# stresstu       # sum of stressu over all 6 soil layers (dimensionless)
# stsuml         # soil temperature summation for lower canopy (deg C)
# stsumu         # soil temperature summation for upper canopy (deg C)
# td             # daily average temperature (K)
# tnplant        # total nitrogen in plant dry matter
# totbiol        # total biomass in the lower canopy (kg_C m-2)
# totbiou        # total biomass in the upper canopy (kg_C m-2)
# totlail        # total leaf area index for the lower canopy
# totlaiu        # total leaf area index for the upper canopy
# tw             # warmest monthly temperature (C)
# vegtype0       # annual vegetation type - ibis classification
# woodnorm       # value of woody biomass for upper canopy closure (ie when wood = woodnorm fu = 1.0) (kg_C m-2)
# xinveg         # fixed vegetation map
# xminlai        # Minimum LAI for each existing PFT
# za             # height above the surface of atmospheric forcing (m)
# zbot           # height of lowest branches above ground (m)
# ztop           # height of plant top above ground (m)
# ztopmxsgc      # height maximum (m) for sugarcane

iniveg <- function (isimveg) {
  
  depth <- array(0, nsoilay)
  
  cntops[] <- 40
  cnroot[] <- 60
  tnplant[] <- 0
  harvdate[] <- 999
  
  
  # initialize the moisture stress factors
  stresstu <- 1
  stresstl <- 1
  
  # initialize running - mean air temperature
  a10td <- 273.16
  a11soiltd <- 273.16
  daylength <- 0
  a3tdmin <- 0
  precipsum <- 0
  stsumu <- 0
  stsuml <- 0
  
  
  # initialize running - mean values of canopy photosynthesis rates
  a10ancub <- 10e-06
  a10ancuc <- 10e-06
  a10ancls <- 10e-06
  a10ancl4 <- 10e-06
  a10ancl3 <- 10e-06
  
  # PFT_UPDATE
  a10anc[] <- 10e-06
  assign("a10anc", a10anc, envir = env)
  
  
  # initialize running - mean values of the scaling parameter
  a10scalparamu <- 0.5 * 5
  a10scalparaml <- 0.5 * 5
  a10daylightu <- 5
  a10daylightl <- 5
  
  # initialize litter fall
  falll <- 0
  fallr <- 0
  fallw <- 0
  
  # reset counters
  ievgr <- 0
  ideci <- 0
  ishrub <- 0
  igrass <- 0
  icrop <- 0
  ilower <- 0
  iupper <- 0
  
  # determine number of evergreen plant functional types
  # if (round(exist[1]) == 1) ievgr <- ievgr + 1
  # if (round(exist[3]) == 1) ievgr <- ievgr + 1
  # if (round(exist[4]) == 1) ievgr <- ievgr + 1
  # if (round(exist[6]) == 1) ievgr <- ievgr + 1
  # 
  # # determine number of deciduous plant functional types
  # if (round(exist[2]) == 1) ideci <- ideci + 1
  # if (round(exist[5]) == 1) ideci <- ideci + 1
  # if (round(exist[7]) == 1) ideci <- ideci + 1
  # if (round(exist[8]) == 1) ideci <- ideci + 1
  # 
  # # make sure counter is at least 1 (to avoid division by zero)
  # ievgr <- max (1, ievgr)
  # ideci <- max (1, ideci)
  # 
  # # determine number of shrub functional types
  # if (round(exist[9]) == 1)  ishrub <- ishrub + 1
  # if (round(exist[10]) == 1) ishrub <- ishrub + 1
  # 
  # # determine number of herbaceous plant functional types
  # if (round(exist[11]) == 1) igrass <- igrass + 1
  # if (round(exist[12]) == 1) igrass <- igrass + 1
  
  # make sure counter is at least 1 (to avoid division by zero)
  # ishrub <- max (1, ishrub)
  # igrass <- max (1, igrass)
  
  # total number of possible pfts for each canopy
  iupper <- ievgr + ideci
  ilower <- ishrub + igrass 
  
  # make sure counter is at least 1 (to avoid division by zero)
  
  # PFT_UPDATE: verificar croplive.
  
  # determine number of crop functional types 
  for(i in 1:npft) {
    if(round(exist[i]==1) && plantList[[i]]$type == CROPS) {
      icrop       <- icrop + 1    
      croplive[i] <- 0 
    } 
  }
  # for(jj in scpft:ecpft) { 
  #   if(round(exist[jj]) == 1) {
  #     icrop <- icrop + 1    
  #     croplive[jj] <- 0 
  #   }
  # }
  
  # make sure counter is at least 1 (to avoid division by zero)
  icrop  <- max (1, icrop)
  ilower <- ilower + icrop
  
  iupper <- max (1, iupper)
  ilower <- max (1, ilower)
  
  # ************************************************************************
  # case (0) assign vegetation characteristics for static vegetation
  # ************************************************************************
  #
  # and
  #
  # ************************************************************************
  # case (1) assign vegetation characteristics for dynamic vegtation
  #          that is initialized with fixed vegetation map
  # ************************************************************************
  
  if(isimveg == 0 || isimveg == 1) {
    # translate vegetation type (real) to nearest integer
    inveg <- round (xinveg)
    
    # for initialization purposes, set the predicted vegetation type
    # to the initial vegetation type
    vegtype0 <- xinveg
    
    # ---------------------------------------------------
    #  1: tropical evergreen forest / woodland
    #  2: tropical deciduous forest / woodland
    #  3: temperate evergreen broadleaf forest / woodland
    #  4: temperate evergreen conifer forest / woodland
    #  5: temperate deciduous forest / woodland
    #  6: boreal evergreen forest / woodland
    #  7: boreal deciduous forest / woodland
    #  8: mixed forest / woodland
    #  9: savanna
    # 10: grassland / steppe
    # 11: dense shrubland
    # 12: open shrubland
    # 13: tundra
    # 14: desert
    # 15: polar desert / rock / ice
    # 16: croplands
    # ---------------------------------------------------
    #
    # these classes consist of some combination of 
    # plant functional types:
    #
    # ---------------------------------------------------
    #  1: tropical broadleaf evergreen trees
    #  2: tropical broadleaf drought - deciduous trees
    #  3: warm - temperate broadleaf evergreen trees
    #  4: temperate conifer evergreen trees
    #  5: temperate broadleaf cold - deciduous trees
    #  6: boreal conifer evergreen trees
    #  7: boreal broadleaf cold - deciduous trees
    #  8: boreal conifer cold - deciduous trees
    #  9: evergreen shrubs
    # 10: cold - deciduous shrubs
    # 11: warm (c4) grasses
    # 12: cool (c3) grasses
    # 13: soybean
    # 14: maize 
    # 15: winter and spring wheat 
    # ---------------------------------------------------
    #
    # initially all values are set to zero
    #
    #*** DTP 2001 / 05 / 25 The following code replaces the 450+
    #    lines of stuff that follows it (hence the temporary goto
    #    statement). Note that values of plai_init are read in as
    #    parameters from params.veg. Note also that the declarations
    #    of the four local variables plaievgr, plaideci, plaishrub 
    #    and plaigrass can all be dropped.
    
    # PFT_UPDATE (Santiago): Verificar depois
    
    # plai[1] <- exist[1] / (ievgr) * plai_init[1,inveg]
    # plai[2] <- exist[2] / (ideci) * plai_init[2,inveg]
    # plai[3] <- exist[3] / (ievgr) * plai_init[1,inveg]
    # plai[4] <- exist[4] / (ievgr) * plai_init[1,inveg]
    # plai[5] <- exist[5] / (ideci) * plai_init[2,inveg]
    # plai[6] <- exist[6] / (ievgr) * plai_init[1,inveg]
    # plai[7] <- exist[7] / (ideci) * plai_init[2,inveg]
    # plai[8] <- exist[8] / (ideci) * plai_init[2,inveg]
    # plai[9] <- exist[9] / (ishrub) * plai_init[3,inveg]
    # plai[10] <- exist[10] / (ishrub) *plai_init[3,inveg]
    # 
    # if((inveg == 9) || (inveg == 10)) {
    #   if(tw > 22) {
    #     plai[11] <- exist[11] * 0.80 * plai_init[4,inveg]
    #     plai[12] <- exist[12] * 0.20 * plai_init[4,inveg]
    #   } else {
    #     plai[11] <- exist[11] * 0 * plai_init[4,inveg]
    #     plai[12] <- exist[12] * 1 * plai_init[4,inveg]
    #   }
    # } else {
    #   plai[11] <- exist[11] / (igrass) * plai_init[4,inveg]
    #   plai[12] <- exist[12] / (igrass) * plai_init[4,inveg]
    # }
    
    # cropping systems - just set equal to zero
    plai[] <- 0   
    
    
  }
  
  # ************************************************************************
  # case (2) assign vegetation characteristics for dynamic vegtation
  #          that is initialized with uniform vegetation conditions
  # ************************************************************************
  #
  # specify uniform initial conditions
  
  # PFT_UPDATE: Vegetação natural
  if(isimveg == 2) {
    
    for(i in 1:npft) {
      if(plantList[[i]]$canopy == UPPER) {
        plai[i]     <- exist[i] / (iupper) * plaiupper
      } else {
        plai[i] <- exist[i] / (ilower) * plailower
      }
    }
    
  }
  
  # ************************************************************************
  # for both cases (1) and (2)
  # ************************************************************************
  #
  # set minimum lai for each existing plant type
  #
  #         xminlai <- 0.010
  
  
  for(j in 1:npft) {
    plai[j] <- max (plai[j] , exist[j] * xminlai) 
  }
  
  # set sapwood fraction and biomass characteristics
  sapfrac <- sapfrac_init   # from params.veg
  
  wood <- 0
  
  for(j in 1:npft) { 
    
    # TODO: Crir parâmetro para vegetação natural, altura inicial.
    if(plantList[[j]]$type == NATURAL_VEG) {
      if (plantList[[j]]$name == "BroadleafDD") {
        plai[j] <- 1.00
      } else if (plantList[[j]]$name == "BroadleafE") {
        plai[j] <- 5.00
      } else {
        plai[j] <- 0.250
      }  
      # if (!(plantList[[j]]$name == "BroadleafE") && !(plantList[[j]]$name == "BroadleafDD"))
      # plai[j] <- 0.125
    }
    
    print(paste0(j,' / ',plai[j]))
    cbiol[j] <- plai[j] / specla[j]
    cbior[j] <- 0.5 * cbiol[j]
    
    cbiow[j] <- 0
    
    # crop biomass storage -- stem and grain (fruit)
    cbios[j] <- 0
    cbiog[j] <- 0
    
    if (plantList[[j]]$type == NATURAL_VEG && plantList[[j]]$canopy == UPPER)
      cbiow[j] <- plai[j] * 10 / 6
    
    biomass[j] <- cbiol[j] + cbiow[j] + cbior[j] + cbios[j] + cbiog[j]
    wood <- wood + cbiow[j]
    
  }
  
  # ************************************************************************
  # determine basic vegetation structure characteristics
  # ************************************************************************
  # 
  # total leaf area for upper and lower canopies
  
  for(i in 1:npft) {
    if(plantList[[i]]$canopy == UPPER) {
      totlaiu <- totlaiu + plai[i]
      totbiou <- totbiou + biomass[i]
    }
  }
  
  for(i in 1:npft) {
    if(plantList[[i]]$canopy == LOWER) {
      totlail <- totlail + plai[i]
      totbiol <- totbiol + biomass[i]
      
    }
  }
  
  # initial single-sided sai for upper and lower canopies
  sai[1] <- 0.050 * totlail
  sai[2] <- 0.250 * totlaiu
  
  # fractional cover
  fu <- (1 - exp( - wood)) / (1 - exp( - woodnorm))
  
  fl <- totlail / 1
  
  fu <- max (0.25, min (0.975, fu))
  fl <- max (0.25, min (0.975, fl))
  
  # initial lai for canopy physics
  lai[1] <- totlail / fl
  lai[2] <- totlaiu / fu
  
  #sant	inital atmosphere height, after first time step it is updated in canopy.f / subroutine canini(jday)
  za <- 60
  
  # specify canopy height parameters
  # calculated as a function of only the vegetative fraction of each grid cell
  # TET changed 6 / 26 / 02, CSANT changed 2011
  # if(cropsums == 0) {  #csant
  
  zbot[1] <- 0.05
  ztop[1] <- max (0.25, lai[1] * 0.25)
  
  zbot[2] <- ztop[1] + 1 
  ztop[2] <- max (zbot[2] + 1, 2.50 * totbiou / fu * 0.75)
  
  # for(ij in 1:npft) { 
  #   print(paste0('point ',0,' | PFT = ',ij,'  exist <- ',exist[ij],'  initial LAI = ',plai[ij]))
  # }
  # print(paste('initial canopy LAI  ',lai[1],lai[2],sep = ' / '	))
  # print(paste('initial canopy height ',zbot[1],ztop[1],zbot[2],ztop[2],sep = ' / '	))
  
  # } else {
  #   
  #   #sant. for crops ztop is calculated in crops.f 
  #   zbot[1] <- 0.01
  #   ztop[1] <- 0.50
  #   #to do: Santiago, read the flux tower hight 
  #   ztop[2] <- 5  #to match the tower, wind
  #   zbot[2] <- 4.5  #to match the tower, wind
  #   
  #   # PFT_UPDATE (Santiago): Pq para a cana faz essa verificação?
  #   # for(i in 1:npft) {
  #   #   if(zbot[2] <= ztopmxPft[i]) {
  #   #     print(paste0('upper canopy botton cannot be lower than maximum crop height'))
  #   #     stop()
  #   #   }
  #   # }
  # }
  
  # ************************************************************************
  # assign some physical properties of vegetation
  # ************************************************************************
  #
  # leaf optical properties were taken from Sellers et al., 1996
  # and Bonan, 1995
  #
  #      rhoveg[1,1] <- 0.10     # vis leaf reflectance, lower story
  #      rhoveg[1,2] <- 0.10     # vis leaf reflectance, upper story 
  #
  #      rhoveg[2,1] <- 0.60     # nir leaf reflectance, lower story
  #      rhoveg[2,2] <- 0.40     # nir leaf reflectance, upper story
  #
  #      tauveg[1,1] <- 0.07     # vis leaf transmittance, lower story
  #      tauveg[1,2] <- 0.05     # vis leaf transmittance, upper story
  #
  #      tauveg[2,1] <- 0.25     # nir leaf transmittance, lower story
  #      tauveg[2,2] <- 0.20     # nir leaf transmittance, upper story
  
  #SANT - for the global crop model,have to be specified for each culture..
  
  #to do: Jair, colocar esses valores na tabela params.can na coluna final onde ha "vmax_pft"
  # depois fazer um for e atribuir em funcando do exist, como esta abaixo
  #inserir tambem no planting, de forma que apos o plantio esse valor seja atualizado
  # ler como chif[j] e atribuir aqui em funcao da exist
  
  #if(exist[13] == 1) { chiflz <-  0.65 
  #}else if(exist[14] == 1) { chiflz <- -0.25
  #}else if(exist[15] == 1) { chiflz <- -0.5
  #}else if(exist[16] == 1) { chiflz <- -0.5
  #}else if(exist[17] == 1) { chiflz <-  0.5
  #}else if(exist[18] == 1) { chiflz <-  0.7
  #}else  {                  chiflz <-  0.0}
  #chifuz <- 0
  
  if(iwheat > 0) {
    chiflz <- 0.65 
  } else {
    chiflz <-  -0.5        # leaf orientation factors ( - 1 vertical, 0 random, 1 horizontal)
  }
  # CJK chiflz <- 0        ! leaf orientation factors ( - 1 vertical, 0 random, 1 horizontal)
  
  chifuz <- 0
  
  
  oriev[1] <- max ( - chiflz, 0)
  oriev[2] <- max ( - chifuz, 0)
  
  orieh[1] <- max ( chiflz, 0)
  orieh[2] <- max ( chifuz, 0)
  
  # ************************************************************************
  # define rooting profiles
  # ************************************************************************
  #
  # define rooting profiles based upon data published in:
  #
  # Jackson et al., 1996:  A global analysis of root distributions
  # for terrestrial biomes, Oecologia, 108, 389 - 411
  #
  # and
  #
  # Jackson et al., 1997:  A global budget for fine root biomass, 
  # surface area, and nutrient contents, Proceedings of the National
  # Academy of Sciences, 94, 7362 - 7366
  #
  # rooting profiles are defined by the "beta" parameter
  #
  # beta1 is assigned to the lower vegetation layer (grasses and shrubs)
  # beta2 is assigned to the upper vegetation layer (trees)
  #
  # according to Jackson et al. (1996, 1997), the values of beta
  # typically fall in the following range
  #
  # note that the 1997 paper specifically discusses the distribution
  # of * fine roots * (instead of total root biomass), which may be more
  # important for water and nutrient uptake
  #
  # --------------                 ------------   ------------
  # forest systems                 beta2[1996]   beta2[1997]
  # --------------                 ------------   ------------
  # tropical evergreen forest:        0.962          0.972
  # tropical deciduous forest:        0.961          0.982
  # temperate conifer forest:         0.976          0.980
  # temperate broadleaf forest:       0.966          0.967
  # all tropical / temperate forest:    0.970  
  # boreal forest:                    0.943          0.943
  # all trees:                                       0.976
  #
  # -------------------------      ------------   ------------
  # grassland / shrub systems      beta1[1996]   beta1[1997]
  # -------------------------      ------------   ------------
  # tropical grassland / savanna:     0.972          0.972
  # temperate grassland:              0.943          0.943
  # all grasses:                      0.952          0.952
  # schlerophyllous shrubs:           0.964          0.950
  # all shrubs:                       0.978          0.975
  # crops:                            0.961
  # desert:                           0.975          0.970
  # tundra:                           0.914
  #
  # --------------                 ------------
  # all ecosystems                 beta  (1996)
  # --------------                 ------------
  # all ecosystems:                   0.966
  #
  # for global simulations, we typically assign the following
  # values to the beta parameters
  #
  # beta1 <- 0.950, which is typical for tropical / temperate grasslands
  # beta2 <- 0.970, which is typical for tropical / temperate forests
  #
  # however, these values could be (and should be) further refined
  # when using the model for specific regions
  # 
  #       beta1 <- 0.950  # for lower layer herbaceous plants
  #       beta2 <- 0.975  # for upper layer trees
  #
  # calculate total depth in centimeters
  
  totdepth <- 0
  
  for(k in 1: nsoilay) { 
    totdepth <- totdepth + hsoi[k] * 100
  }
  
  # normalization factors
  frootnorm1 <- 1 - beta1[1] ** totdepth
  frootnorm2 <- 1 - beta2 ** totdepth
  
  # calculate rooting profiles
  for(k in 1: nsoilay) { 
    if(k == 1) {
      depth[k] <- hsoi[k] * 100
      
      froot[k,1] <- 1 - beta1[1] ** depth[k]
      froot[k,2] <- 1 - beta2 ** depth[k]
    } else {
      depth[k] <- depth[k - 1] + hsoi[k] * 100
      
      froot[k,1] <- (1 - beta1[1] ** depth[k]) -  
        (1 - beta1[1] ** depth[k - 1]) 
      
      froot[k,2] <- (1 - beta2 ** depth[k]) -  
        (1 - beta2 ** depth[k - 1]) 
    }
    
    froot[k,1] <- froot[k,1] / frootnorm1
    froot[k,2] <- froot[k,2] / frootnorm2
  }
  
  ###----------------------------------------------------
  ### Michel: Fraction of root in the first 30 cm from top soil
  sumfroot <- matrix(nrow = 1, ncol = 2)
  
  for(k in 1: nsoilay) {
    
    if(depth[k] <= 30){
      
      sumfroot[1, 1] <- sum(froot[1:k, 1])
      sumfroot[1, 2] <- sum(froot[1:k, 2])
      
    } else if (depth[k] > 30 && depth[k-1] <= 30) {
      
      sumfroot[1, 1] <- sum(sumfroot[1, 1], 1 - beta1 ** (30 - depth[k-1]))
      sumfroot[1, 2] <- sum(sumfroot[1, 2], 1 - beta2 ** (30 - depth[k-1]))
      break
      
    }
    
  }
  
  ### END
  ###----------------------------------------------------
  
  assign("cntops", cntops, envir = env)
  assign("cnroot", cnroot, envir = env)
  assign("tnplant", tnplant, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("stresstu", stresstu, envir = env)
  assign("stresstl", stresstl, envir = env)
  assign("a10td", a10td, envir = env)
  assign("a11soiltd", a11soiltd, envir = env)
  assign("daylength", daylength, envir = env)
  assign("a3tdmin", a3tdmin, envir = env)
  assign("precipsum", precipsum, envir = env)
  assign("stsumu", stsumu, envir = env)
  assign("stsuml", stsuml, envir = env)
  assign("a10ancub", a10ancub, envir = env)
  assign("a10ancuc", a10ancuc, envir = env)
  assign("a10ancls", a10ancls, envir = env)
  assign("a10ancl4", a10ancl4, envir = env)
  assign("a10ancl3", a10ancl3, envir = env)
  assign("a10scalparamu", a10scalparamu, envir = env)
  assign("a10scalparaml", a10scalparaml, envir = env)
  assign("a10daylightu", a10daylightu, envir = env)
  assign("a10daylightl", a10daylightl, envir = env)
  assign("falll", falll, envir = env)
  assign("fallr", fallr, envir = env)
  assign("fallw", fallw, envir = env)
  assign("croplive", croplive, envir = env)
  assign("vegtype0", vegtype0, envir = env)
  assign("plai", plai, envir = env)
  assign("sapfrac", sapfrac, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("cbios", cbios, envir = env)
  assign("cbiog", cbiog, envir = env)
  assign("biomass", biomass, envir = env)
  assign("totlaiu", totlaiu, envir = env)
  assign("totlail", totlail, envir = env)
  assign("totbiou", totbiou, envir = env)
  assign("totbiol", totbiol, envir = env)
  assign("sai", sai, envir = env)
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)
  assign("lai", lai, envir = env)
  assign("za", za, envir = env)
  assign("zbot", zbot, envir = env)
  assign("ztop", ztop, envir = env)
  assign("exist", exist, envir = env)
  assign("oriev", oriev, envir = env)
  assign("orieh", orieh, envir = env)
  assign("froot", froot, envir = env)
  assign("sumfroot", sumfroot, envir = env)
  
  # return to main program
  return()
}

