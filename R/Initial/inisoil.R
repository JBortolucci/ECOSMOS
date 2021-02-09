

inisoil <- function() {

  # MICHEL

  # added for Green-Ampt
  assign("fwpudtot", array(0, 1), envir = env)
  assign("wpud", array(0, 1), envir = env)


  # set sand/silt/clay vectors (xdat,ydat,zdat) for 11 data points
  assign("xdat", texdat[1, 1:ndat], envir = env)
  assign("ydat", texdat[2, 1:ndat], envir = env)
  assign("zdat", texdat[3, 1:ndat], envir = env)

  # initialization and normalization constant for puddle model (kg m-2)
  assign("wipud", array(0, 1), envir = env)

  # set physical parameters of soil
  assign("z0soi", array(0.005, 1), envir = env)

  # initialize soil water and soil temperature fields
  # also initialize daily average fields for crops

  # TODO: Leandro. Mudei de 0.9 pra 0.5
  assign("wsoi", matrix(0.5, 1, nsoilay), envir = env)
  # assign("wsoi", matrix(0.5, 1, nsoilay), envir = env)

  assign("wisoi", matrix(0, 1, nsoilay), envir = env)
  assign("tsoi", matrix(278.13, 1, nsoilay), envir = env)

  assign("adwsoilay", matrix(0.5, 1, nsoilay), envir = env)
  assign("adwisoilay", matrix(0, 1, nsoilay) , envir = env)
  assign("adtsoilay", matrix(278.13, 1, nsoilay) , envir = env)

  # initialize total irrigation
  assign("totirrig", array(0, 1), envir = env)

  # initialize plant available nitrogen pool (kg/m2)
  assign("aplantn", array(0.008, 1), envir = env)

  # initialize soil nitrogen variables
  assign("totnuptake", matrix(0, 1, npft), envir = env)
  assign("stressn", matrix(1, 1, npft), envir = env)
  assign("tg", array(278.13, 1), envir = env)
  assign("ti", array(273.13, 1), envir = env)


  rhosoi   <- matrix(0, nrow = 1, ncol = nsoilay)
  csoi     <- matrix(0, nrow = 1, ncol = nsoilay)
  fracsand <- matrix(0, nrow = 1, ncol = nsoilay)
  fracsilt <- matrix(0, nrow = 1, ncol = nsoilay)
  fracclay <- matrix(0, nrow = 1, ncol = nsoilay)
  poros    <- matrix(0, nrow = 1, ncol = nsoilay)
  sfield   <- matrix(0, nrow = 1, ncol = nsoilay)
  swilt    <- matrix(0, nrow = 1, ncol = nsoilay)
  bex      <- matrix(0, nrow = 1, ncol = nsoilay)
  ibex     <- matrix(0, nrow = 1, ncol = nsoilay)
  suction  <- matrix(0, nrow = 1, ncol = nsoilay)
  hydraul  <- matrix(0, nrow = 1, ncol = nsoilay)
  cpwf     <- matrix(0, nrow = 1, ncol = nsoilay)
  swater   <- matrix(0, nrow = 1, ncol = nsoilay)
  sice     <- matrix(0, nrow = 1, ncol = nsoilay)

  #TODO Henrique: kept when merging forage into soybean on 2020-02-09
  sand<- array(0, nsoilay)
  clay<- array(0, nsoilay)

  # Henrique: Commented on 2020-11-04
  #fclay    <- 0
  #fsilt    <- 0
  #fsand    <- 0

  cpwfdat <- c(
    0.0495,     # sand
    0.0613,     # loamy sand
    0.1101,     # sandy loam
    0.0889,     # loam
    0.1668,     # silt loam
    0.2185,     # sandy clay loam
    0.2088,     # clay loam
    0.2730,     # silty clay loam
    0.2390,     # sandy clay
    0.2922,     # silty clay
    0.3163)


  
  for(k in 1:nsoilay) {
    
    
    if(!is.na(soilType)) {
      
      # tab.DSSAT <- read.csv('inst/input/perfil_solo_ecosmos_UPDATE.csv',sep = ",")
      # tab.DSSAT<- subset(tab.DSSAT, LAT == point$coord$lat & LON == point$coord$lon)
      
      tab.DSSAT <- layers #subset(tab.DSSAT, SID == soilType)
      
      mclay      <- tab.DSSAT$SLCL[k]  # clay content
      msand      <- 100 - tab.DSSAT$SLSI[k] - tab.DSSAT$SLCL[k]
      
      sand[k]     <- msand
      clay[k]     <- mclay
    }

   # if(k <= 6) {
   #   msand <- sand[k]
   #   mclay <- clay[k]
   # } else {
   #   msand <- sand[6]
   #   mclay <- clay[6]
   # }

    lmin <-textcls(msand, mclay)  # class from the global file.

    # print(msand)

    fracsand[k] <- texdat[1, lmin]
    fracsilt[k] <- texdat[2, lmin]
    fracclay[k] <- texdat[3, lmin]
    
    
    # porosity (fraction):
    poros[k] <- porosdat[lmin]
    
    # field capacity (defined relative to the porosity):
    sfield[k] <- 1.0 / poros[k] * sfielddat[lmin]
    
    # wilting point (defined relative to the porosity):
    swilt[k]  <- 1.0 / poros[k] * swiltdat[lmin]
    
    # "b" exponent for the Campbell moisture-release equation:
    bex[k] <- bexdat[lmin]
    
    # nearest integer of "b" exponent (for computational efficiency):
    ibex[k] <-round(bex[k])
    
    # saturated matric (air entry) potential (m-h2o):
    suction[k] <- suctiondat[lmin]
    
    # saturated hydraulic conductivity (m s-1):
    hydraul[k] <- hydrauldat[lmin]
    
    pedofunct <- 1 # chose if use or not pedo functions
    
    if(pedofunct == 1) {
      
      # csant - From Andrea - representing
      fsand <- 0.01 * msand
      fclay <- 0.01 * mclay
      fsilt <- 0.01 * (100 - msand - mclay)
      
      # porosity (fraction):
      poros[k]  <- (50.5 - 3.7 * fclay - 14.2 * fsand) / 100
      bex[k]    <- 3.10 + 15.7 * fclay - 0.3 * fsand
      
      #saturated matric (air entry) potential (m-h2o):
      suction[k] <- 10 ** (2.17 - 0.63 * fclay - 1.58 * fsand) * 0.01
      
      if(ipast == 1) {
        hydraul[k] <- 0.25 * 10 ** (-0.6 - 0.64 * fclay + 1.26 * fsand) * 0.0254 / (3600) #teste para pasto por Santiago. Nao considerar.
      } else {
        hydraul[k] <- 1.0 * 10 ** (-0.6 - 0.64 * fclay + 1.26 * fsand) * 0.0254 / (3600)
      }
      
      # field capacity (defined relative to the porosity):
      sfield[k] <- 1 / poros[k] * (50.5-3.7*fclay-14.2*fsand)/100 * (1.157e-9 / hydraul[k])**(1/(2*bex[k]+3))
      
      # wilting point (defined relative to the porosity):
      swilt[k]  <- 1 / poros[k] * (50.5-3.7*fclay-14.2*fsand)/100 *((10**(2.17-0.63*fclay-1.58*fsand)*0.01)/(1500/9.80665))**(1/bex[k])
      
      print(paste('     Resulting Properties',k,bex[k],fsand,fclay,poros[k],sfield[k]*poros[k],swilt[k]*poros[k],hydraul[k],sep=" / "))
    }
    
    
    
    if(!is.na(soilType)) {

      # tab.DSSAT <- read.csv('inst/input/perfil_solo_ecosmos_UPDATE.csv',sep = ",")
      # tab.DSSAT<- subset(tab.DSSAT, LAT == point$coord$lat & LON == point$coord$lon)
      
      tab.DSSAT <- layers #subset(tab.DSSAT, SID == soilType)
      
      fclay      <- tab.DSSAT$SLCL[k]/100   # clay content
      fsilt      <- tab.DSSAT$SLSI[k]/100   # silt content
      fsand      <- 1- fclay - fsilt        # sand content 
      poros[k]   <-  tab.DSSAT$SSAT[k]      # porosity
      sfield[k]  <- (1 / poros[k]) * tab.DSSAT$SDUL[k]  # field capacity
      swilt[k]   <- (1 / poros[k]) * tab.DSSAT$SLLL[k]  # wilting point
      hydraul[k] <- tab.DSSAT$SSKS[k] / (100 * 3600)    
      suction[k] <- swilt[k]*1.5
      bex[k]     <- tab.DSSAT$BEXP[k]
      SRGF[k] <- tab.DSSAT$SRGF[k]   # Root hospitality factor, used to compute root distribution
      bulkd[k] <- tab.DSSAT$SBDM[k]  # Bulk density, soil layer L
      assign("SRGF", SRGF, envir = env)
      assign("bulkd", bulkd, envir = env)
      
      if(!is.na(tab.DSSAT$BEXP[k])) {
        bex[k]    <- tab.DSSAT$BEXP[k]
      } else{
        bex[k]  <- 3.10 + 15.7 * fclay - 0.3 * fsand
      }
      assign("bex", bex, envir = env) # bexp = Campbell's 'b' exponent
      
      if(!is.na(tab.DSSAT$Bperm[k])) {
        bperm <-  tab.DSSAT$Bperm[1]
        assign("bperm", bperm, envir = env)
      }
      
      if(k==1) {  
        hsoi[k]<-tab.DSSAT$SLB[k]/100
      } else { 
        hsoi[k]<-tab.DSSAT$SLB[k]/100-tab.DSSAT$SLB[k-1]/100  
      }
     
      #      hsoi[k]<-tab.DSSAT$SLB[k]
      #bex[k]<- rep(3,8)#tab.perfil$BEXP[k]

      print("WARNING  -  SOIL DATABASE PROPERTIES ")
      print(paste('     Resulting Properties',k,bex[k],fsand,fclay,poros[k],sfield[k]*poros[k],swilt[k]*poros[k],hydraul[k],sep=" / "))

    } 

    # Convert input sand and clay percents to fractions
    # Changed by TET
    # for now, we assume that all soils have a 1% organic content --
    # this is just a place holder until we couple the soil carbon
    # dynamics to the soil physical properties

    forganic  <- 0.010

    # density of soil material (without pores, not bulk) (kg m-3)
    # from Campbell and Norman, 1998
    rhosoi[k] <- 2650.0 * (1.0 - forganic) + 1300.0 * forganic

    # specific heat of soil material (j kg-1 k-1):
    # from Campbell and Norman, 1998
    csoi[k]   <- 870.0 * (1.0 - forganic) + 1920.0 * forganic

    # added for Green-Ampt
    cpwf[k]   <- cpwfdat[lmin]
    swater[k] <- 0.000001
    sice[k]   <- 0

  }
  
  # Henrique & Leandro: including initial conditions (IC) [2020-11-04]
  for (k in 1:nsoilay) {
    wsoi[k] <- (1 / poros[k]) * ic$SWic[k] # soil water (relative to poros)
    tsoi[k] <- ic$STic[k] + 273.15 # soil temperature (K)
  }
  
  assign("wsoi",  wsoi  , envir = env)
  assign("hsoi", hsoi, envir = env)
  assign("tsoi",  tsoi, envir = env)
  assign("rhosoi",  rhosoi, envir = env)
  assign("csoi",  csoi, envir = env)
  assign("fracsand",  fracsand, envir = env)
  assign("fracsilt",  fracsilt, envir = env)
  assign("fracclay",  fracclay, envir = env)
  assign("poros",  poros, envir = env)
  assign("sfield",  sfield, envir = env)
  assign("swilt",  swilt, envir = env)
  assign("bex",  bex, envir = env)
  assign("ibex",  ibex, envir = env)
  assign("suction",  suction, envir = env)
  assign("hydraul",  hydraul, envir = env)
  assign("cpwf",  cpwf, envir = env)
  assign("swater",  swater, envir = env)
  assign("sice",  sice, envir = env)

  assign("sand",  sand, envir = env)
  assign("clay",  clay, envir = env)


  # surface parameters
  assign("albsav",  fracsand[ ,1] * 0.120 + fracsilt[,1] * 0.085 + fracclay[,1] * 0.050, envir = env)
  assign("albsan",  2.0 * albsav, envir = env)
  

}



# JAIR

# # added for Green-Ampt
# assign("fwpudtot", 0, envir = env)
# assign("wpud", 0, envir = env)
# 
# 
# # set sand/silt/clay vectors (xdat,ydat,zdat) for 11 data points
# assign("xdat", texdat[1, 1:ndat], envir = env)
# assign("ydat", texdat[2, 1:ndat], envir = env)
# assign("zdat", texdat[3, 1:ndat], envir = env)
# 
# # initialization and normalization constant for puddle model (kg m-2)
# assign("wipud", 0, envir = env)
# 
# # set physical parameters of soil
# assign("z0soi", array(0.005, 1), envir = env)
# 
# # initialize soil water and soil temperature fields
# # also initialize daily average fields for crops
# assign("wsoi", matrix(0.5, 1, nsoilay), envir = env)
# assign("wisoi", matrix(0, 1, nsoilay), envir = env)
# assign("tsoi", matrix(278.13, 1, nsoilay), envir = env)
# 
# assign("adwsoilay", matrix(0.5, 1, nsoilay), envir = env)
# assign("adwisoilay", matrix(0, 1, nsoilay) , envir = env)
# assign("adtsoilay", matrix(278.13, 1, nsoilay) , envir = env)
# 
# # initialize total irrigation
# assign("totirrig", 0, envir = env)
# 
# # initialize plant available nitrogen pool (kg/m2)  
# assign("aplantn", array(0.008, 1), envir = env)
# 
# # initialize soil nitrogen variables
# assign("totnuptake", matrix(0, 1, npft), envir = env)
# assign("stressn", matrix(1, 1, npft), envir = env)
# assign("tg", array(278.13, 1), envir = env)
# assign("ti", array(273.13, 1), envir = env)
# 
# 
# rhosoi   <- matrix(0, nrow = 1, ncol = nsoilay)
# csoi     <- matrix(0, nrow = 1, ncol = nsoilay)
# fracsand <- matrix(0, nrow = 1, ncol = nsoilay)
# fracsilt <- matrix(0, nrow = 1, ncol = nsoilay)
# fracclay <- matrix(0, nrow = 1, ncol = nsoilay)
# poros    <- matrix(0, nrow = 1, ncol = nsoilay)
# sfield   <- matrix(0, nrow = 1, ncol = nsoilay)
# swilt    <- matrix(0, nrow = 1, ncol = nsoilay)
# bex      <- matrix(0, nrow = 1, ncol = nsoilay)
# ibex     <- matrix(0, nrow = 1, ncol = nsoilay)
# suction  <- matrix(0, nrow = 1, ncol = nsoilay)
# hydraul  <- matrix(0, nrow = 1, ncol = nsoilay)
# cpwf     <- matrix(0, nrow = 1, ncol = nsoilay)
# swater   <- matrix(0, nrow = 1, ncol = nsoilay)
# sice     <- matrix(0, nrow = 1, ncol = nsoilay)
# 
# 
# #fclay <-matrix(0, 1, nsoilay)
# #fsilt <-matrix(0, 1, nsoilay)
# #fsand <-matrix(0, 1, nsoilay)
# 
# cpwfdat <- c(
#   0.0495,     # sand
#   0.0613,     # loamy sand
#   0.1101,     # sandy loam
#   0.0889,     # loam
#   0.1668,     # silt loam
#   0.2185,     # sandy clay loam
#   0.2088,     # clay loam
#   0.2730,     # silty clay loam
#   0.2390,     # sandy clay
#   0.2922,     # silty clay
#   0.3163 
# )  
# 
# # TODO: Substituir por implementação do michel
# if(!is.nan(soilType)) {
#   # use perfil table
# } else {
#   # use default values
# }
# 
# 
# if(SOIL_OBS == 1) {   
#   
#   tab.perfil<- read.csv('inst/input/tab.perfil.csv',sep = ",")
#   #    tab.perfil<- read.csv('inst/input/tab.perfil.VCP.csv',sep = ",")
# }
# 
# 
# if(SOIL_BR == 1) {   
#   tab.DSSAT<- read.csv('inst/input/perfil_solo_ecosmos.csv',sep = ",")
#   tab.DSSAT<- tab.DSSAT[tab.DSSAT$SID==soilType,]
# }
# 
# # Convert input sand and clay percents to fractions
# # Changed by TET
# for(k in 1:nsoilay) {
#   
#   if(k <= 6) {
#     msand <- sand[k]
#     mclay <- clay[k] 
#   } else {
#     msand <- sand[6] 
#     mclay <- clay[6]
#     
#   }
#   
#   # for now, we assume that all soils have a 1% organic content -- 
#   # this is just a place holder until we couple the soil carbon
#   # dynamics to the soil physical properties
#   
#   forganic <- 0.010
#   
#   # density of soil material (without pores, not bulk) (kg m-3)
#   # from Campbell and Norman, 1998
#   rhosoi[k] <- 2650.0 * (1.0 - forganic) + 1300.0 * forganic 
#   
#   # specific heat of soil material (j kg-1 k-1):
#   # from Campbell and Norman, 1998
#   csoi[k] <- 870.0 * (1.0 - forganic) + 1920.0 * forganic
#   
#   
#   lmin <-textcls(msand, mclay)  # class from the global file.
#   
#   
#   
#   fracsand[k] <- texdat[1, lmin]
#   fracsilt[k] <- texdat[2, lmin]
#   fracclay[k] <- texdat[3, lmin]
#   
#   
#   
#   # porosity (fraction):
#   poros[k] <- porosdat[lmin]
#   
#   # field capacity (defined relative to the porosity):
#   sfield[k] <- 1.0 / poros[k] * sfielddat[lmin]
#   
#   # wilting point (defined relative to the porosity):
#   swilt[k]  <- 1.0 / poros[k] * swiltdat[lmin]
#   
#   # "b" exponent for the Campbell moisture-release equation:
#   bex[k] <- bexdat[lmin]
#   
#   # nearest integer of "b" exponent (for computational efficiency):
#   ibex[k] <-round(bex[k])
#   
#   
#   # saturated matric (air entry) potential (m-h2o):
#   
#   suction[k] <- suctiondat[lmin]
#   
#   # saturated hydraulic conductivity (m s-1):
#   
#   hydraul[k] <- hydrauldat[lmin]
#   
#   
#   pedofunct <- 1 # chose if use or not pedo functions
#   
#   if(pedofunct == 1) {
#     
#     # csant - From Andrea - representing 
#     fsand <- 0.01 * msand
#     fclay <- 0.01 * mclay
#     fsilt <- 0.01 * (100 - msand - mclay)
#     
#     
#     if(SOIL_OBS==1){
#       fsand <- tab.perfil$Sand
#       fclay <- tab.perfil$Clay
#       fsilt <- 1 - fsand - fsilt
#     } # para euclfux
#     #   
#     
#     if(SOIL_BR==1){
#       fclay <- tab.DSSAT$SLCL[k]/100
#       fsilt <- tab.DSSAT$SLSI[k]/100
#       fsand <- 1- fclay - fsilt
#     } # para euclfux
#     #   
#     
#     # porosity (fraction):
#     poros[k] <- (50.5 - 3.7 * fclay - 14.2 * fsand) / 100
#     
#     bex[k]    <- 3.10 + 15.7 * fclay - 0.3 * fsand 
#     
#     
#     #saturated matric (air entry) potential (m-h2o):
#     suction[k] <- 10 ** (2.17 - 0.63 * fclay - 1.58 * fsand) * 0.01
#     
#     if(ipast == 1) {
#       hydraul[k] <- 0.25 * 10 ** (-0.6 - 0.64 * fclay + 1.26 * fsand) * 0.0254 / (3600)
#     } else {
#       hydraul[k] <- 1.0 * 10 ** (-0.6 - 0.64 * fclay + 1.26 * fsand) * 0.0254 / (3600)
#     }
#     
#     # field capacity (defined relative to the porosity):
#     sfield[k] <- 1 / poros[k] * (50.5-3.7*fclay-14.2*fsand)/100 * (1.157e-9 / hydraul[k])**(1/(2*bex[k]+3))
#     
#     # wilting point (defined relative to the porosity):
#     swilt[k]  <- 1 / poros[k] * (50.5-3.7*fclay-14.2*fsand)/100 *((10**(2.17-0.63*fclay-1.58*fsand)*0.01)/(1500/9.80665))**(1/bex[k])
#     
#     
#     
#     
#     
#   }
#   
#   # added for Green-Ampt
#   cpwf[k] <- cpwfdat[lmin]
#   swater[k] <- 0.000001   
#   sice[k] <- 0        
#   
#   
#   print(paste('     Resulting Properties ',k,bex[k],fsand,fclay,poros[k],sfield[k]*poros[k],swilt[k]*poros[k],hydraul[k],sep=" / "))
#   
#   
#   if(SOIL_OBS == 1) {      
#     poros[k]<-tab.perfil$Porosity[k]   
#     sfield[k]<- (1 / poros[k]) * tab.perfil$FC[k]
#     swilt[k] <- (1 / poros[k]) * tab.perfil$WP[k]#/poros[k]
#     hydraul[k]<-tab.perfil$SHC[k]
#     bex[k]<-tab.perfil$BEXP[k]
#     
#     print("WARNING  -  USING EUCAFLUX SOIL PROPERTIES ")
#     print(paste('     Resulting Properties ',k,bex[k],fsand,fclay,poros[k],sfield[k]*poros[k],swilt[k]*poros[k],hydraul[k],sep=" / "))
#     
#   }   # para rodarar com dados do perfil de input
#   
#   
#   if(SOIL_BR == 1) {
#     poros[k]   <-  tab.DSSAT$SSAT[k]
#     sfield[k]  <- (1 / poros[k]) * tab.DSSAT$SDUL[k]
#     swilt[k]   <- (1 / poros[k]) * tab.DSSAT$SLLL[k]
#     hydraul[k] <- tab.DSSAT$SSKS[k] / (100 * 3600)
#     suction[k] <- swilt[k]*1.5
#     
#     if(k==1){  hsoi[k]<-tab.DSSAT$SLB[k]/100} else{ hsoi[k]<-tab.DSSAT$SLB[k]/100-tab.DSSAT$SLB[k-1]/100  }
#     #      hsoi[k]<-tab.DSSAT$SLB[k]
#     #bex[k]<- rep(3,8)#tab.perfil$BEXP[k]
#     
#     print("WARNING  -  SOIL DATABASE PROPERTIES ")
#     print(paste('     Resulting Properties ',k,bex[k],fsand,fclay,poros[k],sfield[k]*poros[k],swilt[k]*poros[k],hydraul[k],sep=" / "))
#     
#   }   # para rodarar com dados do perfil de input
#   
#   
# }
# 
# 
# assign("rhosoi",  rhosoi, envir = env)
# assign("csoi",  csoi, envir = env)
# assign("fracsand",  fracsand, envir = env)
# assign("fracsilt",  fracsilt, envir = env)
# assign("fracclay",  fracclay, envir = env)
# assign("poros",  poros, envir = env)
# assign("sfield",  sfield, envir = env)
# assign("swilt",  swilt, envir = env)
# assign("bex",  bex, envir = env)
# assign("ibex",  ibex, envir = env)
# assign("suction",  suction, envir = env)
# assign("hydraul",  hydraul, envir = env)
# assign("cpwf",  cpwf, envir = env)
# assign("swater",  swater, envir = env)
# assign("sice",  sice, envir = env)
# assign("hsoi",  hsoi, envir = env)
# 
# # surface parameters
# assign("albsav",  fracsand[ ,1] * 0.120 + fracsilt[,1] * 0.085 + fracclay[,1] * 0.050, envir = env)
# assign("albsan",  2.0 * albsav, envir = env)    





# TODO: Substituir por função mais eficiente (reimplementar ou usar função built-in do R)

#------------------------------------------------------------------------------
#
#                             INPOLY
#   Function to tell if a point is inside a polygon or not.
#-------------------------------------------------------------------------------
#   Copyright (c) 1995-1996 Galacticomm, Inc.  Freeware source code.
#
#   Please feel free to use this source code for any purpose, commercial
#   or otherwise, as long as you don't restrict anyone else's use of
#   this source code.  Please give credit where credit is due.
# 
#   Point-in-polygon algorithm, created especially for World-Wide Web
#   servers to process image maps with mouse-clickable regions.
#
#   Home for this file:  http://www.gcomm.com/develop/inpoly.c
#
#                                       6/19/95 - Bob Stein & Craig Yap
#                                       stein@gcomm.com
#                                       craig@cse.fau.edu
#--------------------------------------------------------------------------------
#   Modified by:
#   Aris Gerakis, apr. 1998: 1.  translated to Fortran
#                            2.  made it work with real coordinates
#                            3.  now resolves the case where point falls
#                                on polygon border.
#   Aris Gerakis, nov. 1998: Fixed error caused by hardware arithmetic
#   Aris Gerakis, july 1999: Now all borderline cases are valid
#--------------------------------------------------------------------------------
#   Glossary:
#   function inpoly: true=inside, false=outside (is target point inside a 2D polygon?)
#   poly(*,2):  polygon points, [0]=x, [1]=y
#   npoints: number of points in polygon
#   xt: x (horizontal) of target point
#   yt: y (vertical) of target point
#--------------------------------------------------------------------------------

inpoly <- function(poly, npoints, xt, yt) {
  
  inside    <- FALSE
  on_border <- FALSE
  
  if(npoints < 3) {
    inpoly_output <- FALSE
    return(inpoly_output)
  }
  
  xold <- poly[npoints, 1]
  yold <- poly[npoints, 2]
  
  for(i in 1:npoints) {
    
    xnew <- poly[i, 1]
    ynew <- poly[i, 2]
    
    if (xnew > xold) {
      x1 <- xold
      x2 <- xnew
      y1 <- yold
      y2 <- ynew
    } else {
      x1 <- xnew
      x2 <- xold
      y1 <- ynew
      y2 <- yold
    }
    
    if ((xnew < xt && xt <= xold) || (!(xnew < xt) && !(xt <= xold))) {
      if ((yt-y1)*(x2-x1) == (y2-y1)*(xt-x1)) {
        on_border <- TRUE
      } else if(((yt-y1)*(x2-x1) < (y2-y1)*(xt-x1) + 0.001)) {
        inside <- !inside 
      }
      
    } else if((xnew == xt || xold == xt) && 
              (yt-y1)*(x2-x1) == (y2-y1)*(xt-x1) &&
              ((ynew <= yt && yt <= yold) || 
               (!(ynew < yt) && !(yt < yold)))) {
      
      on_border <- TRUE 
    }
    
    xold <- xnew
    yold <- ynew
  }
  
  # If test point is not on a border, the function result is the last state 
  # of INSIDE variable.  Otherwise, INSIDE doesn't matter.  The point is
  # inside the polygon if it falls on any of its borders:
  
  if (!on_border)
    inpoly_output <- inside
  else
    inpoly_output <- TRUE
  
  
  return(inpoly_output)
  
}




# -------------------------------------------------------------------------
# 
#                           T R I A N G L E
#
#  Main program that calls WHAT_TEXTURE, a function that classifies soil
#  in the USDA textural triangle using sand and clay %
#--------------------------------------------------------------------------
#  Created by: aris gerakis, apr. 98 with help from brian baer
#  Modified by: aris gerakis, july 99: now all borderline cases are valid
#  Modified by: aris gerakis, 30 nov 99: moved polygon initialization to
#               main program
#--------------------------------------------------------------------------
#  COMMENTS
#  o Supply a data file with two columns, in free format:  1st column sand,
# 2nd column clay %, no header.  The output is a file with the classes.
#--------------------------------------------------------------------------
#  You may use, distribute and modify this code provided you maintain
#  this header and give appropriate credit.
#--------------------------------------------------------------------------
textcls <- function(msand, mclay) {
  
  
  sandy      <- matrix(c(85, 90, 100, 0, 0, 0, 0,
                         0, 10,   0, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  loamy_sand <- matrix(c( 70, 85, 90, 85, 0, 0, 0,
                          0, 15, 10, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_loam <- matrix(c(50, 43, 52, 52, 80, 85, 70,
                         0,  7,  7, 20, 20, 15,  0), nrow = 7, ncol = 2)
  
  loam      <- matrix(c(43, 23, 45, 52, 52, 0, 0,
                        7, 27, 27, 20,  7, 0, 0),nrow = 7, ncol = 2)
  
  silty_loam <- matrix(c(0, 0, 23, 50, 0, 0, 0,
                         0, 27, 27, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_clay_loam <- matrix(c(52, 45, 45, 65, 80, 0, 0,
                              20, 27, 35, 35, 20, 0, 0), nrow = 7, ncol = 2)
  
  clay_loam <- matrix(c(20, 20, 45, 45, 0, 0, 0,
                        27, 40, 40, 27, 0, 0, 0), nrow = 7, ncol = 2)
  
  silty_clay_loam <- matrix(c(0,  0, 20, 20, 0, 0, 0, 
                              27, 40, 40, 27, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_clay <- matrix(c(45, 45, 65, 0, 0, 0, 0,
                         35, 55, 35, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  silty_clay <- matrix(c(0,  0, 20, 0, 0, 0, 0,
                         40, 60, 40, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  clayey <- matrix(c(20,  0,   0, 45, 45, 0, 0,
                     40, 60, 100, 55, 40, 0, 0), nrow = 7, ncol = 2)
  
  
  
  # polygon coordinates  
  # 
  #      sand
  # 
  #      >  85, 90, 100, 0, 0, 0, 0,       ! sand
  #      >  70, 85, 90, 85, 0, 0, 0,       ! loamy sand
  #      >  50, 43, 52, 52, 80, 85, 70,    ! sandy loam
  #      >  43, 23, 45, 52, 52, 0, 0,      ! loam
  #      >   0, 0, 23, 50, 0, 0, 0,        ! silt loam (combined with silt)
  #      >  52, 45, 45, 65, 80, 0, 0,      ! sandy clay loam
  #      >  20, 20, 45, 45, 0, 0, 0,       ! clay loam
  #      >   0, 0, 20, 20, 0, 0, 0,        ! silty clay loam
  #      >  45, 45, 65, 0, 0, 0, 0,        ! sandy clay
  #      >   0, 0, 20, 0, 0, 0, 0,         ! silty clay 
  #      >  20, 0, 0, 45, 45, 0, 0         ! clay
  # 
  #       clay
  # 
  #      > 0, 10, 0, 0, 0, 0, 0,           ! sand
  #      > 0, 15, 10, 0, 0, 0, 0,          ! loamy sand
  #      > 0, 7, 7, 20, 20, 15, 0,         ! sandy loam 
  #      > 7, 27, 27, 20, 7, 0, 0,         ! loam
  #      > 0, 27, 27, 0, 0, 0, 0,          ! silt loam (combined with silt)
  #      > 20, 27, 35, 35, 20, 0, 0,       ! sandy clay loam
  #      > 27, 40, 40, 27, 0, 0, 0,        ! clay loam
  #      > 27, 40, 40, 27, 0, 0, 0,        ! silty clay loam
  #      > 35, 55, 35, 0, 0, 0, 0,         ! sandy clay
  #      > 40, 60, 40, 0, 0, 0, 0,         ! silty clay
  #      > 40, 60, 100, 55, 40, 0, 0       ! clay
  # 
  #-----------------------------------------------------------------------
  # figure out what texture grid cell and layer are part of  
  # classify a soil in the triangle based on sand and clay %
  #-----------------------------------------------------------------------
  # Created by: aris gerakis, apr. 98
  # Modified by: aris gerakis, june 99.  Now check all polygons instead of
  # stopping when a right solution is found.  This to cover all borderline 
  # cases.
  #-----------------------------------------------------------------------
  
  # C.Molling 8/18/2004
  # The following lines of code are necessary, as the algorithm always
  # gives siltloam if sand or clay is 0%.  We use a minimum of 1% for
  # sand and clay, and then adjust the greater of the two if the sum
  # is more than 100%.
  
  msand <- max(msand, 1)
  mclay <- max(mclay, 1)
  
  if (msand + mclay > 100) {
    if (msand > mclay) {
      msand <- msand - 1
    } else
      mclay <- mclay - 1
  }
  # end C. Molling 8/18/2004
  
  # find polygon(s) where the point is.  
  textcls_output <- 0
  
  
  if (msand > 0 && mclay > 0) {
    
    if(inpoly(sandy, 3, msand, mclay)) 
      textcls_output <- 1      # sand
    
    if(inpoly(loamy_sand, 4, msand, mclay))  
      textcls_output <- 2      # loamy sand
    
    if(inpoly(sandy_loam, 7, msand, mclay))  
      textcls_output <- 3      # sandy loam
    
    if(inpoly(loam, 5, msand, mclay))  
      textcls_output <- 4      # loam
    
    if(inpoly(silty_loam, 4, msand, mclay))  
      textcls_output <- 5      # silt loam
    
    if(inpoly(sandy_clay_loam, 5, msand, mclay))  
      textcls_output <- 6      # sandy clay loam
    
    if(inpoly(clay_loam, 4, msand, mclay))  
      textcls_output <- 7      # clay loam
    
    if(inpoly(silty_clay_loam, 4, msand, mclay))  
      textcls_output <- 8      # silty clay loam
    
    if(inpoly(sandy_clay, 3, msand, mclay))  
      textcls_output <- 9      # sandy clay
    
    if(inpoly(silty_clay, 3, msand, mclay))  
      textcls_output <- 10     # silty clay
    
    if(inpoly(clayey, 5, msand, mclay))  
      textcls_output <- 11     # clay
    
  }
  
  if (textcls_output == 0)
    textcls_output <- 5 #silt loam
  
  return(textcls_output)
  
}


