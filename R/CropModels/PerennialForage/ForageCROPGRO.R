# Dados de suporte da integração do DSSAT/CROPGRO para o ECOSMOS
simDataVars$TGRO_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/TGRO.OUT')
#simDataVars$PROG_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/PROG.OUT')
simDataVars$VARAUX  <- read.table(file = 'C:/DSSAT47/Brachiaria/VARAUX.OUT', header = T)
simDataVars$PGAVLAUX   <-read.table(file = 'C:/DSSAT47/Brachiaria/PGAVL.OUT',header = T)
simDataVars$NAVLAUX   <-read.table(file = 'C:/DSSAT47/Brachiaria/NAVL.OUT',header = T)
simDataVars$ST_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/ST.OUT',row.names = NULL)
simDataVars$SW_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/SW.OUT',row.names = NULL)
simDataVars$NO3_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/NO3.OUT',row.names = NULL)
simDataVars$NH4_T   <-read.table(file = 'C:/DSSAT47/Brachiaria/NH4.OUT',row.names = NULL)
simDataVars$DSSATdb <- read.table(file = 'C:/DSSAT47/Brachiaria/INTEGRACAO_CONTROLE.OUT', header = F)


# simDataVars$PGAVLCount <- 1
# simDataVars$NAVLCount  <- 1

# Ligando/Desligando a conexão com o DSSAT/CROPGRO
# T <- DSSAT/fortran, F <- Ecosmos 
                      # PG  DAYL PAR  TMIN TAVG TGRO TURFAC SWFAC  SW  ST  NO3  NH4
# simDataVars$integr <- c(T  ,T   ,T   ,T   ,T   ,T   ,T     ,T     ,T  ,T  ,T   ,T)
simDataVars$integr <- c(F  ,F   ,F   ,F   ,F   ,F   ,F     ,F     ,F  ,F  ,F   ,F)
# OK  OK   OK   OK   OK   !   OK      OK

NL <- 20
simDataVars$SW              <- rep(0, NL)
simDataVars$ST              <- rep(0, NL)
simDataVars$NO3             <- rep(0, NL)
simDataVars$NH4             <- rep(0, NL)

# Mudar em caso de uso de outras culturas da família CROPGRO 
simDataVars$CROP            <- 'BR'

simDataVars$CMINEP          <- 0
simDataVars$CNODMN          <- 0
simDataVars$CTONOD          <- 0
simDataVars$NAVL            <- 0
simDataVars$RPROAV          <- 0
simDataVars$RSPNO3          <- 0
simDataVars$RSPNH4          <- 0
simDataVars$KSTRES          <- 0
simDataVars$PGAVL           <- 0

simDataVars$CSAVEV          <- 0
simDataVars$CGRSD           <- 0
simDataVars$CGRSH           <- 0
simDataVars$CTONODR         <- 0

simDataVars$plotVARAUX      <- 0
simDataVars$DAYL            <- 0

simDataVars$TMIN            <- 0
simDataVars$NLAYR           <- 0
simDataVars$DLAYR           <- rep(0, simDataVars$NLAYR)
simDataVars$DS              <- rep(0, simDataVars$NLAYR)
simDataVars$LL              <- rep(0, simDataVars$NLAYR)
simDataVars$DUL             <- rep(0, simDataVars$NLAYR)
simDataVars$SAT             <- rep(0, simDataVars$NLAYR)
simDataVars$WR              <- rep(0, simDataVars$NLAYR)
simDataVars$BD              <- rep(0, simDataVars$NLAYR)


simDataVars$Cumul_FHTOT     <- 0
simDataVars$Cumul_FHTOTN    <- 0


# Carregando subrotinas necessárias
source("R/CropModels/PerennialForage/ForagePhenocrop.R")
source("R/CropModels/PerennialForage/ForageGrowth.R")
# source("R/CropModels/PerennialForage/CH2OREF.R")
# source("R/CropModels/PerennialForage/ForageMOW.R")
source("R/CropModels/PerennialForage/HARV.R")
source("R/CropModels/PerennialForage/UTILS.R")

ForageCROPGRO <- function(iyear, iyear0, imonth, iday, jday, index) {
  
  environment(PHENOL)               <- env
  environment(SENES)                <- env
  environment(GROW)                 <- env
  environment(ROOTS)                <- env
  environment(DEMAND)               <- env
  environment(PODS)                 <- env
  environment(VEGGR)                <- env
  environment(PODDET)               <- env
  environment(FREEZE)               <- env
  environment(INCOMP)               <- env
  environment(NUPTAK)               <- env
  environment(MOBIL)                <- env
  environment(NFIX)                 <- env
  environment(RESPIR)               <- env
  environment(forage_harvest)       <- env
  environment(CH2OREF)              <- env
  environment(SENMOB)               <- env
  environment(DORMANCY)             <- env
  
  i <- index
  pgreenfrac[i] <- 1.0 
  
  NLAYR <- nsoilay
  assign("NLAYR",NLAYR, envir = env)
  
  # Carregando os parâmetros genéticos da cultura (Cultivar, Ecótipo & Espécie) + aqueles oriundos do Ecosmos
  params <- plantList$forage$params
  
  #_______________________________________________________________________________  
  # ATRIBUTOS DO SOLO
  depth <- numeric(nsoilay)
  
  for(k in 1:NLAYR) { 
    if(k == 1) {
      depth[k] <- hsoi[k]
    } else {
      depth[k] <- depth[k - 1] + hsoi[k]
    }
  }
  
  for (L in 1:NLAYR)
  {
    DLAYR[L] <- hsoi[L]    * 100
    DS[L]    <- depth[L]   * 100
    LL[L]    <- swilt[L]   * poros[L]
    DUL[L]   <- sfield[L]  * poros[L]
    SAT[L]   <- poros[L]
    WR[L]    <- SRGF[L]
    BD[L]    <- bulkd[L]
  }
  #_______________________________________________________________________________  
  
  if (croplive[i]==1) {
    
    idpp[i] <- idpp[i] + 1
    
    DAS     <- idpp[i]
    
    YRDOY   <- paste0(iyear,sprintf("%03d", jday))
    
    RWUEP1 <- 1.50
    
    #_______________________________________________________________________________  
    # Vars solved by DSSAT/CROPGRO and ECOSMOS  
    
    #                  VARS FROM DSSAT/CROPGRO                              |      VARS FROM ECOSMOS
    ifelse(integr[1],  PG <- VARAUX$PG[VARAUX$DAS==DAS]                     ,      PG <- adan * (30/12) * 1000 ) # converter kg C / m2.d para g CH2O / m2.d
    ifelse(integr[2],  DAYL <- TGRO_T$V4[TGRO_T$V1==DAS & TGRO_T$V2==1]     ,      DAYL <- daylength/60. ) # ! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
    ifelse(integr[3],  PAR <- VARAUX$PAR[VARAUX$DAS==DAS]                   ,      PAR <- adpar* (86400/1000000)* 4.59 ) # (86400/1000000) W/m2 para MJ/m2.d  and 4.59 # MJ/m2.d para mol/m2.d 
    ifelse(integr[4],  TMIN  <- TGRO_T$V7[TGRO_T$V1==DAS & TGRO_T$V2==1]    ,      TMIN <- tmin - 273.16 )
    ifelse(integr[5],  TAVG  <- VARAUX$TAVG[VARAUX$DAS==DAS]                ,      TAVG <- mean(ta_h) - 273.16 )
    ifelse(integr[6],  TGRO  <- TGRO_T$V3[TGRO_T$V1==DAS]                   ,      TGRO <- ta_h - 273.16 )
    if (integr[7]) {   TURFAC  <- VARAUX$TURFAC[VARAUX$DAS==DAS]          }else{   if(stresstl<=0.9) {TURFAC = (1./RWUEP1) * stresstl} else {TURFAC=1} }
    if (integr[8]) {   SWFAC   <- VARAUX$SWFAC[VARAUX$DAS==DAS]           }else{   if(stresstl<=0.9) {SWFAC  = stresstl} else {SWFAC=1} }
    if (integr[9]) {   SW <- as.double(SW_T[DAS,][-1])                    }else{   for (L in 1:NLAYR) {SW[L]  <- wsoi[L] * poros[L]}}
    if (integr[10]){   ST <- as.double(ST_T[DAS,][-1])                    }else{   for (L in 1:NLAYR) {ST[L]  <- tsoi[L] - 273.16}}
    if (integr[11]){   NO3 <- as.double(NO3_T[DAS,][-1])                  }else{   for (L in 1:NLAYR) {NO3[L]  <- 10}}
    if (integr[12]){   NH4 <- as.double(NH4_T[DAS,][-1])                  }else{   for (L in 1:NLAYR) {NH4[L]  <- 10}}
    
    
    assign("TMIN",  TMIN  , envir = env)
    assign("NLAYR", NLAYR , envir = env)
    assign("DLAYR", DLAYR , envir = env)
    assign("DS",    DS    , envir = env)
    assign("LL",    LL    , envir = env)
    assign("DUL",   DUL   , envir = env)
    assign("SAT",   SAT   , envir = env)
    assign("WR",    WR    , envir = env)
    assign("BD",    BD    , envir = env)
    assign("SW",    SW    , envir = env)
    assign("ST",    ST    , envir = env)
    assign("NO3",   NO3   , envir = env)
    assign("NH4",   NH4   , envir = env)
    assign("TGRO",  TGRO  , envir = env)
    
    
    # plotVARAUX <- PG
    
    # PAR     <- VARAUX$PAR[VARAUX$DAS==DAS]
    # AGEFAC  <- VARAUX$AGEFAC[VARAUX$DAS==DAS]  # To do: Henrique, implementar e linkar com o ECOSMOS
    # TRWUP   <- VARAUX$TRWUP[VARAUX$DAS==DAS] # To do: Henrique, implementar no ECOSMOS
    # TURFACIN  <- VARAUX$TURFAC[VARAUX$DAS==DAS]
    # SWFACIN   <- VARAUX$SWFAC[VARAUX$DAS==DAS]  
    # TAVG    <- VARAUX$TAVG[VARAUX$DAS==DAS]
    # SW     <-  as.double(SW_T[DAS,][-1])
    # ST     <-  as.double(ST_T[DAS,][-1])
    # NO3     <-  as.double(NO3_T[DAS,][-1])
    # NH4     <-  as.double(NH4_T[DAS,][-1])
    
    #  Vars solved by ECOSMOS  
    # PG      <- adan * (30/12) * 1000 # converter kg C / m2.d para g CH2O / m2.d
    # PAR     <- adpar* (86400/1000000)* 4.59 # (86400/1000000) W/m2 para MJ/m2.d  and 4.59 # MJ/m2.d para mol/m2.d 
    # AGEFAC <- ????
    # if(stresstl<=0.9) TURFACIN = (1./RWUEP1) * stresstl
    # if(stresstl<=0.9) SWFACIN  = stresstl
    # if(stresstl<=0.9) { auxPG2 <- (1./RWUEP1) * stresstl} else {  auxPG2 = 1 }
    # TAVG <- mean(ta_h)-273.16
    
    
    # for (L in 1:NLAYR) {
    #   # SW[L] <-   wsoi[L]*poros[L]
    #   # ST[L] <- (tsoi[L]-273.16)
    #   
    #   NH4[L]  <- 0.1   
    #   NO3[L]  <- 1.1
    # }
    
    
    
    
    #_______________________________________________________________________________  
    # Vars calculada pelo CROPGRO, verificar novamente    
    # MAINR   <- VARAUX$MAINR[VARAUX$DAS==DAS]
    
    # PGAVL   <- VARAUX$PGAVL[VARAUX$DAS==DAS]
    # CMINEP   <- VARAUX$CMINEP[VARAUX$DAS==DAS]
    
    
    
    
    # to do, comparar o valor PAR com o usado pelo CROPGRO
    # vamos ter que criar uma leitura trazendo as variaveis do fortran
    
    # PAR       Daily photosynthetically active radiation or photon flux density (moles[quanta]/m2-d)
    # PAR = (stinrad) * 4.59e-06 # from W/m2 to mole.m2/s # TODO: Usando VARAUX , VERIFICAR
    # TAVG = td -273.16
    
    #PG        Daily gross photosynthesis (g[CH2O] / m2 - d)
    # TODO: Usando VARAUX , VERIFICAR
    # PG = max (0.0, adnpp[i]) *(1/0.45) * 10^3  ## adnpp       # daily total npp for each plant type (kg-C/m**2/day) 
    
    
    ISWDIS<-'N'
    ISWWAT<-'Y'
    ISWNIT<-'Y'
    ISWSYM<-'N'
    # TODO: VERIFICAR
    # ISWPHO<-'N'
    # MEPHO<-'L'
    
    #!*POD LOSS PARAMETERS
    DETACH  <-'N'
    
    
    
    #  TGRO esta sendo atribuido internamente, para testar temos que passar via leituro do fortran
    
    #To do: levar os parametros para a plant_params.csv    
    if(idpp[i]==1){     
      cbior[i]  <- 0.00
      cbiol[i]  <- 0.05
    }
    
    #_____________________________________________________________        
    #__________INICIO DAS CHAMADAS do CROPGRO ____________________    
    
    #_______________________________________________        
    # DYNAMIC = 'RUNINIT'
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    # DYNAMIC == SEASINIT       
    
    
    #=======================================================================
    #  IPPLNT, Subroutine, C.H. Porter
    #-----------------------------------------------------------------------
    #  Reads variables from crop or species specific data file
    
    
    # To do: Henrique, limpar os parametros que ja estao sendo criados mais de uma vez        
    # CONTROL VARS (.SBX) 
    
    PROSRI  <- params$PROSRI
    
    #!*CARBON AND NITROGEN MINING PARAMETERS
    CADPR1  <-  params$CADPR1 # 0.260   
    CMOBMX  <- params$CMOBMX  #0.024 
    #!*EVAPOTRANSPIRATION    
    EORATIO <- params$EORATIO  #1.1
    KEP     <- params$KEP  #0.68
    # To do, remover completamente, pois fizemos as leituras
    KTRANS = KEP
    # KSEVAP = params$KSEVAP  #-99.   #Defaults to old method of light
    #!*VEGETATIVE PARTITIONING PARAMETERS
    FRCNOD  <- params$FRCNOD  #0.05
    #!*LEAF SENESCENCE FACTORS
    FREEZ1  <- params$FREEZ1  #-2.22    
    FREEZ2  <- params$FREEZ2  #-5.00 
    assign("FREEZ2",FREEZ2, envir = env)
    #!*PHOTOSYNTHESIS PARAMETERS
    KCAN     <-  params$KCAN  #0.67 
    KC_SLOPE <-  params$KC_SLOPE  #0.10     
    #!*PLANT COMPOSITION VALUES
    PCARSH <- params$PCARSH  #0.380
    PLIPSH <- params$PLIPSH  #0.020
    PLIGSD <- params$PLIGSD  #0.020  
    PLIGSH <- params$PLIGSH  #0.280
    PMINSD <- params$PMINSD  #0.025
    PMINSH <- params$PMINSH  #0.030
    POASD  <- params$POASD   #0.040 
    POASH  <- params$POASH   #0.040
    PROLFI <- params$PROLFI  #0.356
    PRORTI <- params$PRORTI  #0.092
    PROSHI <- params$PROSHI  #0.250
    PROSTI <- params$PROSTI  #0.150
    #!*RESPIRATION PARAMETERS
    PCH2O  <- params$PCH2O  #1.13
    R30C2  <- params$R30C2  #0.0040 
    RCH2O  <- params$RCH20  #1.242
    RES30C <- params$RES30C  #3.5E-04
    RFIXN  <- params$RFIXN  #2.830
    RLIG   <- params$RLIG  #2.174
    RLIP   <- params$RLIP  #3.106
    RMIN   <- params$RMIN  #0.05
    RNH4C  <- params$RNH4C  #2.556
    RNO3C  <- params$RNO3C  #2.556
    ROA    <- params$ROA  #0.929
    RPRO   <- params$RPRO  #0.360
    #!*ROOT PARAMETERS
    PORMIN <- params$PORMIN  #0.02
    RWUEP1 <- params$RWUEP1  #1.50
    RWUMX  <- params$RWUMX  #0.04
    #!*NITROGEN FIXATION PARAMETERS
    TTFIX  <- params$TTFIX  #0
    # Não usados
    # ECONO (ecotype id)  
    # NOUTDO     Logical unit for OVERVIEW.OUT file 
    
    #  PHENOL_OUT <- PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
    
    
    # Estava antes das declaracoes de variaveis 
    if(DAS==1){
      
      # for (L in 1:NLAYR)
      # {
      #   wsoi[L]    <- 1
      # }
      # assign("wsoi",  wsoi  , envir = env)
      
      CMINEP <- 0.0
      CNOD   <- 0.0
      CNODMN <- 0.0
      CTONOD <- 0.0
      NAVL   <- 0.0
      RPROAV <- RFIXN
      
      #     from SWFACS
      TURFAC <- 1.0
      SWFAC  <- 1.0
      
      #     CHP/KJB 4/24/2017
      Cumul_FHTOT  <- 0.0
      Cumul_FHTOTN <- 0.0
      
      DYNAMIC <- 'SEASINIT'
      
      PHENOL (iyear, iyear0, jday, DAS, DYNAMIC, TAVG, ISWWAT)
      
      #-----------------------------------------------------------------------
      #-----------------------------------------------------------------------
      #     Seasonal initialization for Dormancy processes
      #-----------------------------------------------------------------------
      
      DORMANCY(DYNAMIC, DAYL)
      
      #-----------------------------------------------------------------------
      #     Initialize pest coupling point and damage variables, first day only
      #         Moved to FOR_PEST module - CHP
      #-----------------------------------------------------------------------
      if ((CROP != 'FA') & (ISWDIS == 'Y')) {
        # CALL FOR_PEST(CONTROL, ISWITCH, 
        #               &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
        #               &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
        #               &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
        #               &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
        #               &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
        #               &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
        #               &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
        #               &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,         !Input
        #               &  WSRIDOT,                                      !Output
        #               
        #               &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)           !Output
      }
      
      #-----------------------------------------------------------------------
      #     Initialization call to DEMAND must preceed initialization calls
      #         to INCOMP and GROW (need to initialize values of F, FRLF,
      #         FRRT, and FRSTM for use in those routines)  chp 9/22/98
      #-----------------------------------------------------------------------
      
      DEMAND(DYNAMIC,DAS , CROP, PAR, PGAVL,RPROAV, TAVG)
      
      #-----------------------------------------------------------------------
      #     Call plant COMPosition INitialization
      #     This call must preceed initialization call to GROW (need to
      #         initialize value of SDPROR for use in that routine) chp 9/22/98
      #-----------------------------------------------------------------------
      if (CROP != 'FA') {
        INCOMP(DYNAMIC)
      }
      
      #-----------------------------------------------------------------------
      GROW(DYNAMIC,iyear,jday, ISWNIT,ISWSYM)
      
      #-----------------------------------------------------------------------
      #      09/23/05 SJR Added subroutine
      #    Call Subroutine to calculate Nitrogen mobilized from natural 
      #      senescence and light stress
      #-----------------------------------------------------------------------
      SENMOB(DYNAMIC, DAS, ISWWAT, PAR)
      #-----------------------------------------------------------------------
      
      #-----------------------------------------------------------------------
      if (ISWNIT == 'Y') {
        NUPTAK(DYNAMIC, PGAVL)
      }
      
      #-----------------------------------------------------------------------
      MOBIL(DYNAMIC)
      
      #-----------------------------------------------------------------------
      NFIX(DYNAMIC, DAS, CNODMN, CTONOD) 
      
      #-----------------------------------------------------------------------
      PODS(DYNAMIC, DAS, NAVL,ISWWAT,iyear,jday,PGAVL)
      
      #-----------------------------------------------------------------------
      VEGGR (DYNAMIC,DAS,iyear,jday,CSAVEV,NAVL,PAR,PG,PGAVL)
      
      #-----------------------------------------------------------------------
      #     Call leaf senescence routine for initialization
      #-----------------------------------------------------------------------
      # SENES(DYNAMIC,DAS,PAR)
      
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS(DYNAMIC,CROP, PG, ISWWAT)
      
    }
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE CALCULATIONS
    #***********************************************************************
    DYNAMIC = 'RATE'
    if (YREMRG != -99){
      yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
      yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
      
      yremrg <- as.character(paste0(substr(YREMRG,1,4),'-01-01'))
      yremrg <- as.Date(yremrg)+as.numeric(substr(YREMRG,5,7))-1
      
      if (yrdoy > yremrg & yremrg > 0 & ISWWAT == 'Y') {
        #       Calculate daily water stess factors (from SWFACS)
        #       EOP in mm/d
        #       TRWUP and EP1 in cm/d
        # assign("SWFAC",SWFAC , envir = env)  
        # assign("TURFAC",TURFAC, envir = env)  
        
        #         if (EOP > 0.001) {
        #           EP1 = EOP * 0.1
        #           if (TRWUP / EP1 < RWUEP1) {
        #             TURFAC = (1./RWUEP1) * TRWUP / EP1
        #           }
        #           if (EP1 >= TRWUP) {
        #             SWFAC = TRWUP / EP1
        #           }
        #         }
        
      }
    }
    
    #-----------------------------------------------------------------------
    #     CALL vegetative and reproductive development subroutine
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      PHENOL (iyear, iyear0, jday, DAS, DYNAMIC, TAVG, ISWWAT)
    }
    
    
    #-----------------------------------------------------------------------
    #     Call Dormancy module to determine today's
    #     dormancy status and adjustment factors.
    #-----------------------------------------------------------------------
    DORMANCY(DYNAMIC, DAYL)
    
    
    if (ISWDIS == 'Y') {
      # CALL FOR_PEST(CONTROL, ISWITCH, 
      #               &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
      #               &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
      #               &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
      #               &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
      #               &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
      #               &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
      #               &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
      #               &    CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,           !Input
      #               &    WSRIDOT,                                        !Output
      #               &    CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)             !Output
      
    }
    
    
    #----------------------------------------------------------------------
    
    if (CROP != 'FA' & DAS > NVEG0) {
      #TODO usar PHOTO.R ou trazer AGEFAC and PG do DSSAT/Fortran
      #TODO: Estamos trazendo AGEFAC and PG do DSSAT/Fortran
      # if (MEPHO == 'L') {
      #   #Retrieve AGEFAC and PG from ETPHOT routine.
      #   #CALL GET('SPAM', 'AGEFAC', AGEFAC)
      #   #CALL GET('SPAM', 'PG'    , PG)
      # } else if (MEPHO == 'C') {
      #   PHOTO(CONTROL, 
      #         BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
      #         NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
      #         SWFAC, TDAY, XHLAI, XPOD,                       #Input
      #         AGEFAC, PG)                                     #Output
      # }
    }
    
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY INTEGRATION 
    #***********************************************************************
    DYNAMIC = 'INTEGR'
    #***********************************************************************
    #-----------------------------------------------------------------------
    #     Move PHENOL integration up here.
    #     Need to set NVEG0 before test for DAS = NVEG0, otherwise,
    #     initialization on day of emergence will never occur.
    #-----------------------------------------------------------------------
    PHENOL (iyear, iyear0, jday, DAS, DYNAMIC, TAVG, ISWWAT)
    
    # -----------------------------------------------------------------------
    #      Compute adjustment factors for Dormancy processes
    # -----------------------------------------------------------------------
    
    DORMANCY(DYNAMIC, DAYL)
    
    
    #-----------------------------------------------------------------------
    if (DAS == NVEG0) {
      #----------------------------------------------------------------------
      #     On day of emergence, initialize:
      #-----------------------------------------------------------------------
      DEMAND("EMERG", CROP, PAR, PGAVL,RPROAV, TAVG)
      
      GROW("EMERG",iyear,jday, ISWNIT,ISWSYM)
      plai[i]  <- max(XLAI,0.1) # Henrique: atribui IAF inicial para ECOSMOS (2020-10-1)
      
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS("EMERG",CROP, PG, ISWWAT)
      
      #-----------------------------------------------------------------------
      #       DYNAMIC = EMERG (not INTEGR) here
      #-----------------------------------------------------------------------
      PODS("EMERG", DAS, NAVL,ISWWAT,iyear,jday,PGAVL)
      #-----------------------------------------------------------------------
      VEGGR ("EMERG",DAS,iyear,jday, CSAVEV,   NAVL,  PAR, PG, PGAVL)
      
      #-----------------------------------------------------------------------
    }
    #----------------------------------------------------------------------
    # if (DETACH == 'Y' & DAS <= NVEG0+1) {
    #   PODDET("EMERG", iyear, jday)
    # }
    
    #***********************************************************************
    #     Skip growth processes and N and C balances before plants emerge
    #-----------------------------------------------------------------------
    if (DAS >= NVEG0) {
      #-----------------------------------------------------------------------
      #     Initialize available N and C for beginning of daily calcs.
      #-----------------------------------------------------------------------
      NAVL = 0.0
      PGAVL = 0.0
      #      WLFDOT = 0.0
      #      WSRFDOT = 0.0
      #-----------------------------------------------------------------------
      #     Reduce daily photosynthesis for soil water stress
      #     This has been moved to photosynthesis routines
      #-----------------------------------------------------------------------
      #      IF (MEEVP .NE. 'Z') THEN
      #        PG = PG * SWFAC
      #        PGNOON = PGNOON * SWFAC
      #      ENDIF
      
      #-----------------------------------------------------------------------
      #    Reduce daily photosynthesis for Dormancy effect
      PG = PG * PPGFAC
      
      #-----------------------------------------------------------------------
      #    Initialize variables that represent N and C availability during a day
      #    Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
      #    PGAVL is the total available CH2O available for growth & respiration
      #
      #    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
      #    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
      #    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
      #
      #-----------------------------------------------------------------------
      
      # DSSAT4 code for CMINEP
      #      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)
      
      
      #-----------------------------------------------------------------------
      #      09/23/05 SJR Added subroutine
      #    Call Subroutine to calculate Nitrogen mobilized from natural 
      #      senescence and light stress
      #-----------------------------------------------------------------------
      
      SENMOB(DYNAMIC, DAS, ISWWAT, PAR)
      
      #-----------------------------------------------------------------------
      # New code to deal with forages and dormancy
      # Boosts mobilization from storage organs after harvests (LAI<CLAIT)
      # Lowers mobilization during dormancy (when PPMFAC>0.0)
      #-----------------------------------------------------------------------
      #      IF (XLAI .LE. CLAIT) THEN
      #      CMOBSR=CMOBSRX*(PPMFAC)
      #      NMOBSR=NMOBSRX*(PPMFAC)
      #      ELSE
      #      CMOBSR=CMOBSRN*(PPMFAC)
      #      NMOBSR=NMOBSRN*(PPMFAC)
      #      ENDIF
      
      
      #      LAIMOBR = CURV(TYPLMOB,LRMOB(1),LRMOB(2),LRMOB(3),
      #                     !     &         LRMOB(4), MIN(XLAI,LRMOB(4)))
      
      #-----------------------------------------------------------------------
      #      Increase mobilization from storage if N status of plant is high.
      #-----------------------------------------------------------------------
      # 
      #      VEGNCNT = PCNL/100*WTLF + PCNST/100*STMWT +  
      #     &              PCNRT/100*RTWT + PCNSR/100*STRWT
      #      VEGNCMX = FNINL*WTLF + FNINS*STMWT + FNINR*RTWT + FNINSR*STRWT
      #      VNSTAT = MIN((VEGNCNT / VEGNCMX),  1.0)
      
      #      VNMOBR = CURV(TYPNMOB,NRMOB(1),NRMOB(2),NRMOB(3),NRMOB(4),VNSTAT)
      #-----------------------------------------------------------------------
      #      Set N mobilization rate from storage
      #      Default to NMOBSRN under most conditions
      #      set to NMOBSRX (max rate) after harvest or severe damage
      #      Reduce from either level depending on degree of dormancy
      #      Mobilization from storage is unaffected by water or N stress
      #      but is accelerated by low N status.
      #-----------------------------------------------------------------------
      #      NMOBSR = (NMOBSRN + (NMOBSRX-NMOBSRN)*VNMOBR)
      #      NMOBSR = (NMOBSR + (NMOBSRX-NMOBSR)*LAIMOBR)*PPMFAC
      #-----------------------------------------------------------------------
      #      Set C mobilization rate from storage
      #      Default to CMOBSRN under most conditions
      #      set to CMOBSRX (max rate) after harvest or severe damage
      #      Reduce from either level depending on degree of dormancy
      #      Mobilization from storage is unaffected by water or N stress
      #-----------------------------------------------------------------------
      #      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*VNMOBR)
      #      CMOBSR = (CMOBSR + (CMOBSRX-CMOBSR)*LAIMOBR)*PPMFAC
      #      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*LAIMOBR)*PPMFAC
      
      #      CMINEP= CMOBMX * (DTX + DXR57) * (WCRLF - WTLF * PCHOLFF) +
      #     &        CMOBMX * (DTX + DXR57) * (WCRST - STMWT * PCHOSTF) +
      #     &        CMOBMX * (DTX + DXR57) * WCRSH +
      #     &        CMOBMX * (DTX+DXR57)*(PPMFAC)*(WCRRT - RTWT * PCHORTF) +
      #     &        CMOBSR * (DTX+DXR57)*(WCRSR - STRWT * PCHOSRF)
      
      
      PGAVL = PG + CMINEP
      
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      #-----------------------------------------------------------------------
      #       Compute maintenance respiration and subtract from available CH2O
      #-----------------------------------------------------------------------
      RESPIR(DAS,PG) 
      
      if (MAINR > PGAVL) {
        PGAVL  = 0.0
      } else {
        PGAVL = PGAVL - MAINR
      }
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      
      #-----------------------------------------------------------------------
      #     Reduce PGAVL if pest damage occurs to C assimilation
      #     Moved to PEST module - chp
      #-----------------------------------------------------------------------
      #     Call PEST Module for INTEGRATION calculations
      #-----------------------------------------------------------------------
      if (ISWDIS == 'Y') {
        # CALL FOR_PEST(CONTROL, ISWITCH, 
        #               &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
        #               &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
        #               &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
        #               &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
        #               &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
        #               &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
        #               &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
        #               &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,         !Input
        #               &  WSRIDOT,                                      !Output
        #               
        #               &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)           !Output
        
        if (ASMDOT > 0.0) {
          PGAVL = PGAVL - ASMDOT
          PGAVL = max(0.0,PGAVL)
        }
        
      }
      
      #-----------------------------------------------------------------------
      #     5/23/05 SJR Add call to CH2O Refill Module for routine refilling
      #      of mobile CH2O during vegetaive stages
      #      10/30/05 SJR - Move routine here.  Had been just prior to FOR_VEGGR
      #      because that is where CSAVEV had been in CROPGRO.  Problem was
      #      that we calculate DEMAND on PG+CMINEP-MAINR then reduce PGAVL by 
      #      CSAVEV.  This results in slightly lower growth in FOR_VEGGR with
      #      a correspondingly lower N requirement - leaving NLEAK.  This 
      #      takes cake of NLEAK.
      #      10/31/05 Change output variable name from CSAVEV to CADVG to 
      #      to prevent confusion with CH2O reserved for refill after NR1
      #      and used for later seed development.
      #-----------------------------------------------------------------------      
      
      
      if (DAS < NR1) {
        CH2OREF(DYNAMIC, PG, PGAVL)
        
      } else {
        CADVG <- 0.0
      }
      
      PGAVL <- PGAVL - CADVG 
      
      CSAVEV <- CADPR1  * FRACDN* min(PG,PGAVL)
      #-----------------------------------------------------------------------
      #     CSAVEV is a faction of PG for vegetative growth that is stored
      #     as CH2O.  Increase this as plant moves from R1 into seed fill.
      #  These two statements came from the FOR_VEGGR subroutine - chp
      #-----------------------------------------------------------------------
      
      PGAVL <- PGAVL - CSAVEV
      
      
      #-----------------------------------------------------------------------
      #    Call Subroutine to calculate Nitrogen and Carbon Demand for new growth
      #-----------------------------------------------------------------------
      DEMAND(DYNAMIC,DAS, CROP, PAR, PGAVL,RPROAV, TAVG)
      
      # if (YRDOY == YREND) return()
      
      #-----------------------------------------------------------------------
      #    Compute N Available From Seed, During Early Growth
      #     chp - this takes much longer than 7 ptd to deplete seed N.
      #-----------------------------------------------------------------------
      if (SDNPL > 0.0001) {
        NAVL = max(SDNPL * DTX / 7. , 0.0)
        # NAVL <- NAVLAUX$NAVL[NAVLCount]
        # NAVLCount <- NAVLCount + 1
        SDNPL = SDNPL - NAVL
      } else {
        SDNPL = 0.0
        NAVL = 0.0
        # NAVL <- NAVLAUX$NAVL[NAVLCount]
        # NAVLCount <- NAVLCount + 1
      }
      
      #-----------------------------------------------------------------------
      #    If ISWNIT = Y - Call soil N routines. Balance Available C and N
      #    If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
      #-----------------------------------------------------------------------
      if (ISWNIT == 'Y') {
        NUPTAK(DYNAMIC, PGAVL)
        
        #-----------------------------------------------------------------------
        #    Account for C Used to reduce N Uptake to protein
        #-----------------------------------------------------------------------
        RSPNO3 = TRNO3U/0.16 * RNO3C
        RSPNH4 = TRNH4U/0.16 * RNH4C
        if (PGAVL < (RSPNO3+RSPNH4)) {
          PGAVL = 0.0
        } else {
          PGAVL = PGAVL - (RSPNO3 + RSPNH4)
        }
        # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
        # PGAVLCount <- PGAVLCount + 1
        #-----------------------------------------------------------------------
        #       Accumulate nitrogen for today's growth, NAVL
        #-----------------------------------------------------------------------
        NAVL = NAVL + TRNU
        # NAVL <- NAVLAUX$NAVL[NAVLCount]
        # NAVLCount <- NAVLCount + 1
      }
      
      #-----------------------------------------------------------------------
      #    CALL Nitrogen mobilization subroutine
      #    to compute availability of N from other tissue (NMINEA)
      #-----------------------------------------------------------------------
      MOBIL(DYNAMIC)
      
      #-----------------------------------------------------------------------
      #    Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
      #-----------------------------------------------------------------------
      if (PGAVL > NMINEA/0.16*RPRO) {
        PGAVL = PGAVL - NMINEA/0.16*RPRO
      } else {
        PGAVL = 0.0
      }
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      NAVL   = NAVL + NMINEA
      # NAVL <- NAVLAUX$NAVL[NAVLCount]
      # NAVLCount <- NAVLCount + 1
      #-----------------------------------------------------------------------
      #     Allow some of today's PG to be used for N fixation, depending
      #     on N uptake and mining, and on demand for N.
      #     NAVLV = N available for veg growth from uptake and mining
      #     CAVVEG = C available for veg growth
      #     NDMVEG = N required for veg growth if all PGAVL is used as computed
      #     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
      #     PROVEG = average protein composition of growing tissue today
      #     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
      #-----------------------------------------------------------------------
      CTONODR = max(0.0, (NDMREP-NAVL)*RFIXN/0.16)
      CTONODR = min(CTONODR,PGAVL)
      CTONOD = 0.0
      CAVVEG = max(0.,(PGAVL - CDMREP))
      NAVLV = max(0.,(NAVL-NDMREP))
      CNDFX = max(0.,(RFIXN/0.16)*(NDMVEG-NAVLV))
      if (CAVVEG > 0.0 & CNDFX > 0.0) {
        PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
        CTONOD = CAVVEG - (CAVVEG + (NAVLV*RFIXN/0.16))*AGRVG/(AGRVG+PROVEG*RFIXN)
      }
      
      #-----------------------------------------------------------------------
      #     Reserve for nodule growth an amount of C equivalent to a fixed
      #     fraction (FRCNOD) of C allocated to root growth.  JWH 7/11/95
      #-----------------------------------------------------------------------
      if (DAS < NR2) {
        CNODMN = CAVVEG * FRRT * FRCNOD
      } else {
        CNODMN = 0.0
      }
      CTONOD = min(CNODMN + max(0.0, CTONOD), CAVVEG) + CTONODR
      
      #-----------------------------------------------------------------------
      #     Call nitrogen fixation routine if ISWSYM is set to Y
      #     and if thermal time exceeds the lag phase for n-fixation
      #-----------------------------------------------------------------------
      if (ISWNIT == 'Y' & ISWSYM == 'Y') {
        if (VSTAGE > TTFIX) {
          NFIX(DYNAMIC, DAS, CNODMN, CTONOD)
        }
      }
      #-----------------------------------------------------------------------
      #       If ISWSYM = U, then N-FIXATION is assumed to occur at a rate
      #       that carbon will allow, and nodules are not grown explicitely
      #-----------------------------------------------------------------------
      if ((ISWNIT == 'Y') & (ISWSYM == 'U') | (ISWNIT != 'Y')) {
        NFIXN = max(0.0,NDMREP + NDMVEG - NAVL)
        CNOD = RFIXN * NFIXN/0.16
      }
      #-----------------------------------------------------------------------
      #    Accumulate NAVL for growth, reduce PGAVL by cost to fix N
      #-----------------------------------------------------------------------
      if (PGAVL > CNOD) {
        PGAVL = PGAVL - CNOD
      } else {
        PGAVL = 0.0
      }
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      NAVL = NAVL + NFIXN
      # NAVL <- NAVLAUX$NAVL[NAVLCount]
      # NAVLCount <- NAVLCount + 1
      #-----------------------------------------------------------------------
      #     Call routine to compute actual seed and shell growth
      #-----------------------------------------------------------------------
      PODS(DYNAMIC, DAS, NAVL,ISWWAT,iyear,jday,PGAVL)
      
      #-----------------------------------------------------------------------
      #     Call specific routines for peanut to determine
      #         Seed size
      #         Pod color
      #         Pod Detachment
      #-----------------------------------------------------------------------
      if (DETACH == 'Y' & DAS >= NR1) {
        PODDET(DYNAMIC, iyear, jday)
      }
      
      #-----------------------------------------------------------------------
      #     Compute carbon required for seed (CGRSD) and shell (CGRSH) growth
      #-----------------------------------------------------------------------
      CGRSD = WSDDTN * AGRSD3
      CGRSH = WSHDTN * AGRSH1
      #-----------------------------------------------------------------------
      #     Reduce PGAVL by C used for seed growth
      #     Also reduce NAVL by N used for seed growth
      #-----------------------------------------------------------------------
      if (PGAVL > (CGRSD + CGRSH)) {
        PGAVL = PGAVL - CGRSD - CGRSH
      } else {
        PGAVL = 0.0
      }
      NAVL   = NAVL - (NGRSD + NGRSH)
      PGAVL  = max(0.0,PGAVL)
      NAVL   = max(0.0,NAVL)
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      # NAVL <- NAVLAUX$NAVL[NAVLCount]
      # NAVLCount <- NAVLCount + 1
      
      
      #-----------------------------------------------------------------------
      #     Call routine to compute actual vegetative growth, C to mine or add
      #-----------------------------------------------------------------------
      VEGGR (DYNAMIC,DAS,iyear,jday, CSAVEV,   NAVL,  PAR, PG, PGAVL)
      
      #-----------------------------------------------------------------------
      #     Compute C required for LF, ST, and RT growth, and remaining C and N
      #-----------------------------------------------------------------------
      # DSSAT4 code
      #      PGAVL = PGAVL - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
      #-----------------------------------------------------------------------
      # Deduct cost of new storage tissue from PGAVL and NAVL
      #-----------------------------------------------------------------------
      PGAVL <- PGAVL + CHORECOVER - AGRVG * (WLDOTN + WSDOTN + WRDOTN + WSRDOTN) - (CMINEP - CMINEA)
      NAVL <- NAVL - NLKSPENT - (NGRLF + NGRST + NGRRT + NGRSR)
      NAVL <- NAVL - (NADLF + NADST + NADRT + NADSR)
      PGAVL <- PGAVL - (CADST + CADLF + CADSR + CADRT) * PCH2O + CADVG + CSAVEV
      
      
      # PGAVL <- PGAVLAUX$PGAVL[PGAVLCount]
      # PGAVLCount <- PGAVLCount + 1
      # NAVL <- NAVLAUX$NAVL[NAVLCount]
      # NAVLCount <- NAVLCount + 1
      #-----------------------------------------------------------------------
      #     Call leaf senescence routine to compute leaf loss variables
      #-----------------------------------------------------------------------
      # SENES(DYNAMIC,DAS,PAR)
      
      #-----------------------------------------------------------------------
      #     Call freeze damage routine if TMIN is less than FREEZ1 deg C
      #-----------------------------------------------------------------------
      if ((TMIN < FREEZ1) | (TMIN < FREEZ2)) {
        FREEZE(TMIN, iyear, jday)
      } else {
        WLFDOT  <- 0.0
        WSRFDOT <- 0.0
        WSFDOT  <- 0.0
      }
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS(DYNAMIC,CROP, PG, ISWWAT)
      
      #-----------------------------------------------------------------------
      #     Compute total C cost for growing seed, shell, and vegetative tissue
      #     for tomorrow's potential growth calculations
      #-----------------------------------------------------------------------
      #       Calculate the respiration required for seed, shell, and veg tissue
      #       depending on the source of N uptake
      #-----------------------------------------------------------------------
      if ((TRNU + NFIXN + NMINEA) > 0.0) {
        RPROAV <- ((RSPNO3 + RSPNH4) * 0.16 + NFIXN * RFIXN + NMINEA * RPRO) / (TRNU + NFIXN + NMINEA)
      } else {
        RPROAV <- (RNO3C + RNH4C) / 2.
      }
      #-----------------------------------------------------------------------
      #     AGRSD2 = SDPRO*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
      #    &         + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
      #-----------------------------------------------------------------------
      AGRSD2 <- FNINSD*6.25*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA   +  POTLIP*RLIP + POTCAR*RCH2O
      AGRSH2 <-  PROSHI*RPROAV + PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA + PMINSH*RMIN + PCARSH*RCH2O
      #-----------------------------------------------------------------------
      #     SJR 6/14/05 - Add cost for STOR protein to AGRVG2
      #-----------------------------------------------------------------------
      
      
      AGRVG2 <- AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI+FRSTR*PROSRI)*RPROAV
      #-----------------------------------------------------------------------
      #     Call routine to integrate growth and damage
      #-----------------------------------------------------------------------
      GROW(DYNAMIC,iyear,jday, ISWNIT,ISWSYM)
      plai[i]  <- max(XLAI,0.1) #TODO Henrique: verificar se aqui seria o lugar ideal dessa atribuição
      
      
      #FHWAH  <- 0.0
      FHTOTN <- 0.0
      FHLPH  <- 0.0
      #      fhpctn = 0.0
      MOWC  <- 0.0
      RSPLC <- 0.0
      
      forage_harvest(iday, imonth, iyear, FHTOTN)
      
      #Cumul_FHTOT  <- Cumul_FHTOT  + FHWAH   #TODO Henrique: verificar valores para acumular [2020-11-26]
      Cumul_FHTOTN <- Cumul_FHTOTN + FHTOTN
      
      assign("Cumul_FHTOT", Cumul_FHTOT, envir = env)
      assign("Cumul_FHTOTN", Cumul_FHTOTN, envir = env)
      
      #-----------------------------------------------------------------------
      #     End of DAS > NVEG0 if construct
      #-----------------------------------------------------------------------
    }
    #-----------------------------------------------------------------------
    
    
    # parametros
    
    # Função precisaremos para integração com o balanço de carbono
    # (Henrique, 2020-08-25)
    # HARVRES e/ou HRes_CGRO
    
    #_________ FIM DAS CHAMADAS DO CROPGRO _______________________
    #_____________________________________________________________    
    
    
    
    
    
    
    
    # aroot<- min(max((1 -    FSHTBa),0),1)
    # aleaf<- min(max((FLVTBa*FSHTBa),0),1)
    # astem<- min(max((FSTTBa*FSHTBa),0),1)
    # arepr<- min(max((FSOTBa*FSHTBa),0),1)
    
    
    # update carbon reservoirs using an analytical solution
    # to the original carbon balance differential equation
    # cbior[i] <- cbior[i] * exp(-1.0 / tauroot[i]) + aroot[i] * tauroot[i] * max(0.0,adnpp[i]) * (1.0 - exp(-1.0 / tauroot[i]))
    
    # cbiol[i] <- cbiol[i] + aleaf[i] * max (0.0, adnpp[i])  # - ??*cbiol[i]
    # cbios[i] <- cbios[i] + astem[i] * max (0.0, adnpp[i]) 
    # cbiop[i] <- cbiop[i] + arepr[i] * max (0.0, adnpp[i]) 
    
    
    cbiol[i] <- WTLF * 0.45 * (1/1000)
    cbios[i] <- STMWT * 0.45 * (1/1000)
    cbior[i] <- RTWT * 0.45 * (1/1000)
    cbiog[i] <- SDWT * 0.45 * (1/1000)
    cbiop[i] <- (PODWT - SDWT) * 0.45 * (1/1000)
    
    
    #    !----------Check sink limitation based on yesterday's growth rates
    # ! and adapt partitioning of stem-storage organ accordingly
    
    # update vegetation's physical characteristics
    # peaklai[i] <- LAIMX #TODO Henrique: verificar ! [2021-03-09]
    # peaklai[i]  <- max(peaklai[i]  ,plai[i] )
    pgreenfrac[i] <- 1.0 
    biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbiop[i]
    
    #####################################################################
    # check for climatic and phenological limits on maturity, growth,
    # and harvest date
    #
    
    #    if (tmin <= tkill[i]) {
    #      ccdays[i] <- ccdays[i] + 1
    #    } else {
    #      ccdays[i] <- 0
    #    }
    #    
    #    if (ccdays[i] >= 1 &&
    #        hui[i] >= 0.6 * gddmaturity[i] &&
    #        croplive[i] == 1) {
    #      croplive[i]     <- 0.0
    #      print(paste0('tkill!!!!!',1,iyear,jday,idpp[i]))
    #      harvdate[i]     <- jday
    #    }
    
    
    
    #___________________________________________________
    #       Harvest
    
    # fileout=paste("RICE_DAILY.csv")
    # ID<-simConfigs[[i]]$id
    # if(idpp[i]==1)ID<-paste0(jday,iyear)
    # write(paste( ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS ,
    #              aroot[i],aleaf[i],astem[i],arepr[i],cbior[i],cbiol[i],cbios[i],cbiog[i],cbiop[i],plai[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
    
    
    
    if(cropy == 1) {
      
      if ( RSTAGE == 8 | frost ) { #TODO Henrique: adapt for Perennial Forages (perhaps take a look at cane may help) [2020-11-26]
        # frost is when TMIN < FREEZE2 (Temperature below which plant growth stops completely)
        # R8 is the physiological maturity, usually when growers should harvest the crop
        # TODO 3rd option to be implemented: user chooses the harvesting date
        
        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        pgreenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        endCycle      <- T
        
      }
    } else {
      print('Forage has only one cycle - Stop')
      stop()
    }
    
  }
  
  #TO DO: Alexandre - 
  ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i] 
  
  
  
  assign("endCycle", endCycle, envir = env)
  assign("ztopPft", ztopPft, envir = env)
  assign("pgreenfrac", pgreenfrac, envir = env)
  assign("idpp", idpp, envir = env)
  # assign("idpe", idpe, envir = env)
  # assign("aroot", aroot, envir = env)
  # assign("aleaf", aleaf, envir = env)
  # assign("astem", astem, envir = env)
  # assign("arepr", arepr, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiog", cbiog, envir = env)
  assign("cbiop", cbiop, envir = env)
  assign("cbios", cbios, envir = env)
  assign("cbior", cbior, envir = env)
  assign("plai", plai, envir = env)
  assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("cropy", cropy, envir = env)
  
  
  assign("CMINEP",CMINEP, envir = env)
  assign("CNODMN",CNODMN, envir = env)
  assign("CTONOD",CTONOD, envir = env)
  assign("RPROAV",RPROAV, envir = env)
  assign("RSPNO3",RSPNO3, envir = env)
  assign("RSPNH4",RSPNH4, envir = env)
  assign("KSTRES",KSTRES, envir = env)
  assign("PGAVL",PGAVL, envir = env)
  # assign("PGAVLCount",PGAVLCount, envir = env)
  # assign("NAVLCount",NAVLCount, envir = env)
  assign("NAVL",NAVL, envir = env)
  assign("SDNPL", SDNPL, envir = env)
  assign("AGRSH2", AGRSH2, envir = env)
  assign("AGRVG2", AGRVG2, envir = env)
  assign("AGRSD2", AGRSD2, envir = env)
  assign("CSAVEV", CSAVEV, envir = env)
  assign("CGRSD", CGRSD, envir = env)
  assign("CTONODR", CTONODR, envir = env)
  assign("CGRSH", CGRSH, envir = env)
  assign("TURFAC", TURFAC, envir = env)
  assign("SWFAC", SWFAC, envir = env)
  # assign("plotVARAUX", plotVARAUX, envir = env)
  assign("DAYL", DAYL, envir = env)
}


