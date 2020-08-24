



source("R/CropModels/Soybean/SoybeanPhenocrop.R")
source("R/CropModels/Soybean/SoybeanGrowth.R")
source("R/CropModels/Soybean/UTILS.R")

  SoybeanCROPGRO <- function(iyear, iyear0, imonth, iday, jday, index) {
  
    
    environment(PHENOL)              <- env
    environment(SENES)               <- env
    environment(GROW)              <- env
    environment(ROOTS)              <- env
    environment(DEMAND)              <- env
    environment(PODS)              <- env
    environment(VEGGR)              <- env
    environment(PODDET)              <- env
    environment(FREEZE)              <- env
    environment(INCOMP)              <- env
    environment(NUPTAK)              <- env
    environment(MOBIL)              <- env
    environment(NFIX)              <- env
    
    
  
  
  i <- index
  
  greenfrac[i]<-1.0
  
  if (croplive[i]==1) {
    
    
    idpp[i] <- idpp[i] + 1
    
    
    
# to do, comparar o valor PAR com o usado pelo CROPGRO
# vamos ter que criar uma leitura trazendo as variaveis do fortran
        
# PAR       Daily photosynthetically active radiation or photon flux density (moles[quanta]/m2-d)
    PAR = (stinrad) * 4.59e-06 # from W/m2 to mole.m2/s
    TAVG = td -273.16
    
#PG        Daily gross photosynthesis (g[CH2O] / m2 - d)
    PG = max (0.0, adnpp[i]) *(1/0.45) * 10^3  ## adnpp       # daily total npp for each plant type (kg-C/m**2/day) 



    
    ISWWAT='Y'
    

   #  TGRO esta' sendo atribuido internamente, para testar temos que passar via leituro do fortran
    
        
    
    #To do: levar os parametros para a plant_params.csv    
    if(idpp[i]==1){     
      cbior[i]  <- 0.00
      cbiol[i]  <- 0.05
    }

#_____________________________________________________________        
#__________INICIO DAS CHAMADAS do CROPGRO ____________________    

    
    
    TESTE <- 'Y'
    
    if (TESTE == 'Y'){ #### Subrotina: PHENOL ####  
      
      
      
      DAS    <- idpp[i]   
      
      
      if(DAS==1){

        
                
#_______________________________________________        
        # DYNAMIC = 'RUNINIT'
        # To do, remover completamente, pois fizemos as leituras
        KTRANS = KEP
        KSEVAP = -99.   #Defaults to old method of light
        
        
        
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
        CROP    <-'SB' 
        
        #!*CARBON AND NITROGEN MINING PARAMETERS
        CADPR1  <- 0.260   
        CMOBMX  <- 0.024 
        #!*POD LOSS PARAMETERS
        DETACH  <-'N'  
        #!*EVAPOTRANSPIRATION    
        EORATIO <- 1.1
        KEP     <- 0.68
        #!*VEGETATIVE PARTITIONING PARAMETERS
        FRCNOD  <- 0.05
        #!*LEAF SENESCENCE FACTORS
        FREEZ1  <- -2.22    
        FREEZ2  <- -5.00 
        #!*PHOTOSYNTHESIS PARAMETERS
        KCAN     <-  0.67 
        KC_SLOPE <-  0.10     
        #!*PLANT COMPOSITION VALUES
        PCARSH <- 0.380
        PLIPSH <- 0.020
        PLIGSD <- 0.020  
        PLIGSH <- 0.280
        PMINSD <- 0.025
        PMINSH <- 0.030
        POASD  <- 0.040 
        POASH  <- 0.040
        PROLFI <- 0.356
        PRORTI <- 0.092
        PROSHI <- 0.250
        PROSTI <- 0.150
        #!*RESPIRATION PARAMETERS
        PCH2O  <- 1.13
        R30C2  <- 0.0040 
        RCH2O  <- 1.242
        RES30C <- 3.5E-04
        RFIXN  <- 2.830
        RLIG   <- 2.174
        RLIP   <- 3.106
        RMIN   <- 0.05
        RNH4C  <- 2.556
        RNO3C  <- 2.556
        ROA    <- 0.929
        RPRO   <- 0.360
        #!*ROOT PARAMETERS
        PORMIN <- 0.02
        RWUEP1 <- 1.50
        RWUMX  <- 0.04
        #!*NITROGEN FIXATION PARAMETERS
        TTFIX  <- 0
        # Não usados
        # ECONO (ecotype id)  
        # NOUTDO     Logical unit for OVERVIEW.OUT file 
        
        #  PHENOL_OUT <- PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
        
        
        CMINEP = 0.0
        CNOD   = 0.0
        CNODMN = 0.0
        CTONOD = 0.0
        MAINR  = 0.0
        NAVL   = 0.0
        RO     = 0.0
        RP     = 0.0
        RPROAV = RFIXN
        
        TURFAC = 1.0
        SWFAC  = 1.0
        
        RSPNO3 = 0.0
        RSPNH4 = 0.0
        
        KSTRES = 1.0
        
        
        DYNAMIC = 'SEASINIT'
        PHENOL (iyear, iyear0, jday, DAS,DYNAMIC) 
        
  
        
        
        #-----------------------------------------------------------------------
        #     Initialization call to DEMAND must preceed initialization calls
        #         to INCOMP and GROW (need to initialize values of F, FRLF,
        #         FRRT, and FRSTM for use in those routines)  chp 9/22/98
        #-----------------------------------------------------------------------
        
        
        DEMAND(DYNAMIC, CROP, PAR, PGAVL,RPROAV, TAVG,TGRO)     #Input
        
        #-----------------------------------------------------------------------
        #     Call plant COMPosition INitialization
        #     This call must preceed initialization call to GROW (need to
        #         initialize value of SDPROR for use in that routine) chp 9/22/98
        #-----------------------------------------------------------------------
        if (CROP != 'FA') {
          INCOMP(DYNAMIC)          #Input
        }
        
        #-----------------------------------------------------------------------
        GROW(DYNAMIC)                        #Output
        
        #-----------------------------------------------------------------------
        # To do Santiago
        NUPTAK(DYNAMIC,
               DLAYR, DUL,  KG2PPM, LL,    #Input
               NH4, NO3, NLAYR, SAT, SW)               #Input
        
        #-----------------------------------------------------------------------
        MOBIL(DYNAMIC)         #Output
        
        #-----------------------------------------------------------------------
        # To do Santiago
        
        NFIX(DYNAMIC, CNODMN, CTONOD) # falta linkar, DLAYR, NLAYR,SAT, ST, SW)                           #Input
        
        #-----------------------------------------------------------------------
        PODS(DINAMYC, TGRO, NAVL,ISWWAT)                            #Input
        
        #-----------------------------------------------------------------------
        VEGGR (DINAMYC,DAS,iyear,jday, CMINEP, CSAVEV,   NAVL,  PAR, PG, PGAVL,TGRO)                 #Input
        
        #-----------------------------------------------------------------------
        #     Call leaf senescence routine for initialization
        #-----------------------------------------------------------------------
        SENES(DYNAMIC,DAS,PAR)                   #Input
        
        #-----------------------------------------------------------------------
        #     Call to root growth and rooting depth routine
        #-----------------------------------------------------------------------
        ROOTS(DINAMYC,CROP,  ISWWAT)               #Input
        
        
      }
      
      DYNAMIC = 'RATE'
      PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
      
      DYNAMIC = 'INTEGR'
      PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
      
      

      
      
    }
    
    
    # parametros
    
#_________ FIM DAS CHAMADAS DO CROPGRO _______________________
#_____________________________________________________________    
    
    
    
    
    
    
    
    aroot<- min(max((1 -    FSHTBa),0),1)
    aleaf<- min(max((FLVTBa*FSHTBa),0),1)
    astem<- min(max((FSTTBa*FSHTBa),0),1)
    arepr<- min(max((FSOTBa*FSHTBa),0),1)
    
    
    # update carbon reservoirs using an analytical solution
    # to the original carbon balance differential equation
    cbior[i] <- cbior[i] * exp(-1.0 / tauroot[i]) + aroot[i] * tauroot[i] * max(0.0,adnpp[i]) * (1.0 - exp(-1.0 / tauroot[i]))
    
    cbiol[i] <- cbiol[i] + aleaf[i] * max (0.0, adnpp[i])  - DRLVTa*cbiol[i]
    cbios[i] <- cbios[i] + astem[i] * max (0.0, adnpp[i]) 
    cbiop[i] <- cbiop[i] + arepr[i] * max (0.0, adnpp[i]) 
    if(DVS>=0.95) cbiog[i] <- cbiog[i] + arepr[i] * max (0.0, adnpp[i]) 
    
    # Translocation starts only when  storage organs are formed (at development stage 0.95),
    if(ndiasR9==1)  SRCT<-cbios[i]*0.14  # stem reserves pool for translocation 
    
    if(ndiasR9>=1) {
      
      cbios[i] <- cbios[i] -  SRCT/20 # TO DO: Bryan, definir a equação desse percental e o tempo de retirada 
      cbiop[i] <- cbiop[i] +  SRCT/20
      cbiog[i] <- cbiog[i] +  SRCT/20
    }
    
    #    !----------Check sink limitation based on yesterday's growth rates
    # ! and adapt partitioning of stem-storage organ accordingly
    # BRYAN TO DO ->  IF (GRAINS) THEN
    # BRYAN TO DO ->  IF (GGR.GE.(PWRR-WRR)) THEN
    # BRYAN TO DO ->  FSO = MAX(0.,(PWRR-WRR)/(GCR*FSH))
    # BRYAN TO DO ->  FST = 1.-FSO-FLV
    # BRYAN TO DO ->  END IF
    # BRYAN TO DO ->  END IF    
    
    # update vegetation's physical characteristics
    plai[i] <- cbiol[i] * specla[i] 
    
    peaklai[i]  <- max(peaklai[i]  ,plai[i] )
    
    greenfrac[i] <- 1.0
    
    
    biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbiop[i]
    
    # keep track of aboveground annual npp
    ayanpp[i] <- ayanpp[i] + adnpp[i] 
    
    
    #END TEST RICE MODEL FROM ORYZA    
    #____________________________________        
    
    #_____________________________________________
    
    # keep track of total biomass production for the entire year, and the
    aybprod[i] <- aybprod[i] +
      aleaf[i] * max(0.0,adnpp[i]) +
      abranch[i] * max(0.0,adnpp[i]) +
      aroot[i] * max(0.0,adnpp[i]) +
      awood[i] * max(0.0,adnpp[i]) +
      acroot[i] * max(0.0,adnpp[i])
    
    # aboveground value to calculate harvest index
    ayabprod[i] <- ayabprod[i] +
      aleaf[i] * max(0.0,adnpp[i]) +
      abranch[i] * max(0.0,adnpp[i]) +
      awood[i] * max(0.0,adnpp[i])
    
    
    # keep track of annual total root production carbon
    ayrprod[i] <- ayrprod[i] +
      aroot[i] * max(0.0,adnpp[i]) +
      acroot[i] * max(0.0,adnpp[i])
    
    
    # keep track of total carbon allocated to
    # leaves for litterfall calculation
    aylprod[i] <- aylprod[i] +
      aleaf[i] * max (0.0, adnpp[i])
    
    
    
    
    
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
    
    fileout=paste("RICE_DAILY.csv")
    # ID<-simConfigs[[i]]$id
    if(idpp[i]==1)ID<-paste0(jday,iyear)
    write(paste( ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS ,
                 aroot[i],aleaf[i],astem[i],arepr[i],cbior[i],cbiol[i],cbios[i],cbiog[i],cbiop[i],plai[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
    
    
    
    if(cropy == 1) {
      
      if ( DVS >= 2.0 ) { # maximum harvest date
        
        print(paste('Harvest RICE ',ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai[i],cbiog[i],sep = " ; "    ))
        
        fileout=paste("RICE_SEASON.csv")
        write(paste(ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai,cbiog[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
        
        
        
        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        greenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        peaklai[i]    <- 0.0
        endCycle      <- T
        
        ndiasV6       <-0
        ndiasR0       <-0
        ndiasR4       <-0
        ndiasR9       <-0
        DVS           <-0 
        TMAXC         <-0  
        TMINC         <-0 
        TTSUM         <-0 
        
        
      }
    } else {
      print('Rice has only one cycle - Stop')
      stop()
    }
    
  }
  
  #TO DO: Alexandre - 
  ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i] 
  
  
  
  assign("endCycle", endCycle, envir = env)
  
  assign("ztopPft", ztopPft, envir = env)
  
  assign("greenfrac", greenfrac, envir = env)
  assign("idpp", idpp, envir = env)
  assign("idpe", idpe, envir = env)
  assign("aroot", aroot, envir = env)
  assign("aleaf", aleaf, envir = env)
  assign("astem", astem, envir = env)
  assign("arepr", arepr, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiog", cbiog, envir = env)
  assign("cbiop", cbiop, envir = env)
  assign("cbios", cbios, envir = env)
  assign("cbior", cbior, envir = env)
  assign("plai", plai, envir = env)
  assign("peaklai", peaklai, envir = env)
  assign("aerial", aerial, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  assign("biomass", biomass, envir = env)
  assign("ayanpp", ayanpp, envir = env)
  assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("cropy", cropy, envir = env)
  
  assign("DRLVTa",DRLVTa, envir = env)
  assign("ndiasV6",ndiasV6,envir = env)
  assign("ndiasR0",ndiasR0,envir = env)
  assign("ndiasR4",ndiasR4,envir = env)
  assign("ndiasR9",ndiasR9,envir = env)
  assign("DVS"    ,DVS    ,envir = env)
  assign("TMAXC"  ,TMAXC  ,envir = env)
  assign("TMINC"  ,TMINC  ,envir = env)
  assign("TTSUM"  ,TTSUM  ,envir = env)
  assign("ID"  ,ID  ,envir = env)
  
  assign("SRCT"  ,SRCT  ,envir = env)
  
  
  
  }
  
  
  