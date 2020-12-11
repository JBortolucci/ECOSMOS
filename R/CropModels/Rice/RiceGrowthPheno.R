



simDataVars$ndiasV6    <- 0
simDataVars$ndiasR0    <- 0
simDataVars$ndiasR4    <- 0
simDataVars$ndiasR9    <- 0
simDataVars$DVS        <- 0
simDataVars$TMAXC      <- 0
simDataVars$TMINC      <- 0
simDataVars$TTSUM      <- 0
simDataVars$DRLVTa     <- 0
simDataVars$ID     <- 0
simDataVars$SRCT     <- 0



RiceGrowthPheno <- function(iyear, iyear0, imonth, iday, jday, index) {
  
  
  environment(Phenology)    <- env
  
  
  # parametros
  
  
  i <- index
  
  greenfrac[i]<-1.0
  
  if (croplive[i]==1) {
    
    
    idpp[i] <- idpp[i] + 1
    
    
    #____________________________________    
    #START TEST RICE MODEL FROM ORYZA    
    
    ##
    ##==========================================================#
    #
    
    Phenology(idpp[i],jday)
    
    
    
    
    #To do: levar os parametros para a plant_params.csv    
    if(idpp[i]==1){     
      cbiow[i]  <- 0.00
      cbiob[i]  <- 0.00
      cbior[i]  <- 0.00
      cbiol[i]  <- 0.05
      cbiocr[i] <- 0.01  
    }
    
    
    # TO DO - Jair, levar essa tabela para o plant_params     
    # Partitioning tables as a function of development stage (-; X value):
    
    # Table of fraction total dry matter partitioned to the shoot (-; Y-value)                                                                                                                                                                                      
    
    FSHTB <- as.data.frame(matrix(c( 0.00000000,  0.61965418,                                                               
                                     0.43000001, 0.93724251,
                                     1.00000000, 1.00000000,
                                     2.50000000, 1.00000000),ncol=2,byrow=TRUE))
    
    
    # Table of fraction shoot dry matter partitioned to the leaves (-; Y-value)                                                                                                                                                                                     
    FLVTB <- as.data.frame(matrix(c(    0.00000000,    0.62187672,                                                                
                                        0.61000001,    0.37338009,
                                        0.72000003,    0.24875069,
                                        0.88000000,    0.07399303,
                                        1.23000002,    0.00000000,
                                        2.50000000,    0.00000000),ncol=2,byrow=TRUE))
    
    # Table of fraction shoot dry matter partitioned to the stems (-; Y-value)                                                                                                                                                                                      
    FSTTB <-  as.data.frame(matrix(c( 0.00000000,    0.37812328,                                                                
                                      0.61000001,    0.62661994,
                                      0.72000003,    0.75124931,
                                      0.88000000,    0.45483339,
                                      1.23000002,    0.01019287,
                                      2.50000000,    0.02147198),ncol=2,byrow=TRUE))
    
    # Table of fraction shoot dry matter partitioned to the panicles (-; Y-value)                                                                                                                                                                                   
    FSOTB  <- as.data.frame(matrix(c( 0.00000000,    0.00000000,                                                                
                                      0.61000001,    0.00000000,
                                      0.72000003,    0.00000000,
                                      0.88000000,    0.47117361,
                                      1.23000002,    0.98980713,
                                      2.50000000,    0.97852802),ncol=2,byrow=TRUE))
    
    # Table of leaf death coefficient (d-1; Y-value) as a function of development                                                                                                                                                                                   
    DRLVT <- as.data.frame(matrix(c( 0.00000000,    0.00000000,                                                                
                                     0.60000002,    0.00000000,
                                     1.00000000,    0.02347292,
                                     1.60000002,    0.02389400,
                                     2.09999990,    0.03722218,
                                     2.50000000,    0.05177798),ncol=2,byrow=TRUE))
    
    
    
    
    FSHTB$DVS<-DVS
    FLVTB$DVS<-DVS
    FSTTB$DVS<-DVS
    FSOTB$DVS<-DVS
    DRLVT$DVS<-DVS
    FSHTBa<-INTERLIN(FSHTB)
    FLVTBa<-INTERLIN(FLVTB)
    FSTTBa<-INTERLIN(FSTTB)
    FSOTBa<-INTERLIN(FSOTB)
    DRLVTa<-INTERLIN(DRLVT)
    
    
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






Phenology = function(DAS,jday){
 
# To do: levar para o plant_params   
  #========================================#
  DEGTRAD =  0.017453292   
  TMD     =  42         
  TOD     =  30         
  TBD     =  12         
  MOPP    =  11.5       
  PPSE    =  0          
  TBLV    = 12   
  #========================================#
  
  #========================================#
  
    #DVRJ = 0.001323  # Catiana
    #DVRI = 0.000842  # Catiana
    #DVRP = 0.000799  # Catiana
    #DVRR = 0.002423  # Catiana
    DVRJ = 0.001323 # TAIM
    DVRI = 0.000842 # TAIM
    DVRP = 0.000799 # TAIM
    DVRR = 0.004423 # TAIM

  
  
  #========================================#
  
  
  #==========================================================#
  
  if(DAS==1) {
    ndiasV6 = 0
    ndiasR0 = 0
    ndiasR4 = 0
    ndiasR9 = 0
    DVS     = 0 
    TMAXC   = 0 
    TMINC   = 0 
    TTSUM   = 0 
  }
  
  
  condition4 = (DVS < 2.0) 
  
  #==========================================================#
  if(condition4) {  
    
    TM = ((tmax + tmin)/2)-273.16
    TT = 0
    X1 = (TOD-TBD)/(TMD-TOD)
    X2 = 0.0					
    
    TDM = 0
    TBDC = 0
    TMDC = 0
    TIMED = 1:24  
    
    #==========================================#
    for (I in TIMED) { 
      
      TD = TM + (0.5 * abs(tmax-tmin) * cos(0.2618*(I-14)) )
      
      TDM[I] = TD
      TBDC[I] = TBD
      TMDC[I] = TMD
      
      if ((TD > TBD) && (TD < TMD)) { 
        if (TD > TOD) {
          TD = TOD - (TD-TOD) * (TOD-TBD)/(TMD-TOD)
        }
        
        TT = TT + (TD-TBD)/24
      }
    }
    #==========================================#
    
    
    DEC = -asin(sin(23.45*DEGTRAD) * cos(2*pi*(jday+10)/365))
    AOB = tan(DEGTRAD*lat) * tan(DEC)
    
    DAYL = 12*(1+2 * asin(AOB)/pi)
    
    #==========================================#
    if((DVS >= 0.0) && (DVS < 0.40)){
      
      DVR = DVRJ*TT
      ndiasV6 = ndiasV6+1
      
      #==========================================#
    }else if((DVS >= 0.40) && (DVS < 0.65)){ 
      
      DL = DAYL+0.9
      
      if(DL < MOPP){
        PPFAC = 1
      }else{
        PPFAC = 1-(DL-MOPP)*PPSE
      }
      
      PPFAC = min(1,max(0,PPFAC))
      DVR = DVRI*TT*PPFAC
      ndiasR0 = ndiasR0+1
      
      #==========================================#
    }else if((DVS >= 0.65) && (DVS < 1.00)){
      
      DVR = DVRP*TT
      ndiasR4 = ndiasR4+1
      
      #==========================================#
    }else if(DVS >= 1.00){
      
      DVR = DVRR*TT
      ndiasR9 = ndiasR9+1
      #==========================================#
    }
    
    DVS = min(max((DVS + DVR),0),2.5)
    TTSUM=TTSUM+TT
    TMAXC= TMAXC+(tmax-273.16)
    TMINC= TMINC+(tmin-273.16)
    #==========================================#
    
  }
  #==========================================================#
  
  
  
  assign("ndiasV6", ndiasV6, envir = env)
  assign("ndiasR0", ndiasR0, envir = env)
  assign("ndiasR4", ndiasR4, envir = env)
  assign("ndiasR9", ndiasR9, envir = env)
  assign("DVS", DVS, envir = env)    #to do: Leandro, guardar os valores diarios 
  assign("TMAXC", DVS, envir = env)  #to do: Leandro, guardar os valores diarios
  assign("TMINC", DVS, envir = env)  #to do: Leandro, guardar os valores diarios
  assign("TTSUM", DVS, envir = env)  #to do: Leandro, guardar os valores diarios
  
  
  
  
  return()
}
#                                                          #
#==========================================================#

INTERLIN <- function(MAT){
  DVS<-MAT$DVS
  
  MAT$V3=MAT$V1-DVS
  
  Fsel<-MAT[MAT$V3<0,]
  VALMIN<-Fsel$V2[nrow(Fsel)]
  DVSMIN<-Fsel$V1[nrow(Fsel)]
  
  Fsel<-MAT[MAT$V3>=0,]
  VALMAX<-Fsel$V2[1]
  DVSMAX<-Fsel$V1[1]
  
  VALINT<- VALMIN + (VALMAX-VALMIN)*(DVS-DVSMIN)/(DVSMAX-DVSMIN) 
  return(VALINT)
}


