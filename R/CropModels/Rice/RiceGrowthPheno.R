



simDataVars$ndiasV6    <- 0
simDataVars$ndiasR0    <- 0
simDataVars$ndiasR4    <- 0
simDataVars$ndiasR9    <- 0
simDataVars$DVS        <- 0
simDataVars$TMAXC      <- 0
simDataVars$TMINC      <- 0
simDataVars$TTSUM      <- 0



RiceGrowthPheno <- function(iyear, iyear0, imonth, iday, jday, index) {
  
   
  environment(Phenology)    <- env
  
  
  # parametros
  Alleaf1    <- plantList$rice$params$Alleaf1
  Alleaf2    <- plantList$rice$params$Alleaf2
  Alleafinit <- plantList$rice$params$Alleafinit
  Alleafmin  <- plantList$rice$params$Alleafmin
  Alleafremain <- plantList$rice$params$Alleafremain
  Allocsensb <- plantList$rice$params$Allocsensb
  Allocsenscr <- plantList$rice$params$Allocsenscr
  Allocsensf  <- plantList$rice$params$Allocsensf 
  Bdecay <- plantList$rice$params$Bdecay
  BdecayStart <- plantList$rice$params$BdecayStart
  Bfall <- plantList$rice$params$Bfall
  BfallStart <- plantList$rice$params$BfallStart
  Branch1 <- plantList$rice$params$Branch1
  Branch2 <- plantList$rice$params$Branch2
  Callocb <- plantList$rice$params$Callocb
  Calloccr  <- plantList$rice$params$Calloccr
  Callocf   <- plantList$rice$params$Callocf
  Cdecay    <- plantList$rice$params$Cdecay
  Cfracts   <- plantList$rice$params$Cfracts
  Coroot1   <- plantList$rice$params$Coroot1
  Coroot2   <- plantList$rice$params$Coroot2
  deltay    <- plantList$rice$params$deltay 
  Density   <- plantList$rice$params$Density
  Fdecay1   <- plantList$rice$params$Fdecay1
  Fdecay2   <- plantList$rice$params$Fdecay2
  Fdecay3   <- plantList$rice$params$Fdecay3
  Fdecay4   <- plantList$rice$params$Fdecay4
  Fineroot1 <- plantList$rice$params$Fineroot1
  Fwpmax    <- plantList$rice$params$Fwpmax
  Fwpmin    <- plantList$rice$params$Fwpmin
  Leafsap1  <- plantList$rice$params$Leafsap1
  Leafsap2  <- plantList$rice$params$Leafsap2
  Leafsap3  <- plantList$rice$params$Leafsap3
  LimLai    <- plantList$rice$params$LimLai
  nrn       <- plantList$rice$params$nrn
  nrx       <- plantList$rice$params$nrx
  Rdecay1   <- plantList$rice$params$Rdecay1
  Rdecay2   <- plantList$rice$params$Rdecay2
  Sapheight <- plantList$rice$params$Sapheight
  Siginit   <- plantList$rice$params$Siginit
  Sigmin    <- plantList$rice$params$Sigmin
  Wdecay    <- plantList$rice$params$Wdecay
  
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
    
    
    print(paste(idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,sep = " | "    ))
    
    
#To do: levar os parametros para a plant_params.csv    
if(idpp[i]==1){     
  
    cbiow[i]  <- 0.00
    cbiob[i]  <- 0.00
    cbior[i]  <- 0.00
    cbiol[i]  <- 0.05
    cbiocr[i] <- 0.01  

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

}
    
    
    FSHTB$DVS
    FLVTB$DVS
    FSTTB$DVS
    FSOTB$DVS
    DRLVT$DVS
    
    FSHTBa<-INTERLIN(FSHTB)
    FLVTBa<-INTERLIN(FLVTB)
    FSTTBa<-INTERLIN(FSTTB)
    FSOTBa<-INTERLIN(FSOTB)
    DRLVTa<-INTERLIN(DRLVT)
    
    
    
    aroot<- min(max((1 -    FSHTBa),0),1)
    aleaf<- min(max((FLVTBa*FSHTBa),0),1)
    astem<- min(max((FSTTBa*FSHTBa),0),1)
    arepr<- min(max((FSOTBa*FSHTBa),0),1)

    
    
    cbior[i] <- cbior[i] + aroot[i] * max (0.0, adnpp[i]) - cbior[i]*tauroot[i]  
    cbiol[i] <- cbiol[i] + aleaf[i] * max (0.0, adnpp[i]) - cbiol[i]*tauleaf[i]   #-     (laidecl[i] / specla[i])
    cbios[i] <- cbios[i] + astem[i] * max (0.0, adnpp[i]) 
    cbiog[i] <- cbiog[i] + arepr[i] * max (0.0, adnpp[i]) 
    
    
    # update vegetation's physical characteristics
    plai[i] <- cbiol[i] * specla[i]  
    
    greenfrac[i] <- 1.0
    
    
    
    biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbiog[i]
    
    # keep track of aboveground annual npp
    ayanpp[i] <- ayanpp[i] + adnpp[i] 
    
    
#END TEST RICE MODEL FROM ORYZA    
#____________________________________        
    


# To do: Remover depois     
    # phenology for additional leaf drop - if drought related or temperature related at
    # end of growing season


    hui[i] <- gddplant[i]
    leafout[i]   <- gddplant[i]

      rm <- min(mxmat[i]/365, idpp[i]/365)


      #   if( (idpp[i]+1)>= gday$idpp[length(gday$idpp)]) { gday_c<- gday[length(gday$idpp),]}else{gday_c<- gday[which(gday$idpp ==(idpp[i]+1)),]   }


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
      # check to see if minimum temperature has fallen below freeze
      # kill threshold for 3 consecutive days and if lai is above a minimum,
      # plant will
      # be damaged/killed.  This function is more for spring freeze events
      # or for early fall freeze events
      #
      # currently simulates too many grid cells that are killed by
      # freezing temperatures
      #
      # spring wheat is affected by this, winter wheat kill function
      # is determined in crops.f - is a more elaborate function of
      # cold hardening of the plant
      
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
      
      if(cropy == 1) {
        if ( DVS >= 2.5 ) { # maximum harvest date
          
          Deadfineroots <- cbior[i]

          croplive[i]   <- 0.0
          greenfrac[i]  <- 0.0 # turn all vegetation to brown
          harvdate[i] <- jday
          plai[i]         <- 0.01 # simulates remaining stubble/mulch
          endCycle <- T
          print(paste('Harvest Eucalyptus - = ',cropy,iyear,jday,idpp[i],rm))
        }
      } else {
        print('Eucalyptus has only one cycle - Stop')
        stop()
      }
      
    }
    
     #TO DO: Alexandre - 
    ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i] 
    

  
  assign("endCycle", endCycle, envir = env)
  
  assign("ztopPft", ztopPft, envir = env)
  assign("sapfrac", sapfrac, envir = env)
  
  assign("gddplant", gddplant, envir = env)
  assign("gddtsoi", gddtsoi, envir = env)
  assign("aplantn", aplantn, envir = env)
  assign("fleafi", fleafi, envir = env)
  assign("mxgddgf", mxgddgf, envir = env)
  assign("mxmat", mxmat, envir = env)
  assign("greenfrac", greenfrac, envir = env)
  assign("fleaf", fleaf, envir = env)
  assign("hui", hui, envir = env)
  assign("leafout", leafout, envir = env)
  assign("idpp", idpp, envir = env)
  assign("idpe", idpe, envir = env)
  assign("awood", awood, envir = env)
  assign("aleaf", aleaf, envir = env)
  assign("acroot", acroot, envir = env)
  assign("aroot", aroot, envir = env)
  assign("abranch", abranch, envir = env)
  assign("tlai", tlai, envir = env)
  assign("peaklai", peaklai, envir = env)
  assign("plai", plai, envir = env)
  assign("astemi", astemi, envir = env)
  assign("aleafi", aleafi, envir = env)
  assign("dpgf", dpgf, envir = env)
  assign("grainday", grainday, envir = env)
  assign("thrlai", thrlai, envir = env)
  assign("templai", templai, envir = env)
  assign("gddemerg", gddemerg, envir = env)
  assign("aerial", aerial, envir = env)
  assign("rm", rm, envir = env)
  assign("af1", af1, envir = env)
  assign("af2", af2, envir = env)
  assign("af3", af3, envir = env)
  assign("af4", af4, envir = env)
  assign("af5", af5, envir = env)
  assign("af6", af6, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiocr", cbiocr, envir = env)
  assign("cbiob", cbiob, envir = env)
  assign("fallrsgc", fallrsgc, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("biomass", biomass, envir = env)
  assign("ayanpp", ayanpp, envir = env)
  assign("ccdays", ccdays, envir = env)
  assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("Deadwood",Deadwood      , envir = env)
  assign("Deadbranch",Deadbranch    , envir = env)
  assign("DBranch",DBranch    , envir = env)
  assign("Deadfineroots",Deadfineroots , envir = env)
  assign("Deadleaves",Deadleaves    , envir = env)
  assign("Deadcoroots",Deadcoroots   , envir = env)

  assign("cbiold",cbiold, envir = env)
  assign("cbiols",cbiols, envir = env)

  assign("Sapwood",Sapwood  , envir = env)
  assign("Heartwood",Heartwood, envir = env)

  assign("DBranch_attached",DBranch_attached, envir = env)
  assign("DBranch_decay",DBranch_decay, envir = env)
  assign("Signew",Signew, envir = env)
  
}






Phenology = function(DAS,jday){
  
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
  
  DVRJ = 0.001323
  DVRI = 0.000842
  DVRP = 0.000799
  DVRR = 0.002423
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
  
  MAT$V3=MAT$V1-DSV
  Fsel<-MAT[MAT$V3<0,]
  VALMIN<-Fsel$V2[nrow(Fsel)]
  DVSMIN<-Fsel$V1[nrow(Fsel)]
  
  Fsel<-MAT[MAT$V3>=0,]
  VALMAX<-Fsel$V2[1]
  DVSMAX<-Fsel$V1[1]
  
  VALINT<- VALMIN + (VALMAX-VALMIN)*(DSV-DVSMIN)/(DVSMAX-DVSMIN) 
  return(VALINT)
}


