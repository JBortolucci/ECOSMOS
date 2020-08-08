
source("R/CropModels/Soybean/SoybeanPhenocrop.R")
#source("R/CropModels/Soybean/SoybeanGrowth.R")
source("R/CropModels/Soybean/CURV.R")

  SoybeanCROPGRO <- function(iyear, iyear0, imonth, iday, jday, index) {
  
    
    environment(PHENOL)              <- env
  
    
#   SUBROUTINE CROPGRO(CONTROL, ISWITCH, 
#                      &    EOP, HARVFRAC, NH4, NO3, SOILPROP, SPi_AVAIL,   !Input
#                      &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
#                      &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, MDATE, !Output
#                      &    NSTRES, PSTRES1,                                !Output
#                      &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
#                      &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
    
    
    
#C=======================================================================
#C=======================================================================
#C  CROPGRO, Subroutine, G. Hoogenboom, J.W. Jones, K.J. Boote, C. Porter
#C-----------------------------------------------------------------------
#C  CROPGRO template plant growth subroutine.
#C  Computes plant development and growth.
#C-----------------------------------------------------------------------
  
    
   
#   
# To do, parametros do Eucalipto que iremos remover após a implementacao completa
  
#RM 1 Codigo antigo do eucalipto, remover depois que implementar tudo  
  
  # parametros
  Alleaf1    <- plantList$soybean$params$Alleaf1
  Alleaf2    <- plantList$soybean$params$Alleaf2
  Alleafinit <- plantList$soybean$params$Alleafinit
  Alleafmin  <- plantList$soybean$params$Alleafmin
  Alleafremain <- plantList$soybean$params$Alleafremain
  Allocsensb <- plantList$soybean$params$Allocsensb
  Allocsenscr <- plantList$soybean$params$Allocsenscr
  Allocsensf  <- plantList$soybean$params$Allocsensf 
  Bdecay <- plantList$soybean$params$Bdecay
  BdecayStart <- plantList$soybean$params$BdecayStart
  Bfall <- plantList$soybean$params$Bfall
  BfallStart <- plantList$soybean$params$BfallStart
  Branch1 <- plantList$soybean$params$Branch1
  Branch2 <- plantList$soybean$params$Branch2
  Callocb <- plantList$soybean$params$Callocb
  Calloccr  <- plantList$soybean$params$Calloccr
  Callocf   <- plantList$soybean$params$Callocf
  Cdecay    <- plantList$soybean$params$Cdecay
  Cfracts   <- plantList$soybean$params$Cfracts
  Coroot1   <- plantList$soybean$params$Coroot1
  Coroot2   <- plantList$soybean$params$Coroot2
  deltay    <- plantList$soybean$params$deltay 
  Density   <- plantList$soybean$params$Density
  Fdecay1   <- plantList$soybean$params$Fdecay1
  Fdecay2   <- plantList$soybean$params$Fdecay2
  Fdecay3   <- plantList$soybean$params$Fdecay3
  Fdecay4   <- plantList$soybean$params$Fdecay4
  Fineroot1 <- plantList$soybean$params$Fineroot1
  Fwpmax    <- plantList$soybean$params$Fwpmax
  Fwpmin    <- plantList$soybean$params$Fwpmin
  Leafsap1  <- plantList$soybean$params$Leafsap1
  Leafsap2  <- plantList$soybean$params$Leafsap2
  Leafsap3  <- plantList$soybean$params$Leafsap3
  LimLai    <- plantList$soybean$params$LimLai
  nrn       <- plantList$soybean$params$nrn
  nrx       <- plantList$soybean$params$nrx
  Rdecay1   <- plantList$soybean$params$Rdecay1
  Rdecay2   <- plantList$soybean$params$Rdecay2
  Sapheight <- plantList$soybean$params$Sapheight
  Siginit   <- plantList$soybean$params$Siginit
  Sigmin    <- plantList$soybean$params$Sigmin
  Wdecay    <- plantList$soybean$params$Wdecay
#RM 1 Codigo antigo do eucalipto, remover depois que implementar tudo  

    
  i <- index
  
  if (croplive[i]==1) {  
  
    
    
#RM 2 Codigo antigo do eucalipto, remover depois que implementar tudo  
    
    huileaf <- array(0, npft)             # heat unit index needed to attain leaf emergence after planting
    huigrain <- array(0, npft)            # heat unit index needed to reach vegetative maturity
    laidecl <- matrix(0, 1, npft)  # decline in leaf area for crop
    # phenology for additional leaf drop - if drought related or temperature related at
    # end of growing season
    
    ddays      <- 7.0            #inp
    ddfac      <- 1.0 / ddays    #inp
    tthreshold <- 273.16         #par
    
    # number of corn plants per square meter
    # this is only important if using the leaf expansion equations
    # of Ritchie, that is temperature dependent.  Our standard procedure
    # here however is to use the allocation of C to leaf (aleaf) and
    # specific leaf area (specla) to accumulate LAI during the season
    
    nplants <- 7                #inp
    
    aplantn <- 0.0
    
    for(k in 1:nsoilay) {
      aplantn <- aplantn + smsoil[k] + smsoln[k]
    }
    
    
    
    # Eucalyptus phenology, Carbon Aloccation and Harvest
#RM 2 Codigo antigo do eucalipto, remover depois que implementar tudo  
    
    if (croplive[i] == 1.0) { #RM 3 nao compreendi a funcao desse if


#RM 4 Codigo antigo do eucalipto, remover depois que implementar tudo  
      
      huileaf[i]  <- lfemerg[i]  * gddmaturity[i]  # typically between 3 - 7% in wheat
      
      crmeuca       <- max(73., min((gddmaturity[i]+ 53.683) / 13.882,135.))
      huigrain[i] <- -0.002  * (crmeuca - 73.) + grnfill[i]
      huigrain[i] <- min(max(huigrain[i],grnfill[i] - 0.1), grnfill[i])
      huigrain[i] <- huigrain[i]   * gddmaturity[i]  # from Cabelguenne et al. 1999
      
      
      # accumulate growing degree days for planted crops past planting
      gddplant[i] <- gddplant[i] + max(0, min(td - baset[i], mxtmp[i]))
      gddtsoi[i] <- gddtsoi[i] + max(0, min(tsoi[1] - baset[i], mxtmp[i]))
      
      
      greenfrac[i] <- 1.0
      
      # calculate fraction allocated to leaf (from i. Norman allocation curve)
      # bfact and fleafi are set in params.crp
      fleaf[i] <- fleafi[i] * (exp(-bfact[i]) - exp(-bfact[i] * gddplant[i] / huigrain[i])) / (exp(-bfact[i]) - 1)
      
      # calculate accumulated growing degree days since planting (gddplant)
      # determine if growing degree days calculated from top layer soil temperature
      # are enough for leaf emergence to occur
      hui[i] <- gddplant[i]
      
      leafout[i]   <- gddplant[i]
      
      laidecl[i] <- 0.0
      
      
#RM 4 Codigo antigo do eucalipto, remover depois que implementar tudo  
      

            
      idpp[i] <- idpp[i] + 1
      
      
            
#---------------------      
   
#  
  
  TESTE <- 'Y'
  
  if (TESTE == 'Y'){ #### Subrotina: PHENOL ####  
    

  
    DAS    <- idpp[i]      
    
    if(DAS==1){
     
     # DYNAMIC = 'RUNINIT'
    #  PHENOL_OUT <- PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
      DYNAMIC = 'SEASINIT'
      PHENOL (iyear, iyear0, jday, DAS,DYNAMIC) 
        }
      
      DYNAMIC = 'RATE'
      PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
      DYNAMIC = 'INTEGR'
      PHENOL (iyear, iyear0, jday, DAS,DYNAMIC)
      
      saidax<-paste(DYNAMIC,DAS,VSTAGE,RSTAGE,sep = ';')
      
        write.table(saidax,file = 'Phenol.txt',append = T,row.names = F)
        
         print(paste(DYNAMIC,DAS,VSTAGE,RSTAGE,sep=" / "))
         
         if(DAS==115)stop()
         

    }

   
   
   # DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        # Output
   # NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       # Output
   # RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        # Output
   # TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         # Output
   # YRNR2, YRNR3, YRNR5, YRNR7
    
    

      if (leafout[i] >= huileaf[i])   idpe[i] <- idpe[i] + 1
      
      # crop phenology from leaf emergence to start of leaf decline
      
      ######################################################################
      ########## Start Allocation to Perenial (Eucalyptus) crops ############
      ######################################################################
      
      # Phase 1 completed:
      
      if(idpp[i]==1){
        cbiow[i] <- 0.0001455428/kg_C_M2_to_T_ha
        cbiob[i] <- 0.0002444583/kg_C_M2_to_T_ha
        cbior[i] <- 0.0001482158/kg_C_M2_to_T_ha
        cbiol[i] <- 0.005065926 /kg_C_M2_to_T_ha
        cbiocr[i] <- 0.000163217 /kg_C_M2_to_T_ha
        plai[i]  <- cbiol[i]*specla[i]  }
      
      
      rm <- min(mxmat[i]/365, idpp[i]/365)
      
      
      #   if( (idpp[i]+1)>= gday$idpp[length(gday$idpp)]) { gday_c<- gday[length(gday$idpp),]}else{gday_c<- gday[which(gday$idpp ==(idpp[i]+1)),]   }
      
      
      
      hsum   <- 0
      water  <- 0
      Wcapac <- 0
      
      for(k in 1:(nsoilay)) {
        
        hsum <- hsum+hsoi[k]
        
        # new michel
        mh_aw <- 16.61*(1-exp(-0.00202 * idpp[i]))^(1.5883) # Maximum root depth Christina et al. (2017)
        mh_aw <- min(mh_aw, sum(hsoi))                  # mh_w can not be greater than the last soil layer
        
        #     print(paste(sum(hsoi),mh_aw,sep="/"))
        
        if(hsum <= mh_aw) {
          Wcapac <- Wcapac + 1000*(1.0     * hsoi[k] *  poros[k] ) #*froot[k,1] 
          water  <- water  + 1000*(wsoi[k] * hsoi[k] *  poros[k] ) #*froot[k,1] 
        }
      }
      
      
      capac <- 30 + 1.2*Wcapac *min(idpp[i]/600,1) #fazer funcao do tempo
      
      
      waterfact <-  ((water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin )
      # waterfact <-  gday_c$waterfact
      
      waterfact<-max(min(waterfact,1),0)
      
      greenfrac[i] <- 1
      
      #Fine root C allocation
      Finerootexp <- Fineroot1 * (plai[i]*greenfrac[i])
      
      
      aroot[i] = (0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf )
      # Finerootexp <- Fineroot1 * (gday_c$lai*greenfrac[i])
      # aroot[i] = (0.5 + 0.5 * (1.- (gday_c$cbior) / Finerootexp ) / Allocsensf )
      aroot[i]= aroot[i]*(nrx*nrn)/(nrn+(nrx-nrn)*waterfact)
      aroot[i]<-max(min(aroot[i],1),0)
      
      
      #---------------------
      #Leaf C allocation
      # ModelLai==2 from Param_eucaflux.h
      # Recalcular o LAI no passo anterior: biomassa de folha input, add o sla
      if (plai[i]<=0.1) {
        aleaf[i] = Alleafinit # for very small LAI at begining, constant alloc
      } else {
        # leaf allocation fraction is the second priority after fine roots, and has a height (age) constraint
        aleaf[i]= max(Alleafmin,Alleafmin+Alleaf1*exp(-Alleaf2*ztop[1]))
        aleaf[i]= max(aleaf[i], 1-aroot[i]-Alleafremain) #second priority after fine root, with 20% kept apart
      }
      
      aleaf[i]<-max(min(aleaf[i],1),0)
      #      aleaf[i]<-max(min(aleaf[i],0.16),0)
      
      
      #---------------------
      #Branch C allocation (Stem)
      Branchexp = Branch1 * ( plai[i] ^ Branch2 )
      if (Branchexp < 0.)  Branchexp = 0.001
      abranch[i] =  (0.5 + 0.5 * (1.- (cbiob[i]*kg_C_M2_to_T_ha) / Branchexp ) / Allocsensb )
      #  abranch[i] =  (0.5 + 0.5 * (1.- (gday_c$cbiob) / Branchexp ) / Allocsensb )
      
      abranch[i]<-max(min(abranch[i],1),0)
      
      
      #---------------------
      #Root C allocation
      Corootexp <- Coroot1 * ( (cbiow[i]*kg_C_M2_to_T_ha) ^ Coroot2 )
      #Corootexp <- Coroot1 * ( (gday_c$cbiow) ^ Coroot2 )
      
      if (Corootexp < 0.) Corootexp = 0.001
      acroot[i] = (0.5 + 0.5 * (1.- (cbiocr[i]*kg_C_M2_to_T_ha) / Corootexp ) / Allocsenscr )
      #  acroot[i] = (0.5 + 0.5 * (1.-gday_c$cbiocr / Corootexp ) / Allocsenscr )
      
      acroot[i]<-max(min(acroot[i],1),0)
      
      
      
      #---------------------
      #Stem (G'DAY) C allocation or Wood (ECOSMOS)
      # reduction factor was used to guaranteer the values (Alleaf + Alfineroot + Albran + Alcoroot) were all lower than 1
      Callocfr<- 1- (Callocf  + Callocb + Calloccr)
      aroot[i] = aroot[i]*Callocfr
      aleaf[i] = aleaf[i]*Callocf
      abranch[i] = abranch[i]*Callocb
      acroot[i] = acroot[i]*Calloccr
      
      #SVC ___ just test to make LAI increase
      #if(plai[i]<=4){
      #  aleaf[i]<-max(0.3,aleaf[i])
      #  print('increase aleaf to male leaf increase')
      #
      #}
      
      if ( (aroot[i] + aleaf[i] + abranch[i] + acroot[i]) > 1 ) {
        reductionfactor <- 1 / (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
        aroot[i] = aroot[i]*reductionfactor
        aleaf[i] = aleaf[i]*reductionfactor
        abranch[i] = abranch[i]*reductionfactor
        acroot[i] = acroot[i]*reductionfactor
      }
      
      awood[i] = 1 - (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
      
      awood[i] <- max(0.0, awood[i])
      aroot[i] <- max(0.0, aroot[i])
      aleaf[i] <- max(0.0, aleaf[i])
      abranch[i] <- max(0.0, abranch[i])
      acroot[i] <- max(0.0, acroot[i])
      
      
      
      # END EUCALYPTUS
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
      
      
      
      
      #---------------------
      #Mortality - Litterfall C fluxes
      #---------------------
      
      #dead stem computation
      Deadwood   <- 0
      Deadcoroots <- 0
      if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        fdec <- exp(2.*log(2.)*(rm-BdecayStart))-1
        fdec <- max(min(fdec,1),0)
        Deadwood    =  fdec * Wdecay * Sapwood # Stock of dead branch that do not fall into the ground. It is assumed to start at 3 years old
        Deadcoroots  =  fdec * Cdecay * cbiocr[i]*kg_C_M2_to_T_ha  # cm
      }
      
      #---------------------
      #dead branches computation
      
      if (rm <= BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        #dead stem computation
        DeadGbranch <- 0
        Deadbranch  <- 0
        
      } else if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        fdec <- exp(2.*log(2.)*(rm-BdecayStart))-1
        fdec <- max(min(fdec,1),0)
        DeadGbranch  =  fdec * Bdecay * cbiob[i]*kg_C_M2_to_T_ha
        
      } else if ( rm > BfallStart ) {
        #beginning of the dead branches fall
        fdec = exp(2*log(2)*(rm-BfallStart))-1.
        fdec <- max(min(fdec,1),0)
        Deadbranch   =  fdec * Bfall * DBranch_attached*kg_C_M2_to_T_ha   # Dead branch that falls into the ground and entered the above-ground structural litter pool
        
      } else  if (rm > 4 ) {
        # After 4.5 years, both branch death and branch fall increase considerably ==> empirical correction
        fdec = exp(2*log(2)*(rm-4))-1.
        fdec <- max(min(fdec,1),0)
        # TODO: Reve valores de cada
        DeadGbranch = fdec * 4 * cbiob[i]*kg_C_M2_to_T_ha  # DeadGbranch increasis after 4 years old
        
        # implementar corretamente depois
        # DBranch<-cbiob[i]*kg_C_M2_to_T_ha/5
        
        Deadbranch  = fdec * 3 * DBranch_attached*kg_C_M2_to_T_ha
      }
      
      
      
      #---------------------
      #dead fine roots computation
      Finerootexp   <- Fineroot1 * plai[i]
      Rdecay        <- Rdecay1+ Rdecay2 * (cbior[i]*kg_C_M2_to_T_ha / Finerootexp )
      Deadfineroots <-  Rdecay * cbior[i]*kg_C_M2_to_T_ha
      
      #---------------------
      #dead leaves computation
      
      #there is a double litterfall cause: sapwood area target, and higher fall when higher production
      leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*ztop[1])
      
      if (leaftosapexp>Leafsap2) leaftosapexp <- Leafsap2
      #cm : Sapwoodarea is now directly inferred from mean height, according to experimental observations
      Sapwoodarea   <- Sapheight*ztop[1]
      Leaftosaparea <- plai[i]*10000./Sapwoodarea
      
      Fdecay <- Fdecay1 + Fdecay2*(Leaftosaparea/leaftosapexp)*
        (1+(aleaf[i] * max (0.0, adnpp[i])*kg_C_M2_to_T_ha)/Fdecay3)*
        (1-(max(0,min(water/capac,1)))/Fdecay4)
      
      Deadleaves <- Fdecay * cbiol[i]*kg_C_M2_to_T_ha  #rever dif entre Leaves_Predicted  e Shoot_Predicted
      
      
      #---------------------
      #C Pool update
      #---------------------
      
      
      #computation of SLA of new leaves (expanded)
      Sigmax <- Siginit
      Signew <- Sigmax - (Sigmax-Sigmin) * (ztop[1] - 1.)/(10. - 1.)
      Signew <- min(max(Signew,Sigmin), Sigmax)
      Signew <- Signew * (0.5+0.5*waterfact)
      
      #computation of new LAI from G'DAY  (not using, see bellow that LAI is re-calculated)
      # Deadleaves/Shoot = fraction of leaf mass which turns over; it is assumed that the same fraction of leaf area turns over.
      plai[i] <- plai[i] + (deltay * ((aleaf[i] * max (0.0, adnpp[i]))
                                      * Signew * M2_AS_HA / KG_AS_TONNES / Cfracts  - Deadleaves
                                      * plai[i] / (cbiol[i]+1E-15)))
      
      plai[i] <- max(plai[i],0.01)
      
      
      #  branch decay rate
      #Deadbranch <- DeadGbranch - Deadbranch
      #DBranch<- DBranch + DeadGbranch - Deadbranch
      
      
      
      #C pool update
      cbiow[i] <- cbiow[i] + (awood[i] * max (0.0, adnpp[i])) - (deltay * Deadwood     /kg_C_M2_to_T_ha)
      cbiob[i] <- cbiob[i] + (abranch[i] * max (0.0, adnpp[i])) - (deltay * DeadGbranch   /kg_C_M2_to_T_ha)
      
      
      #    tauleaf_branch <- min(365,365*((4*365)/idpp[i]))
      #    DBranch_decay <- DBranch_attached/tauleaf_branch
      
      if(idpp[i]<=2*365){
        DBranch_decay<-0
      } else if (idpp[i]>2*365 & idpp[i]<=4*365) {
        tauleaf_branch <- (1/280)*(1-exp(-0.0065*(idpp[i]-2*365)))  
        DBranch_decay <- DBranch_attached*tauleaf_branch
      } else {
        DBranch_decay <- DBranch_attached*(1/280)
      }
      
      #        if(idpp[i]<=2*365){
      #          DBranch_decay=0
      #        }else if (idpp[i]>2*365 & idpp[i]<=4*365) {
      #          tauleaf_branch <- (1/365)*(1-exp(-0.005*(idpp[i]-2*365)))
      #          DBranch_decay  <- DBranch_attached*tauleaf_branch
      #        }else{
      #        tauleaf_branch <- (1/365)*(2-exp(-0.005*(idpp[i]-4*365)))
      #        DBranch_decay  <- DBranch_attached*tauleaf_branch
      #        }
      #
      DBranch_attached <- DBranch_attached + (deltay * DeadGbranch   /kg_C_M2_to_T_ha) - DBranch_decay
      
      # DBranch_attached<-DBranch_attached + (deltay * DeadGbranch   /kg_C_M2_to_T_ha) - (deltay * Deadbranch/kg_C_M2_to_T_ha)
      # DBranch_decay <-(deltay * Deadbranch/kg_C_M2_to_T_ha)
      
      cbior[i] <- cbior[i] + (aroot[i] * max (0.0, adnpp[i])) - (deltay * Deadfineroots/kg_C_M2_to_T_ha)
      cbiol[i] <- cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (deltay * Deadleaves   /kg_C_M2_to_T_ha)  #foliar biomass turnover from G'DAY
      #   cbiol[i] = cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (cbiol[i] / tauleaf[i])  #foliar biomass turnover from ECOSMOS (other crops)
      cbiocr[i] <- cbiocr[i] + (acroot[i] * max (0.0, adnpp[i])) - (deltay * Deadcoroots  /kg_C_M2_to_T_ha)
      
      #       print(paste(idpp[i],Signew/ Cfracts,ztop[1],sep=" / "))
      # computation of LAI by the ECOSMOS's standards
      plai[i] <- max(cbiol[i]*Signew/ Cfracts,0.02) # G'DAYS SLA
      # plai[i] = max(cbiol[i]*specla[i],0.02)      # SLA from ECOSMOS (other crops)
      
      
      #  debug_str <- paste(iyear, idpp[i],plai[i],cbior[i]*kg_C_M2_to_T_ha,cbiob[i]*kg_C_M2_to_T_ha,cbiow[i]*kg_C_M2_to_T_ha,cbiocr[i]*kg_C_M2_to_T_ha,
      #                     Finerootexp,(0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf ),
      #                     (nrx*nrn)/(nrn+(nrx-nrn)*waterfact),waterfact,aroot[i],
      #                     Alleafmin+Alleaf1*exp(-Alleaf2*ztop[1]),aleaf[i],
      #                     Branchexp, abranch[i],Corootexp,(0.5 + 0.5 * (1.- (cbiocr[i]*kg_C_M2_to_T_ha) / Corootexp ) / Allocsenscr ),
      #                     acroot[i],awood[i],deltay*Deadwood,deltay*Deadbranch,deltay*Deadfineroots,deltay*Deadleaves,deltay*Deadcoroots,sep=";")
      #  writeLines(debug_str, out_debug)
      
      
      
      #                     NOT IMPLEMENTED
      # #sapwood update
      Sapwoodarea <- Sapheight*ztop[1] # cm2/ha
      sap <- Sapwoodarea * ztop[1] * Cfracts * Density * 0.001 ; #new sapwood mass, tC/ha
      if (sap > cbiow[i]*kg_C_M2_to_T_ha) sap <- cbiow[i]*kg_C_M2_to_T_ha
      # cphw <- cbiow[i]*kg_C_M2_to_T_ha - sap - Heartwood  #increase in heartwood mass (new stem - new sapwood - old heartwood), needed for N alloc
      Sapwood   <- sap
      Heartwood <- cbiow[i]*kg_C_M2_to_T_ha - Sapwood
      
      
      
      biomass[i] <- cbiol[i] + cbiocr[i] + cbior[i] + cbiob[i] + cbiow[i]
      
      # keep track of aboveground annual npp
      ayanpp[i] <- (aleaf[i] + acroot[i] + abranch[i] + awood[i]) * adnpp[i] + ayanpp[i]
      
      
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
      
      if (tmin <= tkill[i]) {
        ccdays[i] <- ccdays[i] + 1
      } else {
        ccdays[i] <- 0
      }
      
      if (ccdays[i] >= 1 &&
          hui[i] >= 0.6 * gddmaturity[i] &&
          croplive[i] == 1) {
        croplive[i]     <- 0.0
        print(paste0('tkill!!!!!',1,iyear,jday,idpp[i]))
        harvdate[i]     <- jday
      }
      
      
      
      #___________________________________________________
      #       Harvest
      
      if(cropy == 1) {
        if ( rm == mxmat[i]/365 ) { # maximum harvest date
          
          Deadfineroots <- cbior[i]
          Deadcoroots   <- cbiocr[i]
          
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
    
    
# Ver com o Michel pq essas variaveis sao definidas mesmo que a planta não estando ativa    
    sapfrac <- Sapwood / (Sapwood + Heartwood)
    
    ztopPft[i] <- (min(plai[i]/3, 1)) * ztopmxPft[i] * min(1,(rm /0.7))
    
  }
  
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
