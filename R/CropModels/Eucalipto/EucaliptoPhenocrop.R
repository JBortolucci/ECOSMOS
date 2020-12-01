

EucaliptoPhenocrop <- function(iyear, iyear0, imonth, iday, jday, index) {
  
  # parametros
  Alleaf1      <- plantList$eucalipto$params$Alleaf1
  Alleaf2      <- plantList$eucalipto$params$Alleaf2
  Alleafinit   <- plantList$eucalipto$params$Alleafinit
  Alleafmin    <- plantList$eucalipto$params$Alleafmin
  Alleafremain <- plantList$eucalipto$params$Alleafremain
  Allocsensb   <- plantList$eucalipto$params$Allocsensb
  Allocsenscr  <- plantList$eucalipto$params$Allocsenscr
  Allocsensf   <- plantList$eucalipto$params$Allocsensf 
  Bdecay       <- plantList$eucalipto$params$Bdecay
  BdecayStart  <- plantList$eucalipto$params$BdecayStart
  Bfall        <- plantList$eucalipto$params$Bfall
  BfallStart   <- plantList$eucalipto$params$BfallStart
  Branch1      <- plantList$eucalipto$params$Branch1
  Branch2      <- plantList$eucalipto$params$Branch2
  Callocb      <- plantList$eucalipto$params$Callocb
  Calloccr     <- plantList$eucalipto$params$Calloccr
  Callocf      <- plantList$eucalipto$params$Callocf
  Cdecay       <- plantList$eucalipto$params$Cdecay
  Cfracts      <- plantList$eucalipto$params$Cfracts
  Coroot1      <- plantList$eucalipto$params$Coroot1
  Coroot2      <- plantList$eucalipto$params$Coroot2
  deltay       <- plantList$eucalipto$params$deltay 
  Density      <- plantList$eucalipto$params$Density
  Fdecay1      <- plantList$eucalipto$params$Fdecay1
  Fdecay2      <- plantList$eucalipto$params$Fdecay2
  Fdecay3      <- plantList$eucalipto$params$Fdecay3
  Fdecay4      <- plantList$eucalipto$params$Fdecay4
  Fineroot1    <- plantList$eucalipto$params$Fineroot1
  Fwpmax       <- plantList$eucalipto$params$Fwpmax
  Fwpmin       <- plantList$eucalipto$params$Fwpmin
  Leafsap1     <- plantList$eucalipto$params$Leafsap1
  Leafsap2     <- plantList$eucalipto$params$Leafsap2
  Leafsap3     <- plantList$eucalipto$params$Leafsap3
  LimLai       <- plantList$eucalipto$params$LimLai
  nrn          <- plantList$eucalipto$params$nrn
  nrx          <- plantList$eucalipto$params$nrx
  Rdecay1      <- plantList$eucalipto$params$Rdecay1
  Rdecay2      <- plantList$eucalipto$params$Rdecay2
  Sapheight    <- plantList$eucalipto$params$Sapheight
  Siginit      <- plantList$eucalipto$params$Siginit
  Sigmin       <- plantList$eucalipto$params$Sigmin
  Wdecay       <- plantList$eucalipto$params$Wdecay
  Ht0          <- plantList$eucalipto$params$Ht0
  Htpower      <- plantList$eucalipto$params$Htpower

  i <- index

  if (croplive[i]==1) {


    huileaf  <- array(0, npft)           # heat unit index needed to attain leaf emergence after planting
    huigrain <- array(0, npft)           # heat unit index needed to reach vegetative maturity
    laidecl  <- matrix(0, 1, npft)       # decline in leaf area for crop
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

    if (croplive[i] == 1.0) {

      huileaf[i]  <- lfemerg[i]  * gddmaturity[i]  # typically between 3 - 7% in wheat
      crmeuca     <- max(73., min((gddmaturity[i]+ 53.683) / 13.882,135.))
      huigrain[i] <- -0.002  * (crmeuca - 73.) + grnfill[i]
      huigrain[i] <- min(max(huigrain[i],grnfill[i] - 0.1), grnfill[i])
      huigrain[i] <- huigrain[i]   * gddmaturity[i]  # from Cabelguenne et al. 1999


      # accumulate growing degree days for planted crops past planting
      gddplant[i] <- gddplant[i] + max(0, min(td - baset[i], mxtmp[i]))
      gddtsoi[i]  <- gddtsoi[i] + max(0, min(tsoi[1] - baset[i], mxtmp[i]))

      greenfrac[i] <- 1.0

      # calculate fraction allocated to leaf (from i. Norman allocation curve)
      # bfact and fleafi are set in params.crp
      fleaf[i] <- fleafi[i] * (exp(-bfact[i]) - exp(-bfact[i] * gddplant[i] / huigrain[i])) / (exp(-bfact[i]) - 1)

      # calculate accumulated growing degree days since planting (gddplant)
      # determine if growing degree days calculated from top layer soil temperature
      # are enough for leaf emergence to occur
      hui[i]     <- gddplant[i]
      leafout[i] <- gddplant[i]
      laidecl[i] <- 0.0
      idpp[i]    <- idpp[i] + 1

      if (leafout[i] >= huileaf[i])   idpe[i] <- idpe[i] + 1

      # crop phenology from leaf emergence to start of leaf decline

      ######################################################################
      ########## Start Allocation to Perenial (Eucalyptus) crops ############
      ######################################################################

      # Phase 1 completed:

      if(idpp[i]==1){
        cbiow[i]  <- 0.0001455428/kg_C_M2_to_T_ha
        cbiob[i]  <- 0.0002444583/kg_C_M2_to_T_ha
        cbior[i]  <- 0.0001482158/kg_C_M2_to_T_ha
        cbiol[i]  <- 0.005065926 /kg_C_M2_to_T_ha
        cbiocr[i] <- 0.000163217 /kg_C_M2_to_T_ha
        plai[i]   <- cbiol[i] * specla[i]  }

      rm          <- min(mxmat[i]/365, idpp[i]/365)
      hsum        <- 0
      water       <- 0
      Wcapac      <- 0
    
      
      for(k in 1:(nsoilay)) {
        
        hsum <- hsum+hsoi[k]
        
        # new michel
        mh_aw <- 16.61*(1-exp(-0.00202 * idpp[i]))^(1.5883) # Maximum root depth Christina et al. (2017)
        mh_aw <- min(mh_aw, sum(hsoi))                      # mh_w can not be greater than the last soil layer

        if(hsum <= mh_aw) {
          Wcapac <- Wcapac + 1000*(1.0     * hsoi[k] *  poros[k] ) *froot[k,1] 
          water  <- water  + 1000*(wsoi[k] * hsoi[k] *  poros[k] ) *froot[k,1] 
        }
      }


      capac <- 30 + 1.2*Wcapac *min(idpp[i]/600,1) #fazer funcao do tempo


      waterfact <-  ((water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin )
      # waterfact <-  gday_c$waterfact

      waterfact <-max(min(waterfact,1),0)

      greenfrac[i] <- 1

      #Fine root C allocation
      Finerootexp <- Fineroot1 * (plai[i]*greenfrac[i])
      
      
      aroot[i] <- (0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf )
      # Finerootexp <- Fineroot1 * (gday_c$lai*greenfrac[i])
      # aroot[i] = (0.5 + 0.5 * (1.- (gday_c$cbior) / Finerootexp ) / Allocsensf )
      aroot[i] <- aroot[i]*(nrx*nrn)/(nrn+(nrx-nrn)*waterfact)
      aroot[i] <- max(min(aroot[i],1),0)


      #---------------------
      #Leaf C allocation
      # ModelLai==2 from Param_eucaflux.h
      # Recalcular o LAI no passo anterior: biomassa de folha input, add o sla
      if (plai[i]<=0.1) {
        aleaf[i] <- Alleafinit # for very small LAI at begining, constant alloc
      } else {
        # leaf allocation fraction is the second priority after fine roots, and has a height (age) constraint
        aleaf[i] <- max(Alleafmin, Alleafmin + Alleaf1 * exp(-Alleaf2 * ztop[1]))
        aleaf[i] <- max(aleaf[i], 1 - aroot[i] - Alleafremain) #second priority after fine root, with 20% kept apart
      }

      aleaf[i]   <-max(min(aleaf[i],1),0)


      #---------------------
      #Branch C allocation
      Branchexp  <- Branch1 * ( plai[i] ^ Branch2 )
      if (Branchexp < 0.)  Branchexp <- 0.001
      abranch[i] <-  (0.5 + 0.5 * (1.- (cbiob[i]*kg_C_M2_to_T_ha) / Branchexp ) / Allocsensb )
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
      Callocfr    <- 1- (Callocf  + Callocb + Calloccr)
      aroot[i]    <- aroot[i]*Callocfr
      aleaf[i]    <- aleaf[i]*Callocf
      abranch[i]  <- abranch[i]*Callocb
      acroot[i]   <- acroot[i]*Calloccr

      if ( (aroot[i] + aleaf[i] + abranch[i] + acroot[i]) > 1 ) {
        reductionfactor <- 1 / (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
        aroot[i]        <- aroot[i]*reductionfactor
        aleaf[i]        <- aleaf[i]*reductionfactor
        abranch[i]      <- abranch[i]*reductionfactor
        acroot[i]       <-acroot[i]*reductionfactor
      }

      awood[i] = 1 - (aroot[i] + aleaf[i] + abranch[i] + acroot[i])

      awood[i]     <- max(0.0, awood[i])
      aroot[i]     <- max(0.0, aroot[i])
      aleaf[i]     <- max(0.0, aleaf[i])
      abranch[i]   <- max(0.0, abranch[i])
      acroot[i]    <- max(0.0, acroot[i])



      # END EUCALYPTUS
      #_____________________________________________

      # keep track of total biomass production for the entire year, and the
      aybprod[i]   <- aybprod[i] +
        aleaf[i]   * max(0.0,adnpp[i]) +
        abranch[i] * max(0.0,adnpp[i]) +
        aroot[i]   * max(0.0,adnpp[i]) +
        awood[i]   * max(0.0,adnpp[i]) +
        acroot[i]  * max(0.0,adnpp[i])

      # aboveground value to calculate harvest index
      ayabprod[i]  <- ayabprod[i] +
        aleaf[i]   * max(0.0,adnpp[i]) +
        abranch[i] * max(0.0,adnpp[i]) +
        awood[i]   * max(0.0,adnpp[i])


      # keep track of annual total root production carbon
      ayrprod[i]  <- ayrprod[i] +
        aroot[i]  * max(0.0,adnpp[i]) +
        acroot[i] * max(0.0,adnpp[i])


      # keep track of total carbon allocated to
      # leaves for litterfall calculation
      aylprod[i] <- aylprod[i] +
        aleaf[i] * max (0.0, adnpp[i])


      # ---------------------
      # Mortality - Litterfall C fluxes
      # ---------------------
      #dead stem computation
      if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        # fdec         <- exp(2.*log(2.) * (rm - BdecayStart)) - 1
        # fdec         <- max(min(fdec,1), 0)
        # fdec         <- 2 * exp(log(1.4) * (rm - BdecayStart))
        fdec         <- exp(log(1.3) * (rm - BdecayStart))
        fdec         <- max(min(fdec, 20), 0)
        Deadwood     <- fdec * Wdecay * Sapwood # Stock of dead branch that do not fall into the ground. It is assumed to start at 3 years old
        Deadcoroots  <- fdec * Cdecay * cbiocr[i] * kg_C_M2_to_T_ha  # cm
      }
      
      # ---------------------
      # Dead branches computation
      ###Michel : Nov. 2020
      if (rm <= BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        DeadGbranch <- 0
        Deadbranch  <- 0
        
      } else if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        fdec        <- exp(0.2*(rm-BdecayStart))-1
        fdec        <- max(min(fdec,1),0)#*0.3
        DeadGbranch <- fdec * Bdecay * cbiob[i]*kg_C_M2_to_T_ha # Original Bdecay = 0.24839385
        # DeadGbranch <- fdec * 0.7 * cbiob[i]*kg_C_M2_to_T_ha
        
      }
      
      if ( rm > BfallStart ) {
        #beginning of the dead branches fall
        fdec        <- exp(0.2 * (rm-BfallStart)) - 1
        fdec        <- max(min(fdec, 1), 0)#*0.3
        Deadbranch  <- fdec * Bfall * DBranch*kg_C_M2_to_T_ha   # Dead branch that falls into the ground and entered the above-ground structural litter pool
        # Deadbranch  <- fdec * 0.24 * DBranch * kg_C_M2_to_T_ha   # Dead branch that falls into the ground and entered the above-ground structural litter pool . Bfall original: 0.786798096
        Deadbranch  <- max(Deadbranch,0)

      }
      
      # if (rm > 4 ) {
      #   # After 4.5 years, both branch death and branch fall increase considerably ==> empirical correction
      #   # fdec        <- exp(2*log(2)*(rm-4))-1
      #   fdec        <- exp(0.2*(rm-4))-1
      #   fdec        <- max(min(fdec,1),0)
      #   # DeadGbranch <- fdec * 4 * cbiob[i]*kg_C_M2_to_T_ha  # DeadGbranch increasis after 4 years old
      #   # Deadbranch  <- fdec * 3 * DBranch*kg_C_M2_to_T_ha
      #   DeadGbranch <- fdec * 0.4 * cbiob[i]*kg_C_M2_to_T_ha  # DeadGbranch increasis after 4 years old
      #   Deadbranch  <- fdec * 0.3 * DBranch*kg_C_M2_to_T_ha
      #   Deadbranch  <- max(Deadbranch,0)
      #   # print(paste("teste-4",fdec,DBranch*kg_C_M2_to_T_ha,DeadGbranch,Deadbranch,sep="/"))
      # 
      # }
      
      
      DBranch     <- max((DeadGbranch-Deadbranch),0)

      # print(paste(DeadGbranch,DBranch,Deadbranch,sep="///"))

      # Deadbranch  : dead branch on the ground
      # deadGbranch : intermediary pool, the dead branches that are attached in the trees
      # DBranch     : updated pool of attached dead branches. This start before the attached dead brances start to fall into the ground

      
      #---------------------
      #dead fine roots computation
      #---------------------
      Finerootexp   <- Fineroot1 * plai[i]
      Rdecay        <- Rdecay1+ Rdecay2 * (cbior[i]*kg_C_M2_to_T_ha / Finerootexp )
      Deadfineroots <- Rdecay * cbior[i]*kg_C_M2_to_T_ha
      
      #---------------------
      #dead leaves computation
      #---------------------
      #there is a double litterfall cause: sapwood area target, and higher fall when higher production
      leaftosapexp <- Leafsap1 + Leafsap2 * exp(-Leafsap3 * ztop[1])
      
      if (leaftosapexp>Leafsap2) leaftosapexp <- Leafsap2
      #cm : Sapwoodarea is now directly inferred from mean height, according to experimental observations
      Sapwoodarea   <- Sapheight * ztop[1]
      Leaftosaparea <- plai[i] * 10000 / Sapwoodarea
      
      Fdecay <- Fdecay1 + Fdecay2*(Leaftosaparea/leaftosapexp)*
        (1+(aleaf[i] * max (0.0, adnpp[i]) * kg_C_M2_to_T_ha)/Fdecay3)*
        (1-(max(0,min(water/capac,1))) / Fdecay4)
      
      Deadleaves <- Fdecay * cbiol[i] * kg_C_M2_to_T_ha  #rever dif entre Leaves_Predicted  e Shoot_Predicted
      
      
      #---------------------
      #C Pool update
      #---------------------
      
      
      #computation of SLA of new leaves (expanded)
      Sigmax <- Siginit
      Signew <- Sigmax - (Sigmax-Sigmin) * (ztop[1] - 1.)/(10 - 1.)
      # Signew <- Sigmax - (Sigmax-8) * (ztop[1] - 1)/(10 - 1)
      Signew <- min(max(Signew,Sigmin), Sigmax)
      Signew <- Signew * (0.5+0.5 * waterfact)
      
      print(paste(Signew,ztop[1],sep="//"))
      
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
      cbiob[i] <- cbiob[i] + (abranch[i] * max (0.0, adnpp[i])) - (deltay * Deadbranch   /kg_C_M2_to_T_ha)
      cbior[i] <- cbior[i] + (aroot[i] * max (0.0, adnpp[i])) - (deltay * Deadfineroots/kg_C_M2_to_T_ha)
      cbiol[i] <- cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (deltay * Deadleaves   /kg_C_M2_to_T_ha)  #foliar biomass turnover from G'DAY
      #   cbiol[i] = cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (cbiol[i] / tauleaf[i])  #foliar biomass turnover from ECOSMOS (other crops)
      cbiocr[i] <- cbiocr[i] + (acroot[i] * max (0.0, adnpp[i])) - (deltay * Deadcoroots  /kg_C_M2_to_T_ha)
      plai[i] <- max(cbiol[i]*Signew/ Cfracts,0.02) # G'DAYS SLA
      # plai[i] = max(cbiol[i]*specla[i],0.02)      # SLA from ECOSMOS (other crops)
      
      
      #                     NOT IMPLEMENTED
      # #sapwood update
      Sapwoodarea <- Sapheight * ztop[1] # cm2/ha
      sap         <- Sapwoodarea * ztop[1] * Cfracts * Density * 0.001 ; #new sapwood mass, tC/ha
      
      if (sap > cbiow[i]*kg_C_M2_to_T_ha) {
        
        sap <- cbiow[i]*kg_C_M2_to_T_ha
        
      }
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
    
    sapfrac <- Sapwood / (Sapwood + Heartwood)
    
    # ztopPft[i] <- (min(plai[i]/3, 1)) * ztopmxPft[i] * min(1,(rm /0.7))
    ztopPft[i] <- Ht0 * (cbiow[i]*kg_C_M2_to_T_ha)^(Htpower)
    
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
  assign("DeadGbranch",DeadGbranch    , envir = env)
  assign("Deadfineroots",Deadfineroots , envir = env)
  assign("Deadleaves",Deadleaves    , envir = env)
  assign("Deadcoroots",Deadcoroots   , envir = env)
  
  assign("cbiold",cbiold, envir = env)
  assign("cbiols",cbiols, envir = env)

  assign("Sapwood",Sapwood  , envir = env)
  assign("Heartwood",Heartwood, envir = env)

  # assign("DBranch_attached",DBranch_attached, envir = env)
  assign("DBranch_decay",DBranch_decay, envir = env)
  assign("Signew",Signew, envir = env)
  assign("waterfact",waterfact, envir = env)
  
  
}
