

EucaliptoPhenocrop <- function(iyear, iyear0, imonth, iday, jday, index) {
  
  # parametros
  Alleaf1      <- plantList[[index]]$params$Alleaf1
  Alleaf2      <- plantList[[index]]$params$Alleaf2
  Alleafinit   <- plantList[[index]]$params$Alleafinit
  Alleafmin    <- plantList[[index]]$params$Alleafmin
  Alleafremain <- plantList[[index]]$params$Alleafremain
  Allocsensb   <- plantList[[index]]$params$Allocsensb
  Allocsenscr  <- plantList[[index]]$params$Allocsenscr
  Allocsensf   <- plantList[[index]]$params$Allocsensf 
  Bdecay       <- plantList[[index]]$params$Bdecay
  BdecayStart  <- plantList[[index]]$params$BdecayStart
  Bfall        <- plantList[[index]]$params$Bfall
  BfallStart   <- plantList[[index]]$params$BfallStart
  Branch1      <- plantList[[index]]$params$Branch1
  Branch2      <- plantList[[index]]$params$Branch2
  Callocb      <- plantList[[index]]$params$Callocb
  Calloccr     <- plantList[[index]]$params$Calloccr
  Callocf      <- plantList[[index]]$params$Callocf
  Cdecay       <- plantList[[index]]$params$Cdecay
  Cfracts      <- plantList[[index]]$params$Cfracts
  Coroot1      <- plantList[[index]]$params$Coroot1
  Coroot2      <- plantList[[index]]$params$Coroot2
  deltay       <- plantList[[index]]$params$deltay 
  Density      <- plantList[[index]]$params$Density
  Fdecay1      <- plantList[[index]]$params$Fdecay1
  Fdecay2      <- plantList[[index]]$params$Fdecay2
  Fdecay3      <- plantList[[index]]$params$Fdecay3
  Fdecay4      <- plantList[[index]]$params$Fdecay4
  Fineroot1    <- plantList[[index]]$params$Fineroot1
  Fwpmax       <- plantList[[index]]$params$Fwpmax
  Fwpmin       <- plantList[[index]]$params$Fwpmin
  Leafsap1     <- plantList[[index]]$params$Leafsap1
  Leafsap2     <- plantList[[index]]$params$Leafsap2
  Leafsap3     <- plantList[[index]]$params$Leafsap3
  LimLai       <- plantList[[index]]$params$LimLai
  nrn          <- plantList[[index]]$params$nrn
  nrx          <- plantList[[index]]$params$nrx
  Rdecay1      <- plantList[[index]]$params$Rdecay1
  Rdecay2      <- plantList[[index]]$params$Rdecay2
  Sapheight    <- plantList[[index]]$params$Sapheight
  Siginit      <- plantList[[index]]$params$Siginit
  Sigmin       <- plantList[[index]]$params$Sigmin
  Wdecay       <- plantList[[index]]$params$Wdecay
  Ht0          <- plantList[[index]]$params$Ht0
  Htpower      <- plantList[[index]]$params$Htpower
  
  i <- index
  
  if (croplive[i]==1) {
    
    

#! available inorganic nitrogen in soil profile for plant growth (kg_n m-2 y-1)     
    aplantn <- 0.0
    
    for(k in 1:nsoilay) {
      aplantn <- aplantn + smsoil[k] + smsoln[k]
    }
    
    
    # Eucalyptus phenology, Carbon Aloccation and Harvest
    
    if (croplive[i] == 1.0) {
      

      # accumulate growing degree days for planted crops past planting
      gddplant[i] <- gddplant[i] + max(0, min(td - baset[i], mxtmp[i]))

      pgreenfrac[i] <- 1.0
      
      idpp[i]    <- idpp[i] + 1
      

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
        plai[i]   <- cbiol[i] * specla[i]  
        plai[i]   <- max(plai[i],0.01)
        cbiol[i]  <- plai[i]/specla[i]    
        
        }
      

        
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
      
      pgreenfrac[i] <- 1
      
      ########################################################################################
      #############    ## ### ####     ##    ##     ##     ## ##     ## #### #################
      ############# ## ## ### #### ### ## ##### ### #### #### ## ### ## # ## #################
      #############    ## ### #### ### ## #####     #### #### ## ### ## ## # #################
      ############# ## ##   #   ##     ##    ## ### #### #### ##     ## ###  #################
      ########################################################################################
      #---------------------
      #Fine root C allocation
      #---------------------
      Finerootexp <- Fineroot1 * (plai[i]*pgreenfrac[i])
      
      aroot[i] <- (0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf )
      aroot[i] <- aroot[i]*(nrx*nrn)/(nrn+(nrx-nrn)*waterfact)
      aroot[i] <- max(min(aroot[i],1),0)
      
      
      #---------------------
      #Leaf C allocation
      #---------------------
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
      #---------------------
      Branchexp  <- Branch1 * ( plai[i] ^ Branch2 )
      
      if (Branchexp < 0.)  Branchexp <- 0.001
      abranch[i] <-  (0.5 + 0.5 * (1.- (cbiob[i]*kg_C_M2_to_T_ha) / Branchexp ) / Allocsensb )

      abranch[i]<-max(min(abranch[i],1),0)
      
      
      #---------------------
      # Coarse Root C allocation
      #---------------------
      Corootexp <- Coroot1 * ( (cbiow[i]*kg_C_M2_to_T_ha) ^ Coroot2 )
      
      if (Corootexp < 0.) Corootexp = 0.001
      acroot[i] = (0.5 + 0.5 * (1.- (cbiocr[i]*kg_C_M2_to_T_ha) / Corootexp ) / Allocsenscr )
      acroot[i]<-max(min(acroot[i],1),0)
      
      
      #---------------------
      #Stem (G'DAY) C allocation or Wood (ECOSMOS)
      #---------------------
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
      
      awood[i] <- 1 - (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
      
      awood[i]     <- max(0.0, awood[i])
      aroot[i]     <- max(0.0, aroot[i])
      aleaf[i]     <- max(0.0, aleaf[i])
      abranch[i]   <- max(0.0, abranch[i])
      acroot[i]    <- max(0.0, acroot[i])
      
      
    
      
      ###############################################################################
      ######################    ###    ##    ##     ## #### #########################
      ###################### ### ## ##### ##### ### ### ## ##########################
      ###################### ### ##    ## ##### ### #### ############################
      ###################### ### ## ##### #####     #### ############################
      ######################    ###    ##    ## ### #### ############################
      ###############################################################################
      # ---------------------
      # Mortality - Litterfall C fluxes
      # ---------------------
      #dead stem computation
      if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        # fdec         <- exp(2.*log(2.) * (rm - BdecayStart)) - 1
        # fdec         <- max(min(fdec,1), 0)
        # fdec         <- 2 * exp(log(1.4) * (rm - BdecayStart))
        fdec         <- exp(log(1.27) * (rm - BdecayStart))
        fdec         <- max(min(fdec, 20), 0)
        Deadwood     <- fdec * Wdecay * Sapwood # Stock of dead branch that do not fall into the ground. It is assumed to start at 3 years old
        Deadcoroots  <- fdec * Cdecay * cbiocr[i] * kg_C_M2_to_T_ha  # cm
        
      }
      
      # ---------------------
      # Dead branches computation
      # ---------------------
      ### Michel : New implementation at Nov-2020
      # Deadbranch  : dead branch on the ground
      # deadGbranch : intermediary pool, the dead branches that are attached in the trees
      # DBranch     : updated pool of attached dead branches. This start before the attached dead brances start to fall into the ground
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

      DBranch     <- max((DeadGbranch-Deadbranch),0)
      # print(paste(DeadGbranch,DBranch,Deadbranch,sep="///"))
      
      
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
      
      
      #######################################################################
      ########################     ### #####     ############################
      ######################## ####### ##### ### ############################
      #########################   #### #####     ############################
      ########################### #### ##### ### ############################
      ########################    ####    ## ### ############################
      #######################################################################
      #---------------------
      ### Computation of SLA of new leaves (expanded)
      #---------------------
      Sigmax <- Siginit
      Signew <- Sigmax - (Sigmax-Sigmin) * (ztop[1] - 1.)/(10 - 1.)
      # Signew <- Sigmax - (Sigmax-8) * (ztop[1] - 1)/(10 - 1)
      Signew <- min(max(Signew,Sigmin), Sigmax)
      Signew <- Signew * (0.5+0.5 * waterfact)
      
      # print(paste(Signew,ztop[1],sep="//"))
      
      #computation of new LAI from G'DAY  (not using, see bellow that LAI is re-calculated)
      # Deadleaves/Shoot = fraction of leaf mass which turns over; it is assumed that the same fraction of leaf area turns over.
     # plai[i] <- plai[i] + (deltay * ((aleaf[i] * max (0.0, adnpp[i]))
     #                                 * Signew * M2_AS_HA / KG_AS_TONNES / Cfracts  - Deadleaves
     #                                 * plai[i] / (cbiol[i]+1E-15)))
      plai[i]    <- (cbiol[i] * specla[i])
      
      plai[i] <- max(plai[i],0.01)
      
      
      #########################################################################
      ############## ### ##    ####   ####     ##     ##     ##################
      ############## ### ## ### ### ##  ## ### #### #### ######################
      ############## ### ##    #### ### ##     #### ####     ##################
      ############## ### ## ####### ##  ## ### #### #### ######################
      ##############     ## #######   #### ### #### ####     ##################
      #########################################################################
      #---------------------
      #C Pool update
      #---------------------
      cbiow[i]  <- cbiow[i] + (awood[i]    * max (0.0, adnpp[i]))  - (deltay * Deadwood      / kg_C_M2_to_T_ha)
      cbiob[i]  <- cbiob[i] + (abranch[i]  * max (0.0, adnpp[i]))  - (deltay * Deadbranch    / kg_C_M2_to_T_ha)
      cbior[i]  <- cbior[i] + (aroot[i]    * max (0.0, adnpp[i]))  - (deltay * Deadfineroots / kg_C_M2_to_T_ha)
      cbiol[i]  <- cbiol[i] + (aleaf[i]    * max (0.0, adnpp[i]))  - (deltay * Deadleaves    / kg_C_M2_to_T_ha)
      cbiocr[i] <- cbiocr[i] + (acroot[i] * max (0.0, adnpp[i]))  - (deltay * Deadcoroots   / kg_C_M2_to_T_ha)
      plai[i]   <- max(cbiol[i]*Signew/ Cfracts,0.02)
      
      
      #---------------------
      # Sapwood update
      #---------------------
      Sapwoodarea <- Sapheight * ztop[1] # cm2/ha
      sap         <- Sapwoodarea * ztop[1] * Cfracts * Density * 0.001 ; #new sapwood mass, tC/ha
      
      if (sap > cbiow[i]*kg_C_M2_to_T_ha) {
        
        sap <- cbiow[i]*kg_C_M2_to_T_ha
        
      }
      Sapwood   <- sap
      Heartwood <- cbiow[i]*kg_C_M2_to_T_ha - Sapwood
      
      
  
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
        pstart[j] <- 0     
        croplive[i]     <- 0.0
        print(paste0('tkill!!!!!',1,iyear,jday,idpp[i]))
        harvdate[i]     <- jday
      }
      
      
      #####################################################################
      ######## ### ##     ##    ##   ####  ##    ###     ###      #########
      ######## ### ## ### ## ## ###  ###  ### ###### ######### ############
      ########     ##     ##   #####  #  ####    ###     ##### ############
      ######## ### ## ### ## ## #####   ##### #########  ##### ############
      ######## ### ## ### ## ## ###### ######    ###    ###### ############
      #####################################################################
      #------------------------
      ### Harvest
      #------------------------
      
      if(cropy == 1) {
        if ( rm == mxmat[i]/365 ) { # maximum harvest date
          
          Deadfineroots <- cbior[i]
          Deadcoroots   <- cbiocr[i]
          
          croplive[i]   <- 0.0
          pgreenfrac[i]  <- 0.0 # turn all vegetation to brown
          harvdate[i] <- jday
          plai[i]         <- 0.01 # simulates remaining stubble/mulch
          endCycle[i] <- T
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
  assign("pgreenfrac", pgreenfrac, envir = env)
  assign("idpp", idpp, envir = env)
  assign("awood", awood, envir = env)
  assign("aleaf", aleaf, envir = env)
  assign("acroot", acroot, envir = env)
  assign("aroot", aroot, envir = env)
  assign("abranch", abranch, envir = env)
  assign("plai", plai, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiocr", cbiocr, envir = env)
  assign("cbiob", cbiob, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
   assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("Deadwood",Deadwood      , envir = env)
  assign("Deadbranch",Deadbranch    , envir = env)
  assign("DBranch",DBranch    , envir = env)
  assign("DeadGbranch",DeadGbranch    , envir = env)
  assign("Deadfineroots",Deadfineroots , envir = env)
  assign("Deadleaves",Deadleaves    , envir = env)
  assign("Deadcoroots",Deadcoroots   , envir = env)
  assign("Sapwood",Sapwood  , envir = env)
  assign("Heartwood",Heartwood, envir = env)
  assign("DBranch_decay",DBranch_decay, envir = env)
  assign("Signew",Signew, envir = env)
  assign("waterfact",waterfact, envir = env)
  
}
