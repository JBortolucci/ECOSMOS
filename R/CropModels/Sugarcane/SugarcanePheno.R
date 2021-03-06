

SugarcanePheno <- function(year, iyear0, month, day, jday, index) {
  
  nratoon  <- plantList$sugarcane$params$nratoon
  rootd    <- plantList$sugarcane$params$rootd
  sf1      <- plantList$sugarcane$params$sf1
  ipf1     <- plantList$sugarcane$params$ipf1
  ecf2     <- plantList$sugarcane$params$ecf2
  ipf2     <- plantList$sugarcane$params$ipf2
  sf3      <- plantList$sugarcane$params$sf3
  ipf3     <- plantList$sugarcane$params$ipf3
  ecf4     <- plantList$sugarcane$params$ecf4
  ipf4     <- plantList$sugarcane$params$ipf4
  ecf5     <- plantList$sugarcane$params$ecf5
  tf5      <- plantList$sugarcane$params$tf5
  wf5      <- plantList$sugarcane$params$wf5
  ecf6     <- plantList$sugarcane$params$ecf6
  ipf6     <- plantList$sugarcane$params$ipf6
  laidc    <- plantList$sugarcane$params$laidc
  ldf      <- plantList$sugarcane$params$ldf
  tmld     <- plantList$sugarcane$params$tmld
  ecf7     <- plantList$sugarcane$params$ecf7
  firecane <- plantList$sugarcane$params$firecane
  
  j <- index
  
  if (croplive[j]==1) {
    
    
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
    
    # vernalization(year, month, day, jday)
    
    # begin global grid
    
    
    aplantn <- 0.0
    
    for(k in 1:nsoilay) {
      aplantn <- aplantn + smsoil[k] + smsoln[k]
    }
    
    
    # Sugarcane phenology, Carbon Aloccation and Harvest
    
    if (j ==j ) {
      
      
      if (croplive[j] == 1.0) {
        
        if(cropy == 1) {
          huileaf[j]  <- lfemerg[j]  * gddmaturity[j] 
        } else {
          huileaf[j]  <- lfemerg[j] * (1. / 6.)* gddmaturity[j]
        }
        
        
        crmsgc       <- max(73., min((gddmaturity[j]+ 53.683) / 13.882,135.))
        huigrain[j] <- -0.002  * (crmsgc - 73.) + grnfill[j]
        huigrain[j] <- min(max(huigrain[j],grnfill[j] - 0.1), grnfill[j]) 
        huigrain[j] <- huigrain[j]   * gddmaturity[j]  # from Cabelguenne et al. 1999
        
        
        
        # accumulate growing degree days for planted crops past planting
        gddplant[j] <- gddplant[j] + max(0, min(td - baset[j], mxtmp[j]))
        gddtsoi[j] <- gddtsoi[j] + max(0, min(tsoi[1] - baset[j], mxtmp[j]))
        
        
        greenfrac[j] <- 1.0  
        
        # calculate fraction allocated to leaf (from J. Norman allocation curve)
        # bfact and fleafi are set in params.crp
        fleaf[j] <- fleafi[j] * (exp(-bfact[j]) - exp(-bfact[j] * gddplant[j] / huigrain[j])) / (exp(-bfact[j]) - 1)  
        
        # calculate accumulated growing degree days since planting (gddplant) 
        # determine if growing degree days calculated from top layer soil temperature
        # are enough for leaf emergence to occur 
        hui[j] <- gddplant[j] 
        leafout[j]   <- gddplant[j]
        
        laidecl[j] <- 0.0
        
        idpp[j] <- idpp[j] + 1
        
        if (leafout[j] >= huileaf[j])   idpe[j] <- idpe[j] + 1
        
        #_____________________________________________________________            
        # crop phenology from leaf emergence to start of leaf decline   
        
        if (leafout[j] < huileaf[j]) {
          gddemerg    <- 0.0
          awood[j]    <- 0.0
          aroot[j]    <- 0.0
          aerial[j]   <- 0.0
          aleaf[j]    <- 0.0
          astem[j]    <- 0.0
          arepr[j]    <- 0.0
          rm          <- 0.0 
        } else if (leafout[j] >= huileaf[j]) {
          
          # Phase 1 completed: Emergence
          
          
          awood[j]  <- 0.0
          aroot[j]  <- 0.00001
          aerial[j] <- 0.00001
          aleaf[j]  <- 0.00001
          astem[j]  <- 0.00001
          arepr[j]  <- 0.00001
          
          rm <- min(100., 100. * (gddplant[j] - gddemerg) / gddmaturity[j] )
          
          # scheme based on CANEGRO model (Singels et al. 2005) 
          # if sugarcane is planted, it takes long to construct the 
          # root sistem, if ratoon leafs develop faster 
          if(cropy == 1) {
            aerial[j] <- (1 - arootf[j]) * min(1.0, ( 1 - exp(-rootd * 0.2 * rm))) 
          } else {
            aerial[j] <- (1 - arootf[j]) * min(1.0, ( 1 - exp(-rootd * rm))) 
          }
          
          
          aroot[j] <- 1. - aerial[j]
          af1 <- max(0.0, rm * sf1 - sf1 * ipf1)
          af2 <- max(0.0, 1.0 - (exp( - (ecf2 * rm - ecf2 * ipf2))))
          astem[j] <- aerial[j] * min(1.0, max(af1,af2))
          
          # make sure that aleaf[j] is at least == to aleaff[j] 
          astem[j] <- min( max(0.,aerial[j] - aleaff[j]) ,astem[j] ) 
          aleaf[j] <- aerial[j] - astem[j]
          
          ################ leaf / Stalk allocation ################
          # Adjust the leaf / Stalk allocation in function os 
          # Temperature (physiological effect)
          #      astem[j] <- astem[j] + 
          #        min(aleaf[j], 
          #            (astem[j] / aerial[j]) * ldf * aleaf[j] * 
          #              min(1.,exp(tmld * ecf7) / exp((td - 273.16) * ecf7) ) )
          
          if(plai[j] * greenfrac[j] < 2 && rm < 75 && rm > 10) {
            #if green lai < 2. then aleaf is equal to 0.5 
            astem[j] <- min( max(0.,aerial[j] - 0.5) ,astem[j] ) 
          } else {
            astem[j] <- min( max(0.,aerial[j] - aleaff[j]) ,astem[j] ) 
          }
          
          aleaf[j] <- aerial[j] - astem[j]
          
          #        print(paste(year,jday,idpp[j],rm,aleaf[j],aerial[j],astem[j],plai[j],sep=" / "))        
          
          ################ leaf / Stalk allocation ################
          
          # For sugarcane astem is the structural carbon, 
          # and arepr is the sucrose carbon
          sipf3 <- ipf1 + (100. - ipf1) * (ipf3 / 100.)
          af3 <- max(0.0, rm * sf3 - sf3 * sipf3)
          
          sipf4 <- ipf1 + (100. - ipf1) * (ipf4 / 100.)
          af4 <- max(0.0, 1.0 - ( exp( - (ecf4 * rm - ecf4 * sipf4))))
          arepr[j] <- astem[j] * min(1., max(af3,af4))
          arepr[j] <- min(aerial[j] - aleaf[j], arepr[j] )
          astem[j] <- astem[j] -  arepr[j] 
          
          # Adjust the Sucrose / Stalk allocation in function os 
          # Temperature (physiological effect)
          af5 <- min(1., max(0., 1. - (exp((td - 273.16) * ecf5) / exp(tf5 * ecf5)))) + 
            min(0., min(0., (exp(tf5 * ecf5) / exp((td - 273.16) * ecf5)) - 1))
          
          sipf6 <- sipf4 + (100. - sipf4) * (ipf6 / 100.)
          af6 <- max(0.0, 1.0- ( exp(ecf6 * sipf6) / exp(ecf6 * rm) ) )
          
          ccf5 <- arepr[j]
          arepr[j] <- arepr[j] + astem[j] * wf5 * af5 * af6
          arepr[j] <- min(aerial[j] - aleaf[j], arepr[j] )
          astem[j] <- astem[j] - (arepr[j] - ccf5)
          
          ############################################################
          # STAR of leaf adjust  -> adjust aleaf; check for LAI max 
          # and apply the leaf turn over
          ############################################################
          
          # calculate actual lai increase based on npp and allocation rules in ibis 
          # only increment lai if it hasn't reached maximum allowable value 
          
          leaftemp <- aleaf[j]
          
          tlai[j] <- plai[j] + (specla[j] * aleaf[j] * max(0.0, adnpp[j]))
          
          
          if (tlai[j] >= laimx[j]) {
            aleaf[j] <- min(aleaf[j], (laimx[j] - plai[j]) / (specla[j] * adnpp[j]))
            
            aleaf[j] <- max(0.0, aleaf[j])
            
            aroot[j] <- aroot[j] + ((leaftemp - aleaf[j]) * aroot[j] / (astem[j] + arepr[j] + aroot[j]))
            astem[j] <- astem[j] + ((leaftemp - aleaf[j]) * astem[j] / (astem[j] + arepr[j] + aroot[j]))
            arepr[j] <- arepr[j] + ((leaftemp - aleaf[j]) * arepr[j] / (astem[j] + arepr[j] + aroot[j]))
          }
          
          aroot[j] <- max(0.0, aroot[j])
          aleaf[j] <- max(0.0, aleaf[j])
          astem[j] <- max(0.0, astem[j])
          arepr[j] <- max(0.0, arepr[j])
          
          # keep day of year that sucrose (grain fill) begins
          if(arepr[j] > 0.001 && grainday[j] > 999)
            grainday[j] <- min(grainday[j], jday)
          
          templai[j] <- (cbiol[j] * specla[j])
          
          plai[j] <- (cbiol[j] * specla[j]) - (cbiol[j] * specla[j]) * ( (1. / tauleaf[j]) )
          
          #  test the APSIM (parametrization) - lai declines linear for temp lower than 10C till zero if td = 0C
          if(td <= 278.16 && td >= 268.16) {
            print(paste0('td[i] <    5 C',year,jday,i,td - 273.16,plai[j]))
            plai[j] <- plai[j]* max(0.4,min(1.,0.5 + ((td - 268.16) / 20.) )) # let at least 10%
            print(paste0('plai reduction',year,jday,1,max(0.4,min(1.,0.5 + ((td - 268.16) / 20.))),plai[j]))
          } else if(td < 268.16) {
            plai[j] <- 0.01
            print(paste0('temp < -5, sugarcane die (from APSIM) ',year,jday,1))
          }
          
          plai[j] <- max(0.01,plai[j])
          
          # calculate decrease in lai for purpose of updating 
          # aboveground biomass pools
          laidecl[j] <- max(0.0, templai[j] - plai[j])
          
          ############################################
          ############ END of leaf adjusts ###########
          ############################################
        }
        
        
        # keep track of total biomass production for the entire year, and the
        aybprod[j] <- aybprod[j] + 
          aleaf[j] * max(0.0,adnpp[j]) +
          astem[j] * max(0.0,adnpp[j]) +
          aroot[j] * max(0.0,adnpp[j]) +
          awood[j] * max(0.0,adnpp[j]) +
          arepr[j] * max(0.0,adnpp[j])
        
        
        # aboveground value to calculate harvest index
        ayabprod[j] <- ayabprod[j] +
          aleaf[j] * max(0.0,adnpp[j]) +
          astem[j] * max(0.0,adnpp[j]) +
          arepr[j] * max(0.0,adnpp[j]) +
          awood[j] * max(0.0,adnpp[j]) 
        
        # keep track of annual total root production carbon
        ayrprod[j] <- ayrprod[j] +
          aroot[j] * max(0.0,adnpp[j])
        
        # keep track of total carbon allocated to 
        # leaves for litterfall calculation
        aylprod[j] <- aylprod[j] + 
          aleaf[j] * max (0.0, adnpp[j])
        
        cbiow[j] <- max(0.0, cbiow[j]) 
        cbior[j] <- max(0.0, cbior[j]) 
        cbiol[j] <- max(0.0, cbiol[j])
        cbios[j] <- max(0.0, cbios[j])
        cbiog[j] <- max(0.0, cbiog[j])  
        
        # update carbon reservoirs using an analytical solution
        # to the original carbon balance differential equation
        
        cbiol[j] <- cbiol[j] + aleaf[j] * max (0.0, adnpp[j]) -
          (laidecl[j] / specla[j])
        
        cbiog[j] <- cbiog[j] + arepr[j] * max (0.0, adnpp[j])
        
        cbios[j] <- cbios[j] + astem[j] * max (0.0, adnpp[j])
        
        fallrsgc[3] <- cbior[j] + aroot[j] * max(0.0,adnpp[j])
        
        cbior[j] <- cbior[j] * exp(-1.0 / tauroot[j]) + 
          aroot[j] * tauroot[j] * max(0.0,adnpp[j]) * (1.0 - exp(-1.0 / tauroot[j]))
        
        fallrsgc[3] <- fallrsgc[3] - cbior[j]
        
        
        # update vegetation's physical characteristics
        plai[j] <- cbiol[j] * specla[j]
        
        
        # sencon try as function of GDD, age and self-shade
        if( rm > 1) {
          plai[j] <- cbiol[j] * specla[j] + (aylprod[j]- cbiol[j] ) * specla[j] * 0.15 
          greenfrac[j] <- cbiol[j] * specla[j] / plai[j] 
        }
        
        biomass[j] <- cbiol[j] + cbiog[j] + cbior[j] + cbios[j] + cbiow[j]
        
        # keep track of aboveground annual npp 
        ayanpp[j] <- (aleaf[j] + arepr[j] + astem[j] + awood[j]) * adnpp[j] + ayanpp[j]
        
        
        #####################################################################
        # check for climatic and phenological limits on maturity, growth, 
        # and harvest date
        #
        # check to see if minimum temperature has fallen below freeze
        # kill threshold for 3 consecutive days and if lai is above a minimum, plant will
        # be damaged/killed.  This function is more for spring freeze events
        # or for early fall freeze events
        
        if (tmin <= tkill[j]) {
          ccdays[j] <- ccdays[j] + 1
        } else {
          ccdays[j] <- 0
        }
        
        if (ccdays[j] >= 1 && hui[j] >= 0.6 * gddmaturity[j] && croplive[j] == 1) {
          croplive[j]     <- 0.0
          print(paste0('tkill!!!!!',1,year,jday,idpp[j]))
          harvdate[j]     <- jday
        }
        
        
        
        #___________________________________________________
        #       Harvest  sugarcane
        
        # planted
        if(cropy == 1) {
#          if ( (hui[j] >= gddmaturity[j]) && (idpp[j] >= mxmat[j] - 15) || 
            if ( (hui[j] >= gddmaturity[j]) && (idpp[j] >= mxmat[j] - 15) || 
               ( idpp[j] >= mxmat[j] +15) ||
               year==2016 && jday== 303   ) { # maximum harvest date
            
            croplive[j]     <- 0.0
            greenfrac[j]  <- 0.0 # turn all vegetation to brown
            if (harvdate[j] == 999) harvdate[j] <- jday 
            plai[j]         <- 0.25 # simulates remaining stubble/mulch
            print(paste('Sug.Cane - 1st cycle = ',cropy, year, jday, idpp[j], hui[j], gddmaturity[j]))
          }
        } else {
          # ratoon
          if(((hui[j] >= gddmaturity[j]) && (idpp[j] >= 365) ) ||  year==2017 && jday== 242 ||
             idpp[j] >= 395 ||
             ((idpp[j] >= 335) &&
              (day == pdmin[j] && month == ((pmmin[j] + (mxmat[j] / 30.) - 1)%%12 + 1)))) { # maximum harvest date
            
            croplive[j]     <- 0.0
            greenfrac[j]  <- 0.0 # turn all vegetation to brown
            if(harvdate[j] == 999) harvdate[j] <- jday 
            plai[j]         <- 0.25 # simulates remaining stubble/mulch
            
            if(cropy > nratoon) endCycle <- T
            
            print(paste0('Sug.Cane - ratoon = ',cropy,year,jday,idpp[j],hui[j],gddmaturity[j]))
          }
        }
        
        
      }
      
    }
  
    ztopPft[j] <- ztopmxPft[j] * min(1,(rm / 50))* (min(plaimx[j] / (laimx[j]), 1)) ** 2
    
  }
  
  assign("endCycle", endCycle, envir = env)
  
  assign("ztopPft", ztopPft, envir = env)
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
  assign("arepr", arepr, envir = env)
  assign("aroot", aroot, envir = env)
  assign("astem", astem, envir = env)
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
  assign("cbiog", cbiog, envir = env)
  assign("cbios", cbios, envir = env)
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
  # assign("Deadcoroots",Deadcoroots   , envir = env)
  
  assign("cbiold",cbiold, envir = env)
  assign("cbiols",cbiols, envir = env)
  # assign("plmaleafmat",plmaleafmat, envir = env)
  
  # assign("Sapwood",Sapwood  , envir = env)
  # assign("Heartwood",Heartwood, envir = env)
  
  assign("DBranch_attached",DBranch_attached, envir = env)
  assign("DBranch_decay",DBranch_decay, envir = env)
  
  
  
}