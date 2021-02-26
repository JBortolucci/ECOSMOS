

SugarcaneResidue <- function(year, iyear0, jday, index) {
  
  nratoon  <- plantList[[index]]$param$nratoon
  firecane <- plantList[[index]]$param$firecane
  
  j <- index
  
  # zero out litter fall rates
  # falll <- 0
  # fallw <- 0
  # fallr <- 0
  
  if(croplive[j] == 1) {

    fallr <- fallr + fallrsgc[3] * sumfroot[1,2] # Michel: 28-Dez
        
    #remove all old root in 60 days after harvest
    fallrsgc[2] <- max( fallrsgc[2] - fallrsgc[1] * (1 / 90) , 0) 
    if(fallrsgc[2] > 0) fallr <- fallr + fallrsgc[1] * (1 / 90)
    

    
  }  #decay today 
  
  
  # for(j in scpft:ecpft) {
  


  # only write out values at harvest date, and re-initialize crop variables
  # at this time  - this allows for the same crop (e.g., wheat) to be grown
  # across two consecutive calendar years   
  
  if(exist[j] == 1 && harvdate[j] == jday) {

    ayabprod[j] <- max(cbiol[j], ayabprod[j])
    
    # calculate dry matter material (Mg / ha); yield in Mg / ha dry matter (sucrose carbon)
    dmyield[j] <- cbiog[j]* fyield[j] * 10 / cgrain[j]  
    dmstem[j]  <- cbios[j]* fyield[j] * 10 / cgrain[j]   
    dmleaf[j]  <- cbiol[j] * 10 / cgrain[j]   
    dmroot[j]  <- cbior[j] * 10 / cgrain[j]   

    dmcrop[j] <- dmyield[j] + dmstem[j] + dmleaf[j] + dmroot[j]
    
   
    
    # calculate above ground residue dry matter (Mg / ha) at harvest
      dmresidue[j] <- dmleaf[j] + ( (cbios[j] + cbiog[j]) * (1 - fyield[j]) * 10 / cgrain[j] ) #meristem + the base not harvested

    
    # calculate aboveground residue dry matter total (including along the grow season)
    rdm <- dmresidue[j] + (aylprod[j] * 10 / cgrain[j]) - dmleaf[j] # - dmleaf[i,j]; since dmleaf[i,j] is accounted in aylprod    
    
    # calculate fractions for leaf and stem
    fdml <- (aylprod[j] * 10 / cgrain[j]) / rdm 
    
    fdms <- (dmresidue[j] - dmleaf[j]) / rdm
    
    fnresidue <- fnleaf[j] * fdml + fnstem[j] * fdms  
    
    # calculate amount of N in aboveground residue (kg / ha) 
    residuen[j] <- (fnleaf[j] * dmleaf[j] +  fnstem[j] * (dmresidue[j] - dmleaf[j])  ) * 1e+04
    
    # assign leaf, stem, root, and grain nitrogen concentrations
    # to new variables (in percent)
    nconcl[j] <- fnleaf[j]  * 100
    nconcs[j] <- fnstem[j]  * 100
    nconcr[j] <- fnroot[j]  * 100
    nconcg[j] <- fngrain[j] * 100
    
    # assign total nitrogen plant uptake to new variable for
    # purposes of outputting data at end of year (kg / ha)
    cropn[j] <- totnuptake[j] * 1e+04  
    
    # assign total nitrogen fixation for crop to new variable for
    # purposes of outputting this data at end of calendar year
    # units of kg / ha
    cropfixn[j] <- totnfix[j] * 1e+04
    
    # carbon nitrogen ratio of plant residue goes to biogeochem.f
    # and fine roots
    if(fnresidue  > 0) {
      cntops[j] <- min(cfrac[j] / fnresidue, 200) 
    } else {
      cntops[j] <- 60
    }
    
    if(fnroot[j] > 0) {
      cnroot[j] <- min(cfrac[j] / fnroot[j], 200) 
    } else {
      cnroot[j] <- 80
    }
    
    # assume that stem and leaf are both included in leaf litterfall value
    # these annual total values (falll, fallr, fallw) cannot be changed 
    # on a daily basis because biogeochem.f uses the annual total, split
    # between each day of the year equally 
    # carbon returned as residue

      if(firecane  == 1) {
        falll <- falll + (cbios[j] + cbiog[j]) * (1 - fyield[j]) / 2  #meristem 
      } else {
        falll <- falll + aylprod[j] + (cbios[j] + cbiog[j]) * (1 - fyield[j])  #leaf + meristem and base not harvested
      }

    
    if( cropy <= nratoon) {

      fallr <- fallr + cbior[j] * 0.30 * sumfroot[1,2] # Michel: 28-Dez
      fallrsgc[1] <- cbior[j] * 0.70
      fallrsgc[2] <- cbior[j] * 0.70
      
    } else {     # plant again 
      fallr <- fallr + cbior[j] * sumfroot[1,2] # Michel: 28-Dez
      fallrsgc[1] <- cbior[j] * 0
      fallrsgc[2] <- cbior[j] * 0
    }
    
    
    #______________________________________________________
    #__________ Update GDD ________________________________
    
    if( cropy == 1) {
      gddsgcp <- (gddsgcp + gddmaturity[j]) / 2  
    }
    if( cropy > 1)  { 
      gddsgcr <- (gddsgcr + gddmaturity[j]) / 2  
    }
    
    
    if( cropy <= nratoon) {
      cropy <- cropy + 1	
      croplive[j] <- 1   
      cbiol[j] <- 0.05/ specla[j]
      plai[j]  <- cbiol[j] * specla[j]
      gddmaturity[j]  <- gddsgcp
      

      ##### Check if cycle was complete #####
      # temperature has fallen below freeze 
      if(tmin <= tkill[j]) {
        croplive[j] <- 0    
        cropy       <- 0     
        print(paste0('sugarcane planted didnt get the minimum of development, start to planting again'))
      }
      
    } else if ( cropy > nratoon) {
      croplive[j] <- 0    
      cropy       <- 0
      endCycle[i] <- T
    }  
    
  }  # harvest <- jday

  # }


  assign("falll", falll, envir = env)
  assign("fallw", fallw, envir = env)
  assign("fallr", fallr, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("dmyield", dmyield, envir = env)
  assign("dmstem", dmstem, envir = env)
  assign("dmleaf", dmleaf, envir = env)
  assign("dmroot", dmroot, envir = env)
  assign("dmcrop", dmcrop, envir = env)
  assign("dmresidue", dmresidue, envir = env)
  assign("residuen", residuen, envir = env)
  assign("nconcl", nconcl, envir = env)
  assign("nconcs", nconcs, envir = env)
  assign("nconcr", nconcr, envir = env)
  assign("nconcg", nconcg, envir = env)
  assign("cropn", cropn, envir = env)
  assign("cropfixn", cropfixn, envir = env)
  assign("cntops", cntops, envir = env)
  assign("cnroot", cnroot, envir = env)
  assign("cropy", cropy, envir = env)  
  assign("endCycle", endCycle, envir = env)
  assign("croplive", croplive, envir = env)
  assign("gddsgcp", gddsgcp, envir = env)
  assign("gddsgcr", gddsgcr, envir = env)
  assign("gddmaturity", gddmaturity, envir = env)
  assign("fallrsgc", fallrsgc, envir = env)

}