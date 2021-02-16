CropPhenoUpdate <- function() {
  
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(exist[j] == 1 && croplive[j] == 1) {
        cbiol[j] <- max((exist[j] * xminlai / specla[j]), cbiol[j])
      }
    }
  }
  
  totlail <- 0
  totlaiu <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      totlail <- totlail + plai[j] * temp[j]
    } else {
      totlaiu <- totlaiu + plai[j] * temp[j]
    }
  }
  
  totlail <- max(totlail, 0.01)
  totlaiu <- max(totlaiu, epsilon)
  
  fu <- totlaiu / 2.0
  fu <- max(0.025, min(0.975, fu))
  lai[2] <- totlaiu / fu
  lai[2] <- max(0.025, min (lai[2], 12.0) )

    
  totlail <- max(totlail, 0.025)
  fl <- totlail/1.0
  fl <- max(0.025, min(0.975, fl))
  lai[1] <- totlail / fl
  lai[1] <- max(0.025, min (lai[1], 12.0) )
  

#LOWER CANOPY
  ztop[1] <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER) {
      # ztop[1] <- ztop[1] + (plai[i] / lai[1] * ztopPft[i])
      ztopTemp <- lai[1] * 0.25
      if(plantList[[i]]$type == CROPS)
                    ztopTemp <- ztopPft[i]
      
      ztop[1] <- ztop[1] + ztopTemp * (plai[i] / lai[1])
      # ztop[1] <- ztop[1] + ztopPft[i]
    }
  }
  
  zbot[1] <- max(0.0,ztop[1] * 0.5)
  ztop[1] <- max(zbot[1]+0.05, ztop[1])
  
  # UPPER CANOPY
  ztop[2] <- 0
  for(i in 1:npft) {
    if(plantList[[i]]$canopy == UPPER) {
      ztopTemp <- lai[2] * 0.25
      if(plantList[[i]]$type == CROPS)
        ztopTemp <- ztopPft[i]
      ztop[2] <- ztop[2] + ztopTemp * (plai[i] / lai[2])
    }
  }
  
  zbot[2] <- max(ztop[1]+0.1,max(0.10,ztop[2] * 0.5))
  
  ztop[2] <- max(zbot[2]+0.05, ztop[2])
  

  sai[1] <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER) {
      sai[1] <- sai[1] + plai[i] * 0.10
    }
  }
  
  
  sai[2] <- 0
  for(i in 1:npft) {
    if(plantList[[i]]$canopy == UPPER) {
      sai[2] <- sai[2] + plai[i] * 0.10
    }
  }
  

  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      frac[j] <-  plai[j] * temp[j] /  max (totlail, epsilon)
    } else {
      frac[j] <-  plai[j] * temp[j] /  max (totlaiu, epsilon)
    }
  }
  
    greenfracl <- 0
    rhovegvlg  <- 0
    rhovegvlb  <- 0
    rhovegirlg <- 0
    rhovegirlb <- 0
    tauvegirlg <- 0
    tauvegirlb <- 0
    tauvegvlg  <- 0
    tauvegvlb  <- 0
    chiflzavg  <- 0
    
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER) {
      
    greenfracl <- greenfracl + frac[j] * greenfrac[j]
    rhovegvlg  <- rhovegvlg  + frac[j] * rhovegvlgin[i]     # vis leaf reflectance, lower story, green leaves
    rhovegvlb  <- rhovegvlb  + frac[j] * rhovegvlbin[i]     # vis leaf reflectance, lower story, brown leaves     
    rhovegirlg <- rhovegirlg + frac[j] * rhovegirlgin[i]    # nir leaf reflectance, lower story, green leaves
    rhovegirlb <- rhovegirlb + frac[j] * rhovegirlbin[i]    # nir leaf reflectance, lower story, brown leaves  
    tauvegirlg <- tauvegirlg + frac[j] * tauvegirlgin[i]    # nir leaf transmittance, lower story, green leaves
    tauvegirlb <- tauvegirlb + frac[j] * tauvegirlbin[i]    # nir leaf transmittance, lower story, brown leaves
    tauvegvlg  <- tauvegvlg  + frac[j] * tauvegvlgin[i]     # vis leaf transmittance, lower story, green leaves
    tauvegvlb  <- tauvegvlb  + frac[j] * tauvegvlbin[i]     # vis leaf transmittance, lower story, brown leaves
    chiflzavg  <- chiflzavg  + frac[j] * chiflz[j]
    }
  }
  
    # Define other vegetation properties    
    # leaf orientation factors ( - 1 vertical, 0 random, 1 horizontal)
    
    oriev[1] <- max ( - chiflzavg, 0)
    orieh[1] <- max ( chiflzavg, 0)

    
      rhovegvu   <- 0
      rhovegiru  <- 0
      tauvegvu   <- 0
      tauvegiru  <- 0 
      chifuzavg  <- 0
      
    for(i in 1:npft) {
      if(!plantList[[i]]$active) next
      if(plantList[[i]]$canopy == UPPER) {

   rhovegvu   <- rhovegvu  + frac[j] * rhovegvuin[i]      # vis leaf reflectance, upper story, green leaves
   rhovegiru  <- rhovegiru + frac[j] * rhovegiruin[i]    # nir leaf reflectance, upper story, green leaves
   tauvegvu   <- tauvegvu  + frac[j] * tauvegvuin[i]     # vis leaf transmittance, upper story, green leaves
   tauvegiru  <- tauvegiru + frac[j] * tauvegiruin[i]    # nir leaf transmittance, upper story, green leaves
   chifuzavg  <- chifuzavg + frac[j] * chifuz[i]
      }
    }
      
      oriev[2] <- max ( - chifuzavg, 0)
      orieh[2] <- max ( chifuzavg, 0)    
      
# Fim da Atualizacao das propriedades dos Dosseis
#______________________________________________________

#______________________________________________________    
# Start track of total biomass production  

    #----------------------------
  #### Annual production
  #----------------------------
  # keep track of total biomass production for the entire year, and the
  aybprod[i]   <- aybprod[i] +
                  aleaf[i]   * max(0.0,adnpp[i]) +
                  astem[j]   * max(0.0,adnpp[i]) +
                  arepr[j]   * max(0.0,adnpp[i]) +
                  abranch[i] * max(0.0,adnpp[i]) +
                  aroot[i]   * max(0.0,adnpp[i]) +
                  acroot[i]  * max(0.0,adnpp[i]) +
                  awood[i]   * max(0.0,adnpp[i]) 
  
  # aboveground value to calculate harvest index
  ayabprod[i]  <- ayabprod[i] +
                  aleaf[i]   * max(0.0,adnpp[i]) +
                  astem[j]   * max(0.0,adnpp[i]) +
                  arepr[j]   * max(0.0,adnpp[i]) +
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
  
  biomass[i] <- cbiol[i] + cbios[j] + cbiog[j] + cbiocr[i] + cbior[i] + cbiob[i] + cbiow[i]
  

# End track of total biomass production  
#______________________________________________________      
  
  
  
  assign("cbiol", cbiol, envir = env) 
  assign("frac", frac, envir = env)              
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)                   
  assign("lai", lai, envir = env)                 
  assign("greenfracl", greenfracl, envir = env)   
  assign("zbot", zbot, envir = env)               
  assign("ztop", ztop, envir = env)               
  assign("sai", sai, envir = env)   
  
  assign("biomass", biomass, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  
}