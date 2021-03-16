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
      plai[j]<- max( plai[j],0.01)
      totlail <- totlail + plai[j] * temp[j]
    } else {
      plai[j]<- max( plai[j],0.01)
      totlaiu <- totlaiu + plai[j] * temp[j]
    }
  }
  
  totlail <- max(totlail, 0.005)
  totlaiu <- max(totlaiu, epsilon)
  
  fu <- totlaiu / 1.0
  fu <- max(0.001, min(0.98, fu))
  lai[2] <- totlaiu / fu
  lai[2] <- max(0.005, min (lai[2], 12.0) )

    
  totlail <- max(totlail, 0.005)
  fl <- totlail/1.0
  fl <- max(0.001, min(0.99, fl))
  lai[1] <- totlail / fl
  lai[1] <- max(0.005, min (lai[1], 12.0) )
  

#LOWER CANOPY
  ztop[1] <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      # ztop[1] <- ztop[1] + (plai[j] / lai[1] * ztopPft[j])
      ztopTemp <- lai[1] * 0.25
      if(plantList[[j]]$type == CROPS)
                    ztopTemp <- ztopPft[j]
      
      ztop[1] <- ztop[1] + ztopTemp * (plai[j] / lai[1])
      # ztop[1] <- ztop[1] + ztopPft[j]
    }
  }
  
  zbot[1] <- max(0.0,ztop[1] * 0.5)
  ztop[1] <- max(zbot[1]+0.05, ztop[1])
  
  # UPPER CANOPY
  ztop[2] <- 0
  for(j in 1:npft) {
    if(plantList[[j]]$canopy == UPPER) {
      ztopTemp <- lai[2] * 0.25
      if(plantList[[j]]$type == CROPS)
        ztopTemp <- ztopPft[j]
      ztop[2] <- ztop[2] + ztopTemp * (plai[j] / lai[2])
    }
  }
  
  zbot[2] <- max(ztop[1]+0.1,max(0.10,ztop[2] * 0.5))
  
  ztop[2] <- max(zbot[2]+0.05, ztop[2])
  

  sai[1] <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      sai[1] <- sai[1] + plai[j] * 0.10
    }
  }
  
  
  sai[2] <- 0
  for(j in 1:npft) {
    if(plantList[[j]]$canopy == UPPER) {
      sai[2] <- sai[2] + plai[j] * 0.10
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
  
    greenfrac[1] <- 0
    rhovegvlg  <- 0
    rhovegvlb  <- 0
    rhovegirlg <- 0
    rhovegirlb <- 0
    tauvegirlg <- 0
    tauvegirlb <- 0
    tauvegvlg  <- 0
    tauvegvlb  <- 0
    chiflzavg  <- 0

  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      
    greenfrac[1] <- greenfrac[1] + frac[j] * pgreenfrac[j]
    rhovegvlg  <- rhovegvlg  + frac[j] * rhovegvgin[j]     # vis leaf reflectance, lower story, green leaves
    rhovegvlb  <- rhovegvlb  + frac[j] * rhovegvbin[j]     # vis leaf reflectance, lower story, brown leaves     
    rhovegirlg <- rhovegirlg + frac[j] * rhovegirgin[j]    # nir leaf reflectance, lower story, green leaves
    rhovegirlb <- rhovegirlb + frac[j] * rhovegirbin[j]    # nir leaf reflectance, lower story, brown leaves  
    tauvegirlg <- tauvegirlg + frac[j] * tauvegirgin[j]    # nir leaf transmittance, lower story, green leaves
    tauvegirlb <- tauvegirlb + frac[j] * tauvegirbin[j]    # nir leaf transmittance, lower story, brown leaves
    tauvegvlg  <- tauvegvlg  + frac[j] * tauvegvgin[j]     # vis leaf transmittance, lower story, green leaves
    tauvegvlb  <- tauvegvlb  + frac[j] * tauvegvbin[j]     # vis leaf transmittance, lower story, brown leaves
    chiflzavg  <- chiflzavg  + frac[j] * chiflz[j]
    }
  }
    
    if (greenfrac[1]  == 0) greenfrac[1]   <- 1 
    if(rhovegvlg  == 0 ) rhovegvlg  <- rhovegvgin[1] 
    if(rhovegvlb  == 0 ) rhovegvlb  <- rhovegvbin[1] 
    if(rhovegirlg == 0 ) rhovegirlg <- rhovegirgin[1]
    if(rhovegirlb == 0 ) rhovegirlb <- rhovegirbin[1]
    if(tauvegirlg == 0 ) tauvegirlg <- tauvegirgin[1]
    if(tauvegirlb == 0 ) tauvegirlb <- tauvegirbin[1]
    if(tauvegvlg  == 0 ) tauvegvlg  <- tauvegvgin[1] 
    if(tauvegvlb  == 0 ) tauvegvlb  <- tauvegvbin[1] 
    if(chiflzavg  == 0 ) chiflzavg  <- chiflz[1]
  

    # Define other vegetation properties    
    # leaf orientation factors ( - 1 vertical, 0 random, 1 horizontal)
    
    oriev[1] <- max ( - chiflzavg, 0)
    orieh[1] <- max ( chiflzavg, 0)

      greenfrac[2] <- 0
      rhovegvug   <- 0
      rhovegvub   <- 0
      rhovegirug  <- 0
      rhovegirub  <- 0
      tauvegvug   <- 0
      tauvegvub   <- 0
      tauvegirug  <- 0
      tauvegirub  <- 0
      chifuzavg   <- 0
      
    for(j in 1:npft) {
      if(!plantList[[j]]$active) next
      if(plantList[[j]]$canopy == UPPER) {

   greenfrac[2] <- greenfrac[2] + frac[j] * pgreenfrac[j]
   rhovegvug   <- rhovegvug  + frac[j] * rhovegvgin[j]      # vis leaf reflectance, upper story, green leaves
   rhovegvub   <- rhovegvub  + frac[j] * rhovegvbin[j]      # vis leaf reflectance, upper story, green leaves
   rhovegirug  <- rhovegirug + frac[j] * rhovegirgin[j]    # nir leaf reflectance, upper story, green leaves
   rhovegirub  <- rhovegirub + frac[j] * rhovegirbin[j]    # nir leaf reflectance, upper story, green leaves
   tauvegvug   <- tauvegvug  + frac[j] * tauvegvgin[j]     # vis leaf transmittance, upper story, green leaves
   tauvegvub   <- tauvegvub  + frac[j] * tauvegvbin[j]     # vis leaf transmittance, upper story, green leaves
   tauvegirug  <- tauvegirug + frac[j] * tauvegirgin[j]    # nir leaf transmittance, upper story, green leaves
   tauvegirub  <- tauvegirub + frac[j] * tauvegirbin[j]    # nir leaf transmittance, upper story, green leaves
   
   chifuzavg  <- chifuzavg + frac[j] * chifuz[j]
      }
    }
      
        if (greenfrac[2]  == 0) greenfrac[2]   <- 1 
        if (rhovegvug  == 0) rhovegvug   <- rhovegvgin[1] 
        if (rhovegvub  == 0) rhovegvub   <- rhovegvbin[1] 
        if (rhovegirug == 0) rhovegirug  <- rhovegirgin[1]
        if (rhovegirub == 0) rhovegirub  <- rhovegirbin[1]
        if (tauvegvug  == 0) tauvegvug   <- tauvegvgin[1] 
        if (tauvegvub  == 0) tauvegvub   <- tauvegvbin[1] 
        if (tauvegirug == 0) tauvegirug  <- tauvegirgin[1]
        if (tauvegirub == 0) tauvegirub  <- tauvegirbin[1]
        if (chifuzavg  == 0) chifuzavg   <- chifuz[1]
      
      
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
  aybprod[j]   <- aybprod[j] +
                  aleaf[j]   * max(0.0,adnpp[j]) +
                  astem[j]   * max(0.0,adnpp[j]) +
                  arepr[j]   * max(0.0,adnpp[j]) +
                  abranch[j] * max(0.0,adnpp[j]) +
                  aroot[j]   * max(0.0,adnpp[j]) +
                  acroot[j]  * max(0.0,adnpp[j]) +
                  awood[j]   * max(0.0,adnpp[j]) 
  
  # aboveground value to calculate harvest index
  ayabprod[j]  <- ayabprod[j] +
                  aleaf[j]   * max(0.0,adnpp[j]) +
                  astem[j]   * max(0.0,adnpp[j]) +
                  arepr[j]   * max(0.0,adnpp[j]) +
                  abranch[j] * max(0.0,adnpp[j]) +
                  awood[j]   * max(0.0,adnpp[j]) 
  
  # keep track of annual total root production carbon
  ayrprod[j]  <- ayrprod[j] +
                 aroot[j]  * max(0.0,adnpp[j]) +
                 acroot[j] * max(0.0,adnpp[j])
  
  # keep track of total carbon allocated to
  # leaves for litterfall calculation
  aylprod[j] <- aylprod[j] +
                aleaf[j] * max (0.0, adnpp[j])
  
  biomass[j] <- cbiol[j] + cbios[j] + cbiog[j] + cbiocr[j] + cbior[j] + cbiob[j] + cbiow[j]
  

# End track of total biomass production  
#______________________________________________________      
  
  
  
  assign("cbiol", cbiol, envir = env) 
  assign("frac", frac, envir = env)              
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)                   
  assign("lai", lai, envir = env)                 
  assign("greenfrac", greenfrac, envir = env)   
  assign("zbot", zbot, envir = env)               
  assign("ztop", ztop, envir = env)               
  assign("sai", sai, envir = env)   
  
  assign("biomass", biomass, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  
  
  assign("rhovegvlg",rhovegvlg ,envir = env)
  assign("rhovegvlb",rhovegvlb ,envir = env)
  assign("rhovegirlg",rhovegirlg,envir = env)
  assign("rhovegirlb",rhovegirlb,envir = env)
  assign("tauvegvlg",tauvegvlg ,envir = env)
  assign("tauvegvlb",tauvegvlb ,envir = env)
  assign("tauvegirlg",tauvegirlg,envir = env)
  assign("tauvegirlb",tauvegirlb,envir = env)
  
  assign("rhovegvug",rhovegvug ,envir = env)
  assign("rhovegvub",rhovegvub ,envir = env)
  assign("rhovegirug",rhovegirug,envir = env)
  assign("rhovegirub",rhovegirub,envir = env)
  assign("tauvegvug",tauvegvug ,envir = env)
  assign("tauvegvub",tauvegvub ,envir = env)
  assign("tauvegirug",tauvegirug,envir = env)
  assign("tauvegirub",tauvegirub,envir = env)
  
  

  
  
}