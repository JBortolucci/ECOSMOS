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
  
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      frac[j] <-  plai[j] * temp[j] /  max (totlail, epsilon)
    } else {
      frac[j] <-  plai[j] * temp[j] /  max (totlaiu, epsilon)
    }
  }
  
  greenfracl <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER) {
      greenfracl <- greenfracl + frac[j] * greenfrac[j]
    }
  }
  

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
  
  
  
  
  
  assign("cbiol", cbiol, envir = env) 
  assign("frac", frac, envir = env)              
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)                   
  assign("lai", lai, envir = env)                 
  assign("greenfracl", greenfracl, envir = env)   
  assign("zbot", zbot, envir = env)               
  assign("ztop", ztop, envir = env)               
  assign("sai", sai, envir = env)                 
}