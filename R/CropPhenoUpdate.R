CropPhenoUpdate <- function() {
  
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(exist[j] == 1 && croplive[j] == 1) {
        cbiol[j] <- max((exist[j] * xminlai / specla[j]), cbiol[j])
        # check for maximum plant leaf area index
        if (plai[j] > plaimx[j]) 
          plaimx[j] <- plai[j]
      }
    }
  }
  
  avglail <- 0
  avglaiu <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      avglail <- avglail + plai[j] * temp[j]
    } else {
      avglaiu <- avglaiu + plai[j] * temp[j]
    }
  }
  
  avglail <- max(avglail, 0.025)
  avglaiu <- max(avglaiu, epsilon)
  
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$canopy == LOWER) {
      frac[j] <-  plai[j] * temp[j] /  max (avglail, epsilon)
    } else {
      frac[j] <-  plai[j] * temp[j] /  max (avglaiu, epsilon)
    }
  }
  
  greenfracl <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER) {
      greenfracl <- greenfracl + frac[j] * greenfrac[j]
    }
  }
  
  totlail <- avglail
  totlaiu <- avglaiu
  
  totlaiu <- max(totlaiu, epsilon)
  fu <- totlaiu / 1
  fu <- max(0.025, min(0.975, fu))
  
  totlail <- max(totlail, 0.025)
  fl <- totlail
  fl <- max(0.025, min(0.975, fl))
  
  lai[1] <- avglail / fl
  lai[2] <- avglaiu / fu
  
  lai[1] <- max(0.025, min (lai[1], 12.0) )
  lai[2] <- max(0.025, min (lai[2], 12.0) )
  
  totbiol <- 0
  totbiou <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(plantList[[j]]$canopy == LOWER) {
        totbiol <- totbiol + biomass[j]
      } else {
        totbiou <- totbiou + biomass[j]
      }
    }
  }
  
  ayanpptot <- 0
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS)
      ayanpptot <- ayanpptot + ayanpp[j] 
  }
  
  # UPPER
  ztop[2] <- 0
  for(i in 1:npft) {
    if(plantList[[i]]$canopy == UPPER) {
      ztopTemp <- lai[2] * 0.25
      if(plantList[[i]]$type == CROPS)
        ztopTemp <- ztopPft[i]
      ztop[2] <- ztop[2] + ztopTemp * (plai[i] / lai[2])
    }
  }
  

  zbot[2] <- max(1.5,ztop[2] * 0.5)
  # zbot[2] <- ztop[2] * 0.5

  htmx[2] <- max(htmx[2], ztop[2])
  ztop[2] <- max(zbot[2]+0.05, max(htmx[2], ztop[2]))
  
  # LOWER  (Fazer mesmo calculo para o dossel superior)
  zbot[1] <- 0.02
  
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
  
  htmx[1] <- max(htmx[1], ztop[1])
  ztop[1] <- max(0.05, max(htmx[1], ztop[1]))
  
  # TODO: Quando estiver rodando para apenas uma planta, não deixar verificar. Precisa testar isso.
  # Verifica se o topo do dossel inferior não ultrapassou a base do dossel superior 
  if(ztop[1] > zbot[2] && length(plantList) > 1)  {
    ztop[1] <- zbot[2] - 0.5
  }
  
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
  assign("plaimx", plaimx, envir = env)           
  assign("frac", frac, envir = env)              
  assign("totlail", totlail, envir = env)  
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)                   
  assign("lai", lai, envir = env)                 
  assign("greenfracl", greenfracl, envir = env)   
  assign("totbiol", totbiol, envir = env)        
  assign("zbot", zbot, envir = env)               
  assign("ztop", ztop, envir = env)               
  assign("htmx", htmx, envir = env)              
  assign("sai", sai, envir = env)                 
  assign("ayanpptot", ayanpptot, envir = env)
}