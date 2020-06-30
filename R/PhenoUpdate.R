

PhenoUpdate <- function() {
  
  # PFT_UPDATE:
  
  # Substituir tempu e temps por temp
  # Atenção: Temp deve ser inicializado com 1 para todas as plantas
  
  # ---------------------------------------------------------------------
  # * * * update lai and canopy fractions * * *
  # ---------------------------------------------------------------------
  # 
  # Here the leaf display of shrubs, templs, is set equal to that of
  # trees, tempu, since it was determined that even though shrubs are
  # in the lower canopy, their leaf display follows more closely that
  # of trees than grasses.
  
  # templs <- tempu
  
  # PFT_UPDATE
  avglaiu <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == UPPER){
      avglaiu <- avglaiu + plai[i] * temp[i]
    }
  }
  # avglaiu <- plai[1] + plai[2] + plai[3] + plai[4] + (plai[5] * temp[5]) + plai[6] + (plai[7] * temp[7]) + (plai[8] * temp[8])
  
  # PFT_UPDATE
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == UPPER)
      frac[i] <- plai[i] * temp[i] / max (avglaiu, epsilon)
  }
  # upper canopy fractions
  # frac[1] <- plai[1] / max (avglaiu, epsilon)
  # frac[2] <- plai[2] / max (avglaiu, epsilon)
  # frac[3] <- plai[3] / max (avglaiu, epsilon)
  # frac[4] <- plai[4] / max (avglaiu, epsilon)
  # frac[5] <- plai[5] * temp[i] / max (avglaiu, epsilon)
  # frac[6] <- plai[6] / max (avglaiu, epsilon)
  # frac[7] <- plai[7] * temp[i] / max (avglaiu, epsilon)
  # frac[8] <- plai[8] * temp[i] / max (avglaiu, epsilon)
  
  # PFT_UPDATE
  avglail <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER){
      avglail <- avglail + plai[i] * temp[i]
    }
  }
  # lower canopy single sided leaf area index (area-weighted)
  # avglail <- plai[9] + plai[10] * temp[10] + plai[11] + plai[12]
  
  # lower canopy fractions
  # templs is included in frac(i,10) to allow
  # deciduous shrubs to drop their leaves
  # All other pfts keep leaves year-round
  
  # PFT_UPDATE
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER && plantList[[i]]$type == NATURAL_VEG)
      frac[i] <- plai[i] * temp[i] / max (avglail, epsilon)
  }
  # frac[9]  <- plai[9] / max (avglail, epsilon)
  # frac[10] <- plai[10] * temp[10] / max (avglail, epsilon)
  # frac[11] <- plai[11] / max (avglail, epsilon)  
  # frac[12] <- plai[12] / max (avglail, epsilon)
  
  
  # Find an average fraction of green vegetation in the lower canopy
  # to be used in stomata and twoset subroutines
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER && plantList[[i]]$type == NATURAL_VEG)
      greenfracl <- greenfrac[i] * frac[i]
  }
  # greenfracl <- frac[9] + frac[10] + greenfrac[11] * frac[11] + (greenfrac[12] * frac[12])
  
  # calculate the canopy leaf area index using the fractional vegetation cover
  
  # sant - Ive included this fl update here - as LAI is updated every day.
  
  avglaiu <- max (0.025, avglaiu)
  fu      <- avglaiu  / 1.0
  fu      <- max (0.025, min (0.975, fu))
  
  avglail <- max (0.025, avglail)
  fl      <- avglail  / 1.0
  fl      <- max (0.025, min (0.975, fl))
  
  # sant - end of modifications
  lai[1] <- avglail / fl
  lai[2] <- avglaiu / fu
  
  # put a fix on canopy lais to avoid problems in physics
  
  lai[1] <- max(0.025, min (lai[1], 12.0) )
  lai[2] <- max(0.025, min (lai[2], 12.0) )
  
  
  # ---------------------------------------------------------------------
  # * * * update canopy height parameters * * *
  # ---------------------------------------------------------------------
  # 
  # update lower canopy height parameters
  # 
  # note that they are based on vegetation fraction and not
  # averaged over the entire gridcell
  
  zbot[1] <- 0.05
  ztop[1] <- max (0.25, lai[1] * 0.25)
  
  # constrain ztop to be at least 0.5 meter lower than 
  # zbot for upper canopy
  
  ztop[1] <- min (ztop[1], zbot[2] - 0.5)
  
  assign("frac", frac, envir = env)
  assign("greenfracl", greenfracl, envir = env)
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)
  assign("lai", lai, envir = env)
  assign("zbot", zbot, envir = env)
  assign("ztop", ztop, envir = env)
  
  
}