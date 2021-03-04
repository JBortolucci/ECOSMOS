###############################
# subroutine cropupdate(jday)
###############################

# Global Vars:
# biomass(npoi,npft)     # total biomass of each plant functional type  (kg_C m-2)
# cbiol(npoi,npft)       # carbon in leaf biomass pool (kg_C m-2)
# croplive(npoi,npft)    # 0 crops have been planted and living : 1 crops not living
# ecpft                  # ending index for crop pfts
# epsilon                # small quantity to avoid zero-divides and other
# exist(npoi,npft)       # probability of existence of each plant functional type in a gridcell
# fallr(npoi)            # annual root litter input                     (kg_C m-2/year)
# fl(npoi)               # fraction of snow-free area covered by lower  canopy
# frac(npoi,npft)        # split of lignified litter material between protected/non-protected slow OM pools
# greenfracl(npoi)       # não comentado
# grnfraccrop(npoi,npft) # green fraction of aboveground vegetation for crops
# icropsum(npoi)         # index - number of crop types planted in each grid cell
# lai(npoi,2)            # canopy single-sided leaf area index (area leaf/area veg)
# laimx(npft)            # maximum LAI of each crop allowed
# npoi                   # total number of land points
# plai(npoi,npft)        # initial total LAI for each vegtype (used in iniveg)
# rm                     # sugarcane relative maturity GDD/GDDmaturity
# sai(npoi,2)            # current single-sided stem area index
# scpft                  # starting index for crop pfts - C. Kucharik
# specla(npft)           # specific leaf area (m**2/kg)
# totbiol(npoi)          # total biomass in the lower canopy (kg_C m-2)
# totlail(npoi)          # total leaf area index for the lower canopy
# xminlai                # Minimum LAI for each existing PFT
# zbot(npoi,2)           # height of lowest branches above ground (m)
# ztop(npoi,2)           # height of plant top above ground (m)
# ztopmxmze              # height maximum (m) for maize
# ztopmxsgc              # height maximum (m) for sugarcane
# ztopmxsoy              # height maximum (m) for soybean
# ztopmxwht              # height maximum (m) for wheat


# PFT_UPDATE (Santiago): Se deve ser genérica para todo tipo, então os calculos devem ser extendidos para os tipos naturais?
#                        No momento, é calculado apenas para culturas agrícolas.

# to do: Santiago e Jair, essa rotina de atualizacao deve ser generica, independente se é uma cultura agricola

# TO DO 2012: SVC e Jair, verificar se essa rotina esta' sendo chamada 

Cropupdate <- function (jday) {
  
  
  
  # TODO: Quando estiver funcionando corretamente para upper e lower, mudar a forma como troca UPPER/LOWER
  #       pois assim está ineficiente. Talvez criar duas funções e setar para o modelo no inicio da simulação.
  
  # TODO: Retirar exist no futuro, pois a cultura será definida pelo usuário.
  # # maintain minimum value of leaf carbon in crops that exist
  for(j in seq(1,npft)) {
    
    if(!plantList[[j]]$active) next
    
    if(plantList[[j]]$type == CROPS) {
      if(exist[j] == 1 && croplive[j] == 1) {
        cbiol[j] <- max((exist[j] * xminlai / specla[j]), cbiol[j])
      }
    }
  }
  
  # crop canopy single sided leaf area index (area - weighted)
  avglail <- 0
  # avglaiu <- 0
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(plantList[[j]]$canopy == LOWER) {
        avglail <- avglail + plai[j]
      } else {
        # avglaiu <- avglaiu + plai[j]
      }
    }
  }

  avglail <- max(avglail, 0.025)
  # avglaiu <- max(avglaiu, epsilon)
  
  # crop canopy fractions
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(plantList[[j]]$canopy == LOWER) {
        frac[j] <-  plai[j]  /  max (avglail, epsilon)
      } else {
        # frac[j] <-  plai[j]  /  max (avglaiu, epsilon)
      }
    }
  }
  
  # calculate total crop leaf are index
  totlail <- 0
  # totlaiu <- 0
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(plantList[[j]]$canopy == LOWER) {
        totlail <- totlail + plai[j]
      } else {
        # totlaiu <- totlaiu + plai[j]
      }
    }
  }
  
  totlail <- max(totlail, 0.025)
  # totlaiu <- max(totlaiu, epsilon)
  
  # calculate the crop canopy leaf area index using the fractional vegetation cover
  # fl <- totlail / 1
  fl <- totlail
  fl <- max(0.025, min(0.975, fl))
  lai[1] <- avglail / fl

  print(paste("Cropupdate",lai[1],avglail, fl,sep = " / "))
  
  # fu <- totlaiu / 1  
  # fu <- max(0.025, min(0.975, fu))
  # lai[2] <- avglaiu / fu
  # TODO (JAIR): Coloquei isso por que no PhenoUpdate usa. Não sei se é necessário aqui, mas parece que é uma forma de evitar
  #              divisão por zero no calculo da física. No lai[1] também.
  # lai[2] <- max(0.025, min (lai[2], 12.0) )
  
  # C. Kucharik  04.02.01
  # calculate greenness fraction of crop canopy
  # if plant optical properties were ever changed - due to browning of
  # vegetation in the future on a daily basis, do it here...this will effect leaf nir and vis
  # transmittance and reflectance in radiation.f
  # greenfracl[i] is lower canopy total green fraction - which is also being calculated
  # for natural vegetation in vegetation.f 
  
  
  # TODO (JAIR): Apenas para lower?
  greenfracl <- 0
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS & plantList[[j]]$canopy == LOWER)
        greenfracl <- greenfracl + frac[j] * greenfrac[j]
    
    print(paste("cropupdate ",j,greenfracl , frac[j] , greenfrac[j],sep = " / "))
    
    
  }
  
  
  
  totbiol <- 0
  # totbiou <- 0
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      if(plantList[[j]]$canopy == LOWER) {
        totbiol <- totbiol + biomass[j]
      } else {
        # totbiou <- totbiou + biomass[j]
      }
    }
  }
 
  # calculate crop bottom / top height
  # ztop is calculated in phenology subroutine based leaf area index 
  # is the weighted mean of plai of each crop pft in the grid cell
  # will only be important if we allow more than one crop pft to exist
  # within the same grid cell
  
  # LOWER  (Fazer mesmo calculo para o dossel superior)
  zbot[1] <- 0.02
  
  ztop[1] <- 0
  for(i in seq(1,npft)) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$type == CROPS && plantList[[i]]$canopy == LOWER) {
      # ztop[1] <- ztop[1] + (plai[i] / lai[1] * ztopPft[i])
      ztop[1] <- ztop[1] + ztopPft[i] * (plai[i] / lai[1])
      # ztop[1] <- ztop[1] + ztopPft[i]
    }
  }
  
  ztop[1] <- max(0.05,  ztop[1])
  
  # TODO: Quando estiver rodando para apenas uma planta, não deixar verificar. Precisa testar isso.
  # Verifica se o topo do dossel inferior não ultrapassou a base do dossel superior 
  # if(ztop[1] > zbot[2] && length(plantList) > 1)  {
  #   ztop[1] <- zbot[2]-0.5
  # }
  
  sai[1] <- 0
  for(i in seq(1,npft)) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$type == CROPS && plantList[[i]]$canopy == LOWER) {
      sai[1] <- sai[1] + plai[i] * 0.10
    }
  }

  
  #. SVC - Jair, pq não está usando ?
  
  # UPPER
  # ztop[2] <- 0
  # for(i in 1:npft) {
  #   if(plantList[[i]]$type == CROPS && plantList[[i]]$canopy == UPPER) {
  #     ztop[2] <- ztop[2] + (plai[i] / lai[2] * ztopPft[i])
  #   }
  # }

  # TODO (Santiago): find a better function to define bottom height of upper canopy. Currently is 50% of height.
  # zbot[2] <- ztop[2] * 0.5
  # 
  # htmx[2] <- max(htmx[2], ztop[2])
  # ztop[2] <- max(zbot[2]+0.05, max(htmx[2], ztop[2]))
  # 
  # sai[2] <- 0
  # for(i in 1:npft) {
  #   if(plantList[[i]]$type == CROPS && plantList[[i]]$canopy == UPPER) {
  #     sai[2] <- sai[2] + plai[i] * 0.10
  #   }
  # }
  

  
  
  
  
  
  # To do: levar para o plant_params e fazer uma media ponderada pelo lai aqui
 #  rhovegvlg 
 #  rhovegvlb 
 #  rhovegvu  
 #  rhovegirlg
 #  rhovegirlb
 #  rhovegiru 
 #  tauvegvlg 
 #  tauvegvlb 
 #  tauvegvu  
 #  tauvegirlg
 #  tauvegirlb
 #  tauvegiru 
  
  
  # oriev[1] <- max ( - chiflz, 0)
  # oriev[2] <- max ( - chifuz, 0)
  # 
  # orieh[1] <- max ( chiflz, 0)
  # orieh[2] <- max ( chifuz, 0)
  
  
  
  
  
  assign("cbiol", cbiol, envir = env) 
  assign("frac", frac, envir = env)              
  assign("totlail", totlail, envir = env)         
  assign("fl", fl, envir = env)                   
  assign("lai", lai, envir = env)                 
  assign("greenfracl", greenfracl, envir = env)   
  assign("totbiol", totbiol, envir = env)        
  assign("zbot", zbot, envir = env)               
  assign("ztop", ztop, envir = env)               
  assign("sai", sai, envir = env)                 
  
}