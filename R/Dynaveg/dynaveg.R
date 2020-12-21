# Global Vars:
# aleaf      # fraction allocation to leaves
# aroot      # fraction allocation to fine roots
# awood      # fraction allocation to wood
# ayanpp     # annual above-ground npp for each plant type(kg-c/m**2/yr)
# ayanpptot  # annual above-ground npp for ecosystem (kg-c/m**2/yr)
# ayneetot   # annual total NEE for ecosystem (kg-C/m**2/yr)
# aynpp      # annual total npp for each plant type(kg-c/m**2/yr)
# biomass    # total biomass of each plant functional type  (kg_C m-2)
# cbiol      # carbon in leaf biomass pool (kg_C m-2)
# cbior      # carbon in fine root biomass pool (kg_C m-2)
# cbiow      # carbon in woody biomass pool (kg_C m-2)
# cdisturb   # annual amount of vegetation carbon lost 
# disturbf   # annual fire disturbance regime (m2/m2/yr)
# disturbo   # fraction of biomass pool lost every year to disturbances other than fire
# exist      # probability of existence of each plant functional type in a gridcell
# falll      # annual leaf litter fall                      (kg_C m-2/year)
# fallr      # annual root litter input                     (kg_C m-2/year)
# fallw      # annual wood litter fall                      (kg_C m-2/year)
# fl         # fraction of snow-free area covered by lower  canopy
# fu         # fraction of overall area covered by upper canopy
# icropsum   # index - number of crop types planted in each grid cell
# plai       # total leaf area index of each plant functional type
# sai        # current single-sided stem area index
# sapfrac    # fraction of woody biomass that is in sapwood
# specla     # specific leaf area (m**2/kg)
# tauleaf    # foliar biomass turnover time constant (years)
# tauroot    # fine root biomass turnover time constant (years)
# tauwood    # wood biomass turnover time constant (years)
# tauwood0   # wood biomass turnover time constant (years) 
# totbiol    # total biomass in the lower canopy (kg_C m-2)
# totbiou    # total biomass in the upper canopy (kg_C m-2)
# totlail    # total leaf area index for the lower canopy
# totlaiu    # total leaf area index for the upper canopy
# woodnorm   # value of woody biomass for upper canopy closure (ie when wood = woodnorm fu = 1.0) (kg_C m-2)
# xminlai    # Minimum LAI for each existing PFT
# zbot       # height of lowest branches above ground (m)
# ztop       # height of plant top above ground (m)

dynaveg <- function (isimfire) {
  
  # environment(vegmap) <- env
  environment(fire) <- env

  # ibis uses a small number of plant functional types:
  
  #  1: tropical broadleaf evergreen tree
  #  2: tropical broadleaf drought - deciduous trees
  #  3: warm - temperate broadleaf evergreen tree
  #  4: temperate conifer evergreen tree
  #  5: temperate broadleaf cold - deciduous tree
  #  6: boreal conifer evergreen tree
  #  7: boreal broadleaf cold - deciduous tree
  #  8: boreal conifer cold - deciduous tree
  #  9: evergreen shrub
  # 10: deciduous shrub
  # 11: warm (c4) grass
  # 12: cool (c3) grass
  
  # 13: soybeans
  # 14: maize 
  # 15: spring and winter wheat
  # 16: sugarcane
  # 17: eucalyptus
  # 18: oil palm
  
  # ---------------------------------------------------------------------
  # *  *  * specify biomass turnover parameters (years) *  * *
  # ---------------------------------------------------------------------

  # if(cropsums == 0) {
    
    # ---------------------------------------------------------------------
    # *  *  * initialize vegetation dynamics pools *  * *
    # ---------------------------------------------------------------------
    
    # zero out litter fall fields
    falll <- 0
    fallr <- 0
    fallw <- 0
    
    # zero out carbon lost due to disturbance
    cdisturb <- 0
    
    wood <- 0.001
    
    # ---------------------------------------------------------------------
    # *  *  * update npp, and pool losses *  * *
    # ---------------------------------------------------------------------
    
    # JAIR: Variável aynpp zerada na primeira vez que passa pela função.
    
    # go through all the pfts
    for(j in 1:npft) {
      
      if(!plantList[[j]]$active || plantList[[j]]$type == CROPS) next
      # apply this year's existence arrays to npp
      aynpp[j] <- exist[j] * aynpp[j]
      
      # determine above-ground npp for each plant type
      ayanpp[j] <- (aleaf[j] + awood[j]) * aynpp[j]
      
      # determine turnover rates for woody biomass:
      #
      # if pft can exist,    then tauwood <- tauwood0[normal turnover],
      # if pft cannot exist, then tauwood <- taufin years (to kill off trees)
      
      taufin <- tauwood0[j] / 2
      
      tauwood[j] <- tauwood0[j] - (tauwood0[j] - taufin) * (1 - exist[j])
      
      # assume a constant fine root turnover time
      tauroot[j] <- 1
      
      # determine litter fall rates
      falll <- falll + cbiol[j] / tauleaf[j]
      fallr <- fallr + cbior[j] / tauroot[j]
      fallw <- fallw + cbiow[j] / tauwood[j]
      
      # ---------------------------------------------------------------------
      # *  *  * update biomass pools *  * *
      # ---------------------------------------------------------------------
      
      # update carbon reservoirs using an analytical solution
      # to the original carbon balance differential equation
      cbiol[j] <- cbiol[j] * exp( - 1 / tauleaf[j]) + aleaf[j] * tauleaf[j] * max (0, aynpp[j]) * (1 - exp( - 1 / tauleaf[j]))
      
      cbiow[j] <- cbiow[j] * exp( - 1 / tauwood[j]) + awood[j] * tauwood[j] * max (0, aynpp[j]) * (1 - exp( - 1 / tauwood[j]))
      
      cbior[j] <- cbior[j] * exp( - 1 / tauroot[j]) + aroot[j] * tauroot[j] * max (0, aynpp[j]) * (1 - exp( - 1 / tauroot[j]))
      
      # if (j <= 8) wood <- wood + max (0, cbiow[j])
      if(plantList[[j]]$canopy == UPPER) {
        wood <- wood + max (0,  cbiow[j])
      }
      
    }
    
    # ---------------------------------------------------------------------
    # *  *  * apply disturbances *  *  *
    # ---------------------------------------------------------------------
    
    # set fixed disturbance regime
    disturbf <- 0.005
    disturbo <- 0.005
    
    # call fire disturbance routine
    
    if(isimfire  == 1) fire()
    
    for(j in 1:npft) {
      if(!plantList[[j]]$active || plantList[[j]]$type == CROPS) next
      # calculate biomass[vegetations] carbon lost to atmosphere   
      # used to balance net ecosystem exchange  
      cdisturb <- cdisturb + cbiol[j] * (disturbf + disturbo) + cbiow[j] * (disturbf + disturbo) + cbior[j] * (disturbf + disturbo)                  
      
      # adjust biomass pools due to disturbances
      cbiol[j] <- cbiol[j] * (1 - disturbf - disturbo)
      cbiow[j] <- cbiow[j] * (1 - disturbf - disturbo)
      cbior[j] <- cbior[j] * (1 - disturbf - disturbo)
      
      # constrain biomass fields to be positive
      cbiol[j] <- max (0, cbiol[j])
      cbiow[j] <- max (0, cbiow[j])
      cbior[j] <- max (0, cbior[j])
      
      # maintain minimum value of leaf carbon in areas that plants exist
      cbiol[j] <- max (exist[j] * xminlai / specla[j], cbiol[j])
      
      # update vegetation's physical characteristics
     
      plai[j] <- cbiol[j] * specla[j]
      biomass[j] <- cbiol[j] + cbiow[j] + cbior[j]
      
    }

    # ---------------------------------------------------------------------
    # *  *  * update annual npp, lai, and biomass *  * *
    # ---------------------------------------------------------------------
    
    # adjust annual net ecosystem exchange (calculated in stats.f) 
    # by loss of carbon to atmosphere due to biomass burning (fire)
    ayneetot <- ayneetot - cdisturb
    
    # determine total ecosystem above-ground npp
    ayanpptot <- sum(ayanpp[1:npft])
    
    # PFT_UPDATE: Soma todos os plais do dossel superior
    for(i in 1:npft) {
      if(!plantList[[i]]$active || plantList[[j]]$type == CROPS) next
      if(plantList[[i]]$canopy == UPPER) {
          totlaiu <- totlaiu + plai[i]
          totbiou <- totbiou + biomass[i]
      }
    }
    
    # update total canopy leaf area
    # totlaiu <- sum(plai[1:8])
    
    for(i in 1:npft) {
      if(!plantList[[i]]$active || plantList[[j]]$type == CROPS) next
      if(plantList[[i]]$canopy == LOWER) {
        totlail <- totlail + plai[i]
        totbiol <- totbiol + biomass[i]
      }
    }
    # totlail <- sum(plai[9:12])
    

    # update total biomass
    # totbiou <- sum(biomass[1:8])
    # 
    # totbiol <- sum(biomass[9:12])

    
    # ---------------------------------------------------------------------
    # *  *  * update fractional cover and vegetation height parameters *  * *
    # ---------------------------------------------------------------------
    #
    # update fractional cover of forest and herbaceous canopies:
    # 
    totlaiu <- max(0.25, totlaiu)
    totlail <- max(0.25, totlail)
    
    fu <- (1 - exp( - wood)) / (1 - exp( - woodnorm))
    
    # TODO: Alterado somente para testes, voltar ao valor original   fl <- totlail / 0.5
    # fl <- totlail / 0.5
    fl <- 1
#    fl <- totlail / 1
    
    # apply disturbances to fractional cover
    fu <- fu * (1 - disturbf - disturbo)
    fl <- fl * (1 - disturbf - disturbo)
    
    # constrain the fractional cover
    fu <- max (0.025, min (0.975, fu))
    #sant - original        fl[i] <- max (0.25, min (0.975, fl[i]))
    fl <- max (0.025, min (0.975, fl))
    
    # annual update upper canopy height parameters
    # should be calculated based on vegetative fraction and not the
    # average over the entire grid cell
    zbot[2] <- 3
    ztop[2] <- max(zbot[2] + 1, 2.50 * totbiou / fu * 0.75)
    
    # ---------------------------------------------------------------------
    # *  *  * update stem area index and sapwood fraction *  * *
    # ---------------------------------------------------------------------
    #
    # estimate stem area index (sai) as a fraction of the lai
    
    #san - original        sai[i,1] <- 0.050 * totlail[i]
    #san - original        sai[i,2] <- 0.250 * totlaiu[i]
    
    sai[1] <- max(0.05,0.050 * totlail)
    sai[2] <- max(0.25,0.250 * totlaiu)
    
    
    # estimate sapwood fraction of woody biomass
    sapspeed <- 25                        # (m / day)
    trans <- 0.0025                      # (2.5 mm / day) 
    saparea <- (trans / sapspeed)          # m ** 2
    
    sapvolume <- saparea * ztop[2] * 0.75  # m ** 3
    
    denswood <- 400                       # kg / m**3
    
    sapfrac <- min (0.50, max (0.05, sapvolume  * denswood / wood))
    
  # }  # check for crop existence 
  
  
  # ---------------------------------------------------------------------
  # *  *  * map out vegetation classes for this year *  * *
  # ---------------------------------------------------------------------
  
  # call vegmap
  # vegmap()
  
  # return to the main program
  
  assign("falll", falll, envir = env)
  assign("fallr", fallr, envir = env)
  assign("fallw", fallw, envir = env)
  assign("cdisturb", cdisturb, envir = env)
  assign("aynpp", aynpp, envir = env)
  assign("ayanpp", ayanpp, envir = env)
  assign("tauwood", tauwood, envir = env)
  assign("tauroot", tauroot, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("cbior", cbior, envir = env)
  assign("disturbf", disturbf, envir = env)
  assign("disturbo", disturbo, envir = env)
  assign("plai", plai, envir = env)
  assign("biomass", biomass, envir = env)
  assign("ayneetot", ayneetot, envir = env)
  assign("ayanpptot", ayanpptot, envir = env)
  assign("totlaiu", totlaiu, envir = env)
  assign("totlail", totlail, envir = env)
  assign("totbiou", totbiou, envir = env)
  assign("totbiol", totbiol, envir = env)
  assign("fu", fu, envir = env)
  assign("fl", fl, envir = env)
  assign("zbot", zbot, envir = env)
  assign("ztop", ztop, envir = env)
  assign("sai", sai, envir = env)
  assign("sapfrac", sapfrac, envir = env)
  
  return()
}
