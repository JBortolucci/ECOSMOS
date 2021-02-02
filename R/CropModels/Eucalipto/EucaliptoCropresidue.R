#to do: Santiago, residuo tem que ser indepente se é uma cultura agricola
# juntar com o residuo dos ecossistemas naturais

EucaliptoCropresidue <- function (year, year0, jday, index) {
  
  deltay    <- plantList$eucalipto$params$deltay 
  
  j <- index
  
  # zero out litter fall rates

  
  if(croplive[j] == 1) {
    falll <- falll + (Deadleaves) * deltay/kg_C_M2_to_T_ha
    fallr <- fallr + (Deadcoroots + Deadfineroots) * deltay/kg_C_M2_to_T_ha  * sumfroot[1,2] # Michel: 23-Out
    # fallw <- fallw + (Deadwood) * deltay/kg_C_M2_to_T_ha + DBranch_decay
    fallw <- fallw + (Deadwood) * deltay/kg_C_M2_to_T_ha + Deadbranch * deltay/kg_C_M2_to_T_ha
  }

  
  # gddplant gets reinitialized to 0 at maturity date so save value here
  if(gddplant[j] > 0 && croplive[j] == 1) {
    crmplant[j] <- max(73, min((gddplant[j] + 53.683) / 13.882,135))
  }
  

  if(exist[j] == 1 && harvdate[j] == jday) {
    

    ayabprod[j] <- max(cbiol[j], ayabprod[j])
    
    # added so division by zero doesn't take place in those places where
    # production was zero 
    
    # adjust actual grain yield value (params.crp)
    dumg <- cbiocr[j] 
    cbiocr[j] <- cbiocr[j] * fyield[j] 
    
    # add excess (i.e. pod of soybeans) to stem storage pool of plant 
    cbiob[j] <- max(0, cbiob[j] + (dumg - cbiocr[j]))
    

    # calculate dry matter material (Mg / ha); yield in Mg / ha dry matter (sucrose carbon)
    dmyield[j] <- cbiocr[j] * 10 / cgrain[j]  
    dmstem[j]  <- cbiob[j]  * 10 / cgrain[j]   
    dmleaf[j]  <- cbiol[j]  * 10 / cgrain[j]   
    dmroot[j]  <- cbior[j]  * 10 / cgrain[j]   
    dmwood[j]  <- cbiow[j]  * 10 / cgrain[j]   
    
    dmcrop[j] <- dmyield[j] + dmstem[j] + dmleaf[j] + dmroot[j]
    
    
    # calculate above ground residue dry matter (Mg / ha) at harvest
    dmresidue[j] <- dmleaf[j] + dmstem[j] + (dmwood[j]*fyield[j])
    
    
    
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
    nconcl[j] <- fnleaf[j] * 100
    nconcs[j] <- fnstem[j] * 100
    nconcr[j] <- fnroot[j] * 100
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
    falll <- falll +  cbiol[j] 
    
    fallr <- fallr + cbior[j] + cbiocr[j] * 0.6 * sumfroot[1,2] # Michel: 23-Out # A estrutura da raiz grossa é similar ap tronco, portanto, parte desse componente (40%) foi adicionado ao fallw para decomposicao no soilbgc
    
    # fallw <- fallw + cbiow[j] * (1 -fyield[j]) + cbiocr[j] * 0.4 * sumfroot[1,2] + cbiob[j] + DBranch_attached
    fallw <- fallw + cbiow[j] * (1 -fyield[j]) + cbiocr[j] * 0.4 * sumfroot[1,2] + cbiob[j] + Deadbranch * deltay/kg_C_M2_to_T_ha
        
    
    
    # print(paste(fallw, cbiow[1], cbiob[1], DBranch_attached,sep = "/"))
    
    
   
    print('cropy = 0 start plant again for Eucalyptus')
    cropy <- 0
    
    
  }  # harvest <- jday
  
  falll <- as.vector(falll )
  fallr <- as.vector(fallr )
  fallw <- as.vector(fallw )
  
  
  # assign("cnleaf", cnleaf, envir = env)
  # assign("cnfroot", cnfroot, envir = env)
  # assign("totts", totts, envir = env)
  
  assign("falll", falll, envir = env)
  assign("fallw", fallw, envir = env)
  assign("fallr", fallr, envir = env)
  assign("crmplant", crmplant, envir = env)
  assign("pdate", pdate, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("cbiocr", cbiocr, envir = env)
  assign("cbiob", cbiob, envir = env)
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
  assign("cbiol", cbiol, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("acroot", acroot, envir = env)
  assign("cropy", cropy, envir = env)

}
