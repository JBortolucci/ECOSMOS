#to do: Santiago, residuo tem que ser indepente se é uma cultura agricola
# juntar com o residuo dos ecossistemas naturais

EucaliptoCropresidue <- function (year, year0, jday, index) {
  
  deltay    <- plantList$eucalipto$params$deltay 
  
  j <- index
  
  # zero out litter fall rates

  
  if(croplive[j] == 1) {
    falll <- falll + (Deadleaves) * deltay/kg_C_M2_to_T_ha
    fallr <- fallr + (Deadcoroots + Deadfineroots) * deltay/kg_C_M2_to_T_ha  * sumfroot[1,2] # Michel: 23-Out
    fallw <- fallw + (Deadwood) * deltay/kg_C_M2_to_T_ha + DBranch_decay
  }
  
  # calculate CRM values from Pioneer regression relationships 
  crmclim[j] <- max(73, min((gddmaturity[j] + 53.683) / 13.882,135))
  
  
  # gddplant gets reinitialized to 0 at maturity date so save value here
  if(gddplant[j] > 0 && croplive[j] == 1) {
    crmplant[j] <- max(73, min((gddplant[j] + 53.683) / 13.882,135))
  }
  
  # only write out values at harvest date, and re-initialize crop variables
  # at this time  - this allows for the same crop (e.g., wheat) to be grown
  # across two consecutive calendar years   
  
  if(exist[j] == 1 && harvdate[j] == jday) {
    
    pdate[j] <- idop[j]
    idppout[j] <- idpp[j]
    hdate[j] <- harvdate[j]
    
    ayabprod[j] <- max(cbiol[j], ayabprod[j])
    
    # added so division by zero doesn't take place in those places where
    # production was zero 
    
    # adjust actual grain yield value (params.crp)
    dumg <- cbiocr[j] 
    cbiocr[j] <- cbiocr[j] * fyield[j] 
    
    # add excess (i.e. pod of soybeans) to stem storage pool of plant 
    cbiob[j] <- max(0, cbiob[j] + (dumg - cbiocr[j]))
    
    # impose upper limit on harvest index as per JMN suggestion 
    # if harvest index is > allowable, put excess to stem which will add to more
    # litterfall and adjust the grain accordingly
    # might have to revisit logic with respect to what actually get harvested, versus
    # what is left in the field after harvest as litter input to the soil 
    
    # to do: Santiago, verificar essa bloco                
    harvidx[j] <- cbiocr[j] / ayabprod[j]
    
    croplaimx[j] <- plaimx[j]
    
    if(harvidx[j] > maxhi[j]) {
      harvidx[j] <- maxhi[j] 
      dumg <- cbiocr[j] 
      cbiocr[j] <- maxhi[j] * ayabprod[j]
      
      # add excess to stem of plant
      cbiob[j] <- cbiob[j] + (dumg - cbiocr[j])
    }
    
    
    # calculate n in grain[kg / ha]
    grainn[j] <- (cbiocr[j] / cfrac[j]) * fngrain[j] * 1e+04
    
    # to do: Santiago, inserir um parametro inserido no params.crp 
    #      que represente o conteudo de agua no material colhido   
    
    # calculate crop fresh yield  t / ha of fresh weight
    cropyld[j] <- cbiow[j]  * fyield[j] * (1 / 0.9) * 10 * (1 / cgrain[j])
    
    
    
    # calculate dry matter material (Mg / ha); yield in Mg / ha dry matter (sucrose carbon)
    dmyield[j] <- cbiocr[j] * 10 / cgrain[j]  
    dmstem[j] <- cbiob[j] * 10 / cgrain[j]   
    dmleaf[j] <- cbiol[j] * 10 / cgrain[j]   
    dmroot[j] <- cbior[j] * 10 / cgrain[j]   
    dmwood[j] <- cbiow[j] * 10 / cgrain[j]   
    
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
    
    fallw <- fallw + cbiow[j] * (1 -fyield[j]) + cbiocr[j] * 0.4 * sumfroot[1,2] + cbiob[j] + DBranch_attached
    
    
    
    # print(paste(fallw, cbiow[1], cbiob[1], DBranch_attached,sep = "/"))
    
    
    # re-initialize crop variables
    # for current crop at harvest date 
    # plai[j] <- 0.01
    # thrlai[j] <- 0
    # peaklai[j] <- 0                    
    # ccdays[j] <- 0
    # cbiol[j] <- 0
    # cbior[j] <- 0	
    # cbiob[j] <- 0
    # cbiocr[j] <- 0
    # cbiow[j] <- 0
    # hui[j] <- 0
    # aybprod[j] <- 0
    # ayrprod[j] <- 0
    # ayabprod[j] <- 0
    # aylprod[j] <- 0
    # leafout[j] <- 0
    # htmx[1] <- 0
    # cumlvs[j] <- 0
    # plaimx[j] <- 0
    # dpgf[j] <- 0
    # biomass[j] <- 0
    # totnuptake[j] <- 0
    # tnplant[j] <- 0
    # totnfix[j] <- 0
    # idpp[j] <- 0
    # idpe[j] <- 0
    # gddplant[j] <- 0
    # gddtsoi[j] <- 0
    # sai[1] <- 0
    # fu <- 0
    # lai[1] <- 0
    # zbot[1] <- 0
    # ztop[1] <- 0
    # totbiol <- 0
    # totlail <- 0  
    # vf <- 0  # vernalization factor for winter wheat
    # acroot[j] <- 0
    # idop[j] <- 999
    # grainday[j] <- 9999
    # 
    # cnleaf  <- max(40, cntops[j])   # calculated in crop residue at harvest
    # cnfroot <- max(60, cnroot[j])   # calculated in crop residue at harvest  
    # totts   <- 1
    # 
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
  assign("fallrsgc", fallrsgc, envir = env)
  assign("crmclim", crmclim, envir = env)
  assign("crmact", crmact, envir = env)
  assign("crmplant", crmplant, envir = env)
  assign("pdate", pdate, envir = env)
  assign("idppout", idppout, envir = env)
  assign("hdate", hdate, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("cbiocr", cbiocr, envir = env)
  assign("cbiob", cbiob, envir = env)
  assign("harvidx", harvidx, envir = env)
  assign("croplaimx", croplaimx, envir = env)
  assign("grainn", grainn, envir = env)
  assign("cropyld", cropyld, envir = env)
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
  assign("plai", plai, envir = env)
  assign("thrlai", thrlai, envir = env)
  assign("peaklai", peaklai, envir = env)
  assign("ccdays", ccdays, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("hui", hui, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  assign("leafout", leafout, envir = env)
  assign("htmx", htmx, envir = env)
  assign("cumlvs", cumlvs, envir = env)
  assign("plaimx", plaimx, envir = env)
  assign("dpgf", dpgf, envir = env)
  assign("biomass", biomass, envir = env)
  assign("totnuptake", totnuptake, envir = env)
  assign("tnplant", tnplant, envir = env)
  assign("totnfix", totnfix, envir = env)
  assign("idpp", idpp, envir = env)
  assign("idpe", idpe, envir = env)
  assign("gddplant", gddplant, envir = env)
  assign("gddtsoi", gddtsoi, envir = env)
  assign("sai", sai, envir = env)
  assign("fu", fu, envir = env)
  assign("lai", lai, envir = env)
  assign("zbot", zbot, envir = env)
  assign("ztop", ztop, envir = env)
  assign("totbiol", totbiol, envir = env)
  assign("totlail", totlail, envir = env)
  assign("vf", vf, envir = env)
  assign("acroot", acroot, envir = env)
  assign("idop", idop, envir = env)
  assign("grainday", grainday, envir = env)
  assign("gddsgcp", gddsgcp, envir = env)
  assign("gddsgcr", gddsgcr, envir = env)
  assign("hybgdd"  ,hybgdd  , envir = env)  
  # assign("gddwwh" ,gddwwh , envir = env) 
  assign("cropy", cropy, envir = env)
  assign("croplive", croplive, envir = env)
  assign("gddmaturity", gddmaturity, envir = env)
  assign("avehybrid", avehybrid, envir = env)
  
}
