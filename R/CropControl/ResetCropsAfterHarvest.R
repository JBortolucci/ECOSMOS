


ResetCropsAfterHarvest <- function() {
  
  # TODO: Aplicado para todas as culturas. Fazer loop no futuro
  # RESET CROP VARIABLES
  for(i in seq(1,npft)) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$type == CROPS) {
      if(harvdate[i] == jday) {
        
        plai[i] <- 0.01
        thrlai[i] <- 0
        peaklai[i] <- 0
        ccdays[i]  <- 0
        cbiol[i]   <- 0
        cbior[i]   <- 0
        cbios[i]   <- 0
        cbiog[i]   <- 0
        cbiow[i]   <- 0
        
        # ADICIONADO NOVO
        cbiocr[i] <- 0
        cbiob[i]  <- 0
        
        hui[i]     <- 0
        aybprod[i] <- 0
        ayrprod[i]  <- 0
        ayabprod[i] <- 0
        aylprod[i] <- 0
        leafout[i] <- 0
        htmx[1] <- 0      # ou 2
        cumlvs[i] <- 0
        plaimx[i] <- 0
        dpgf[i] <- 0
        biomass[i] <- 0
        totnuptake[i] <- 0
        tnplant[i] <- 0
        totnfix[i] <- 0
        idpp[i] <- 0
        idpe[i] <- 0
        gddplant[i] <- 0
        gddtsoi[i] <- 0
        sai[1]     <- 0   # ou 2
        fu         <- 0
        lai[1]     <- 0  # ou 2
        zbot[1] <- 0    # ou 2
        ztop[1] <- 0    # ou 2
        totbiol <- 0
        totlail <- 0
        vf <- 0  # vernalization factor for winter wheat
        arepr[i] <- 0
        
        # ADICIONADO NOVO
        acroot[i] <- 0
        
        idop[i] <- 999
        grainday[i] <- 9999
        
        # JAIR: Não estava resetando a cana por causa do ratoon.
        harvdate[i] <- 999
        assign("harvdate", harvdate, envir = env)
        
        # ADICIONADO NOVO
        assign("cbiocr", cbiocr, envir = env)
        assign("cbiob", cbiob, envir = env)
        
        assign("acroot", acroot, envir = env)
        assign("ayabprod", ayabprod, envir = env)
        assign("cbiog", cbiog, envir = env)
        assign("cbios", cbios, envir = env)
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
        assign("arepr", arepr, envir = env)
        assign("idop", idop, envir = env)
        assign("grainday", grainday, envir = env)
        
      }
    }
  }
  
}