


ResetCropsAfterHarvest <- function() {
  
  # TODO: Aplicado para todas as culturas. Fazer loop no futuro
  # RESET CROP VARIABLES
  for(i in seq(1,npft)) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$type == CROPS) {
      if(harvdate[i] == jday) {
        
        plai[i]    <- 0.01
        cbiol[i]   <- 0
        cbios[i]   <- 0
        cbiog[i]   <- 0
        cbiow[i]   <- 0
        cbiob[i]   <- 0  
        cbior[i]   <- 0   
        cbiocr[i]  <- 0


        
        aybprod[i]    <- 0
        ayrprod[i]    <- 0
        ayabprod[i]   <- 0
        aylprod[i]    <- 0
        biomass[i]    <- 0
        totnuptake[i] <- 0
        tnplant[i]    <- 0
        totnfix[i]    <- 0
        idpp[i]       <- 0
        gddplant[i]   <- 0
        arepr[i]      <- 0
        

# TO DO: Victor, Verificar se essas variaveis sao usadas antes de serem resetadas 
        harvdate[i]   <- 999
        dmyield[i]    <- 0
        dmleaf[i]     <- 0
        dmstem[i]     <- 0
        dmroot[i]     <- 0
        dmresidue[i]  <- 0
        dmcrop[i]     <- 0
        residuen[i]   <- 0
        nconcl[i]     <- 0
        nconcs[i]     <- 0
        nconcr[i]     <- 0
        nconcg[i]     <- 0
        cropn[i]      <- 0
        cropfixn[i]   <- 0
        cntops[i]     <- 40
        cnroot[i]     <- 60
        fertinput[i]  <- 0

        assign("harvdate", harvdate, envir = env)
        assign("dmyield", dmyield, envir = env)
        assign("dmleaf", dmleaf, envir = env)
        assign("dmstem", dmstem, envir = env)
        assign("dmroot", dmroot, envir = env)
        assign("dmresidue", dmresidue, envir = env)
        assign("dmcrop", dmcrop, envir = env)
        assign("residuen", residuen, envir = env)
        assign("nconcl", nconcl, envir = env)
        assign("nconcs", nconcs, envir = env)
        assign("nconcr", nconcr, envir = env)
        assign("nconcg", nconcg, envir = env)
        assign("cropn", cropn, envir = env)
        assign("cropfixn", cropfixn, envir = env)
        assign("cntops", cntops, envir = env)
        assign("cnroot", cnroot, envir = env)

        # TO DO: Victor Verificar se essas variaveis do bloco acima sao usadas antes de serem resetadas 
        
        
        
        assign("harvdate", harvdate, envir = env)
        assign("plai", plai, envir = env)
        assign("cbiog", cbiog, envir = env)
        assign("cbios", cbios, envir = env)
        assign("cbiol", cbiol, envir = env)
        assign("cbior", cbior, envir = env)
        assign("cbiow", cbiow, envir = env)
        assign("cbiocr", cbiocr, envir = env)
        assign("cbiob", cbiob, envir = env)  
        assign("ayabprod", ayabprod, envir = env)
        assign("aybprod", aybprod, envir = env)
        assign("ayrprod", ayrprod, envir = env)
        assign("aylprod", aylprod, envir = env)
        assign("biomass", biomass, envir = env)
        assign("totnuptake", totnuptake, envir = env)
        assign("tnplant", tnplant, envir = env)
        assign("totnfix", totnfix, envir = env)
        assign("idpp", idpp, envir = env)
        assign("gddplant", gddplant, envir = env)
        

      }
    }
  }
  
}