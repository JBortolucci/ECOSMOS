OilPalmPhenocrop <- function(iyear, iyear0, imonth, iday, jday, index) {

  # baset     <- plantList$oilpalm$params$baset
  # mxtmp     <- plantList$oilpalm$params$mxtmp
  laitrans  <- plantList$oilpalm$params$laitrans
  # specla    <- plantList$oilpalm$params$specla
  mxage     <- plantList$oilpalm$params$mxage
  phylini   <- plantList$oilpalm$params$phylini
  mxgddexp  <- plantList$oilpalm$params$mxgddexp
  mxgddveg  <- plantList$oilpalm$params$mxgddveg
  mxgddfill <- plantList$oilpalm$params$mxgddfill
  mxgddmat  <- plantList$oilpalm$params$mxgddmat
  mxgddsen  <- plantList$oilpalm$params$mxgddsen
  # arooti    <- plantList$oilpalm$params$arooti
  # arootf    <- plantList$oilpalm$params$arootf
  paleafi   <- plantList$oilpalm$params$paleafi
  paleaff   <- plantList$oilpalm$params$paleaff
  dmat      <- plantList$oilpalm$params$dmat
  leafalloc <- plantList$oilpalm$params$leafalloc
  bcoef     <- plantList$oilpalm$params$bcoef
  acoef     <- plantList$oilpalm$params$acoef
  ifdisp    <- plantList$oilpalm$params$ifdisp
  mngddlmat <- plantList$oilpalm$params$mngddlmat
  mxgddlmat <- plantList$oilpalm$params$mxgddlmat
  # tauroot   <- plantList$oilpalm$params$tauroot
  # ztopmxPft <- plantList$oilpalm$params$ztopmxPft
  taper     <- plantList$oilpalm$params$taper
  stocking  <- plantList$oilpalm$params$stocking
  dw        <- plantList$oilpalm$params$dw
  fls       <- plantList$oilpalm$params$fls

  i <- index

  if (croplive[i] == 1) {

    gddplant[i] <- gddplant[i] + max(0, min(td - baset[i], mxtmp[i]))

    for(k in 1:nsoilay) {
      aplantn <- aplantn + smsoil[k] + smsoln[k]
    }

    # Oil palm phenology, Carbon Aloccation and Harvest

    idpp[i] <- idpp[i] + 1

    ######################################################################
    ########## Start Allocation to Perenial (Oil palm) crops ############
    ######################################################################

    # Phase 1 completed:

    # Variables initialization
    if(idpp[i] == 1) {
      cbiow[i]  <- 0.0001455428 # kg-C m²
      cbios[i]  <- 0.0002444583 # kg-C m²
      cbior[i]  <- 0.0001482158 # kg-C m²
      cbiol[i]  <- laitrans[i] / specla[i] # kg-C m²
      cbiog[i]  <- 0.0 # kg-C m²
      plai[i]   <- laitrans[i] # Initial LAI of seedlings
      cbiold    <- cbiol[i] # All carbon of leaves is allocated in displayed ones
      cbiols    <- 0.0
      spates$dm <- cbiols
    }

    rm <- min(mxage, idpp[i] / 365.25)

    # The phyllochron grows linearly during the first 10 years after planting.
    # From the tenth year onwards, the phyllochron is 1.5x greater than the initial value.

    if(idpp[i] < 3653) {
      phyl <- phylini[i] + (((phylini[i]*1.5)-phylini[i]) / 3653) * idpp[i]
    } else {
      phyl <- 1.5 * phylini[i]
    }

    if(gddspate / phyl < 1) {
      gddspate <- gddspate + max(0, min(td - baset[i], mxtmp[i]))
    } else {
      gddspate <- 0
      spates <- rbind(spates, 0)
      spates$islive[nrow(spates)] <- 1
    }

    spates$gdd[which(spates$islive == 1)] <- spates$gdd[which(spates$islive == 1)] + max(0, min(td - baset[i], mxtmp[i]))

    if(any(spates$gdd >= mxgddexp)) {

      spates$dpp[which(spates$gdd >= mxgddexp)] <- idpp[i]
      spates$dmmax[which(spates$gdd >= mxgddexp)] <- spates$dm[which(spates$gdd >= mxgddexp)]

      spates$islive[which(spates$gdd >= mxgddexp)] <- 0
      spates$gdd[which(spates$gdd >= mxgddexp)] <- 0

      leaves  <- rbind(leaves, 0)
      fruits <- rbind(fruits, 0)

      leaves$islive[nrow(leaves)] <- 1
      leaves$dpp[nrow(leaves)] <- idpp[i]
      leaves$index[which(leaves$islive == 1)] <- leaves$index[which(leaves$islive == 1)] + 1
      leaves$index[nrow(leaves)] <- 0
      leaves$dry[nrow(leaves)] <- 0
      leaves$n[nrow(leaves)] <- 0

      fruits$dpp[nrow(fruits)] <- idpp[i]

    }

    age <- idpp[i] / 365.25

    fruits$dmmax[which(leaves$index == 8)] <- max(0.0, 0.1588 / (1 + exp(-0.7309 * (age - 3)))) * leaves$dry[which(leaves$index == 8)]

    leaves$gdd[which(leaves$islive == 1)] <- leaves$gdd[which(leaves$islive == 1)] + max(0, min(td - baset[i], mxtmp[i]))

    # outl <- paste(idpp[i], sum(leaves$islive), sep = ",")
    # writeLines(outl,out_leaf)

    if(any(leaves$gdd >= mxgddfill) & (gddplant[i] >= mxgddveg[i])) {
      fruits$ssi[which(leaves$gdd >= mxgddfill)] <- (leaves$gdd[which(leaves$gdd >= mxgddfill)] - mxgddfill) / (mxgddmat - mxgddfill)
    }

    if(any(fruits$hrvday == 0) & any(fruits$ssi >= 1)) {
      fruits$hrvday[which(fruits$hrvday == 0 & fruits$ssi >= 1)] <- idpp[i]
    }

    fruits$ssi[which(fruits$ssi < 0 | fruits$ssi > 1 )] <- 0
    Deadleaves <- 0

    if(any(leaves$gdd >= mxgddsen)) {
      Deadleaves <- sum(leaves$dm[which(leaves$gdd >= mxgddsen)])
      leaves$islive[which(leaves$gdd >= mxgddsen)] <- 0
      leaves$index[which(leaves$gdd >= mxgddsen)] <- NA
      leaves$gdd[which(leaves$gdd >= mxgddsen)] <- 0
    }

    leaves$n[which(leaves$index == 8)] <- leaves$n[which(leaves$index == 8)] + 1
    leaves$dry[which(leaves$index == 8)] <- ((leaves$dry[which(leaves$index == 8)] * (leaves$n[which(leaves$index == 8)] - 1)) +
                                             stresstl) / leaves$n[which(leaves$index == 8)]

    hsum   <- 0
    water  <- 0
    Wcapac <- 0

    for(k in 1:(nsoilay)) {

      hsum <- hsum+hsoi[k]

      # new michel
      mh_aw <- 16.61*(1-exp(-0.00202 * idpp[i]))^(1.5883) # Maximum root depth Christina et al. (2017)
      mh_aw <- min(mh_aw, sum(hsoi))                  # mh_w can not be greater than the last soil layer

      if(hsum <= mh_aw) {
        Wcapac <- Wcapac + 1000*(1.0     * hsoi[k] *  poros[k] )
        water  <- water  + 1000*(wsoi[k] * hsoi[k] *  poros[k] )
      }
    }


    capac <- 30 + 1.2*Wcapac *min(idpp[i]/600,1) #fazer funcao do tempo


    waterfact <-  ((water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin )

    waterfact<-max(min(waterfact,1),0)

    greenfrac[i] <- 1

    aroot[i] <- arooti[i] - ((arooti[i] - arootf[i]) * min(1.0, max(0.0, idpp[i] / (365 * mxage[i]))))
    aroot[i] <- min(1.0, max(0.0, aroot[i]))

    if (gddplant[i] < mxgddveg[i]) {

      aleaf[i] <- paleafi[i] * (1 - aroot[i])
      aleaf[i] <- min(1.0, max(0.0, aleaf[i]))

      paleafmat <- aleaf[i]
      dppmat <- idpp[i]

      arepr[i] <- 0

    } else {

      aleaf[i] <- paleafmat - ((paleafmat - paleaff[i]) *
                                  (((idpp[i] - dppmat[i]) / ( (365 * mxage[i] * dmat[i]) - dppmat[i]))^leafalloc[i]))

      aleaf[i] <- min(1.0, max(0.0, aleaf[i]))

      arepr[i] <- (2.0 / (1.0 + exp(-bcoef[i] * ((1000*amnpp[i]) - 100)))) - acoef[i]

      arepr[i] <- arepr[i] * min((1/6)*age, 1)

      arepr[i] <- min(2.0, max(0.0, arepr[i]))

    }

    astem[i] <- 1 - aroot[i] - aleaf[i]
    astem[i] <- min(1.0, max(0.0, astem[i]))

    reductionfactor <- 1 / (1 + arepr[i])

    aroot[i] <- aroot[i] * reductionfactor
    aleaf[i] <- aleaf[i] * reductionfactor
    astem[i] <- astem[i] * reductionfactor
    arepr[i] <- 1 - aroot[i] - aleaf[i] - astem[i]

    awood[i] <- max(0.0, awood[i])
    aroot[i] <- max(0.0, aroot[i])
    aleaf[i] <- max(0.0, aleaf[i])
    astem[i] <- max(0.0, astem[i])
    arepr[i] <- max(0.0, arepr[i])

    # keep track of total biomass production for the entire year, and the
    aybprod[i] <- aybprod[i] +
      aleaf[i] * max(0.0,adnpp[i]) +
      abranch[i] * max(0.0,adnpp[i]) +
      aroot[i] * max(0.0,adnpp[i]) +
      awood[i] * max(0.0,adnpp[i]) +
      acroot[i] * max(0.0,adnpp[i])

    # aboveground value to calculate harvest index
    ayabprod[i] <- ayabprod[i] +
      aleaf[i] * max(0.0,adnpp[i]) +
      abranch[i] * max(0.0,adnpp[i]) +
      awood[i] * max(0.0,adnpp[i])

    # keep track of annual total root production carbon
    ayrprod[i] <- ayrprod[i] +
      aroot[i] * max(0.0,adnpp[i]) +
      acroot[i] * max(0.0,adnpp[i])

    # keep track of total carbon allocated to
    # leaves for litterfall calculation
    aylprod[i] <- aylprod[i] +
      aleaf[i] * max (0.0, adnpp[i])

    aleaves <- (1.0 - ifdisp) * aleaf[i]
    aleafd <- ifdisp * aleaf[i]

    ind1 <- na.omit(match(spates$dpp[spates$dpp > 0 & spates$dm > 0], leaves$dpp))
    ind2 <- na.omit(match(leaves$dpp[ind1], spates$dpp))


    transl <- pmax(0.0, (spates$dmmax[ind2] / mngddlmat) * (leaves$gdd[ind1] - gddl))
    if (length(transl) == 0) transl <- 0

    spates$dm[ind2] <- pmax(0.0, spates$dm[ind2] - transl)
    leaves$dm[ind1] <- pmax(0.0, leaves$dm[ind1] + transl)

    gddl <- leaves$gdd[match(spates$dpp[spates$dpp > 0 & spates$dm > 0], leaves$dpp)]

    respld <- max(0.0, cbiold - (cbiol[i] * cbiold / (cbiols + cbiold)))
    respls <- max(0.0, cbiols - (cbiol[i] * cbiols / (cbiols + cbiold)))

    cbiold <- max(0.0, cbiold - respld)
    cbiols <- max(0.0, cbiols - respls)

    cbiold <- cbiold + (aleafd * max(0.0, adnpp[i])) - Deadleaves + sum(transl, na.rm = T)
    cbiols <- cbiols + (aleaves * max(0.0, adnpp[i])) - sum(transl, na.rm = T)

    cbiol[i] <- cbiols + cbiold

    ind <- which(leaves$islive == 1 & leaves$gdd < mxgddlmat)

    leaves$dm[ind] <- leaves$dm[ind] + (((aleafd * max(0.0, adnpp[i])) - respld) / sum(leaves$islive[ind]))
    spates$dm[which(spates$islive == 1)] <- spates$dm[which(spates$islive == 1)] + (((aleaves * max(0.0, adnpp[i])) - respls) / sum(spates$islive))

    cbios[i] <- cbios[i] + (fls[i] * astem[i] * max (0.0, adnpp[i]))
    cbiow[i] <- cbiow[i] + ((1.0 - fls[i]) * astem[i] * max (0.0, adnpp[i]))

    cbior[i] <- cbior[i] + (aroot[i] * max (0.0, adnpp[i])) - (cbior[i] / tauroot[i])

    Deadcoroots <-  (cbior[i] / tauroot[i])

    cbiog[i] <- cbiog[i] + arepr[i] * max (0.0, adnpp[i])

    plai[i] <- cbiold * specla[i]

    if(any(fruits$ssi > 0)) {
      areprp <- arepr[i] * fruits$ssi / sum(fruits$ssi)
      fruits$dm[which(fruits$hrvday == 0)] <- fruits$dm[which(fruits$hrvday == 0)] + (areprp[fruits$hrvday == 0] * max(0.0, adnpp[i]))
      if(any(fruits$hrvday == idpp[i])) {
        cbiog[i] <- cbiog[i] - fruits$dm[which(fruits$hrvday == idpp[i])]
        yld <- yld + fruits$dm[which(fruits$hrvday == idpp[i])]
        fruits$dm[which(fruits$hrvday == idpp[i])] <- 0
      }
    }

    Sapwood <- cbiow[i]
    Heartwood <- cbios[i]

    sapfrac <- 0.0 #Sapwood / (Sapwood + Heartwood)

    ztopPft[i] <- (min(ztopmxPft[i], ((cbiow[i] * (taper^2) * 1.0E4)/(stocking * dw * pi))^(1/3)))

    if (rm == mxage) { # maximum harvest date
      croplive[i]  <- 0.0
      greenfrac[i] <- 0.0 # turn all vegetation to brown
      harvdate[i]  <- jday
      plai[i]      <- laitrans[i] # simulates remaining stubble/mulch
      print(paste('Cut oil palm tree = ',cropy,iyear,jday,idpp[i],rm))

      # Finaliza o ciclo
      endCycle <- T
    }

  }

  assign("ztopPft", ztopPft, envir = env)
  assign("sapfrac", sapfrac, envir = env)

  assign("gddplant", gddplant, envir = env)
  assign("gddtsoi", gddtsoi, envir = env)
  assign("aplantn", aplantn, envir = env)
  assign("fleafi", fleafi, envir = env)
  assign("mxgddgf", mxgddgf, envir = env)
  assign("greenfrac", greenfrac, envir = env)
  assign("fleaf", fleaf, envir = env)
  assign("hui", hui, envir = env)
  assign("leafout", leafout, envir = env)
  assign("idpp", idpp, envir = env)
  assign("idpe", idpe, envir = env)
  assign("awood", awood, envir = env)
  assign("aleaf", aleaf, envir = env)
  assign("acroot", acroot, envir = env)
  assign("aroot", aroot, envir = env)
  assign("abranch", abranch, envir = env)
  assign("tlai", tlai, envir = env)
  assign("peaklai", peaklai, envir = env)
  assign("plai", plai, envir = env)
  assign("astem", astem, envir = env)
  assign("aleafi", aleafi, envir = env)
  assign("dpgf", dpgf, envir = env)
  assign("grainday", grainday, envir = env)
  assign("thrlai", thrlai, envir = env)
  assign("templai", templai, envir = env)
  assign("gddemerg", gddemerg, envir = env)
  assign("aerial", aerial, envir = env)
  assign("rm", rm, envir = env)
  assign("af1", af1, envir = env)
  assign("af2", af2, envir = env)
  assign("af3", af3, envir = env)
  assign("af4", af4, envir = env)
  assign("af5", af5, envir = env)
  assign("af6", af6, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiocr", cbiocr, envir = env)
  assign("cbiob", cbiob, envir = env)
  assign("fallrsgc", fallrsgc, envir = env)
  assign("cbior", cbior, envir = env)
  assign("cbiow", cbiow, envir = env)
  assign("biomass", biomass, envir = env)
  assign("ayanpp", ayanpp, envir = env)
  assign("ccdays", ccdays, envir = env)
  assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("Deadwood",Deadwood      , envir = env)
  assign("Deadbranch",Deadbranch    , envir = env)
  assign("DBranch",DBranch    , envir = env)
  assign("Deadfineroots",Deadfineroots , envir = env)
  assign("Deadleaves",Deadleaves    , envir = env)
  assign("Deadcoroots",Deadcoroots   , envir = env)

  assign("cbiold",cbiold, envir = env)
  assign("cbiols",cbiols, envir = env)

  assign("Sapwood",Sapwood  , envir = env)
  assign("Heartwood",Heartwood, envir = env)

  assign("DBranch_attached",DBranch_attached, envir = env)
  assign("DBranch_decay",DBranch_decay, envir = env)
  assign("Signew",Signew, envir = env)

  assign("paleafmat", paleafmat, envir = env)
  assign("dppmat", dppmat, envir = env)
  assign("cbiod", cbiod, envir = env)
  assign("cbios", cbios, envir = env)
  assign("arepr", arepr, envir = env)
  assign("gddspate", gddspate, envir = env)

  assign("leaves",leaves, envir = env)
  assign("spates",spates, envir = env)
  assign("yld", yld, envir = env)
  assign("fruits", fruits, envir = env)
  assign("cbiog", cbiog, envir = env)
  assign("gddl", gddl, envir = env)



}
