



simDataVars$ndiasV6    <- 0
simDataVars$ndiasR0    <- 0
simDataVars$ndiasR4    <- 0
simDataVars$ndiasR9    <- 0
simDataVars$DVS        <- 0
simDataVars$TMAXC      <- 0
simDataVars$TMINC      <- 0
simDataVars$TTSUM      <- 0
simDataVars$DRLVTa     <- 0
simDataVars$ID     <- 0
simDataVars$SRCT     <- 0



MaizeGrowthPheno <- function(iyear, iyear0, imonth, iday, jday, index) {

  environment(calc_dtt) <- env
  
  i <- index
  
  # PARAMETERS #####
  gddf <- 170
  gddt <- 1500
  gdds <- (0.41 * gddt) + 145.4
  laic <- 4
  pden <- 7 # plants per m⁻²
  phyl <- 38.9
  laim <- 0.7
  dsstop <- 1.15
  
  fresp <- list('leaf'  = 0.47, # g-CH2O g-DM⁻¹
                'root'  = 0.45, # g-CH2O g-DM⁻¹
                'stem'  = 0.52, # g-CH2O g-DM⁻¹
                'grain' = 0.49) # g-CH2O g-DM⁻¹
  
  rgrowth <- 0.30
  ##################
  
  greenfrac[i] < -1.0
  
  if (croplive[i] == 1) {
    
    idpp[i] <- idpp[i] + 1
    
    if(idpp[i] == 1) {
      
      cumPh <- 1.0
      xn <- 3.0
      
      cbios[i]  <- 0.00
      cbior[i]  <- 0.00
      cbiol[i]  <- 2.00 * pden * 0.4 * 1e-3
      cbioc[i]  <- 0.00
      plai[i]  <- cbiol[i] * specla[i]
      
      dum8 <- 0.0
      
      gdd8 <- 0.0
      gdd10 <- 0.0
      pla <- 0.0
      leafwb <- 0.0
      sumgdd8 <- 0.0
      
    }
    
    sen <- list()
    
    dtt8     <- calc_dtt(idpp[i], jday,  8, 34)
    dtt10    <- calc_dtt(idpp[i], jday, 10, 34)
    gdd8  <- gdd8  + dtt8
    gdd10 <- gdd10 + dtt10
    
    if (idpp[i] == 1) {
      tlno <- (gdd8 / (phyl * 0.5)) + 6
      P3 <- (tlno - 2.0) * phyl + 96 - gdd8
    }
    
    tmean  <- (tmax + tmin) / 2.0
    fr <- 1.0 / (1.0 - rgrowth)
  
    if (cumPh < 5.0) {
      pc <- 0.66 + 0.068 * cumPh
    } else {
      pc <- 1
    }
    
    ti <- dtt8 / (phyl * pc)
    cumPh <- cumPh + ti
    xn <- max(xn, cumPh + 1)
    
    slfc <- ifelse(test = plai[i] < laic,
                   yes  = 1.0,
                   no   = min(1.0, max(0.0, 1.0 - 0.008 * (plai[i] - laic))))
    
    slft <- ifelse(test = tmean > 279.15,
                   yes  = 1.0,
                   no   = min(1.0, max(0.0, 1.0 - (279.15 - tmean) / 279.15)))
    
    lsr <- ifelse(test = slfc <= slft,
                  yes  = 1.0 - slfc,
                  no   = 1.0 - slft)
    
    if (gdd8 / (gdd8 + P3) > gdd10 / gdds) {
      
      # STAGE 1: from emergence to ´tassel initiation
      
      print(c('rGDD8' = gdd8 / (gdd8 + P3), 'rGDD10' = gdd10 / gdds))
      
      tlno <- (gdd8 / (phyl * 0.5)) + 6
      P3 <- (tlno - 2.0) * phyl + 96 - gdd8
      
      plag <- ifelse(test = xn < 4,
                     yes  = 3.0 * xn * ti,
                     no   = 3.5 * (xn^2) * ti)

      pla <- pla + plag
      leafwt <- pla / (specla[i] * 25) # value 25 is a convertion factor for SPECLA (from m²/kg-C to cm²/g-DM)
      leafwg <- leafwt - leafwb
      leafwb <- leafwt
      xnti <- xn
      
      sen$slan <- gdd8 * pla / 1e4
      sen$plas <- plag * lsr
      sen <- max(unlist(sen))
      pla <- pla - sen
      fsen <- (leafwg - (sen / (specla[i] * 25))) / leafwg # value 25 is a convertion factor for SPECLA (from m²/kg-C to cm²/g-DM)
      
      ds <- max(0.0, min(1.0, gdd10 / gdds))
      
      aroot <- max(0.0, min(0.5, arooti - (ds * (arooti / dsstop))))
      aleaf <- max(0.0, 1.0 - aroot)
      
      cbiorg <- max(0.0, aroot * adnpp[i] * fr / (1.0 + fresp$root))
      cbiolg <- max(0.0, aleaf * adnpp[i] * fr / (1.0 + fresp$leaf))
      
      cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
      cbiol[i] <- cbiol[i] + cbiolg * fsen
      
      # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i]))
      
    } else if (gdd10 < gdds) {
      
      # STAGE 2: from tassel initiation to silking
      
      # print(c('xn' = xn, 'tlno' = tlno))
      
      if (xn <= 12) {
        plag <- 3.5 * xn^2 * ti
        pla <- pla + plag
        leafwg <- 0.00116 * plag * pla^0.25
        stemwg <- leafwg * 0.0182 * (xn - xnti)^2
      } else if (xn > 12 & xn <= tlno - 3) {
        plag <- 595 * ti
        pla <- pla + plag
        leafwg <- 0.00116 * plag * pla^0.25
        stemwg <- leafwg * 0.0182 * (xn - xnti)^2
      } else if(xn > tlno - 3) {
        plag <- 595 * ti / sqrt(xn + 5 - tlno)
        pla <- pla + plag
        leafwg <- 0.00116 * plag * pla^0.25
        stemwg <- 10.85 * ti
      }
      
      sen$slan <- pla / 1e3
      sen$plas <- plag * lsr
      
      sen <- max(unlist(sen))
      pla <- pla - sen
      fsen <- (leafwg - (sen / (specla[i] * 25))) / leafwg # value 25 is a convertion factor for SPECLA (from m²/kg-C to cm²/g-DM)
      
      leafwg <- leafwg / (1.0 + fresp$leaf)
      stemwg <- stemwg / (1.0 + fresp$stem)
      
      ds <- max(0.0, min(1.0, gdd10 / gdds))
      
      aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
      aleaf <- max(0.0, (1.0 - aroot) * (leafwg / (leafwg + stemwg)))
      astem <- max(0.0, 1.0 - aroot - aleaf)
      
      cbiorg <- max(0.0, aroot * adnpp[i] * fr / (1 + fresp$root))
      cbiolg <- max(0.0, aleaf * adnpp[i] * fr / (1 + fresp$leaf))
      cbiosg <- max(0.0, astem * adnpp[i] * fr / (1 + fresp$stem))
      
      cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot[i])
      cbiol[i] <- cbiol[i] + cbiolg * fsen
      cbios[i] <- cbios[i] + cbiosg
      
      # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i]))
      
      dum8 <- gdd8
      
    } else if (gdd8 < dum8 + gddf) {
      
      # STAGE 3: from silking to effective grain filling
      
      sumgdd8 <- sumgdd8 + (dtt8 / (1 - lsr))
      sen <- laim * (sumgdd8 / gddf) ^ sg
      pla <- pla - sen
      
      stemwg <- 0.220 * dtt8 / (1.0 + fresp$stem)
      cobwg  <- 0.088 * dtt8 / (1.0 + fresp$grain)
      
      ds <- max(1.0, min(2.0, 1.0 + (gdd10 - gdds)/(gddt - gdds)))
      
      aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
      acob  <- max(0.0, cobwg * (1.0 - aroot) / (cobwg + stemwg))
      astem <- max(0.0, 1.0 - aroot - acob)
      
      cbiorg <- max(0.0, aroot * adnpp[i] * fr / (1 + fresp$root))
      cbiocg <- max(0.0, acob  * adnpp[i] * fr / (1 + fresp$grain))
      cbiosg <- max(0.0, astem * adnpp[i] * fr / (1 + fresp$stem))
      
      if(cbioc[i] == 0.0) {
        cbioc[i] <- cbios[i] * 0.167
        cbios[i] <- cbios[i] - cbios[i]
      }
      
      cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
      cbioc[i] <- cbioc[i] + cbiocg
      cbios[i] <- cbios[i] + cbiosg
      
      # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbioc' = cbioc[i], 'cbios' = cbios[i]))
      
    } else if (gdd10 < gddt) {
      
      # STAGE 4: from effective grain filling to physiological maturity
      
      sumgdd8 <- sumgdd8 + (dtt8 / (1 - lsr))
      sen <- laim * (sumgdd8 / gddf) ^ sg
      
    } else {
      
      
      endCycle <- T
      
    }
    
    #    !----------Check sink limitation based on yesterday's growth rates
    # ! and adapt partitioning of stem-storage organ accordingly
    # BRYAN TO DO ->  IF (GRAINS) THEN
    # BRYAN TO DO ->  IF (GGR.GE.(PWRR-WRR)) THEN
    # BRYAN TO DO ->  FSO = MAX(0.,(PWRR-WRR)/(GCR*FSH))
    # BRYAN TO DO ->  FST = 1.-FSO-FLV
    # BRYAN TO DO ->  END IF
    # BRYAN TO DO ->  END IF
    
    # update vegetation's physical characteristics
    plai[i] <- max(0.0, cbiol[i] * specla[i])
    print(c('lai' = plai[i]))
    
    peaklai[i]  <- max(peaklai[i], plai[i])
    
    greenfrac[i] <- 1.0
    
    
    biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbioc[i]
    
    # keep track of aboveground annual npp
    ayanpp[i] <- ayanpp[i] + adnpp[i]
    
    
    #END TEST Maize MODEL FROM ORYZA
    #____________________________________
    
    #_____________________________________________
    
    # keep track of total biomass production for the entire year, and the
    aybprod[i] <- aybprod[i] +
      aleaf[i] * max(0.0, adnpp[i] * fr / (1 + fresp$leaf)) +
      aroot[i] * max(0.0, adnpp[i] * fr / (1 + fresp$root)) +
      astem[i] * max(0.0, adnpp[i] * fr / (1 + fresp$stem)) +
      acob[i]  * max(0.0, adnpp[i] * fr / (1 + fresp$grain))
    
    # aboveground value to calculate harvest index
    ayabprod[i] <- ayabprod[i] +
      aleaf[i] * max(0.0, adnpp[i] * fr / (1 + fresp$leaf)) +
      astem[i] * max(0.0, adnpp[i] * fr / (1 + fresp$stem)) +
      acob[i]  * max(0.0, adnpp[i] * fr / (1 + fresp$grain))
    
    
    # keep track of annual total root production carbon
    ayrprod[i] <- ayrprod[i] +
      aroot[i] * max(0.0, adnpp[i] * fr / (1 + fresp$root))
    
    
    # keep track of total carbon allocated to
    # leaves for litterfall calculation
    aylprod[i] <- aylprod[i] +
      aleaf[i] * max(0.0, adnpp[i] * fr / (1 + fresp$leaf))

    #####################################################################
    # check for climatic and phenological limits on maturity, growth,
    # and harvest date
    #
    
    #    if (tmin <= tkill[i]) {
    #      ccdays[i] <- ccdays[i] + 1
    #    } else {
    #      ccdays[i] <- 0
    #    }
    #
    #    if (ccdays[i] >= 1 &&
    #        hui[i] >= 0.6 * gddmaturity[i] &&
    #        croplive[i] == 1) {
    #      croplive[i]     <- 0.0
    #      print(paste0('tkill!!!!!',1,iyear,jday,idpp[i]))
    #      harvdate[i]     <- jday
    #    }
    
    
    
    #___________________________________________________
    #       Harvest
    
    fileout=paste("Maize_DAILY.csv")
    # ID<-simConfigs[[i]]$id
    if(idpp[i]==1)ID<-paste0(jday,iyear)
    write(paste( ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS ,
                 aroot[i],aleaf[i],astem[i],arepr[i],cbior[i],cbiol[i],cbios[i],cbiog[i],cbiop[i],plai[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
    
    
    
    if(cropy == 1) {
      
      if ( DVS >= 2.0 ) { # maximum harvest date
        
        print(paste('Harvest Maize ',ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai[i],cbiog[i],sep = " ; "    ))
        
        fileout=paste("Maize_SEASON.csv")
        write(paste(ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai,cbiog[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
        
        
        
        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        greenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        peaklai[i]    <- 0.0
        endCycle      <- T
        
        ndiasV6       <-0
        ndiasR0       <-0
        ndiasR4       <-0
        ndiasR9       <-0
        DVS           <-0
        TMAXC         <-0
        TMINC         <-0
        TTSUM         <-0
        
        
      }
    } else {
      print('Maize has only one cycle - Stop')
      stop()
    }
    
    assign('gdd8'    , gdd8    , envir = env)
    assign('gdd10'   , gdd10   , envir = env)
    assign('cumPh'   , cumPh   , envir = env)
    assign('pla'     , pla     , envir = env)
    assign('leafwb'  , leafwb  , envir = env)
    assign('dum8'    , dum8    , envir = env)
    assign('xnti'    , xnti    , envir = env)
    assign('sumgdd8' , sumgdd8 , envir = env)
    assign('xn'      , xn      , envir = env)
    assign('P3'      , P3      , envir = env)
    assign('tlno'    , tlno    , envir = env)
    
  }
  
  #TO DO: Alexandre -
  ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i]

  assign("endCycle", endCycle, envir = env)
  assign("ztopPft", ztopPft, envir = env)
  assign("greenfrac", greenfrac, envir = env)
  assign("idpp", idpp, envir = env)
  assign("idpe", idpe, envir = env)
  assign("aroot", aroot, envir = env)
  assign("aleaf", aleaf, envir = env)
  assign("astem", astem, envir = env)
  assign("arepr", arepr, envir = env)
  assign("cbiol", cbiol, envir = env)
  assign("cbiog", cbiog, envir = env)
  assign('cbioc'   , cbioc   , envir = env)
  assign("cbiop", cbiop, envir = env)
  assign("cbios", cbios, envir = env)
  assign("cbior", cbior, envir = env)
  assign("plai", plai, envir = env)
  assign("peaklai", peaklai, envir = env)
  assign("aerial", aerial, envir = env)
  assign("aybprod", aybprod, envir = env)
  assign("ayabprod", ayabprod, envir = env)
  assign("ayrprod", ayrprod, envir = env)
  assign("aylprod", aylprod, envir = env)
  assign("biomass", biomass, envir = env)
  assign("ayanpp", ayanpp, envir = env)
  assign("croplive", croplive, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("cropy", cropy, envir = env)
  assign("DRLVTa",DRLVTa, envir = env)
  assign("ndiasV6",ndiasV6,envir = env)
  assign("ndiasR0",ndiasR0,envir = env)
  assign("ndiasR4",ndiasR4,envir = env)
  assign("ndiasR9",ndiasR9,envir = env)
  assign("DVS"    ,DVS    ,envir = env)
  assign("TMAXC"  ,TMAXC  ,envir = env)
  assign("TMINC"  ,TMINC  ,envir = env)
  assign("TTSUM"  ,TTSUM  ,envir = env)
  assign("ID"  ,ID  ,envir = env)
  assign("SRCT"  ,SRCT  ,envir = env)
  
}

calc_dtt <- function(dpp, jday, ldtt, udtt) {
  
  dtt <- 0.0
  ldtt <- ldtt + 273.15
  udtt <- udtt + 273.15
  
  tmean  <- (tmax + tmin) / 2.0
  alpha  <- (tmax - tmin) / 2.0
  
  theta1 <- asin((pi / 180) * (ldtt - tmean) / alpha)
  theta2 <- asin((pi / 180) * (udtt - tmean) / alpha)
  
  if(tmin >= ldtt & tmax <= udtt) {
    dtt <- tmean - ldtt
  } else if (tmin >= ldtt & tmax > udtt) {
    dtt <- (1/pi) * ((tmean - ldtt) * (theta2 - theta1) +
                       (udtt - ldtt) * ((pi/2) - theta2) - cos(theta2))
  } else if (tmin < ldtt & tmax <= udtt) {
    dtt <- (1/pi) * ((udtt - ldtt) * ((pi/2)-theta2) +
                       alpha * cos(theta2))
  } else if (tmin < ldtt & tmax > udtt) {
    dtt <- (1/pi) * ((tmean - ldtt) * (theta2 - theta1) +
                       alpha * (cos(theta1) - cos(theta2)) +
                       (udtt - ldtt) * ((pi/2) - theta2))
  } else if (tmin > udtt & tmax > udtt) {
    dtt <- udtt - ldtt
  } else if (tmin < ldtt & tmax < ldtt){
    dtt <- 0.0
  }
  
  return(dtt)
  
}

