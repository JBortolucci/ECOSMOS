
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

simDataVars$cumPh  <-  0
simDataVars$xn  <-  0
simDataVars$gdd8  <-  0
simDataVars$gdd10  <-  0
simDataVars$sumgdd8  <-  0
simDataVars$pla  <-  0
simDataVars$leafwb  <-  0
simDataVars$dtt8  <-  0
simDataVars$dtt10  <-  0
simDataVars$tlno  <-  0
simDataVars$P3  <-  0
simDataVars$fr  <-  0
simDataVars$slfc  <-  0
simDataVars$slft  <-  0
simDataVars$lsr  <-  0
simDataVars$plag  <-  0
simDataVars$leafwt  <-  0
simDataVars$leafwg  <-  0
simDataVars$leafwb  <-  0
simDataVars$xnti  <-  0
simDataVars$ds  <-  0
simDataVars$stemwg  <-  0
simDataVars$cobwg  <-  0
  

MaizeGrowthPheno <- function(iyear, iyear0, imonth, iday, jday, index) {

  environment(calc_dtt) <- env
  
  i <- index
  
  # PARAMETERS #####
  gddgerm  <- 15
  gddf <- 170
  gddt <- 1500
  gdds <- (0.41 * gddt) + 145.4
  laic <- 4
  sg   <- 4.0
  pden <- config$plant1$plantPop # plants per m⁻²
  pdpt <- config$plant1$plantingDepht
  phyl <- 38.9
  laim <- 0.7
  dsstop <- 1.15
  
  fresp <- list('leaf'  = 0.47, # g-CH2O g-DM⁻¹
                'root'  = 0.45, # g-CH2O g-DM⁻¹
                'stem'  = 0.52, # g-CH2O g-DM⁻¹
                'grain' = 0.49) # g-CH2O g-DM⁻¹
  
  rgrowth <- 0.30
  G2 <- 676 # kernels ear⁻¹
  G5 <- 8.7 # mg day⁻¹
  efftrans <- 0.26 # Efficiency of carbon translocation from leaves/stem to grain filling
  gddemerg <- 6.0
  ##################
  
  greenfrac[i] <- 0.0
  
  if (croplive[i] == 1) {
    
    idpp[i] <- idpp[i] + 1
    
    if(idpp[i] == 1) {  # Zerando variáveis no plantio
      
      # cumPh <- 1.0
      # xn <- 3.0
      
      # cbios[i]  <- 0.00
      # cbior[i]  <- 0.00
      # cbiol[i]  <- 0.00 # 2.00 * pden * 0.4 * 1e-3
      # cbioc[i]  <- 0.00
      # plai[i]   <- 0.00 # cbiol[i] * specla[i]
      
      dum8 <- 0.0
      
      gdd8 <- 0.0
      gdd10 <- 0.0
      gdd10p <- 0.0
      pla <- 0.0
      leafwb <- 0.0
      sumgdd8 <- 0.0
      
      ndaysS3 <- 0.0
      sumP <- 0.0
      fcbios <- 0.0
      fcbiol <- 0.0
     
      gddemerg <- gddemerg * pdpt
      
      plaf <- 0.0
       
    }
    
    ##############################
    # From sowing to germination #
    ##############################
    
    if (gdd10p <= gddgerm + gddemerg) {
      
      dtt10p <- calc_dtt(idpp[i], jday, 10, 34)
      gdd10p <- gdd10p + dtt10p
      
      if (gdd10p <= gddgerm) date_germ <- idpp[i]
      
      date_emerg <- idpp[i]
      
      if (idpp[i] > 25) { # TODO: Parar o ciclo
        print(paste0('Crop failure: plants did not emerge until 25 days after sowing.'))
      }
    
    #################################
    # From germination to emergence #
    #################################
      
    } else if (idpp[i] == date_emerg) {
      
      # Initialize variables at emergence
      cumPh <- 1.0
      xn <- 3.0
      
      cbiol[i]  <- 2.00 * pden * 0.4 * 1e-3
      plai[i]   <- cbiol[i] * specla[i]
      
    } else {
    
      sen <- list()
      
      fr <- 1.0 / (1.0 - rgrowth)
      
      adnppl <- adnpp[i] * fr / (1 + fresp$leaf)
      adnpps <- adnpp[i] * fr / (1 + fresp$stem)
      adnppr <- adnpp[i] * fr / (1 + fresp$root)
      adnppg <- adnpp[i] * fr / (1 + fresp$grain)
      
      dtt8  <- calc_dtt(dpp = idpp[i], jday = jday, ldtt =  8, udtt = 34)
      dtt10 <- calc_dtt(dpp = idpp[i], jday = jday, ldtt = 10, udtt = 34)
      gdd8  <- gdd8  + dtt8
      gdd10 <- gdd10 + dtt10
      
      if (idpp[i] == 1) {
        tlno <- (gdd8 / (phyl * 0.5)) + 6
        P3 <- (tlno - 2.0) * phyl + 96 - gdd8
      }
      
      tmean  <- (tmax + tmin) / 2.0
    
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
        
        ################################################
        # STAGE 1: from emergence to tassel initiation #
        ################################################
        
        tlno <- (gdd8 / (phyl * 0.5)) + 6
        P3 <- (tlno - 2.0) * phyl + 96 - gdd8
        
        plag <- ifelse(test = xn < 4,
                       yes  = 3.0 * xn * ti,
                       no   = 3.5 * (xn^2) * ti)
  
        pla <- pla + plag
        leafwt <- pla / (specla[i] * 10) # 10 (convertion SPECLA from m²/kg DM to cm²/g DM)
        leafwg <- leafwt - leafwb
        leafwb <- leafwt
        xnti <- xn
        
        sen$slan <- gdd8 * pla / 1e4
        sen$plas <- plag * lsr
        sen <- max(unlist(sen))
        fsen <- sen / pla
        pla <- pla - sen
        leafwg <- leafwg / (1.0 + fresp$leaf)
        
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        
        aroot <- max(0.0, min(0.5, arooti - (ds * (arooti / dsstop))))
        aleaf <- max(0.0, 1.0 - aroot)
        
        cbiorg <- max(0.0, aroot * adnppr)
        cbiolg <- max(0.0, aleaf * adnppl)
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        
        cat('\nStage 1\n')
        print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
      } else if (gdd10 < gdds) {
        
        ##############################################
        # STAGE 2: from tassel initiation to silking #
        ##############################################
        
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
        
        leafwg <- leafwg / (1.0 + fresp$leaf)
        stemwg <- stemwg / (1.0 + fresp$stem)
        
        sen$slan <- gdd8 * pla / 1e4
        sen$plas <- plag * lsr
        sen <- max(unlist(sen))
        fsen <- sen / pla
        pla <- pla - sen
        
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        
        aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        aleaf <- max(0.0, (1.0 - aroot) * (leafwg / (leafwg + stemwg)))
        astem <- max(0.0, 1.0 - aroot - aleaf)
        
        cbiorg <- max(0.0, aroot * adnppr)
        cbiolg <- max(0.0, aleaf * adnppl)
        cbiosg <- max(0.0, astem * adnpps)
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot[i])
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        cbios[i] <- cbios[i] + cbiosg
        
        cat('\nStage 2\n')
        print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
        dum8 <- gdd8
        plaf <- pla
        
      } else if (gdd8 < dum8 + gddf) {
        
        ####################################################
        # STAGE 3: from silking to effective grain filling #
        ####################################################
        
        sen$slan <- pla / 1e3
        sumgdd8 <- sumgdd8 + (dtt8 / (1 - lsr))
        sf <- laim * ((sumgdd8 / gddf) ^ sg)
        sen$plas <- plaf * sf
        sen <- max(unlist(sen))
        pla <- pla - sen
        fsen <- sen / plaf
        
        stemwg <- 0.220 * dtt8 / (1.0 + fresp$stem)
        cobwg  <- 0.088 * dtt8 / (1.0 + fresp$grain)
        
        ds <- max(1.0, min(2.0, 1.0 + (gdd10 - gdds)/(gddt - gdds)))
        
        aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        acob  <- max(0.0, cobwg * (1.0 - aroot) / (cobwg + stemwg))
        astem <- max(0.0, 1.0 - aroot - acob)
        
        cbiorg <- max(0.0, aroot * adnppr)
        cbiocg <- max(0.0, acob  * adnppg)
        cbiosg <- max(0.0, astem * adnpps)
        
        sumP <- sumP + cbiorg + cbiocg + cbiosg
        
        if(cbioc[i] == 0.0) {
          cbioc[i] <- cbios[i] * 0.167
          cbios[i] <- cbios[i] - cbios[i]
        }
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbioc[i] <- cbioc[i] + cbiocg
        cbios[i] <- cbios[i] + cbiosg
        cbiol[i] <- cbiol[i] - cbiol[i] * fsen
        # cbiol[i] <- 
        # TODO (Victor): Implement the senescence biomass fall in stage 3
        
        ndaysS3 <- ndaysS3 + 1
        
        fcbios <- cbios[i] * 0.60
        fcbiol <- cbiol[i] * 0.15
        
        cat('\nStage 3\n')
        print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
      } else if (gdd10 < gddt) {
        
        ###################################################################
        # STAGE 4: from effective grain filling to physiological maturity #
        ###################################################################
        
        psker <- sumP * 1e3 / ndaysS3 * 3.4 / 5.0
        gpp <- G2 * (676 / (psker/1e3))
        
        filleffi <- 1.47 - (0.09 * pden) + (0.0036 * pden^2)
        
        RGfill <- sum(sapply(X = 1:8, FUN = function(x) {
          tmfac <- 0.931 + 0.114 * x - 0.0703 * x^2 + 0.0053 * x^3
          ttmp <- (tmin - 273.15) + tmfac * (tmax - tmin)
          RGfill <- ifelse(test = ttmp > 6,
                           yes  = (1.0 - 0.0025 * (ttmp - 26.0)^2) / 8.0,
                           no   = 0.0)
          # return(RGfill)
        }))
        
        grainwg <- (RGfill * gpp * G5 * filleffi * 0.001) / (1 + fresp$grain)
        grainwg <- 0.40 * grainwg * pden / 1e3 # convert from g-DM plant⁻¹ day⁻¹ to kg-C m⁻² day⁻¹
        
        if (grainwg > adnppg & fcbios >= abs(grainwg - adnppg)) {
          transs <- abs(grainwg - adnppg)
          grainwg <- grainwg + efftrans * transs
          fcbios <- fcbios - efftrans * transs
          cbios[i] <- cbios[i] - transs * efftrans
        } else {
          transl <- - min(0.005 * fcbiol, abs(grainwg - adnppg))
          grainwg <- grainwg + efftrans * transl
          fcbiol <- fcbiol - efftrans * transl
          cbiol[i] <- cbiol[i] - transl * efftrans
        }
        
        if (grainwg < adnppg) {
          fcbiol <- fcbiol + abs(grainwg - adnppg)
          cbiol[i] <- cbiol[i] + abs(grainwg - adnppg)
        }
        
          cbiog[i] <- cbiog[i] + grainwg
        
          cat('\nStage 4\n')
          print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
      }
      
      # update vegetation's physical characteristics
      # plai[i] <- min(max(0.0, cbiol[i] * specla[i]),5) #TODO check
      plai[i] <- max(0.0, cbiol[i] * specla[i])
      # print(c('lai' = plai[i]))
      
      peaklai[i]  <- max(peaklai[i], plai[i])
      
      greenfrac[i] <- 1.0
      
      biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbioc[i]
      # print(c('biomass' = biomass[i]))
      
      # keep track of aboveground annual npp
      ayanpp[i] <- ayanpp[i] + adnpp[i]
      
      # keep track of total biomass production for the entire year, and the
      aybprod[i] <- aybprod[i] +
        aleaf[i] * max(0.0, adnppl) +
        aroot[i] * max(0.0, adnppr) +
        astem[i] * max(0.0, adnpps) +
        acob[i]  * max(0.0, adnppg)
      
      # aboveground value to calculate harvest index
      ayabprod[i] <- ayabprod[i] +
        aleaf[i] * max(0.0, adnppl) +
        astem[i] * max(0.0, adnpps) +
        acob[i]  * max(0.0, adnppg)
      
      
      # keep track of annual total root production carbon
      ayrprod[i] <- ayrprod[i] +
        aroot[i] * max(0.0, adnppr)
      
      
      # keep track of total carbon allocated to
      # leaves for litterfall calculation
      aylprod[i] <- aylprod[i] +
        aleaf[i] * max(0.0, adnppl * fr / (1 + fresp$leaf))
  
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
      
      fileout <- paste("Maize_DAILY.csv")
      
      if(idpp[i] == 1) ID <- paste0(jday, iyear)
      write(x = paste(ID, idpp[i], aroot[i], aleaf[i], astem[i], cbior[i], cbiol[i], cbios[i], cbioc[i], plai[i], sep = ";"),
            file = fileout,
            append = TRUE,
            sep = "\n")
    
    }
    
    
    if(cropy == 1) {
      if ( gdd10 >= gddt ) { # maximum harvest date
        
        # print(paste('Harvest Maize ',ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai[i],cbiog[i],sep = " ; "    ))
        
        cat('\nDates: Germination:',date_germ,'| Emergence:',date_emerg)
        
        fileout <- paste("Maize_SEASON.csv")
        write(paste(ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai,cbiog[i],sep=";"),file =fileout,append=TRUE,sep = "\n")

        cat('\nMaize harvested! Harvest date:', paste(sprintf('%04d',year), sprintf('%02d', month), sprintf('%02d', day), sep = '-'),'\n')
        
        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        greenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        peaklai[i]    <- 0.0
        endCycle      <- T
        
      }
    } else {
      print('Maize has only one cycle - Stop')
      stop()
    }
    
    assign('gdd8'      , gdd8      , envir = env)
    assign('gdd10'     , gdd10     , envir = env)
    assign('cumPh'     , cumPh     , envir = env)
    assign('pla'       , pla       , envir = env)
    assign('leafwb'    , leafwb    , envir = env)
    assign('dum8'      , dum8      , envir = env)
    assign('xnti'      , xnti      , envir = env)
    assign('sumgdd8'   , sumgdd8   , envir = env)
    assign('xn'        , xn        , envir = env)
    assign('P3'        , P3        , envir = env)
    assign('tlno'      , tlno      , envir = env)
    assign('ndaysS3'   , ndaysS3   , envir = env)
    assign('fcbios'    , fcbios    , envir = env)
    assign('fcbiol'    , fcbiol    , envir = env)
    assign('sumP'      , sumP      , envir = env)
    assign('gdd10p'    , gdd10p    , envir = env)
    assign('gddemerg'  , gddemerg  , envir = env)
    assign('gddgerm'   , gddgerm   , envir = env)
    assign('date_emerg', date_emerg, envir = env)
    assign('date_germ' , date_germ , envir = env)
    assign('plaf'      , plaf      , envir = env)
    
  }
  
  #TO DO: Alexandre -
  ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i]

  assign("endCycle" , endCycle , envir = env)
  assign("ztopPft"  , ztopPft  , envir = env)
  assign("greenfrac", greenfrac, envir = env)
  assign("idpp"     , idpp     , envir = env)
  assign("idpe"     , idpe     , envir = env)
  assign("aroot"    , aroot    , envir = env)
  assign("aleaf"    , aleaf    , envir = env)
  assign("astem"    , astem    , envir = env)
  assign("arepr"    , arepr    , envir = env)
  assign("cbiol"    , cbiol    , envir = env)
  assign("cbiog"    , cbiog    , envir = env)
  assign('cbioc'    , cbioc    , envir = env)
  assign("cbiop"    , cbiop    , envir = env)
  assign("cbios"    , cbios    , envir = env)
  assign("cbior"    , cbior    , envir = env)
  assign("plai"     , plai     , envir = env)
  assign("peaklai"  , peaklai  , envir = env)
  assign("aerial"   , aerial   , envir = env)
  assign("aybprod"  , aybprod  , envir = env)
  assign("ayabprod" , ayabprod , envir = env)
  assign("ayrprod"  , ayrprod  , envir = env)
  assign("aylprod"  , aylprod  , envir = env)
  assign("biomass"  , biomass  , envir = env)
  assign("ayanpp"   , ayanpp   , envir = env)
  assign("croplive" , croplive , envir = env)
  assign("harvdate" , harvdate , envir = env)
  assign("cropy"    , cropy    , envir = env)
  assign("gddemerg" , gddemerg , envir = env)
  
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

