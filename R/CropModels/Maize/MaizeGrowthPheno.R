



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
  
  greenfrac[i] < -1.0
  
  if (croplive[i] == 1) {
    
    idpp[i] <- idpp[i] + 1
    
    if(idpp[i] == 1) {
      
      cbiow[i]  <- 0.00
      cbiob[i]  <- 0.00
      cbior[i]  <- 0.00
      cbiol[i]  <- 0.20
      cbiocr[i] <- 0.01
      
      dum8 <- 0.0
      cumPh <- 1.0
      xn <- 3.0
      
      sumdtt8 <- 0.0
      sumdtt10 <- 0.0
      pla <- 0.0
      leafwb <- 0.0
      cumdtt8 <- 0.0
      
    }
    
    # PARAMETERS #####
    dtts <- 400
    dttf <- 170
    dttt <- 1200
    specla[i] <- 400 # cm2 g-1
    laic <- 10
    pden <- 7 # plants per m-2
    phyl <- 38.9
    ##################
    
    gdd8     <- calc_dtt(idpp[i], jday,  8, 34)
    gdd10    <- calc_dtt(idpp[i], jday, 10, 34)
    sumdtt8  <- sumdtt8  + gdd8
    sumdtt10 <- sumdtt10 + gdd10
    
    tmean  <- (tmax + tmin) / 2.0
    
    tlno <- (sumdtt8 / 21.0) + 6
    P3 <- (tlno - 2.0) * phyl + 96 - sumdtt8
    
    if (cumPh < 5.0) {
      pc <- 0.66 + 0.068 * cumPh
    } else {
      pc <- 1
    }
    
    ti <- gdd8 / (phyl * pc)
    cumPh <- cumPh + ti
    xn <- cumPh + 1
    
    slfc <- ifelse(test = plai[i] < laic, yes = 1.0, no = min(1.0, max(0.0, 1.0 - 0.008 * (plai[i] - laic))))
    slft <- ifelse(test = tmean > 279.15, yes = 1.0, no = min(1.0, max(0.0, 1.0 - (279.15 - tmean) / 279.15)))
    
    lsr <- ifelse(test = slfc <= slft, yes = 1.0 - slfc, no = 1.0 - slft)
    
    if (sumdtt10 < dtts - P3) {
      
      # STAGE 1: from emergence to tassel initiation
      
      if (xn < 4) {
        plag <- 3 * xn * ti
      } else {
        plag <- 3.5 * xn^2 * ti
      }
      
      pla <- pla + plag
      leafwt <- pla / specla[i]
      leafwg <- leafwt - leafwb
      leafwb <- leafwt
      
      xnti <- xn
      
      slan <- sumdtt8 * pla / 1e4
      plas <- plag * lsr
      
      plas <- max(plas, slan)
      cbiol[i] <- cbiol[i] + (leafwg - plas / specla[i]) * pden
      
    } else if (sumdtt10 < dtts) {
      
      # STAGE 2: from tassel initiation to silking
      
      if (xn <= 12) {
        plag <- 3.5 * xn^2 * ti
        leafwg <- 0.00116 * plag * pla^0.25
        stemg <- leafwg * 0.0182 * (xn - xnti)^2
      } else if (xn > 12 & xn <= tlno - 3) {
        plag <- 595 * ti
        leafwg <- 0.00116 * plag * pla^0.25
        stemg <- leafwg * 0.0182 * (xn - xnti)^2
      } else if(xn > tlno - 3) {
        plag <- 595 * ti / sqrt(xn + 5 - tlno)
        leafwg <- 0.00116 * plag * pla^0.25
        stemg <- 10.85 * ti
      }
      
      slan <- pla / 1e3
      plas <- plag * lsr
      
      dum8 <- sumdtt8
      mplai <- plai[i]
      
      plas <- max(plas, slan)
      cbiol[i] <- cbiol[i] + (leafwg - plas / specla[i]) * pden 
      
    } else if (sumdtt8 < dum8 + dttf) {
      
      # STAGE 3: from silking to effective grain filling
      
      cumdtt8 <- cumdtt8 + (gdd8 / (1 - lsr))
      slan <- (mplai / peaklai[i]) * (cumdtt8 / dttf) ^ sg
      
      cbiol[i] <- cbiol[i] - (slan / specla[i]) * pden 
      
    } else if (sumdtt10 < dttt) {
      
      # STAGE 4: from effective grain filling to physiological maturity
      
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
    plai[i] <- cbiol[i] * specla[i] / 1e4
    # print(plai[i])
    
    peaklai[i]  <- max(peaklai[i]  ,plai[i])
    
    greenfrac[i] <- 1.0
    
    
    biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbiop[i]
    
    # keep track of aboveground annual npp
    ayanpp[i] <- ayanpp[i] + adnpp[i]
    
    
    #END TEST Maize MODEL FROM ORYZA
    #____________________________________
    
    #_____________________________________________
    
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
    
    assign('sumdtt8' , sumdtt8 , envir = env)
    assign('sumdtt10', sumdtt10, envir = env)
    assign('cumPh'   , cumPh   , envir = env)
    assign('pla'     , pla     , envir = env)
    assign('leafwb'  , leafwb  , envir = env)
    assign('dum8'    , dum8    , envir = env)
    assign('xnti'    , xnti    , envir = env)
    assign('cumdtt8' , cumdtt8 , envir = env)
    try(expr = assign('mplai'   , mplai   , envir = env), silent = T)
    
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

