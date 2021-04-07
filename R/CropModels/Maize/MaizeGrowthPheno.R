# building simdatavars

simDataVars$TMAXC      <- 0
simDataVars$TMINC      <- 0
simDataVars$TTSUM      <- 0
simDataVars$DRLVTa     <- 0
simDataVars$ID     <- 0
simDataVars$SRCT     <- 0

simDataVars$cumPh  <-  0
simDataVars$xn  <-  0
simDataVars$tiphyl  <-  0 # was 'ti' before
simDataVars$gdd8  <-  0
simDataVars$gdd10  <-  0
simDataVars$sumgdd8  <-  0
simDataVars$pla  <-  0
simDataVars$leafwb  <-  0
simDataVars$dtt8  <-  0
simDataVars$dtt10  <-  0
simDataVars$tlno  <-  30
simDataVars$P3  <-  0
simDataVars$slfc  <-  0
simDataVars$slft  <-  0
simDataVars$lsr  <-  0
simDataVars$plag  <-  0
simDataVars$plaf  <-  0
simDataVars$laistg3  <-  0
simDataVars$leafwt  <-  0
simDataVars$leafwb  <-  0
simDataVars$xnti  <-  0
simDataVars$ds  <-  0
simDataVars$gdd8stg1 <-  0

simDataVars$dm <- list('leaf' = 0.0, 'stem' = 0.0, 'root' = 0.0, 'cob' = 0.0, 'grain' = 0.0, 'sleaf' = 0)
simDataVars$date_germ <- 0
simDataVars$date_emerg <- 0
simDataVars$coleog  <- 0
simDataVars$stage   <- 0
simDataVars$vstage  <- 0
simDataVars$rstage  <- 0
simDataVars$sen     <- 0
simDataVars$senstg4     <- 0
simDataVars$senstg4l     <- 0
simDataVars$fsen    <- 0
simDataVars$sumP    <- 0
simDataVars$psker   <- 0
simDataVars$gpp     <- 0 
simDataVars$RGfill  <- 0
simDataVars$transs  <- 0
simDataVars$transl  <- 0
simDataVars$cbiosr  <- 0
simDataVars$cbiolr  <- 0
simDataVars$ndaysS3 <- 0

simDataVars$cbiols <- 0 #TODO check with the new version in 'refactoring_v0'
simDataVars$acob <- 0
simDataVars$lais <- 0

MaizeGrowthPheno <- function(iyear, iyear0, imonth, iday, jday, index) {
  
  environment(calc_dtt) <- env
  
  i <- index
  
  # Management Parameters
  pden <- config$plant1$plantPop # plant population (plants per m⁻²)
  pdpt <- config$plant1$plantingDepht # sowing depth (cm)
  
  # Crop Parameters
  params <- plantList$maize$params
  gddgerm        <- params$gddgerm      # 15      # GDD (base 10ºC) required for germination (default = 15)
  gddemerg       <- params$gddemerg     # 6.0     # GDD (base 10ºC) required for emergence per cm depth (default = 6) [we believe this is in cm/day]
  emerg_limit    <- params$emerg_limit  # 25      # maximum days allowed from sowing to emergence (default = 25)
  gddf           <- params$gddf         # 170     # P5 in CERES-MAIZE and in the manual??? GDD (base 8ºC) from silking to effective grain filling
  gddt           <- params$gddt         # 1600    # GDD (base 10ºC) required from germination to maturity (default = 1500?)
  gddsfrac       <- params$gddsfrac         # 1600    # GDD (base 10ºC) required from germination to maturity (default = 1500?)
  laic           <- params$laic         # 4       # critical LAI for light competition (condition for SLFC have a chance to affect leaf expansion)
  laim           <- params$laim         # 0.7     # LAI at maturity 
  sg             <- params$sg           # 4.0     # ‘stay-green’ factor, which controls how fast leaf senesces proceeds after silking
  phyl           <- params$phyl         # 38.9    # Phylochron interval; the interval in thermal time (degree days)
  dsstop         <- params$dsstop       # 1.15    # development stage when root growth stops
  ph1            <- params$ph1          # 47
  ph2            <- params$ph2          # 28
  G2             <- params$G2           # 676 # the potential number of grains per plantkernels ear⁻¹ (default = 676)
  G5             <- params$G5           # 8.7 # the potential grain filling rate (mg d-1 kernel-1) (default = 8.7)
  efftrans       <- params$efftrans     # 0.26 # Efficiency of carbon translocation from leaves/stem to grain filling
  P5 <- 800 # Calculando abaixo SVC (03/04/2021) 
  
    gdds <- gddt*gddsfrac
  # gdds <- (0.41 * gddt) + 145.4         # GDD (base 10ºC) required from silking to physiological maturity
  # gdds <- 100+0.4451*gddt -50           # last page in the manual
  
   # gdds = 0.514*gddt - 30.4  #Pioneer      
   # gdds = 0.520*gddt - 2.3   #DEKALB     
   # gdds = 0.453*gddt + 97.1  #Asgrow   
   # gdds = 0.308*gddt + 273.3 #NC+     
  
  

  gddv <- c(ph1, ph2)
  
  # Fatores de conversao 
  m2.kgC_cm2.gDM   <- (100*100)*(1/1000)*(1/(1/0.45))
  gDMPlant_kgCm2 <- (12/30) * pden / 1e3 # g-DM(CH2O)/plant to kg-C/m^2
  
  
  # adgpp <- adgpp[i]* (30/12) * 1000  # converter kg C / m2.d para g CH2O / m2.d
  
  rgrowth <- 0.30 # default ECOSMOS growth respiration
  irgrowth <- 1.0 / (1.0 - rgrowth) # 1/ rgrowth  - (crop growth respiration)
  
  # daily maintenance respiration parameters maize basic morphological components
  #TODO Pensar levamos para plantparams (não vejo necessidade, por hora) [2021-01-20]
  fresp <- list('leaf'  = 0.47, # g-CH2O g-DM⁻¹
                'root'  = 0.45, # g-CH2O g-DM⁻¹
                'stem'  = 0.52, # g-CH2O g-DM⁻¹
                'cob'   = 0.52, # g-CH2O g-DM⁻¹ (same as stem at the moment)
                'grain' = 0.49) # g-CH2O g-DM⁻¹
  
  # R stages adjusted proportionally based on the total GDD10 from R1 to R6 (which is basically the gdds parameter).
  # For R stages, the following scheme was derived from a hybrid maturity with a total GDD10 
  # from silking to maturity of 660: the interval from R1 (silking) to R2 (blister) and R2 to R3 (milk) is 89 GDD10, 
  # 100 GDD10 from R3 to R4 (dough), and 167 GDD10 from R4 to R5 (dent), and 217 GDD10 from R5 to R6 
  # (blacklayer, or physiological maturity). For hybrids of other maturities, the intervals for R stages are adjusted 
  # proportionally based on the total GDD10 from R1 to R6.
  p_rstage <- list('1to2' = (gddt-gdds)+gdds*0.13, #R1 - Silk  
                   '2to3' = (gddt-gdds)+gdds*0.27, #R2 - Blister 
                   '3to4' = (gddt-gdds)+gdds*0.42, #R3 - Milk 
                   '4to5' = (gddt-gdds)+gdds*0.67, #R4 - Dough 
                   '5to6' = (gddt-gdds)+gdds*1.00) #R5 - Dent
  
  # Crop initiation
  pgreenfrac[i] <- 0.0 
  
  if (croplive[i] == 1) {
    
    idpp[i] <- idpp[i] + 1
    
    if(idpp[i] == 1) {  # Zerando variáveis no plantio
      
      # cumPh <- 1.0
      # xn <- 3.0
      
      dum8 <- 0.0
      
      gdd8 <- 0.0
      gdd10 <- 0.0
      gdd10p <- 0.0
      pla <- 0.0
      leafwb <- 0.0
      sumgdd8 <- 0.0
      gdd8stg1 <- 0.0
      ndaysS3 <- 0.0
      sumP <- 0.0
      cbiosr <- 0.0
      cbiolr <- 0.0
      rest   <- 0.0
      vstage <- 0.0
      rstage <- 0
      plas <- 0
      sen    <- 0
      senstg4 <- 0
      senstg4l <- 0
      fsen   <- 0
      senf <-array(0,4)
      
      gddvcum<-gddv[1]
      
      dm <- list('leaf' = 0.0, 'stem' = 0.0, 'root' = 0.0, 'cob' = 0.0, 'grain' = 0.0, 'sleaf' = 0)
      
      #gddemerg <- gddemerg * pdpt # gddgerm taking into account the sowing depth already here
      
      plaf <- 0.0
      laistg3 <- 0.0
      
      tlno <- 0
      P3 <- 0
      
      
    }
    
    ##############################
    # From sowing to germination #
    ##############################
    
    # TODO: emergência está levando em torno de 8 dias apenas... pensar num fator f(água no solo) [2021-02-26]
    # Sugestão abaixo implementada! comparar com dados de Nebraska e talvez calibrar [Henrique; 2021-02-10]
    # Victor, acho que vamos precisar de um contador aqui! [2021-01-20]
    # Ciclo tem terminado bem antes, mesmo eu subindo o ggdt de 1500 pra 1600
    # Imagino que o gdd10 da germ-emerg é em ºCd por cada cm naquele dia.
    # Ou seja, meu entendimento é que a plântula (coleóptilo e primórdios/1ª folha) pode crescer 1 cm no máximo por dia.
    # Se atingiu, beleza, vai pro proximo dia e veja se atingiu novamente
    # O modo que entendemos do manual (confuso) e estamos fazendo é o ggd10 total na profundidade total e não cm/d.
    # É como se nesse caso a 'plântula' sob o solo crescesse sei la, 5 cm no dia e isso acho que é irrealistico.
    # Se tiver mto complicado ou os valores não fazem sentido, podemos ver se:
    #  - O CERES-MAIZE tem essa opção e trazer aqui.
    #  - fixar minimo de dias requirido e aplicar a 'tempvmax' pra regular a taxa
    #  - simplesmente usar dias fixos 
    #  - ou ainda usar a data de emerg e não semeadura (mundo ideal é ambos funfando)
    
    if (gdd10p <= gddgerm) {
      
      dtt10p <- calc_dtt(idpp[i], jday, 10, 34)
      gdd10p <- gdd10p + dtt10p
      
      if (gdd10p > gddgerm){
        date_germ <- idpp[i]
        print(paste0('Seeds germinated'))
        # initialize variables for emergence phase
        coleog <- 0
      }
      
      
    } else if (idpp[i] > date_germ & coleog <= pdpt) {
      
      #################################
      #    Germination to emergence   #
      
      
      dtt10p <- calc_dtt(idpp[i], jday, 10, 34)
      # coleodg is the maximum daily growth em cm/day
      coleodg <- min(dtt10p/gddemerg,1) # limiting to 1 cm per day of coleoptile growth towards the soil surface
      # coleodg is the cumulated growth in cm
      coleog <- coleog + coleodg
      
      # crop failure due to seed reserves exhaustion regardless of breeding brand quality and environmental conditions
      if (idpp[i] > emerg_limit) {
        print(paste0('Crop failure: plants did not emerge until 25 days after sowing.'))
        stop()
      }
      
      if (coleog > pdpt) {
        
        date_emerg <- idpp[i]
        print(paste0('Crop emerged at ',date_emerg,' days after sowing'))
        
        # Initialize variables at emergence
        cumPh <- 1.0
        xn <- 3.0
        vstage <- 1
        cbiol[i]  <- 0.06 * gDMPlant_kgCm2 # primeiro termo seria a massa inicial
        plai[i]   <- cbiol[i] * specla[i]
        cbiols[i] <- 0
        lais[i] <- 0
      }
      
      
      # End Germination to emergence  #
      #################################
      
    } else {
      
      
      
      
      ph <- ifelse(test = vstage < 10, yes = 1, no = 2)
      
      if( gdd10 >= gddvcum & gdd10 < gdds) {
        vstage <- vstage + 1
        gddvcum<-gddvcum+gddv[ph]
      }
      
      
      # TODO CO2 assimilation in HM does not seem to be penalised by water stress [Henrique; 2021-02-26]
      #     read sections 4.1.2, 4.1.3 & 4.2.4 in the manual
      #     should we include the 'stress' variable from ECOSMOS here, in addition to that applied to leaf expansion?
      adnppl <- adnpp[i] * irgrowth / (1 + fresp$leaf)
      adnpps <- adnpp[i] * irgrowth / (1 + fresp$stem)
      adnppr <- adnpp[i] * irgrowth / (1 + fresp$root)
      adnppg <- adnpp[i] * irgrowth / (1 + fresp$grain)
      
      dtt8  <- calc_dtt(dpp = idpp[i], jday = jday, ldtt =  8, udtt = 34)
      dtt10 <- calc_dtt(dpp = idpp[i], jday = jday, ldtt = 10, udtt = 34)
      dtt8   <- max(dtt8,0.0)
      dtt10  <- max(dtt10,0.0)
      
      gdd8  <- gdd8  + dtt8
      gdd10 <- gdd10 + dtt10
      
      tmean  <- (tmax + tmin) / 2.0 #TODO talvez usamos do ECOSMOS direto e não precisemos esse passo toda vez [2021-01-20]
      
      # Leaf Growth and Senescence #
      if (cumPh < 5.0) {
        pc <- 0.66 + 0.068 * cumPh
      } else {
        pc <- 1
      }
      
      tiphyl <- dtt8 / (phyl * pc)   # DTT8 is daily accumulation of GDD8, obs. manual is GDD8 (DTT8 is the correct)
      cumPh <- cumPh + tiphyl
      xn <- max(xn, cumPh + 1)
      
      # TI is the fraction of daily increase in leaf number
      #     'ti' here had to be changed to 'tiphyl' because the same term is used in 'turvap' subroutine [Henrique; 2021-03-04]
      # cumPh is the number of fully expanded leaves
      # XN is the leaf number of the oldest expanding leaf
      # PC is an intermediate variable
      # Note: at emergence cumPh = 1 and XN = 3.
      
      
      # Daily leaf senescence due to competition for light and temperature stress (PLAS, cm2 plant-1 d-1)
      # is computed from a stress rate factor (LSR, 0~1):
      slfc <- ifelse(test = plai[i] < laic,
                     yes  = 1.0,
                     no   = min(1.0, max(0.0, 1.0 - 0.008 * (plai[i] - laic))))
      
      slft <- ifelse(test = tmean > 279.15, #TODO pensar se o limiar de 6º C possa ser um parametro, futuramente. [2021-01-20]
                     yes  = 1.0,
                     no   = min(1.0, max(0.0, 1.0 - (279.15 - tmean) / 6)))
      
      # LSR is the stress rate factor (0-1) | 0 means no stress and 1 full stress
      lsr <- ifelse(test = slfc <= slft,
                    yes  = 1.0 - slfc,
                    no   = 1.0 - slft)
      
      
      leafwg  <-  0
      stemwg  <-  0
      cobwg   <-  0
      grainwg <-  0
      
      
   
      
      #
      #     Phenological stages used in the Hybrid-Maize after emergence:
      #     Stage 1: from emergence to tassel initiation                   *** Stages 1 & 2 in CERES ***
      #     Stage 2: from tassel initiation to silking
      #     Stage 3: from silking to effective grain filling
      #     Stage 4: from effective grain filling to physiological maturity.
      
      
      if ((gdd10+P3 <= gdds) & (gdd10 < gdds)) { 
        
        stage <- 1
        ################################################
        # STAGE 1: from emergence to tassel initiation #  *** Stages 1 & 2 in CERES ***
        ################################################
        
        # Leaf Growth and Senescence #
        # The daily expansion of leaf area (PLAG) and growth of leaf biomass is driven by temperature, and the choice of functions depends on growth stage.
        
        # Testings after CERES-MAIZE review [Henrique; 2021-03-04]
        #VEGPHASE <- gddt-gdds
        #tlno <- (VEGPHASE / (phyl * 0.5)) + 6
        # tlno <- (gdds / (phyl * 0.5)) + 6
        # P3 <- (tlno - 2.0) * phyl + 96 - gdds
        
        #The duration of Stage 2 in terms of GDD equals P3 whose value is determined at the end of Stage 1:
        # this parameter is fixed as 30 in CERES in stages 1 & 2
        #SVC tlno <- (gdd8 / (phyl * 0.5)) + 6      
        tlno <- (gdd8 / 21) + 6    
        # this parameter remains as 0 in CERES in stages 1 & 2
        P3 <- (tlno - 2.0) * phyl + 96 - gdd8   #GDD8 is for the duration of Stage 2, and
        
        # Daily expansion of leaf area (PLAG), PLA (cm2 plant-1)
        plag <- ifelse(test = xn < 4,
                       yes  = 3.0 * xn * tiphyl,
                       no   = 3.5 * (xn^2) * tiphyl)
        
        #TODO includes water and others 'stress' in plag following... [Henrique; 2021-02-26]
        #    Keating et al. (2003) in pages 69-70 and/or CERES-MAIZE
        #    CEREs also includes waterlogging, and seems to be accomodated for K and P [*AMIN1(TURFAC,(1.0-SATFAC),PStres2, KSTRES)]
        
        pla <- pla + plag
        
        #TODO CERES-MAIZE includes **1.25 in leafwt (XLFWT)
        # SLA (cm2 g-1, ≤ 400 cm2 g-1) -> g of DM
        leafwt <- pla / (specla[i] * m2.kgC_cm2.gDM) # 10 (convertion SPECLA from m²/kg DM to cm²/g DM)
        #TODO CERES-MAIZE has an intermediate variable called LFWT which is used XLFWT  = AMAX1 (XLFWT,LFWT)
        leafwg <- leafwt - leafwb
        leafwb <- leafwt
        
        # PLA (cm2 plant-1) is the total leaf area per plant
        # SLA (cm2 g-1, ≤ 400 cm2 g-1) is the specific leaf area
        # leafWtToday (g plant-1) is the leaf biomass after update
        # leafWt is the leaf biomass of the previous day
        # leafWtGrow (g plant-1) is the daily growth in leaf biomass
        
        slan <- gdd8 * pla / 1e4
        plas <- plas + plag * lsr
        
        fsen <- (max(plas,slan)- sen) / pla
        
        
        sen <- max(plas,slan)
        
        
        leafwg <- leafwg / (1.0 + fresp$leaf)
        #TODO CERES uses a different approach here
        #TODO includes 'CumLeafSenes' now
        
        # SLAN (cm2 plant-1) is the accumulated leaf senescence caused by natural development
        # PLAS (cm2 plant-1 d-1) is the daily leaf senescence due to competition for light and temperature stress (affected by )
        # PLAG (cm2 plant-1 d-1) is the daily expansion of leaf area
        
        # Root Allocation #
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        aroot[i] <- max(0.0, min(0.5, arooti - arooti*(ds/dsstop) ))
        
        # aroot equals to ACroot at emergence (=biomass allocation coefficient for root at emergence).
        # DSstop is the development stage when root growth stops
        # The default values for ACEroot and DSstop are 0.35 and 1.15, respectively.
        
        aleaf[i] <- max(0.0, 1.0 - aroot[i])
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot[i] * adnppr)
        cbiolg <- max(0.0, aleaf[i] * adnppl)
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen
        
        senf[1] <- sen
        xnti <- xn
        gdd8stg1 <- gdd8
        
      } else if ( gdd10 < gdds ) {
        
        ##############################################
        # STAGE 2: from tassel initiation to silking #
        ##############################################
        stage <- 2
        # Leaf Growth and Senescence #
        
        # Stem and Root Growth #
        if (xn <= 12) {
          plag <- 3.5 * xn^2 * tiphyl
          pla <- pla + plag
          leafwg <- 0.00116 * plag * pla^0.25
          stemwg <- leafwg * 0.0182 * (xn - xnti)^2
        } else if (xn > 12 & xn <= tlno - 3) {
          plag <- 595 * tiphyl
          pla <- pla + plag
          leafwg <- 0.00116 * plag * pla^0.25
          stemwg <- leafwg * 0.0182 * (xn - xnti)^2
        } else if(xn > tlno - 3) {
          plag <- 595 * tiphyl / sqrt(xn + 5 - tlno)
          pla <- pla + plag
          leafwg <- 0.00116 * plag * pla^0.25
          stemwg <- 10.85 * tiphyl
        }
        
        #TODO includes water and others 'stress' in plag following... [Henrique; 2021-02-26]
        #    Keating et al. (2003) in pages 69-70 and/or CERES-MAIZE
        #    CEREs also includes waterlogging, and seems to be accomodated for K and P [*AMIN1(TURFAC,(1.0-SATFAC),PStres2, KSTRES)]
        
        # XNTI is the value of XN at the end of Stage 1
        # Note that both PLAG and leafWtGrow are the values projected based on temperature, but whether they can be realized depends on (a) availability of net carbohydrate from photosynthesis and (b) crop water stress (Section 4.2.4).
        # Leaf growth stops at the end of Stage 2 when silking occurs.
        
        
        slan <- gdd8 * pla / 1e4
        plas <- plas + plag * lsr #TODO includes water 'stress' here following Keating et al. (2003) in pages 69-70 [Henrique; 2021-02-26]
        
        fsen <- (max(plas,slan)- sen) / pla
        
        sen <- max(slan,plas)
        
        
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        # aroot[i] equals to ACroot at emergence (=biomass allocation coefficient for root at emergence).
        # DSstop is the development stage when root growth stops
        # The default values for ACEroot and DSstop are 0.35 and 1.15, respectively.
        
        aroot[i] <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        
        aleaf[i] <- max(0.0, (1.0 - aroot[i]) * (leafwg / (leafwg + stemwg)))
        astem[i] <- max(0.0, 1.0 - aroot[i] - aleaf[i])
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot[i] * adnppr)
        cbiolg <- max(0.0, aleaf[i] * adnppl)
        cbiosg <- max(0.0, astem[i] * adnpps)
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot[i])
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen
        cbios[i] <- cbios[i] + cbiosg
  
        # DM mass-based for model evaluation
        dm$stem <- dm$stem +  pden *stemwg/1000  # convert stemwg (g DM plant-1) to kg DM/m^2
  
        dum8 <- gdd8 # GDD8 to reach Stage 3
        
        plaf <- max(pla,plaf)

        senf[2] <- sen
        
        #average plant growth rate (PSKER) during a critical kernel set window of 340 sumDTT8 centered on silking date
        if(gdd8 >= (gdd8stg1+P3-(340/2))){ 
        sumP = sumP + adnpp[i] /gDMPlant_kgCm2 # g C to g CH2O per plant
        ndaysS3 <- ndaysS3 + 1
        }
        
        
      } else if (gdd8 > dum8 & gdd8 <= dum8 + gddf) { 
        
        ####################################################
        # STAGE 3: from silking to effective grain filling #
        ####################################################
        stage <- 3
        # Leaf Growth and Senescence #
        
        # SVC e  VHB               
        slan <- pla / 1e3 #  Warning: divergence between HM Manual and Yang et al. in FCR 87 (2004) [Henrique/Victor; 2021-03-03]
        #        plas <- plas + plag * lsr #TODO includes water 'stress' here following Keating et al. (2003) in pages 69-70 [Henrique; 2021-02-26]
        sen <- senf[2] + slan
        
        fsen <- slan / pla
        
        # SF (0 to 1) is the fraction of senesced leaf of the maximum green leaf area, which is achieved at silking
        # LAImature is the fraction of LAI at maturity of the maximum LAI
        # SG is a ‘stay-green’ factor, which controls how fast leaf senesces proceeds after silking
        
        # Stem, Cob and Root Growth #
        stemwg <- 0.220 * dtt8 
        cobwg  <- 0.088 * dtt8 
        
        # The course of crop development is measured by development stage (DS)
        #   on a scale ranging from 0 to 2, with 1 at silking.
        ds <- max(1.0, min(2.0, 1.0 + (gdd10 - gdds)/(gddt - gdds)))
        
        aroot[i] <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        acob[i]  <- max(0.0, (1.0 - aroot[i]) * (cobwg/ (cobwg + stemwg)))
        astem[i] <- max(0.0, 1.0 - aroot[i] - acob[i])
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot[i] * adnppr)
        cbiocg <- max(0.0, acob[i]  * adnpps)
        cbiosg <- max(0.0, astem[i] * adnpps)
        
        if(cbioc[i] == 0.0) {
          cbioc[i] <- cbios[i] * 0.167
          cbios[i] <- cbios[i] - cbios[i]* 0.167
        }
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbioc[i] <- cbioc[i] + cbiocg
        cbios[i] <- cbios[i] + cbiosg
        cbiol[i] <- cbiol[i] - cbiol[i] * fsen
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen
        
        cbiosr <- cbiosr + cbiosg * 0.60
        cbiolr <- cbiol[i] * 0.15
        
        # DM mass-based for model evaluation per plant here
        dm$stem <- dm$stem +  pden *stemwg/1000  # convert stemwg (g DM plant-1) to kg DM/m^2
        dm$cob  <- dm$cob  + pden *cobwg/1000   # convert cobwg (g DM plant-1) to kg DM/m^2
        
        senf[3] <- sen
        
        #average plant growth rate (PSKER) during a critical kernel set window of 340 sumDTT8 centered on silking date
        if(gdd8 >= (gdd8stg1+P3-(340/2))){ 
          sumP = sumP + adnpp[i] /gDMPlant_kgCm2 # g C to g CH2O per plant
          ndaysS3 <- ndaysS3 + 1
        }
        
        #SVC (03/04/2021) -> 0.15 is allocated to be translocated 
        laistg3 <- max((1.-0.15)*cbiol[i] * specla[i],laistg3)
        
      } else if ( (gdd8 > dum8 + gddf) & (gdd10 < gddt) ) {
        
        ###################################################################
        # STAGE 4: from effective grain filling to physiological maturity #
        ###################################################################
        stage <- 4

        
        
        # grainGrow is the actual grain filling rate(g plant-1 day-1)
        # GPP is the number of viable grain per plant (assuming one ear per plant) -> most likely from Fig. 3. in Andrade et al. Crop Sci. 39:453-459 (1999)
        # FillEffi is the filling efficiency related to plant density
        # RGfill (0 to 1) is the temperature driven filling scale
        # sumP (g CH2O plant-1) is the cumulative net assimilation adjusted for maintenance respiration of grain (GRRG; 0.49 g CH2O g-1 DM)
        # IDURP is the duration in days of the 340 sumDTT8 period (ndaysS3)
        # PSKER is the average daily biomass accumulation per plant (mg d-1) during this period
        # GRRG is maintenance respiration of grain (0.49 g CH2O g-1 DM)
        # The threshold value of PSKER for grain setting is 1000 mg d-1 plant-1,
        #     as found by Tollenaar et al. (1992) and Andrade et al. (1999, 2002),
        #     which is higher than the threshold for grain setting used in the original version of CERES-Maize (Jones and Kiniry, 1986)
        #     and subsequent versions (Lopez Cedron et al., 2003).
        
       
        
        # CERES-MAIZE
        # PSKER = SUMP*1000.0/IDURP*3.4/5.0
        # GPP   = G2*PSKER/7200.0 + 50.0
        # GPP   = AMIN1 (GPP, G2)
        # GPP   = AMAX1 (GPP,0.0)
        
        psker <- sumP *(1 / (1 + fresp$grain)) * (1e3 / ndaysS3) * (3.4 / 5.0) # we don't know the mathematical manipulation here
        psker <- max(psker,1000) # The threshold value of PSKER for grain setting is 1000 mg d-1 plant-1
        gpp <- G2 - 676 / (psker/1e3) # mg as in the manual, but it may be wrong! 

        
        # FillEffi is for adjusting the potential grain filling rate based on plant population, and its function was derived from a high-yield maize experiment at Lincoln, Nebraska (Yang et al., 2004)
        filleffi <- 1.47 - (0.09 * pden) + (0.0036 * pden^2)
        
        # RGfill is computed as a sum of eight consecutive 3-hour interval filling scales (RGfilli):
        RGfill <- sum(sapply(X = 1:8, FUN = function(x) {
          tmfac <- 0.931 + 0.114 * x - 0.0703 * x^2 + 0.0053 * x^3
          ttmp <- (tmin - 273.15) + tmfac * (tmax - tmin)
          RGfill <- ifelse(test = ttmp > 6,
                           yes  = (1.0 - 0.0025 * (ttmp - 26.0)^2) / 8.0,
                           no   = 0.0)
          # return(RGfill)
        }))
        
        grainwg <- (RGfill * gpp * G5 * filleffi * 0.001) #TODO CERES-MAIZE applies *(0.45+0.55*SWFAC) here too
        

        grainwg <-  grainwg * gDMPlant_kgCm2  # convert from g-DM plant⁻¹ day⁻¹ to kg-C m⁻² day⁻¹

        transs <- 0.0
        transl <- 0.0
        
        #Potential DM at grain 
        dm$grain <- dm$grain + (grainwg /cfrac[i]) # kg DM/m^2
        
        # C mass-based for ECOSMOS 
        #if(idpp==79) browser()
        if (grainwg > adnppg & cbiosr > 0 ) {
          transs <- min(cbiosr,abs(grainwg - adnpp[i]))
          grainwg <- adnppg + efftrans * transs
          cbiosr <- cbiosr - transs
          cbios[i] <- cbios[i] - transs
        } else if (grainwg > adnppg & cbiolr > 0 ) {
          transl <- min(0.005 * cbiolr, abs(grainwg - adnpp[i]))
          grainwg <- adnppg + efftrans * transl
          cbiolr <- cbiolr - transl
          cbiol[i] <- cbiol[i] - transl
        }
        
        if (grainwg < adnppg) {
          cbiosr   <- cbiosr   + (abs(grainwg - adnppg)*((1 + fresp$grain)/irgrowth))* irgrowth / (1 + fresp$root)
          cbios[i] <- cbios[i] + (abs(grainwg - adnppg)*((1 + fresp$grain)/irgrowth))* irgrowth / (1 + fresp$stem)
          dm$stem  <- dm$stem  + abs(grainwg - adnpp[i])/cfrac[i]  # convert kg-C/m^2 to kg DM/m^2
        }
        
        cbiog[i] <- cbiog[i] + grainwg 
        
        
#______________________________________________        
# Leaf senescence # (leaf growth stops from here onwards)
        sumgdd8 <- sumgdd8 + (dtt8 / max(0.05,(1 - lsr)))
        
        sf <- (laistg3) * min(1, ((sumgdd8 / (P5)) ^ sg)) #
        if(plai[i]<=0.01) sf = 0
         dl <- max(0,sf - senstg4l)
        senstg4l <- sf
      
        cbiols[i] <- cbiols[i] + min(dl*(1 / specla[i] ),cbiol[i])    
        cbiol[i]  <- cbiol[i]  - min(dl*(1 / specla[i] ),cbiol[i])

        sf <- (laim * plaf) * min(1, ((sumgdd8 / P5) ^ sg)) #TODO gdds = P5 in model's manual (?) is base 10! e AGORA?
        dl <- sf - senstg4
        senstg4 <- sf 
        sen <- senf[3]+sf
        #convert sf (cm2 plant-1) to kg-C/m2 
        #cbiol[i]  <- cbiol[i]  - dl * pden * (1/(100*100))*(1 / specla[i] )
        #cbiols[i] <- cbiols[i] + dl * pden * (1/(100*100))*(1 / specla[i] )
        
              senf[4] <- sen
        
      } # END OF PHENOLOGICAL GROWTH STAGES PHASES BLOCKS
      

      #Maintenance  respiration     
      cbiol[i] <- cbiol[i] - cbiol[i]*0.0 # leaf respiration cust is in rdarkc ('gamma' parameter)
      cbios[i] <- cbios[i] - cbios[i]*0.0 # stem respiration is applied at sumnow
      cbioc[i] <- cbioc[i] - cbioc[i]*0.006 
      cbiog[i] <- cbiog[i] - cbiog[i]*0.005
      
      plai[i] <- max(0.005, cbiol[i] * specla[i])
      lais[i] <- cbiols[i] * specla[i] #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
      pgreenfrac[i] <- max(0.01,plai[i]/(plai[i]+0.3*lais[i]))
      
      biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbioc[i] + cbiog[i]

      # keep track annual npp
      ayanpp[i] <- ayanpp[i] + adnpp[i]
      
      # aboveground value to calculate harvest index
      ayabprod[i] <- ayabprod[i] + max(0.0, adnpp[i] * (1 - aroot[i]))  *irgrowth/(1+fresp$grain) # considering fresp$grain as average
      
      # keep track of annual total root production carbon
      ayrprod[i] <- ayrprod[i] +  max(0.0, adnpp[i]*irgrowth/(1+fresp$root))
      
      # keep track of total biomass production for the entire year, and the
      aybprod[i] <- ayabprod[i] + ayrprod[i]
      
      # keep track of total carbon allocated to
      # leaves for litterfall calculation
      aylprod[i] <- aylprod[i] +  max(0.0, adnpp[i]*irgrowth/(1+fresp$leaf)) #Applying 
      
    }
    
    # getting reproductive stages (rstage)
    if(gdd10 < (gddt-gdds)) {
      rstage = 0
    } else if (gdd10 < p_rstage$`1to2`){
      rstage = 1
    } else if (gdd10 < p_rstage$`2to3`){
      rstage = 2
    } else if (gdd10 < p_rstage$`3to4`){
      rstage = 3
    } else if (gdd10 < p_rstage$`4to5`){
      rstage = 4
    } else if (gdd10 < p_rstage$`5to6`){
      rstage = 5
    } else {
      rstage = 6 #R6 - Mature
    }
    
    assign('rstage'    , rstage    , envir = env)
 
    if(cropy == 1) {
      if ( gdd10 >= gddt | tmin <= tkill[i] | idpp[i] >= mxmat[i] ) { # physiological maturity predicted by the model
        
        if(tmin <= tkill[i]){ print(paste('Forst-Harvest Maize Yield (t/ha)',ID,idpp[i],10*1.14*cbiog[i]/cfrac[i],sep = " ; "    ))}else{
                              print(paste('Harvest Maize Yield (t/ha)',ID,idpp[i],10*1.14*cbiog[i]/cfrac[i],sep = " ; "    ))}
        
        # 
        fileout <- paste("Maize_SEASON.csv")
        write(paste(ID,idpp[i],cbiog[i]/cfrac[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
        # 

        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        pgreenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        lais[i]       <- 0
        endCycle      <- T
        
        dm$root <- 0
        dm$leaf <- 0
        dm$sleaf <- 0
        dm$stem <- 0
        dm$cob <- 0
        dm$grain <- 0
        
      }
    } else {
      print('Maize has only one cycle - Stop')
      stop()
    }
    
    
    # Potential DM mass-based on Hybrid-Maize
    dm$leaf  <- pden * (pla / (specla[i] * m2.kgC_cm2.gDM))/1000  #kg DM m2
    dm$sleaf <- pden * (sen / (specla[i] * m2.kgC_cm2.gDM))/1000  #kg DM m2
    dm$root  <- cbior[i]/cfrac[i]
    
    assign("plas" , plas , envir = env)
    assign("gddvcum" , gddvcum , envir = env)
    assign('gdd8'      , gdd8      , envir = env)
    assign('gdd10'     , gdd10     , envir = env)
    assign('cumPh'     , cumPh     , envir = env)
    assign('pla'       , pla       , envir = env)
    assign('leafwb'    , leafwb    , envir = env)
    assign('dum8'      , dum8      , envir = env)
    assign('xnti'      , xnti      , envir = env)
    assign('sumgdd8'   , sumgdd8   , envir = env)
    assign('xn'        , xn        , envir = env)
    assign('tiphyl'    , tiphyl        , envir = env)
    assign('P3'        , P3        , envir = env)
    assign('tlno'      , tlno      , envir = env)
    assign('ndaysS3'   , ndaysS3   , envir = env)
    assign('cbiosr'    , cbiosr    , envir = env)
    assign('cbiolr'    , cbiolr    , envir = env)
    assign('sumP'      , sumP      , envir = env)
    assign('gdd10p'    , gdd10p    , envir = env)
    assign('gddemerg'  , gddemerg  , envir = env)
    assign('gddgerm'   , gddgerm   , envir = env)
    assign('date_emerg', date_emerg, envir = env)
    assign('date_germ' , date_germ , envir = env)
    assign('plaf'      , plaf      , envir = env)
    assign('laistg3'   , laistg3      , envir = env)
    assign('slfc', slfc, envir = env)
    assign('slft', slft, envir = env)
    assign('lsr', lsr, envir = env)
    assign('plag', plag, envir = env)
    assign('stage'    , stage    , envir = env)
    assign('vstage'    , vstage    , envir = env)
    assign('rstage'    , rstage    , envir = env)
    assign('dm'        , dm        , envir = env)
    assign('coleog'    , coleog    , envir = env)
    assign('sen'       , sen       , envir = env)
    assign('senstg4'   , senstg4       , envir = env)
    assign('senstg4l'   , senstg4l       , envir = env)
    assign('fsen'      , fsen      , envir = env)
    assign('senf'       , senf       , envir = env)
    assign('dtt8'      , dtt8      , envir = env)
    
    assign('psker' , psker , envir = env)
    assign('gpp', gpp, envir = env)
    assign('RGfill', RGfill, envir = env)
    assign('transs', transs, envir = env)
    assign('transl', transl, envir = env)
    
    assign('lais', lais, envir = env)
    
    
    
    # flag to export or not the following csv that Victor uses
    export <- T 
    if(export) {
      fileout <- paste("Maize_DAILY.csv")
      ID <- paste0(jday, iyear)
      if(file.exists(fileout)==F){ 
        write(x = paste(iyear,jday, idpp[i],gdd10,gdd8,stage ,vstage,rstage, aroot[i], aleaf[i], astem[i],
                        cbior[i]/cfrac[i], cbiol[i]/cfrac[i], cbios[i]/cfrac[i],
                        cbioc[i]/cfrac[i], cbiog[i]/cfrac[i],dm$root,dm$leaf,dm$stem,dm$cob,dm$grain,
                        plai[i],pla*pden/(100*100),sen*pden/(100*100),cbiols[i]*specla[i],cbiols[i]/cfrac[i], sep = ";"),
              file = fileout,
              append = F,
              sep = "\n")}else{
                
                # write(x = unlist(dm), file = fileout, append = T, sep = '\t')
                write(x = paste(iyear,jday, idpp[i],gdd10,gdd8,stage ,vstage,rstage, aroot[i], aleaf[i], astem[i],
                                cbior[i]/cfrac[i], cbiol[i]/cfrac[i], cbios[i]/cfrac[i],
                                cbioc[i]/cfrac[i], cbiog[i]/cfrac[i],dm$root,dm$leaf,dm$stem,dm$cob,dm$grain,
                                plai[i],pla*pden/(100*100),sen*pden/(100*100),cbiols[i]*specla[i],cbiols[i]/cfrac[i], sep = ";"),
                      file = fileout,
                      append = TRUE,
                      sep = "\n")
              }
    }
    
    
  }
  
  ztopPft[i] <- (min(plai[i]/5, 1)) * ztopmxPft[i]
  
  assign("endCycle" , endCycle , envir = env)
  assign("ztopPft"  , ztopPft  , envir = env)
  assign("pgreenfrac", pgreenfrac, envir = env)
  assign("idpp"     , idpp     , envir = env)
  #assign("idpe"     , idpe     , envir = env)
  assign("aroot"    , aroot    , envir = env)
  assign("aleaf"    , aleaf    , envir = env)
  assign("astem"    , astem    , envir = env)
  assign("acob"     , acob    , envir = env)
  assign("cbiol"    , cbiol    , envir = env)
  assign("cbiog"    , cbiog    , envir = env)
  assign('cbioc'    , cbioc    , envir = env)
  assign("cbiop"    , cbiop    , envir = env)
  assign("cbios"    , cbios    , envir = env)
  assign("cbior"    , cbior    , envir = env)
  assign("plai"     , plai     , envir = env)
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

# function to get daily degree days (or thermal time)
calc_dtt <- function(dpp, jday, ldtt, udtt) {
  
  dtt <- 0.0
  ldtt <- ldtt + 273.15
  udtt <- udtt + 273.15
  
  tmean  <- (tmax + tmin) / 2.0
  alpha  <- (tmax - tmin) / 2.0
  
  theta1 <- asin((pi / 180) * (ldtt - tmean) / alpha)
  theta2 <- asin((pi / 180) * (udtt - tmean) / alpha)
  
  if(tmin >= ldtt & tmax <= udtt) {                                       #(3) Entirely between both thresholds TL and TU:
    dtt <- tmean - ldtt
  } else if (tmin >= ldtt & tmax > udtt) {                                #(2) Intercepted by the upper threshold TU:
    # SVC ERRADO  dtt <- (1/pi) * ((tmean - ldtt) * (theta2 - theta1) +
    dtt <- (1/pi) * ((tmean - ldtt) * (theta2 + (pi/2)) + 
                       (udtt - ldtt) * ((pi/2) - theta2) - cos(theta2))   
  } else if (tmin < ldtt & tmax <= udtt) {                                # (4) Intercepted by the lower threshold TL:
    # SVC ERRADO  dtt <- (1/pi) * ((udtt - ldtt) * ((pi/2)-theta2) +  alpha * cos(theta2)) 
    dtt <- (1/pi) * ((tmean - ldtt) * ((pi/2)-theta1) +  alpha * cos(theta1))
  } else if (tmin < ldtt & tmax > udtt) {                                 # (1) Intercepted by both thresholds TL and TU:
    dtt <- (1/pi) * ((tmean - ldtt) * (theta2 - theta1) + alpha * (cos(theta1) 
                                                                   - cos(theta2)) + (udtt - ldtt) * ((pi/2) - theta2))
  } else if (tmin > udtt & tmax > udtt) {                                 # (5) Completely above both thresholds TL and TU:
    dtt <- udtt - ldtt
  } else if (tmin < ldtt & tmax < ldtt){                                  # (6) Completely below both thresholds TL and TU:
    dtt <- 0.0
  }
  
  return(dtt)
  
}

