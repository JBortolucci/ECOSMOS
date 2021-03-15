# building simdatavars
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
simDataVars$fr  <-  0
simDataVars$slfc  <-  0
simDataVars$slft  <-  0
simDataVars$lsr  <-  0
simDataVars$plag  <-  0
simDataVars$plaf  <-  0
simDataVars$leafwt  <-  0
simDataVars$leafwg  <-  0
simDataVars$leafwb  <-  0
simDataVars$xnti  <-  0
simDataVars$ds  <-  0
simDataVars$stemwg  <-  0
simDataVars$cobwg  <-  0

simDataVars$dm <- list('leaf' = 0.0, 'stem' = 0.0, 'root' = 0.0, 'cob' = 0.0, 'grain' = 0.0, 'sleaf' = 0)
simDataVars$date_germ <- 0
simDataVars$date_emerg <- 0
simDataVars$coleog <- 0
simDataVars$vstage <- 0
simDataVars$rstage <- 0
simDataVars$sen <- 0
simDataVars$fsen <- 0
simDataVars$senmaxstage2 <- 0
simDataVars$senmaxstage3 <- 0
simDataVars$sumP <- 0
simDataVars$psker <- 0
simDataVars$gpp <- 0 
simDataVars$RGfill <- 0
simDataVars$transs <- 0
simDataVars$transl <- 0
simDataVars$fcbios <- 0
simDataVars$fcbiol <- 0
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
  gddgerm        <- params$gddgerm      # 15  # GDD (base 10ºC) required for germination (default = 15)
  gddemerg       <- params$gddemerg     # 6.0 # GDD (base 10ºC) required for emergence per cm depth (default = 6) [we believe this is in cm/day]
  emerg_limit    <- params$emerg_limit  # 25 # maximum days allowed from sowing to emergence (default = 25)
  gddf           <- params$gddf         # 170     # P5 in CERES-MAIZE and in the manual??? GDD (base 8ºC) from silking to effective grain filling
  gddt           <- params$gddt         # 1600    # GDD (base 10ºC) required from germination to maturity (default = 1500?)
  laic           <- params$laic         # 4       # critical LAI for light competition (condition for SLFC have a chance to affect leaf expansion)
  laim           <- params$laim         # 0.7     # fraction of LAI at maturity of the maximum LAI
  sg             <- params$sg           # 4.0     # ‘stay-green’ factor, which controls how fast leaf senesces proceeds after silking
  phyl           <- params$phyl         # 38.9    #TODO Victor, o que significa esse parametro? vc 'criou' ele? [2021-01-20]
  dsstop         <- params$dsstop       # 1.15  # development stage when root growth stops
  ph1            <- params$ph1          # 47
  ph2            <- params$ph2          # 28
  G2             <- params$G2           # 676 # the potential number of grains per plantkernels ear⁻¹ (default = 676)
  G5             <- params$G5           # 8.7 # the potential grain filling rate (mg d-1 kernel-1) (default = 8.7)
  efftrans       <- params$efftrans     # 0.26 # Efficiency of carbon translocation from leaves/stem to grain filling
  
  gdds <- (0.41 * gddt) + 145.4         # GDD (base 10ºC) required from silking to physiological maturity
  gdds <- 100+0.4451*gddt -50           # last page in the manual
  gddv <- c(ph1, ph2)
  
  rgrowth <- 0.30 #TODO vamos ter que pensar a respeito desse parâmetro ('rgrowthc' é dos crops)... [2021-01-20]
  
  # daily maintenance respiration parameters maize basic morphological components
  #TODO Pensar levamos para plantparams (não vejo necessidade, por hora) [2021-01-20]
  fresp <- list('leaf'  = 0.47, # g-CH2O g-DM⁻¹
                'root'  = 0.45, # g-CH2O g-DM⁻¹
                'stem'  = 0.52, # g-CH2O g-DM⁻¹
                'cob'   = 0.52, # g-CH2O g-DM⁻¹ (same as stem at the moment)
                'grain' = 0.49) # g-CH2O g-DM⁻¹
  
  # R stages adjusted proportionally based on the total GDD10 from R1 to R6 (which is basically the gdds parameter).
  #TODO Pensar levamos para plantparams (não vejo necessidade, por hora) [2021-02-10]
  p_rstage <- list('1to2' = (gddt-gdds)+gdds*0.13, # R1 (silking) to R2 (blister)
                   '2to3' = (gddt-gdds)+gdds*0.27, # R2 to R3 (milk)
                   '3to4' = (gddt-gdds)+gdds*0.42, # R3 to R4 (dough)
                   '4to5' = (gddt-gdds)+gdds*0.67, # R4 to R5 (dent)
                   '5to6' = (gddt-gdds)+gdds*1.00) # R5 to R6 (blacklayer, or physiological maturity)
  
  # Crop initiation
  pgreenfrac[i] <- 0.0 #TODO pensar sobre cultura anterior ou setar 0 na initialização do ECOSMOS e não aqui [2021-01-20] 
  
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
      rest   <- 0.0
      vstage <- 0.0
      rstage <- 0
      sen    <- 0
      fsen   <- 0
      senmaxstage2 <- 0
      senmaxstage3 <- 0
      
      dm <- list('leaf' = 0.0, 'stem' = 0.0, 'root' = 0.0, 'cob' = 0.0, 'grain' = 0.0, 'sleaf' = 0)
     
      #gddemerg <- gddemerg * pdpt # gddgerm taking into account the sowing depth already here
      
      plaf <- 0.0
      
      #TODO check if 'tlno' and 'P3' are dynamic or static [Henrique; 2021-03-04]
      tlno <- 30
      P3 <- 0
      # tlno <- (gdd8 / (phyl * 0.5)) + 6
      # P3 <- (tlno - 2.0) * phyl + 96 - gdd8
      # TLNO is the total number of leaves that will eventually appear (#)
      # P3: "The duration of Stage 2 in terms of GDD equals P3 whose value is determined at the end of Stage 1"
       
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

    #################################
    # From germination to emergence #
    #################################
      
    } else if (idpp[i] > date_germ & coleog <= pdpt) {
      
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
        
        cbiol[i]  <- 2 * pden * 0.4 * 1e-3  # primeiro termo seria a massa inicial
        plai[i]   <- cbiol[i] * specla[i]
        
        cbiols[i] <- 0
        lais[i] <- 0
      }
      
    } else {
    
      # From now on it is similiar to some extent to RATE or INTEGR in the CROPGRO models [check affirmation]
      # In term of model structure, the entire maize growth from emergence to physiological maturity is divided into four stages,
      #     following largely CERES-Maize (Jones and Kiniry, 1986),
      #     but with a merger of stages 1 and 2 into one stage.
      # The four periods used in the Hybrid-Maize model are:
      #     Stage 1: from emergence to tassel initiation                   *** Stages 1 & 2 in CERES ***
      #     Stage 2: from tassel initiation to silking
      #     Stage 3: from silking to effective grain filling
      #     Stage 4: from effective grain filling to physiological maturity.
      
      sen <- list()
      
      ph <- ifelse(test = vstage < 10, yes = 1, no = 2)
      
      if(rest >= gdd10%%gddv[ph] & gdd10 < gdds) {
        vstage <- vstage + 1
      }
      rest <- gdd10%%gddv[ph]
      
      fr <- 1.0 / (1.0 - rgrowth) # check rgrowth comment previously mentioned
      
      # TODO CO2 assimilation in HM does not seem to be penalised by water stress [Henrique; 2021-02-26]
      #     read sections 4.1.2, 4.1.3 & 4.2.4 in the manual
      #     should we include the 'stress' variable from ECOSMOS here, in addition to that applied to leaf expansion?
      adnppl <- adnpp[i] * fr / (1 + fresp$leaf)
      adnpps <- adnpp[i] * fr / (1 + fresp$stem)
      adnppr <- adnpp[i] * fr / (1 + fresp$root)
      adnppg <- adnpp[i] * fr / (1 + fresp$grain)
      
      dtt8  <- calc_dtt(dpp = idpp[i], jday = jday, ldtt =  8, udtt = 34)
      dtt10 <- calc_dtt(dpp = idpp[i], jday = jday, ldtt = 10, udtt = 34)
      gdd8  <- gdd8  + dtt8
      gdd10 <- gdd10 + dtt10

      tmean  <- (tmax + tmin) / 2.0 #TODO talvez usamos do ECOSMOS direto e não precisemos esse passo toda vez [2021-01-20]
    
      # Leaf Growth and Senescence #
      if (cumPh < 5.0) {
        pc <- 0.66 + 0.068 * cumPh
      } else {
        pc <- 1
      }
      
      tiphyl <- dtt8 / (phyl * pc)
      cumPh <- cumPh + tiphyl
      xn <- max(xn, cumPh + 1)
      
      # TI is the fraction of daily increase in leaf number
      #     'ti' here had to be changed to 'tiphyl' because the same term is used in 'turvap' subroutine [Henrique; 2021-03-04]
      # cumPh is the number of fully expanded leaves
      # XN is the leaf number of the oldest expanding leaf
      # PC is an intermediate variable
      # Note: at emergence cumPh = 1 and XN = 3.
      
      # SLFC and SLFT are the leaf stress factors due to competition for light and low temperature, respectively.
      slfc <- ifelse(test = plai[i] < laic,
                     yes  = 1.0,
                     no   = min(1.0, max(0.0, 1.0 - 0.008 * (plai[i] - laic))))
      
      slft <- ifelse(test = tmean > 279.15, #TODO pensar se o limiar de 6º C possa ser um parametro, futuramente. [2021-01-20]
                     yes  = 1.0,
                     no   = min(1.0, max(0.0, 1.0 - (279.15 - tmean) / 279.15)))
      
      # LSR is the stress rate factor (0-1) | 0 means no stress and 1 full stress
      lsr <- ifelse(test = slfc <= slft,
                    yes  = 1.0 - slfc,
                    no   = 1.0 - slft)
      
      if (gdd8 / (gdd8 + P3) > gdd10 / gdds) {
        
        ################################################
        # STAGE 1: from emergence to tassel initiation #  *** Stages 1 & 2 in CERES ***
        ################################################
        
        # Leaf Growth and Senescence #
        # The daily expansion of leaf area (PLAG) and growth of leaf biomass is driven by temperature, and the choice of functions depends on growth stage.
        
        # Testings after CERES-MAIZE review [Henrique; 2021-03-04]
        #VEGPHASE <- gddt-gdds
        #tlno <- (VEGPHASE / (phyl * 0.5)) + 6
        tlno <- (gdds / (phyl * 0.5)) + 6
        P3 <- (tlno - 2.0) * phyl + 96 - gdds
        #tlno <- (gdd8 / (phyl * 0.5)) + 6      # # this parameter is fixed as 30 in CERES in stages 1 & 2
        #P3 <- (tlno - 2.0) * phyl + 96 - gdd8  # this parameter remains as 0 in CERES in stages 1 & 2
        
        plag <- ifelse(test = xn < 4,
                       yes  = 3.0 * xn * tiphyl,
                       no   = 3.5 * (xn^2) * tiphyl)

        #TODO includes water and others 'stress' in plag following... [Henrique; 2021-02-26]
        #    Keating et al. (2003) in pages 69-70 and/or CERES-MAIZE
        #    CEREs also includes waterlogging, and seems to be accomodated for K and P [*AMIN1(TURFAC,(1.0-SATFAC),PStres2, KSTRES)]
        
        pla <- pla + plag
        #TODO CERES-MAIZE includes **1.25 in leafwt (XLFWT)
        leafwt <- pla / (specla[i] * 10) # 10 (convertion SPECLA from m²/kg DM to cm²/g DM)
        #TODO CERES-MAIZE has an intermediate variable called LFWT which is used XLFWT  = AMAX1 (XLFWT,LFWT)
        leafwg <- leafwt - leafwb
        leafwb <- leafwt
        xnti <- xn
        #TODO include roots?
        
        # PLA (cm2 plant-1) is the total leaf area per plant
        # SLA (cm2 g-1, ≤ 400 cm2 g-1) is the specific leaf area
        # leafWtToday (g plant-1) is the leaf biomass after update
        # leafWt is the leaf biomass of the previous day
        # leafWtGrow (g plant-1) is the daily growth in leaf biomass
        
        sen$slan <- gdd8 * pla / 1e4
        sen$plas <- plag * lsr
        sen <- max(unlist(sen))
        fsen <- sen / pla
        pla <- max(0, pla - sen)
        leafwg <- leafwg / (1.0 + fresp$leaf)
        #TODO CERES uses a different approach here
        #TODO includes 'CumLeafSenes' now
        
        # SLAN (cm2 plant-1) is the accumulated leaf senescence caused by natural development
        # PLAS (cm2 plant-1 d-1) is the daily leaf senescence due to competition for light and temperature stress (affected by )
        # PLAG (cm2 plant-1 d-1) is the daily expansion of leaf area
        
        # Root growth #
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        aroot <- max(0.0, min(0.5, arooti - (ds * (arooti / dsstop))))
        # aroot equals to ACroot at emergence (=biomass allocation coefficient for root at emergence).
        # DSstop is the development stage when root growth stops
        # The default values for ACEroot and DSstop are 0.35 and 1.15, respectively.
        
        aleaf <- max(0.0, 1.0 - aroot)
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot * adnppr)
        cbiolg <- max(0.0, aleaf * adnppl)
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen

        # DM mass-based for model evaluation
        dm$leaf <- dm$leaf + leafwg - sen / (specla[i] * 10)
        dm$sleaf <- dm$sleaf + sen / (specla[i] * 10) #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
        dm$root <- dm$root + (cbiorg * 1e3 / (pden * 0.40)) - (dm$root / tauroot)
        #TODO Victor, a taxa de morte da raiz está sendo considerada via tauroot ? [2021-01-20]
        
        # cat('\nStage 1\n')
        # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
      } else if (gdd10 < gdds) {
        
        ##############################################
        # STAGE 2: from tassel initiation to silking #
        ##############################################
        
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
        
        leafwg <- leafwg / (1.0 + fresp$leaf)
        stemwg <- stemwg / (1.0 + fresp$stem)
        
        sen$slan <- gdd8 * pla / 1e4
        sen$plas <- plag * lsr #TODO includes water 'stress' here following Keating et al. (2003) in pages 69-70 [Henrique; 2021-02-26]
        sen <- max(unlist(sen))
        
        senmaxstage2 <- sen #dummy variable to avoid starting with low fsen in stage 3
        
        fsen <- sen / pla
        pla <- max(0, pla - sen)
        
        ds <- max(0.0, min(1.0, gdd10 / gdds))
        # aroot equals to ACroot at emergence (=biomass allocation coefficient for root at emergence).
        # DSstop is the development stage when root growth stops
        # The default values for ACEroot and DSstop are 0.35 and 1.15, respectively.
        
        aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        aleaf <- max(0.0, (1.0 - aroot) * (leafwg / (leafwg + stemwg)))
        astem <- max(0.0, 1.0 - aroot - aleaf)
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot * adnppr)
        cbiolg <- max(0.0, aleaf * adnppl)
        cbiosg <- max(0.0, astem * adnpps)
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot[i])
        cbiol[i] <- cbiol[i] + cbiolg - (cbiol[i] * fsen)
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen
        cbios[i] <- cbios[i] + cbiosg
        
        # DM mass-based for model evaluation
        dm$leaf <- dm$leaf + leafwg - sen / (specla[i] * 10)
        dm$sleaf <- dm$sleaf + sen / (specla[i] * 10) #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
        dm$root <- dm$root + (cbiorg * 1e3 / (pden * 0.40)) - (dm$root / tauroot)
        dm$stem <- dm$stem + stemwg
        
        # cat('\nStage 2\n')
        # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
        dum8 <- gdd8
        plaf <- pla
        
      } else if (gdd8 < dum8 + gddf) {
        
        ####################################################
        # STAGE 3: from silking to effective grain filling #
        ####################################################
        
        # Leaf Growth and Senescence #
        
        #sen$slan <- pla / 1e3 #  Warning: divergence between HM Manual and Yang et al. in FCR 87 (2004) [Henrique/Victor; 2021-03-03]
        #CERES-MAIZE sen$slan <- plaf * 0.05 *(1+(sumgdd8/gddf))
        
        sumgdd8 <- sumgdd8 + (dtt8 / (1 - lsr))
        #sf <- laim * min(1, ((sumgdd8 / gddf) ^ sg)) # gddf = P5 in model's manual ? | had to limit to avoid excessive sen [Henrique/Victor; 2021-03-03]
        sf <- laim * min(1, ((sumgdd8 / gdds) ^ sg)) #TODO gdds = P5 in model's manual (?) is base 10! e AGORA?
        sen$plas <- plaf * sf # plaf antes & ver fator temp
        sen <- max(unlist(sen))
        
        #TODO LAI still decreasing rapidly after R1 compared to Yang et al. in FCR 87 (2004) 131–154 [Henrique; 2021-02-26]
        #TODO verify this manipulation. perhaps 'lsr', 'plag' & 'plaf' calculations should be included here again [Henrique; 2021-02-26]
        #TODO includes water 'stress' here following Keating et al. (2003) in page 70 [Henrique; 2021-02-26]
        # check manipulation to avoid low sen & fsen values between stages 2 & 3 [Henrique; 2021-02-26]
        sen <- max(senmaxstage2, sen)
        
        pla <- max(0, pla - sen)
        fsen <- sen / plaf
        
        # SF (0 to 1) is the fraction of senesced leaf of the maximum green leaf area, which is achieved at silking
        # LAImature is the fraction of LAI at maturity of the maximum LAI
        # SG is a ‘stay-green’ factor, which controls how fast leaf senesces proceeds after silking
        
        # Stem, Cob and Root Growth #
        stemwg <- 0.220 * dtt8 / (1.0 + fresp$stem)
        cobwg  <- 0.088 * dtt8 / (1.0 + fresp$cob)
        
        # The course of crop development is measured by development stage (DS)
        #   on a scale ranging from 0 to 2, with 1 at silking.
        ds <- max(1.0, min(2.0, 1.0 + (gdd10 - gdds)/(gddt - gdds)))
        
        aroot <- max(0.0, min(0.50, arooti - (ds * (arooti / dsstop))))
        acob  <- max(0.0, cobwg * (1.0 - aroot) / (cobwg + stemwg))
        astem <- max(0.0, 1.0 - aroot - acob)
        
        # C mass-based for ECOSMOS 
        cbiorg <- max(0.0, aroot * adnppr)
        cbiocg <- max(0.0, acob  * adnpps)
        cbiosg <- max(0.0, astem * adnpps)
        
        # sumP <- sumP + cbiorg + cbiocg + cbiosg
        
        if(cbioc[i] == 0.0) {
          cbioc[i] <- cbios[i] * 0.167
          cbios[i] <- cbios[i] - cbios[i]
        }
        
        cbior[i] <- cbior[i] + cbiorg - (cbior[i] / tauroot)
        cbioc[i] <- cbioc[i] + cbiocg
        cbios[i] <- cbios[i] + cbiosg
        cbiol[i] <- cbiol[i] - cbiol[i] * fsen
        cbiols[i] <- cbiols[i] + cbiol[i]*fsen
        
        ndaysS3 <- ndaysS3 + 1
        
        fcbios <- cbios[i] * 0.60
        fcbiol <- cbiol[i] * 0.15
        
        # DM mass-based for model evaluation per plant here
        dm$leaf <- dm$leaf - sen / (specla[i] * 10)
        dm$sleaf <- dm$sleaf + sen / (specla[i] * 10) #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
        dm$root <- dm$root + (cbiorg * 1e3 / (pden * 0.40)) - (dm$root / tauroot)
        dm$stem <- dm$stem + stemwg
        dm$cob  <- dm$cob + cobwg
        
        sumP = sumP + adnpp[i] * 1e3 / (pden * 0.40) # g C to g CH2O per plant
        senmaxstage3 <- sen 
        
        # cat('\nStage 3\n')
        # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
          
      } else if (gdd10 < gddt) {
        
        ###################################################################
        # STAGE 4: from effective grain filling to physiological maturity #
        ###################################################################
        
        # Leaf senescence # (leaf growth stops from here onwards)
        sumgdd8 <- sumgdd8 + (dtt8 / (1 - lsr))
        # CERES-MAIZE: sen$slan <- plaf * (0.1+0.8*((sumgdd8/gddf)^3)) 
        #sf <- laim * min(1, ((sumgdd8 / gddf) ^ sg)) # gddf = P5 in model's manual? | had to limit to avoid excessive sen [Henrique/Victor; 2021-03-03]
        sf <- laim * min(1, ((sumgdd8 / gdds) ^ sg)) #TODO gdds = P5 in model's manual (?) is base 10! e AGORA?
        sen$plas <- plaf * sf  # plaf antes & fator temp
        sen <- max(unlist(sen))
        #if(idpp==115)browser()
        #sen <- ifelse(sen < senmaxstage3, sen+senmaxstage3, sen)
        sen <- max(senmaxstage3, sen)
      
        pla <- max(0, pla - sen)
        fsen <- sen / plaf
        
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
        
        # OLD manual?
        # psker = sumP * 1000 / ndaysS3 * 3.4 / 5
        # gpp = G2 * (psker - 195) / (1213.2 + psker - 195)
        
        # NEW manual?
        # psker <- sumP / (1 + fresp$grain) * 1e3 / ndaysS3 * 3.4 / 5.0 # we don't know the mathematical manipulation here
        # gpp <- G2 - 676 / (psker/1e3) # mg as in the manual, but it may be wrong! 
        
        # Our version
        # KNP = 573 - 676/PGR
        #    where, KNP in # kernels per plant with one ear; PGR = g/d per plant [0-15 +-]
        psker <- sumP / ndaysS3
        gpp <- min(max(0, G2 - 676 / psker), G2)
        
        # CERES-MAIZE
        # PSKER = SUMP*1000.0/IDURP*3.4/5.0
        # GPP   = G2*PSKER/7200.0 + 50.0
        # GPP   = AMIN1 (GPP, G2)
        # GPP   = AMAX1 (GPP,0.0)
        # such model still considers barrenness (esterelidade in Portuguese)
        
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
        grainwg <- 0.40 * grainwg * pden / 1e3 # convert from g-DM plant⁻¹ day⁻¹ to kg-C m⁻² day⁻¹
        #TODO Victor, a unidade do 'grain' ainda está bem diferente das demais... [2021-01-21]
        
        transs <- 0.0
        transl <- 0.0

        # C mass-based for ECOSMOS 
        #if(idpp==79) browser()
        if (grainwg > adnppg & fcbios >= abs(grainwg - adnppg)) {
          transs <- abs(grainwg - adnppg)
          grainwg <- grainwg + efftrans * transs
          fcbios <- fcbios - efftrans * transs
          cbios[i] <- cbios[i] - transs * efftrans
        } else {
          transl <- min(0.005 * fcbiol, abs(grainwg - adnppg))
          grainwg <- grainwg + efftrans * transl
          fcbiol <- fcbiol - efftrans * transl
          cbiol[i] <- cbiol[i] - transl * efftrans
        }
        
        if (grainwg < adnppg) {
          fcbios <- fcbios + abs(grainwg - adnppg)
          cbios[i] <- cbios[i] + abs(grainwg - adnppg)
        }
        
          cbiog[i] <- cbiog[i] + grainwg
          cbiols[i] <- cbiols[i] + fsen*cbiol[i]
          
          # DM mass-based for model evaluation
          dm$leaf  <- dm$leaf - (efftrans * transl * 1e3 / (pden * 0.40))
          dm$sleaf <- dm$sleaf + sen / (specla[i] * 10) #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
          dm$stem  <- dm$stem - (efftrans * transs * 1e3 / (pden * 0.40))
          dm$grain <- dm$grain + (grainwg * 1e3 / (pden * 0.40))
          
          # cat('\nStage 4\n')
          # print(c('dpp' = idpp, 'cbior' = cbior[i], 'cbiol' = cbiol[i], 'cbios' = cbios[i], 'cbioc' = cbioc[i], 'cbiog' = cbiog[i], 'lai' = plai[i]))
        
      }
      
      #TODO Victor, o que precisamos pensar e eventualmente fazer: [2021-01-20]
      # 1) se vamos usar o froot ou vamos adaptar ele pro item 4.2.1. Rooting Depth and Water Uptake Weighting Factor do manual
      #    aqui teria parâmetros novos, como: 
      #    - Depthmax (represents the depth of soil without physical or chemical restrictions to root growth) default = 150 cm 
      #    - VDC (is the vertical distribution coefficient that determines) default = 3
      # 2) se vamos trazer a parte de absorção de água e penalização de por estresse hídrico que é ~ DSSAT
      # 3) trazer as condições e cálculos do gdd10 ou gdd8 (aquele lance da empresa de melhoramento/pesquisa)
      #    - item 4.3. Correlations of total GDD to RM and GDD-to-silking to total GDD
      #    - um dia procurei saber se tem algo pros híbridos daqui do GDD8, mas nada
      #    - tem a maturidade relativa (RM), que daria pra usar a equação da fig 4.1 como aproximação
      #    - podemos ver com Santiago se ele descobre algo com pessoal da Embrapa Milho & Sorgo
      # 4) retornar os R stages, usando as proporções que virão dos valores da pagina 69 (pdf) [Ok]
      
      # Victor, 'desliguei' o print por hora aqui pras minhas simulações [2021-01-20]
      # print('\n')
      # print(unlist(dm, use.names = T))
      
      # update vegetation's physical characteristics
      # plai[i] <- min(max(0.0, cbiol[i] * specla[i]),5) #TODO check
      #TODO verificar na relação folha verde/folha seca [2021-01-20]
      #TODO verificar o comportamento do 'sen', que está meio estranho [2021-02-25]
      plai[i] <- max(0.01, cbiol[i] * specla[i])
      lais[i] <- cbiols[i] * specla[i] #TODO think about reducing the specla for dead leaves [Henrique/Victor; 2021-03-03]
      pgreenfrac[i] <- max(0.01,1-lais[i]/plai[i])
      # print(c('lai' = plai[i]))
      peaklai[i]  <- max(peaklai[i], plai[i])

      biomass[i] <- cbiol[i] +  cbior[i] + cbios[i] + cbioc[i]
      # print(c('biomass' = biomass[i]))
      
      # keep track of aboveground annual npp
      ayanpp[i] <- ayanpp[i] + adnpp[i]
      
      # keep track of total biomass production for the entire year, and the
      aybprod[i] <- aybprod[i] +
        aleaf[i] * max(0.0, adnppl) +
        aroot[i] * max(0.0, adnppr) +
        astem[i] * max(0.0, adnpps) +
        acob  * max(0.0, adnpps)
      
      # aboveground value to calculate harvest index
      ayabprod[i] <- ayabprod[i] +
        aleaf[i] * max(0.0, adnppl) +
        astem[i] * max(0.0, adnpps) +
        acob  * max(0.0, adnpps)
      
      
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
      
      # flag to export or not the following csv that Victor uses
      export <- F 
      if(export) {
      fileout <- paste("Maize_DAILY.csv")
      
      if(idpp[i] == 1) ID <- paste0(jday, iyear)
      # write(x = unlist(dm), file = fileout, append = T, sep = '\t')
      write(x = paste(ID, idpp[i], aroot[i], aleaf[i], astem[i], cbior[i], cbiol[i], cbios[i], cbioc[i], cbiog[i], plai[i], sep = ";"),
            file = fileout,
            append = TRUE,
            sep = "\n")
      }
    
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
      rstage = 6
    }
    
    assign('rstage'    , rstage    , envir = env)
    
    if(cropy == 1) {
      if ( gdd10 >= gddt ) { # physiological maturity predicted by the model
        
        # print(paste('Harvest Maize ',ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai[i],cbiog[i],sep = " ; "    ))
        
        # Victor, 'desliguei' o print e os outputs por hora aqui pras minhas simulações [2021-01-20]
        # cat('\nDates: Germination:',date_germ,'| Emergence:',date_emerg)
        # 
        # fileout <- paste("Maize_SEASON.csv")
        # write(paste(ID,idpp[i],ndiasV6,ndiasR0,ndiasR4,ndiasR9,DVS,peaklai,cbiog[i],sep=";"),file =fileout,append=TRUE,sep = "\n")
        # 
        # cat('\nMaize harvested! Harvest date:', paste(sprintf('%04d',year), sprintf('%02d', month), sprintf('%02d', day), sep = '-'),'\n')
        
        croplive[i]   <- 0.0
        cropy         <- 0.0
        idpp[i]       <- 0.0
        pgreenfrac[i]  <- 0.0 # turn all vegetation to brown
        harvdate[i]   <- jday
        plai[i]       <- 0.01 # simulates remaining stubble/mulch
        lais[i]       <- 0
        peaklai[i]    <- 0.0
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
    assign('fcbios'    , fcbios    , envir = env)
    assign('fcbiol'    , fcbiol    , envir = env)
    assign('sumP'      , sumP      , envir = env)
    assign('gdd10p'    , gdd10p    , envir = env)
    assign('gddemerg'  , gddemerg  , envir = env)
    assign('gddgerm'   , gddgerm   , envir = env)
    assign('date_emerg', date_emerg, envir = env)
    assign('date_germ' , date_germ , envir = env)
    assign('plaf'      , plaf      , envir = env)
    assign('fr', fr, envir = env)
    assign('slfc', slfc, envir = env)
    assign('slft', slft, envir = env)
    assign('lsr', lsr, envir = env)
    assign('plag', plag, envir = env)
    assign('vstage'    , vstage    , envir = env)
    # assign('rstage'    , rstage    , envir = env)
    assign('rest'      , rest      , envir = env)
    assign('dm'        , dm        , envir = env)
    assign('coleog'    , coleog    , envir = env)
    assign('sen'       , sen       , envir = env)
    assign('fsen'      , fsen      , envir = env)
    assign('senmaxstage2'      , senmaxstage2      , envir = env)
    assign('senmaxstage3'      , senmaxstage3      , envir = env)
    assign('dtt8'      , dtt8      , envir = env)

    assign('psker' , psker , envir = env)
    assign('gpp', gpp, envir = env)
    assign('RGfill', RGfill, envir = env)
    assign('transs', transs, envir = env)
    assign('transl', transl, envir = env)
    
    assign('lais', lais, envir = env)

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

# function to get daily degree days (or thermal time)
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

