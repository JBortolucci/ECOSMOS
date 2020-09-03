# Trouxe todas os parametros de planta (.CUL .ECO .SPE) aqui para organização
# Precisa remover os parametros 'duplicados', ainda. '#remover' para auxilio

#______________________________________________________________        
# MANAGEMENT VARS (.SBX) 

# Find and Read Planting Details Section 
PLME <- 'S'      # equivalente ao [.SBX] *PLANTING DETAILS: PLME 
SDEPTH <- 2.5    # PLPD from .SBX
SDAGE <-  -99.0  # ! SDAGE     Transplant age (days)
ATEMP <-  -99.0
 
IHARI  <- 'M' # TODO VERIFICAR (provável que pertença ao '[.SBX] *HARVEST DETAILS')
PLTPOP <- 40  # equivalente ao [.SBX] *PLANTING DETAILS: PPOE
ROWSPC <- 0.5 # equivalente ao [.SBX] *PLANTING DETAILS: TPLRS
SDWTPL <- -99 # equivalente ao [.SBX] *PLANTING DETAILS: PLDS

#______________________________________________________________        
# CONTROL VARS (.SBX) 
CROP <-'SB'

# CROPGRO (checar)
{
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CADPR1  <- 0.260   
  CMOBMX  <- 0.024 
  #!*POD LOSS PARAMETERS
  DETACH  <-'N'  
  #!*EVAPOTRANSPIRATION    
  EORATIO <- 1.1
  KEP     <- 0.68
  #!*VEGETATIVE PARTITIONING PARAMETERS
  FRCNOD  <- 0.05
  #!*LEAF SENESCENCE FACTORS
  FREEZ1  <- -2.22    
  FREEZ2  <- -5.00 
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN     <-  0.67 
  KC_SLOPE <-  0.10     
  #!*PLANT COMPOSITION VALUES
  PCARSH <- 0.380
  PLIPSH <- 0.020
  PLIGSD <- 0.020  
  PLIGSH <- 0.280
  PMINSD <- 0.025
  PMINSH <- 0.030
  POASD  <- 0.040 
  POASH  <- 0.040
  PROLFI <- 0.356
  PRORTI <- 0.092
  PROSHI <- 0.250
  PROSTI <- 0.150
  #!*RESPIRATION PARAMETERS
  PCH2O  <- 1.13
  R30C2  <- 0.0040 
  RCH2O  <- 1.242
  RES30C <- 3.5E-04
  RFIXN  <- 2.830
  RLIG   <- 2.174
  RLIP   <- 3.106
  RMIN   <- 0.05
  RNH4C  <- 2.556
  RNO3C  <- 2.556
  ROA    <- 0.929
  RPRO   <- 0.360
  #!*ROOT PARAMETERS
  PORMIN <- 0.02
  RWUEP1 <- 1.50
  RWUMX  <- 0.04
  #!*NITROGEN FIXATION PARAMETERS
  TTFIX  <- 0
}

#SoybeanPhenocrop (PHENOL, VSTAGES, RSTAGES)
{
#______________________________________________________________        
# *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
CSDL       <- 12.58 
CSDVAR     <- CSDL  #code uses CSDVAR
PPSEN      <- 0.311
PH2T5      <- 23.1  # EM-FL - Time between plant emergence and flower appearance (R1) (photothermal days)

PHTHRS <-rep(0,20)
PHTHRS[6]  <- 7.0   # FL-SH - Time between first flower and first pod (R3) (photothermal days)
PHTHRS[8]  <- 16.0  # FL-SD -  Time between first flower and first seed (R5) (photothermal days)
PHTHRS[10] <- 27.00 # SD-PM - Time between first seed (R5) and physiological maturity (R7) (photothermal days)
PHTHRS[13] <- 18.00 # FL-LF - Time between first flower (R1) and end of leaf expansion (photothermal days)

#______________________________________________________________        
# *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
THVAR      <- 0.0      # THVAR   Minimum rate of reproductive development under long days and optimal temperature
#PHTHRS [5,7,9] 
PHTHRS[1]  <-  3.6     # PL-EM  - Time between planting and emergence (V0) (thermal days)           
PHTHRS[2]  <-  6.0     # EM-V1  - Time required from emergence to first true leaf (V1), thermal days           
PHTHRS[3]  <-  0.0     # V1-JU  - Time required from first true leaf to end of juvenile phase, thermal days          
PHTHRS[4]  <-  5.0     # JU-R0  - Time required for floral induction, equal to the minimum number of days for 
#          floral induction under optimal temperature and daylengths, photothermal days 
PM06       <- 0.0      # Proportion of time between first flower and first pod for first peg (peanut only)
PM09       <- 0.35     # Proportion of time between first seed and physiological maturity that the last seed can be formed
PHTHRS[11] <- 12.0     # R7-R8  - Time between physiological (R7) and harvest maturity (R8) (days)           
PHTHRS[12] <- 12.00    # FL-VS  - Time from first flower to last leaf on main stem (photothermal days)          
TRIFL      <- 0.32     # TRIFL   Rate of appearance of leaves on the mainstem (leaves per thermal day)
R1PPO      <- 0.459    # Increase in daylength sensitivity after R1 (CSDVAR and CLDVAR both decrease with the same amount) (h)
OPTBI      <- 20.0     # Minimum daily temperature above which there is no effect on slowing normal development toward flowering (oC)
SLOBI      <- 0.035    # Slope of relationship reducing progress toward flowering if tmin for the day is less than OPTBI

#______________________________________________________________        
# SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
# LEAF GROWTH PARAMETERS
EVMODC <-0.0           # Modifier of rate of vegetative node appearance for the first few nodes, primarily used for peanut 
# ROOT PARAMETERS
#remover RWUEP1<-1.50
# PHENOLOGY PARAMETERS   
TB	<- c( 7,  6, -15, 0, 0)
TO1	<- c(28, 26,  26, 0, 0)
TO2	<- c(35, 30,  34, 0, 0)
TM	<- c(45, 45,  45, 0, 0)

# FOLLOWING LINE: STAGE; REF STAGE; PHOTOPERIOD FUNCTION; TEMPERATURE FUNCT;
# POINTER TO VEGD(1) OR REPDA(2) OR REPDB(3) TEMP SENS; SENS TO WATER;N; AND P     
NPRIOR <- c(1,2,2,4,5,6,6,6,9,9,11,6,6)                                 # The phase of growth at which phase I accumulator can start
DLTYP  <- c('NON','NON','NON','INL','INL','INL','INL','INL','INL','INL','NON','INL','INL')  # Type of curve used for daylength function for phase I:  NON=no  photoperiod sensitivity, INL=inverse linear      
CTMP   <- c('LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','NON','LIN','LIN')  # Type of curve used for temp. function for phase I: LIN=linear,  QDR=quadratic, SIN=sine function
TSELC  <- c(1,1,1,2,2,2,2,2,3,3,1,2,2)                                   # Number of temperature curve to be used for phase I development rate: 1=veg, 2=early rep, 3=late rep 
WSENP  <- c(-0.2,-0.2,-0.4,-0.4,-0.4,-0.4,-0.4,-0.4,0.7,0.7,0,-0.6,-0.9) # Sensitivity of phase I to water stress, varying from -1 (slows dev) to 1 (hastens dev) 
NSENP  <- c(0,0,0,0,0,0,0,0,0.4,0.4,0,0,0)                               # Sensitivity of phase I to Nitrogen stress. Varies from -1 (slows dev) to +1 (hastens dev)
PSENP  <- c(0,0,0,0,0,0,0,0,0.0,0.0,0,0,0)                               # PSENP     Sensitivity of phase I to phosphorus stress (not yet used) 
}

#GROW
{
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- 0.200 # Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  SDPRO <- 0.400 # Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  WTPSD <- 0.19  # Maximum weight per seed (g)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PROLFF  <- 0.112
  PROSTF  <- 0.035
  PRORTF  <- 0.056
  PROSHF  <- 0.050
  PCARLF  <- 0.405000001
  PCARNO  <- 0.479999989
  PCARRT  <- 0.711000025
  PCARSD  <- 0.314999998
  #remover PCARSH  <- 0.379999995
  PCARST  <- 0.663999975
  PLIGLF  <- 7.00000003E-02
  PLIGNO  <- 7.00000003E-02
  PLIGRT  <- 7.00000003E-02
  #remover PLIGSD  <- 1.99999996E-02
  #remover PLIGSH  <- 0.280000001
  PLIGST  <- 7.00000003E-02
  PLIPLF  <- 2.50000004E-02
  PLIPNO  <- 5.00000007E-02
  PLIPRT  <- 1.99999996E-02
  #remover PLIPSH  <- 1.99999996E-02
  PLIPST  <- 1.99999996E-02
  PMINLF  <- 9.39999968E-02
  PMINNO  <- 5.00000007E-02
  PMINRT  <- 5.70000000E-02
  #remover PMINSD  <- 2.50000004E-02
  #remover PMINSH  <- 2.99999993E-02
  PMINST  <- 4.60000001E-02
  POALF   <- 5.00000007E-02
  POANO   <- 5.00000007E-02
  POART   <- 5.00000007E-02
  POASD   <- 3.99999991E-02
  #remover POASH   <- 3.99999991E-02
  POAST   <- 5.00000007E-02
  #remover PROLFI  <- 0.356000006
  PRONOD  <- 0.300000012
  #remover PRORTI  <- 9.20000002E-02
  #remover PROSTI  <- 0.150000006
  #!*CARBON AND NITROGEN MINING PARAMETERS
  ALPHL  <- 0.04
  ALPHS  <- 0.08
  ALPHR  <- 0.04
  ALPHSH <- 0.08
  #!*VEGETATIVE PARTITIONING PARAMETERS
  WTFSD  <- 0.55
  #!*RESPIRATION PARAMETERS
  #remover RMIN   <- 0.05 #todo
}

#ROOTS
{
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RFAC1  <- 7500.0
  RLDSM  <- 0.1
  RTSDF  <- 0.015 
  RTSEN  <- 0.020
  RTDEPI  <- 20.0 # INROOT que chama
  #remover PORMIN <- 0.02
  RTEXF  <- 0.10
  RTDEPI <- 20.0
  XRTFAC <- c(0.00, 3.00, 6.00, 30.00)
  YRTFAC <- c(2.50, 2.50, 2.60, 2.60)
  RTWTMIN <- 0.0 #Soybean - Sempre ZERO
}

#DEMAND
{
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  #remover SDLIP <- 0.200 # Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  #remover SDPRO <- 0.400 # Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  SLAVAR <- 370   # Specific leaf area of cultivar under standard growth conditions (cm2/g)
  SIZELF <- 200   # Maximum size of full leaf (three leaflets) (cm2) [SIZLF no .CUL]
  THRESH <- 78    # Threshing percentage. The maximum ratio of (seed/(seed+shell)) at maturity. Causes seeds to stop growing as their dry weight
  XFRUIT <- 1.000  # Maximum fraction of daily growth that is partitioned to seed + shell TODO: Igual a variavel XFRT no arquivo .cult
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  LNGSH <- 10.0  # Time required for growth of individual shells (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF GROWTH PARAMETERS
  FINREF <- 180.
  SLAREF <- 350.
  SIZREF <- 171.399994
  VSSINK <- 5.0
  SLAMAX <- 950.
  SLAMIN <- 250.0
  SLAPAR <- -0.048
  TURSLA <- 1.50
  XVGROW <- c( 0.0,  1.0,  2.0,  3.0,  4.0,  5.0)
  YVREF  <- c( 0.0, 20.0, 55.0,110.0,200.0,320.0)
  YVGROW <- rep(0,6) #preenchido com uma função de interpolacao/lookup (TABEX)
  XSLATM <- c(-50.0,  00.0,  12.0,  22.0,  60.0)         
  YSLATM <- c( 0.25,  0.25,  0.25,  1.00,   1.0)
  #!*VEGETATIVE PARTITIONING PARAMETERS
  FRLFF  <- 0.24
  FRSTMF <- 0.55
  FRLFMX <- 0.70
  FRLFM   <- 0.70 #TODO Ver exatamento qual está sendo usado
  XLEAF   <- c( 0.0,  1.,   3.3,   5.0,  7.8,  10.5,  30.0,  40.0)
  YLEAF   <- c(0.41, 0.4,  0.42,  0.41, 0.36,  0.32,  0.31,  0.31)
  YSTEM   <- c(0.09, 0.1,  0.21,  0.29, 0.37,  0.49,  0.49,  0.49)
  #!*SEED  COMPOSITION VALUES 
  CARMIN <- 0.180
  LIPOPT <- 23.65 
  LIPTB  <- 7.16
  SLOSUM <- 9.08000022E-03
  #!*SEED AND SHELL GROWTH PARAMETERS
  FNSDT  <- c(6.0,  21.0,  23.5,  41.0) #+ QDR in .SPE
  TYPSDT <- "QDR"
  SHLAG  <- 0
  SRMAX  <- 0.300000012
  XFRMAX <- 0
  XXFTEM <- c(0.00, 5.00, 20.00, 35.00, 45.00, 60.00)
  YXFTEM <- c(1.00, 1.00, 1.00 ,  1.00,  0.00,  0.00)
  XTRFAC <- c(0.00,  0.50,  0.75,  1.00)              
  YTRFAC <- c(0.00,  0.00,  0.00,  0.00)
  #!*CARBON AND NITROGEN MINING PARAMETERS
  NMOBMX <- 0.090
  NRCVR  <- 0.15
  NVSMOB <- 0.35
  #!*PLANT COMPOSITION VALUES
  #remover PLIGSD <- 0.020
  #remover PMINSD <- 0.025
  #remover POASD  <- 0.040
  #remover PROLFF <- 0.112
  #remover PROLFI <- 0.356
  #remover PRORTF <- 0.056
  #remover PRORTI <- 0.092
  #remover PROSTF <- 0.035
  #remover PROSTI <- 0.150
  #!*RESPIRATION PARAMETERS
  #remover RCH2O  <- 1.242
  #remover  RLIG   <- 2.174
  #remover RLIP   <- 3.106
  #remover RMIN   <- 0.05
  #remover RNO3C  <- 2.556
  #remover ROA    <- 0.929
  #remover RPRO   <- 0.360
}

#SDCOMP
{
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  #remover SDLIP <- 0.200 #Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*SEED  COMPOSITION VALUES 
  #remover CARMIN <- 0.180
  #remover LIPOPT <- 23.65 
  #remover LIPTB  <- 7.16
  #!*PLANT COMPOSITION VALUES
  #remover PLIGSD <- 0.020 
  #remover PMINSD <- 0.025
  #remover POASD  <- 0.040
  #!*RESPIRATION PARAMETERS
  #remover RCH2O  <- 1.242
  #remover RLIG   <- 2.174
  #remover RLIP   <- 3.106
  #remover RMIN   <- 0.05
  #remover RNO3C  <- 2.556
  #remover ROA    <- 0.929
  #remover RPRO   <- 0.360
}

#PODS
{
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  #remover XFRT    <- 1.000 # Maximum fraction of daily growth that is partitioned to seed + shell
  SDPDVR  <- 2.05 # ***SDPDV no .CUL*** Average seed per pod under standard growing conditions (#/pod)
  PODUR   <- 10.0  # Time required for cultivar to reach final pod load under optimal conditions (photothermal days)
  #remover THRESH  <- 78    # Threshing percentage. The maximum ratio of (seed/(seed+shell)) at maturity. Causes seeds to stop growing as their dry weight
  #removeR WTPSD   <- 0.19  # Maximum weight per seed (g)
  SFDUR   <- 21.0  # Seed filling duration for pod cohort at standard growth conditions (photothermal days)
  
  #remover PHTHRS  <- rep(0,20)
  #remover PHTHRS[6]  <- 7.0   # FL-SH - Time between first flower and first pod (R3) (photothermal days)
  #remover PHTHRS[8]  <- 16.0  # FL-SD -  Time between first flower and first seed (R5) (photothermal days)
  #remover PHTHRS[10] <- 27.00 # SD-PM - Time between first seed (R5) and physiological maturity (R7) (photothermal days)
  #remover PHTHRS[13] <- 18.00 # FL-LF - Time between first flower (R1) and end of leaf expansion (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  #remover LNGSH <- 10.0 #Time required for growth of individual shells (photothermal days)
  
  #PHTHRS[1]  <-  3.6     # PL-EM  - Time between planting and emergence (V0) (thermal days)           
  #PHTHRS[2]  <-  6.0     # EM-V1  - Time required from emergence to first true leaf (V1), thermal days           
  #PHTHRS[3]  <-  0.0     # V1-JU  - Time required from first true leaf to end of juvenile phase, thermal days          
  #PHTHRS[4]  <-  5.0     # JU-R0  - Time required for floral induction, equal to the minimum number of days for
  # floral induction under optimal temperature and daylengths, photothermal days 
  #PHTHRS[11] <-  12.0     # R7-R8  - Time between physiological (R7) and harvest maturity (R8) (days)           
  #PHTHRS[12] <-  12.0     # FL-VS  - Time from first flower to last leaf on main stem (photothermal days)          
  
  #TODO REMINDER: 5, 7 and 9 are solved in PHENOL.for .:. bring them here?
  #PHTHRS[5] = max(0.,PH2T5 - PHTHRS[3] - PHTHRS[4])
  #PHTHRS[7] = PHTHRS[6] + max(0.,(PHTHRS[8] - PHTHRS[6])* PM06)
  #PHTHRS[9] = max(0.,PHTHRS[10] * PM09)
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*SEED AND SHELL GROWTH PARAMETERS
  DSWBAR  <- 15.0
  SETMAX  <- 0.60
  RFLWAB  <- 0.0
  XMPAGE  <- 100.0
  #TODO verificar esses vetores
  FNPDT   <- c(14.0,21.0,26.5,40.0) # + QDR no .SPE
  TYPPDT <- "QDR"
  XSWBAR  <- c(0.00,  0.01,  0.25,  1.00,  1.00)
  YSWBAR  <- c(1.00,  1.00,  1.00,  1.00,  1.00)
  XSWFAC  <- c(0.00,  0.50,  1.00,  1.00)
  YSWFAC  <- c(0.00,  1.00,  1.00,  1.00)
  #!*PLANT COMPOSITION VALUES
  #remover PROSHI  <- 0.250
  #remover PROLFF  <- 0.112
  #remover PROSHF  <- 0.050
}

#PODCOMP
{
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  #remover PLIGSD <- 0.020 
  #remover PMINSD <- 0.025
  #remover POASD  <- 0.040
  PROMAX <- 0.080
  PROMIN <- 0.030
  THETA  <- 0.800
  #!*RESPIRATION PARAMETERS
  #remover RCH2O  <- 1.242
  #remover RLIG   <- 2.174
  #remover RLIP   <- 3.106
  #remover RMIN   <- 0.05
  #remover ROA    <- 0.929
}

#VEGGR
{
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  #remover KCAN   <- 0.67
  #!*CARBON AND NITROGEN MINING PARAMETERS
  #remover CMOBMX <- 0.024
  #!*RESPIRATION PARAMETERS
  #remover PCH2O  <- 1.13
  #!*PLANT COMPOSITION VALUES
  #remover PROLFI <- 0.356 
  #remover PRORTI <- 0.092
  #remover PROSTI <- 0.150
  PROLFG <- 0.285
  PRORTG <- 0.064
  PROSTG <- 0.100
  #!*VEGETATIVE PARTITIONING PARAMETERS
  ATOP   <- 1.00
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CADSTF <- 0.75
  #*NITROGEN STRESS PARAMETERS
  NRATIO <- 1.00 
}

#CANOPY
{
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  RHGHT  <- 0.9
  RWIDTH <- 1.0 #RWDTH no .ECO
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  #remover KCAN   <- 0.67
  #!*CANOPY HEIGHT AND WIDTH GROWTH PARAMETERS
  # TODO verificar se são 5, 8 (.SPE) ou 10 (.for) posições no vetor 
  XHWPAR  <- c(0.00,  5.00,  7.50, 10.00, 15.00, 20.00, 30.00, 80.00)
  YHWPAR  <- c(4.00,  2.00,  1.50,  1.25,  1.05,  1.00,  1.00,  1.00)
  XHWTEM  <- c(-50.0,  00.0,  15.0,  26.0,  60.0)
  YHWTEM  <- c(0.40,  0.40,  0.50,  1.00,  1.00)
  XVSHT   <- c(0.00,  1.00,  4.00,  6.00,  8.00, 10.00, 14.00, 16.00, 20.00, 40.00)
  YVSHT   <- c(0.0300, 0.0530, 0.0630, 0.0660, 0.0690, 0.0660, 0.0620, 0.0510, 0.0340, 0.0060)
  YVSWH   <- c(0.0300, 0.0510, 0.0620, 0.0640, 0.0660, 0.0630, 0.0590, 0.0460, 0.0250, 0.0010)
}

#PODDET
{
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*POD LOSS PARAMETERS
  DWC    <- 6.0
  PR1DET <- 0.3961
  PR2DET <- -0.865
  XP1DET <- 1.00
  XP2DET <- 0.00
  #!*PHENOLOGY PARAMETERS   
  #remover TB	<- c( 7,  6, -15, 0, 0)
  #remover TO1	<- c(28, 26,  26, 0, 0)
  #remover TO2	<- c(35, 30,  34, 0, 0)
  #remover TM	<- c(45, 45,  45, 0, 0)
}

#SENES
{
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF SENESCENCE FACTORS
  ICMP   <- 0.80
  TCMP   <- 10.0
  SENDAY <- 0.06
  SENRT2 <- 0.20
  SENRTE <- 0.80
  SENMAX <- c(0.0, 0.2, 0.6 , 0.6)
  SENPOR <- c(0.0, 0.0, 0.12, 0.12)
  XSENMX <- c(3.0, 5.0, 10.0, 30.0)
  XSTAGE <- c(0.0, 5.0, 14.0, 30.0)
  #!*VEGETATIVE PARTITIONING PARAMETERS
  PORPT  <- 0.58
  #!*PHOTOSYNTHESIS PARAMETERS
  #remover KCAN   <- 0.67
}

#FREEZE
{
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF SENESCENCE FACTORS
  #remover FREEZ2 <- -5.00
}

#NFIX
{
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*NITROGEN FIXATION PARAMETERS
  TYPFXT <- 'LIN'
  TYPNGT <- 'LIN'
  TYPFXD <- 'LIN'
  TYPFXW <- 'LIN'
  TYPFXA <- 'INL'
  FNFXT  <- c(5.00,    20.0,  35.0,  44.0)
  FNNGT  <- c(7.00,    22.0,  35.0,  44.0)
  FNFXD  <- c(0.00,    0.85,  1.00,  10.0)
  FNFXW  <- c(-0.02,  0.001,  1.00,  2.00)
  FNFXA  <- c(0.00,    0.10,  1.00,  0.00)
  NDTHMX <- 0.07
  NODRGM <- 0.170
  DWNODI <- 0.014
  SNACTM <- 0.045
  CNODCR <- 0.05
  #!*RESPIRATION PARAMETERS
  #remover RFIXN  <- 2.830
  #!*PLANT COMPOSITION VALUES
  #remover PRONOD <- 0.300
}

#NUPTAK
{
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RTNO3  <- 0.006
  RTNH4  <- 0.006
}

#INCOMP
{
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  #remover SDLIP <- 0.200 #Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  #remover SDPRO <- 0.400 #Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  #remover PCARLF   <- 0.405000001
  #remover PCARNO   <- 0.479999989
  #remover PCARRT   <- 0.711000025
  #remover PCARSD   <- 0.314999998
  #remover PCARSH   <- 0.379999995
  #remover PCARST   <- 0.663999975
  #remover PLIGLF   <- 7.00000003E-02
  #remover PLIGNO   <- 7.00000003E-02
  #remover PLIGRT   <- 7.00000003E-02
  #remover PLIGSD   <- 1.99999996E-02
  #remover PLIGSH   <- 0.280000001
  #remover PLIGST   <- 7.00000003E-02
  #remover PLIPLF   <- 2.50000004E-02
  #remover PLIPNO   <- 5.00000007E-02
  #remover PLIPRT   <- 1.99999996E-02
  #remover PLIPSH   <- 1.99999996E-02
  #remover PLIPST   <- 1.99999996E-02
  #remover PMINLF   <- 9.39999968E-02
  #remover PMINNO   <- 5.00000007E-02
  #remover PMINRT   <- 5.70000000E-02
  #remover PMINSD   <- 2.50000004E-02
  #remover PMINSH   <- 2.99999993E-02
  #remover PMINST   <- 4.60000001E-02
  #remover POALF    <- 5.00000007E-02
  #remover POANO    <- 5.00000007E-02
  #remover POART    <- 5.00000007E-02
  #remover POASD    <- 3.99999991E-02
  #remover POASH    <- 3.99999991E-02
  #remover POAST    <- 5.00000007E-02
  #remover PROLFI   <- 0.356000006
  #remover PRORTI   <- 9.20000002E-02
  #remover PROSHI   <- 0.250000000
  #remover PROSTI   <- 0.150000006
  SDPROS   <- 0.400000006
  #!*RESPIRATION PARAMETERS
  #remover RCH2O    <- 1.24199998
  #remover RLIG     <- 2.17400002
  #remover RLIP     <- 3.10599995
  #remover RMIN     <- 5.00000007E-02
  #remover RNO3C    <- 2.55599999
  #remover ROA      <- 0.929000020
}

#MOBIL
{
  #!*RESPIRATION PARAMETERS (.SPE), mas não usado, aparentemente
  #remover RPRO   <- 0.360 
}
