
# Contem as subrotinas: (aqui como 'function's)
# 1) PHENOL  (Calculates phenological development.)
# 2) VSTAGES (Calculates V-stages)
# 3) RSTAGES (Calculates phenological stages and individual phase durations)

# os comentarios com 'R' na frente são para controle e serão removidos posteriormente
# 'C' ou '!' sao comentarios do codigo original em fortran, portanto, nao usados pelo CROPGRO
# checar 'ifelse's e 'inputs'
# checar GO TO's e IF's sem THEN

# fortran to R
# .LT.  meaning <
# .LE.  meaning <=
# .GT.  meaning >
# .GE.  meaning >=
# .EQ.  meaning =
# .NE.  meaning !=
# .OR.  meaning |
# .AND. meaning &
# DO/ENDDO   meaning for()
# ELSE IF    meaning else if
# AMAX1 & AMIN1  meaning max and min


#=======================================================================
# PHENOL Subroutine CROPGRO-DSSAT
#  PHENOL, Subroutine, J. W. Jones
#  Calculates phenological development.
#-----------------------------------------------------------------------
#     Called from:    CROPGRO
#     Calls:          RSTAGES
#                     VSTAGES
#                     CURV
#=======================================================================

simDataVars$DRPP     <- 0.0
simDataVars$DTX      <- 0.0
simDataVars$DXR57    <- 0.0
simDataVars$FRACDN   <- 0.0
simDataVars$TDUMX    <- 0.0
simDataVars$TDUMX2   <- 0.0
simDataVars$TNTFAC   <- 0.0
simDataVars$TNTFC2   <- 0.0
simDataVars$SWFAC    <- 1.0
simDataVars$TURFAC   <- 1.0
simDataVars$FNSTR    <- rep(1.,20)
simDataVars$FPSTR    <- rep(1.,20)
simDataVars$FSW      <- rep(1.,20)
simDataVars$FT       <- rep(0.,20)
simDataVars$FUDAY    <- rep(0.,20)
simDataVars$MDATE    <-0
simDataVars$NDLEAF   <-0
simDataVars$NDSET    <-0
simDataVars$NR1      <-0
simDataVars$NR2      <-0
simDataVars$NR5      <-0
simDataVars$NR7      <-0
simDataVars$NVEG0    <-0
simDataVars$PHTHRS   <-0
simDataVars$RSTAGE   <-0
simDataVars$RVSTGE   <-0
simDataVars$VSTGED   <-0
simDataVars$VSTAGP   <-0
simDataVars$STGDOY   <- rep(9999999,20)
simDataVars$SeedFrac <-0
simDataVars$TDUMX    <-0
simDataVars$TDUMX2   <-0
simDataVars$VegFrac  <-0
simDataVars$VSTAGE   <-0
simDataVars$YREMRG   <-0
simDataVars$YRNR1    <-0
simDataVars$YRNR2    <-0
simDataVars$YRNR3    <-0
simDataVars$YRNR5    <-0
simDataVars$YRNR7    <-0
simDataVars$JPEND    <-0 
simDataVars$NDVST    <-0 
simDataVars$NVALPH   <- rep(10000,20)
simDataVars$NVEG1    <-0
simDataVars$NR0      <-0
simDataVars$NR3      <-0
simDataVars$PHZACC   <- rep(0,20)
simDataVars$VegTime  <-0
simDataVars$PROG     <- rep(0,20)
simDataVars$TGRO     <- rep(1.,24)



PHENOL <- function (iyear, iyear0, jday,DAS,DYNAMIC){
  
  
  environment(VSTAGES)             <- env
  environment(RSTAGES)             <- env
  
#_______________________________________________________________________________  
# Vars that are solverd by ECOSMOS  
  
  TGRO_T <-read.table(file = 'C:/DSSAT47/Soybean/TGRO.OUT')
  
#  TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
   DAYL   <- TGRO_T$V4[TGRO_T$V1==DAS & TGRO_T$V2==1]
   XPOD   <- TGRO_T$V5[TGRO_T$V1==DAS & TGRO_T$V2==1]
   SWFEM  <- TGRO_T$V6[TGRO_T$V1==DAS & TGRO_T$V2==1]
   TMING  <- TGRO_T$V7[TGRO_T$V1==DAS & TGRO_T$V2==1]
   TSDEP  <- TGRO_T$V8[TGRO_T$V1==DAS & TGRO_T$V2==1]

  #  DAYL   <- daylength/60. # ! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
  # XPOD   <- ??? # TO Do depois de implementar GROW
  # SWFEM  <- modelo resolve abaixo com base na umiade do solo
   # TMING  <- tmin 
  # SWFEM  <- modelo resolve abaixo com base na temperatura do solo
# Vars that are solverd by ECOSMOS  
#____________________________________________________________________________      
      
      
#    
  YRDOY   = paste0(iyear,jday)
  YRSIM   = paste0(iyear0,1)
  
  NPHS = 13
  TS   = 24
  
  ISIMI  = 'P' 
  #         ISIMI      Start of simulation code
  #               E = On reported emergence day
  #               I = When initial conditions measured
  #               P = On reported planting date
  #               S = On specified date
  

  NSTRES   <- 1 #Nitrogen stress factor (1=no stress, 0=max stress) 
  PStres2  <- 1 
  
  

#   if (DYNAMIC == 'RUNINIT') {
    
    YRPLT   = paste0(iyear,jday)
    
    
    #***************************************************************************
    #            Run Initialization - Called once per simulation               #
    #***************************************************************************        
    
    
    #------------------------------------------------------------------------
    #     Subroutine IPPHENOL reads required phenology variables from input #
    #------------------------------------------------------------------------
    
    
    #______________________________________________________________        
    # CONTROL VARS (.SBX) 
    CROP <-'SB'        
    
    # Find and Read Planting Details Section 
    PLME <- 'S'        
    SDEPTH <- 2.5  # PLPD from .SBX
    SDAGE <-  -99.0  # ! SDAGE     Transplant age (days)
    ATEMP <-  -99.0 
    
    
    #______________________________________________________________        
    # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
    PHTHRS<-rep(0,20) 
    CSDL       <- 12.58 
    CSDVAR     <- CSDL  #code uses CSDVAR
    PPSEN      <- 0.311
    PH2T5      <- 23.1  # EM-FL - Time between plant emergence and flower appearance (R1) (photothermal days)
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
    RWUEP1<-1.50
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
    
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
#   } else if (DYNAMIC == 'SEASINIT') {
     if (DYNAMIC == 'SEASINIT') {
    
       
       TRIFOL<-TRIFL
       
       PHTHRS[5] = max(0.,PH2T5 - PHTHRS[3] - PHTHRS[4])
       PHTHRS[7] = PHTHRS[6] + max(0.,(PHTHRS[8] - PHTHRS[6])* PM06)
       PHTHRS[9] = max(0.,PHTHRS[10] * PM09)
       
       #          CLDVAR    Critical daylength above which development rate remains at min value (prior to flowering) (hours)                    
       if (PPSEN >= 0.0) {
         CLDVAR = CSDVAR + (1.-THVAR)/max(PPSEN,0.000001)
       } else if (PPSEN <= 0.0) {
         CLDVAR = CSDVAR + (1.-THVAR)/min(PPSEN,-0.000001)
       }
       
       CLDVRR = CLDVAR - R1PPO     # Critical daylength above which development rate remains at min value (after flowering) (hours)     
       CSDVRR = CSDVAR - R1PPO     # Critical daylength above which development rate decreases (after flowering) (hours)                  
       
       #------------------------------------------------------
       #      END  SUBROUTINE IPPHENO                        #
       #------------------------------------------------------
       
       
       #-----------------------------------------------------------------------
       #     Set minimum days for phenological events under optimum conditions
       #     (temperature and short photoperiod)
       #-----------------------------------------------------------------------
       if (CROP != "FA") {
         # Minimum days from emergence to Vegetative Growth Stage 1:
         MNEMV1 = PHTHRS[2]
         
         # Minimum days from start of flowering to last leaf appearance:
         MNFLLL = PHTHRS[13]
         
         # Number of days from flowering to harvest maturity
         MNFLHM = PHTHRS[8] + PHTHRS[10] + PHTHRS[11]
       }
       
       
       
    #-----------------------------------------------------------------------
    #     Initialization variables from INPLNT
    #-----------------------------------------------------------------------
      DRPP   = 0.0
      DTX    = 0.0
      DXR57  = 0.0
      FRACDN = 0.0
      TDUMX  = 0.0
      TDUMX2 = 0.0
      TNTFAC = 0.0
      TNTFC2 = 0.0
      SWFAC  = 1.0
      TURFAC = 1.0
      FNSTR <- rep(1,20)
      FPSTR <- rep(1,20)
      FSW   <- rep(1,20)
      FT    <- rep(0,20)
      FUDAY <- rep(0,20)
    
    
    RSTAGES (DAS,DYNAMIC,
                             FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    # Input
                             PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM)      # Input
    
    
     VSTAGES (DAS, DTX, EVMODC, MNEMV1, NDVST,             # Input
                            NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
                            TURFAC, XPOD, YRDOY, YRPLT,                     # Input
                            DYNAMIC)                                       # Control
    
    
    #***********************************************************************
    #***********************************************************************
    #     Daily Rate calculations
    #***********************************************************************
  } else if (DYNAMIC == 'RATE') {      
    
    #-----------------------------------------------------------------------
    #     Compute temp, daylength, and water effects on development,
    #-----------------------------------------------------------------------
    #   EMERGENCE PHASE ONLY
    #-----------------------------------------------------------------------
    if (NVEG0 > DAS) {
      FUDAY[1] = 1.
      FNSTR[1] = 1.
      FPSTR[1] = 1.
      K = TSELC[1]
      
      #-----------------------------------------------------------------------
      #      Compute average soil temp, water in top 10 cm for emergence phase
      #         SWFEM = Average soil water content of top 10 cm
      #         TSDEP = Average temperature of top 10 cm
      #-----------------------------------------------------------------------
      
      XDEP  = 0.0
     # SWFEM = 0.0
     # TSDEP = 0.0
      
      
      for (k in 1:(nsoilay)){
        XDEPL = XDEP
        XDEP = XDEP + hsoi[k]*100        #use in cm 
        DTRY = min(hsoi[k]*100, 10. - XDEPL)
        
        
        if (wsoi[k] <= sfield[k]) {
        #  SWFEM = SWFEM + DTRY * (max(wsoi[k] - swilt[k],0.0)) / (sfield[k] - swilt[k])
        } else {
        #  SWFEM = SWFEM + DTRY * (max(poros[k] - wsoi[k]*poros[k],0.0)) / (poros[k] - sfield[k]*poros[k])
        }
        # TSDEP = TSDEP + DTRY * (tsoi[k]-273.16)
        if (XDEP >= 10.) { break}
      }
      #  TSDEP = TSDEP / 10.
      
      #-----------------------------------------------------------------------
      #      Compute temperature and soil water effects for phase 1, emergence
      #-----------------------------------------------------------------------
      
      FT[1] = CURV(CTMP[1],TB[K],TO1[K],TO2[K],TM[K],TSDEP) #todo: escrever função CURV () em algum outro script
       # SWFEM = (SWFEM / 10.) * 100.0
      FSW[1] = CURV("LIN",0.0,20.0,100.,1000.,SWFEM)
      
      FSW[1] = 1. + (1.-FSW[1])*WSENP[1]
    }
    
    
    
    #       Calculate daily water stess factors (from SWFACS)
    #       EOP -  Potential plant transpiration rate (mm/d)
    #       TRWUP and EP1 in cm/d
    
    # Transpiration ratio (CO2=330 vpm gives 1.0)
    # To do: Implementar a chamada da função StomataC3Crops e StomataC4Crops fazendo 'stresst = 1', 
    #          e salvar totcond como 'totconduns' (i.e., sem estress) e 'ag' como 'aguns'. 
    #        Depois calcular fvaput e fvaplt considerando sut e slt0 com os valores de 'totconduns'
    #        Finalmente, calcularmos gtransu e gtransl para a vegetacao sem stresse hidrico (assim, podemos calcular o estresse) 
    #          e EOP = gtransu ou gtransu        
    
    #         EOP = EO * (1.0-EXP(-LAI*KEP)) * TRATIO
    #        EOP = max(EOP,0.0)
    
    SWFAC  = 1.0
    TURFAC = 1.0
    if(stresstl<=0.9) TURFAC = (1./RWUEP1) * stresstl 
    if(stresstl<=0.9) SWFAC  = stresstl 
    
    #        if (EOP > 0.001) {
    #        EP1 = EOP * 0.1           # EOP mm and EP1 cm
    #        if ((TRWUP/EP1) < RWUEP1) {TURFAC = (1./RWUEP1) * TRWUP / EP1  }
    #        if (EP1 >= TRWUP) {  SWFAC = TRWUP / EP1  }
    #        }
    #To do: Jair, como haviamos falado essa variavel tem que ser em funcao da planta
    #         adicionalmente, como saber se usamos aqui stresstl ou stresstu?
    
    
    #-----------------------------------------------------------------------
    #     Compute dev rates for all other phases, using hourly air temp
    #-----------------------------------------------------------------------
 
        
    for (J in 2:NPHS) {
      K = TSELC[J]
      FT[J] = 0.0
      
      for (I in 1:TS) {
        
        # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
         TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
        
        
        FTHR = CURV(CTMP[J],TB[K],TO1[K],TO2[K],TM[K],TGRO[I]) #todo: escrever função CURV ('curvilinar' provavelmente)
        FT[J] = FT[J] + FTHR/TS
      }
      

      if (DAS < NR1) {
        FUDAY[J] = CURV(DLTYP[J],1.0,CSDVAR,CLDVAR,THVAR,DAYL)
      } else {
        FUDAY[J] = CURV(DLTYP[J],1.0,CSDVRR,CLDVRR,THVAR,DAYL)
      }
      
      FSW[J]   = 1. + (1. - SWFAC)  * WSENP[J]
      FNSTR[J] = 1. + (1. - NSTRES) * NSENP[J]
      FPSTR[J] = 1. + (1. - PStres2) * PSENP[J]
    }
    
    #-----------------------------------------------------------------------
    #     Transplants
    #-----------------------------------------------------------------------
    if (PLME == "T" & YRPLT == YRDOY) {
      K = TSELC[2]
      FT[2] = CURV(CTMP[2],TB[K],TO1[K],TO2[K],TM[K],ATEMP)  #todo: escrever função CURV ('curvilinar' provavelmente)
      PHZACC[2] = FT[2] * SDAGE
    }
    
    #-----------------------------------------------------------------------
    #     The effect of tmin on rate of development from emergence to
    #     flowering. Piper et al., (submitted to Field Crops Research, 1995)
    #-----------------------------------------------------------------------
    ZMODTE = 1.0
    
    if (TMING < OPTBI) {
      ZMODTE = 1. - (SLOBI * (OPTBI - TMING))
      ZMODTE = max(0.0, ZMODTE)
      ZMODTE = min(1.0, ZMODTE)
    }
    
    FT[4] = FT[4] * ZMODTE
    FT[5] = FT[5] * ZMODTE
    
    #-----------------------------------------------------------------------
    #     Compute rates of development to be used in other parts of model
    #     based on veg, early rep, and late rep temp sensitivities, respectively.
    #     Physiological days during today for vegetative development (DTX),
    #     physiological days during the day for reproductive development
    #     (TNTFAC & TNTFC2), and photothermal days during the day (TDUMX & TDUMX2)
    #-----------------------------------------------------------------------
    DTX    = FT[2]
    TNTFAC = FT[6]
    TNTFC2 = FT[10]
    #-----------------------------------------------------------------------
    #     DRPP affects seed & shell numbers set, seed and shell growth rates,
    #     ACCAGE, PODADD, FLWADD, FLWADD.  Okay to use the "SEEDFILL"
    #     photoperiod.  This change will make TDUMX2 sensitive to the R5-R7
    #     period and affect N mobilization.
    #-----------------------------------------------------------------------
    DRPP   = FUDAY[6]
    TDUMX  = TNTFAC * DRPP
    TDUMX2 = TNTFC2 * FUDAY[10]
    
    #-----------------------------------------------------------------------
    #    Calculate rate of V-stage change for height and width determination
    #-----------------------------------------------------------------------
     VSTAGES( DAS, DTX, EVMODC, MNEMV1, NDVST,                # Input
                            NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
                            TURFAC, XPOD, YRDOY, YRPLT,                     # Input
                            DYNAMIC)                                         # Control
    
    
    
    #**********************************************************************
    #**********************************************************************
    #     Daily Integration
    #**********************************************************************
  } else if (DYNAMIC == 'INTEGR') { 
    
    #----------------------------------------------------------------------
    #     Check to see if stages occur today, if so set them in RSTAGES
    #----------------------------------------------------------------------
   RSTAGES(DAS,DYNAMIC,
                            FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    # Input
                            PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM)      # Input
    
    
    #-----------------------------------------------------------------------
    #     Special accumulators used in other parts of the model
    #-----------------------------------------------------------------------
    #     Canopy age, flowering to harvest maturity, AGELF
    #-----------------------------------------------------------------------
    #     FRACDN is relative time from flowering to last leaf, modify leaf part
    #-----------------------------------------------------------------------
    if (DAS >= NR1) {
      FRACDN = PHZACC[13]/MNFLLL
      FRACDN = min(1.0,FRACDN)
    }
    
    #-----------------------------------------------------------------------
    #     DXR57-rel time from R5 to R7, modifies N mobilization
    #-----------------------------------------------------------------------
    if (DAS > NR5) {
      DXR57 = PHZACC[10]/PHTHRS[10]
      DXR57 = min(DXR57,1.0)
    } else {
      DXR57 = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Calculate V-stages
    #-----------------------------------------------------------------------
    VSTAGES( DAS, DTX, EVMODC, MNEMV1, NDVST,                # Input
                            NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
                            TURFAC, XPOD, YRDOY, YRPLT,                     # Input
                            DYNAMIC)                                         # Control
    
    #***********************************************************************
    #     End of DYNAMIC IF construct
    #***********************************************************************
    
  }
  

    assign("FPSTR",FPSTR , envir = env)  
    assign("FSW",FSW   , envir = env)  
    assign("FT",FT    , envir = env)  
    assign("FUDAY",FUDAY , envir = env)  
    assign("PHTHRS",PHTHRS, envir = env)  
    assign("TDUMX",TDUMX , envir = env)  
    assign("TDUMX2",TDUMX2, envir = env)  
    assign("DRPP",DRPP  , envir = env)  
    assign("DTX",DTX   , envir = env)  
    assign("DXR57",DXR57 , envir = env)  
    assign("FRACDN",FRACDN, envir = env)  
    assign("TNTFAC",TNTFAC, envir = env)  
    assign("TNTFC2",TNTFC2, envir = env)  
    assign("SWFAC",SWFAC , envir = env)  
    assign("TURFAC",TURFAC, envir = env)  
    assign("FNSTR",FNSTR , envir = env)  

    
    
    
  #-----------------------------------------------------------------------
  #     End Subroutine PHENOL
  #-----------------------------------------------------------------------
  
} # END PHENOL



#### Subrotina: VSTAGES ####
#  Vegetative Stage (VARIABLE NAME):
#    VE    (NVEG0) -  First day with 50 Percent of plants with some part visible at soil surface
#    NVEG1         - First day with 50 Percent of plants with completely unrolled leaf at unifoliate node
#    V1            - First day with 50 Percent of plants with completely unrolled leaf at first node above the unifoliate node
#    V2            - First day with 50 Percent of plants with 2 leaves above the unifoliate on the main stem
#    V(n)          - First day with 50 Percent of plants with n leaves above the unifoliate on the main stem
#         (NDVST)  - Day on which last main stem node formed
#         (NDLEAF) - Day when leaf expansion ceased (often on branches)  

#=======================================================================
#  VSTAGES, Subroutine
#  Calculates V-stages
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  07/24/98 CHP Pulled code from PHENOL subroutine
#-----------------------------------------------------------------------
#     Called from:    PHENOL
#     Calls:          None
#=======================================================================

VSTAGES <- function(DAS, DTX, EVMODC, MNEMV1, NDVST,         
                     NVEG0, NVEG1, PHZACC, PLME, TRIFOL,      
                     TURFAC, XPOD, YRDOY, YRPLT,DYNAMIC) {                        
  
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    
    #-----------------------------------------------------------------------

    VSTAGE = 0.0
    RVSTGE = 0.0
    VSTGED = 0.0
    VSTAGP = 0.0

    #***********************************************************************
    #***********************************************************************
    #     Daily Rate Calculations
    #***********************************************************************
  } else if (DYNAMIC == 'RATE') { 
    #-----------------------------------------------------------------------
    #    Calculate rate of V-stage change for height and width determination
    #-----------------------------------------------------------------------
    RVSTGE = 0.
    
    if (DAS > NVEG0 & DAS <= (NDVST + round (VSTGED))) {
      if (DAS > NDVST & VSTGED > 0.001) { RVSTGE = 1. / VSTGED
      } else { RVSTGE = VSTAGE - VSTAGP }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Daily Integration
    #***********************************************************************
    #-----------------------------------------------------------------------
  } else if (DYNAMIC == 'INTEGR') {
    #     V-STAGE between emergence and unifoliate (V-1) is determined
    #     by the physiological accumulator and the minimum number of
    #     days between emergence and V-1 under optimum temperature (MNEMV1)
    #     V-STAGE after V-1 is determined by the leaf appearance rate
    #     (TRIFOL), a water stress factor (TURFAC) and physiological units
    #     for today (DTX).
    #-----------------------------------------------------------------------
    if (RVSTGE > 1.E-6) { VSTGED = 1. / RVSTGE } else { VSTGED = 0.0 }
    
    VSTAGP = VSTAGE
    
    #-----------------------------------------------------------------------
    #     V-Stage for transplants
    #-----------------------------------------------------------------------
    if (PLME == "T" & YRPLT == YRDOY) { VSTAGE = 1. + (PHZACC[2] - MNEMV1) * TRIFOL }
    
    #-----------------------------------------------------------------------
    if (DAS >= NVEG0 & DAS <= NDVST) {
      if (DAS < NVEG1) {
        VSTAGE  = PHZACC[2]/MNEMV1
      } else {
        if (VSTAGE < abs(EVMODC) & abs(EVMODC) > 0.0001) {
          
          EVMOD = 1.0 + (abs(EVMODC)- VSTAGE) / EVMODC
          EVMOD = min(2.0,EVMOD)
          EVMOD = max(0.0,EVMOD)
        } else {
          EVMOD = 1.0
        }
        VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD*TURFAC*(1.0-XPOD)
      }
    }
    
    #***********************************************************************
    #     End of DYNAMIC IF construct
    #***********************************************************************    
  }

  

  
  assign("RVSTGE", RVSTGE, envir = env)
  assign("VSTAGE", VSTAGE, envir = env)
  assign("VSTGED", VSTGED, envir = env)
  assign("VSTAGP", VSTAGP, envir = env)
  
    
  } # END SUBROUTINE VSTAGES
  
  
  
  #### Subrotina: RSTAGES ####
  
  #  Reproductive STAGE (VARIABLE NAME):
  #  R00 (JPEND) - Day when juvenile phase ends and plants first become sensitive to photoperiod
  #  R0  (NR0)   - Day when floral induction occurs
  #  R1  (NR1)   - Day when 50 Percent of plants have at least one flower at any node on the  plant
  #  R2  (NR2)   - Day when 50 Percent of plants have one peg at any node (for peanut only)
  #  R3+ (NR3)   - Day when 50 Percent of plants have at least one pod formed (at least 0.5 cm in length) and ready to grow
  #  R4+         - Day when 50 Percent of plants have at least one fully expanded pod
  #  R5+ (NR5)   - Day when 50 Percent of plants have pods with seeds beginning to grow
  #  R6+         - Day when 50 Percent of plants have at least one pod containing a full-sized green seed NDSET Day when last pod can form
  #  R7  (NR7)   - Day when 50 Percent of plants first have at least one pod that is yellowing, physiological maturity
  #  R8  (NR8)   - Day when 50 Percent of plants have at least 95 Percent of pods brown, harvest  maturity
  #  +Intentionally deviates from Fehr and Caviness (1971) to designate first occurrences of reproductive trait rather
  #  than looking at top 4 nodes with fully expanded leaves on the plant.
  
  #************************************************************************
  #************************************************************************
  
  RSTAGES <- function (DAS,DYNAMIC,
                       FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,  
                       PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM)   {
    
    
    NVALP0 = 10000
    
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    if (DYNAMIC == 'SEASINIT') { 
      #-----------------------------------------------------------------------
      
      
      
      RSTAGE = 0
      PHZACC <- rep(0,20)
      NVALPH <- rep(NVALP0,20)
      STGDOY <- rep(9999999,20)
      PROG   <- rep(0,20)
      
      
      NVALPH[1]  = 1
      STGDOY[14] = YRSIM
      STGDOY[15] = YRPLT
      
      NVEG0  = NVALP0
      NVEG1  = NVALP0
      JPEND  = NVALP0
      NR0    = NVALP0
      NR1    = NVALP0
      NR2    = NVALP0
      NR3    = NVALP0
      NR5    = NVALP0
      NDLEAF = NVALP0
      NDVST  = NVALP0
      NDSET  = NVALP0
      NR7    = NVALP0
      YRNR1  = -99
      YRNR2  = -99
      YRNR3  = -99
      YRNR5  = -99
      YRNR7  = -99
      MDATE  = -99
      YREMRG = -99
      # For P module:
      SeedFrac = 0.0
      VegFrac  = 0.0
      VegTime = PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHTHRS[8]
      
      #***********************************************************************
      #***********************************************************************
      #     Daily Integration
      #***********************************************************************
    } else if (DYNAMIC == 'INTEGR') {
      #-----------------------------------------------------------------------
      REM <- rep(1.0,20)
      
      
      if (YRDOY == YRPLT) { STGDOY[15] = YRPLT  }
      
      #-----------------------------------------------------------------------
      #     Transplants
      #-----------------------------------------------------------------------
      if (PLME == "T" & YRPLT == YRDOY) { 
        NVEG0 = DAS
        NVALPH[2] = NVEG0
        YREMRG    = YRDOY
        if ((PHZACC[2] - PHTHRS[2]) > -1.E-6) {
          NVEG1 = DAS
          NVALPH[3] = NVEG1
          PHZACC[3] = PHZACC[2] - PHTHRS[2]
          if ((PHZACC[3] - PHTHRS[3]) > -1.E-6) {
            JPEND = DAS
            NVALPH[4] = JPEND
            PHZACC[4] = PHZACC(3) - PHTHRS(3)
            if ((PHZACC[4] - PHTHRS[4]) > -1.E-6) {
              NR0 = DAS
              NVALPH[5] = NR0
              RSTAGE    = 0
              PHZACC[5] = PHZACC[4] - PHTHRS[4]
              if ((PHZACC[5] - PHTHRS[5]) > -1.E-6) {
                NR1 = DAS
                NVALPH[6] = NR1
                YRNR1     = YRDOY
                RSTAGE    = 1
                PHZACC[6] = PHZACC[5] - PHTHRS[5]
              }
            }
          }
        }
      }
      
      #-----------------------------------------------------------------------
      #     Check for emergence, if NVEG0 has been set to less than its
      #         initial value
      #-----------------------------------------------------------------------
      if (NVEG0 >= NVALP0) {
        
        PHTEM = PHTHRS[1] + SDEPTH * 0.6
        PROG[1] = FT[1] * FUDAY[1] * min(FSW[1],FNSTR[1],FPSTR[1])
        PHZACC[1] = PHZACC[1] + PROG[1]
        
        if ((PHZACC[1] - PHTEM) > -1.E-6 | (ISIMI == "E")) {
          
          #-----------------------------------------------------------------------
          #       Emergence, next stage, occurs on day DAS
          #-----------------------------------------------------------------------
          NVEG0 = DAS
          NVALPH[2] = NVEG0
          YREMRG    = YRDOY
          STGDOY[1] = YRDOY
          #-----------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          if (ISIMI != "E") { REM[2] = (PHZACC[1] - PHTEM)/(PROG[1] + 0.00001)}
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for veg stage, V1
      #-------------------------------------------------------------------------------
      #     Skip accumulator section if not time to start accumulating for phase 2
      #          also skips this accumulator on day when stage 2 occurred, with
      #          initial value of PHZACC(2) already computed
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[2]] & NVEG1 >= NVALP0) {
        #-------------------------------------------------------------------------------
        #     Skip section if stage 3 has already occurred
        #-------------------------------------------------------------------------------
        PROG[2] = FT[2] * FUDAY[2] * min(FSW[2],FNSTR[2],FPSTR[2]) * REM[NPRIOR[2]]
        PHZACC[2] = PHZACC[2] + PROG[2]
        
        if (PHZACC[2] - PHTHRS[2] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       V1 occurs on day DAS
          #-------------------------------------------------------------------------------
          NVEG1 = DAS
          NVALPH[3] = NVEG1
          STGDOY[2] = YRDOY
          REM[3] = (PHZACC[2] - PHTHRS[2]) / (PROG[2] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for end of juvenile phase
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[3]] & JPEND >= NVALP0) {
        
        PROG[3] = FT[3] * FUDAY[3] * min(FSW[3],FNSTR[3],FPSTR[3]) * REM[NPRIOR[3]]
        PHZACC[3] = PHZACC[3] + PROG[3]
        
        if (VegTime > 0) {VegFrac = PHZACC[3] / VegTime } else { VegFrac = 0.0}
        
        if( (PHZACC[3] - PHTHRS[3]) > -1.E-6) {
          
          #-------------------------------------------------------------------------------
          #       End of juvenile phase occurs on day DAS
          #-------------------------------------------------------------------------------
          JPEND = DAS
          NVALPH[4] = JPEND
          STGDOY[3] = YRDOY
          REM[4] = (PHZACC[3] - PHTHRS[3])/(PROG[3] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for floral induction, end of induction phase
      #-------------------------------------------------------------------------------
      if ( DAS >= NVALPH[NPRIOR[4]] & NR0 >= NVALP0) {
        
        PROG[4] = FT[4] * FUDAY[4] * min(FSW[4],FNSTR[4],FPSTR[4]) * REM[NPRIOR[4]]
        PHZACC[4] = PHZACC[4] + PROG[4]
        VegFrac = (PHTHRS[3] + PHZACC[4]) / VegTime
        
        if((PHZACC[4] - PHTHRS[4]) > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Floral induction occurs on day DAS, end of phase 4
          #-------------------------------------------------------------------------------
          NR0 = DAS
          NVALPH[5] = NR0
          RSTAGE    = 0
          STGDOY[4] = YRDOY
          REM[5] = (PHZACC[4] - PHTHRS[4])/(PROG[4] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for first flower, stage 6, end of phase 5
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[5]] & NR1 >= NVALP0) {
        
        PROG[5] = FT[5] * FUDAY[5] * min(FSW[5],FNSTR[5],FPSTR[5]) * REM[NPRIOR[5]]
        PHZACC[5] = PHZACC[5] + PROG[5]
        VegFrac = (PHTHRS[3] + PHTHRS[4] + PHZACC[5]) / VegTime
        
        if(PHZACC[5] - PHTHRS[5] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       First flower occurs on day DAS
          #-------------------------------------------------------------------------------
          NR1 = DAS
          STGDOY[5] = YRDOY
          NVALPH[6] = NR1
          YRNR1     = YRDOY
          RSTAGE    = 1
          REM[6] = (PHZACC[5] - PHTHRS[5])/(PROG[5] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for beginning ovule (peg), stage 7, end of phase 6
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[6]] & NR2 >= NVALP0) {
        
        PROG[6] = FT[6] * FUDAY[6] * min(FSW[6],FNSTR[6],FPSTR[6]) * REM[NPRIOR[6]]
        PHZACC[6] = PHZACC[6] + PROG[6]
        
        if(PHZACC[6] - PHTHRS[6] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       First peg occurs on day DAS
          #-------------------------------------------------------------------------------
          NR2 = DAS
          STGDOY[6] = YRDOY
          NVALPH[7] = NR2
          YRNR2     = YRDOY
          RSTAGE    = 2
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[7] = (PHZACC[6] - PHTHRS[6])/(PROG[6] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage beginning shell, stage 8, end of phase 7
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[7]] & NR3 >= NVALP0) {
        
        PROG[7] = FT[7] * FUDAY[7] * min(FSW[7],FNSTR[7],FPSTR[7]) * REM[NPRIOR[7]]
        PHZACC[7] = PHZACC[7] + PROG[7]
        
        if(PHZACC[7] - PHTHRS[7] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage R3 occurs on day DAS
          #-------------------------------------------------------------------------------
          NR3 = DAS
          STGDOY[7] = YRDOY
          if (STGDOY[7] == STGDOY[6]) STGDOY[7] = 9999999
          NVALPH[8] = NR3
          YRNR3     = YRDOY
          RSTAGE    = 3
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[8] = (PHZACC[7] - PHTHRS[7])/(PROG[7] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage beginning seed (R5), stage 9, end of phase 8
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[8]] & NR5 >= NVALP0) {
        
        PROG[8] = FT[8] * FUDAY[8] * min(FSW[8],FNSTR[8],FPSTR[8]) * REM[NPRIOR[8]]
        PHZACC[8] = PHZACC[8] + PROG[8]
        VegFrac =(PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHZACC[8])/VegTime
        
        if(PHZACC[8] - PHTHRS[8] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage R5 occurs on day DAS
          #-------------------------------------------------------------------------------
          NR5 = DAS
          STGDOY[8] = YRDOY
          NVALPH[9] = NR5
          YRNR5     = YRDOY
          RSTAGE    = 5
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[9] = (PHZACC[8] - PHTHRS[8])/(PROG[8] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage NDSET, stage 10, end of phase 9
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[9]] & NDSET >= NVALP0) {
        
        PROG[9] = FT[9] * FUDAY[9] * max(FSW[9],FNSTR[9],FPSTR[9]) * REM[NPRIOR[9]] #max ao inves de MIN
        PHZACC[9] = PHZACC[9] + PROG[9]
        
        if(PHZACC[9] - PHTHRS[9] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NDSET occurs on day DAS
          #-------------------------------------------------------------------------------
          NDSET = DAS
          STGDOY[9] = YRDOY
          NVALPH[10] = NDSET
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[10] = (PHZACC[9] - PHTHRS[9])/(PROG[9] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage NR7, stage 11, end of phase 10
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[10]] & NR7 >= NVALP0) {
        
        PROG[10] = FT[10] * FUDAY[10]*max(FSW[10],FNSTR[10],FPSTR[10]) * REM[NPRIOR[10]] #max ao inves de MIN
        PHZACC[10] = PHZACC[10] + PROG[10]
        SeedFrac = PHZACC[10] / PHTHRS[10]
        
        if(PHZACC[10] - PHTHRS[10] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NR7, physiological maturity, occurs on day DAS
          #-------------------------------------------------------------------------------
          NR7 = DAS
          STGDOY[10] = YRDOY
          NVALPH[11] = NR7
          YRNR7      = YRDOY
          RSTAGE     = 7
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[11] = (PHZACC[10] - PHTHRS[10])/(PROG[10] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NR8, stage 12, end of phase 11
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[11]] & MDATE <= YRSIM) {
        
        PROG[11] = FT[11] * FUDAY[11]*min(FSW[11],FNSTR[11],FPSTR[11]) * REM[NPRIOR[11]]
        PHZACC[11] = PHZACC[11] + PROG[11]
        
        if(PHZACC[11] - PHTHRS[11] > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NR8, harvest maturity, occurs on day DAS
          #-------------------------------------------------------------------------------
          STGDOY[11] = YRDOY
          NVALPH[12] = DAS
          MDATE      = YRDOY
          RSTAGE     = 8
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[12] = (PHZACC[11] - PHTHRS[11])/(PROG[11] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[12]] & NDVST >= NVALP0) {
        
        PROG[12] = FT[12] * FUDAY[12]*min(FSW[12],FNSTR[12],FPSTR[12]) * REM[NPRIOR[12]]
        PHZACC[12] = PHZACC[12] + PROG[12]
        
        if((PHZACC[12] - PHTHRS[12]) > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NDVST, end of V-stage addition, occurs on day DAS
          #-------------------------------------------------------------------------------
          NDVST = DAS
          STGDOY[12] = YRDOY
          NVALPH[13] = NDVST
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[13] = (PHZACC[12] - PHTHRS[12])/(PROG[12] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
      #-------------------------------------------------------------------------------
      if (DAS >= NVALPH[NPRIOR[13]] & NDLEAF >= NVALP0) {
        
        PROG[13] = FT[13] * FUDAY[13]*min(FSW[13],FNSTR[13],FPSTR[13]) * REM[NPRIOR[13]]
        PHZACC[13] = PHZACC[13] + PROG[13]
        
        if(PHZACC[13] - PHTHRS[13] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #      Stage NDLEAF, end of leaf growth, occurs on day DAS
          #-------------------------------------------------------------------------------
          NDLEAF = DAS
          STGDOY[13] = YRDOY
          NVALPH[14] = NDLEAF
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[14] = (PHZACC[13] - PHTHRS[13])/(PROG[13] + 0.00001)
        }
      }
      
      #************************************************************************
      #************************************************************************
      #     End of DYNAMIC IF construct
      #************************************************************************
    }
    #************************************************************************
  

    assign("JPEND",JPEND   , envir = env)    
    assign("MDATE",MDATE   , envir = env)    
    assign("NDLEAF",NDLEAF  , envir = env)     
    assign("NDSET",NDSET   , envir = env)    
    assign("NDVST",NDVST   , envir = env)    
    assign("NVALPH",NVALPH  , envir = env)     
    assign("NVEG0",NVEG0   , envir = env)    
    assign("NVEG1",NVEG1   , envir = env)    
    assign("NR0",NR0     , envir = env)     
    assign("NR1",NR1     , envir = env)     
    assign("NR2",NR2     , envir = env)     
    assign("NR3",NR3     , envir = env)     
    assign("NR5",NR5     , envir = env)     
    assign("NR7",NR7     , envir = env)     
    assign("PHZACC",PHZACC  , envir = env)      
    assign("RSTAGE",RSTAGE  , envir = env)      
    assign("STGDOY",STGDOY  , envir = env)      
    assign("SeedFrac",SeedFrac, envir = env)     
    assign("VegFrac",VegFrac , envir = env)    
    assign("VegTime",VegTime , envir = env)    
    assign("YREMRG",YREMRG  , envir = env)      
    assign("YRNR1",YRNR1   , envir = env)     
    assign("YRNR2",YRNR2   , envir = env)    
    assign("YRNR3",YRNR3   , envir = env)     
    assign("YRNR5",YRNR5   , envir = env)     
    assign("YRNR7",YRNR7   , envir = env)     
    assign("PROG",PROG    , envir = env)        
  
    
    }
  