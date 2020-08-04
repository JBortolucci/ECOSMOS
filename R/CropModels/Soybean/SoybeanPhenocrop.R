# move 'source' to SoybeanModel.R
source("R/CropModels/Soybean/SoybeanPhenoFunctions.R")


SoybeanPhenocrop <- function(iyear, iyear0, imonth, iday, jday, index) {
  
   
  
  TESTE <- 'N'
  
  if (TESTE == 'Y'){ #### Subrotina: PHENOL ####  
  
# PHENOL Subroutine CROPGRO-DSSAT
  
#=======================================================================
#  PHENOL, Subroutine, J. W. Jones
#  Calculates phenological development.
#-----------------------------------------------------------------------
#     Called from:    Main program
#     Calls:          IPPHENOL
#                     RSTAGES
#                     VSTAGES
#                     CURV
#=======================================================================


    
    #    PHENOL <-          (CONTROL, ISWITCH,
    #                        DAYL, NSTRES, PStres2, SOILPROP, ST,            # Input
    #                        SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     # Input
    #                        DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        # Output
    #                        NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       # Output
    #                        RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        # Output
    #                        TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         # Output
    #                        YRNR2, YRNR3, YRNR5, YRNR7) {                   # Output
    #      
    #----------------------------------------------------------------------------
    #!----------------------------------------------------------------------------
    #R INTEGER NPHS
    #R PARAMETER (NPHS = 13) #todo
    #R 
    #R CHARACTER*1 ISIMI, ISWWAT, PLME  #todo
    #R CHARACTER*2 CROP
    #R CHARACTER*3 CTMP(20), DLTYP(20)
    CTMP <- rep(0,20); DLTYP <- rep(0,20) #todo
    CTMP[1] <- "C1" #todo
    CTMP[2] <- "C2" #todo
    
    #R INTEGER JPEND, NDVST, NVEG1, YREMRG
    #R INTEGER DAS, YRDOY, YRPLT, YRSIM
    #R INTEGER DYNAMIC
    #R INTEGER I, J, K, NLAYR
    #R INTEGER NDLEAF, NDSET, NR1, NR2, NR5, NR7, NVEG0, RSTAGE
    #R INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE
    #R INTEGER STGDOY(20)
    #R INTEGER NPRIOR(20), NVALPH(20), TSELC(20)
    
    STGDOY <- rep(0,20) #todo: ver se seguiremos assim
    NPRIOR <- rep(0,20); NVALPH <- rep(0,20); TSELC <- rep(0,20) #todo: ver se seguiremos assim
    
    #R REAL DAYL, NSTRES, SWFAC, TMIN, TURFAC, XPOD    #R !, TGROAV
    #R REAL DRPP, DTX, DXR57, FRACDN, RVSTGE, TDUMX, TDUMX2, VSTAGE
    #R REAL ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, EVMODC
    #R REAL MNEMV1, MNFLLL, MNFLHM, OPTBI
    #R REAL SDEPTH, SDAGE, SLOBI, THVAR, TRIFOL
    #R REAL DTRY, FTHR, SWFEM, TNTFAC, TNTFC2
    #R REAL TSDEP, XDEP, XDEPL, ZMODTE
    
    #R REAL TB(5), TO1(5), TO2(5), TM(5)
    #R REAL PHTHRS(20)
    #R REAL LL(NL), DUL(NL), SAT(NL), DLAYR(NL)
    #R REAL WSENP(20), NSENP(20), PSENP(20), PHZACC(20)
    #R REAL SW(NL), ST(NL)
    #R REAL FNSTR(20), FPSTR(20), FSW(20), FT(20), FUDAY(20)
    #R REAL TGRO(TS)
    
    TB <- rep(0,5); TO1 <- rep(0,5); TO2 <- rep(0,5); TM <- rep(0,5)
    PHTHRS <- rep(0,20)
    LLNL[NL]; DUL[NL]; SAT[NL]; DLAYR[NL] #todo
    WSENP <- rep(0,20); NSENP <- rep(0,20); PSENP <- rep(0,20); PHZACC <- rep(0,20)
    SW[NL]; ST[NL] #todo
    FNSTR <- rep(0,20); FPSTR <- rep(0,20); FSW <- rep(0,20); FT <- rep(0,20); FUDAY <- rep(0,20)
    TGRO[TS]
    
    #R REAL  CURV  #R !Function subroutine
    
    #R !     P Module
    #R REAL PStres2
    #R REAL SeedFrac, VegFrac
    
    #-----------------------------------------------------------------------
    TYPE (ControlType) CONTROL     #todo
    TYPE (SoilType) SOILPROP       #todo
    TYPE (SwitchType) ISWITCH      #todo
    
    #R !     Transfer values from constructed data types into local variables.
    DAS     = CONTROL % DAS        #todo
    DYNAMIC = CONTROL % DYNAMIC    #todo
    YRDOY   = CONTROL % YRDOY      #todo
    YRSIM   = CONTROL % YRSIM      #todo
    #todo
    DLAYR  = SOILPROP % DLAYR      #todo
    DUL    = SOILPROP % DUL        #todo
    LL     = SOILPROP % LL         #todo
    NLAYR  = SOILPROP % NLAYR      #todo
    SAT    = SOILPROP % SAT        #todo
    
    #R ISWWAT = ISWITCH % ISWWAT
    #R ISIMI  = ISWITCH % ISIMI
    
    
    
if (DAS == 1) {     #R if (DYNAMIC .EQ. RUNINIT) THEN
      
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
  SDAGE < -99.0  # ! SDAGE     Transplant age (days)
  ATEMP <-  -99.000 
  

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
 SLOBI      <- 0.035    # Slope of relationship reducing progress toward flowering if TMIN for the day is less than OPTBI

#______________________________________________________________        
# SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
 
 #!*LEAF GROWTH PARAMETERS
 EVMODC <-0.0           # Modifier of rate of vegetative node appearance for the first few nodes, primarily used for peanut 
 
 TB	<- c( 7,  6, -15, 0, 0)
 TO1	<- c(28, 26,  26, 0, 0)
 TO2	<- c(35, 30,  34, 0, 0)
 TM	<- c(45, 45,  45, 0, 0)
 
 # !FOLLOWING LINE: STAGE; REF STAGE; PHOTOPERIOD FUNCTION; TEMPERATURE FUNCT;
 # !POINTER TO VEGD(1) OR REPDA(2) OR REPDB(3) TEMP SENS; SENS TO WATER;N; AND P     
 NPRIOR <- c(1,2,2,4,5,6,6,6,9,9,11,6,6)                                 # The phase of growth at which phase I accumulator can start
 DLTYP  <- c('NON','NON','NON','INL','INL','INL','INL','INL','INL','INL','NON','INL','INL')  # Type of curve used for daylength function for phase I:  NON=no  photoperiod sensitivity, INL=inverse linear      
 CTMP   <- c('LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','LIN','NON','LIN','LIN')  # Type of curve used for temp. function for phase I: LIN=linear,  QDR=quadratic, SIN=sine function
 TSELC  <- c(1,1,1,2,2,2,2,2,3,3,1,2,2)                                   # Number of temperature curve to be used for phase I development rate: 1=veg, 2=early rep, 3=late rep 
 WSENP  <- c(-0.2,-0.2,-0.4,-0.4,-0.4,-0.4,-0.4,-0.4,0.7,0.7,0,-0.6,-0.9) # Sensitivity of phase I to water stress, varying from -1 (slows dev) to 1 (hastens dev) 
 NSENP  <- c(0,0,0,0,0,0,0,0,0.4,0.4,0,0,0)                               # Sensitivity of phase I to Nitrogen stress. Varies from -1 (slows dev) to +1 (hastens dev)
 PSENP  <- c(0,0,0,0,0,0,0,0,0.0,0.0,0,0,0)                               # PSENP     Sensitivity of phase I to phosphorus stress (not yet used) 
 
 
 
 PHTHRS[5] = MAX(0.,PH2T5 - PHTHRS[3] - PHTHRS[4])
 PHTHRS[7] = PHTHRS[6] + MAX(0.,(PHTHRS[8] - PHTHRS[6])* PM06)
 PHTHRS[9] = MAX(0.,PHTHRS[10] * PM09)
 
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
      
      #***********************************************************************
      #***********************************************************************
      #     Seasonal initialization - run once per season
      #***********************************************************************
    } else if (DYNAMIC == SEASINIT ) { #todo checar 'ifelse'
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
      
      for(J in 1:20){
        FNSTR[J] = 1.
        FPSTR[J] = 1.
        FSW[J]   = 1.
        FT[J]    = 0.
        FUDAY[J] = 0.
      }
      
      # Em teoria, essas funções virão do source no SoybeanModel.R
      RSTAGES (CONTROL,
               FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    # Input
               PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      # Input
               JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     # Output
               NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       # Output
               RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      # Output
               YRNR1, YRNR2, YRNR3, YRNR5, YRNR7)              # Output
      
      VSTAGES (DAS, DTX, EVMODC, MNEMV1, NDVST,                # Input
               NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
               TURFAC, XPOD, YRDOY, YRPLT,                     # Input
               RVSTGE, VSTAGE,                                 # Output
               SEASINIT)                                       # Control
      
      #***********************************************************************
      #***********************************************************************
      #     Daily Rate calculations
      #***********************************************************************
    } else if (DYNAMIC == RATE) { #todo: checar 'ifelse'
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
        XDEP = 0.0
        SWFEM = 0.0
        TSDEP = 0.0
        
        
        for (I in 1:NLAYR){
          XDEPL = XDEP
          XDEP = XDEP + DLAYR[I]
          DTRY = Min(DLAYR[I], 10. - XDEPL)
          
          if (ISWWAT == "Y"){
            if (SW[I] <= DUL[I]) {
              SWFEM = SWFEM + DTRY * (max(SW[I] - LL(I),0.0)) / (DUL[I] - LL[I])
            } else {
              SWFEM = SWFEM + DTRY * (max(SAT[I] - SW[I],0.0)) / (SAT[I] - DUL[I])
            }
          }
          
          TSDEP = TSDEP + DTRY * ST[I]
          if (XDEP >= 10.) { break
          }
        }
        
        TSDEP = TSDEP / 10.
        
        #-----------------------------------------------------------------------
        #      Compute temperature and soil water effects for phase 1, emergence
        #-----------------------------------------------------------------------
        
        FT[1] = CURV(CTMP[1],TB[K],TO1[K],TO2[K],TM[K],TSDEP) #todo: escrever função CURV () em algum outro script
        
        if (ISWWAT == "Y") {
          SWFEM = (SWFEM / 10.) * 100.0
          FSW[1] = CURV("LIN",0.0,20.0,100.,1000.,SWFEM)
        } else {
          FSW[1] = 1.
        }
        FSW[1] = 1. + (1.-FSW[1])*WSENP[1]
      }
      
      #-----------------------------------------------------------------------
      #     Compute dev rates for all other phases, using hourly air temp
      #-----------------------------------------------------------------------
      for (J in 2:NPHS) {
        K = TSELC[J]
        FT[J] = 0.0
        
        for (I in 1:TS) {
          FTHR = CURV(CTMP[J],TB[K],TO1[K],TO2[K],TM[K],TGRO[I]) #todo: escrever função CURV ('curvilinar' provavelmente)
          FT[J] = FT[J] + FTHR/REAL[TS]
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
      #     The effect of Tmin on rate of development from emergence to
      #     flowering. Piper et al., (submitted to Field Crops Research, 1995)
      #-----------------------------------------------------------------------
      ZMODTE = 1.0
      
      if (TMIN < OPTBI) {
        ZMODTE = 1. - (SLOBI * (OPTBI - TMIN))
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
      VSTAGES(
        DAS, DTX, EVMODC, MNEMV1, NDVST,                # Input
        NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
        TURFAC, XPOD, YRDOY, YRPLT,                     # Input
        RVSTGE, VSTAGE,                                 # Output
        RATE)                                           # Control
      
      
      
      #**********************************************************************
      #**********************************************************************
      #     Daily Integration
      #**********************************************************************
    } else if (DYNAMIC == INTEGR) { #todo check 'ifelse'
      
      #----------------------------------------------------------------------
      #     Check to see if stages occur today, if so set them in RSTAGES
      #----------------------------------------------------------------------
      RSTAGES(CONTROL,
              FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    # Input
              PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      # Input
              JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     # Output
              NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       # Output
              RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      # Output
              YRNR1, YRNR2, YRNR3, YRNR5, YRNR7)              # Output
      
      #-----------------------------------------------------------------------
      #     Special accumulators used in other parts of the model
      #-----------------------------------------------------------------------
      #     Canopy age, flowering to harvest maturity, AGELF
      #-----------------------------------------------------------------------
      #     FRACDN is relative time from flowering to last leaf, modify leaf part
      #-----------------------------------------------------------------------
      if (DAS >= NR1) {
        FRACDN = PHZACC(13)/MNFLLL
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
      VSTAGES(
        DAS, DTX, EVMODC, MNEMV1, NDVST,                # Input
        NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             # Input
        TURFAC, XPOD, YRDOY, YRPLT,                     # Input
        RVSTGE, VSTAGE,                                 # Output
        INTEGR)                                         # Control
      
      #***********************************************************************
      #     End of DYNAMIC IF construct
      #***********************************************************************
    }
    #-----------------------------------------------------------------------
    # RETURN
    #todo return, or assign?
    #R END   #R !SUBROUTINE PHENOL
    #-----------------------------------------------------------------------
    #     End Subroutine PHENOL
    #-----------------------------------------------------------------------
    # END  PHENOL from DSSAT CROPGRO
    
    
  } # para facilitar a programacao 
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Codigo antigo do eucalipto, remover depois que implementar tudo  
  
  # parametros
  Alleaf1    <- plantList$soybean$params$Alleaf1
  Alleaf2    <- plantList$soybean$params$Alleaf2
  Alleafinit <- plantList$soybean$params$Alleafinit
  Alleafmin  <- plantList$soybean$params$Alleafmin
  Alleafremain <- plantList$soybean$params$Alleafremain
  Allocsensb <- plantList$soybean$params$Allocsensb
  Allocsenscr <- plantList$soybean$params$Allocsenscr
  Allocsensf  <- plantList$soybean$params$Allocsensf 
  Bdecay <- plantList$soybean$params$Bdecay
  BdecayStart <- plantList$soybean$params$BdecayStart
  Bfall <- plantList$soybean$params$Bfall
  BfallStart <- plantList$soybean$params$BfallStart
  Branch1 <- plantList$soybean$params$Branch1
  Branch2 <- plantList$soybean$params$Branch2
  Callocb <- plantList$soybean$params$Callocb
  Calloccr  <- plantList$soybean$params$Calloccr
  Callocf   <- plantList$soybean$params$Callocf
  Cdecay    <- plantList$soybean$params$Cdecay
  Cfracts   <- plantList$soybean$params$Cfracts
  Coroot1   <- plantList$soybean$params$Coroot1
  Coroot2   <- plantList$soybean$params$Coroot2
  deltay    <- plantList$soybean$params$deltay 
  Density   <- plantList$soybean$params$Density
  Fdecay1   <- plantList$soybean$params$Fdecay1
  Fdecay2   <- plantList$soybean$params$Fdecay2
  Fdecay3   <- plantList$soybean$params$Fdecay3
  Fdecay4   <- plantList$soybean$params$Fdecay4
  Fineroot1 <- plantList$soybean$params$Fineroot1
  Fwpmax    <- plantList$soybean$params$Fwpmax
  Fwpmin    <- plantList$soybean$params$Fwpmin
  Leafsap1  <- plantList$soybean$params$Leafsap1
  Leafsap2  <- plantList$soybean$params$Leafsap2
  Leafsap3  <- plantList$soybean$params$Leafsap3
  LimLai    <- plantList$soybean$params$LimLai
  nrn       <- plantList$soybean$params$nrn
  nrx       <- plantList$soybean$params$nrx
  Rdecay1   <- plantList$soybean$params$Rdecay1
  Rdecay2   <- plantList$soybean$params$Rdecay2
  Sapheight <- plantList$soybean$params$Sapheight
  Siginit   <- plantList$soybean$params$Siginit
  Sigmin    <- plantList$soybean$params$Sigmin
  Wdecay    <- plantList$soybean$params$Wdecay
  
  i <- index
  
  if (croplive[i]==1) {
    
    
    huileaf <- array(0, npft)             # heat unit index needed to attain leaf emergence after planting
    huigrain <- array(0, npft)            # heat unit index needed to reach vegetative maturity
    laidecl <- matrix(0, 1, npft)  # decline in leaf area for crop
    # phenology for additional leaf drop - if drought related or temperature related at
    # end of growing season
    
    ddays      <- 7.0            #inp
    ddfac      <- 1.0 / ddays    #inp
    tthreshold <- 273.16         #par
    
    # number of corn plants per square meter
    # this is only important if using the leaf expansion equations
    # of Ritchie, that is temperature dependent.  Our standard procedure
    # here however is to use the allocation of C to leaf (aleaf) and
    # specific leaf area (specla) to accumulate LAI during the season
    
    nplants <- 7                #inp
    
    aplantn <- 0.0
    
    for(k in 1:nsoilay) {
      aplantn <- aplantn + smsoil[k] + smsoln[k]
    }
    
    
    
    # Eucalyptus phenology, Carbon Aloccation and Harvest
    
    if (croplive[i] == 1.0) {
      
      
      huileaf[i]  <- lfemerg[i]  * gddmaturity[i]  # typically between 3 - 7% in wheat
      
      crmeuca       <- max(73., min((gddmaturity[i]+ 53.683) / 13.882,135.))
      huigrain[i] <- -0.002  * (crmeuca - 73.) + grnfill[i]
      huigrain[i] <- min(max(huigrain[i],grnfill[i] - 0.1), grnfill[i])
      huigrain[i] <- huigrain[i]   * gddmaturity[i]  # from Cabelguenne et al. 1999
      
      
      # accumulate growing degree days for planted crops past planting
      gddplant[i] <- gddplant[i] + max(0, min(td - baset[i], mxtmp[i]))
      gddtsoi[i] <- gddtsoi[i] + max(0, min(tsoi[1] - baset[i], mxtmp[i]))
      
      
      greenfrac[i] <- 1.0
      
      # calculate fraction allocated to leaf (from i. Norman allocation curve)
      # bfact and fleafi are set in params.crp
      fleaf[i] <- fleafi[i] * (exp(-bfact[i]) - exp(-bfact[i] * gddplant[i] / huigrain[i])) / (exp(-bfact[i]) - 1)
      
      # calculate accumulated growing degree days since planting (gddplant)
      # determine if growing degree days calculated from top layer soil temperature
      # are enough for leaf emergence to occur
      hui[i] <- gddplant[i]
      
      leafout[i]   <- gddplant[i]
      
      laidecl[i] <- 0.0
      
      idpp[i] <- idpp[i] + 1
      
      if (leafout[i] >= huileaf[i])   idpe[i] <- idpe[i] + 1
      
      # crop phenology from leaf emergence to start of leaf decline
      
      ######################################################################
      ########## Start Allocation to Perenial (Eucalyptus) crops ############
      ######################################################################
      
      # Phase 1 completed:
      
      if(idpp[i]==1){
        cbiow[i] <- 0.0001455428/kg_C_M2_to_T_ha
        cbiob[i] <- 0.0002444583/kg_C_M2_to_T_ha
        cbior[i] <- 0.0001482158/kg_C_M2_to_T_ha
        cbiol[i] <- 0.005065926 /kg_C_M2_to_T_ha
        cbiocr[i] <- 0.000163217 /kg_C_M2_to_T_ha
        plai[i]  <- cbiol[i]*specla[i]  }
      
      
      rm <- min(mxmat[i]/365, idpp[i]/365)
      
      
      #   if( (idpp[i]+1)>= gday$idpp[length(gday$idpp)]) { gday_c<- gday[length(gday$idpp),]}else{gday_c<- gday[which(gday$idpp ==(idpp[i]+1)),]   }
      
      
      
      hsum   <- 0
      water  <- 0
      Wcapac <- 0
      
      for(k in 1:(nsoilay)) {
        
        hsum <- hsum+hsoi[k]
        
        # new michel
        mh_aw <- 16.61*(1-exp(-0.00202 * idpp[i]))^(1.5883) # Maximum root depth Christina et al. (2017)
        mh_aw <- min(mh_aw, sum(hsoi))                  # mh_w can not be greater than the last soil layer
        
        #     print(paste(sum(hsoi),mh_aw,sep="/"))
        
        if(hsum <= mh_aw) {
          Wcapac <- Wcapac + 1000*(1.0     * hsoi[k] *  poros[k] ) #*froot[k,1] 
          water  <- water  + 1000*(wsoi[k] * hsoi[k] *  poros[k] ) #*froot[k,1] 
        }
      }
      
      
      capac <- 30 + 1.2*Wcapac *min(idpp[i]/600,1) #fazer funcao do tempo
      
      
      waterfact <-  ((water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin )
      # waterfact <-  gday_c$waterfact
      
      waterfact<-max(min(waterfact,1),0)
      
      greenfrac[i] <- 1
      
      #Fine root C allocation
      Finerootexp <- Fineroot1 * (plai[i]*greenfrac[i])
      
      
      aroot[i] = (0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf )
      # Finerootexp <- Fineroot1 * (gday_c$lai*greenfrac[i])
      # aroot[i] = (0.5 + 0.5 * (1.- (gday_c$cbior) / Finerootexp ) / Allocsensf )
      aroot[i]= aroot[i]*(nrx*nrn)/(nrn+(nrx-nrn)*waterfact)
      aroot[i]<-max(min(aroot[i],1),0)
      
      
      #---------------------
      #Leaf C allocation
      # ModelLai==2 from Param_eucaflux.h
      # Recalcular o LAI no passo anterior: biomassa de folha input, add o sla
      if (plai[i]<=0.1) {
        aleaf[i] = Alleafinit # for very small LAI at begining, constant alloc
      } else {
        # leaf allocation fraction is the second priority after fine roots, and has a height (age) constraint
        aleaf[i]= max(Alleafmin,Alleafmin+Alleaf1*exp(-Alleaf2*ztop[1]))
        aleaf[i]= max(aleaf[i], 1-aroot[i]-Alleafremain) #second priority after fine root, with 20% kept apart
      }
      
      aleaf[i]<-max(min(aleaf[i],1),0)
      #      aleaf[i]<-max(min(aleaf[i],0.16),0)
      
      
      #---------------------
      #Branch C allocation (Stem)
      Branchexp = Branch1 * ( plai[i] ^ Branch2 )
      if (Branchexp < 0.)  Branchexp = 0.001
      abranch[i] =  (0.5 + 0.5 * (1.- (cbiob[i]*kg_C_M2_to_T_ha) / Branchexp ) / Allocsensb )
      #  abranch[i] =  (0.5 + 0.5 * (1.- (gday_c$cbiob) / Branchexp ) / Allocsensb )
      
      abranch[i]<-max(min(abranch[i],1),0)
      
      
      #---------------------
      #Root C allocation
      Corootexp <- Coroot1 * ( (cbiow[i]*kg_C_M2_to_T_ha) ^ Coroot2 )
      #Corootexp <- Coroot1 * ( (gday_c$cbiow) ^ Coroot2 )
      
      if (Corootexp < 0.) Corootexp = 0.001
      acroot[i] = (0.5 + 0.5 * (1.- (cbiocr[i]*kg_C_M2_to_T_ha) / Corootexp ) / Allocsenscr )
      #  acroot[i] = (0.5 + 0.5 * (1.-gday_c$cbiocr / Corootexp ) / Allocsenscr )
      
      acroot[i]<-max(min(acroot[i],1),0)
      
      
      
      #---------------------
      #Stem (G'DAY) C allocation or Wood (ECOSMOS)
      # reduction factor was used to guaranteer the values (Alleaf + Alfineroot + Albran + Alcoroot) were all lower than 1
      Callocfr<- 1- (Callocf  + Callocb + Calloccr)
      aroot[i] = aroot[i]*Callocfr
      aleaf[i] = aleaf[i]*Callocf
      abranch[i] = abranch[i]*Callocb
      acroot[i] = acroot[i]*Calloccr
      
      #SVC ___ just test to make LAI increase
      #if(plai[i]<=4){
      #  aleaf[i]<-max(0.3,aleaf[i])
      #  print('increase aleaf to male leaf increase')
      #
      #}
      
      if ( (aroot[i] + aleaf[i] + abranch[i] + acroot[i]) > 1 ) {
        reductionfactor <- 1 / (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
        aroot[i] = aroot[i]*reductionfactor
        aleaf[i] = aleaf[i]*reductionfactor
        abranch[i] = abranch[i]*reductionfactor
        acroot[i] = acroot[i]*reductionfactor
      }
      
      awood[i] = 1 - (aroot[i] + aleaf[i] + abranch[i] + acroot[i])
      
      awood[i] <- max(0.0, awood[i])
      aroot[i] <- max(0.0, aroot[i])
      aleaf[i] <- max(0.0, aleaf[i])
      abranch[i] <- max(0.0, abranch[i])
      acroot[i] <- max(0.0, acroot[i])
      
      
      
      # END EUCALYPTUS
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
      
      
      
      
      #---------------------
      #Mortality - Litterfall C fluxes
      #---------------------
      
      #dead stem computation
      Deadwood   <- 0
      Deadcoroots <- 0
      if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        fdec <- exp(2.*log(2.)*(rm-BdecayStart))-1
        fdec <- max(min(fdec,1),0)
        Deadwood    =  fdec * Wdecay * Sapwood # Stock of dead branch that do not fall into the ground. It is assumed to start at 3 years old
        Deadcoroots  =  fdec * Cdecay * cbiocr[i]*kg_C_M2_to_T_ha  # cm
      }
      
      #---------------------
      #dead branches computation
      
      if (rm <= BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        #dead stem computation
        DeadGbranch <- 0
        Deadbranch  <- 0
        
      } else if (rm > BdecayStart ) {    # progressive start of branch, coarse root and bark decay after age 1./Bdecay
        fdec <- exp(2.*log(2.)*(rm-BdecayStart))-1
        fdec <- max(min(fdec,1),0)
        DeadGbranch  =  fdec * Bdecay * cbiob[i]*kg_C_M2_to_T_ha
        
      } else if ( rm > BfallStart ) {
        #beginning of the dead branches fall
        fdec = exp(2*log(2)*(rm-BfallStart))-1.
        fdec <- max(min(fdec,1),0)
        Deadbranch   =  fdec * Bfall * DBranch_attached*kg_C_M2_to_T_ha   # Dead branch that falls into the ground and entered the above-ground structural litter pool
        
      } else  if (rm > 4 ) {
        # After 4.5 years, both branch death and branch fall increase considerably ==> empirical correction
        fdec = exp(2*log(2)*(rm-4))-1.
        fdec <- max(min(fdec,1),0)
        # TODO: Reve valores de cada
        DeadGbranch = fdec * 4 * cbiob[i]*kg_C_M2_to_T_ha  # DeadGbranch increasis after 4 years old
        
        # implementar corretamente depois
        # DBranch<-cbiob[i]*kg_C_M2_to_T_ha/5
        
        Deadbranch  = fdec * 3 * DBranch_attached*kg_C_M2_to_T_ha
      }
      
      
      
      #---------------------
      #dead fine roots computation
      Finerootexp   <- Fineroot1 * plai[i]
      Rdecay        <- Rdecay1+ Rdecay2 * (cbior[i]*kg_C_M2_to_T_ha / Finerootexp )
      Deadfineroots <-  Rdecay * cbior[i]*kg_C_M2_to_T_ha
      
      #---------------------
      #dead leaves computation
      
      #there is a double litterfall cause: sapwood area target, and higher fall when higher production
      leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*ztop[1])
      
      if (leaftosapexp>Leafsap2) leaftosapexp <- Leafsap2
      #cm : Sapwoodarea is now directly inferred from mean height, according to experimental observations
      Sapwoodarea   <- Sapheight*ztop[1]
      Leaftosaparea <- plai[i]*10000./Sapwoodarea
      
      Fdecay <- Fdecay1 + Fdecay2*(Leaftosaparea/leaftosapexp)*
        (1+(aleaf[i] * max (0.0, adnpp[i])*kg_C_M2_to_T_ha)/Fdecay3)*
        (1-(max(0,min(water/capac,1)))/Fdecay4)
      
      Deadleaves <- Fdecay * cbiol[i]*kg_C_M2_to_T_ha  #rever dif entre Leaves_Predicted  e Shoot_Predicted
      
      
      #---------------------
      #C Pool update
      #---------------------
      
      
      #computation of SLA of new leaves (expanded)
      Sigmax <- Siginit
      Signew <- Sigmax - (Sigmax-Sigmin) * (ztop[1] - 1.)/(10. - 1.)
      Signew <- min(max(Signew,Sigmin), Sigmax)
      Signew <- Signew * (0.5+0.5*waterfact)
      
      #computation of new LAI from G'DAY  (not using, see bellow that LAI is re-calculated)
      # Deadleaves/Shoot = fraction of leaf mass which turns over; it is assumed that the same fraction of leaf area turns over.
      plai[i] <- plai[i] + (deltay * ((aleaf[i] * max (0.0, adnpp[i]))
                                      * Signew * M2_AS_HA / KG_AS_TONNES / Cfracts  - Deadleaves
                                      * plai[i] / (cbiol[i]+1E-15)))
      
      plai[i] <- max(plai[i],0.01)
      
      
      #  branch decay rate
      #Deadbranch <- DeadGbranch - Deadbranch
      #DBranch<- DBranch + DeadGbranch - Deadbranch
      
      
      
      #C pool update
      cbiow[i] <- cbiow[i] + (awood[i] * max (0.0, adnpp[i])) - (deltay * Deadwood     /kg_C_M2_to_T_ha)
      cbiob[i] <- cbiob[i] + (abranch[i] * max (0.0, adnpp[i])) - (deltay * DeadGbranch   /kg_C_M2_to_T_ha)
      
      
      #    tauleaf_branch <- min(365,365*((4*365)/idpp[i]))
      #    DBranch_decay <- DBranch_attached/tauleaf_branch
      
      if(idpp[i]<=2*365){
        DBranch_decay<-0
      } else if (idpp[i]>2*365 & idpp[i]<=4*365) {
        tauleaf_branch <- (1/280)*(1-exp(-0.0065*(idpp[i]-2*365)))  
        DBranch_decay <- DBranch_attached*tauleaf_branch
      } else {
        DBranch_decay <- DBranch_attached*(1/280)
      }
      
      #        if(idpp[i]<=2*365){
      #          DBranch_decay=0
      #        }else if (idpp[i]>2*365 & idpp[i]<=4*365) {
      #          tauleaf_branch <- (1/365)*(1-exp(-0.005*(idpp[i]-2*365)))
      #          DBranch_decay  <- DBranch_attached*tauleaf_branch
      #        }else{
      #        tauleaf_branch <- (1/365)*(2-exp(-0.005*(idpp[i]-4*365)))
      #        DBranch_decay  <- DBranch_attached*tauleaf_branch
      #        }
      #
      DBranch_attached <- DBranch_attached + (deltay * DeadGbranch   /kg_C_M2_to_T_ha) - DBranch_decay
      
      # DBranch_attached<-DBranch_attached + (deltay * DeadGbranch   /kg_C_M2_to_T_ha) - (deltay * Deadbranch/kg_C_M2_to_T_ha)
      # DBranch_decay <-(deltay * Deadbranch/kg_C_M2_to_T_ha)
      
      cbior[i] <- cbior[i] + (aroot[i] * max (0.0, adnpp[i])) - (deltay * Deadfineroots/kg_C_M2_to_T_ha)
      cbiol[i] <- cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (deltay * Deadleaves   /kg_C_M2_to_T_ha)  #foliar biomass turnover from G'DAY
      #   cbiol[i] = cbiol[i] + (aleaf[i] * max (0.0, adnpp[i])) - (cbiol[i] / tauleaf[i])  #foliar biomass turnover from ECOSMOS (other crops)
      cbiocr[i] <- cbiocr[i] + (acroot[i] * max (0.0, adnpp[i])) - (deltay * Deadcoroots  /kg_C_M2_to_T_ha)
      
      #       print(paste(idpp[i],Signew/ Cfracts,ztop[1],sep=" / "))
      # computation of LAI by the ECOSMOS's standards
      plai[i] <- max(cbiol[i]*Signew/ Cfracts,0.02) # G'DAYS SLA
      # plai[i] = max(cbiol[i]*specla[i],0.02)      # SLA from ECOSMOS (other crops)
      
      
      #  debug_str <- paste(iyear, idpp[i],plai[i],cbior[i]*kg_C_M2_to_T_ha,cbiob[i]*kg_C_M2_to_T_ha,cbiow[i]*kg_C_M2_to_T_ha,cbiocr[i]*kg_C_M2_to_T_ha,
      #                     Finerootexp,(0.5 + 0.5 * (1.- (cbior[i]*kg_C_M2_to_T_ha) / Finerootexp ) / Allocsensf ),
      #                     (nrx*nrn)/(nrn+(nrx-nrn)*waterfact),waterfact,aroot[i],
      #                     Alleafmin+Alleaf1*exp(-Alleaf2*ztop[1]),aleaf[i],
      #                     Branchexp, abranch[i],Corootexp,(0.5 + 0.5 * (1.- (cbiocr[i]*kg_C_M2_to_T_ha) / Corootexp ) / Allocsenscr ),
      #                     acroot[i],awood[i],deltay*Deadwood,deltay*Deadbranch,deltay*Deadfineroots,deltay*Deadleaves,deltay*Deadcoroots,sep=";")
      #  writeLines(debug_str, out_debug)
      
      
      
      #                     NOT IMPLEMENTED
      # #sapwood update
      Sapwoodarea <- Sapheight*ztop[1] # cm2/ha
      sap <- Sapwoodarea * ztop[1] * Cfracts * Density * 0.001 ; #new sapwood mass, tC/ha
      if (sap > cbiow[i]*kg_C_M2_to_T_ha) sap <- cbiow[i]*kg_C_M2_to_T_ha
      # cphw <- cbiow[i]*kg_C_M2_to_T_ha - sap - Heartwood  #increase in heartwood mass (new stem - new sapwood - old heartwood), needed for N alloc
      Sapwood   <- sap
      Heartwood <- cbiow[i]*kg_C_M2_to_T_ha - Sapwood
      
      
      
      biomass[i] <- cbiol[i] + cbiocr[i] + cbior[i] + cbiob[i] + cbiow[i]
      
      # keep track of aboveground annual npp
      ayanpp[i] <- (aleaf[i] + acroot[i] + abranch[i] + awood[i]) * adnpp[i] + ayanpp[i]
      
      
      #####################################################################
      # check for climatic and phenological limits on maturity, growth,
      # and harvest date
      #
      # check to see if minimum temperature has fallen below freeze
      # kill threshold for 3 consecutive days and if lai is above a minimum,
      # plant will
      # be damaged/killed.  This function is more for spring freeze events
      # or for early fall freeze events
      #
      # currently simulates too many grid cells that are killed by
      # freezing temperatures
      #
      # spring wheat is affected by this, winter wheat kill function
      # is determined in crops.f - is a more elaborate function of
      # cold hardening of the plant
      
      if (tmin <= tkill[i]) {
        ccdays[i] <- ccdays[i] + 1
      } else {
        ccdays[i] <- 0
      }
      
      if (ccdays[i] >= 1 &&
          hui[i] >= 0.6 * gddmaturity[i] &&
          croplive[i] == 1) {
        croplive[i]     <- 0.0
        print(paste0('tkill!!!!!',1,iyear,jday,idpp[i]))
        harvdate[i]     <- jday
      }
      
      
      
      #___________________________________________________
      #       Harvest
      
      if(cropy == 1) {
        if ( rm == mxmat[i]/365 ) { # maximum harvest date
          
          Deadfineroots <- cbior[i]
          Deadcoroots   <- cbiocr[i]
          
          croplive[i]   <- 0.0
          greenfrac[i]  <- 0.0 # turn all vegetation to brown
          harvdate[i] <- jday
          plai[i]         <- 0.01 # simulates remaining stubble/mulch
          endCycle <- T
          print(paste('Harvest Eucalyptus - = ',cropy,iyear,jday,idpp[i],rm))
        }
      } else {
        print('Eucalyptus has only one cycle - Stop')
        stop()
      }
      
    }
    
    sapfrac <- Sapwood / (Sapwood + Heartwood)
    
    ztopPft[i] <- (min(plai[i]/3, 1)) * ztopmxPft[i] * min(1,(rm /0.7))
    
  }
  
  assign("endCycle", endCycle, envir = env)
  
  assign("ztopPft", ztopPft, envir = env)
  assign("sapfrac", sapfrac, envir = env)
  
  assign("gddplant", gddplant, envir = env)
  assign("gddtsoi", gddtsoi, envir = env)
  assign("aplantn", aplantn, envir = env)
  assign("fleafi", fleafi, envir = env)
  assign("mxgddgf", mxgddgf, envir = env)
  assign("mxmat", mxmat, envir = env)
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
  assign("astemi", astemi, envir = env)
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
  
}
