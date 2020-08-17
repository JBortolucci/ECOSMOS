#  PODDET, Subroutine, W. D. Batchelor
#-----------------------------------------------------------------------
#  Computes pod detachment rates.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/1993 WDB Written.
#  04/24/1994 NBP Changed TAIRHR to TGRO.
#  02/02/1998 GH  Fixed dimensions of TB,TO,TO2,TM
#  07/18/1998 CHP Modified for modular format
#  05/11/1999 GH  Incorporated in CROPGRO
#  08/12/2003 CHP Added I/O error checking
#-----------------------------------------------------------------------
#  Called from:  PLANT
#  Calls:        ERROR, FIND, IGNORE
#=======================================================================

simDataVars$PODWTD  <-  0
simDataVars$SDNO    <-  0
simDataVars$SHELN   <-  0
simDataVars$SWIDOT  <-  0
simDataVars$WSHIDT  <-  0
simDataVars$WTSD    <-  0
simDataVars$WTSHE   <-  0
simDataVars$EMERG   <-  0
  

#TODO REMINDER -> não usado pelo modelo, por enquanto!
PODDET <- function(
  FILECC, TGRO, WTLF, YRDOY, YRNR2,                 #Input
  PODWTD, SDNO, SHELN, SWIDOT,                      #Output
  WSHIDT, WTSD, WTSHE,                              #Output
  DYNAMIC) {                                          #Control
  
  PODDET <- 0
  #-----------------------------------------------------------------------
  #USE ModuleDefs     #Definitions of constructed variable types, 
  # which contain control information, soil
  # parameters, hourly weather data.
  #IMPLICIT NONE
  #SAVE
  #CHARACTER*6 ERRKEY
  #PARAMETER (ERRKEY = 'PODDET')
  #CHARACTER*6 SECTION
  #CHARACTER*80 C80
  #CHARACTER*92 FILECC
  #INTEGER LUNCRP, ERR, LINC, LNUM, FOUND, ISECT, I
  
  #TODO checar conexão no ECOSMOS 
  #DYNAMIC
  #YRDOY
  YRNR2 <- 0 #calculado no RSTAGES.for
  # NPP
  
  SWIDOT <- 0
  WSHIDT <- 0
  WTLF   <- 0 # calculado no GROW.for
  XPD    <- 0
  PODWTD <- 0
  
  TPODM  <- 0
  RLMPM  <- 0
  SL10   <- 0
  FT     <- 0
  FTHR   <- 0
  #CURV   <-  curva de temp
  
  SDDAM  <- 0
  SHDAM  <- 0
  SUMSD  <- 0
  SUMSH  <- 0
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*POD LOSS PARAMETERS
  DWC    <- 6.0
  PR1DET <- 0.3961
  PR2DET <- -0.865
  XP1DET <- 1.00
  XP2DET <- 0.00
  #!*PHENOLOGY PARAMETERS   
  TB	<- c( 7,  6, -15, 0, 0)
  TO1	<- c(28, 26,  26, 0, 0)
  TO2	<- c(35, 30,  34, 0, 0)
  TM	<- c(45, 45,  45, 0, 0)
  
  TDLM <- rep(0,20)
  #TGRO[TS]
  TGRO   <- rep(1.,24)
  
  NCOHORTS <- 300 #from line 51 in ModuleDefs.for NCOHORTS = 300, !Maximum number of cohorts
  SDNO   <- rep(0, NCOHORTS)
  SHELN  <- rep(0, NCOHORTS)
  WTSD   <- rep(0, NCOHORTS)
  WTSHE  <- rep(0, NCOHORTS)
  WPODY  <- rep(0, NCOHORTS)
  PDET   <- rep(0, NCOHORTS)
  DAYS   <- rep(0, NCOHORTS)
  MSHELN <- rep(0, NCOHORTS)
  DTC    <- rep(0, NCOHORTS)
  
  
  #***********************************************************************
  #***********************************************************************
  #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
  #         or transplanting of plants
  #***********************************************************************
  if (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    # ALTERADO: Provavelmente esse número é como o fortran referencia até onde o loop vai repetir.
    for (I in 1:NCOHORTS) {
      DTC[I]   = 0.0
      MSHELN[I]= 0.0
      WPODY[I] = 0.0
      DAYS[I]  = 0.0
    }
    PODWTD = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #     Compute thermal time using hourly predicted air temperature
    #     based on observed max and min temperature.
    #--------------------------------------------------------------------
    FT = 0.0
    for (I in 1:TS) {
      FTHR = CURV('LIN',TB[3],TO1[3],TO2[3],TM[3],TGRO[I])
      FT = FT + FTHR/TS
    }
    #      24 changed to TS on 5 July 2017 by Bruce Kimball
    # -------------------------------------------------------------------
    #  Compute ratio of leaf area per pod cm2/pod
    #  and leaf mass per pod mass g/g
    # -------------------------------------------------------------------
    TPODM = 0.0
    RLMPM = 1.0
    
    # -------------------------------------------------------------------
    #      Compute 10 day running average of leaf mass and PGAVL
    # -------------------------------------------------------------------
    # TODO VERIFICAR: 10,2,-1 deve ser de 10 até 2 no passo -1 (10, 9, 8, 7...)
    for (I in seq(10, 2)) { #TODO: checar numero e sintate
      TDLM[I]= TDLM[I-1]
    }
    TDLM[1] = WTLF
    # -------------------------------------------------------------------
    #     Compute slope of leaf mass curve
    # -------------------------------------------------------------------
    SL10 = (TDLM[1] - TDLM[10])/ 10.0
    
    #---------------------------------------------------------------------
    if (YRNR2 >= 0) {
      #---------------------------------------------------------------------
      # ALTERADO: Sempre usar parenteses no for
      for (NPP in 1:(YRDOY - YRNR2))  { 
        TPODM = TPODM + WTSHE[NPP] + WTSD[NPP]
      }
      
      if (TPODM > 10.0) RLMPM = WTLF / TPODM
      #-------------------------------------------------------------------
      #     Main loop that cycles through detachment model
      #--------------------------------------------------------------------
      for (NPP in 1:(YRDOY - YRNR2)) { 
        #--------------------------------------------------------------------
        #     Determine maximum cohort shell mass and accumulate
        #     days without carbohydrate on a cohort basis
        #--------------------------------------------------------------------
        if (SHELN[NPP] > MSHELN[NPP]) {
          MSHELN[NPP] = SHELN[NPP]
        }
        if (WTSD[NPP] + WTSHE[NPP] >= 0.01) {
          if (WTSD[NPP] + WTSHE[NPP] <= WPODY[NPP] &  WTSD[NPP] > 0.0) {
            DAYS[NPP] = DAYS[NPP] + 1.
          }
          
          if (WTSD[NPP] + WTSHE[NPP] > WPODY[NPP]) {
            DAYS[NPP] = 0
          }
          
          #-----------------------------------------------------------------------
          #     Accumulate pod detachment thermal time counter (DTC) based on
          #     ratio of LFM/PDM and 10 day average slope of the leaf mass curve
          #-----------------------------------------------------------------------
          #           if (RLMPM > PR1DET | SL10 > PR2DET) GOTO 700
          if (RLMPM <= PR1DET & SL10 <= PR2DET) {
            if ((SL10 <= PR2DET) | DAYS[NPP] > DWC | WTLF <= 10.) {
              DTC[NPP] = DTC[NPP] + FT
            }
          } else {
            #           Accumulate DTC based on days without carbon before RLMPM < PR1DET
            #           and SL10 < PR2DET
            if (DAYS[NPP] > DWC | WTLF <= 10.) {
              DTC[NPP] = DTC[NPP] + FT
            }
          }
          #-----------------------------------------------------------------------
        }
      }
      #--------------------------------------------------------------------
      #     Compute detachment for each cohort
      #--------------------------------------------------------------------
      for (NPP in 1:(YRDOY - YRNR2)) { 
        #       curve based on Drew control, disease and Lowman tag pod cohort study
        if (DTC[NPP] > 0) {
          
          # ALTERADO: EXP por exp
          XPD = MSHELN[NPP] * (1.0 - XP1DET * exp(XP2DET*DTC[NPP])/100)
          XPD = max(0.0,XPD)
          if (SHELN[NPP] > XPD) {
            if (SHELN[NPP] >= 0.01 & DTC[NPP] <= 34.) {
              PDET[NPP] = SHELN[NPP] - XPD
              PDET[NPP] = max(0.0,PDET[NPP])
              PODWTD = PODWTD + (WTSHE[NPP] + WTSD[NPP])*PDET[NPP] / SHELN[NPP]
              
              SDDAM =  WTSD[NPP] * PDET[NPP] / SHELN[NPP]
              if (SDDAM > WTSD[NPP]) {
                SWIDOT = SWIDOT + WTSD[NPP]
              } else {
                SWIDOT = SWIDOT + SDDAM
              }
              
              SHDAM = WTSHE[NPP] * PDET[NPP] / SHELN[NPP]
              if (SHDAM > WTSHE[NPP]) {
                WSHIDT = WSHIDT + WTSHE[NPP]
              } else {
                WSHIDT = WSHIDT + SHDAM
              }
              
              WTSD[NPP]  = WTSD[NPP] * (1. - PDET[NPP] / SHELN[NPP])
              SDNO[NPP]  = SDNO[NPP] * (1. - PDET[NPP] / SHELN[NPP])
              WTSHE[NPP] = WTSHE[NPP]* (1. - PDET[NPP] / SHELN[NPP])
              SHELN[NPP] = SHELN[NPP]* (1. - PDET[NPP] / SHELN[NPP])
              
              WTSHE[NPP] = max(0.0,WTSHE[NPP])
              SHELN[NPP] = max(0.0,SHELN[NPP])
              WTSD[NPP]  = max(0.0,WTSD[NPP])
              SDNO[NPP]  = max(0.0,SDNO[NPP])
            }
          }
        }
        WPODY[NPP] = WTSD[NPP] + WTSHE[NPP]
      }
      
      SUMSD = 0.0
      SUMSH = 0.0
      for (NPP in 1:(YRDOY - YRNR2))  { 
        SUMSD = SUMSD + WTSD[NPP]
        SUMSH = SUMSH + WTSHE[NPP]
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #RETURN
  #END # SUBROUTINE PODDET
  assign("PODWTD", PODWTD, envir = env)
  assign("SDNO", SDNO, envir = env)
  assign("SHELN", SHELN, envir = env)
  assign("SWIDOT", SWIDOT, envir = env)
  assign("WSHIDT", WSHIDT, envir = env)
  assign("WTSD", WTSD, envir = env)
  assign("WTSHE", WTSHE, envir = env)
  assign("EMERG", EMERG, envir = env)
  
  return()
}
#=======================================================================

#***********************************************************************
#       Variable definitions
#***********************************************************************
# CURV      Function subroutine 
# DAYS(J)   Days without carbohydrate on a cohort basis (days)
# DTC       Pod detachment thermal time counter 
# DWC       Threshold number of days without carbon to trigger pod 
#             detachment (days)
# ERRKEY    Subroutine name for error file 
# FILECC    Path plus filename for species file (*.spe) 
# FT        Temperature function (0-1) 
# FTHR      Used to calculate hourly air temperature (°C)
# LUNCRP    Logical unit number for FILEC (*.spe file) 
# MSHELN(J) Maximum cohort shell mass (#/m2)
# NPP       Cohort number used as index in loops 
# PDET(J)   # of detached pods by cohort (# / m2)
# PODWTD    Mass of detached pods (g[pods] / m2[ground])
# PR1DET    Threshold for comparison of ratio of leaf mass to pod mass 
#             (RLMPM) 
# PR2DET    Threshold for comparison of slope of leaf mass (for 
#             defoliation) 
# RLMPM     Ratio of leaf mass to pod mass 
# SDDAM     Mass of seeds destroyed by detachment (g/m2)
# SDNO(J)   Number of seeds for cohort J (#/m2)
# SHDAM     Mass of shells destroyed by detachment (g/m2)
# SHELN(J)  Number of shells for cohort J (#/m2)
# SL10      Slope of leaf mass curve 
# SUMSD     Total seed mass (g/m2)
# SUMSH     Total shell mass (g/m2)
# SWIDOT    Daily seed mass damage (g/m2/day)
# TB,       |
# TO1,      | Coefficients which define daily temperature distribution:
# TO2,      | TB=base temp, T01=1st optimum, T02=2nd optimum, TM=max temp. (°C)
# TM        |
# TDLM      Last 10 days values of leaf mass (g[leaf] / m2[ground])
# TGRO(I)   Hourly air temperature (°C)
# TPODM     Total pod mass (g/m2)
# TS        Number of intermediate time steps (=24) 
# WPODY(J)  Pod mass  for cohort J (g/m2)
# WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WTLF      Dry mass of leaf tissue including C and N
#             (g[leaf] / m2[ground])
# WTSD(J)   Seed mass  for cohort J (g/m2)
# WTSHE(J)  Shell mass  for cohort J (g/m2)
# XP1DET    Coefficient which defines pod detachment equation 
# XP2DET    Coefficient which defines pod detachment equation 
# XPD       Number of shells which can be supported by plant (?) (#/m2)
# YRDOY     Current day of simulation (YYDDD)
# YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
#***********************************************************************
#      END SUBROUTINE PODDET
#=======================================================================
