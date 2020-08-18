#=======================================================================
#  NUPTAK, Subroutine
#  Determines N uptake (adapted from CERES)
#-----------------------------------------------------------------------
#  Revision history
#  09/01/1989 JWJ,GH Written
#  03/01/1993 WTB Modified.
#  01/20/1997 GH  Modified.
#  07/10/1998 CHP modified for modular format.
#  05/11/1998 GH  Incorporated in CROPGRO
#-----------------------------------------------------------------------
#  Called from:  PLANT
#  Calls:        ERROR, FIND, IGNORE
#=======================================================================

simDataVars$TRNH4U <- 0
simDataVars$TRNO3U <- 0
simDataVars$TRNU   <- 0
simDataVars$UNH4   <- 0
simDataVars$UNO3   <- 0

NUPTAK <- function (DYNAMIC,
                    DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,   #Input
                    NH4, NO3, NLAYR, RLV, SAT, SW,                    #Input
                    TRNH4U, TRNO3U, TRNU, UNH4, UNO3) {                 #Output
  
  NUPTAK <- 0
  #-----------------------------------------------------------------------
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RTNO3  <- 0.006
  RTNH4  <- 0.006
  # fim dos parametros de planta
  
  #TODO ver padrão ECOSMOS
  DLAYR(NL)
  LL(NL)
  DUL(NL)
  SAT(NL)
  SW(NL)
  RLV(NL)
  SNO3(NL)   #vem do INSOIL.for
  SNH4(NL)   #vem do INSOIL.for
  KG2PPM(NL) #vem do INSOIL.for  [KG2PPM(L) = 1.0/(BD(L) * 0.1 * DLAYR(L))]
  NO3(NL)    #vem do INSOIL.for
  NH4(NL)    #vem do INSOIL.for

  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    TRNO3U = 0.0 
    TRNH4U = 0.0 
    TRNU   = 0.0 
    UNH4   = 0.0
    UNO3   = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #   Initialize variables
    #-----------------------------------------------------------------------
    TRNU   = 0.0
    TRNO3U = 0.0
    TRNH4U = 0.0
    NUF    = 0.0
    XMIN   = 0.0
    for (L in 1:NLAYR) {
      RNO3U[L] = 0.0
      RNH4U[L] = 0.0
      UNH4[L]  = 0.0
      UNO3[L]  = 0.0
      #KG2PPM(L) = 10. / (BD(L) * DLAYR(L))
      SNO3[L] = NO3[L] / KG2PPM[L]
      SNH4[L] = NH4[L] / KG2PPM[L]
    }
    #-----------------------------------------------------------------------
    #   Determine crop N demand (kg N/ha), after subtracting mobilized N
    #-----------------------------------------------------------------------
    ANDEM = (NDMTOT - NDMSDR) * 10.0
    if (ANDEM > 1.E-9) {
      #-----------------------------------------------------------------------
      #   Calculate potential N uptake in soil layers with roots
      #-----------------------------------------------------------------------
      for (L in 1:NLAYR) {
        if (RLV[L] > 1.E-6) {
          FNH4 = 1.0 - exp(-0.08 * NH4[L])
          FNO3 = 1.0 - exp(-0.08 * NO3[L])
          if (FNO3 < 0.04) { FNO3 = 0.0 }
          if (FNO3 > 1.0)  { FNO3 = 1.0 }
          if (FNH4 < 0.04) { FNH4 = 0.0 }
          if (FNH4 > 1.0)  { FNH4 = 1.0 }
          
          SMDFR = (SW[L] - LL[L]) / (DUL[L] - LL[L])
          if (SMDFR < 0.0) {
            SMDFR = 0.0
          }
          
          if (SW[L] > DUL[L]) {
            SMDFR = 1.0 - (SW[L] - DUL[L]) / (SAT[L] - DUL[L])
          }
          RFAC = RLV[L] * SMDFR * SMDFR * DLAYR[L] * 100.0
          #-----------------------------------------------------------------------
          #  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
          #  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
          #  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
          #-----------------------------------------------------------------------
          RNO3U[L] = RFAC * FNO3 * RTNO3
          RNH4U[L] = RFAC * FNH4 * RTNH4
          RNO3U[L] = max(0.0,RNO3U[L])
          RNH4U[L] = max(0.0,RNH4U[L])
          TRNU = TRNU + RNO3U[L] + RNH4U[L] #kg[N]/ha
        }
      }
      #-----------------------------------------------------------------------
      #   Calculate N uptake in soil layers with roots based on demand (kg/ha)
      #-----------------------------------------------------------------------
      if (ANDEM > TRNU) {
        ANDEM = TRNU
      }
      #        IF (TRNU == 0.0) GO TO 600
      if (TRNU > 0.001) {
        NUF = ANDEM / TRNU
        for (L in 1:NLAYR) {
          if (RLV[L] > 0.0) {
            UNO3[L] = RNO3U[L] * NUF
            UNH4[L] = RNH4U[L] * NUF
            XMIN    = 0.25 / KG2PPM[L]
            MXNO3U  = max(0.0,(SNO3[L] - XMIN))
            if (UNO3[L] > MXNO3U) {
              UNO3[L] = MXNO3U
            }
            XMIN = 0.5 / KG2PPM[L]
            MXNH4U  = max(0.0,(SNH4[L] - XMIN))
            if (UNH4[L] > MXNH4U) {
              UNH4[L] = MXNH4U
            }
            TRNO3U  = TRNO3U + UNO3[L]
            TRNH4U  = TRNH4U + UNH4[L]
          }
        }
        #-----------------------------------------------------------------------
        #   Convert uptake to g/m^2
        #-----------------------------------------------------------------------
        TRNO3U = TRNO3U / 10.0
        TRNH4U = TRNH4U / 10.0
        TRNU   = TRNO3U + TRNH4U
        #-----------------------------------------------------------------------
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("TRNH4U", TRNH4U, envir = env)
  assign("TRNO3U", TRNO3U, envir = env)
  assign("TRNU", TRNU, envir = env)
  assign("UNH4", UNH4, envir = env)
  assign("UNO3", UNO3, envir = env)
  
  return()
  #RETURN
  #END # SUBROUTINE NUPTAK
}

#=======================================================================

#-----------------------------------------------------------------------
#       Variable definitions
#-----------------------------------------------------------------------
# ANDEM    Total crop N demand (kg[N]/ha)
# CHAR     Contains the contents of last record read 
# DLAYR(L) Soil thickness in layer L (cm)
# DUL(L)   Volumetric soil water content at Drained Upper Limit in soil 
#            layer L (cm3 [H2O] /cm3 [soil])
# ERR      Error code for file operation 
# ERRKEY   Subroutine name for error file 
# FILECC   Path plus filename for species file (*.spe) 
# FNH4     Potential NH4 availability factor 
# FNO3     Potential NO3 availability factor 
# KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
#            [soil] for soil layer L 
# LL(L)    Volumetric soil water content in soil layer L at lower limit
#            ( cm3/cm3)
# LUNCRP   Logical unit number for FILEC (*.spe file) 
# MXNH4U   Maximum NH4 uptake from soil (kg N/ha)
# MXNO3U   Maximum NO3 uptake from soil (kg N/ha)
# NDMSDR   Amount of Mobilized N which can be used for seed growth
#            (g[N] / m2 / d)
# NDMTOT   Total N demand (g[N] / m2 / d)
# NH4(L)   Ammonium N in soil layer L (µg[N] / g[soil])
# NL       maximum number of soil layers = 20 
# NLAYR    Number of soil layers 
# NO3(L)   Nitrate in soil layer L (µg[N] / g[soil])
# NUF      N uptake fraction (ratio of demand to N uptake), <= 1.0 
# RFAC     Nitrogen uptake conversion factor ((kg N/ha) / (mg N / cm root))
# RLV(L)   Root length density for soil layer L ((cm root / cm3 soil))
# RNH4U(L) Ammonium uptake (kg N/ha)
# RNO3U(L) Nitrate uptake (kg N/ha)
# RTNH4    Ammonium uptake per unit root length (mg N / cm)
# RTNO3    Nitrate uptake per unit root length (mg N / cm)
# SAT(L)   Volumetric soil water content in layer L at saturation
#            (cm3 [water] / cm3 [soil])
# SMDFR    Relative drought factor 
# SNH4(L)  Total extractable ammonium N in soil layer L (kg [N] / ha)
# SNO3(L)  Total extractable nitrate N in soil layer L (kg [N] / ha)
# SW(L)    Volumetric soil water content in layer L
#            (cm3 [water] / cm3 [soil])
# TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
# TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
# TRNU     Total N uptake in a day (kg[N] / ha / d)
# UNH4     Uptake of NH4 from soil (interim value) (kg N/ha)
# UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
# XMIN     Amount of NH4 that cannot be immobilized but stays behind in 
#            soil as NH4; Also, Amount of NO3 that cannot denitrify but 
#            stays behind in the soil as NO3 (kg [N] / ha)
#-----------------------------------------------------------------------
#       END SUBROUTINE NUPTAK
#=======================================================================
