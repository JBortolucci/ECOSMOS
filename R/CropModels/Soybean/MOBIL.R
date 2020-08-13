#=======================================================================
#  MOBIL, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
#-----------------------------------------------------------------------
#  Calculating of the Mobilization of N
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/09/1989 GH  Written
#  04/02/1996 KJB Mobilization modified
#  08/15/1998 CHP Modified for modular format
#  05/10/1990 GH  Incorporated in CROPGRO
#-----------------------------------------------------------------------
#  Called by:  PLANT
#  Calls:      None
#=======================================================================
#  SUBROUTINE MOBIL(DYNAMIC,                           !Control
#                   &    NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              !Input
#                   &    WNRLF, WNRRT, WNRSH, WNRST,                     !Input
#                   &    NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST)         !Output
#
#!-----------------------------------------------------------------------
#  USE ModuleDefs     !Definitions of constructed variable types, 
#! which contain control information, soil
#! parameters, hourly weather data.
#IMPLICIT NONE
#SAVE
#
#INTEGER DYNAMIC

MOBIL <- function(DYNAMIC,
                 NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              #!Input
                 WNRLF, WNRRT, WNRSH, WNRST,                     #!Input
                 NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST) {         #!Output

#REAL CNMINE, NDMNEW, NMINEA, NMINEP, NMINER, NMOBR
#REAL NRUSLF, NRUSRT, NRUSSH, NRUSST, RPRO
#REAL TRNU, WNRLF, WNRRT, WNRSH, WNRST
CNMINE <- 0 # não usado, aparentemente
NDMNEW <- 0 # calculado no DEMAND.for
NMINEA <- 0
NMINEP <- 0 # calculado no DEMAND.for
NMINER <- 0
NMOBR  <- 0 # calculado no DEMAND.for
NRUSLF <- 0
NRUSRT <- 0
NRUSSH <- 0
NRUSST <- 0
RPRO   <- 0.360 #!*RESPIRATION PARAMETERS (.SPE), mas não usado, aparentemente
TRNU   <- 0 # calculado no NUPTAKE.for ?? iremos utilizar ??
WNRLF  <- 0 # calculado no GROW.for
WNRRT  <- 0 # calculado no GROW.for
WNRSH  <- 0 # calculado no GROW.for
WNRST  <- 0 # calculado no GROW.for

#***********************************************************************
#***********************************************************************
#     Seasonal initialization - run once per season
#***********************************************************************
  if (DYNAMIC == SEASINIT) {
#-----------------------------------------------------------------------
  CNMINE = 0.0         
  NMINEA = 0.0         
  NRUSLF = 0.0         #moved from INPLNT
  NRUSST = 0.0         
  NRUSRT = 0.0         
  NRUSSH = 0.0         
  
#***********************************************************************
#***********************************************************************
#     DAILY RATE/INTEGRATION
#***********************************************************************
 } else if (DYNAMIC == INTEGR) {
#-----------------------------------------------------------------------
CNMINE = 0.0
NMINEA = 0.0
NRUSLF = 0.0
NRUSST = 0.0
NRUSRT = 0.0
NRUSSH = 0.0

#-----------------------------------------------------------------------
#    Leave MOBIL with N Mined from Leaf, Stem,Root, Shell, and
#    Total Plant Tissue, and CH2O used in the Re-synthesis of Protein
#-----------------------------------------------------------------------
#      IF (TRNU .LT. NDMNEW .AND. NMINEP .GT. 1.E-4) THEN
if (NDMNEW - TRNU > 1.E-5 & NMINEP > 1.E-4) {
  NMINEA = NDMNEW - TRNU
}
if (NMINEA > NMINEP) {NMINEA = NMINEP
NMINER = NMINEA/NMINEP * NMOBR
NRUSLF = NMINER * WNRLF
NRUSST = NMINER * WNRST
NRUSRT = NMINER * WNRRT
NRUSSH = NMINER * WNRSH
CNMINE = NMINEA / 0.16 * RPRO        #Not used
}

#***********************************************************************
#***********************************************************************
#     END OF DYNAMIC IF CONSTRUCT
#***********************************************************************
}
#***********************************************************************
#  RETURN
#END ! SUBROUTINE MOBIL
return()
}

#-----------------------------------------------------------------------
# CNMINE  Protein re-synthesis cost (g[CH2O] / m2)
# NDMNEW  Total N demand for new growth (g[N] / m2 / d)
# NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NMINEP  Potential N mobilization from storage (g[N] / m2 / d)
# NMINER  Total N actually mobilized from plant in a day (g[N]/m2-d)
# NMOBR   Stage dependent N mining rate 
# NRUSLF  N actually mobilized from leaves in a day (g[N]/m2-d)
# NRUSRT  N actually mobilized from roots in a day (g[N]/m2-d)
# NRUSSH  N actually mobilized from shells in a day (g[N]/m2-d)
# NRUSST  N actually mobilized from stems in a day (g[N]/m2-d)
# RPRO    Respiration required for re-synthesizing protein from mobilized N
#           (g[CH2O] / g[protein])
# TRNU    Total N uptake in a day (g[N] / m2 / d)
# WNRLF   N available for mobilization from leaves above lower limit of 
#           mining (g[N] / m2)
# WNRRT   N available for mobilization from roots above lower limit of 
#           mining (g[N] / m2)
# WNRSH   N available for mobilization from shells above lower limit of 
#           mining (g[N] / m2)
# WNRST   N available for mobilization from stems above lower limit of 
#           mining (g[N] / m2)
#-----------------------------------------------------------------------
#      END SUBROUTINE MOBIL
#-----------------------------------------------------------------------