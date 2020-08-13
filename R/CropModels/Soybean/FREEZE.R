#=======================================================================
#  FREEZE, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
#-----------------------------------------------------------------------
#  Calculates freeze damage.  The plant will loose all its leaves if
#  the temperature goes below FREEZ1 and stops growth entirely if the
#  temperature goes below FREEZ2.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/1989     Written.
#  12/31/1996 GH  Deleted phenology statements.
#  09/15/1998 CHP Modified for modular format.
#  05/10/1999 GH  Incorporaed in CROPGRO
#-----------------------------------------------------------------------
#  Called by  : CROPGRO
#  Calls      : None
#========================================================================
#      SUBROUTINE FREEZE(
#     &    FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,           #Input
#     &    TMIN, WTLF, YRDOY,  YRPLT,                      #Input
#     &    MDATE,                                          #Input/Output
#     &    WLFDOT)                                         #Output
##-----------------------------------------------------------------------
#      IMPLICIT NONE
#      SAVE
##-----------------------------------------------------------------------
FREEZE <- function(
  FREEZ2, NRUSLF, SLDOT,           #!Input
  TMIN, WTLF, YRDOY,  YRPLT,       #!Input
  MDATE,                           #!Input/Output
  WLFDOT) {
  
  FREEZE <- 0
  
  #CHARACTER*1  IDETO
  #CHARACTER*78 MESSAGE(10)
  #INTEGER MDATE, YRDOY, DAP, NOUTDO, YRPLT, TIMDIF
  #REAL  WLFDOT, WTLF, SLDOT, NRUSLF, TMIN, FREEZ2
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF SENESCENCE FACTORS
  FREEZ2 <- -5.00
  
  # acredito que nao serao usados! (variaveis para o OVERVIEW.OUT no DSSAT)
  #IDETO
  #NOUTDO
  
  #TODO descobrir origens dessas variaveis
  NRUSLF <- 0 # calculado no MOBIL.for
  SLDOT  <- 0 # calculado no SENES.for
  WTLF   <- 0 # calculado no GROW.for
  MDATE  <- #!Input/Output
    
    #TODO buscar no padrao do Ecosmos
    TMIN  <- 0   
  YRDOY <- 0 # usado para calcular dap/das == paste0(iyear,jday)
  YRPLT <- 0  # usado para calcular dap/das
  
  # outputs
  WLFDOT <- 0
  
  
  #-----------------------------------------------------------------------
  #DAP   = max(0,TIMDIF(YRPLT,YRDOY)) #TODO tradução timdif 
  DAP = idpp[i] #TODO VERIFICAR
  WLFDOT = WTLF - SLDOT - NRUSLF/0.16
  
  if (TMIN < FREEZ2) {
    if (MDATE < 0) {
      MDATE = YRDOY
    }
  }
  
  #    WRITE(MESSAGE(1),100) DAP
  #    WRITE(MESSAGE(2),110) YRDOY
  #    CALL WARNING(1, 'FREEZE', MESSAGE)
  #100 FORMAT('Freeze occurred at ',I4,' days after planting.')
  #110 FORMAT('  (DAY : ',I7,' )')
  #    WRITE (*,'(/,2X,A78,/,2X,A78)') MESSAGE(1), MESSAGE(2)
  #    if (IDETO == 'Y')  {
  #      WRITE (NOUTDO,'(/,5X,A78,/,5X,A78)') MESSAGE(1), MESSAGE(2)
  #    }
  
  #-----------------------------------------------------------------------
  #RETURN
  #END SUBROUTINE FREEZE
  return()
}

#-----------------------------------------------------------------------
#     FREEZE VARIABLES: 
#-----------------------------------------------------------------------
# DAP    Number of days after planting (d)
# FREEZ2 Temperature below which plant growth stops completely. (°C)
# IDETO  Switch for printing OVERVIEW.OUT file 
# NOUTDO Logical unit for OVERVIEW.OUT file 
# NRUSLF N actually mobilized from leaves in a day (g[N]/m2-d)
# SLDOT  Defoliation due to daily leaf senescence (g/m2/day)
# TMIN   Minimum daily temperature (°C)
# WLFDOT Leaf weight losses due to freezing (g[leaf]/m2-d)
# WTLF   Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
# YRDOY  Current day of simulation (YYDDD)
# MDATE  Harvest maturity (YYDDD)
# YRPLT  Planting date (YYDDD)
#-----------------------------------------------------------------------
#     END FREEZE SUBROUTINE
#-----------------------------------------------------------------------
