#=======================================================================
#  SENES, Subroutine, K.J. Boote, J.W. Jones and G. Hoogenboom
#  Calculates leaf senescence due to natural aging, drought stress,
#  light stress, and physiological maturity.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  05/01/1989     Written.
#  01/20/1997 GH  Revised.
#  06/16/1998 CHP revised for modular format.
#  05/11/1999 GH  Incorporated in CROPGRO
#  06/19/2001 GH  Fix SSNDOT, SSDOT
#  08/12/2003 CHP Added I/O error checking
#  06/30/2006 CHP/CDM Added optional KCAN to ECO file.
#-----------------------------------------------------------------------
#  Called : PLANT
#  Calls  : ERROR, FIND, IGNORE
#========================================================================

simDataVars$SLDOT  <- 0
simDataVars$SLNDOT <- 0
simDataVars$SSDOT  <- 0
simDataVars$SSNDOT <- 0

SENES <- function (DYNAMIC,DAS,
                   FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       #Input
                   RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI  #Input
                   ){                   #Output
  
  #-----------------------------------------------------------------------
  #TODO checar conexão no ECOSMOS 
  #INTEGER DYNAMIC
  #INTEGER DAS
  
  #______________________________________________________________        
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
  KCAN   <- 0.67
  # fim dos parametros de especie
  
  SWFCAB <- rep(0,NSWAB) #vetor usado internamente
  
  #TYPE (ControlType) CONTROL
  
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    SSDOT  = 0.0
    SLDOT  = 0.0
    SLNDOT = 0.0
    SSNDOT = 0.0
    RATTP  = 1.0
    
    for (I in 1:5) {
      SWFCAB[I] = 1.0
    }
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #Update value of RATTP.
    
    # ALTERADO: NSWAB, 2, -1 to seq(NSWAV, 2)
    # TODO VERIFICAR: Se I começa em 1, pois no indice abaixo ele subtrai. Se começar em 1 o indice vai ficar 0.
    for (I in seq(NSWAB,2)) { 
      SWFCAB[I] = SWFCAB[I-1]
    }
    SWFCAB[1] = SWFAC
    RATTP = SWFCAB[NSWAB]
    
    SSDOT = 0.0
    SLDOT = 0.0
    SLNDOT = 0.0
    SSNDOT = 0.0
    
    if (DAS <= NR7 & VSTAGE >= 1.0) {
      #-----------------------------------------------------------------------
      #     This section calculates natural senescence prior to the
      #     beginning of seed growth
      #-----------------------------------------------------------------------
      if (VSTAGE >= 5.0) {
        PORLFT = 1.0 - TABEX(SENPOR,XSTAGE,VSTAGE,4)
        if ((WTLF * ( 1.0 - RHOL)) > CLW*PORLFT) {
          SLDOT = WTLF * ( 1.0 - RHOL) - CLW * PORLFT
        }
      }
      #-----------------------------------------------------------------------
      #     This section calculates leaf senescence due to N mobilization.
      #     Concentration of N in stems and leaves must
      #     be recalculated since senescing leaves and petioles are assumed
      #     to contain no more mineable Protein--i.e., N content of senesced
      #     leaves and petioles is different from canopy average.
      #-----------------------------------------------------------------------
      LFSEN = SENRTE * NRUSLF / 0.16
      LFSEN = min(WTLF,LFSEN)
      SLDOT = SLDOT + LFSEN
      SLDOT = min(WTLF,SLDOT)
      #-----------------------------------------------------------------------
      #     This section calculates senescence due to low light in lower
      #     canopy.  First compute LAI at which light compensation is reached
      #     then allow LAI above this amount to be senesced over TCMP thermal
      #     days.
      #-----------------------------------------------------------------------
      LTSEN = 0.0
      if (PAR > 0.) {
        LCMP = -(1. / KCAN) * ALOG(ICMP / PAR)
        LTSEN = DTX * (XLAI - LCMP) / TCMP
        LTSEN = max(0.0, LTSEN)
      }
      #-----------------------------------------------------------------------
      #     Convert area loss to biomass(m2 *10000cm2/m2)/(cm2/g)=g/m2
      #-----------------------------------------------------------------------
      SLDOT = SLDOT + LTSEN * 10000. / SLAAD
      #-----------------------------------------------------------------------
      #     Calculate senescence due to water stress.
      #-----------------------------------------------------------------------
      WSLOSS = SENDAY * (1. - RATTP) * WTLF
      if (WSLOSS > 0.0) {
        PORLFT = 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
        WSLOSS = min(WSLOSS, WTLF - CLW * PORLFT)
        WSLOSS = max(WSLOSS, 0.0)
        SLNDOT = WSLOSS
      }
      SLDOT = SLDOT + SLNDOT
      SSDOT = SLDOT * PORPT
      SSDOT = min(SSDOT,0.1*STMWT)
      SSNDOT = SLNDOT * PORPT
      SSNDOT = min(SSDOT,SSNDOT)
      #-----------------------------------------------------------------------
      #     This section calculates senescence of leaves and petioles
      #     after R7.
      #-----------------------------------------------------------------------
    } else if (DAS > NR7) {
      if (WTLF > 0.0001) {
        SLDOT = WTLF * SENRT2
        SLNDOT = SLDOT
        SSDOT = SLDOT * PORPT
        SSNDOT = SSDOT
      } else {
        SLDOT = 0.0
        SSDOT = 0.0
        SLNDOT = 0.0
        SSNDOT = 0.0
      }
      if (STMWT < 0.0001) {
        SLNDOT = 0.0
        SSNDOT = 0.0
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #RETURN
  #END # SUBROUTINE SENES
  assign("SLDOT",SLDOT , envir = env)
  assign("SLNDOT",SLNDOT, envir = env)
  assign("SSDOT",SSDOT , envir = env)
  assign("SSNDOT",SSNDOT, envir = env)
  
  return()
}
#***********************************************************************
#     SENES VARIABLE DEFINITIONS:
#-----------------------------------------------------------------------
# CHAR      Contains the contents of last record read 
# CLW       Cumulative leaf growth (g[leaf]/m2)
# DAS       Days after start of simulation (days)
# DTX       Thermal time that occurs in a real day based on vegetative 
#             development temperature function (thermal days / day)
# ERR       Error code for file operation 
# FILECC    Path plus filename for species file (*.spe) 
# FOUND     Indicator that good data was read from file by subroutine FIND 
#             (0 - End-of-file encountered, 1 - NAME was found) 
# ICMP      Light compensation point for senescence of lower leaves because 
#             of excessive self-shading by crop canopy (moles / m2 / day)
# ISECT     Data record code (0 - End of file encountered, 1 - Found a good 
#             line to read, 2 - End of Section in file encountered, denoted 
#             by * in column 1
# KCAN      Canopy light extinction coefficient for daily PAR, for 
#             equidistant plant spacing, modified when in-row and between 
#             row spacings are not equal 
# LCMP      LAI at which today's light compensation (ICMP) is reached
#             (m2[leaf] / m2[ground])
# LNUM      Current line number of input file 
# LTSEN     Senescence of lower leaves due to self-shading of canopy
#             (1/day)
# LUNCRP    Logical unit number for FILEC (*.spe file) 
# NR7       Day when 50% of plants first have yellowing or maturing pods
#             (days)
# NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
# PAR       Daily photosynthetically active radiation or photon flux 
#             density (moles[quanta]/m2-d)
# PORLFT    Proportion of leaf weight grown which will have been senesced 
#             if no water stress has occurred prior to this V-stage 
# PORPT     Ratio of petiole to leaf weight 
# RATTP     Factor used in determining increased senescence due to water 
#             stress 
# RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# SECTION   Section name in input file 
# SENDAY    Maximum fraction of existing leaf weight which can be senesced 
#             on day N as a function of severe water stress 4 days earlier. 
# SENMAX(I) Maximum proportion of total leaf weight as a function of 
#             V-stage (XSENMX(I)) which can be senesced due to water stress. 
# SENPOR(I) Proportion of leaf weight grown which will have been senesced 
#             by a given V- stage (XSTAGE(I)) if no water stress has 
#             occurred prior to this V-stage (XSTAGE(I)) -- normal 
#             vegetative senescence does not occur if prior water stress 
#             has already  reduced leaf  
# SENRT2    Factor by which leaf weight is multiplied to determine 
#             senescence each day after NR7 (g(leaf) / g(protein loss))
# SENRTE    Factor by which protein mined from leaves each day is 
#             multiplied to determine LEAF senescence.
#             (g(leaf) / g(protein loss))
# LFSEN     Leaf senescence due to N mobilization (g[leaf] / m2[ground])
# SLAAD     Specific leaf area, excluding weight of C stored in leaves
#             (cm2[leaf] / g[leaf])
# SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
# SLNDOT    Leaf senescence due to water stress (g/m2/day)
# SSDOT     Daily senescence of petioles (g / m2 / d)
# SSNDOT    Petiole senescence due to water stress (g/m2/day)
# STMWT     Dry mass of stem tissue, including C and N
#             (g[stem] / m2[ground)
# TABEX     Function subroutine - Lookup utility 
# TCMP      Time constant for senescence of lower leaves because of 
#             excessive self-shading by crop canopy (thermal days)
# TIMDIF    Integer function which calculates the number of days between 
#             two Julian dates (da)
# VSTAGE    Number of nodes on main stem of plant 
# WSLOSS    Leaf senescence due to water stress (g/m2/day)
# WTLF      Dry mass of leaf tissue including C and N
#             (g[leaf] / m2[ground])
# XLAI      Leaf area (one side) per unit of ground area
#             (m2[leaf] / m2[ground])
# XSENMX(I) V-stage at which maximum fraction of cumulative leaf growth 
#             vulnerable to loss due to water stress is SENMAX(I).
#             (# leaf nodes)
# XSTAGE(I) V-stage at which SENPOR(I) fraction of cumulative leaf growth 
#             will have been senesced if no water stress occurred.
#             (# leaf nodes)
# YRDOY     Current day of simulation (YYDDD)
# YRSIM     Start of simulation date (YYDDD)
#-----------------------------------------------------------------------
#     END SUBROUTINE SENES
#-----------------------------------------------------------------------
