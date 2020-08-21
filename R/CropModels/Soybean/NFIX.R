#=======================================================================
#  NFIX, Subroutine, J.W. Jones, G. Hoogenboom, and K.J. Boote
#-----------------------------------------------------------------------
#  Calculates nitrogen fixation
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/09/1989 JWJ Written.
#  01/03/1993 WTB Modified.
#  07/14/1995 JWH Modified to reserve some C for nodule growth.
#  09/26/1995 KJB Modified to put temperature, water deficit, flooding,
#                 and age effects, not on RGR or SNA, but on growth or
#                 N-fix per unit land area.  Helped greatly for water
#                 deficit effects.  Also, C not used for N-fixation
#                 because of water deficit, is not allowed to be used
#                 for growth of nodules, except a small amount.
#  10/07/1995 KJB Modified to exclude evaporating (layer 1) zone from
#                 the nodule zone (DNOD).  Cancelled 6/25/97 KJB
#  01/19/1996 KJB Add a eigtht day memory of soil water deficiet (SWMEM8)
#                 on specific nodule activity.
#  06/25/1997 KJB Modified to use TURFAC to affect nodule growth and NFIX
#                 rather than SWFACT (frac avail. soil water in nodule
#                 zone.  Poor experience with it and recent lit indicates
#                 that nodules respond more to plant water status than to
#                 soil water around nodules.  Also, now we can use more
#                 shallow nodule zone (starting at top to 30 cm) to
#                 compute soil temperature effect and flooding effect.
#  02/02/1998 GH  Deleted CUSTOV, not used
#  07/07/1998 CHP Modified for modular format
#  05/11/1999 GH  Incorporated in CROPGRO
#  06/26/2001 GH  Correct DAS
#  06/11/2002 GH  Modified for Y2K
#  08/12/2003 CHP Added I/O error checking
#-----------------------------------------------------------------------
#  Called from:  PLANT
#  Calls:        FIND, ERROR, IGNORE
#=======================================================================

simDataVars$CNOD   <-  0
simDataVars$DWNOD  <-  0
simDataVars$DWNODA <-  0
simDataVars$NDTH   <-  0
simDataVars$NFIXN  <-  0
simDataVars$NODGR  <-  0
simDataVars$WTNFX  <-  0

NFIX <- function(DYNAMIC,
                 AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           #Input
                 FILECC, FILEIO, NLAYR, NR7, PLTPOP,             #Input
                 SAT, ST, SW, TURFAC) {                            #Input
                 # CNOD, DWNOD, DWNODA, NDTH, NFIXN,               #Output
                 # NODGR, WTNFX, SENNOD) {                          #Output
  
  # TODO:Remover 
  DAS <- idpp[i] 
  
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
  RFIXN  <- 2.830
  #!*PLANT COMPOSITION VALUES
  PRONOD <- 0.300
  
  #fim dos parametros de planta
  
  CTONOD <- 0  #TODO descobrir de onde vem exatamente (CROPGRO.for inicia como 0)
  CNODMN <- 0 #TODO descobrir de onde vem exatamente (CROPGRO.for inicia como 0)
  
  EFNFIX <- 1 #TODO descobrir de onde vem exatamente (relacionado à inoculação)
  EFINOC <- 1 #TODO descobrir de onde vem exatamente (relacionado à inoculação)
  EFINOC <- ifelse(EFINOC <= 0.0, 1.0, EFINOC) # veio da inicialização ('INITI' section)
  EFNFIX <- ifelse(EFNFIX <= 0.0, 1.0, EFNFIX) # veio da inicialização ('INITI' section)
  
  PLTPOP <- 40  # equivalente ao [.SBX] *PLANTING DETAILS: PPOE

  SWMEM  <- rep(0, 9) #TODO ver com JAIR
  
  #TODO ver padrão ECOSMOS
  # DLAYR  <- rep(0, NL)
  DLAYR <-  c(5.00000000  ,    10.0000000   ,    15.0000000   ,    15.0000000   ,    15.0000000   ,    30.0000000    ,   30.0000000   ,    30.0000000     , -99.0000000    ,  -99.0000000   ,   -99.0000000   ,   -99.0000000    ,  -99.0000000    ,  -99.0000000    ,  -99.0000000    ,  -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000  ,    -99.0000000 )
  # SAT    <- rep(0, NL)
  SAT   <-  c(0.360000014 ,    0.349999994  ,    0.333000004  ,    0.326999992  ,    0.319999993  ,    0.319999993   ,   0.319999993  ,    0.319999993    ,  -99.0000000   ,   -99.0000000  ,    -99.0000000  ,    -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000  ,    -99.0000000  ,    -99.0000000  ,    -99.0000000 ,     -99.0000000)
  # SW     <- rep(0, NL)
  SW    <-  c(0.219999999 ,    0.219999999  ,    0.213000000  ,    0.207000002  ,    0.200000003  ,    0.180000007   ,   0.180000007  ,    0.180000007    ,   0.00000000   ,    0.00000000  ,     0.00000000  ,     0.00000000   ,    0.00000000   ,    0.00000000   ,    0.00000000   ,    0.00000000  ,     0.00000000  ,     0.00000000  ,     0.00000000 ,      0.00000000)
  
  #NL = 20
  ST<- rep(0,20)
  LAYERFRAC <- rep(0,20)
  SENNOD    <- rep(0,20)
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    CNOD   = 0.0
    DWNOD  = 0.0    
    DWNODA = 0.0  
    NDTH   = 0.0    
    NFIXN  = 0.0    
    NODGR  = 0.0    
    WTNFX  = 0.0    
    SDWNOD = 0 
    SENNOD = rep(0,9)
    
    DNOD   = 30.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #     DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
    #CALL GET(CONTROL)
    # TODO:Remover 
    DAS <- idpp[i]
    #-----------------------------------------------------------------------
    #   Set initial nodule mass to DWNODI as read from crop species file
    #-----------------------------------------------------------------------
    if (SDWNOD < 1) {
      DWNOD  = DWNODI * PLTPOP
      SDWNOD = 1
      DWNODA = DWNODI * PLTPOP
      WTNFX  = DWNODA * 0.16 * PRONOD
      for (J in 1:8) {
        SWMEM[J] = 1.0
      }
    }
    
    #-----------------------------------------------------------------------
    #   Initialize soil water and temperature factors (top DNOD cm of soil)
    #-----------------------------------------------------------------------
    SWFACT = 1.0
    FLDACT = 1.0
    ACSTF  = 0.0
    ACSTG  = 0.0
    DSW    = 0.0
    FLDSUM = 0.0
    #-----------------------------------------------------------------------
    #     Calculate carbon allocated per unit of nodule biomass:
    #     CNODCR = C requirement for nodule respiration (g C/g nodule/d)
    #-----------------------------------------------------------------------
    CNFACT = 1.
    if (DWNOD > 1.E-4) {
      FRCNM = CTONOD/DWNOD
      if (FRCNM < CNODCR) { CNFACT = FRCNM / CNODCR }
    }
    #-----------------------------------------------------------------------
    #   Calculate soil water and temperature factors for each layer to DNOD
    #-----------------------------------------------------------------------
    LAYERFRAC = 0.0
    DSWP = 0.0
    DNOD = 50.0
    for (I in 1:NLAYR) {
      FLAYR = 1.0
      DSW = DSW + DLAYR[I]
      if (DSW > DNOD) { FLAYR = (DNOD-(DSW-DLAYR[I]))/DLAYR[I] }
    
      ACSTF = ACSTF + DLAYR[I] * FLAYR * CURV(TYPFXT,FNFXT[1],FNFXT[2],FNFXT[3],FNFXT[4],ST[I])
      
      ACSTG = ACSTG + DLAYR[I] * FLAYR * CURV(TYPNGT,FNNGT[1],FNNGT[2],FNNGT[3],FNNGT[4],ST[I])
      
      EPORS = max(SAT[I] - SW[I], 0.0)
      FLDSUM = FLDSUM + DLAYR[I] * FLAYR * CURV(TYPFXW,FNFXW[1],FNFXW[2],FNFXW[3],FNFXW[4],EPORS)
      
      if (I == 1) {
        LAYERFRAC[1] = DSW / DNOD
      } else {
        LAYERFRAC[I] = (DSW - DSWP)*FLAYR / DNOD
      }
      DSWP = DSW
      if ( FLAYR < 1.0 ) { TNFIX  = ACSTF / DNOD }#GOTO 400
      
    }
    #-----------------------------------------------------------------------
    #   Constraints due to soil water and T and average nodule age:
    #   TNFIX : soil T effect on N2 fixation
    #   TNGRO : soil T effect on nodule growth
    #   SWFACT: soil water deficit effect on N2 fixation and nodule growth
    #   FLDACT: soil water flooding effect on N2 fixation and nodule growth
    #   NFXAGE: average nodule age effect on nodule growth
    #-----------------------------------------------------------------------
    #400 TNFIX  = ACSTF / DNOD
    TNGRO  = ACSTG / DNOD
    FLDACT = FLDSUM / DNOD
    
    SWFACT = CURV(TYPFXD,FNFXD[1],FNFXD[2],FNFXD[3],FNFXD[4],TURFAC)
    NFXAGE = CURV(TYPFXA,FNFXA[1],FNFXA[2],FNFXA[3],FNFXA[4],DXR57)
    #-----------------------------------------------------------------------
    # DETERMINE MEMORY OF PREVIOUS EIGHT DAYS OF SOIL WATER DEFICITS
    #-----------------------------------------------------------------------
    #for (J in 8,2,-1) { #TODO VER COM JAIR
      # TODO VERIFICAR: 10,2,-1 deve ser de 10 até 2 no passo -1 (10, 9, 8, 7...) [from PODDET.for]
    for (J in seq(8, 2)) {  
      SWMEM[J] = SWMEM[J-1]
    }
    SWMEM(1) = SWFACT
    
    SWMEM8 = 0.0
    for (J in 1:8) {
      SWMEM8 = SWMEM8 + SWMEM[J]
    }
    SWMEM8 = SWMEM8/8
    #-----------------------------------------------------------------------
    #     Reserve CNODMN for nodule growth.  JWH 7/9/95
    #-----------------------------------------------------------------------
    CLEFT = CTONOD - CNODMN
    #-----------------------------------------------------------------------
    #    Compute Specific Nodule Activity taking into account the maximum
    #       activity of the nodules (SNACTM), and strain effects only.
    #    9/27/95 moved temp, water deficit, and soil water flooding effects
    #    below to the primary rate.  We are not getting proper stress effects.
    #-----------------------------------------------------------------------
    SNACT  = SNACTM  * EFNFIX
    #-----------------------------------------------------------------------
    #       Compute nodule death rate as function of SW deficit, SW flooding,
    #                               and carbon deficit (chp)
    #-----------------------------------------------------------------------
    RNDTH = NDTHMX * max((1.-FLDACT),(1.-SWFACT),(1.-CNFACT))
    NDTH = min(1.0,RNDTH) * DWNOD               #g/m2
    for (I in 1:NLAYR) {
      SENNOD[I] = NDTH * LAYERFRAC[I] * 10.     #kg/ha
    }
    #-----------------------------------------------------------------------
    #    Compute N-Fixation
    #
    #-----------------------------------------------------------------------
    if (DAS < NR7) {
      PNFIXN = min((CLEFT * 0.16 / RFIXN), (DWNOD * SNACT)) * TNFIX
      NFIXN = PNFIXN * min(SWFACT, SWMEM8, FLDACT)
    } else {
      PNFIXN = 0.0
      NFIXN = 0.0
    }
    #-----------------------------------------------------------------------
    #    Compute C Used for N-Fixation
    #-----------------------------------------------------------------------
    PCSFIX = (PNFIXN / 0.16) * RFIXN
    CUSFIX = (NFIXN  / 0.16) * RFIXN
    CNOFIX = PCSFIX - CUSFIX
    #-----------------------------------------------------------------------
    #     Compute C Left to Grow New Nodule Mass
    #     Includes minimum reserved for nodule growth (CNODMN) plus any C
    #     left after N fixation.  JWH 7/11/95
    #-----------------------------------------------------------------------
    CLEFT = max(0.0,CLEFT - CUSFIX- 0.9*CNOFIX) + CNODMN
    #-----------------------------------------------------------------------
    #    Compute Potential Growth of Nodules (Demand)
    #    EFNFIX = strain efficiency
    #    EFINOC = inoculation effectiveness (or rhizobium density factor)
    #-----------------------------------------------------------------------
    if (DAS < NR7) {
      NODRGR = NODRGM  * EFNFIX * EFINOC
    } else {
      NODRGR = 0.0
    }
    #-----------------------------------------------------------------------
    #    Compute Nodule Growth, Limiting by Either Supply or Demand for C
    #-----------------------------------------------------------------------
    NODGR = min(CLEFT/AGRNOD,DWNOD*NODRGR) * TNGRO * min(SWFACT,FLDACT) * NFXAGE
    CNODGR = NODGR * AGRNOD
    #-----------------------------------------------------------------------
    #    Compute C used in N-Fixation and Nodule Growth (Including
    #    Respiration Costs) Today
    #-----------------------------------------------------------------------
    CNOD = CUSFIX + CNODGR
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("CNOD", CNOD, envir = env)
  assign("DWNOD", DWNOD, envir = env)
  assign("DWNODA", DWNODA, envir = env)
  assign("NDTH", NDTH, envir = env)
  assign("NFIXN", NFIXN, envir = env)
  assign("NODGR", NODGR, envir = env)
  assign("WTNFX", WTNFX, envir = env)
  assign("SENNOD", SENNOD, envir = env)
  
  return()
}
#=======================================================================
# ACSTF    Weighted average soil temperature effect on N fixation (cm)
# ACSTG    Weighted average soil temperature effect on nodule growth (cm)
# AGRNOD   CH2O requirement for nodule growth (g[CH2O] / g[nodule])
# CLEFT    C left to grow new nodule mass (g[CH2O] / m2 / d)
# CNFACT   Ratio of C needed from nodules for vegetative and reproductive 
#            growth to C needed for nodule respiration (stress factor) 
# CNOD     C used in N-Fixation and nodule growth (including respiration 
#            costs) today (g[CH2O] / m2 / d)
# CNODCR   C requirement for nodule respiration (g[C] / g[nodule] / d)
# CNODGR   C used in nodule growth today (g[CH2O] / m2 / d)
# CNODMN   Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
# CNOFIX   C used for N fixation (g[CH2O] / m2 / d)
# CTONOD   C to allocate to nodules to fix N needed for reproductive and 
#            vegetative growth (g[CH2O] / m2 / d)
# CURV     Function subroutine 
# CUSFIX   C used in N-Fixation today (g[CH2O] / m2 / d)
# DAS      Days after start of simulation (days)
# DLAYR(L) Soil thickness in layer L (cm)
# DNOD     Depth of nodule zone (cm)
# DSW      Accumulated soil depth (cm)
# DWNOD    Current nodule mass (g[nodule] / m2)
# DWNODA   Cumulative nodule growth (g[nodule] / m2)
# DWNODI   Initial nodule mass per plant (g[nodule] / plant)
# DXR57    Relative time between first seed (NR5) and physiological 
#            maturity (NR7) 
# EFINOC   Inoculation effectiveness (or rhizobium density factor) 
# EFNFIX   Strain efficiency 
# EPORS    Soil water content above saturation (cm3/cm3)
# ERR      Error code for file operation 
# ERRKEY   Subroutine name for error file 
# FILECC   Path plus filename for species file (*.spe) 
# FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
# FLAYR    Fraction of layer within nodule zone 
# FLDACT   Soil water flooding effect on N2 fixation and nodule growth 
#            (0-1) 
# FLDSUM   Weighted average flooding conditions effect on N fixation and 
#            nodule growth and nodule death rate (cm)
# FNFXA(I) Critical points in development from first seed to physiological 
#            maturity for reducing N fixation due to canopy physiological age 
# FNFXD(I) Effects of soil water stress factor (TURFAC) on N fixation 
# FNFXT(I) Critical temperature points for function to reduce N fixation 
#            rate when temperature is not optimal (°C)
# FNFXW(I) Critical soil volumetric water contents for reducing N fixation 
#            when the soil is too wet (flooded) (cm3/cm3)
# FNNGT(I) Critical temperature points for function to reduce Nodule growth 
#            rates when temperature is not optimal (°C)
# FRCNM    C required for reproductive and vegetative growth per nodule 
#            mass (g[CH2O] / g[nodule] / d)
# LUNCRP   Logical unit number for FILEC (*.spe file) 
# LUNIO    Logical unit number for FILEIO 
# NDTH     Nodule death rate (g[nodule] / m2 / d)
# NDTHMX   Maximum relative death rate of nodules under flooded or dry 
#            conditions (g[nodule] / g[nodule] / d)
# NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
# NFXAGE   Average nodule age effect on nodule growth (0-1) 
# NL       maximum number of soil layers = 20 
# NLAYR    Number of soil layers 
# NODGR    New nodule growth (g[nod] / m2 / d)
# NODRGM   Maximum nodule relative growth rate (g[nodule] / g[nodule] / d)
# NODRGR   Effective nodule relative growth rate
#            (g[nodule] / g[nodule] / d)
# NR7      Day when 50% of plants first have yellowing or maturing pods
#            (days)
# PCSFIX   Potential C used in N-Fixation today (g[CH2O] / m2 / d)
# PLTPOP   Plant population (# plants / m2)
# PNFIXN   Potential amount of N fixation today (g[N] / m2 / d)
# PRONOD   Protein composition in nodules (g[protein] / g[nodule])
# RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
# RNDTH    Nodule death rate (g[nodule] / g[nodule] / d)
# SAT(L)   Volumetric soil water content in layer L at saturation
#            (cm3 [water] / cm3 [soil])
# SDWNOD   Denotes first entrance to NFIX subroutine 
# SNACT    Specific nodule activity (g [nodule])
# SNACTM   Maximum activity of the nodules (g [nodule])
# ST(L)    Soil temperature in soil layer L (°C)
# SW(L)    Volumetric soil water content in layer L
#            (cm3 [water] / cm3 [soil])
# SWFACT   Soil water deficit effect on N2 fixation and nodule growth (0-1)
#            
# SWMEM(I) Values of last 8 days of soil water deficit factor (SWFACT) 
# SWMEM8   Average of last 8 days of soil water deficit factor 
# TNFIX    Soil temperature effect on N2 fixation (0-1) 
# TNGRO    Soil temperature effect on nodule growth (0-1) 
# TURFAC   Water stress factor for expansion (0 - 1) 
# TYPFXA   Type of function for canopy age effects on N fixation 
# TYPFXD   Type of function for dry soil effects on N fixation 
# TYPFXT   Type of function for temperature effects on N fixation rates 
# TYPFXW   Type of function for wet soil effects on N fixation 
# TYPNGT   Type of function for temperature effects on Nodule growth rates 
# WTNFX    Cumulative weight of N fixed (g[N] / m2)
#-----------------------------------------------------------------------
#       END SUBROUTINE NFIX
#=======================================================================

