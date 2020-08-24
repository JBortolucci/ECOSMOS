# Duas (PHOTO.for & RESPIR.for) rotinas foram 'juntadas' aqui
# Decidimos trazer as duas, por hora, mesmo sabendo que será trocada pelas equivalentes no ECOSMOS

#PHOTO.for
simDataVars$AGEFAC  <-  0
simDataVars$PG      <-  0

#RESPIR.for
simDataVars$RO     <-  0  #I/O
simDataVars$RP     <-  0  #I/O
simDataVars$MAINR  <-  0

#=======================================================================
#  PHOTO, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
#  Compute daily photosynthetic using canopy (C) method.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/1989 KJB Written.
#  06/02/1993 NBP Made into subroutine.
#  02/11/1994 NBP Corrected dimension (15) for XPGSLW, YPGSLW
#  04/24/1994 NBP Replaced TAIRHR, TAVG with TGRO, TGROAV.
#  09/25/1995 KJB Light interception for ET the same for canopy and leaf
#                 models
#  11/11/1999 CHP Modified for modular format
#  01/13/2000 NBP Added SWFAC effect on PG
#  06/11/2002 GH  Modified for Y2K
#  08/12/2003 CHP Added I/O error checking
#  03/24/2004 CHP Added P stress based on Bostick model
#  07/30/2004 CHP Added KC_SLOPE to SPE file and KC_ECO to ECO file.
#  06/11/2007 CHP PStres1 affects photosynthesis
#-----------------------------------------------------------------------
#  Called from:   Main
#  Calls:         PHOTIP
#=======================================================================
  
PHOTO <- function(CONTROL, 
                  BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
                  NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
                  SWFAC, TDAY, XHLAI, XPOD){                       #Input
                  #AGEFAC, PG)                                     #Output
  
  #-----------------------------------------------------------------------
  # trouxe o PHOTIP.for como parametros aqui
  #=======================================================================
  #  PHOTIP, Subroutine, N.B. Pickering
  #  Read input parameters for daily photosynthesis.
  #-----------------------------------------------------------------------
  #  REVISION HISTORY
  #  09/25/99     Written.
  #-----------------------------------------------------------------------
  #  Output:
  #  Local :
  #-----------------------------------------------------------------------
  #  Called: PG
  #  Calls : None
  #=======================================================================
  
  #TODO verificar unidades, mas acredito que no Ecosmos terá que vir em 'cm' OU removemos o ajuste para 'm' abaixo
  ROWSPC <- 0.5 #from .SBX: management details
  ROWSPC = ROWSPC / 100. # CROPGRO uses ROWSPC as m 
  #TODO: BETN é calculado no GROW.for (ver se CROPGRO chama 'photo' antes da 'grow' ou mesmo se iniciar no 'RUNINTI' OU 'SEASINIT' pq na 'photo' está no 'RATE')
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  LMXSTD   <- 1.250 #LFMAX no .CUL
  PHTHRS10 <- 27.00 #LFMAX no   equivalente ao PHTHRS[10] na PHENOL.R
                    # [.CUL] SD-PM - Time between first seed (R5) and physiological maturity (R7) (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN     <- 0.67
  KC_SLOPE <- 0.10
  CCEFF  <- 79.0
  CCMAX  <- 2.08
  CCMP   <- 79.0
  LNREF  <- 4.90
  PARMAX <- 40.00
  PGREF  <- 1.030
  PHTMAX <- 61.00
  XPGSLW <- c(0.0, .001, .002, .003, .0035, .004, .005, .006, .008, .010) #(15 valores no fortran)
  YPGSLW <- c(.162, .679, .867, .966, 1.000, 1.027, 1.069, 1.100, 1.141, 1.167) #(15 valores no fortran)
  TYPPGN <- QDR
  FNPGN  <- c(1.90,  5.50,  20.0,  20.0)
  TYPPGT <- LIN
  FNPGT  <- c(6.00,  22.0,  34.0,  45.0)
  # fim dos parametros de planta
  
  #TODO na integração...
  CO2 <- 406.6 #TODO linkar com parte climática/atmosférica do ECOSMOS
  SLPF <- 1 # vem do arquivo de solo (.SOL), ajuste empírico da ambiente (solo ou ambiente) na PG
  SWFAC <- 1 #TODO linkar com ECOSMOS posteriormente! (Effect of soil-water stress on photosynthesis, 1.0=no stress, 0.0=max stress )

  
  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    
    # Veio do RUNINIT
    #-----------------------------------------------------------------------
    #     Adjust canopy photosynthesis for GENETIC input value of
    #     maximum leaf photosyntheses (LMXSTD).  Exponential curve from
    #     from the hedgerow photosynthesis model (Boote et al, 199?).
    #-----------------------------------------------------------------------
    if (PGREF > 0.) {
      PGLFMX = (1. - exp(-1.6 * LMXSTD)) / (1. - exp(-1.6 * PGREF))
    } else {
      PGLFMX = 1.0
    }
    
    #-----------------------------------------------------------------------
    AGEFAC  = 1.0
    CUMSTR  = 0.0
    COLDSTR = 0.0
    PG      = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if (DYNAMIC == 'RATE') {
    #-----------------------------------------------------------------------
    #     Calculate maximum photosynthesis as function of PAR, g CH2O/m2
    #-----------------------------------------------------------------------
    PTSMAX = PHTMAX * (1.0 - exp(-(1.0 / PARMAX) * PAR))
    
    #-----------------------------------------------------------------------
    #     Calculate reduction in photosynthesis due to incomplete canopy.
    #-----------------------------------------------------------------------
    if (BETN <= ROWSPC) {
      SPACNG = BETN / ROWSPC
    } else {
      SPACNG = ROWSPC / BETN
    }
    #chp per CDM:      KCANR = KCAN - (1. - SPACNG) * 0.1
    KCANR = KCAN - (1. - SPACNG) * KC_SLOPE
    PGFAC = 1. - exp(-KCANR * XHLAI)
    
    #-----------------------------------------------------------------------
    #     Compute reduction in PG based on the average daylight temperature.
    #-----------------------------------------------------------------------
    TPGFAC = CURV(TYPPGT,FNPGT(1),FNPGT(2),FNPGT(3),FNPGT(4),TDAY)
    
    #-----------------------------------------------------------------------
    #     Compute reduction in PG as a function of leaf N concentration.
    #-----------------------------------------------------------------------
    AGEFAC = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP)
    AGEREF = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)
    AGEFAC = AGEFAC / AGEREF
    
    #-----------------------------------------------------------------------
    #     9/24/95 KJB,JWJ Decrease sensitivity of canopy PG to N function
    #     to mimic behavior of leaf version.  AGEFAC corresponds to leaf
    #     PG vs. leaf N. Function does not act strongly on QE, thus, we
    #     need to scale effect back on daily canopy version.
    #-----------------------------------------------------------------------
    AGEFCC = (1.0 - exp(-2.0 * AGEFAC)) / (1. - exp(-2.0 * 1.0))
    
    #-----------------------------------------------------------------------
    #     Compute canopy pg response to changes in specific leaf weight.
    #     (Dornhoff and Shibles, 197?).
    #-----------------------------------------------------------------------
    if (SLAAD > 0.0) {
      SLW = 1. / SLAAD
    } else {
      SLW = 0.0099
    }
    PGSLW = TABEX(YPGSLW, XPGSLW, SLW, 10)
    
    #-----------------------------------------------------------------------
    #     Adjust canopy photosynthesis for CO2 concentration assuming a
    #     reference value of CO2 of 330 ppmv.
    #-----------------------------------------------------------------------
    CCK = CCEFF / CCMAX
    A0 = -CCMAX * (1. - exp(-CCK * CCMP))
    PRATIO = A0 + CCMAX * (1. - exp(-CCK * CO2))
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
    #      ELSEIF (DYNAMIC .EQ. 'INTEGR') THEN
    #-----------------------------------------------------------------------
    #     Effect of daylength on daily gross PG, computer by KJB with
    #     stand alone model, and used by Ernie Piper in his dissertation
    #     see page 127 for the equation and conditions.  Nornamized to
    #     about 13 hours where the function is 1.00.  Actually
    #     normalized to a bit higher between 13 and 14 hours.
    #
    #     DLFAC = 1.0 + 0.6128 - 0.01786*DAYL + 0.006875*DAYL*DAYL
    #    &        - 0.000247*DAYL*DAYL*DAYL
    #
    #     Compute daily gross photosynthesis (g CH2O/m2/d)
    #-----------------------------------------------------------------------
    #      PG =  PTSMAX * SLPF * PGFAC * TPGFAC * AGEFCC * PGSLW
    
    #      PG =  PTSMAX * SLPF * PGFAC * TPGFAC * MIN(AGEFCC, PSTRES2) * 
    #     &            PGSLW * PRATIO * PGLFMX * SWFAC
    
    #     CHP 05/07/2004 
    #     AGEFCC can be > 1.0, so dont want to use minimum of 
    #     PStres1 and AGEFCC.  (PStres1 is always 1.0 or below).
    if (AGEFCC >= 1.0) {
      E_FAC = AGEFCC * PStres1
    } else {
      E_FAC = min(AGEFCC, PStres1)
    }
    
    PG =  PTSMAX * SLPF * PGFAC * TPGFAC * E_FAC * PGSLW * PRATIO * PGLFMX * SWFAC
    
    #From WDB (chp 10/21/03):
    #        PG = PG * MIN(SWFAC ,2*(1-SATFAC) )
    #        PGN = PGN * MIN(SWFAC,2*(1-SATFAC) )
    
    #-----------------------------------------------------------------------
    #     9/27/95 KJB added cumulative water stress effect on PG after R5.
    #     CUMULATIVE STRESS (WATER, TEMP) UPON PG CAPACITY.  REDUCE FROM POT.
    #     PG AFTER R5, WITH TWO FUNCTIONS.  ONE DEPENDING ON THE PRIMARY
    #     STRESS AND THE OTHER DEPENDING ON DISTANCE FROM R5 TO R7, DXR57
    #     INITIALLY THE STRESS IS SOIL WATER, BUT THIS MAY WORK FOR COLD TEMP.
    #     0.5 IS A SCALAR, POSSIBLY COULD GO HIGHER.  MAX CUMSTR IS 0.2856
    #     FOR 78-RF, 0.1858 FOR EGLIN-88, THEN 0.528 FOR 84-RF.  DECREASES
    #     YIELD 77, 52, 50, AND 16 kg/ha FOR 78RF, EGLIN-88, 84RF, AND 81REP
    #     MINOR DECREASE IN SEED SIZE.
    #     1/19/96, USING XPOD CAUSES IT TO BE LESS EFFECTIVE EARLY UNTIL FULL
    #     PARTITIONING TO POD OCCURS (MAYBE JUST SEED, SEE XPOD CALCULATION).
    #     12/19/95 Changed scalar to 0.4.  Too strong for peanut.  Also,
    #     2/6/96 Changed scalar to 0.3.  Too strong for 78RF too. Also,
    #     the problem is really with seed growth potential, not as much on PG.
    #-----------------------------------------------------------------------
    #     NEEDS A FUNCTION.  SEE TMIN AND CHILL IN LEAF SUBROUTINE
    #     AND THEN ADD A SCALAR?
    #       COLDSTR =  COLDSTR + DXR57 * (F(TMIN?)*XPOD / PHTHRS(10)
    #       PG = PG * (1.0 - MAX(0.4*CUMSTR,1.0*COLDSTR))
    #-----------------------------------------------------------------------
    if (DAS > NR5) {
      CUMSTR =  CUMSTR + DXR57 * (1.0 - SWFAC) * XPOD / PHTHRS10
      COLDSTR = 0.0
      PG = PG * (1.0 - 0.3 * CUMSTR)
    } else {
      CUMSTR = 0.0
      COLDSTR = 0.0
    }
    
    PG = PG * EXCESS
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #RETURN
  #END !SUBROUTINE PHOTO
  assign("AGEFAC", AGEFAC, envir = env)
  assign("PG", PG, envir = env)
}


#=======================================================================
# Variable definitions for PHOTO and PHOTIP
#=======================================================================
# AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
#            (fraction)
# AGEFCC   Effect of AGEFAC on photosynthesis 
# AGEREF   Reference value calculated at reference leaf N value to 
#            normalize the effect of N for different plant species 
# BETN     Spacing between plants along a row (m / plant)
# BLANK    Blank character 
# CCEFF    Relative efficiency of CO2 assimilation used in equation to 
#            adjust canopy photosynthesis with CO2 concentrations 
# CCK      Computed exponent for relationship between CO2 and canopy 
#            photosynthesis (=CCEFF / CCMAX) 
# CCMAX    Maximum daily canopy photosynthesis relative to photosynthesis 
#            at a CO2 concentration of 330 vpm 
# CCMP     Canopy CO2 compensation point (CO2 at which daily PG is 0.0) 
# CHAR     Contains the contents of last record read 
# CO2      Atmospheric carbon dioxide concentration (?mol[CO2] / mol[air])
# COLDSTR  Cold weather stress factor for photosynthesis (not currently 
#            used) 
# CUMSTR   Cumulative stress factor for photosynthesis after start of seed 
#            production 
# CURV     Function subroutine 
# DAS      Days after start of simulation (d)
# DXR57    Relative time between first seed (NR5) and physiological 
#            maturity (NR7) 
# ERR      Error code for file operation 
# ERRKEY   Subroutine name for error file 
# EXCESS   Factor based on excess PG used to affect tomorrow's PG 
#            calculation 
# FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
# FILECC   Path plus filename for species file (*.spe) 
# FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
# FNPGN(I) Critical leaf N concentration for function to reduce 
#            photosynthesis due to low leaf N levels (4 values for function 
#                                                     !            CURV) 
# FNPGT(I) Critical values of temperature for the functions to reduce 
#            canopy PG under non-optimal temperatures (in function CURV) 
# FOUND    Indicator that good data was read from file by subroutine FIND 
#            (0 - End-of-file encountered, 1 - NAME was found) 
# ISECT    Indicator of completion of IGNORE routine: 0 - End of file 
#            encountered, 1 - Found a good line to read, 2 - End of Section 
#            in file encountered denoted by * in column 1.  
# KCAN     Canopy light extinction coefficient for daily PAR, for 
#            equidistant plant spacing, modified when in-row and between 
#            row spacing are not equal 
# KCANR    Canopy light extinction coefficient, reduced for incomplete 
#            canopy 
# LMXSTD   Maximum leaf photosyntheses for standard cultivar 
# LNREF    Value of leaf N above which canopy PG is maximum (for standard 
#                                                            !            cultivar) 
# LNUM     Current line number of input file 
# LUNCRP   Logical unit number for FILEC (*.spe file) 
# LUNIO    Logical unit number for FILEIO 
# NR5      Day when 50% of plants have pods with beginning seeds (days)
# PAR      Daily photosynthetically active radiation or photon flux density
#            (moles[quanta]/m2-d)
# PARMAX   Value of PAR at which photosynthesis is 63% of the maximum value 
#            (PHTMAX). Used in daily canopy photosynthesis calculations
#            (moles[quanta]/m2-d)
# PATHCR   Pathname for SPE file or FILEE. 
# PATHL    Number of characters in path name (path plus filename for FILEC)
#            
# ! PG       Daily gross photosynthesis (g[CH2O] / m2 / d)
# PGFAC    Multiplier to compute daily canoy PG as a function of leaf area 
#            index (LAI) 
# PGLFMX   Multiplier for daily canopy photosynthesis to account for 
#            cultivar differences in leaf photosynthesis capabilities 
# PGREF    Reference value for leaf level photosynthesis used in canopy 
#            light response curve (?mol[CO2] / m2-s)
# PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG 
# PHTHRS10 Threshold time that must accumulate in phase 10 for the next 
#            stage to occur.  Equivalent to PHTHRS(10) in Subroutine 
#            PHENOLOG. 
# PHTMAX   Maximum amount of CH20 which can be produced if 
#            photosynthetically active radiation (PAR) is very high (3 
#                                                                    !            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
# PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG 
#            computed for 330 vpm 
# PTSMAX   Potential amount of CH20 which can be produced at the specified 
#            PAR for full canopy (LAI>8), all other factors optimal
#            (g[CH2O]/m2-d)
# RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
#            reduction. (%)
# ROWSPC   Row spacing (m)
# SECTION  Section name in input file 
# SLAAD    Specific leaf area, excluding weight of C stored in leaves
#            (cm2[leaf] / g[leaf])
# SLPF     Empirical multiplier to adjust daily canopy PG due to unknown 
#            soil or environmental factors that persist in a given location 
# SLW      Specific leaf weight (g[leaf] / m2[leaf])
# SPACNG   Ratio of distance between plants in a row to distance between 
#            rows (or vice versa - always < 1) 
# SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress, 
#            0.0=max stress 
# TABEX    Function subroutine - Lookup utility 
# TDAY     Average temperature during daylight hours (?C)
# TIMDIF   Integer function which calculates the number of days between two 
#            Julian dates (da)
# TPGFAC   Reduction in specific leaf area due to daytime temperature being 
#            less than optimal (0-1) 
# TYPPGN   Type of function for the leaf N effects on PG 
# TYPPGT   Character variable specifying the type of function to use for 
#            the relationship between temperature and PG (for use in 
#                                                         !            function subroutine CURV) 
# XHLAI    Leaf area index (m2[leaf] / m2[ground])
# XPGSLW(I) Array of SLW values for table look-up function, used with YPGSLW
#            (g[leaf] / m2[leaf])
# XPOD     Growth partitioning to pods which slows node appearance
#            (fraction)
# YPGSLW(I) Array of PG values corresponding to SLW values in array XPGSLW
#            (g[CH2O] / m2 / d)
#=======================================================================
# ! END SUBROUTINE PHOTO
#=======================================================================

#=======================================================================
#  RESPIR, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
#  Calculates maintainence respiration and net photosythate
#  available.  PGAVL is the net photosynthesis after maintenance
#  respiration is subtracted from gross photosynthesis.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/1989 KJB Written.
#  10/15/1992 NBP Made into subroutine.
#  09/15/1998 CHP Modified for modular format
#  05/11/1999 GH  Incorporated in CROPGRO
#-----------------------------------------------------------------------
#  Called from:   PLANT
#  Calls:         None
#=======================================================================

RESPIR <- function(
  PG, R30C2, RES30C, TGRO, WTMAIN) {                #Input
  #RO, RP,                                         #Input/Output
  #MAINR)                                          #Output
  
  #-----------------------------------------------------------------------
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*RESPIRATION PARAMETERS
  R30C2  <- .0040
  RES30C <- 3.5E-04
  # fim dos parametros de planta
  
  # SCLTS added on 4 July 2017 by Bruce Kimball
  # REAL TGRO(TS)
  
  #-----------------------------------------------------------------------
  #     Temperature effect on maintenance respiration (McCree, 1974)
  #-----------------------------------------------------------------------
  TRSFAC = 0.0
  SCLTS = 24./TS
  
  for (H in 1:TS){
    #        TRSFAC = TRSFAC + 0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2
    TRSFAC = TRSFAC + (0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2)*SCLTS
    #         scaling factor of 24/TS added on 4July2017 by Bruce Kimball
  }
  # 24 changed to TS on 3 July 2017 by Bruce Kimball
  # This equation look really suspicious because TRSFAC very 
  # dependent on number of times through the loop!
  
  #-----------------------------------------------------------------------
  #     Convert maintainence respiration to actual temperature. RES30C is
  #     the g CH2O/g DW/hr used in maintenance respiration at 30 C.
  #-----------------------------------------------------------------------
  RO = RES30C * TRSFAC
  RP = R30C2 * TRSFAC
  MAINR = RO*WTMAIN + RP*PG
  
  #-----------------------------------------------------------------------
  #RETURN
  #END ! SUBROUTINE RESPIR
  assign("RO", RO, envir = env)
  assign("RP", RP, envir = env)
  assign("MAINR", MAINR, envir = env)
}

#-----------------------------------------------------------------------
#     RESPIR variables:
#-----------------------------------------------------------------------
# MAINR   Maintenance respiration (g[CH2O] / m2 / d)
# PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
# R30C2   Respiration coefficient that depends on gross photosynthesis, 
#           value at 30C (g[CH2O] used / g[CH2O] fixed / hr)
# RES30C  Respiration coefficient that depends on total plant mass,
#           value at 30C (g CH2O/g DW/hr)
# RO      Respiration coefficient that depends on total plant mass
#           (g[CH2O] / g[tissue])
# RP      proportion of the day's photosynthesis which is respired in the 
#           maintenance process 
# TGRO(I) Hourly air temperature (?C)
# TRSFAC  Temperature effect on maintenance respiration 
# TS      Number of intermediate time steps (=24) 
# WTMAIN  Mass of tissue assumed to require maintenance (g[tissue] / m2)
#-----------------------------------------------------------------------
#     END SUBROUTINE RESPIR
#-----------------------------------------------------------------------
