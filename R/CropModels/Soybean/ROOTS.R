#=======================================================================
#  ROOTS, Subroutine, G. Hoogenboom, J.W. Jones
#-----------------------------------------------------------------------
#
#  Calculates root growth, extension, respiration, and senescence
#
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/09/1989 GH  Written.
#  09/29/1995 KJB Changed to lessen effect of water deficit on root depth
#                 increase.
#  01/19/1996 JWJ Added effects of excess water.
#  01/20/1996 KJB Increase root extension rate under drought stress
#  09/13/1998 CHP Modified for modular format
#  09/14/1998 CHP Changed TROOT to TRLV to match same variable in ROOTDM
#                 Changed SWDF1 to SWFAC to match variable name in WATBAL
#  05/11/1999 GH  Incorporated in CROPGRO
#  02/21/2005 SJR Moved ISWWAT condition here to allow computation of 
#                 root senescence even when water not simulated.
#  10/04/2005 SJR Include senescence due to water stress in total 
#                 daily senescence.
#  10/20/2005 CHP Added optional minimum root mass for senescence, 
#                 RTWTMIN, to species file
#  01/19/2006 CHP Fixed discrepancies between plant root senescence  
#                 calculated and that sent to soil routines for addition
#                 to organic matter.  
#-----------------------------------------------------------------------
#  Called by  :  PLANT
#  Calls      :  IPROOT, INROOT
#=======================================================================

simDataVars$RLV    <-  0
simDataVars$RTDEP  <-  0
simDataVars$SATFAC <-  0
simDataVars$SENRT  <-  0
simDataVars$SRDOT  <-  0

ROOTS <- function(EMERG,
                  AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #!Input
                  ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #!Input
                  SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #!Input
                  RLV, RTDEP, SATFAC, SENRT, SRDOT)  {            #!Output
  
  ROOTS <- 0
  
  #CHARACTER*1 ISWWAT
  #CHARACTER*2 CROP
  #CHARACTER*92 FILECC
  
  #TODO verificar padrão ECOSMOS
  #INTEGER L, L1, NLAYR
  #INTEGER DYNAMIC
  
  #TODO
  #PG virá da fotossintese!
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RFAC1  <- 7500.0
  RLDSM  <- 0.1
  RTSDF  <- 0.015 
  RTSEN  <- 0.020
  PORMIN <- 0.02
  RTEXF  <- 0.10
  RTDEPI <- 20.0
  XRTFAC <- c(0.00, 3.00, 6.00, 30.00)
  YRTFAC <- c(2.50, 2.50, 2.60, 2.60)
  RTWTMIN <- 0.0 #TODO ver com santiago e/ou Leandro
  
  RWMTXT <- as.character(rep(0,7)) #TODO verificar sintaxe e uso
  
  #TODO ver link com ECOSMOS
  NL       = 20  #!Maximum number of soil layers 
  DLAYR  <- rep(0, NL)
  DS     <- rep(0, NL)
  DUL    <- rep(0, NL)
  ESW    <- rep(0, NL)
  LL     <- rep(0, NL)
  RLDF   <- rep(0, NL)
  RLGRW  <- rep(0, NL)
  RLSEN  <- rep(0, NL)
  RLV    <- rep(0, NL)
  RLV_WS <- rep(0, NL)
  RRLF   <- rep(0, NL)
  SW     <- rep(0, NL)
  SAT    <- rep(0, NL)
  WR     <- rep(0, NL)
  GRESPR <- rep(0, NL)
  MRESPR <- rep(0, NL)
  RESPS  <- rep(0, NL)
  SENRT  <- rep(0, NL)

    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    SRDOT = 0.0       
    RLV   = 0.0
    RTDEP = 0.0       
    SENRT = 0.0
    SUMEX = 0.0
    SUMRL = 0.0
    SATFAC = 0.0
    
    #-----------------------------------------------------------------------
    #     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
    #-----------------------------------------------------------------------
    # if (CROP != 'FA' & ISWWAT != 'N') {
    if (CROP != 'FA') {
      RFAC2 = TABEX(YRTFAC,XRTFAC,0.0,4)
    }
    
    CumRootMass = 0.0
    
    #***********************************************************************
    #***********************************************************************
    # EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    #   Call INROOT for initialization of root variables on
    #   day of emergence.  (GROW emergence initialization
    #   must preceed call to INROOT.)
    #-----------------------------------------------------------------------
    #TODO chamar funcao
    INROOT(DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, #Input
                RLV, RTDEP)                                       #Output
    
    RFAC3 = RFAC1
    
    TRLV = 0.0
    for (L in 1:NLAYR) {
      TRLV = TRLV + RLV[L] * DLAYR[L] # cm[root] / cm2[ground]
    }
    
    CumRootMass = WTNEW * FRRT * PLTPOP * 10. 
    #  kg[root]  g[tissue] g[root]    plants   kg/ha
    #   -------- = ----- * --------- * ------ * ----- 
    #      ha      plant   g[tissue]     m2      g/m2
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #     Calculate Root Depth Rate of Increase, Physiological Day (RFAC2)
    #-----------------------------------------------------------------------
    RFAC2 = TABEX(YRTFAC, XRTFAC, VSTAGE, 4)
    RLNEW = WRDOTN * RFAC1 / 10000.
    CGRRT = AGRRT * WRDOTN
    
    #-----------------------------------------------------------------------
    #     Calculate root length per cm2 soil and initiate growth,
    #     respiration and senescence by layer
    #-----------------------------------------------------------------------
    # TRTDY = 0.0
    # 1/19/2006 Remove TRTDY and replace with TRLV -- RLV is only updated
    #     once, so yesterday's value is stored in TRLV here.
    for (L in 1:NLAYR) {
      # TRTDY = TRTDY + RLV(L) * DLAYR(L) # cm[root] / cm2[ground]
      RRLF[L]   = 0.0
      RLSEN[L]  = 0.0
      RLGRW[L]  = 0.0
      MRESPR[L] = 0.0
      GRESPR[L] = 0.0
      RESPS[L]  = 0.0
    }
    
    # Update RFAC3 based on yesterday's RTWT and TRLV
    if (RTWT - WRDOTN >= 0.0001 & TRLV >= 0.00001) {
      #  RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
      #  RTWT has not yet been updated today, so use yesterday's
      #  value and don't subtract out today's growth - chp 11/13/00
      RFAC3 = TRLV * 10000.0 / RTWT
    } else {
      RFAC3 = RFAC1
    }
    
    # 10/20/2005 Limit RLV decrease due to senscence to 
    #       a minimum resulting root weight 
    if (RTWTMIN > 0.0) {
      TRLV_MIN = RTWTMIN * RFAC3 / 1.E4   #same units as TRLV
      # cm/cm2  =  (g/m2) *(cm/g) / (cm2/m2)
    } else {
      # Set TRLV_MIN to zero -- no minimum root mass
      TRLV_MIN = 0.0
    }
    
    #-----------------------------------------------------------------------
    TRLDF  = 0.0
    CUMDEP = 0.0
    SUMEX  = 0.0
    SUMRL  = 0.0
    RLV_WS = 0.0
    RLSEN  = 0.0
    
    for (L in 1:NLAYR) {
      
      L1 = L
      CUMDEP = CUMDEP + DLAYR[L]
      SWDF = 1.0
      SWEXF = 1.0
      
      #-----------------------------------------------------------------------
      #     2/21/05 - SJR - move conditional call for water stress from CROPGRO 
      #     to ROOTS.  Allows root senescence when Water dynamics option is 
      #     turned off.  Water stress options set to no stress levels.  This 
      #     also allows output of root growth dynamics without limimiting 
      #     water or N uptake. 
      #-----------------------------------------------------------------------
      if (ISWWAT == 'Y') {
        if (SAT[L]-SW[L] < PORMIN) {
          SWEXF = (SAT[L] - SW[L]) / PORMIN
          SWEXF = min(SWEXF, 1.0)
        }
        
        SUMEX = SUMEX + DLAYR[L] * RLV[L] * (1.0 - SWEXF)
        SUMRL = SUMRL + DLAYR[L] * RLV[L]
        
        ESW[L] = DUL[L] - LL[L]
        if (SW[L] - LL[L] < 0.25*ESW[L]) {
          SWDF = (SW[L] - LL[L]) / (0.25*ESW[L])
          SWDF = max(SWDF, 0.0)
        }
      }
      #-----------------------------------------------------------------------
      
      RTSURV = min(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
      if (RLV[L] > RLDSM & TRLV + RLNEW > TRLV_MIN) {
        # 1/14/2005 CHP Don't subtract water stress senescence 
        #           yet - combine with natural senescence and check to see if 
        #           enough RLV for senescence to occur (TRLV > TRLV_MIN)
        #RLV(L) = RLV(L) * RTSURV
        RLV_WS[L] = RLV[L] * (1.0 - RTSURV)
      } else {
        RLV_WS[L] = 0.0
      }
      
      #-----------------------------------------------------------------------
      RLDF[L] = WR[L] * DLAYR[L] * min(SWDF,SWEXF)
      if (CUMDEP < RTDEP) {
        TRLDF = TRLDF + RLDF[L]
      } else {
        if (WR[L] > 0.0 & RLNEW > 0.0) {
          if (L == 1) {
            RTDEP = RTDEP + DTX * RFAC2
          } else {
            RTDEP = RTDEP + DTX * RFAC2 * min(SWDF,SWEXF) * (1. + 0.25 * (1. - max(SWFAC,0.40)))
            #-----------------------------------------------------------------------
            #-KJB  DO NOT WANT TO DECREASE ROOT DEPTH WITH STRESS.  IF PG TO ROOTS
            # IS LOW BECAUSE OF SEED GROWTH OR IF WATER DEFICIT CAUSES LOW PG TO ROOTS
            # DESPITE INCREASED PARTITIONING TO ROOTS, { RLV WILL NOT INCREASE
            # SO THERE WILL BE NO EFFECTIVE INCREASE IN WATER EXTRACTION.  IDEALLY THE
            # DECISION SHOULD BE BASED ON AMOUNT OF ROOT GROWTH VS NORMAL UNSTRESS.
            # ACCELERATE FROM 1.0 TO 0.5, STAY FLAT, SHOULD DROP AGAIN, 0.5 TO 0.0
            # AS THE OTHER FUNCTION ACTS.  NOTE:  SWFAC*2.0 WAS NOT USED IN ALL 40 CASES
            # EXCEPT 1985-RAINFED WHERE DELETING INCREASED YIELD 2764 TO 2770 KG/HA.
            # NOW ACCELERATING ROOT GROWTH BY ABOUT 12-13% AT SWFA!=0.50.  THIS
            # HELPS IOWA 88 AND VEG STRESS TRTS IN 1981 AND 1985. INCR SEED AND BIO.
            #-----------------------------------------------------------------------
          }
          if (RTDEP > DEPMAX) {
            RTDEP = DEPMAX
          }
        }
        RLDF[L] = RLDF[L] * (1. - (CUMDEP - RTDEP) / DLAYR[L])
        TRLDF = TRLDF + RLDF[L]
        
        # ALTERADO: GOTO to break()
        # GO TO 2900 #TODO
        break
      }
    }
    #-----------------------------------------------------------------------
    #     Calculate root senescence, growth, maintenance and growth
    #     respiration, and update root length density for each layer.
    #-----------------------------------------------------------------------
    
    if (SUMRL > 0.0) {
      SATFAC = SUMEX/SUMRL
    } else {
      SATFAC = 0.0
    }
    
    SRDOT = 0.0
    RLSENTOT = 0.0
    
    for (L in 1:L1) {
      if (TRLDF < 0.00001) {
        RRLF[L] = 1.0
      } else {
        RRLF[L] = RLDF[L]/TRLDF
      }
      #-----------------------------------------------------------------------
      #       MRESPR, GRESPR, and RESPS are not used anywhere
      #                       chp 9/22/98
      #-----------------------------------------------------------------------
      MRESPR[L] = (RLV[L]/RFAC1*RO*DLAYR[L]*100.0 +RRLF[L]*FRRT*PG*RP) * 44.0 / 30.0
      GRESPR[L] = RRLF[L] * (CGRRT-WRDOTN) * 44.0 /30.0
      RESPS[L] = MRESPR[L] + GRESPR[L]
      #-----------------------------------------------------------------------
      RLGRW[L] = RRLF[L] * RLNEW / DLAYR[L] #cm[root]/cm3[ground]
      
      if (TRLV + RLNEW > TRLV_MIN) {
        RLSEN[L] = RLV[L] * RTSEN * DTX
      } else {
        RLSEN[L] = 0.0
      }
      
      # Limit total senescence in each layer to existing RLV
      if (RLV[L] - RLSEN[L] - RLV_WS[L] + RLGRW[L] < 0.0) {
        RLSEN[L] = RLV[L] + RLGRW[L] - RLV_WS[L]
      } 
      
      # RLSENTOT is profile senescence, water stress and natural cm/cm2
      RLSENTOT = RLSENTOT + (RLSEN[L] + RLV_WS[L]) * DLAYR[L]
    }
    
    # If senescence too high (results in TRLV < TRLV_MIN) then
    # reduce senescence in each layer by factor.
    if (RLSENTOT > 1.E-6 & TRLV + RLNEW - RLSENTOT < TRLV_MIN){
      FACTOR = (TRLV + RLNEW - TRLV_MIN) / RLSENTOT
      FACTOR = max(0.0, min(1.0, FACTOR))
      RLSEN  = RLSEN  * FACTOR
      RLV_WS = RLV_WS * FACTOR
    }
    
    # Update RLV and TRLV based on today's growth and senescence
    TRLV = 0.0
    for (L in 1:NLAYR) {
      RLV[L] = RLV[L] + RLGRW[L] - RLSEN[L] - RLV_WS[L]
      TRLV = TRLV + RLV[L] * DLAYR[L]
      
      # Keep senescence in each layer for adding C and N to soil
      #SENRT(L) = RLSEN(L) * DLAYR(L) / RFAC1 * 10000. * 10. #kg/ha
      # 1/14/2005 CHP - water stress senesence needs to be inlcuded.
      SENRT[L] = (RLSEN[L] + RLV_WS[L]) * DLAYR[L] / RFAC3 * 1.E5 
      #           cm[root]              g[root]   1000 cm2   10(kg/ha)
      # kg/ha  =  -------- * cm[soil] * ------- * -------- * ---------
      #          cm3[soil]             cm[root]     m2         (g/m2)
      
      SENRT[L] = max(SENRT[L], 0.0)
      SRDOT = SRDOT + SENRT[L]/10.        #g/m2
      
      # Not used:
      #TRLGRW = TRLGRW + RLGRW(L) * DLAYR(L)
      #TRLSEN = TRLSEN + RLSEN(L) * DLAYR(L)
    }
    
    # 11/13/2000 CHP Sum RLSEN for total root senescence today.  
    # SRDOT = TRLSEN / RFAC3 * 10000.     #g/m2
    
    # Total root senescence = water stress + natural senescence
    # 10/3/2005 SJR
    # SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3    #g/m2
    SRDOT = max(SRDOT, 0.0)
    
    TotRootMass = TRLV / RFAC3 * 1.E5
    #          cm[root]   g[root]   10000 cm2   10(kg/ha)
    # kg/ha  = -------- * ------- * -------- * ---------
    #         cm2[soil]   cm[root]     m2         (g/m2)
    
    CumRootMass = CumRootMass + WRDOTN * 10. - SRDOT * 10. 
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #RETURN
  #END SUBROUTINE ROOTS
  assign("RLV", RLV, envir = env)
  assign("RTDEP", RTDEP, envir = env)
  assign("SATFAC", SATFAC, envir = env)
  assign("SENRT", SENRT, envir = env)
  assign("SRDOT", SRDOT, envir = env)
  
  return()
}
#=======================================================================

#=======================================================================
#  INROOT Subroutine
#  Initializes root variables at emergence.
#----------------------------------------------------------------------
#  REVISION HISTORY
#  04/01/91 GH  Adapted for CROPGRO
#  06/17/98 CHP Modified for modular format
#  05/11/99 GH  Incorporated in CROPGRO
#-----------------------------------------------------------------------
#  Called : CROPGRO
#  Calls  : None
#=======================================================================
     # SUBROUTINE INROOT(
     #&  DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, #Input
     #&  RLV, RTDEP)                                       #Output
      
      #TODO verificar se atribuição aqui é necessária!
      simDataVars$RLV  <-  0
      simDataVars$RTDEP  <-  0
      
      INROOT <- function (
        DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, #Input
        RLV, RTDEP) {                                      #Output
        
        INROOT <- 0
        NL       = 20  #!Maximum number of soil layers 

        #INTEGER L
        #TODO adequar ao padrão ECOSMOS
        NLAYR <- 20
        
        #!*ROOT PARAMETERS
        RTDEPI  <- 20.0
        #RFAC1 VERIFICAR: já chamado na ROOTS.for
        
        #TODO adequar ao padrão ECOSMOS
        RLV   <- rep(0, NL)
        DLAYR <- rep(0, NL)
        
        #***********************************************************************
        #     INITIALIZE ROOT DEPTH AT EMERGENCE
        #-----------------------------------------------------------------------
        RTDEP = RTDEPI
        #-----------------------------------------------------------------------
        #     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
        #     RTDEPTI (ROOT DEPTH AT EMERGENCE)
        #-----------------------------------------------------------------------
        CUMDEP = 0.
        
        for (L in 1:NLAYR) {
          RLV[L] = 0.0
        }
        
        for (L in 1:NLAYR) {
          DEP = min(RTDEP - CUMDEP, DLAYR[L])
          RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP * 10000 )
          #        cm[root]   g[root]    plants  cm[root]   m2
          #        -------- = -------- * ------ * ------- * ---
          #      cm2[ground]   plant       m2     g[root]   cm2
          
          CUMDEP = CUMDEP + DEP
          RLV[L] = RLINIT / DLAYR[L]
          if (CUMDEP >= RTDEP) {
            break
            #GO TO 300
          }
        }
        
        #***********************************************************************
        #RETURN
        #END SUBROUTINE INROOT
        assign("RLV", RLV, envir = env)
        assign("RTDEP", RTDEP, envir = env)
        
        return()
      }
#=======================================================================

#-----------------------------------------------------------------------
#       Variable definitions
#-----------------------------------------------------------------------
# AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
# CGRRT     Carbon demand for new root growth (g[CH2O] / m2 / d)
# CROP      Crop identification code 
# CUMDEP    Cumulative depth of soil profile (cm)
# DEP       Cumulative soil depth (cm)
# DEPMAX    Maximum depth of reported soil layers (cm)
# DLAYR(L)  Soil thickness in layer L (cm)
# DS(L)     Cumulative depth in soil layer L (cm)
# DTX       Thermal time that occurs in a real day based on vegetative 
#             development temperature function (thermal days / day)
# DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
#             layer L (cm3 [H2O] /cm3 [soil])
# ESW(L)    Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
# FILECC    Path plus filename for species file (*.spe) 
# FRRT      Fraction of vegetative tissue growth that goes to roots on a 
#             day (g[root] / g[veg])
# GRESPR(L) Growth respiration for new root growth in layer L 
# LL(L)     Volumetric soil water content in soil layer L at lower limit
#             ( cm3/cm3)
# LUNCRP    Logical unit number for FILEC (*.spe file) 
# LUNIO     Logical unit number for FILEIO 
# MRESPR(L) Maintenance respiration for new root growth in layer L 
# NL        Maximum number of soil layers = 20 
# NLAYR     Number of soil layers 
# PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
# PLTPOP    Plant population (# plants / m2)
# PORMIN    Minimum pore space required for supplying oxygen to roots for 
#             optimal growth and function (cm3/cm3)
# RESPS(L)  Total respiration for new root growth in layer L 
# RFAC1     Root length per unit  root weight. (cm/g)
# RFAC2     Root depth increase rate with time (cm / physiol. day)
# RFAC3     Ratio of root length to root weight at the current time (cm/g)
# RLDF(L)   Combined weighting factor to determine root growth distribution
# RLDSM     Minimum root length density in a given layer, below which 
#             drought-induced senescence is not allowed.
#             (cm [root ]/ cm3 [soil])
# RLGRW(L)  Incremental root length density in soil layer L
#             (cm[root] / cm3[soil])
# RLINIT    Initial root density (cm[root]/cm2[ground])
# RLNEW     New root growth added (cm[root]/cm2[ground]/d)
# RLSEN(L)  Root length density senesced today (cm[root]/ cm3[soil])
# RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# RO        Respiration coefficient that depends on total plant mass
#             (g[CH2O] / g[tissue])
# RP        proportion of the day's photosynthesis which is respired in the 
#             maintenance process 
# RRLF(L)   Root length density factor ratio (RLDF(L) / TRLDF) 
# RTDEP     Root depth (cm)
# RTDEPI    Depth of roots on day of plant emergence. (cm)
# RTEXF     Fraction root death per day under oxygen depleted soil 
# RTSDF     Maximum fraction of root length senesced in a given layer per 
#             physiological day when water content in a given layer falls 
#             below 25 % of extractable soil water. 
# RTSEN     Fraction of existing root length which can be senesced per 
#             physiological day. (fraction / ptd)
# RTSURV(L) Fraction survival of roots on a given day, taking into account 
#             death due to excess or deficit water conditions 
# RTWT      Dry mass of root tissue, including C and N
#             (g[root] / m2[ground])
# SAT(L)    Volumetric soil water content in layer L at saturation
#             (cm3 [water] / cm3 [soil])
# SRDOT     Daily root senescence (g / m2 / d)
# SW(L)     Volumetric soil water content in layer L
#             (cm3 [water] / cm3 [soil])
# SWDF      Soil water deficit factor for layer with deepest roots (0-1) 
# SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
# SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
#             0.0=max stress 
# TABEX     Function subroutine - Lookup utility 
# TRLDF     Total root length density factor for root depth (cm)
# TRLGRW    Total new root length density in soil layer L
#             (cm[root] / cm2[soil])
# TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
# TRLV      Total root length per square cm soil today 
#             (cm[root]/cm2[soil])
# TRTDY     Total root length per square cm soil yesterday 
#             (cm[root]/cm2[soil])
# VSTAGE    Number of nodes on main stem of plant 
# WR(L)     Root hospitality factor, used to compute root growth 
# WRDOTN    Dry weight growth rate of new root tissue including N but not C 
#             reserves (g[root] / m2[ground]-d)
# WTNEW     Initial mass of seedling or seed (g / plant)
# XRTFAC(I) V-stage at which rate of increase in root depth per 
#             physiological day is YRTFAC(I). (# leaf nodes)
# YRTFAC(I) Rate of increase in root depth per degree day at V-stage 
#             XRTFAC(I). (cm / (physiol. day))
#***********************************************************************
#      END SUBROUTINES ROOTS, IPROOT, and INROOT
#=======================================================================
