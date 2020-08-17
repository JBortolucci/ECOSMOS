#=======================================================================
#  CANOPY, Subroutine, G. Hoogenboom, K.J. Boote, J.W. Jones
#  Calculates canopy height and canopy width as a function of V-Stage,
#  air temperature, drought stress, daylength, and radiation.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  05/01/1989 Written.
#  04/24/1994 NBP Changed TAIRHR to TGRO.  Edited indentation.
#  01/19/1996 KJB Include PAR effect on expansion.
#  07/15/1998 CHP Modified for modular format
#  05/15/1999 GH  Incorporated into CROPGRO
#  01/22/2003 KJB Add checks for minimum canopy height and width.
#  08/12/2003 CHP Revised I/O error checking
#  06/30/2004 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
#                 Added optional KCAN to ECO file.
#-----------------------------------------------------------------------
#  Called : VEGGR
#  Calls  : ERROR, FIND, IGNORE
#========================================================================

simDataVars$CANHT  <-  0
simDataVars$CANWH  <-  0

CANOPY <- function (DYNAMIC, 
                    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
                    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,             #Input
                    CANHT, CANWH) {                                    #Output
  
  #-----------------------------------------------------------------------
  CANOPY <- 0
  
  #TODO verificar padrão ECOSMOS
  #INTEGER DYNAMIC
  
  ROWSPC <- 0.50 #TODO: ARQUIVO DE MANEJO ROWSPC -> Row spacing (m)
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  RHGHT  <- 0.9
  RWIDTH <- 1.0 #RWDTH no .ECO
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN   <- 0.67
  #!*CANOPY HEIGHT AND WIDTH GROWTH PARAMETERS
  # TODO verificar se são 5, 8 (.SPE) ou 10 (.for) posições no vetor 
  XHWPAR  <- c(0.00,  5.00,  7.50, 10.00, 15.00, 20.00, 30.00, 80.00)
  YHWPAR  <- c(4.00,  2.00,  1.50,  1.25,  1.05,  1.00,  1.00,  1.00)
  XHWTEM  <- c(-50.0,  00.0,  15.0,  26.0,  60.0)
  YHWTEM  <- c(0.40,  0.40,  0.50,  1.00,  1.00)
  XVSHT   <- c(0.00,  1.00,  4.00,  6.00,  8.00, 10.00, 14.00, 16.00, 20.00, 40.00)
  YVSHT   <- c(0.0300, 0.0530, 0.0630, 0.0660, 0.0690, 0.0660, 0.0620, 0.0510, 0.0340, 0.0060)
  YVSWH   <- c(0.0300, 0.0510, 0.0620, 0.0640, 0.0660, 0.0630, 0.0590, 0.0460, 0.0250, 0.0010)
  #fim dos parametros de planta
  
  #TODO: ver conexao com ECOSMOS.. provavel que virá do 'env'
  TS <- 24
  TGRO <- rep(0, TS)
  
  #***********************************************************************
  #***********************************************************************
  #     SEASONAL INITIALIZATION 
  #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    CANHT = 0.0
    CANWH = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if  (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    CANHT  = TABEX(YVSHT,XVSHT,VSTAGE,10)       
    CANWH  = TABEX(YVSWH,XVSHT,VSTAGE,10)       
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if  (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #     Calculate effect of temperature on canopy expansion, HWTEM
    #-----------------------------------------------------------------------
    HWTEM = 0.0
    for (I in 1:TS) {
      HWTEM = HWTEM + TABEX(YHWTEM,XHWTEM,TGRO(I),5)
    }
    HWTEM = HWTEM /TS
    #       24 changed to TS on 5 July 2017 by Bruce Kimball
    
    #-----------------------------------------------------------------------
    #     Calculate effect of day's PAR on canopy expansion, HPAR.
    #     ASSUME THAT UPPER 30% OF CANOPY SHADES THE GROWING POINT
    #     WPAR IS EFFECT ON WIDTH.  SHADE DOES NOT MAKE MUCH WIDER. LOWER K?
    #-----------------------------------------------------------------------
    #     IF (XLAI .GT. 0.1) THEN
    #        PARNOD = PAR * EXP(-KCAN*0.3*(XLAI-0.1))
    #     ELSE
    #        PARNOD = PAR
    #     }
    #-----------------------------------------------------------------------
    PARNOD = PAR * exp(-KCAN*(0.3*XLAI))
    HPAR = TABEX(YHWPAR,XHWPAR,PARNOD,8)
    WPAR = TABEX(YHWPAR,XHWPAR,PAR,8)
    #-----------------------------------------------------------------------
    #     Calculate rate of increase in canopy height and update height, CANHT
    #-----------------------------------------------------------------------
    RCANHT= RVSTGE * TABEX(YVSHT,XVSHT,VSTAGE,10) * HWTEM * TURFAC * HPAR * RHGHT
    CANHT = CANHT + RCANHT
    
    #     Set minimum Canopy height based on lookup function
    CANHT = max(CANHT, TABEX(YVSHT,XVSHT, 0.0, 10))
    
    #-----------------------------------------------------------------------
    #     Calculate rate of increase in canopy width and update width, CANWH
    #     RWIDTH,RHGHT are used to normalize other crops to the values in tables
    #     Values of RHGHT and RWIDTH = 1.00 are for Florunner peanut variety
    #     1/22/03 KJB - Don't allow reduction in vstage to reduce canopy
    #       width.
    #-----------------------------------------------------------------------
    RCANWH = max(0.0,RVSTGE) * TABEX(YVSWH,XVSHT,VSTAGE,10) * HWTEM * TURFAC * WPAR * RWIDTH
    CANWH  = CANWH + RCANWH
    
    #     Set minimum Canopy width based on lookup function
    CANWH = max(CANWH, TABEX(YVSWH, XVSHT, 0.0, 10))  
    CANWH = min(CANWH,ROWSPC)
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("CANHT", CANHT, envir = env)
  assign("CANWH", CANWH, envir = env)
  
  return()
  #RETURN
  #END # SUBROUTINE CANOPY
}

#=======================================================================
# CANOPY Definitions:  updated 25 Feb 2004
#-----------------------------------------------------------------
# C255      255-character record read from file 
# CANHT     Canopy height (m)
# CANWH     Canopy width normal to row (m)
# ECONO     Ecotype code - used to match ECOTYP in .ECO file 
# ECOTYP    Ecotype code for this simulation 
# ERR       Error code for file operation 
# FILECC    Path plus filename for species file (*.spe) 
# FILEGC    Pathname plus filename for ECO file 
# FOUND     Indicator that good data was read from file by subroutine FIND 
#             (0 - End-of-file encountered, 1 - NAME was found) 
# HPAR      Effect of day's PAR on canopy expansion 
# HWTEM     Effect of temperature on canopy expansion 
# ISECT     Indicator of completion of IGNORE routine: 0 - End of file 
#             encountered, 1 - Found a good line to read, 2 - End of 
#             Section in file encountered denoted by * in column 1. 
# KCAN      Canopy light extinction coefficient for daily PAR, for 
#             equidistant plant spacing, modified when in-row and between 
#             row spacing are not equal 
# LINC      Line number of input file 
# LNUM      Current line number of input file 
# LUNCRP    Logical unit number for FILEC (*.spe file) 
# LUNECO    Logical unit number for FILEE (*.eco file) 
# PAR       Daily photosynthetically active radiation or photon flux 
#             density (moles[quanta]/m2-d)
# PARNOD    Effective PAR at growth point (moles[quanta]/m2-d)
# RCANHT    Rate of increase in canopy height (m/d)
# RCANWH    Rate of increase in canopy width (m/d)
# RHGHT     Relative height of this ecotype in comparison to the standard 
#             height per node (YVSHT) defined in the species file (*.SPE) 
# ROWSPC    Row spacing (m)
# RVSTGE    Rate of VSTAGE change (nodes/day)
# RWIDTH    Relative width of this ecotype in comparison to the standard 
#             width per node (YVSWH) defined in the species file (*.SPE) (m)
# SECTION   Section name in input file 
# TGRO(I)   Hourly canopy temperature (Â°C)
# TURFAC    Water stress factor for expansion (0 - 1) 
# VSTAGE    Number of nodes on main stem of plant (nodes)
# WPAR      Effect of PAR on canopy width 
# XHWPAR(I) PAR values for table look-up for modifying height and width 
#             growth rate, particularly to allow etiliolation at low PAR 
#             values (mol/day)
# XHWTEM    Temperatures in a table look-up function for modifying height 
#             and width growth rates (c°C)
# XLAI      Leaf area (one side) per unit of ground area
#            (m2[leaf] / m2[ground])
# XVSHT     Node number on main stem for use in computing height and width 
#             growth rates 
# YHWPAR(I) Relative increase in height and width growth rates with low PAR 
#             as given in XHWPAR 
# YHWTEM(I) Relative (0-1) expansion in height and width with temperatures 
#             given in XHWTEM 
# YVSHT     Length of internode (m) Vs position on the main stem defined by 
#             XVSHT (m/node)
# YVSWH     Increase in canopy width per node developed on the main stem
#            (m/node)
#***********************************************************************
#      END SUBROUTINE CANOPY
#=======================================================================