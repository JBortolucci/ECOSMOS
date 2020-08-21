#=======================================================================
#  VEGGR, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
#  Calculates vegetative partitioning as a function of V stage,
#  and limits growth prior to VSINK.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1989     Written.
#  04/24/1994 NBP Changed TAIRHR to TGRO.
#  09/26/1995 KJB Added shift in partitioning from stem to leaf during
#                 and especially after drought stress.
#  01/19/1996 KBJ & JWJ  Idea that we should allow shift in partitioning
#                 from stem to leaf under cold stress and possibly low
#                 light stress in similar manner.  Non-optimum temp
#                 causes more DM to leaf in SPAR experiment.  What
#                 about root:shoot?  Do these later.
#  02/15/1996 KJB Allow excess (=PGLEFT) to influence PG on next day
#  06/18/1998 CHP Modified for modular format
#  05/10/1999 GH  Incorporated in CROPGRO
#  06/21/2001 GH  Modified seasonal initialization
#  08/12/2003 CHP Added I/O error checking
#  07/13/2006 CHP Added P model
#  06/11/2007 CHP PStres2 affects growth
#-----------------------------------------------------------------------
#  Called by: PLANT
#  Calls:     CANOPY
#             ERROR, FIND, IGNORE
#========================================================================

simDataVars$AGRVG  <-  0 #TODO: VERIFICAR I/O
simDataVars$FRLF  <-  0  #TODO: VERIFICAR I/O
simDataVars$FRRT  <-  0  #TODO: VERIFICAR I/O
simDataVars$FRSTM  <-  0 #TODO: VERIFICAR I/O
simDataVars$CADLF  <-  0
simDataVars$CADST  <-  0
simDataVars$CANHT  <-  0
simDataVars$CANWH  <-  0
simDataVars$CMINEA  <-  0
simDataVars$CRUSLF  <-  0
simDataVars$CRUSRT  <-  0
simDataVars$CRUSSH  <-  0
simDataVars$CRUSST  <-  0
simDataVars$EXCESS  <-  0
simDataVars$NADLF  <-  0
simDataVars$NADRT  <-  0
simDataVars$NADST  <-  0
simDataVars$NGRLF  <-  0
simDataVars$NGRRT  <-  0
simDataVars$NGRST  <-  0
simDataVars$NSTRES  <-  0
simDataVars$TNLEAK  <-  0
simDataVars$WLDOTN  <-  0
simDataVars$WRDOTN  <-  0
simDataVars$WSDOTN  <-  0

#canopy
simDataVars$CANHT  <-  0
simDataVars$CANWH  <-  0

VEGGR <- function(EMERG, #DINAMYC no original
                  AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #!Input
                  DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #!Input
                  FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #!Input
                  NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #!Input
                  PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #!Input
                  TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #!Input
                  WCRST, WTLF, XLAI, YRDOY, YREMRG,               #!Input
                  AGRVG, FRLF, FRRT, FRSTM)      {                #!I/O
                  # CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #!Output
                  # CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #!Output
                  # NADST, NGRLF, NGRRT, NGRST, NSTRES,             #!Output
                  # TNLEAK, WLDOTN, WRDOTN, WSDOTN)      {          #!Output
  
  #-----------------------------------------------------------------------
  
  #TODO: verificar padrão ECOSMOS -> DYNAMIC
  #TODO: verificar padrão ECOSMOS -> YRDOY, YREMRG, NR1, DAS

  ROWSPC <- 0.50 #TODO: ARQUIVO DE MANEJO ROWSPC -> Row spacing (m)
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN   <- 0.67
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CMOBMX <- 0.024
  #!*RESPIRATION PARAMETERS
  PCH2O  <- 1.13
  #!*PLANT COMPOSITION VALUES
  PROLFI <- 0.356 
  PRORTI <- 0.092
  PROSTI <- 0.150
  PROLFG <- 0.285
  PRORTG <- 0.064
  PROSTG <- 0.100
  #!*VEGETATIVE PARTITIONING PARAMETERS
  ATOP   <- 1.00
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CADSTF <- 0.75
  #*NITROGEN STRESS PARAMETERS
  NRATIO <- 1.00

  #TODO: ver conexao com ECOSMOS.. provavel que virá do 'env'
  TS <- 24
  TGRO <- rep(0, TS)
  
  #TODO checar conexão no ECOSMOS
  #DAS = CONTROL % DAS
  
  #-----------------------------------------------------------------------
  #    Call CANOPY for input
  #-----------------------------------------------------------------------
  CANOPY(RUNINIT,
         ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
         RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,             #Input
         CANHT, CANWH)                                   #Output
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    CADLF  = 0.0  
    CADST  = 0.0  
    CMINEA = 0.0  
    CRUSLF = 0.0  
    CRUSRT = 0.0  
    CRUSST = 0.0  
    CUMTUR = 1.0  
    EXCESS = 1.0  
    FNINLG = 0.0  
    FNINRG = 0.0  
    FNINSG = 0.0  
    NADLF  = 0.0  
    NADRT  = 0.0  
    NADST  = 0.0  
    NGRLF  = 0.0  
    NGRRT  = 0.0  
    NGRST  = 0.0  
    NSTRES = 1.0  
    PGLEFT = 0.0
    SUPPN  = 0.0
    TNLEAK = 0.0  
    VGRDEM = 0.0
    WLDOTN = 0.0  
    WRDOTN = 0.0  
    WSDOTN = 0.0  
    
    CANOPY(SEASINIT,
           ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
           RVSTGE, TGRO, TURFAC, VSTAGE, XLAI)             #Input
           # CANHT, CANWH)                                   #Output
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    FNINLG = PROLFG * 0.16   
    FNINRG = PRORTG * 0.16   
    FNINSG = PROSTG * 0.16   
    CUMTUR = 1.0             
    
    CANOPY(EMERG,
           ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
           RVSTGE, TGRO, TURFAC, VSTAGE, XLAI)             #Input
           # CANHT, CANWH)                                   #Output
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #     Partitioning is modified by water stress and nitrogen stress
    #-----------------------------------------------------------------------
    SUPPN = NFIXN + TRNU + NMINEA
    #    chp added check for YRDOY = YREMRG, but on the next day, it still
    #     shows N stress because there is little supply.  Force a lag time?
    #      if (SUPPN < 0.70 * NDMNEW & NDMNEW > 0.) {
    if (SUPPN < 0.70 * NDMNEW & NDMNEW > 0. & YRDOY != YREMRG) {
      NSTRES = min(1.0,SUPPN/(NDMNEW * 0.70))
    } else {
      NSTRES = 1.0
    }
    #      FRRT  = ATOP * (1.0 - (min(TURFAC,NSTRES)))*(1.0-FRRT) + FRRT
    FRRT  = ATOP * (1.0 - (min(TURFAC, NSTRES, PStres2))) * (1.0 - FRRT) + FRRT
    #-----------------------------------------------------------------------
    #     Cumulative turgor factor that remembers veg drought stress
    #     to shift partitioning between leaf and stem toward leaf,
    #     especially after drought is released.
    #     Sort of 20-day rolling average
    #-----------------------------------------------------------------------
    CUMTUR = 0.95*CUMTUR + 0.05*TURFAC
    if (CUMTUR < 1.E-7) {CUMTUR = 0.0}    #prevent underflow
    #-----------------------------------------------------------------------
    #     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
    #     0.7 appears to be too much for peanut, but not for soybean.
    #-----------------------------------------------------------------------
    FRLF  = (1.0 + 0.6*(1.0-CUMTUR))*(1.-FRRT)*FRLF/(FRLF + FRSTM)
    FRLF = min(FRLF, 0.90*(1. - FRRT))
    FRSTM = 1.0 - FRRT - FRLF
    #-----------------------------------------------------------------------
    #     To prevent negative partitioning to root and limit leaf plus
    #     stem to a maximum of 98 % of the vegetative partitioning
    #-----------------------------------------------------------------------
    FRLF  = min(FRLF,FRLF*0.98/(max(0.001,FRLF+FRSTM)))
    FRSTM = min(FRSTM,FRSTM*0.98/(max(0.001,FRLF+FRSTM)))
    FRRT  = 1.0 - FRLF - FRSTM
    #-----------------------------------------------------------------------
    #     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
    #-----------------------------------------------------------------------
    AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
    #-----------------------------------------------------------------------
    #     Calculate New Growth Rate of Leaves, Stems, and Roots
    #-----------------------------------------------------------------------
    VGRDEM = PGAVL / AGRVG
    WLDOTN = FRLF * VGRDEM
    WSDOTN = FRSTM * VGRDEM
    WRDOTN = FRRT * VGRDEM
    #-----------------------------------------------------------------------
    #     Compute maximum N required for tissue growth
    #-----------------------------------------------------------------------
    NGRLF  = WLDOTN * FNINL
    NGRST  = WSDOTN * FNINS
    NGRRT  = WRDOTN * FNINR
    NGRVEG = NGRLF + NGRST + NGRRT
    #-----------------------------------------------------------------------
    #     Compute minimum N required for tissue growth
    #-----------------------------------------------------------------------
    NGRLFG = WLDOTN * FNINLG
    NGRSTG = WSDOTN * FNINSG
    NGRRTG = WRDOTN * FNINRG
    NGRVGG = NGRLFG + NGRSTG + NGRRTG
    
    NRATIO = 1.0
    if (NAVL < NGRVGG) {
      #-----------------------------------------------------------------------
      #     Compute ratio for reducing leaf growth to prevent N conc of
      #       new tissue from being below the minimum for growth
      #-----------------------------------------------------------------------
      if (NGRVGG > 0.0) {
        NRATIO = NAVL / NGRVGG
        WLDOTN = WLDOTN * NRATIO
        WSDOTN = WSDOTN * NRATIO
        WRDOTN = WRDOTN * NRATIO
        NGRLF  = NGRLFG * NRATIO
        NGRST  = NGRSTG * NRATIO
        NGRRT  = NGRRTG * NRATIO
        
        #-----------------------------------------------------------------------
        #     Adjust conversion costs to account for composition of tissue at
        #       lower N concentration
        #-----------------------------------------------------------------------
        AGRVG = AGRLF * FRLF * (1.0 - (PROLFG - PROLFI)/(1.0 - PROLFI) )+ AGRRT * FRRT * (1.0 - (PRORTG - PRORTI)/(1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI)/ (1.0 - PROSTI))
      }
    } else {
      #-----------------------------------------------------------------------
      #     NAVL IS between lower and maximum N limit in this case,
      #       leaf expansion occurs as normal, but N concentration is reduced
      #-----------------------------------------------------------------------
      if (NGRVEG > 0.0 & NAVL < NGRVEG) {
        NGRLF = min(NAVL * NGRLF / NGRVEG, NGRLF)
        NGRST = min(NAVL * NGRST / NGRVEG, NGRST)
        NGRRT = min(NAVL * NGRRT / NGRVEG, NGRRT)
      }
      #-----------------------------------------------------------------------
      #     Compute protein fraction of new vegetative tissue growth
      #-----------------------------------------------------------------------
      if (WLDOTN > 0.0) {
        PROLFT = NGRLF * (100./16.)/WLDOTN
      } else {
        PROLFT = 0.0
      }
      if (WSDOTN > 0.0) {
        PROSTT = NGRST * (100./16.)/WSDOTN
      } else {
        PROSTT = 0.0
      }
      if (WRDOTN > 0.0) {
        PRORTT = NGRRT * (100./16.)/WRDOTN
      } else {
        PRORTT = 0.0
      }
      #-----------------------------------------------------------------------
      #     Recompute respiration costs if expansion occurs at low N-conc.,
      #       allow N dilution during growth of leaves, stems, and roots
      #-----------------------------------------------------------------------
      AGRVG = AGRLF * FRLF * (1.0 - (PROLFT - PROLFI)/ (1.0-PROLFI)) + AGRRT * FRRT * (1.0 - (PRORTT - PRORTI)/ (1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTT - PROSTI)/(1.0 - PROSTI))
    }
    #-----------------------------------------------------------------------
    #     Compute C and N remaining to add to reserves
    #-----------------------------------------------------------------------
    PGLEFT = max(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN) * AGRVG))
    if (PGLEFT < 1.E-5) {PGLEFT = 0.0}
    #-----------------------------------------------------------------------
    #     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
    #     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
    #     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
    #     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
    #     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
    #     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
    #     value 1.04 -0.04 also can not be greater than 1.0 or we get
    #     stimulation of photosynthesis and the sq root works differently.
    #-----------------------------------------------------------------------
    if (PG > 0.0001 & PGLEFT > 0.00001) {
      EXCESS =  (1.20 - min(1.0, max(PGLEFT/PG,0.20)) )^0.5
    } else {
      EXCESS = 1.00
    }
    
    CADST = 0.0
    CADLF = 0.0
    CMINEA = 0.0
    CRUSLF = 0.0
    CRUSST = 0.0
    CRUSRT = 0.0
    CRUSSH = 0.0
    #-----------------------------------------------------------------------
    #    Calculate Increase in Remobilizable C due to N shortage and
    #      add to Carbon Pool.  Distribute to Leaves and Stems.
    #-----------------------------------------------------------------------
    #    Want half as much accumulation in stem in veg phae
    #-----------------------------------------------------------------------
    if (DAS < NR1) {
      LSTR = (1.-0.6*CADSTF)/(0.6*CADSTF)
    } else {
      LSTR = (1.-CADSTF)/CADSTF
    }
    if (STMWT+WTLF > 0.0) {
      LSTR = LSTR * WTLF/(STMWT+WTLF*LSTR)
    }
    if (PGLEFT >= CMINEP) {
      CADLF = (PGLEFT-CMINEP)/PCH2O * LSTR
      CADST = (PGLEFT-CMINEP) * (1. - LSTR) / PCH2O
    } else {
      
      #-----------------------------------------------------------------------
      #    Calculate actual C used (CMINEA) , compute how much is taken
      #    from LF, ST, RT, and SH, which may be less than orig calc of CMINEP
      #
      #    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
      #    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
      #    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
      #
      #-----------------------------------------------------------------------
      if (CMINEP > 0) {
        CMINEA = CMINEP - PGLEFT
        CRUSLF = CMINEA / CMINEP * CMOBMX * WCRLF * (DTX + DXR57)
        CRUSST = CMINEA / CMINEP * CMOBMX * WCRST * (DTX + DXR57)
        CRUSRT = CMINEA / CMINEP * CMOBMX * WCRRT * (DTX + DXR57)
        CRUSSH = CMINEA / CMINEP * CMOBMX * WCRSH * (DTX + DXR57)
      }
    }
    CADLF = CADLF + CSAVEV/PCH2O * LSTR
    CADST = CADST + CSAVEV * (1. - LSTR)/PCH2O
    
    #-----------------------------------------------------------------------
    #    Calculate Increase in Remobilizable N Due to a C shortage,
    #      add to Nitrogen pool
    #-----------------------------------------------------------------------
    NLEFT  = max(0.0,NAVL  -  (NGRLF  + NGRST  + NGRRT))
    
    if (NLEFT > 0.0) {
      if (NLEFT > NDMOLD) {
        NLEAK  = NLEFT  - NDMOLD
        TNLEAK = TNLEAK + NLEAK
        NLEFT  = NLEFT  - NLEAK
      } else {
        NLEAK = 0.0
      }
      NADRAT = NLEFT / (FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR)
      NADLF  = NADRAT * FRLF * FNINL
      NADST  = NADRAT * FRSTM * FNINS
      NADRT  = NADRAT * FRRT * FNINR
    } else {
      NADRAT = 0.0
      NADST  = 0.0
      NADLF  = 0.0
      NADRT  = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Subroutine CANOPY calculates height and width of the canopy as a
    #     function of VSTAGE, air temperature, drought stress (TURFAC),
    #     daylenght and radiation (PAR).
    #-----------------------------------------------------------------------
    CANOPY(INTEGR,
           ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
           RVSTGE, TGRO, TURFAC, VSTAGE, XLAI)             #Input
           # CANHT, CANWH)                                   #Output
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("AGRVG", AGRVG, envir = env)
  assign("FRLF", FRLF, envir = env)
  assign("FRRT", FRRT, envir = env)
  assign("FRSTM", FRSTM, envir = env)
  assign("CADLF", CADLF, envir = env)
  assign("CADST", CADST, envir = env)
  assign("CANHT", CANHT, envir = env)
  assign("CANWH", CANWH, envir = env)
  assign("CMINEA", CMINEA, envir = env)
  assign("CRUSLF", CRUSLF, envir = env)
  assign("CRUSRT", CRUSRT, envir = env)
  assign("CRUSSH", CRUSSH, envir = env)
  assign("CRUSST", CRUSST, envir = env)
  assign("EXCESS", EXCESS, envir = env)
  assign("NADLF", NADLF, envir = env)
  assign("NADRT", NADRT, envir = env)
  assign("NADST", NADST, envir = env)
  assign("NGRLF", NGRLF, envir = env)
  assign("NGRRT", NGRRT, envir = env)
  assign("NGRST", NGRST, envir = env)
  assign("NSTRES", NSTRES, envir = env)
  assign("TNLEAK", TNLEAK, envir = env)
  assign("WLDOTN", WLDOTN, envir = env)
  assign("WRDOTN", WRDOTN, envir = env)
  assign("WSDOTN", WSDOTN, envir = env)
  
  return()
  #RETURN
  #-----------------------------------------------------------------------
  #END # SUBROUTINE VEGGR
}
#-----------------------------------------------------------------------
# AGRLF   Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
# AGRRT   Mass of CH2O required for new root growth (g[CH2O] / g[root])
# AGRSTM  Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
# AGRVG   Mass of CH2O required for vegetative tissue growth including 
#           stoichiometry and respiration (g[CH2O] / g[tissue])
# ATOP    Maximum fraction change in partitioning from top growth to roots 
#           if severe water or nitrogen stresses occur. 
# CADLF   Mass of CH2O added to leaf reserves after growth
#           (g[CH2O] / m2 / d)
# CADST   Mass of CH2O added to stems (g[CH2O] / m2 / d)
# CADSTF  Proportion of CH2O reserves that are added to stems (fraction)
# CANHT   Canopy height (m)
# CANWH   Canopy width normal to row (m)
# CMINEA  Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
# CMINEP  Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
# CMOBMX  Maximum C pool mobilization rate (g[CH2O] / m2 / d)
# CRUSLF  C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
# CRUSRT  C mobilized from root tissue in a day (g[CH2O] / m2 / d)
# CRUSSH  C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
# CRUSST  C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
# CSAVEV  Fraction of PG for VEG that is stored as CH2O 
# CUMTUR  Cumulative turgor factor - 20 day water stress average 
# DAS     Days after start of simulation (days)
# DTX     Thermal time that occurs in a real day based on vegetative 
#           development temperature function (thermal days / day)
# DXR57   Relative time between first seed (NR5) and physiological maturity 
#           (NR7) 
# ECONO   Ecotype code - used to match ECOTYP in .ECO file 
# ERR     Error code for file operation 
# ERRKEY  Subroutine name for error file 
# EXCESS  Factor based on excess PG used to affect tomorrow's PG 
#           calculation 
# FILECC  Path plus filename for species file (*.spe) 
# FILEGC  Pathname plus filename for ECO file 
# FNINL   Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
# FNINLG  Minimum fraction of N for growing leaf tissue (g[N] / g[leaf])
# FNINR   Maximum fraction of N for growing root tissue (g[N] / g[root])
# FNINRG  Minimum fraction of N for growing root tissue (g[N] / g[root])
# FNINS   Maximum fraction of N for growing stem tissue (g[N] / g[stem])
# FNINSG  Minimum fraction of N for growing stem tissue (g[N] / g[stem])
# FOUND   Indicator that good data was read from file by subroutine FIND (0 
#           - End-of-file encountered, 1 - NAME was found) 
# FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
#           (g[leaf] / g[veg])
# FRRT    Fraction of vegetative tissue growth that goes to roots on a day
#           (g[root] / g[veg])
# FRSTM   Fraction of vegetative tissue growth that goes to stems on a day
#           (g[stem] / g[veg])
# ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
#           line to read, 2 - End of Section in file encountered, denoted 
#           by * in column 1
# LNUM    Current line number of input file 
# LSTR    Ratio of excess C to be added to leaves in a day relative to the 
#           amount to be stored in stems 
# LUNCRP  Logical unit number for FILEC (*.spe file) 
# NADLF   N added to leaf N reserves (g[N] / m2 / d)
# NADRAT  Total nitrogen added to vegetative N reserves (g[N] / m2 / d)
# NADRT   N added to root N reserves (g[N] / m2 / d)
# NADST   N added to stem N reserves (g[N] / m2 / d)
# NAVL    Total mass of nitrogen available for growth (g[N] / m2 / d)
# NDMNEW  Total N demand for new growth (g[N] / m2 / d)
# NDMOLD  N demand for old tissue (g[N] / m2 / d)
# NFIXN   Amount of N fixed during the day (g[N] / m2 / d)
# NGRLF   Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
# NGRLFG  Minimum N requirement for leaf growth
#           (g[leaf N] / m2[ground] / d)
# NGRRT   Maximum N demand for root growth (g[root N] / m2[ground] / d)
# NGRRTG  Minimum N requirement for root growth
#           (g[leaf N] / m2[ground] / d)
# NGRST   Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
# NGRSTG  Minimum N requirement for stem growth
#           (g[leaf N] / m2[ground] / d)
# NGRVEG  Maximum N demand for vegetative tissue growth
#           (g[leaf N] / m2[ground] / d)
# NGRVGG  Minimum N requirement for vegetative tissue growth
#           (g[leaf N] / m2[ground] / d)
# NLEAK   Nitrogen leak (g[N] / m2 / d)
# NLEFT   Nitrogen left after vegetative demands are met (g[N] / m2 / d)
# NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NR1     Day when 50% of plants have at least one flower (days)
# NRATIO  Factor to reduce tissue growth based on low available nitrogen 
# NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
# PAR     Daily photosynthetically active radiation or photon flux density
#           (moles[quanta]/m2-d)
# PCH2O   Respiration loss due to storage/mobilization of CH2O
#           (g[CH2O] / g[CH2O])
# PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
# PGAVL   Total available CH2O available for growth & respiration
#           (g[CH2O] / m2)
# PGLEFT  Excess PG after today's tissue growth (g[CH2O] / m2)
# PROLFG  Normal growth protein composition in leaves during growth
#           (g[protein] / g[leaf tissue])
# PROLFI  Maximum protein composition in leaves during growth with 
#           luxurious supply of N (g[protein] / g[leaf tissue])
# PROLFT  Protein fraction of new leaf growth (g[protein] / g[leaf tissue])
# PRORTG  Normal growth protein composition in roots during growth
#           (g[protein] / g[root])
# PRORTI  Maximum protein composition in roots during growth with luxurious 
#           supply of N (g[protein] / g[root])
# PRORTT  Protein fraction of new root growth (g[protein] / g[root])
# PROSTG  Normal growth protein composition in stems during growth
#           (g[protein] / g[stem])
# PROSTI  Maximum protein composition in stems during growth with luxurious 
#           supply of N (g[protein] / g[stem])
# PROSTT  Protein fraction of new root growth (g[protein] / g[stem])
# ROWSPC  Row spacing (m)
# RVSTGE  Rate of VSTAGE change (nodes/day)
# SECTION Section name in input file 
# STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
# SUPPN   Total supply of N (g[N] / m2 / d)
# TGRO(I) Hourly air temperature (ï¿½C)
# TIMDIF  Integer function which calculates the number of days between two 
#           Julian dates (da)
# TNLEAK  Total nitrogen leak (g[N] / m2 / d)
# TRNU    Total N uptake in a day (g[N] / m2 / d)
# TS      Number of intermediate time steps (=24) 
# TURFAC  Water stress factor for expansion (0 - 1) 
# VGRDEM  Vegetative growth demand (g[vegetative tissue] / m2-d)
# VSTAGE  Number of nodes on main stem of plant 
# WCRLF   Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
# WCRRT   Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
# WCRSH   Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
# WCRST   Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
# WLDOTN  Dry weight growth rate of new leaf tissue including N but not C 
#           reserves (g[leaf] / m2[ground]-d)
# WRDOTN  Dry weight growth rate of new root tissue including N but not C 
#           reserves (g[root] / m2[ground]-d)
# WSDOTN  Dry weight growth rate of new stem tissue including N but not C 
#           reserves (g[stem] / m2[ground]-d)
# WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
# XLAI    Leaf area (one side) per unit of ground area
#           (m2[leaf] / m2[ground])
# YRDOY   Current day of simulation (YYDDD)
# YRSIM   Start of simulation date (YYDDD)
#-----------------------------------------------------------------------
#     END SUBROUTINE VEGGR
#-----------------------------------------------------------------------

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

CANOPY <- function (DYNAMIC, 
                    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       #Input
                    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI) {             #Input
                    # CANHT, CANWH) {                                    #Output
  
  #-----------------------------------------------------------------------
  
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

