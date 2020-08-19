#=======================================================================
#  INCOMP  Subroutine
#     This subroutine initializes parameters for composition of tissues
#     which vary with genotype at the beginning of each run.
#----------------------------------------------------------------------
#  REVISION HISTORY
#  03/31/1991 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
#  04/01/1991 GH  Adapted for CROPGRO
#  09/18/1998 CHP Moved to PLANT module and added input statements
#  05/10/1999 GH  Incorporated in CROPGRO
#  08/12/2003 CHP Added I/O error checking
#  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
#-----------------------------------------------------------------------
#  Called : PLANT
#  Calls  : ERROR, FIND, IGNORE
#=======================================================================

#simDataVars$AGRLF   <-  0
#simDataVars$AGRNOD  <-  0
#simDataVars$AGRRT   <-  0
#simDataVars$AGRSD1  <-  0
#simDataVars$AGRSD2  <-  0
#simDataVars$AGRSH1  <-  0
#simDataVars$AGRSH2  <-  0
#simDataVars$AGRSTM  <-  0
#simDataVars$AGRVG   <-  0
#simDataVars$AGRVG2  <-  0
#simDataVars$SDPROR  <-  0

AGRLF   <-  0
AGRNOD  <-  0
AGRRT   <-  0
AGRSD1  <-  0
AGRSD2  <-  0
AGRSH1  <-  0
AGRSH2  <-  0
AGRSTM  <-  0
AGRVG   <-  0
AGRVG2  <-  0
SDPROR  <-  0


INCOMP <- function(DYNAMIC,
                   FILECC, FILEIO, FRLF, FRRT, FRSTM,              #!Input
                   AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   #!Output
                   AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR) {          #!Output
  
  #INCOMP <- 0
  #TODO ver DYNAMIC e datas 
  #INTEGER DYNAMIC, ERR, FOUND, ISECT, LINC, LNUM
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- 0.200 #Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  SDPRO <- 0.400 #Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PCARLF   <- 0.405000001
  PCARNO   <- 0.479999989
  PCARRT   <- 0.711000025
  PCARSD   <- 0.314999998
  PCARSH   <- 0.379999995
  PCARST   <- 0.663999975
  PLIGLF   <- 7.00000003E-02
  PLIGNO   <- 7.00000003E-02
  PLIGRT   <- 7.00000003E-02
  PLIGSD   <- 1.99999996E-02
  PLIGSH   <- 0.280000001
  PLIGST   <- 7.00000003E-02
  PLIPLF   <- 2.50000004E-02
  PLIPNO   <- 5.00000007E-02
  PLIPRT   <- 1.99999996E-02
  PLIPSH   <- 1.99999996E-02
  PLIPST   <- 1.99999996E-02
  PMINLF   <- 9.39999968E-02
  PMINNO   <- 5.00000007E-02
  PMINRT   <- 5.70000000E-02
  PMINSD   <- 2.50000004E-02
  PMINSH   <- 2.99999993E-02
  PMINST   <- 4.60000001E-02
  POALF    <- 5.00000007E-02
  POANO    <- 5.00000007E-02
  POART    <- 5.00000007E-02
  POASD    <- 3.99999991E-02
  POASH    <- 3.99999991E-02
  POAST    <- 5.00000007E-02
  PROLFI   <- 0.356000006
  PRORTI   <- 9.20000002E-02
  PROSHI   <- 0.250000000
  PROSTI   <- 0.150000006
  SDPROS   <- 0.400000006
  #!*RESPIRATION PARAMETERS
  RCH2O    <- 1.24199998
  RLIG     <- 2.17400002
  RLIP     <- 3.10599995
  RMIN     <- 5.00000007E-02
  RNO3C    <- 2.55599999
  ROA      <- 0.929000020
  
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    #     COMPUTE RESPIRATION COEFFICIENTS BASED ON PLANT COMPOSITION
    #-----------------------------------------------------------------------
    #
    AGRLF  =  PLIPLF*RLIP + PLIGLF*RLIG + POALF*ROA + PMINLF*RMIN + PCARLF*RCH2O
    AGRSTM =  PLIPST*RLIP + PLIGST*RLIG + POAST*ROA + PMINST*RMIN + PCARST*RCH2O
    AGRRT  =  PLIPRT*RLIP + PLIGRT*RLIG + POART*ROA + PMINRT*RMIN + PCARRT*RCH2O
    AGRNOD =  PLIPNO*RLIP + PLIGNO*RLIG + POANO*ROA + PMINNO*RMIN + PCARNO*RCH2O
    
    #-----------------------------------------------------------------------
    #     AGRVG2, AGRSH2, AGRSD2 include protein component of vegetative 
    #     growth cost
    #-----------------------------------------------------------------------
    AGRVG  = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
    AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RNO3C
    
    #-----------------------------------------------------------------------
    AGRSH1 =  PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA  + PMINSH*RMIN + PCARSH*RCH2O
    AGRSH2 =  AGRSH1 + PROSHI*RNO3C 
    
    #-----------------------------------------------------------------------
    SDPROR = (SDPRO - SDPROS) / ( SDLIP + PCARSD )
    AGRSD1 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
    AGRSD2 = AGRSD1 + SDPRO*RNO3C 
    
    #***********************************************************************
    #***********************************************************************
    #END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #-----------------------------------------------------------------------
  #RETURN
  #END ! SUBROUTINE INCOMP
  assign("AGRLF", AGRLF, envir = env)
  assign("AGRNOD", AGRNOD, envir = env)
  assign("AGRRT", AGRRT, envir = env)
  assign("AGRSD1", AGRSD1, envir = env)
  assign("AGRSD2", AGRSD2, envir = env)
  assign("AGRSH1", AGRSH1, envir = env)
  assign("AGRSH2", AGRSH2, envir = env)
  assign("AGRSTM", AGRSTM, envir = env)
  assign("AGRVG", AGRVG, envir = env)
  assign("AGRVG2", AGRVG2, envir = env)
  assign("SDPROR", SDPROR, envir = env)
  
  #return()
}
#=======================================================================

#-----------------------------------------------------------------------
#       Variable definitions
#-----------------------------------------------------------------------
# AGRLF   Mass of CH2O required for new leaf growth 
# AGRNOD  CH2O requirement for nodule growth 
# AGRRT   Mass of CH2O required for new root growth 
# AGRSD1  CH2O requirement for seed growth, excluding cost for protein 
#           content 
# AGRSD2  CH2O requirement for seed growth, including cost for protein 
#           content 
# AGRSH1  CH2O required for shell growth, excluding cost for protein 
#           content 
# AGRSH2  CH2O requirement for shell growth, including cost for protein 
#           content 
# AGRSTM  Mass of CH2O required for new stem growth 
# AGRVG   Mass of CH2O required for vegetative tissue growth including 
#           stoichiometry and respiration 
# AGRVG2  Total mass of CH2O required for vegetative tissue growth 
# ECONO   Ecotype code - used to match ECOTYP in .ECO file
#           ","IPDMND, IPGROW, IPIBS, IPPHENOL, PODS, IPPLNT
# ECOTYP  Ecotype code for this simulation "
# ERR     Error code for file operation 
# FILECC  Path plus filename for species file (*.spe) 
# FILEGC  Pathname plus filename for ECO file "
# FOUND   Indicator that good data was read from file by subroutine FIND (0 
#           - End-of-file encountered, 1 - NAME was found) 
# FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
#           
# FRRT    Fraction of vegetative tissue growth that goes to roots on a day 
# FRSTM   Fraction of vegetative tissue growth that goes to stems on a day 
# ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
#           line to read, 2 - End of Section in file encountered, denoted 
#           by * in column 1
# LNUM    Current line number of input file 
# LUNCRP  Logical unit number for FILEC (*.spe file) 
# LUNECO  Logical unit number for FILEE (*.eco file) "
# PCARLF  Proportion of leaf tissue that is carbohydrate
#           (fraction)","IPGROW, INCOMP
# PCARNO  Proportion of nodule tissue that is carbohydrate
#           (fraction)","IPGROW, INCOMP
# PCARRT  Proportion of root tissue that is carbohydrate
#           (fraction)","IPGROW, INCOMP
# PCARSD  Proportion of seed tissue that is carbohydrate
#           (g[CH2O] / g[seed])","IPGROW, INCOMP, IPPLNT
# PCARSH  Proportion of shell tissue that is carbohydrate
#           (fraction)","IPGROW, INCOMP, IPPLNT
# PCARST  Proportion of stem tissue that is carbohydrate
#           (fraction)","IPGROW, INCOMP
# PLIGLF  Proportion of leaf tissue that is lignin
#           (fraction)","IPGROW, INCOMP
# PLIGNO  Proportion of nodule tissue that is lignin
#           (fraction)","IPGROW, INCOMP
# PLIGRT  Proportion of root tissue that is lignin
#           (fraction)","IPGROW, INCOMP
# PLIGSD  Proportion of seed tissue that is lignin
#           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
# PLIGSH  Proportion of shell tissue that is lignin
#           (fraction)","IPPLNT, IPGROW, INCOMP
# PLIGST  Proportion of stem tissue that is lignin
#           (fraction)","IPGROW, INCOMP
# PLIPLF  Proportion of leaf tissue that is lipid
#           (fraction)","IPGROW, INCOMP
# PLIPNO  Proportion of nodule tissue that is lipid
#           (fraction)","IPGROW, INCOMP
# PLIPRT  Proportion of root tissue that is lipid
#           (fraction)","IPGROW, INCOMP
# PLIPSH  Proportion of shell tissue that is lipid
#           (fraction)","IPPLNT, IPGROW, INCOMP
# PLIPST  Proportion of stem tissue that is lipid
#           (fraction)","IPGROW, INCOMP
# PMINLF  Proportion of leaf tissue that is mineral
#           (fraction)","IPGROW, INCOMP
# PMINNO  Proportion of nodule tissue that is mineral
#           (fraction)","IPGROW, INCOMP
# PMINRT  Proportion of root tissue that is mineral
#           (fraction)","IPGROW, INCOMP
# PMINSD  Proportion of seed tissue that is mineral
#           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
# PMINSH  Proportion of shell tissue that is mineral
#           (fraction)","IPPLNT, IPGROW, INCOMP
# PMINST  Proportion of stem tissue that is mineral
#           (fraction)","IPGROW, INCOMP
# POALF   Proportion of leaf tissue that is organic acid
#           (fraction)","IPGROW, INCOMP
# POANO   Proportion of nodule tissue that is organic acid
#           (fraction)","IPGROW, INCOMP
# POART   Proportion of root tissue that is organic acid
#           (fraction)","IPGROW, INCOMP
# POASD   Proportion of seed tissue that is organic acid
#           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
# POASH   Proportion of shell tissue that is organic acid
#           (fraction)","IPPLNT, IPGROW, INCOMP
# POAST   Proportion of stem tissue that is organic acid
#           (fraction)","IPGROW, INCOMP
# PROLFI  Maximum protein composition in leaves during growth with 
#           luxurious supply of N
#           (g[protein] / g[leaf tissue])","IPPLNT, IPDMND, I
# PRORTI  Maximum protein composition in roots during growth with luxurious 
#           supply of N (g[protein] / g[root])","IPPLNT, IPDMND, IPGROW, 
# PROSHI  Maximum protein composition in shells during growth with 
#           luxurious supply of N
#           ( g[protein] / g[shell tissue])","PODS, IPPLNT, I
# PROSTF  Minimum stem protein composition after N mining
#           (g[protein] / g[stem])","IPGROW, INCOMP, IPDMND
# PROSTI  Maximum protein composition in stems during growth with luxurious 
#           supply of N (g[protein] / g[stem])","IPPLNT, IPDMND, IPGROW, 
# RCH2O   Respiration required for synthesizing CH2O structure
#           (g[CH2O] / g[tissue])","IPDMND, PODCOMP, IPPLNT, 
# RLIG    Respiration required for synthesizing lignin structure
#           (g[CH2O] / g[lignin])","IPDMND, PODCOMP, IPPLNT, 
# RLIP    Respiration required for synthesizing lipid structure
#           (g[CH2O] / g[lipid])","IPDMND, PODCOMP, IPPLNT, I
# RMIN    Respiration required for synthesizing mineral structure
#           (g[CH2O] / g[mineral])","IPPLNT, IPDMND, PODCOMP
# RNO3C   Respiration required for reducing NO3 to protein
#           (g[CH2O] / g[protein])","IPDMND, IPPLNT, INCOMP
# ROA     Respiration required for synthesizing organic acids
#           (g[CH2O] / g[product])","IPDMND, PODCOMP, IPPLNT
# SDLIP   Maximum lipid composition in seed
#           (fraction)","IPDMND, IPGROW, INCOMP
# SDPRO   Seed protein fraction at 25oC
#           (g[protein] / g[seed])","IPDMND, IPGROW, INCOMP
# SDPROR  Ratio to adjust lipid and carbohydrate proportions when seed 
#           protein differs from protein composition of standard cultivar 
#           (SDPROS) 
# SDPROS  Seed protein fraction of standard cultivar at 25oC
#           (g[protein] / g[seed])","INCOMP
#-----------------------------------------------------------------------
#      END SUBROUTINE INCOMP
#=======================================================================