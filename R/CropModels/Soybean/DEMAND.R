#=======================================================================
#  DEMAND, Subroutine, J.W. Jones and G. Hoogenboom.
#-----------------------------------------------------------------------
#  Calculates potential demand for C and N based upon new growth and
#  existing N deficiency in old tissue.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1990 JWJ Written.
#  02/01/1993 GH  Revised.
#  04/24/1994 NBP Changed TAIRHR to TGRO.
#  08/22/1995 GH  Added seed composition routine from KJB & ELPiper
#  04/02/1996 JWJ Modified partitioning during early growth
#  01/10/1997 GH  Added TURFAC effect on seed growth and pod addition
#  09/15/1998 CHP Modified for modular format
#  05/10/1999 GH  Incorporated in CROPGRO
#-----------------------------------------------------------------------
#  Called by:  PLANT
#  Calls:      SDCOMP, IPDMND
#=======================================================================

#SUBROUTINE DEMAND(DYNAMIC, CONTROL,
#                  &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
#                  &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
#                  &  LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
#                  &  PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
#                  &  SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
#                  &  TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
#                  &  WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
#                  &  WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
#                  
#                  &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  #Output
#                  &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
#                  &  GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
#                  &  NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
#                  &  POTLIP, SDGR, TURADD, XFRT, YREND)                #Output

DEMAND <- function(DYNAMIC, CONTROL,
                   AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
                   FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
                   LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
                   PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
                   SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
                   TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
                   WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
                   WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
                   
                   AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, Fnew, FNINL,  #Output *** Fnew is 'F' in the original file. Changed because F is logical in R. ***
                   FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
                   GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
                   NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
                   POTLIP, SDGR, TURADD, XFRT, YREND) {                #Output
  
  DEMAND <- 0
  
  ##-----------------------------------------------------------------------
  #USE ModuleDefs
  #USE ModuleData
  #IMPLICIT NONE
  #SAVE
  #CHARACTER*2 CROP
  #CHARACTER*3 TYPSDT
  #CHARACTER*6   ERRKEY
  #PARAMETER (ERRKEY = 'DEMAND')
  #CHARACTER*30 FILEIO
  #CHARACTER*78 MSG(2)
  #CHARACTER*92 FILECC, FILEGC
  #TODO verificar se vamos utilizar e DYNAMIC
  #INTEGER DYNAMIC   #, TIMDIF
  #I
  #NPP

  NAGE   <- 0
  DAS    <- 0 #TODO ver como está sendo usado no ECOSMOS
  NDLEAF <- 0 # calculado no RSTAGES.for
  NR1    <- 0 # calculado no RSTAGES.for
  NR2    <- 0 # calculado no RSTAGES.for
  NR5    <- 0 # calculado no RSTAGES.for
  NR7    <- 0 # calculado no RSTAGES.for
  NVEG0  <- 0 # calculado no RSTAGES.for
  YREND  <- 0 #TODO checar oq significa
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- 0.200 # Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  SDPRO <- 0.400 # Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  XFRT  <- 1.000 # Maximum fraction of daily growth that is partitioned to seed + shell
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  LNGSH <- 10.0  # Time required for growth of individual shells (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*VEGETATIVE PARTITIONING PARAMETERS
  FRLFM   <- 0.70
  #!*LEAF GROWTH PARAMETERS
  FINREF <- 180.
  SLAREF <- 350.
  SIZREF <- 171.399994
  VSSINK <- 5.0
  SLAMAX <- 950.
  SLAMIN <- 250.0
  SLAPAR <- -0.048
  TURSLA <- 1.50
  XVGROW <- c( 0.0,  1.0,  2.0,  3.0,  4.0,  5.0)
  YVREF  <- c( 0.0, 20.0, 55.0,110.0,200.0,320.0)
  YVGROW <- rep(0,6) #preenchido com uma função de interpolacao/lookup (TABEX)
  #!*VEGETATIVE PARTITIONING PARAMETERS
  FRLFF  <- 0.24
  FRSTMF <- 0.55
  FRLFMX <- 0.70
  #!*SEED  COMPOSITION VALUES 
  CARMIN <- 0.180
  LIPOPT <- 23.65 
  LIPTB  <- 7.16
  SLOSUM <- 0.908 #TODO checar SLOSUM*100 = 0.908 (no .SPE)
  #!*SEED AND SHELL GROWTH PARAMETERS
  FNSDT  <- c(14.0, 21.0, 26.5, 40.0) #+ QDR in .SPE
  SHLAG  <- 0
  SRMAX  <- 0.300000012
  XFRMAX <- 0
  #!*CARBON AND NITROGEN MINING PARAMETERS
  NMOBMX <- 0.090
  NRCVR  <- 0.15
  NVSMOB <- 0.35
  #!*PLANT COMPOSITION VALUES
  PLIGSD <- 0.020
  PMINSD <- 0.025
  POASD  <- 0.040
  PROLFF <- 0.112
  PROLFI <- 0.356
  PRORTF <- 0.056
  PRORTI <- 0.092
  PROSTF <- 0.035
  PROSTI <- 0.150
  #!*RESPIRATION PARAMETERS
  RCH2O  <- 1.242
  RLIG   <- 2.174
  RLIP   <- 3.106
  RMIN   <- 0.05
  RNO3C  <- 2.556
  ROA    <- 0.929
  RPRO   <- 0.360
  
  #TODO verificar se (10) e (25) seria rep(0,XX)
  XSLATM <- rep(0,10)
  YSLATM <- rep(0,10)
  XTRFAC <- rep(0,10)
  YTRFAC <- rep(0,10)
  XXFTEM <- rep(0,10)
  YXFTEM <- rep(0,10)
  XLEAF  <- rep(0,25)
  YLEAF  <- rep(0,25)
  YSTEM  <- rep(0,25)
  
  #TGRO[TS]
  TGRO   <- rep(1.,24)
  
  NCOHORTS <- 300 #from line 51 in ModuleDefs.for NCOHORTS = 300, !Maximum number of cohorts
  SDDES <- rep(0, NCOHORTS)
  SDNO  <- rep(0, NCOHORTS)
  SHELN <- rep(0, NCOHORTS)
  WTSD  <- rep(0, NCOHORTS)
  WTSHE <- rep(0, NCOHORTS)
  PHTIM <- rep(0, NCOHORTS)
  PNTIM <- rep(0, NCOHORTS)
  
  FRSTMM  <- 0
  YY      <- 0
  XX      <- 0
  TMPFAC  <- 0
  REDPUN  <- 0
  TMPFCS  <- 0
  PAGE    <- 0
  REDSHL  <- 0
  SDMAX   <- 0
  CDMSH   <- 0
  GDMSH   <- 0
  ADDSHL  <- 0
  TEMXFR  <- 0
  CAVTOT  <- 0
  GDMSDO  <- 0
  CNOLD   <- 0
  NVSTL   <- 0
  NVSTS   <- 0
  NVSTR   <- 0
  FRNLFT  <- 0
  POTLIP  <- 0
  POTCAR  <- 0
  TPHFAC  <- 0
  PARSLA  <- 0
  FFVEG   <- 0
  ROYES   <- 0
  GAINNW  <- 0
  GAINWT  <- 0
  SLAVAR  <- 0
  THRESH  <- 0
  GRSH2   <- 0
  AGRRT   <- 0
  AGRSTM  <- 0
  TURFSL  <- 0
  
  #TODO VERIFICAR nesta lista...
  # ORIGENS DE ALGUNS DESSES VALORES QUE VIERAM DO print*,
  # Aqueles que vem de outra função/subrotina: não sei se atribuo zero [0] ou não aqui
  AGRLF   <- 0.783989966 # fixed value from print*, | não descobri de onde vem!
  AGRSD1  <- 0 # from SDCOM.for
  AGRSD2  <- 0 # from SDCOM.for
  AGRVG   <- 0
  AGRVG2  <- 0
  CDMREP  <- 0
  CDMSD   <- 0
  CDMSDR  <- 0
  CDMTOT  <- 0 # CDMTOT not used - chp
  CDMVEG  <- 0
  DRPP    <- 0 # DRPP = FUDAY[6] no PHENOL.for
  DUMFAC  <- 0
  DXR57   <- 0 # calculado no PHENOL.for
  Fnew    <- 0 #*** Fnew is 'F' in the original file. Changed because F is logical in R. ***
  FNINL   <- 0
  FNINR   <- 0
  FNINS   <- 0
  FNINSD  <- 0
  FNINSH  <- 0
  FRACDN  <- 0 # calculado no PHENOL.for
  FRLF    <- 0
  # FRLFMX  <-  'subi' como parametros de espécie (.SPE)
  FRRT    <- 0
  FRSTM   <- 0
  FVEG    <- 0
  GDMSD   <- 0
  GDMSDR  <- 0
  GROMAX  <- 0
  GRRAT1  <- 0
  LAGSD   <- 0 # calculado no PODS.for
  LNGPEG  <- 0 # calculado no PODS.for
  #LNGSH   <-  'subi' como parametros de ecótipo (.ECO)
  NDMNEW  <- 0
  NDMOLD  <- 0
  NDMREP  <- 0
  NDMSD   <- 0
  NDMSDR  <- 0
  NDMSH   <- 0
  NDMTOT  <- 0
  NDMVEG  <- 0
  NMINEP  <- 0
  #NMOBMX  <-  'subi' como parametros de espécie (.SPE) 
  NMOBR   <- 0
  #NRCVR   <-  'subi' como parametros de espécie (.SPE) 
  NSTRES  <- 1 # N stress factor (1=no stress, 0=max stress) [verificar de onde vem no ECOSMOS se formos usar]
  #NVSMOB  <-  'subi' como parametros de espécie (.SPE)  
  PAR     <- 0 # PAR em moles[quanta]/m2-d (verificar de onde vem do ECOSMOS)
  PCNL    <- 0 # calculado no GROW.for
  PCNRT   <- 0 # calculado no GROW.for
  PCNST   <- 0 # calculado no GROW.for
  PGAVL   <- 0 # inicializado no CROPGRO.for
  #PLIGSD  <-  'subi' como parametros de espécie (.SPE)  
  PLTPOP  <- 0 # plant population -> provavel q venha do arquivo experimental (densidade x espacamento)
  #PMINSD  <-  'subi' como parametros de espécie (.SPE)  
  #POASD   <-  'subi' como parametros de espécie (.SPE)   
  #PROLFF  <-  'subi' como parametros de espécie (.SPE)    
  #PROLFI  <-  'subi' como parametros de espécie (.SPE)    
  #PRORTF  <-  'subi' como parametros de espécie (.SPE)    
  #PRORTI  <-  'subi' como parametros de espécie (.SPE)    
  #PROSTF  <-  'subi' como parametros de espécie (.SPE)    
  #PROSTI  <-  'subi' como parametros de espécie (.SPE)    
  #RCH2O   <-  'subi' como parametros de espécie (.SPE)
  #RLIG    <-  'subi' como parametros de espécie (.SPE)
  #RLIP    <-  'subi' como parametros de espécie (.SPE)
  #RMIN    <-  'subi' como parametros de espécie (.SPE)
  #RNO3C   <-  'subi' como parametros de espécie (.SPE)
  #ROA     <-  'subi' como parametros de espécie (.SPE)
  #RPRO    <-  'subi' como parametros de espécie (.SPE)
  RPROAV  <- 0 # calculado no CROPGRO.for
  RTWT    <- 0 # calculado no GROW.for
  SDGR    <- 0
  #SDLIP   <-  'subi' como parametros de cultivar (.CUL) 
  #SDPRO   <-  'subi' como parametros de cultivar (.CUL)  
  SDVAR   <- 0 # calculado no PODS.for
  #SHLAG   <-  'subi' como parametros de espécie (.SPE) 
  SHVAR   <- 0 # calculado no PODS.for
  SIZELF  <- 200.000000 #indicado em alguns .ECO da familia CROPGRO, mas nao encontrei onde exatamente!
  #SIZREF  <-  'subi' como parametros de espécie (.SPE)   
  SIZRAT  <- 0
  SLAMN   <- 0
  SLAMX   <- 0
  #SLAPAR  <-  'subi' como parametros de espécie (.SPE)
  #SRMAX   <-  'subi' como parametros de espécie (.SPE)
  STMWT   <- 0 # calculado no GROW.for
  SWFAC   <- 0 # water stress factor (verificar de onde vem no ECOSMOS)
  TAVG    <- 0 # buscar do padrao do Ecosmos    
  TDUMX   <- 0 # calculado no PHENOL.for
  TDUMX2  <- 0 # calculado no PHENOL.for
  TURADD  <- 0
  TURFAC  <- 0 # water stress factor (verificar de onde vem no ECOSMOS)
  #TURSLA  <-  'subi' como parametros de espécie (.SPE)
  TURXFR  <- 0
  #VSSINK  <-  'subi' como parametros de espécie (.SPE)
  VSTAGE  <- 0 # calculado no PHENOL.for
  WCRLF   <- 0 # calculado no GROW.for
  WCRRT   <- 0 # calculado no GROW.for
  WCRST   <- 0 # calculado no GROW.for
  WNRLF   <- 0 # calculado no GROW.for
  WNRRT   <- 0 # calculado no GROW.for
  WNRSH   <- 0 # calculado no GROW.for
  WNRST   <- 0 # calculado no GROW.for
  WTLF    <- 0 # calculado no GROW.for
  #XFRMAX  <-  'subi' como parametros de espécie (.SPE)
  #XFRT    <-  'subi' como parametros de cultivar (.CUL)
  XFRUIT  <- 0 #XFRT   = XFRUIT at EMERG
  XPOD    <- 0 # calculado no GROW.for
  
  ##CHP - puncture variables, not functional
  #REAL PUNCSD, PUNCTR, RPRPUN
  
  #TYPE (ControlType) CONTROL
  
  
  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  if (DYNAMIC == RUNINIT) {
    #-----------------------------------------------------------------------
    #CALL IPDMND(
    #  &  FILECC, FILEGC, FILEIO,                           #Input
    #  &  CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             #Output
    #  &  FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             #Output
    #  &  NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             #Output
    #  &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   #Output
    #  &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              #Output
    #  &  RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        #Output
    #  &  SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   #Output
    #  &  SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    #Output
    #  &  XFRUIT, XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM,    #Output
    #  &  YLEAF, YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM)      #Output
    
    IPDMND( #TODO verificar se é necessário
      FILECC, FILEGC, FILEIO,                           #Input
      CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             #Output
      FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             #Output
      NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             #Output
      PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   #Output
      RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              #Output
      RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        #Output
      SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   #Output
      SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    #Output
      XFRUIT, XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM,    #Output
      YLEAF, YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM)      #Output
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  } else if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    CDMSDR = 0.0
    GDMSDR = 0.0
    FNINSD = 0.0
    NDMNEW = 0.0
    NDMREP = 0.0
    NDMSD  = 0.0
    NDMSH  = 0.0
    NDMSDR = 0.0
    NDMVEG = 0.0
    NMOBR  = 0.0
    SDGR   = 0.0
    FNINL  = 0.0
    FNINS  = 0.0
    FNINR  = 0.0
    NMINEP = 0.0
    
    RPRPUN = 1.0 
    TMPFAC = 1.0
    
    #-----------------------------------------------------------------------
    #     SET VARIETY SPECIFIC LEAF PARAMETERS
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      DUMFAC = SLAVAR / SLAREF
      Fnew   = DUMFAC * FINREF # VERIFICAR: F ? palavra reservada da linguagem. Substituir.
      FVEG   = DUMFAC * SLAMAX
      SLAMN  = DUMFAC * SLAMIN
      SLAMX  = DUMFAC * SLAMAX
      GROMAX = 0.0
      SIZRAT = SIZELF / SIZREF
      
      for (I in 1:6){
        YVGROW[I] = SIZRAT * YVREF[I]
      }
      
      #-----------------------------------------------------------------------
      #     INITIALIZE PARTITIONING PARAMETERS
      #-----------------------------------------------------------------------
      FRLF = TABEX(YLEAF,XLEAF,0.0,8)
      FRSTM = TABEX(YSTEM,XLEAF,0.0,8)
      FRRT = 1.0 - FRLF - FRSTM
      
    }
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    XFRT   = XFRUIT
    ADDSHL = 0.0
    TURXFR = 0.0
    GDMSD  = 0.0
    CDMSD  = 0.0
    NDMSD  = 0.0
    GDMSDR = 0.0
    CDMSDR = 0.0
    NDMSDR = 0.0
    CDMREP = 0.0
    NAGE   = 0
    for (NPP in 1:NCOHORTS) {
      PHTIM[NPP] = 0.
      PNTIM[NPP] = 0.
    }
    FNINSD = SDPRO * 0.16   
    FNINL  = PROLFI * 0.16  
    FNINS  = PROSTI * 0.16  
    FNINR  = PRORTI * 0.16  
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    #     DAS = max(0,TIMDIF(YRSIM,YRDOY))
    #CALL GET(CONTROL)
    
    # ALTERADO: Resto da divis?o s?o dois %. [DiasHB: talvez % no Fortran seja equivalente ao $ no R]
    # DAS = CONTROL % DAS 
    DAS = CONTROL %% DAS 
    
    #-----------------------------------------------------------------------
    #     Compute max N mining, NMINEP, based on stage-dependent mining
    #     rate, NMOBR
    #-----------------------------------------------------------------------
    #     Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day
    #     NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
    #-----------------------------------------------------------------------
    #     9/27/95 ACCELERATE N MOBILIZATION AFTER R5, FUNCTION OF (1-SWFAC)
    #     ALLOWS ACCELERATING BY 50% IF MAX DEFICIT.
    #     2/6/96 SOMETIMES SEEDS FILL, XPOD IS LOW, { N MOBILIZATION SLOWS
    #     I DON'T REALLY WANT THAT, LATE IN CYCLE.  KJB
    #     NOW, DXR57 HITS CLOSE TO 1 AT MATURITY AND PREVENTS THAT
    #-----------------------------------------------------------------------
    NMOBR  = NVSMOB * NMOBMX * TDUMX
    if (DAS > NR5) {
      NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC)) * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB) * max(XPOD,DXR57^2.))
    }
    NMINEP = NMOBR * (WNRLF + WNRST + WNRRT + WNRSH)
    
    #-----------------------------------------------------------------------
    if (DAS >= NR1) {
      #-----------------------------------------------------------------------
      #     Accumulate physiological age of flower (PNTIM) and pod (PHTIM) cohorts
      #-----------------------------------------------------------------------
      if (DAS - NR1 + 1 > NCOHORTS) {
        WRITE(MSG(1),'(A,I5)') 'Number of flower cohorts exceeds maximum limit of',NCOHORTS
        CALL WARNING(1,ERRKEY,MSG)
        CALL ErrorCode(CONTROL, 100, ERRKEY, YREND)
        
        # ALTERADO: RETURN
        return()
      }
      
      # VERIFICAR: Nao seria DAS <= NR1 ? Pois caso DAS seja menor, a linha a seguir dar? erro por ser indice negativo.
      if (DAS == NR1) {
        PNTIM[1] = 0.0
      } else {
        PNTIM[DAS - NR1 + 1] = PNTIM[DAS - NR1] + TDUMX
      }
      
      if (DAS <= NR2) {
        PHTIM[1] = 0.0
      } else {
        PHTIM(DAS - NR2 + 1) = PHTIM(DAS - NR2) + TDUMX
      }
      
      #-----------------------------------------------------------------------
      #     Calculate function for modifying seed growth rate with temperature
      #-----------------------------------------------------------------------
      TMPFAC = 0
      TMPFCS = 0
      for (I in 1:TS) {
        TMPFAC = CURV(TYPSDT,FNSDT[1], FNSDT[2], FNSDT[3], FNSDT[4], TGRO[I])
        TMPFCS = TMPFCS + TMPFAC
      }
      
      TMPFAC = TMPFCS / TS
      
      # 24 changed to TS on 3Jul17 by Bruce Kimball
      
      #-----------------------------------------------------------------------
      #       Calculate reduction in seed growth due to insect punctures
      #-----------------------------------------------------------------------
      if (PUNCSD > 0.001) {
        REDPUN = 1.0 - (PUNCTR/PUNCSD) * RPRPUN
        REDPUN = max(0.0,REDPUN)
      } else {
        REDPUN = 1.0
      }
      #-----------------------------------------------------------------------
      #       Water stress factor (TURADD) effect on reproductive growth and
      #       pod addition.  Stress is defined to INCREASE growth and addition.
      #-----------------------------------------------------------------------
      TURADD = TABEX (YTRFAC,XTRFAC,TURFAC,4)
      #-----------------------------------------------------------------------
      #     Calculate maximum growth per seed based on temp and seed punctures
      #-----------------------------------------------------------------------
      SDGR = SDVAR * TMPFAC * REDPUN * (1.-(1.-DRPP)*SRMAX) * (1. + TURADD)
      #-----------------------------------------------------------------------
      #     Initialize Seed Growth Demands and CH2O and N required for seed
      #       growth
      #-----------------------------------------------------------------------
      GDMSD  = 0.0
      CDMSD  = 0.0
      NDMSD  = 0.0
      GDMSDR = 0.0
      CDMSDR = 0.0
      NDMSDR = 0.0
      #-----------------------------------------------------------------------
      if (DAS > NR2) {
        for (NPP in 1:(DAS - NR2)) { 
          #-----------------------------------------------------------------------
          #     Calculate physiol age of seed cohort.  Do not allow seed to grow
          #     until shells are greater than LAGSD physiol age.
          #-----------------------------------------------------------------------
          PAGE = PHTIM[DAS - NR2 + 1] - PHTIM[NPP]
          if (PAGE >= LAGSD) {
            #-----------------------------------------------------------------------
            #     Allow cohort growth until threshing limit (seed wt./pod wt) occurs
            #     taking into account damage by pests to seed and shells
            #-----------------------------------------------------------------------
            REDSHL = 0
            if (SDDES[NPP] > 0) {
              REDSHL = WTSHE[NPP] * SDDES[NPP] / (SDDES[NPP] + SDNO[NPP])
            }
            SDMAX = (WTSHE[NPP] - REDSHL) * THRESH / (100 - THRESH) - WTSD[NPP]
            SDMAX = max(0, SDMAX)
            #-----------------------------------------------------------------------
            #     Compute Seed Growth Demand, GDMSD, and N required for seed, NDMSD
            #-----------------------------------------------------------------------
            GDMSD  = GDMSD  + min(SDGR * SDNO[NPP] * REDPUN, SDMAX)
          }
        }
        #-----------------------------------------------------------------------
        #     Call seed composition routine
        #-----------------------------------------------------------------------
        #CALL SDCOMP(
        #  &      CARMIN, LIPOPT, LIPTB, PLIGSD, PMINSD, POASD, #Input
        #  &      RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SDLIP,   #Input
        #  &      SDPRO, SLOSUM, TAVG,                          #Input
        #  &      AGRSD1, AGRSD2, FNINSD, POTCAR, POTLIP)       #Output
        
        SDCOMP(
          CARMIN, LIPOPT, LIPTB, PLIGSD, PMINSD, POASD, #Input
          RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SDLIP,   #Input
          SDPRO, SLOSUM, TAVG,                          #Input
          AGRSD1, AGRSD2, FNINSD, POTCAR, POTLIP)       #Output
        
        NDMSD  = FNINSD * GDMSD
        #-----------------------------------------------------------------------
        #     Calculate Amount of Mobilized N Which Can be Used for Seed Growth,
        #     NDMSDR, potential seed growth from this source of N, GDMSDR,
        #     and CH2O required for this seed growth from mobilized N, CDMSDR
        #-----------------------------------------------------------------------
        if (NDMSD > NMINEP) {
          NDMSDR = NMINEP
        } else {
          NDMSDR = NDMSD
        }
        GDMSDR = NDMSDR / FNINSD
        CDMSDR = GDMSDR * (AGRSD1 + FNINSD * 6.25 * RPRO)
        #-----------------------------------------------------------------------
        #    Compute Total CH2O Demand to Grow GDMSD g Tissue
        #-----------------------------------------------------------------------
        CDMSD = (max(0, (GDMSD - GDMSDR))) * AGRSD2 + CDMSDR
      }
    }
    #-----------------------------------------------------------------------
    #     Compute max growth per shell, depending on temp, daylength
    #-----------------------------------------------------------------------
    GRRAT1 = SHVAR * TMPFAC * (1- (1-DRPP) * SRMAX) * (1.0 + TURADD)
    #-----------------------------------------------------------------------
    #     Initialize Shell Growth Demand, N (NDMSH) and C (CDMSH) needed for growth
    #-----------------------------------------------------------------------
    GDMSH = 0.0
    NDMSH = 0.0
    CDMSH = 0.0
    #-----------------------------------------------------------------------
    #     Compute growth demand for shells, GDMSH, allowing slow growth
    #     until LNGPEG age, then potential growth until LNGSH
    #-----------------------------------------------------------------------
    if (DAS > NR2) {
      for (NPP in 1:(DAS - NR2)) {  
        NAGE = DAS - NR2 + 1 - NPP  #NAGE not used - chp
        PAGE = PHTIM(DAS - NR2 + 1) - PHTIM[NPP]
        if (PAGE <= LNGSH & SHELN[NPP] >= 0.001 & GRRAT1 >= 0.001) {
          if (PAGE >= LNGPEG) {
            #Shells between LNGPEG and LNGSH
            ADDSHL = GRRAT1 * SHELN[NPP]
          } else {
            #Shells < LNGPEG
            ADDSHL = GRRAT1 * SHELN[NPP] * SHLAG
          }
        }
        GDMSH  = GDMSH + ADDSHL
      }
      #-----------------------------------------------------------------------
      #     Compute CH2O required for the potential shell growth
      #-----------------------------------------------------------------------
      CDMSH = GDMSH * AGRSH2
    }
    #-----------------------------------------------------------------------
    #     Compute TEMXFR, the temp effect on partitioning to pods
    #     High temp would increase fraction growth to vegetative tissue
    #-----------------------------------------------------------------------
    TEMXFR = 0.
    for (I in 1:TS) {
      TEMXFR = TEMXFR + TABEX(YXFTEM,XXFTEM,TGRO[I],6)
    }
    TEMXFR = TEMXFR/TS
    # 24 changed to TS by Bruce Kimball on 3Jul17
    
    #-----------------------------------------------------------------------
    #     Partitioning to pods is increased under drought stress conditions
    #        depending on XFRMAX, an input parameter
    #-----------------------------------------------------------------------
    TURXFR = XFRMAX * (1 - TURFAC)
    TURXFR = min(TURXFR, 1)
    TURXFR = max(TURXFR, 0)
    #-----------------------------------------------------------------------
    #     Night length and temperature are multiplicative
    #     but turgor effect adds to the partitioning
    #-----------------------------------------------------------------------
    XFRT = XFRUIT * TEMXFR + XFRUIT * TURXFR
    #     XFRT = XFRUIT * RNIT * TEMXFR   #NEED TO FIX FOR DAYLENGTH EFFECT
    XFRT = min(XFRT,1.0)
    XFRT = max(XFRT,0.0)
    #-----------------------------------------------------------------------
    #    Total Potential Available CH2O for Reprod Growth (CAVTOT)
    #    and total CH2O needed for potential reproductive growth (CDMREP)
    #-----------------------------------------------------------------------
    CAVTOT = PGAVL * XFRT
    CDMREP = CDMSH + CDMSD
    #-----------------------------------------------------------------------
    #    Adjust #-Demand for New Growth if #-Available is Less than C Demand
    #    Also adjust tissue growth demand for seeds and shells
    #-----------------------------------------------------------------------
    GDMSDO = GDMSD
    if (CDMREP > CAVTOT) {
      if (CDMSD > CAVTOT) {
        CDMSH = 0.0
        GDMSH = 0.0
        CDMSD = CAVTOT
        if (CDMSDR > CAVTOT) {
          CDMSDR = CAVTOT
        }
        GDMSD = (max(0.0,(CDMSD-CDMSDR)))/ AGRSD2 + CDMSDR / (AGRSD1 + FNINSD * 6.25 * RPRO)
        NDMSDR = GDMSDR * FNINSD
      } else {
        CDMSH = CAVTOT - CDMSD
        GDMSH = CDMSH/AGRSH2
      }
      CDMREP = CDMSD + CDMSH
    }
    #-----------------------------------------------------------------------
    #     Compute N demand for seed, shell, and total reproductive growth
    #-----------------------------------------------------------------------
    NDMSD  = GDMSD * FNINSD
    NDMSH  = GDMSH * FNINSH
    NDMREP = NDMSD + NDMSH
    
    #-----------------------------------------------------------------------
    #     Vegetative partitioning factors and demand for C and N for new
    #     growth before VSSINK, assume leaf expansion is fixed, compute
    #     SLA based on function of light, temp, etc, then compute
    #     FRLF (leaf partitioning), then FRRT, FRSTM
    #-----------------------------------------------------------------------
    #     Check to See if New Vegetative Tissue Can Be Grown, Using PGAVL
    #-----------------------------------------------------------------------
    CDMVEG = max(0.0,(1.-XFRT)*PGAVL)
    NDMVEG = 0.0
    CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG
    
    #-----------------------------------------------------------------------
    #       This is from documentation:  check no longer needed?? chp
    #-----------------------------------------------------------------------
    #      CDMVEG = max(0.0,(1.-XFRT)*PGAVL)
    #      if (PGAVL * XFRT > CDMREP) {
    #        if (N <= NDLEAF) CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG
    #      }
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    if (DAS == NR1) {
      #-----------------------------------------------------------------------
      #     Fraction of growth going to leaves and roots decreases
      #     linearly between R1 and NDLEAF.
      #-----------------------------------------------------------------------
      FRLFM  = TABEX (YLEAF, XLEAF, VSTAGE, 8)
      FRSTMM = TABEX (YSTEM, XLEAF, VSTAGE, 8)
      YY = FRLFM - FRLFF 
      XX = FRSTMM - FRSTMF
    }
    #-----------------------------------------------------------------------
    if (DAS < NR1) {
      #-----------------------------------------------------------------------
      #     Calculate Pattern of Vegetative Partitioning, a function of V-STAGE
      #-----------------------------------------------------------------------
      FRLF  = TABEX(YLEAF,XLEAF,VSTAGE,8)
      FRSTM = TABEX(YSTEM,XLEAF,VSTAGE,8)
    } else {
      #-----------------------------------------------------------------------
      #     Partitioning between vegetative tissues depends on development
      #     as expressed by FRACDN, the relative development between R1 and NDLEAF
      #-----------------------------------------------------------------------
      FRLF = FRLFM - YY * FRACDN
      FRSTM = FRSTMM - XX * FRACDN
      if ( DAS >= NDLEAF) {
        FRLF = FRLFF
        FRSTM = FRSTMF
      }
    }
    
    #     This is where to modify partitioning for extra root growth:
    #     check units### fraction vs percentage
    #     FRLF = FRLF - FRLF/(FRLF+FRSTM) * (extra root value)
    #     FRSTM= FRSTM - FRSTM/(FRLF+FRSTM) * (extra root value)
    FRRT = 1 - FRLF - FRSTM
    
    #-----------------------------------------------------------------------
    #     Compute F, specific leaf area for new leaf weight
    #-----------------------------------------------------------------------
    TPHFAC = 0
    for (I in 1:TS){
      TPHFAC = TPHFAC + TABEX (YSLATM,XSLATM,TGRO[I],5)
    }
    TPHFAC = TPHFAC/TS
    # 24 changed to TS by Bruce Kimball on 3Jul17
    
    #-----------------------------------------------------------------------
    # VERIFICAR: EXP seria exponencial? Se for, trocar por 'exp()'
    PARSLA = (SLAMN+(SLAMX-SLAMN) * EXP(SLAPAR*PAR)) / SLAMX
    TURFSL = max(0.1, (1.0 - (1.0 - TURFAC)*TURSLA))
    #-----------------------------------------------------------------------
    #     Compute overall effect of TMP, PAR, water stress on SLA (F), first
    #     for veg stages, then transition to rep stage from R1 to end leaf
    #     effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
    #-----------------------------------------------------------------------
    FFVEG = FVEG * TPHFAC * PARSLA * TURFSL
    
    # VERIFICAR: F novamente. Mudar
    Fnew = FFVEG 
    if (XFRT*FRACDN >= 0.05) { 
      Fnew = FFVEG * (1.0 - XFRT * FRACDN) 
    }
    #-----------------------------------------------------------------------
    #     For determinate plants (XFRUIT=1.) leaf expansion stops at NDLEAF
    #-----------------------------------------------------------------------
    # VERIFICAR: F novamente. Mudar
    if (XFRUIT > 0.9999 & DAS >= NDLEAF) { 
      Few = 0.0 
    }
    
    #-----------------------------------------------------------------------
    #     During early vegetative growth, leaf area expansion depends on
    #     VSTAGE (Prior to VSSINK).  This sets FRLF, partitioning of d.m.
    #     to leaves.  FRRT and FRSTM are then computed by left over C.  When
    #     an upper limit of d.m. goes to leaves, leaf area expansion is
    #     restricted so that F is maintained as computed and minimal amounts
    #     of C is partitioned to FRSTM and FRRT  (JWJ 4/1/96)
    #-----------------------------------------------------------------------
    if (VSTAGE < VSSINK) {
      GROYES = GROMAX
      GROMAX = TABEX(YVGROW,XVGROW,VSTAGE,6) * SIZELF/SIZREF
      GAINNW = (GROMAX - GROYES) * PLTPOP
      #-----------------------------------------------------------------------
      #     CALCULATE MINIMUM WEIGHT NEEDED TO ADD GAINNW LEAF AREA/M2,
      #     AND AMOUNT OF LEAF WEIGHT WHICH CAN BE GROWN WITH PG AVAILABLE
      #-----------------------------------------------------------------------
      # VERIFICAR: F novamente. Mudar
      if (Fnew > 1.E-5) {
        GAINWT = GAINNW/Fnew
      } else {
        GAINWT = 0.0
      }
      #-----------------------------------------------------------------------
      #     Compute fraction of C partitioned to leaves, based on F, VSSINK
      #     Limit leaf pertitioning to FRLFMX (i.e., FRLFMX = 0.7)
      #-----------------------------------------------------------------------
      FRLF = (AGRLF*GAINWT)/(CDMVEG + 0.0001)
      if (FRLF > FRLFMX) {
        GAINWT = (CDMVEG/AGRLF) * FRLFMX
        # VERIFICAR: F novamente. Mudar
        GAINNW = GAINWT * Fnew
        FRLF = FRLFMX
      }
      #-----------------------------------------------------------------------
      #     Recompute FRSTM and FRRT based on FRLF
      #-----------------------------------------------------------------------
      FRSTM = (1. - FRLF) * FRSTM / (FRSTM + FRRT)
      FRRT  = 1. - FRLF - FRSTM
      #-----------------------------------------------------------------------
    }
    #-----------------------------------------------------------------------
    #     Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
    #     and total CH2O cost per g of veg tissue (AGRVG2)
    #-----------------------------------------------------------------------
    AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
    AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RPROAV
    #-----------------------------------------------------------------------
    #    Compute N Demand for New Tissue, including reproductive and vegetative
    #-----------------------------------------------------------------------
    NDMVEG = (CDMVEG/AGRVG2) * (FRLF*FNINL+FRSTM*FNINS+ FRRT*FNINR)
    NDMNEW = NDMREP + NDMVEG
    #-----------------------------------------------------------------------
    #    Check to See if Any C is Left After Reproductive Growth for
    #    Reducing N to Re-Fill Old Tissue, if N Can Be Taken up by Roots
    #-----------------------------------------------------------------------
    CNOLD = max(0.0,PGAVL-CDMREP)
    NDMOLD = 0.0
    #-----------------------------------------------------------------------
    #    Nitrogen Demand for Old Tissue
    #-----------------------------------------------------------------------
    if (DAS > NVEG0 & DAS < NR7 & CNOLD > 0.0) {
      NVSTL = FNINL
      NVSTS = FNINS
      NVSTR = FNINR
      if (DXR57 > 0.0) {
        FRNLFT = (NRCVR + (1. - NRCVR) * (1. - DXR57^2))
        NVSTL = PROLFF*0.16 + (FNINL-PROLFF*0.16) * FRNLFT
        NVSTS = PROSTF*0.16 + (FNINS-PROSTF*0.16) * FRNLFT
        NVSTR = PRORTF*0.16 + (FNINR-PRORTF*0.16) * FRNLFT
      }
      NDMOLD = (WTLF  - WCRLF) * max(0.0,(NVSTL - PCNL /100.)) + (STMWT - WCRST) * max(0.0,(NVSTS - PCNST/100.)) + (RTWT  - WCRRT) * max(0.0,(NVSTR - PCNRT/100.))
      if (NDMOLD > (CNOLD/RNO3C*0.16)) {
        NDMOLD = CNOLD/RNO3C*0.16
      }
    }
    #-----------------------------------------------------------------------
    #    Total N Demand
    #-----------------------------------------------------------------------
    NDMTOT = NDMREP + NDMVEG + NDMOLD
    #-----------------------------------------------------------------------
    #    Compute Total Demand for C, and Max. C that Could be Mined
    #     CDMTOT not used - chp
    #-----------------------------------------------------------------------
    CDMTOT = CDMREP + CDMVEG + NDMOLD*RNO3C/0.16 
    GDMSD = GDMSDO
    #-----------------------------------------------------------------------
    #    At this point, PGAVL will be used entirely, assuming that N can be
    #    made available in the ratio described.
    #     Growth Demands : GDMSD, GDMSH
    #     N-Demands      : NDMREP, NDMVEG, NDMOLD, NDMTOT, NDMNEW
    #     #-Demands      : CDMREP, CDMVEG, CDMTOT, CNOLD
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #-----------------------------------------------------------------------
  
  #RETURN
  #END SUBROUTINE DEMAND
  return()
}


#TODO VERIFICAR se a subrotina IPDMND é necessária!
#=======================================================================

#=======================================================================
#  IPDMND, Subroutine, C.H. Porter
#-----------------------------------------------------------------------
#  Reads input data for DEMAND subroutine
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  07/04/1998 CHP Written.
#  08/12/2003 CHP Added I/O error checking
#  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
#-----------------------------------------------------------------------
#  Called by:  DEMAND
#  Calls:      FIND, ERROR, IGNORE
#=======================================================================
SUBROUTINE IPDMND(
  &  FILECC, FILEGC, FILEIO,                           #Input
  &  CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             #Output
  &  FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             #Output
  &  NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             #Output
  &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   #Output
  &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              #Output
  &  RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        #Output
  &  SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   #Output
  &  SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    #Output
  &  XFRUIT, XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM,    #Output
  &  YLEAF, YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM)      #Output

#-----------------------------------------------------------------------
IMPLICIT NONE
#-----------------------------------------------------------------------
CHARACTER*3   TYPSDT
CHARACTER*6   ERRKEY
PARAMETER (ERRKEY = 'DEMAND')
CHARACTER*6   SECTION
CHARACTER*6   ECOTYP, ECONO
CHARACTER*30  FILEIO
CHARACTER*80  C80
CHARACTER*92  FILECC, FILEGC
CHARACTER*255 C255

INTEGER LUNCRP, LUNIO, LUNECO, ERR, LINC, LNUM, FOUND, ISECT
INTEGER I, II

REAL CARMIN, FINREF, FRLFF, FRLFMX, FRSTMF,
&  LIPOPT, LIPTB, NMOBMX, NRCVR, NVSMOB,
&  PLIGSD, PMINSD, POASD, PROLFF,
&  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,
&  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,
&  RPRO, SHLAG, SLAMAX, SLAMIN, SLAPAR,
&  SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,
&  SRMAX, TURSLA, VSSINK, XFRMAX, XFRUIT
REAL LNGSH, THRESH, SDPRO, SDLIP

REAL FNSDT(4)
REAL XVGROW(6), YVREF(6)
REAL XSLATM(10), YSLATM(10), XTRFAC(10), YTRFAC(10), XXFTEM(10), YXFTEM(10)
REAL XLEAF(25), YLEAF(25), YSTEM(25)

#-----------------------------------------------------------------------
CALL GETLUN('FILEIO', LUNIO)
OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILEIO,0)}
LNUM = 0
#-----------------------------------------------------------------------
#    Find and Read Field Section from FILEIO - previously read in IPIBS
#       Look for the second section header beginning with '*CULTI'
#-----------------------------------------------------------------------
SECTION = '*CULTI'
CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILEIO, LNUM)
}
CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILEIO, LNUM)
} else {
  READ(LUNIO,'(24X,A6,48X,3F6.0,24X,3F6.0)',IOSTAT=ERR) 
  &      ECONO, SLAVAR, SIZELF, XFRUIT, THRESH, SDPRO, SDLIP
  LNUM = LNUM + 1
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)}
}

CLOSE (LUNIO)

#-----------------------------------------------------------------------
#     Read in values from species file
#-----------------------------------------------------------------------
CALL GETLUN('FILEC', LUNCRP)
OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
LNUM = 0
if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
#-----------------------------------------------------------------------
#    Find and Read Respiration Section
#-----------------------------------------------------------------------
SECTION = '#*RESP'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) RNO3C, RPRO
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(5F6.0)',IOSTAT=ERR)RCH2O,RLIP,RLIG,ROA,RMIN
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}

#-----------------------------------------------------------------------
#    Find and Read Plant Composition Section
#-----------------------------------------------------------------------
SECTION = '#*PLAN'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR) PROLFI, PROLFF, PROSTI, PROSTF
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) PRORTI, PRORTF
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(24X,F6.0)',IOSTAT=ERR) PLIGSD
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(24X,F6.0)',IOSTAT=ERR) POASD
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(24X,F6.0)',IOSTAT=ERR) PMINSD
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}

#-----------------------------------------------------------------------
#    Find and Read Seed Composition Section
#-----------------------------------------------------------------------
SECTION = '#*SEED'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(4F6.0)',IOSTAT=ERR) LIPTB, LIPOPT, SLOSUM, CARMIN
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  SLOSUM = SLOSUM / 100.0
}

#-----------------------------------------------------------------------
#    Find and Read Carbon and Nitrogen Mining Section
#-----------------------------------------------------------------------
SECTION = '#*CARB'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB, NRCVR
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}

#-----------------------------------------------------------------------
#    Find and Read Vegetative Partitioning Section
#-----------------------------------------------------------------------
SECTION = '#*VEGE'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(8F6.0)',IOSTAT=ERR)(XLEAF(II),II=1,8)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(8F6.0)',IOSTAT=ERR)(YLEAF(II),II=1,8)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEM(II),II=1,8)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(12X,2F6.0)',IOSTAT=ERR) FRSTMF, FRLFF
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(F6.0)',IOSTAT=ERR) FRLFMX
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}

#-----------------------------------------------------------------------
#    Find and Read Leaf Growth Section
#-----------------------------------------------------------------------
SECTION = '#*LEAF'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(4F6.0)',IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
  if (ERR != 0){CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(4F6.0)',IOSTAT=ERR) SLAMAX, SLAMIN, SLAPAR, TURSLA
  if (ERR != 0){CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6F6.0)',IOSTAT=ERR)(XVGROW(II),II=1,6)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6F6.0)',IOSTAT=ERR)(YVREF(II),II=1,6)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(5F6.0)',IOSTAT=ERR)(XSLATM(II),II = 1,5)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(5F6.0)',IOSTAT=ERR)(YSLATM(II),II = 1,5)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}

#-----------------------------------------------------------------------
#    Find and Read Seed and Shell Growth Section
#-----------------------------------------------------------------------
SECTION = '#*SEED'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
if (FOUND == 0) {
  CALL ERROR(SECTION, 42, FILECC, LNUM)
} else {
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6X,F6.0)',IOSTAT=ERR) SRMAX
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6X,2F6.0)',IOSTAT=ERR) XFRMAX, SHLAG
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)(FNSDT(II),II=1,4), TYPSDT
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6F6.0)',IOSTAT=ERR)(XXFTEM(II),II = 1,6)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(6F6.0)',IOSTAT=ERR)(YXFTEM(II),II = 1,6)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  for (I in 1:5) {
    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  }
  READ(C80,'(4F6.0)',IOSTAT=ERR)(XTRFAC(II),II = 1,4)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
  
  CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  READ(C80,'(4F6.0)',IOSTAT=ERR)(YTRFAC(II),II = 1,4)
  if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
}
#-----------------------------------------------------------------------
CLOSE(LUNCRP)

#-----------------------------------------------------------------------
#    Read Ecotype Parameter File
#-----------------------------------------------------------------------
CALL GETLUN('FILEE', LUNECO)
OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
ECOTYP = '      '
LNUM = 0
DO WHILE (ECOTYP != ECONO) { #TODO: checar essa função no R
  CALL IGNORE(LUNECO, LNUM, ISECT, C255)
  if ((ISECT == 1) & (C255(1:1) != ' ') & (C255(1:1) != '*')) {
    #          READ (C255,'(A6,66X,F6.0,30X,3F6.0)',IOSTAT=ERR)
    #     &        ECOTYP, LNGSH, THRESH, SDPRO, SDLIP
    READ (C255,'(A6,66X,F6.0,30X)',IOSTAT=ERR) ECOTYP, LNGSH
    if (ERR != 0) {CALL ERROR(ERRKEY,ERR,FILECC,LNUM)}
    if (ECOTYP == ECONO) {
      EXIT
    }
    
  } else { if (ISECT == 0) {
    if (ECONO == 'DFAULT') {CALL ERROR(ERRKEY,35,FILEGC,LNUM)}
    ECONO = 'DFAULT'
    REWIND(LUNECO)
    LNUM = 0
  }
  }
  
  CLOSE (LUNECO)
  
  #-----------------------------------------------------------------------
  RETURN
  #-----------------------------------------------------------------------
  END  SUBROUTINE IPDMND
  #=======================================================================
  
  #=======================================================================
  #       Variable definitions for DEMAND and IPDMND
  #         Updated 25 Feb 2004
  #-----------------------------------------------------------------------
  # ADDSHL    Today's growth demand for shells of age NPP (g[shell] / m2 / d)
  # AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
  # AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
  # AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
  #             content (g[CH2O] / g[seed])
  # AGRSD2    CH2O requirement for seed growth, including cost for protein 
  #             content (g[CH2O] / g[seed])
  # AGRSH2    CH2O requirement for shell growth, including cost for protein 
  #             content (g[CH2O] / g[shell])
  # AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
  # AGRVG     Mass of CH2O required for vegetative tissue growth including 
  #             stoichiometry and respiration (g[CH2O] / g[tissue])
  # AGRVG2    Total mass of CH2O required for vegetative tissue growth
  #            (g[CH2O] / g[tissue])
  # C255      255-character record read from file 
  # C80       80-character record read from file 
  # CARMIN    Minimum carbohydrate fraction 
  # CAVTOT    Total potential available CH2O for reproductive growth
  #            (g[CH2O] / m2)
  # CDMREP    Total CH2O needed for potential reproductive growth
  #            (g[CH2O] / m2 / d)
  # CDMSD     Total CH2O demand to grow seed demand (GDMSD)
  #            (g[CH2O] / m2 / d)
  # CDMSDR    CH2O required for seed growth from mobilized N
  #            (g[CH2O] / m2 / d)
  # CDMSH     Total CH2O demand to grow shell demand (GDMSH)
  #            (g[CH2O] / m2 / d)
  # CDMTOT    Total CH2O demand (g[CH2O] / m2 / d)
  # CDMVEG    Carbon demand for vegetative growth (g[CH2O] / m2 / d)
  # CNOLD     Available CH2O after reproductive growth (g[CH2O] / m2 / d)
  # CROP      Crop identification code 
  # DAS       Days after start of simulation (d)
  # DRPP      Photoperiod days which occur in a real day
  #            (photoperiod days / day)
  # DXR57     Relative time between first seed (NR5) and physiological 
  #             maturity (NR7) (fraction)
  # ECONO     Ecotype code - used to match ECOTYP in .ECO file 
  # ECOTYP    Ecotype code for this simulation 
  # ERR       Error code for file operation 
  # F         Specific leaf area of new leaf tissue growth, including N
  #            (cm2[leaf] / g[leaf])
  # FFVEG     Specific leaf area of new leaf tissue growth (interim value)
  #            (cm2[leaf] / g[leaf])
  # FILECC    Path plus filename for species file (*.spe) 
  # FILEGC    Pathname plus filename for ECO file 
  # FINREF    Specific leaf area (SLA) of leaves of standard crop cultivar 
  #             when plants emerge (cm2[leaf] / g[leaf])
  # FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
  # FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
  # FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
  # FNINSD    Maximum fraction of N for growing seed tissue based on 
  #             temperature (g[N] / g[seed])
  # FNINSH    Maximum fraction of N for growing shell tissue
  #            (g[N] / g[shell])
  # FNSDT(I)  Temperature values which describe function for modifying seed 
  #             growth rate with temperature (ï¿½C)
  # FOUND     Indicator that good data was read from file by subroutine FIND 
  #             (0 - End-of-file encountered, 1 - NAME was found) 
  # FRACDN    Relative time between flowering (NR1) and last leaf appearance 
  #             (NDLEAF) 
  # FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
  #             day (g[leaf] / g[veg])
  # FRLFF     Fraction of daily increase in vegetative weight which goes to 
  #             leaves after the day on which the maximum number of V-stages 
  #             occurs (NDVSTG). (g[leaf] / g[veg])
  # FRLFM     Fraction of growth going to leaves, decreases linearly between 
  #             R1 and NDLEAF (g[leaf] / g[veg])
  # FRLFMX    Maximum leaf partitioning (g[leaf] / g[veg])
  # FRNLFT    A quadratic function of the progress from NR5 to NR7 (DXR57), 
  #             used to compute the change in maximum tissue N content 
  #             between its maximum value and a fractional value (NRCVR) 
  #             between the minimum and maximum tissue  N concentrations 
  # FRRT      Fraction of vegetative tissue growth that goes to roots on a 
  #             day (g[root] / g[veg])
  # FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
  #             day (g[stem] / g[veg])
  # FRSTMF    Fraction of daily dry weight increase in vegetative plant parts 
  #             which goes to stems after the day on which the maximum number 
  #             of V-stages occurs (NDVSTG). (g[stem] / g[veg])
  # FRSTMM    Fraction of growth going to stems, decreases linearly between 
  #             R1 and NDLEAF (g[stem] / g[veg])
  # FVEG      Specific leaf area prior to computing effects of temperature, 
  #             PAR, water stress (cm2[leaf] / g[leaf])
  # GAINNW    Leaf area added (prior to VSSINK) (cm2[leaf] / m2[ground])
  # GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
  #            (g[leaf] / m2[ground])
  # GDMSD     Seed growth demand based on temperature and photoperiod
  #            (g[seed] / m2 / d)
  # GDMSDO    Seed growth demand (temporary value) (g[seed] / m2 / d)
  # GDMSDR    Potential seed growth from NDMSDR (amount of Mobilized N which 
  #             can be used for seed growth) (g[seed] / m2 / d)
  # GDMSH     Growth demand for shells (g[shell] / m2 / d)
  # GROMAX    Maximum leaf area which can be added per plant between 
  #             emergence and day of simulation as a function of V-stage on 
  #             day of simulation (cm2[leaf] / plant)
  # GROYES    Maximum leaf area which could have been added per plant between 
  #             emergence and yesterday as a function of V-stage
  #             (cm2[leaf] / plant)
  # GRRAT1    Maximum growth per individual shell (g / shell / d)
  # ISECT     Indicator of completion of IGNORE routine: 0 - End of file 
  #             encountered, 1 - Found a good line to read, 2 - End of 
  #             Section in file encountered denoted by * in column 1. 
  # LAGSD     Time required between shell growth and seed growth, per cohort
  #            (Photo-thermal days)
  # LINC      Line number of input file 
  # LIPOPT    Temperature above which lipid composition is at a maximum (ï¿½C)
  # LIPTB     Temperature below which lipid composition is zero (ï¿½C)
  # LNGPEG    Time between start of peg (full flower) and shell formation 
  #             (for peanuts only).  Defines slow growth period.
  #             (Photo-thermal days)
  # LNGSH     Time required for shell growth (Photo-thermal days)
  # LNUM      Current line number of input file 
  # LUNCRP    Logical unit number for FILEC (*.spe file) 
  # LUNECO    Logical unit number for FILEE (*.eco file) 
  # NAGE      Age of cohort (d)
  # NDLEAF    Day when leaf expansion ceased (d)
  # NDMNEW    Total N demand for new growth (g[N] / m2 / d)
  # NDMOLD    N demand for old tissue (g[N] / m2 / d)
  # NDMREP    Total N needed for potential reproductive growth
  #            (g[N] / m2 / d)
  # NDMSD     Total N demand to grow seed demand (GDMSD) (g[N] / m2 / d)
  # NDMSDR    Amount of Mobilized N which can be used for seed growth
  #            (g[N] / m2 / d)
  # NDMSH     Total N demand to grow shell demand (GDMSH) (g[N] / m2 / d)
  # NDMTOT    Total N demand (g[N] / m2 / d)
  # NDMVEG    N required for vegetative growth if all PGAVL is used as 
  #             computed (g[N] / m2 / d)
  # NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
  # NMOBMX    Maximum fraction of N which can be mobilized in a day 
  # NMOBR     Stage-dependent potential N mining rate expressed as a fraction 
  #             of the maximum rate (NMOBMX) 
  # NPP       Cohort number used as index in loops 
  # NR1       Day when 50% of plants have at least one flower (d)
  # NR2       Day when 50% of plants have one fruit (pod or peg) (d)
  # NR5       Day when 50% of plants have pods with beginning seeds (d)
  # NR7       Day when 50% of plants first have yellowing or maturing pods
  #            (d)
  # NRCVR     Fractional value between minimum and maximum tissue N values 
  #             (0-1) 
  # NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
  # NVEG0     Day of emergence (d)
  # NVSMOB    Relative rate of N mining during vegetative stage to that in 
  #             reproductive stage 
  # NVSTL     N content in leaves (fraction)
  # NVSTR     N content in roots (fraction)
  # NVSTS     N content in stems (fraction)
  # PAGE      Photothermal age of each cohort (Photo-thermal days)
  # PAR       Daily photosynthetically active radiation or photon flux 
  #             density (moles[quanta]/m2-d)
  # PARSLA    Effect of PAR on specific leaf area 
  # PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
  # PCNRT     Percent N in root tissue (100 g[N] / g[root])
  # PCNST     Percent N in stem tissue (100 g[N] / g[stem])
  # PGAVL     Total available CH2O available for growth & respiration
  #            (g[CH2O] / m2)
  # PHTIM     Cumulative photothermal time ages of seeds and shells 
  # PLIGSD    Proportion of seed tissue that is lignin (fraction)
  # PLTPOP    Plant population (# plants / m2)
  # PMINSD    Proportion of seed tissue that is mineral (fraction)
  # PNTIM(I)  Photothermal days from first flower when flowers in age group I 
  #             formed (p-t-d)
  # POASD     Proportion of seed tissue that is organic acid (fraction)
  # POTCAR    Potential carbohydrate composition of seed based on temperature
  #            (fraction)
  # POTLIP    Potential lipid composition of seed based on temperature
  #            (fraction)
  # PROLFF    Minimum leaf protein composition after N mining
  #            (g[protein] / g[leaf])
  # PROLFI    Maximum protein composition in leaves during growth with 
  #             luxurious supply of N (g[protein] / g[leaf tissue])
  # PRORTF    Minimum root protein composition after N mining
  #            (g[protein] / g[root])
  # PRORTI    Maximum protein composition in roots during growth with 
  #             luxurious supply of N (g[protein] / g[root])
  # PROSTF    Minimum stem protein composition after N mining
  #            (g[protein] / g[stem])
  # PROSTI    Maximum protein composition in stems during growth with 
  #             luxurious supply of N (g[protein] / g[stem])
  # PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
  # PUNCTR    Cumulative puncture damage (not yet implemented) 
  # RCH2O     Respiration required for synthesizing CH2O structure
  #            (g[CH2O] / g[tissue])
  # REDPUN    Reduces growth of seed in an age group due to pest-caused 
  #             punctures in seed (0 to 1) (not yet implemented) 
  # REDSHL    Reduces growth of shell in an age group due to pest-caused 
  #             punctures in seed (0 to 1) 
  # RLIG      Respiration required for synthesizing lignin structure
  #            (g[CH2O] / g[lignin])
  # RLIP      Respiration required for synthesizing lipid structure
  #            (g[CH2O] / g[lipid])
  # RMIN      Respiration required for synthesizing mineral structure
  #            (g[CH2O] / g[mineral])
  # RNO3C     Respiration required for reducing NO3 to protein
  #            (g[CH2O] / g[protein])
  # ROA       Respiration required for synthesizing organic acids
  #            (g[CH2O] / g[product])
  # RPRO      Respiration required for re-synthesizing protein from mobilized 
  #             N (g[CH2O] / g[protein])
  # RPROAV    Respiration required for protein synthesis, average based on 
  #             sources of N (g[CH2O] / g[protein])
  # RPRPUN    Puncture damage reduction variable (not yet implemented) (0-1) 
  # RTWT      Dry mass of root tissue, including C and N
  #            (g[root] / m2[ground])
  # SDDES(J)  Number of seeds destroyed today in cohort J when shells are not 
  #             destroyed (#/m2/d)
  # SDGR      Potential growth rate per seed (g / seed / d)
  # SDLIP     Maximum lipid composition in seed (fraction)
  # SDMAX     A maximum amount of remaining growth for each cohort (g/m2)
  # SDNO(J)   Number of seeds for cohort J (#/m2)
  # SDPRO     Seed protein fraction at 25ï¿½C (g[protein] / g[seed])
  # SDVAR     Maximum cultivar-dependent seed growth rate, per seed
  #            (g / seed / d)
  # SECTION   Section name in input file 
  # SHELN(J)  Number of shells for cohort J (#/m2)
  # SHLAG     Shell (peg) growth rate during its initial slow growth phase 
  #             after beginning pegging (R2) as a fraction of shell growth 
  #             rate (SHVAR) during its rapid growth phase. 
  # SHVAR     Shell growth rate during its rapid growth phase, per shell
  #            (g / shell / d)
  # SIZELF    The size of a normal upper node leaf (nodes 8 - 10) used to 
  #             adjust leaf area expansion during sink-limited phase of 
  #             vegetative growth, i.e., prior to VSSINK nodes on the main stem
  #             (cm2/leaf)
  # SIZRAT    Ratio of upper node normal leaf size for given variety to that 
  #             for standard cultivar, used to adjust table of maximum leaf 
  #             area vs. V-stage 
  # SIZREF    The size of a normal upper node  leaf (nodes 8 - 10) of 
  #             standard cultivar. (cm2 / leaf)
  # SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown 
  #             under low (nearly zero) radiation but optimum water and 
  #             temperature for the standard cultivar. (cm2 / g)
  # SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown 
  #             under infinitely high radiation, optimum water and 
  #             temperature for the standard cultivar. (cm2 / g)
  # SLAMN     Minimum specific leaf area for new leaves when grown under high 
  #             radiation and optimum water and temperature conditions (cm2 / g)
  # SLAMX     Maximum specific leaf area for new leaves when grown under low 
  #             radiation, but optimum water and temperature conditions
  #             (cm2 / g)
  # SLAPAR    Coefficient in exponential equation to reduce SLA as PAR 
  #             increases (leaf curvature) 
  # SLAREF    Specific leaf area (SLA) for new leaves during peak vegetative 
  #             growth for the standard cultivar. (cm2/g)
  # SLAVAR    Specific leaf area (SLA) for new leaves during peak vegetative 
  #             growth for cultivar I, modified by environmental factor (cm2/g)
  # SLOSUM    Slope of temperature vs. SUMTEM line (1/ï¿½C)
  # SRMAX     Maximum fraction change in seed growth rate for long day 
  #             lengths 
  # STMWT     Dry mass of stem tissue, including C and N
  #            (g[stem] / m2[ground)
  # SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
  #             0.0=max stress 
  # TAVG      Average daily temperature (ï¿½C)
  # TDUMX     Photo-thermal time that occurs in a real day based on early 
  #             reproductive development temperature function
  #             (photo-thermal days / day)
  # TDUMX2    Photo-thermal time that occurs in a real day based on late 
  #             reproductive development temperature function
  #             (photo-thermal days / day)
  # TEMXFR    Temperature effect on partitioning to pods, high temp. 
  #             increases fraction of growth to vegetative tissue (0-1) 
  # TGRO(I)   Hourly canopy temperature (ï¿½C)
  # THRESH    The maximum ratio mass of seed to mass of seed plus shell at 
  #             maturity.  Causes seed to stop growing as their dry weights 
  #             increase until shells are filled in a cohort. 
  # TMPFAC    Modifies maximum growth rate for seed and shells depending on 
  #             temperature 
  # TMPFCS    Interim value of TMPFAC 
  # TPHFAC    Reduction in specific leaf area due to daytime temperature 
  #             being less than optimal (0-1) 
  # TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
  #             pod addition.  Stress is defined to INCREASE growth and 
  #             addition. 
  # TURFAC    Water stress factor for expansion (0 - 1) 
  # TURFSL    Factor which applies water stress to specific leaf area of new 
  #             leaf tissue growth 
  # TURSLA    Water stress effects on leaf area expansion 
  # TURXFR    Turgor water stress factor used to modify partitioning to 
  #             reproductive growth 
  # TYPSDT    Curve type for temperature factor calculations (for use in 
  #             function subroutine CURV) 
  # VSSINK    Vegetative stage beyond which sink-limited leaf area expansion 
  #             can no longer limit photosynthesis or leaf area growth. 
  # VSTAGE    Number of nodes on main stem of plant (nodes)
  # WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
  # WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
  # WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
  # WNRLF     N available for mobilization from leaves above lower limit of 
  #             mining (g[N] / m2)
  # WNRRT     N available for mobilization from roots above lower limit of 
  #             mining (g[N] / m2)
  # WNRSH     N available for mobilization from shells above lower limit of 
  #             mining (g[N] / m2)
  # WNRST     N available for mobilization from stems above lower limit of 
  #             mining (g[N] / m2)
  # WTLF      Dry mass of leaf tissue including C and N
  #            (g[leaf] / m2[ground])
  # WTSD(J)   Seed mass  for cohort J (g/m2)
  # WTSHE(J)  Shell mass  for cohort J (g/m2)
  # XFRMAX    Maximum increase in partitioning to fruits induced under water 
  #             stress, assuming no problem in pod setting 
  # XFRT      Current day's partitioning to reproductive growth (0-1)
  #            (g[fruit] / g[plant])
  # XFRUIT    Maximum fraction of daily available gross photosynthate (PG) 
  #             which is allowed to go to seeds plus shells, varies from 0 to 
  #             1.0. 
  # XLEAF(I)  V-stage at which partitioning to leaves is YLEAF(I).
  #            (leaf nodes)
  # XPOD      Growth partitioning to pods which slows node appearance
  #            (fraction)
  # XSLATM(I) Temperature values for function that reduces specific leaf area 
  #             (SLA) (ï¿½C)
  # XTRFAC(I) Values of TURFAC for function which reduces reproductive growth 
  #             based on water stress 
  # XVGROW(I) V-stage at which maximum leaf area growth per plant since 
  #             emergence is YVGROW(I). (# leaf nodes)
  # XX        Difference between partitioning fraction to stems at beginning 
  #             bloom (R1) and at the day on which the maximum number of 
  #             V-stages occurs (NDLEAF) 
  # XXFTEM(I) Array of temperature values in table lookup describing effect 
  #             of temperature on partitioning to pods (YXFTEM = 0 TO 1). (ï¿½C)
  # YLEAF(I)  Partitioning fraction to leaves at V-stage XLEAF(I)
  #            (g[leaf] / g[veg. plant])
  # YSLATM(I) Array which describes the effect of temperature on specific 
  #             leaf area 
  # YSTEM(I)  Partitioning factor for stem growth at V-stage XSTEM(I)
  #            (g[stem] / g[veg. plant])
  # YTRFAC(I) Factor which affects reproductive growth based on water stress 
  # YVGROW(I) Maximum leaf area grown per plant at V-stage XVGROW(I)
  #            (cm2 / plant)
  # YVREF(I)  Maximum leaf area grown per plant at V-stage XVGROW(I), for 
  #             reference cultivar. (cm2 / plant)
  # YXFTEM(I) Array describing the relative partitioning to pods (0 to 1 
  #             effect on XFRUIT) as temperature increases. 
  # YY        Used to linearly interpolate the difference in partitioning to 
  #             leaves between stages R1 and NDLEAF 
  #-----------------------------------------------------------------------
  #       END SUBROUTINE DEMAND
  #=======================================================================