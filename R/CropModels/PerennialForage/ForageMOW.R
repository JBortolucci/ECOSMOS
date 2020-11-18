simDataVars$TSDNOS  <- 0
simDataVars$TSDNOL  <- 0
simDataVars$TSDNOM  <- 0
simDataVars$TSDWTS  <- 0
simDataVars$TSDWTL  <- 0
simDataVars$TSDWTM  <- 0
simDataVars$TSHNOS  <- 0
simDataVars$TSHNOL  <- 0
simDataVars$TSHNOM  <- 0
simDataVars$TSHWTS  <- 0
simDataVars$TSHWTL  <- 0
simDataVars$TSHWTM  <- 0
simDataVars$PL      <- rep(0,6)
simDataVars$TLFAD   <- 0 
simDataVars$PLFAD   <- 0
simDataVars$TLFMD   <- 0
simDataVars$PLFMD   <- 0
simDataVars$PCLMT   <- 0
simDataVars$PCLMA   <- 0
simDataVars$PDLA    <- 0
simDataVars$TDLA    <- 0
simDataVars$VSTGD   <- 0
simDataVars$PVSTGD  <- 0 
simDataVars$WSTMD   <- 0
simDataVars$PSTMD   <- 0
simDataVars$PCSTMD  <- 0
simDataVars$WRTMD   <- 0
simDataVars$PRTMD   <- 0
simDataVars$TRTLV   <- 0
simDataVars$PRTLV   <- 0
simDataVars$PRTLF   <- 0
simDataVars$TRTLF   <- 0
simDataVars$PRLV    <- 0
simDataVars$NSDDS   <- 0
simDataVars$PSDDS   <- 0
simDataVars$NSDDL   <- 0
simDataVars$PSDDL   <- 0
simDataVars$NSDDM   <- 0
simDataVars$PSDDM   <- 0
simDataVars$WSDDS   <- 0
simDataVars$WSDDL   <- 0
simDataVars$WSDDM   <- 0
simDataVars$PSDD    <- 0
simDataVars$WSDD    <- 0
simDataVars$NSHDS   <- 0
simDataVars$NSHDL   <- 0
simDataVars$NSHDM   <- 0
simDataVars$PSHDS   <- 0
simDataVars$PSHDL   <- 0
simDataVars$PSHDM   <- 0
simDataVars$WSHDS   <- 0
simDataVars$WSHDL   <- 0
simDataVars$WSHDM   <- 0
simDataVars$NPLTD   <- 0
simDataVars$PPLTD   <- 0
simDataVars$CPPLTD  <- 0
simDataVars$TPSR    <- 0
simDataVars$PPSR    <- 0
simDataVars$LAIW    <- 0
simDataVars$PLAIW   <- 0
simDataVars$PCSTRD  <- 0
simDataVars$PSTRD   <- 0
simDataVars$WSTRD   <- 0
simDataVars$HPDAM   <- 0

simDataVars$SDIDOT <- 0.0
simDataVars$SHIDOT <- 0.0
simDataVars$CSDM   <- 0.0
simDataVars$CSDN   <- 0.0
simDataVars$SDWDES <- rep(0,300)
simDataVars$SDNDES <- rep(0,300)
simDataVars$SHWDES <- rep(0,300)
simDataVars$SHNDES <- rep(0,300)
simDataVars$CSHM   <- 0.0
simDataVars$CSHN   <- 0.0

simDataVars$DAYLY  <- 0.0

simDataVars$RLVDOT  <- 0
simDataVars$CRLV    <- 0
simDataVars$RLFDOT  <- 0
simDataVars$CRLF    <- 0
simDataVars$CRTM    <- 0
simDataVars$WRIDOT  <- 0

simDataVars$CLSEN  <- 0
simDataVars$CLAI   <- 0
simDataVars$CLFM   <- 0
simDataVars$LAIDOT <- 0
simDataVars$DISLA  <- 0
simDataVars$DISLAP <- 0
simDataVars$CLFRZ  <- 0
simDataVars$CSTEM  <- 0
simDataVars$CSFRZ  <- 0
simDataVars$WSIDOT <- 0
simDataVars$CSRFRZ <- 0
simDataVars$CSTRM  <- 0
simDataVars$DSTOR  <- 0
simDataVars$SRDAM  <- 0

simDataVars$DRMST  <- ""
simDataVars$PPGFAC <- 0 
simDataVars$FREEZ2 <- 0

simDataVars$PPMFAC    <- 0
simDataVars$VNMOBR    <- 0
simDataVars$RLSEN     <- rep(0,20)
simDataVars$CMINELF   <- 0
simDataVars$CMINERT   <- 0
simDataVars$CMINESH   <- 0
simDataVars$CMINESR   <- 0
simDataVars$CMINEST   <- 0
simDataVars$NMINELF   <- 0
simDataVars$NMINERT   <- 0
simDataVars$NMINESR   <- 0
simDataVars$NMINEST   <- 0
simDataVars$SHNMINE   <- 0
simDataVars$CMOBSR    <- 0
simDataVars$NMOBSR    <- 0
simDataVars$LAIMOBR   <- 0

simDataVars$ASMDOT <- 0
simDataVars$CASM <- 0

simDataVars$ROW <- rep(0.0,6)
simDataVars$PL  <- rep(0.0,6)

#=======================================================================
#  COPYRIGHT 1998-2003 The University of Georgia, Griffin, Georgia
#                      University of Florida, Gainesville, Florida
#                      Iowa State University, Ames, Iowa
#                      International Center for Soil Fertility and 
#                       Agricultural Development, Muscle Shoals, Alabama
#                      University of Guelph, Guelph, Ontario
#  ALL RIGHTS RESERVED
#=======================================================================
#=======================================================================
#  PEST, Subroutine
#  Calculates pest damage.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  02/23/1998 CHP Written based on Pest damage code in PLANT subroutine
#  01/12/1999 GH  Incorporated into CROPGRO
#  06/19/2001 GH  Added mowing option
#  01/28/2002 GH  Expanded number of pests to 100 (from 40)
#  05/09/2003 CHP Expanded number of pests to 200 (from 100)
#-----------------------------------------------------------------------
#  Called by: CROPGRO
#                  ML_CERES
#                  MZCERES
#                  SGCERES
#  Calls:     FOR_ASMDM
#             FOR_IPPARM
#             FOR_IPPEST
#             FOR_IPPROG
#             FOR_LINDM
#             FOR_OPPEST
#             FOR_PESTCP
#             FOR_ROOTDM
#             FOR_SEEDDM
#             FOR_VEGDM
#=======================================================================
MOWING  <- function(){
  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  if (DYNAMIC == "RUNINIT") {
    #-----------------------------------------------------------------------
    #     Call FOR_IPPEST to read data from FILEIO
    #-----------------------------------------------------------------------
    # CALL FOR_IPPEST(
    #   &    FILEIO, LUNIO,                                  !Input
    #   &    FILEP, FILET, PATHPE, PHTHRS8, TRTNO)           !Output
    
    #-----------------------------------------------------------------------
    #     Subroutine FOR_IPPARM reads FILEP, the PEST progress file.
    #-----------------------------------------------------------------------
    # CALL FOR_IPPARM(
    #   &    FILEP, PATHPE,                                  !Input
    #   &    NPEST, PCPID, PCTID, PDCF1, PID)                !Output
    
    #      IF (IDETD .EQ. 'Y') THEN
    #!     Initialize (or delete existing) output file.
    #        CALL FOR_OPPEST(CONTROL, ISWITCH, 
    #     &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
    #     &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
    #     &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
    #     &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
    #      ENDIF
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  } else if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    #     Subroutine FOR_IPPROG reads FILET, the pest time series file.
    #-----------------------------------------------------------------------
    # CALL FOR_IPPROG(CONTROL, 
    #                 &    FILET, NPEST, PID, YRPLT, TRTNO,                !Input
    #                 &    IDAP, PCN, PNO, POBS, PSTHD, YPL)               !Output
    #-----------------------------------------------------------------------
    #  Initialize whole plant factors
    #-----------------------------------------------------------------------
    PESTCP(DYNAMIC)
    #-----------------------------------------------------------------------
    #  Initialize assimilate, seed, vegetative and root pest damage factors
    #-----------------------------------------------------------------------
    ASMDM(DYNAMIC, PGAVL)
    
    SEEDDM(DYNAMIC, DAS)
    
    VEGDM(DYNAMIC)
    
    ROOTDM(DYNAMIC)
    
    #IF (IDETD .EQ. 'Y') THEN
    # CALL FOR_OPPEST(CONTROL, ISWITCH, 
    #                 &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
    #                 &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
    #                 &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
    #                 &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
    #ENDIF
    
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if (DYNAMIC == "RATE") {
    #-----------------------------------------------------------------------
    #     Interpolate between pest damage factors to get today's pest level.
    #-----------------------------------------------------------------------
    # DAP   <- MAX(0,TIMDIF(YRPLT,YRDOY))  # TODO: Verificar esse DAP, criar variavel - Leandro 12/11/2020
    LINDM(DAP)
    
    #-----------------------------------------------------------------------
    #     Compute damage applied to each coupling point.
    #-----------------------------------------------------------------------
    PESTCP(DYNAMIC)
    
    SEEDDM(DYNAMIC, DAS)
    #-----------------------------------------------------------------------
    #     Call vegetative pest damage routine and compute damage rates
    #-----------------------------------------------------------------------
    VEGDM(DYNAMIC)
    #-----------------------------------------------------------------------
    #     Call root pest damage routine and compute damage rates
    #-----------------------------------------------------------------------
    ROOTDM(DYNAMIC)
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    #-----------------------------------------------------------------------
    #     Call assimilative damage routine to update assimilative damage
    #          variables.
    #-----------------------------------------------------------------------
    ASMDM(DYNAMIC, PGAVL)
    #-----------------------------------------------------------------------
    #     Call routine to apply damage to seed and shell
    #-----------------------------------------------------------------------
    SEEDDM(DYNAMIC, DAS)
    #-----------------------------------------------------------------------
    #     Call routine to apply damage to leaf and stem
    #-----------------------------------------------------------------------
    VEGDM(DYNAMIC)
    #-----------------------------------------------------------------------
    #     Call root pest damage routine and compute damage factors
    #-----------------------------------------------------------------------
    ROOTDM(DYNAMIC)
    
    #***********************************************************************
    #***********************************************************************
    #     OUTPUT/SEASEND
    #***********************************************************************
  } else if ((DYNAMIC == "OUTPUT") || (DYNAMIC == "SEASEND")) {
    #-----------------------------------------------------------------------
    #IF (IDETD .EQ. 'Y') THEN
    # CALL FOR_OPPEST(CONTROL, ISWITCH, 
    #                 &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
    #                 &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
    #                 &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
    #                 &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
    #ENDIF
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
} # SUBROUTINE PEST

#=======================================================================
#  FOR_PESTCP, Subroutine
#-----------------------------------------------------------------------
#     This subroutine computes damage to be applied to each
#     coupling point in the model.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1991 WDB Written
#  02/23/1998 CHP Modified for PEST Module
#  01/12/1999 GH  Incorporated into CROPGRO
#  06/20/2001 GH  Added mowing option
#  04/15/2002 GH  Modified number of pests to 100
#  05/09/2003 CHP Modified number of pests to 200
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:
#=======================================================================
PESTCP <- function(DYNAMIC){
  params <- plantList$forage$params
  
  PCPID <- params$PCPID
  PCTID <- params$PCTID
  PDCF1 <- params$PDCF1
  
  
  
  # PCN <- ?? # TODO: Descobrir o valor de PCN!!!!!!!!!  Tem relação com arquivo de leitura de parametros da peste 
  # PNO(6)  <-?? # TODO: Descobrir o valor de PNO!!!!!!!!! Tem relação com arquivo de leitura de parametros da peste 
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    CPPLTD <- 0.0
    PL <- rep(0.0,6)
    #-----------------------------------------------------------------------
    #     Initialization added by CHP in newer versions
    #-----------------------------------------------------------------------
    # -- Leaf Variables --
    TLFAD  = 0.0
    TLFMD  = 0.0
    PLFAD  = 0.0
    PLFMD  = 0.0
    PCLMT  = 0.0
    PCLMA  = 0.0
    PDLA   = 0.0
    TDLA   = 0.0
    PVSTGD = 0.0
    VSTGD  = 0.0
    # -- Stem Variables --
    WSTMD  = 0.0
    PSTMD  = 0.0
    PCSTMD = 0.0
    # -- Root Variables --
    WRTMD  = 0.0
    PRTMD  = 0.0
    TRTLV  = 0.0
    PRTLV  = 0.0
    PRTLF  = 0.0
    TRTLF  = 0.0
    PRLV   = 0.0
    # -- Seed Variables --
    NSDDS  = 0.0
    PSDDS  = 0.0
    NSDDM  = 0.0
    PSDDM  = 0.0
    NSDDL  = 0.0
    PSDDL  = 0.0
    WSDDS  = 0.0
    WSDDM  = 0.0
    WSDDL  = 0.0
    WSDD   = 0.0
    PSDD   = 0.0
    NSHDS  = 0.0
    NSHDM  = 0.0
    NSHDL  = 0.0
    PSHDS  = 0.0
    PSHDM  = 0.0
    PSHDL  = 0.0
    WSHDS  = 0.0
    WSHDM  = 0.0
    WSHDL  = 0.0
    # -- Whole Plant Variables --
    NPLTD  = 0.0
    PPLTD  = 0.0
    # -- Weed Leaf Area Variables --
    LAIW   = 0.0
    PLAIW  = 0.0
    # -- Photosynthesis Variables --
    TPSR   = 0.0
    PPSR   = 0.0
    # -- Other Variables --
    PDAM   = 0.0
    FSM    = 0.0
    HPDAM  = 0.0
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if(DYNAMIC == "RATE") {
    #-----------------------------------------------------------------------
    #     Initialize coupling point damage variables each day of simulation
    #-----------------------------------------------------------------------
    # -- Leaf Variables --
    TLFAD  = 0.0
    TLFMD  = 0.0
    PLFAD  = 0.0
    PLFMD  = 0.0
    PCLMT  = 0.0
    PCLMA  = 0.0
    PDLA   = 0.0
    TDLA   = 0.0
    PVSTGD = 0.0
    VSTGD  = 0.0
    # -- Stem Variables --
    WSTMD  = 0.0
    PSTMD  = 0.0
    PCSTMD = 0.0
    # -- Root Variables --
    WRTMD  = 0.0
    PRTMD  = 0.0
    TRTLV  = 0.0
    PRTLV  = 0.0
    PRTLF  = 0.0
    TRTLF  = 0.0
    PRLV   = 0.0
    # -- Seed Variables --
    NSDDS  = 0.0
    PSDDS  = 0.0
    NSDDM  = 0.0
    PSDDM  = 0.0
    NSDDL  = 0.0
    PSDDL  = 0.0
    WSDDS  = 0.0
    WSDDM  = 0.0
    WSDDL  = 0.0
    WSDD   = 0.0
    PSDD   = 0.0
    # -- Shell Variables --
    NSHDS  = 0.0
    NSHDM  = 0.0
    NSHDL  = 0.0
    PSHDS  = 0.0
    PSHDM  = 0.0
    PSHDL  = 0.0
    WSHDS  = 0.0
    WSHDM  = 0.0
    WSHDL  = 0.0
    # -- Whole Plant Variables --
    NPLTD  = 0.0
    PPLTD  = 0.0
    # -- Weed Leaf Area Variables --
    LAIW   = 0.0
    PLAIW  = 0.0
    # -- Photosynthesis Variables --
    TPSR   = 0.0
    PPSR   = 0.0
    # -- Other Variables --
    PDAM   = 0.0
    FSM    = 0.0
    HPDAM  = 0.0
    
    # -- Storage Variables --
    WSTRD  = 0.0
    PSTRD  = 0.0
    PCSTRD = 0.0
    
    #***********************************************************************
    #     PEST LOOP
    #     K increments through pests in FILET.
    #-----------------------------------------------------------------------
    for(k in 1:PCN){
      # DO 8000 K=1,PCN
      #--------------------------------------------------------------------
      #     Variable PNO(K) refers to the pest number from FILEP.
      #--------------------------------------------------------------------
      I = PNO[K]
      #-----------------------------------------------------------------------
      #     Bypass pest coupling point loop if pest damage characterization
      #         method is invalid (PCTID>4 or PCTID<1).
      #-----------------------------------------------------------------------
      if ((PCTID[I] > 0) && (PCTID[I] <= 4)) {
        REMDAM = 0.0
        #***********************************************************************
        #     COUPLING POINT LOOP
        #     J increments through coupling points in pest coefficient file.
        #-----------------------------------------------------------------------
        for(j in 1:6) {  
          # DO 7000 J=1,6
          if(PCPID[I,J] == 'xxxxx') return() #GO TO 8000
          #-----------------------------------------------------------------------
          #   Begin PCTID "IF" construct
          #   Values of PCTID:
          #   1 = Absolute daily damage rate
          #   2 = Percent Apparent observed damage
          #   3 = daily percent damage rate
          #   4 = absolute damage rate with pref and competition
          #***********************************************************************
          if (PCTID[I] == 1) {
            #-----------------------------------------------------------------------
            #    Compute damage without preference and competition
            #-----------------------------------------------------------------------
            DAM = PL[K] * PDCF1[I,J]
            #***********************************************************************
          } else if (PCTID[I] == 2) {
            #-----------------------------------------------------------------------
            #     Observed percent leaf or stem damage selected
            #-----------------------------------------------------------------
            DAM = PL[K] * PDCF1[I,J]
            #     ----------------------------------------------------------------
            #     If leaf damage is specified as percent of total leaf mass produced
            #     (WTLF + SENESENCE), then the internal damage variable is PCLMT
            #     -----------------------------------------------------------------
            
            #TODO: Verificar essas comparações  INDEX (PCPID(I,J),'SMD'), grepl(PCPID(I,J),'LMD')
            if ((!grepl(PCPID[I,J],'LMD')) || (grepl(PCPID[I,J],'LAD'))) {
              PCLMT = PCLMT + DAM
            }
            
            #     -----------------------------------------------------------------
            #     Observed percent stem damage
            #     -----------------------------------------------------------------
            if (INDEX (PCPID[I,J],'SMD') > 0) {
              PCSTMD = PCSTMD + DAM
            }
            
            #     ----------------------------------------------------------------
            #     If leaf damage is specified as percent of present leaf mass, WTLF,
            #     then the internal damage variable is PCLMA
            #     ----------------------------------------------------------------
            #     PERCENT TOTAL LEAF IN FIELD (WTLF)
            #              IF (INDEX (PCPID(I,J),'LMD1') .GT. 0 .OR. 
            #     &        INDEX (PCPID(I,J),'LAID1') .GT. 0)  THEN
            #                PCLMA = PCLMA + DAM
            #              ENDIF
            
            #************************************************************************
          } else if (PCTID[I] == 3) {
            #------------------------------------------------------------------------
            #     Apply damage as percent damage rate per day
            #     Note:  these daily percent damages should be accumulated using:
            #         (1 - a) * (1 - b) * (1 - c) where a, b and c are the individual
            #         daily percent damages.
            #-------------------------------------------------------------------------
            DAM = PL[K] * PDCF1[I,J]
            
            if (grepl(PCPID(I,J),'LAD'))  PLFAD <- PLFAD + DAM
            if (grepl(PCPID(I,J),'LMD'))  PLFMD <- PLFMD + DAM
            if (grepl(PCPID(I,J),'SMD'))  PSTMD <- PSTMD + DAM
            if (grepl(PCPID(I,J),'RMD'))  PRTMD <- PRTMD + DAM
            if (grepl(PCPID(I,J),'RLV'))  PRTLV <- PRTLV + DAM
            if (grepl(PCPID(I,J),'RLF'))  PRTLF <- PRTLF + DAM
            if (grepl(PCPID(I,J),'SDNS')) PSDDS <- PSDDS + DAM
            if (grepl(PCPID(I,J),'SDNL')) PSDDL <- PSDDL + DAM
            if (grepl(PCPID(I,J),'SDNM')) PSDDM <- PSDDM + DAM
            if (grepl(PCPID(I,J),'SDNL')) PSDDL <- PSDDL + DAM
            
            if (grepl(PCPID(I,J),'SDM'))  PSDD <- PSDD+DAM
            
            if (grepl(PCPID(I,J),'SDMS')) PSDDS <- PSDDS + DAM
            if (grepl(PCPID(I,J),'SDMM')) PSDDM <- PSDDM + DAM
            if (grepl(PCPID(I,J),'SDML')) PSDDL <- PSDDL + DAM
            if (grepl(PCPID(I,J),'SHNS')) PSHDS <- PSHDS + DAM
            if (grepl(PCPID(I,J),'SHNM')) PSHDM <- PSHDM + DAM
            if (grepl(PCPID(I,J),'SHNL')) PSHDL <- PSHDL + DAM
            if (grepl(PCPID(I,J),'SHMS')) PSHDS <- PSHDS + DAM
            if (grepl(PCPID(I,J),'SHMM')) PSHDM <- PSHDM + DAM
            if (grepl(PCPID(I,J),'SHML')) PSHDL <- PSHDL + DAM
            
            if (grepl(PCPID(I,J),'VSTG')) PVSTGD <- PVSTGD + DAM
            if (grepl(PCPID(I,J),'WPD')) {
              PPLTD  <- PPLTD + DAM
              PPLTD  <- min(100.,PPLTD)
              CPPLTD <- CPPLTD + PPLTD
              PLFMD  <- PLFMD + DAM
              PSTMD  <- PSTMD + DAM
              PRTMD  <- PRTMD + DAM
              
              PSDD  <- PSDD + DAM
              
              PSDDS <- PSDDS + DAM
              PSDDL <- PSDDL + DAM
              PSDDM <- PSDDM + DAM
              PSHDS <- PSHDS + DAM
              PSHDL <- PSHDL + DAM
              PSHDM <- PSHDM + DAM
            }
            
            
            if (grepl(PCPID(I,J),'PPLD') > 0) {
              PPLTD  <- PPLTD + DAM
              PPLTD  <- min(100.,PPLTD)
              CPPLTD <- CPPLTD + PPLTD
              PRLV   <- PRLV + DAM
              #                PLFMD = PLFMD + DAM
              #                PSTMD = PSTMD + DAM
              #                PRTMD = PRTMD + DAM
              #                PSDD = PSDD + DAM
            }
            
            if (grepl(PCPID(I,J),'PPDN') > 0) {
              PSDDS <- PSDDS + DAM
              PSDDL <- PSDDL + DAM
              PSDDM <- PSDDM + DAM
              PSHDS <- PSHDS + DAM
              PSHDL <- PSHDL + DAM
              PSHDM <- PSHDM + DAM
            }
            
            if (grepl(PCPID(I,J),'WLA') > 0) { 
              #             Note:  PLAIW is calculated here, but not used anywhere.
              PLAIW <- PLAIW + DAM
            }
            
            if (grepl(PCPID[I,J],'PDLA') >0) {
              PDLA <- PDLA + DAM
            }
            
            if (grepl(PCPID[I,J],'ASM') > 0) {
              #Accumulate daily percentages
              PPSR <- (1.0 - (1.0 - PPSR) * (1.0 - DAM))
            }
            #-----------------------------------------------------------------------
            #           Mowing option
            #-----------------------------------------------------------------------
            if (grepl( PCPID[I,J],'TOPWT') > 0.0) {
              #-----------------------------------------------------------------------
              #  SJR 3/16/04 Divide PL(K) by 10 to allow MOW input as kg/ha
              #                        instead of old g/m2
              #-----------------------------------------------------------------------
              if ((TOPWT > 0.0) && (TOPWT > PL[K]/10.)) {
                #-----------------------------------------------------------------------                
                #              Determine the fraction of the top weight that will be
                #              removed by mowing. Include an IF statement to protect
                #              against the condition that PL=0 (fileT with grazing or
                #              mowing has a sequence of zero damage - real damage -
                #              zero damage), would otherwise result in 100% damage.
                #-----------------------------------------------------------------------
                #  SJR 9/18/05 Add new variable to "remember" harvest removal for 
                #             two age-class senescence scheme - establishes "old" tissue
                #                  pool size.
                #-----------------------------------------------------------------------
                if (PL[K] > 0.0) {
                  DAM   <- (TOPWT - PL[K]/10.) / TOPWT * 100. * PDCF1[I,J]
                  HPDAM <- DAM
                } else {
                  DAM <- 0.0
                }
              } else {
                DAM <- 0.0
              }
              #-----------------------------------------------------------------------            
              #              Set the top weight to the desired value after mowing.
              #              This also corrects the LAI in subroutine VEGDM.
              #-----------------------------------------------------------------------
              PLFMD <- PLFMD + DAM
              PSTMD <- PSTMD + DAM
              #               PVSTGD = PVSTGD + DAM
            }
            #-----------------------------------------------------------------------
            #  SJR 5/19/04
            #      MVS Leaf Number reduction option - used in conjunction with MOW
            #-----------------------------------------------------------------------
            if (grepl( PCPID[I,J],'NOLF') > 0.0) {
              #-----------------------------------------------------------------------
              #  SJR 5/19/04 PL(K) input as number of leaves left after mowing
              #-----------------------------------------------------------------------
              if ((VSTAGE > 0.0) && (VSTAGE > PL[K])) {
                #-----------------------------------------------------------------------                
                #              Determine the fraction of leaves that will be
                #              removed by mowing. Include an IF statement to protect
                #              against the condition that PL=0 (fileT with grazing or
                #              mowing has a sequence of zero damage - real damage -
                #              zero damage), would otherwise result in 100% damage.
                #-----------------------------------------------------------------------
                if (PL[K] > 0.0) {
                  DAM <- (VSTAGE - PL[K]) / VSTAGE * 100. * PDCF1[I,J]
                } else {
                  DAM <- 0.0
                }
              } else {
                DAM <- 0.0
              }
              #-----------------------------------------------------------------------            
              #              Set the VStage to the desired value after mowing.
              #-----------------------------------------------------------------------
              #               PLFMD = PLFMD + DAM
              #               PSTMD = PSTMD + DAM
              PVSTGD <- PVSTGD + DAM
            }
            #***********************************************************************
          } else if (PCTID[I] == 4) {
            #-----------------------------------------------------------------------
            #  Compute preference and competition
            #  If there is not a coupling point following this one, then
            #  there is not a subsequent preference.  Thus, do not apply
            #  the competition algorithm.  Apply remaining damage to the
            #  current coupling point.
            #-----------------------------------------------------------------------
            DAM  <- 0.
            PDAM <- 0.
            DDAM <- 0.
            if(PL[K] < 0.0001) break()#GOTO 7000
            #-----------------------------------------------------------------------
            #     Compute food source mass or number available for consumption
            #-----------------------------------------------------------------------
            FSM <- 0
            if (grepl(PCPID[I,J],'LAD')  > 0) FSM <- WTLF*SLA/10000.
            if (grepl(PCPID[I,J],'LMD')  > 0) FSM <- WTLF
            if (grepl(PCPID[I,J],'SMD')  > 0) FSM <- STMWT
            if (grepl(PCPID[I,J],'RMD')  > 0) FSM <- RTWT
            if (grepl(PCPID[I,J],'SDNS') > 0) FSM <- TSDNOS
            if (grepl(PCPID[I,J],'SDNL') > 0) FSM <- TSDNOL
            if (grepl(PCPID[I,J],'SDNM') > 0) FSM <- TSDNOM
            if (grepl(PCPID[I,J],'SDMS') > 0) FSM <- TSDWTS
            if (grepl(PCPID[I,J],'SDML') > 0) FSM <- TSDWTL
            if (grepl(PCPID[I,J],'SDMM') > 0) FSM <- TSDWTM
            if (grepl(PCPID[I,J],'SHNS') > 0) FSM <- TSHNOS
            if (grepl(PCPID[I,J],'SHNL') > 0) FSM <- TSHNOL
            if (grepl(PCPID[I,J],'SHNM') > 0) FSM <- TSHNOM
            if (grepl(PCPID[I,J],'SHMS') > 0) FSM <- TSHWTS
            if (grepl(PCPID[I,J],'SHML') > 0) FSM <- TSHWTL
            if (grepl(PCPID[I,J],'SHMM') > 0) FSM <- TSHWTM
            if (grepl(PCPID[I,J],'VSTG') > 0) FSM <- VSTAGE
            
            #-----------------------------------------------------------------------
            #     For first coupling point, potential damage (DDAM) equals entire
            #     pest level times the feeding rate.  For subsequent coupling
            #     points, potential damage equals remaining damage available.
            #-----------------------------------------------------------------------
            if (J == 1) {
              DDAM <- PL[K] * PDCF1[I,J]
            } else {
              DDAM <- REMDAM
            }
            #-----------------------------------------------------------------------
            #    Compute actual damage to be applied (DAM)
            #-----------------------------------------------------------------------
            if (FSM > 0.0001) {
              PDAM <- exp(-DDAM/FSM)
              DAM  <- FSM * (1. - PDAM)
            }
            #-----------------------------------------------------------------------
            #   REMDAM = Remaining potential damage for next coupling point
            #-----------------------------------------------------------------------
            if (PCPID[I,J+1] != 'xxxxx') {
              REMDAM <- (DDAM-DAM)*PDCF1[I,J+1]/PDCF1[I,J]
            } else {
              REMDAM <- 0
            }
            #***********************************************************************
          }     #End of PCTID "IF" construct
          #***********************************************************************
          
          #***********************************************************************
          if ((PCTID[I] == 1) || (PCTID[I] == 4)) {
            #-----------------------------------------------------------------------
            #     Apply damage in absolute units with or without pest preference
            #     and competition effects.
            #-----------------------------------------------------------------------
            if (grepl(PCPID[I,J],'LMD')  > 0) TLFMD <- TLFMD + DAM
            if (grepl(PCPID[I,J],'LAD')  > 0) TLFAD <- TLFAD + DAM
            if (grepl(PCPID[I,J],'PDLA') > 0) TDLA  <- TDLA + DAM
            if (grepl(PCPID[I,J],'SMD')  > 0) WSTMD <- WSTMD + DAM
            if (grepl(PCPID[I,J],'RMD')  > 0) WRTMD <- WRTMD + DAM
            if (grepl(PCPID[I,J],'RLV')  > 0) TRTLV <- TRTLV + DAM
            if (grepl(PCPID[I,J],'RLF')  > 0) TRTLF <- TRTLF + DAM
            if (grepl(PCPID[I,J],'SDNS') > 0) NSDDS <- NSDDS + DAM
            if (grepl(PCPID[I,J],'SDNM') > 0) NSDDM <- NSDDM + DAM
            if (grepl(PCPID[I,J],'SDNL') > 0) NSDDL <- NSDDL + DAM
            
            if (grepl(PCPID[I,J],'SDM')  > 0) WSDD  <- WSDD  + DAM
            
            if (grepl(PCPID[I,J],'SDMS') > 0) WSDDS <- WSDDS + DAM
            if (grepl(PCPID[I,J],'SDMM') > 0) WSDDM <- WSDDM + DAM
            if (grepl(PCPID[I,J],'SDML') > 0) WSDDL <- WSDDL + DAM
            if (grepl(PCPID[I,J],'SHNS') > 0) NSHDS <- NSHDS + DAM
            if (grepl(PCPID[I,J],'SHNM') > 0) NSHDM <- NSHDM + DAM
            if (grepl(PCPID[I,J],'SHNL') > 0) NSHDL <- NSHDL + DAM
            if (grepl(PCPID[I,J],'SHMS') > 0) WSHDS <- WSHDS + DAM
            if (grepl(PCPID[I,J],'SHMM') > 0) WSHDM <- WSHDM + DAM
            if (grepl(PCPID[I,J],'SHML') > 0) WSHDL <- WSHDL + DAM
            #        Note:  LAIW is calculated here, but not used anywhere.
            if (grepl(PCPID[I,J],'WLA')  > 0) LAIW  <- LAIW + DAM
            
            if (grepl(PCPID[I,J],'VSTG') > 0) VSTGD <- VSTGD + DAM
            if (grepl(PCPID[I,J],'ASM')  > 0) TPSR  <- TPSR + DAM
            if (grepl(PCPID[I,J],'WPD')  > 0) {
              NPLTD <- DAM
              if (PLTPOP > 0.0) {
                PPLTD <- PPLTD + 100.0 * NPLTD/PLTPOP
              }
              PPLTD  <- min(PPLTD,100.0)
              CPPLTD <- CPPLTD + PPLTD
              PLFMD  <- PLFMD + PPLTD
              PSTMD  <- PSTMD + PPLTD
              PRTMD  <- PRTMD + PPLTD
              
              #                PSDD = PSDD + PPLTD
              
              PSDDS <- PSDDS + PPLTD
              PSDDL <- PSDDL + PPLTD
              PSDDM <- PSDDM + PPLTD
              PSHDS <- PSHDS + PPLTD
              PSHDL <- PSHDL + PPLTD
              PSHDM <- PSHDM + PPLTD
            }
            if (grepl(PCPID[I,J],'WPDM')  > 0) {
              NPLTD <- DAM
              if (PLTPOP > 0.0) {
                PPLTD <- PPLTD + 100.0 * NPLTD/PLTPOP
              }
              PPLTD  <- min(PPLTD,100.0)
              CPPLTD <- CPPLTD + PPLTD
              #                PLFMD = PLFMD + PPLTD
              #                PSTMD = PSTMD + PPLTD
              #                PRTMD = PRTMD + PPLTD
              #                PSDD = PSDD + PPLTD
            }
          }     #End of PCTID = 1 or 4 if-construct
          #-----------------------------------------------------------------------
        }      #End of Coupling Points Loop
      }         #End of bypass for invalid pest damage method
    }           #End of Pest loop
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  
  assign("TPSRLYR1",TPSRLYR1 , envir = env)
  assign("PL",     PL    , envir = env)
  assign("TLFAD",  TLFAD , envir = env)
  assign("PLFAD",  PLFAD , envir = env)
  assign("TLFMD",  TLFMD , envir = env)
  assign("PLFMD",  PLFMD , envir = env)
  assign("PCLMT",  PCLMT , envir = env)
  assign("PCLMA",  PCLMA , envir = env)
  assign("PDLA",   PDLA  , envir = env)
  assign("TDLA",   TDLA  , envir = env)
  assign("VSTGD",  VSTGD , envir = env)
  assign("PVSTGD", PVSTGD, envir = env)
  assign("WSTMD",  WSTMD , envir = env)
  assign("PSTMD",  PSTMD , envir = env)
  assign("PCSTMD", PCSTMD, envir = env)
  assign("WRTMD",  WRTMD , envir = env)
  assign("PRTMD",  PRTMD , envir = env)
  assign("TRTLV",  TRTLV , envir = env)
  assign("PRTLV",  PRTLV , envir = env)
  assign("PRTLF",  PRTLF , envir = env)
  assign("TRTLF",  TRTLF , envir = env)
  assign("PRLV",   PRLV  , envir = env)
  assign("NSDDS",  NSDDS , envir = env)
  assign("PSDDS",  PSDDS , envir = env)
  assign("NSDDL",  NSDDL , envir = env)
  assign("PSDDL",  PSDDL , envir = env)
  assign("NSDDM",  NSDDM , envir = env)
  assign("PSDDM",  PSDDM , envir = env)
  assign("WSDDS",  WSDDS , envir = env)
  assign("WSDDL",  WSDDL , envir = env)
  assign("WSDDM",  WSDDM , envir = env)
  assign("PSDD",   PSDD  , envir = env)
  assign("WSDD",   WSDD  , envir = env)
  assign("NSHDS",  NSHDS , envir = env)
  assign("NSHDL",  NSHDL , envir = env)
  assign("NSHDM",  NSHDM , envir = env)
  assign("PSHDS",  PSHDS , envir = env)
  assign("PSHDL",  PSHDL , envir = env)
  assign("PSHDM",  PSHDM , envir = env)
  assign("WSHDS",  WSHDS , envir = env)
  assign("WSHDL",  WSHDL , envir = env)
  assign("WSHDM",  WSHDM , envir = env)
  assign("NPLTD",  NPLTD , envir = env)
  assign("PPLTD",  PPLTD , envir = env)
  assign("CPPLTD", CPPLT , envir = env)
  assign("TPSR",   TPSR  , envir = env)
  assign("PPSR",   PPSR  , envir = env)
  assign("LAIW",   LAIW  , envir = env)
  assign("PLAIW",  PLAIW , envir = env)
  assign("PCSTRD", PCSTRD, envir = env)
  assign("PSTRD",  PSTRD , envir = env)
  assign("WSTRD",  WSTRD , envir = env)
  assign("HPDAM",  HPDAM , envir = env)
  assign("TOPWT",  TOPWT , envir = env)
  
  return()
}   # SUBROUTINE FOR_PESTCP

#=======================================================================
#  FOR_SEEDDM, Subroutine
#  Calculates pest damage in seed and shells.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1990 WDB Written
#  02/23/1998 CHP Modified for PEST Module
#  01/12/1999 GH  Incorporated into CROPGRO
#  06/06/2001 CHP Correction for 0 divide
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:     None
#=======================================================================
SEEDDM <- function (DYNAMIC, DAS){
  params <- plantList$forage$params
  
  PHTHRS8 <- params$PHTHRS8
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    for (I in 1:NCOHORTS) {
      SDWDES[I] <- 0.0
      SDNDES[I] <- 0.0
      SHWDES[I] <- 0.0
      SHNDES[I] <- 0.0
      SDDES[I]  <- 0.0
    }
    
    SDIDOT <- 0.0
    SHIDOT <- 0.0
    SWIDOT <- 0.0
    WSHIDT <- 0.0
    
    TSDWTS <- 0.0
    TSDNOS <- 0.0
    TSHWTS <- 0.0
    TSHNOS <- 0.0
    
    TSDWTL <- 0.0
    TSDNOL <- 0.0
    TSHWTL <- 0.0
    TSHNOL <- 0.0
    
    TSDWTM <- 0.0
    TSDNOM <- 0.0
    TSHWTM <- 0.0
    TSHNOM <- 0.0
    
    CSDM <- 0.
    CSDN <- 0.
    CSHM <- 0.
    CSHN <- 0.
    
    TSDNO <- 0.
    TSDWT <- 0.
    TSHNO <- 0.
    TSHWT <- 0.
    
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if (DYNAMIC == "RATE") {
    #-----------------------------------------------------------------------
    SDIDOT <- 0.0
    SHIDOT <- 0.0
    SWIDOT <- 0.0
    WSHIDT <- 0.0
    
    SDWDES <- rep(0,300)    #
    SDNDES <- rep(0,300)    #Arrays
    SHWDES <- rep(0,300)    #
    SHNDES <- rep(0,300)    #
    
    TSDWTS <- 0.0
    TSDNOS <- 0.0
    TSHWTS <- 0.0
    TSHNOS <- 0.0
    
    TSDWTL <- 0.0
    TSDNOL <- 0.0
    TSHWTL <- 0.0
    TSHNOL <- 0.0
    
    TSDWTM <- 0.0
    TSDNOM <- 0.0
    TSHWTM <- 0.0
    TSHNOM <- 0.0
    
    #-----------------------------------------------------------------------
    #     Compute total seed and shell mass and number for small, large and
    #         mature pod classes.
    #-----------------------------------------------------------------------
    for (NPP in 1:(DAS-NR2-1)) {
      PAGE <- PHTIM[DAS-NR2] - PHTIM[NPP]
      #-----------------------------------------------------------------------
      #     Count small seed cohorts less than LAGSD (R3) old
      #-----------------------------------------------------------------------
      if (WTSD[NPP] < 0.001 & SDNO[NPP] > 0.0) {
        TSDWTS <- TSDWTS + WTSD[NPP]
        TSDNOS <- TSDNOS + SDNO[NPP]
        TSHWTS <- TSHWTS + WTSHE[NPP]
        TSHNOS <- TSHNOS + SHELN[NPP]
      }
      #-----------------------------------------------------------------------
      #     Count large seed cohorts between small and full pod (R6)
      #-----------------------------------------------------------------------
      if (WTSD[NPP] > 0.0 & PAGE >= LAGSD & PAGE < PHTHRS8) {
        TSDWTL <- TSDWTL + WTSD[NPP]
        TSDNOL <- TSDNOL + SDNO[NPP]
        TSHWTL <- TSHWTL + WTSHE[NPP]
        TSHNOL <- TSHNOL + SHELN[NPP]
      }
      #-----------------------------------------------------------------------
      #    Count mature cohorts that are older than full pod (R6)
      #-----------------------------------------------------------------------
      if (PAGE > PHTHRS8) {
        TSDWTM <- TSDWTM + WTSD[NPP]
        TSDNOM <- TSDNOM + SDNO[NPP]
        TSHWTM <- TSHWTM + WTSHE[NPP]
        TSHNOM <- TSHNOM + SHELN[NPP]
      }
    }
    
    TSDNOS <- max(0.,TSDNOS)
    TSDWTS <- max(0.,TSDWTS)
    TSHNOS <- max(0.,TSHNOS)
    TSHWTS <- max(0.,TSHWTS)
    
    TSDNOL <- max(0.,TSDNOL)
    TSDWTL <- max(0.,TSDWTL)
    TSHNOL <- max(0.,TSHNOL)
    TSHWTL <- max(0.,TSHWTL)
    
    TSDNOM <- max(0.,TSDNOM)
    TSDWTM <- max(0.,TSDWTM)
    TSHNOM <- max(0.,TSHNOM)
    TSHWTM <- max(0.,TSHWTM)
    #-----------------------------------------------------------------------
    #      Compute total seed and shell number and mass in the damaged
    #      size class (These values not used - chp)
    #-----------------------------------------------------------------------
    TSDNO <- TSDNOS + TSDNOL + TSDNOM
    TSDWT <- TSDWTS + TSDWTL + TSDWTM
    TSHNO <- TSHNOS + TSHNOL + TSHNOM
    TSHWT <- TSHWTS + TSHWTL + TSHWTM
    
    #-----------------------------------------------------------------------
    #     Loop through cohorts and apply damage
    #-----------------------------------------------------------------------
    for (NPP in 1:(DAS-NR2-1)) {
      PAGE <- PHTIM[DAS-NR2] - PHTIM[NPP]
      
      #-----------------------------------------------------------------------
      #    Apply damage as percent of seed number or seed mass
      #-----------------------------------------------------------------------
      if (PSDDS > 0.0 & WTSD[NPP] < 0.0001 & SDNO[NPP] > 0.0) {
        SDWDES[NPP] <- SDWDES[NPP] + PSDDS * WTSD[NPP] / 100.0
        SDNDES[NPP] <- SDNDES[NPP] + PSDDS * SDNO[NPP] / 100.0
        if (PSHDS < 0.001 & NSHDS < 0.001) {
          SDDES[NPP] <- SDDES[NPP] + PSDDS * SDNO[NPP] / 100.0
        }
      }
      
      if (PSDDL > 0.0 & PAGE >= LAGSD & PAGE < PHTHRS8) {
        SDWDES[NPP] <- SDWDES[NPP] + PSDDL * WTSD[NPP] / 100.0
        SDNDES[NPP] <- SDNDES[NPP] + PSDDL * SDNO[NPP] / 100.0
        if (PSHDL < 0.001 & NSHDL < 0.001) {
          SDDES[NPP] <- SDDES[NPP] + PSDDL * SDNO[NPP]/100.0
        }
      }
      
      if (PSDDM > 0.0 & PAGE > PHTHRS8) {
        SDWDES[NPP] <- SDWDES[NPP] + PSDDM * WTSD[NPP] / 100.0
        SDNDES[NPP] <- SDNDES[NPP] + PSDDM * SDNO[NPP] / 100.0
        if (PSHDM < 0.001 & NSHDM < 0.001) {
          SDDES[NPP] <- SDDES[NPP]+ PSDDM * SDNO[NPP]/100.0
        }
      }
      
      #-----------------------------------------------------------------------
      #     Apply damage as number of seed/m2
      #-----------------------------------------------------------------------
      if (NSDDS > 0.0 & WTSD[NPP] < 0.0001 & SDNO[NPP] > 0.0 & TSDNOS > 0.0) {
        NSDDS <- min(NSDDS,TSDNOS)
        SDWDES[NPP] <- SDWDES[NPP] + WTSD[NPP] * NSDDS/TSDNOS
        SDNDES[NPP] <- SDNDES[NPP] + SDNO[NPP] * NSDDS/TSDNOS
        if (PSHDS < 0.001 & NSHDS < 0.001) {
          SDDES[NPP] <- SDDES[NPP] +  NSDDS * SDNO[NPP]/TSDNOS
        }
      }
      
      if (NSDDL > 0.0 & PAGE >= LAGSD & PAGE < PHTHRS8 & TSDNOL > 0.0) {
        NSDDL <- min(NSDDL,TSDNOL)
        SDWDES[NPP] <- SDWDES[NPP] + WTSD[NPP] * NSDDL/TSDNOL
        SDNDES[NPP] <- SDNDES[NPP] + SDNO[NPP] * NSDDL/TSDNOL
        if (PSHDL < 0.001 & NSHDM < 0.001) {
          SDDES[NPP] <- SDDES[NPP] +  NSDDL * SDNO[NPP]/TSDNOL
        }
      }
      
      if (NSDDM > 0.0 & PAGE > PHTHRS8 & TSDNOM > 0.0) {
        NSDDM <- min(NSDDM,TSDNOM)
        SDWDES[NPP] <- SDWDES[NPP] + WTSD[NPP] * NSDDM/TSDNOM
        SDNDES[NPP] <- SDNDES[NPP] + SDNO[NPP] * NSDDM/TSDNOM
        if (PSHDM < 0.001 & NSHDM < 0.001) {
          SDDES[NPP] <- SDDES[NPP] + NSDDM * SDNO[NPP]/TSDNOM
        }
      }
      
      #-----------------------------------------------------------------------
      #     Apply damage as mass of seed
      #-----------------------------------------------------------------------
      if ( !(WTSD[NPP] <= 0.001) ) { #GOTO 1000
        #TODO checar se fiz correto aqui, apenas deixando em branco
        WPS <- SDNO[NPP]/WTSD[NPP]
        
        if (WSDDS > 0.0 & PAGE < LAGSD & PAGE > LNGPEG & TSDNOS > 0.0) {
          WSDDS <- min(WSDDS,TSDWTS)
          SDWDES[NPP] <- SDWDES[NPP] + WSDDS*SDNO[NPP]/TSDNOS
          SDNDES[NPP] <- SDNDES[NPP] + WSDDS*SDNO[NPP]*WPS/TSDNOS
          if (PSHDS > 0.0 & NSHDS > 0.0 & TSDWTS > 0.0001) {
            SDDES[NPP] <- SDDES[NPP] + WPS*WSDDS*SDNO[NPP]/TSDNOS
          }
        }
        
        if (WSDDL > 0.0 & PAGE >= LAGSD & PAGE < PHTHRS8 & TSDNOL > 0.0) {
          WSDDL <- min(WSDDL,TSDWTL)
          SDWDES[NPP] <- SDWDES[NPP] + WSDDL*SDNO[NPP]/TSDNOL
          SDNDES[NPP] <- SDNDES[NPP] + WSDDL*SDNO[NPP]*WPS/TSDNOL
          if (PSHDL > 0.0 & NSHDL > 0.0) {
            SDDES[NPP] <- SDDES[NPP] + WPS*WSDDL*SDNO[NPP]/TSDNOL
          }
        }
        
        if (WSDDM > 0.0 & PAGE > PHTHRS8 & TSDNOM > 0.0) {
          WSDDM <- min(WSDDM,TSDWTM)
          SDWDES[NPP] <- SDWDES[NPP] + WSDDM*SDNO[NPP]/TSDNOM
          SDNDES[NPP] <- SDNDES[NPP] + WSDDM*SDNO[NPP]*WPS/TSDNOM
          if (PSHDM > 0.0 & NSHDM > 0.0) {
            SDDES[NPP] <- SDDES[NPP] + WPS*WSDDM*SDNO[NPP]/TSDNOM
          }
        }
        
      }
      
      #-----------------------------------------------------------------------
      #        Apply damage as percent shell mass
      #-----------------------------------------------------------------------
      if (PSHDS > 0.0 & PAGE > LNGPEG & PAGE < LAGSD) {
        SHWDES[NPP] <- SHWDES[NPP] + PSHDS * WTSHE[NPP] / 100.0
        SHNDES[NPP] <- SHNDES[NPP] + PSHDS * SHELN[NPP] / 100.0
      }
      
      if(PSHDL > 0.0 & PAGE >= LAGSD & PAGE < PHTHRS8) {
        SHWDES[NPP] <- SHWDES[NPP] + PSHDL * WTSHE[NPP] / 100.0
        SHNDES[NPP] <- SHNDES[NPP] + PSHDL * SHELN[NPP] / 100.0
      }
      
      if(PSHDM > 0.0 & PAGE >= PHTHRS8) {
        SHWDES[NPP] <- SHWDES[NPP] + PSHDM * WTSHE[NPP] / 100.0
        SHNDES[NPP] <- SHNDES[NPP] + PSHDM * SHELN[NPP] / 100.0
      }
      
      #-----------------------------------------------------------------------
      #     Apply damage as number of shells /m2
      #-----------------------------------------------------------------------
      if (NSHDS > 0.0 & PAGE > LNGPEG & PAGE < LAGSD & TSHNOS > 0.0) {
        NSHDS <- min(NSHDS,TSHNOS)
        SHWDES[NPP] <- SHWDES[NPP] + WTSHE[NPP]*NSHDS/TSHNOS
        SHNDES[NPP] <- SHNDES[NPP] + SHELN[NPP]*NSHDS/TSHNOS
      }
      
      if (NSHDL > 0.0 & PAGE > LAGSD & PAGE < PHTHRS8 & TSHNOL > 0) {
        NSHDL <- min(NSHDL,TSHNOL)
        SHWDES[NPP] <- SHWDES[NPP] + WTSHE[NPP]*NSHDL/TSHNOL
        SHNDES[NPP] <- SHNDES[NPP] + SHELN[NPP]*NSHDL/TSHNOL
      }
      
      if (NSHDM > 0.0 & PAGE > PHTHRS8 & TSHNOM > 0.0) {
        NSHDM <- min(NSHDM,TSHNOM)
        SHWDES[NPP] <- SHWDES[NPP] + WTSHE[NPP]*NSHDM/TSHNOM
        SHNDES[NPP] <- SHNDES[NPP] + SHELN[NPP]*NSHDM/TSHNOM
      }
      
      #-----------------------------------------------------------------------
      #     Apply damage as weight of shell /m2
      #-----------------------------------------------------------------------
      if(WSHDS > 0.0 & PAGE > LNGPEG & PAGE < LAGSD & TSHNOS > 0.0) {
        WSHDS <- min(WSHDS,TSHWTS)
        SHWDES[NPP] <- SHWDES[NPP] + WSHDS*WTSHE[NPP]/TSHWTS
        DEST <- WSHDS * WTSHE[NPP]/TSHWTS
        SHNDES[NPP] <- SHNDES[NPP] + DEST*SHELN[NPP]/WTSHE[NPP]
      }
      
      if (WSHDL > 0.0 & PAGE > LAGSD & PAGE < PHTHRS8 & TSHNOL > 0.0) {
        WSHDL <- min(WSHDL,TSHWTL)
        SHWDES[NPP] <- SHWDES[NPP] + WSHDL*WTSHE[NPP]/TSHWTL
        DEST <- WSHDL * WTSHE[NPP]/TSHWTL
        SHNDES[NPP] <- SHNDES[NPP] + DEST*SHELN[NPP]/WTSHE[NPP]
      }
      
      if (WSHDM > 0.0 & PAGE > PHTHRS8 & TSHNOM > 0.0) {
        WSHDM <- min(WSHDM,TSHWTM)
        SHWDES[NPP] <- SHWDES[NPP] + WSHDM*WTSHE[NPP]/TSHWTM
        DEST <- WSHDM * WTSHE[NPP]/TSHWTM
        SHNDES[NPP] <- SHNDES[NPP] + DEST*SHELN[NPP]/WTSHE[NPP]
      }
      
      #       Check that mass and number destroyed are not greater than
      #             actual mass and number.
      SDWDES[NPP] <- min(SDWDES[NPP], WTSD[NPP])
      SDNDES[NPP] <- min(SDNDES[NPP], SDNO[NPP])
      SHWDES[NPP] <- min(SHWDES[NPP], WTSHE[NPP])
      SHNDES[NPP] <- min(SHNDES[NPP], SHELN[NPP])
      
      #       Update daily total mass and number of seed and shell destroyed.
      SWIDOT <- SWIDOT + SDWDES[NPP]   
      SDIDOT <- SDIDOT + SDNDES[NPP]   
      WSHIDT <- WSHIDT + SHWDES[NPP]   
      SHIDOT <- SHIDOT + SHNDES[NPP]    
      
    }       #End of cohort loop
    
    
    
    #----------------------------------------------------------------
    # This section was added to compute seed damage in CERES MAIZE.
    # The main difference is that CERES assumes all seeds are the
    # same age, thus there are no seed chorts. 
    #
    # Apply damage as absolute (g seed/m2/d)
    if(WSDD>0) {
      SWIDOT <- SWIDOT + WSDD
    }
    
    # Apply damage as percent (%/d)
    if (PSDD > 0) {
      SWIDOT <- SWIDOT + SDWT*(PSDD/100)
    }
    
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  }      else if (DYNAMIC == "INTEGR") {
    #-----------------------------------------------------------------------
    CSDM <- CSDM + SWIDOT
    CSDN <- CSDN + SDIDOT
    CSHM <- CSHM + WSHIDT
    CSHN <- CSHN + SHIDOT
    
    for (NPP in 1:(DAS-NR2-1)) {
      PAGE <- PHTIM[DAS - NR2] - PHTIM[NPP]
      WTSD[NPP]  <- max(0., WTSD[NPP]  - SDWDES[NPP])
      SDNO[NPP]  <- max(0., SDNO[NPP]  - SDNDES[NPP])
      WTSHE[NPP] <- max(0., WTSHE[NPP] - SHWDES[NPP])
      SHELN[NPP] <- max(0., SHELN[NPP] - SHNDES[NPP])
    }
    
    #***********************************************************************
  }
  #***********************************************************************
  #***********************************************************************
  #     END OF DYNAMIC IF CONSTRUCT
  #***********************************************************************
  assign("WTSD", WTSD , envir = env)
  assign("WTSHE", WTSHE , envir = env)
  assign("SDNO", SDNO , envir = env)
  assign("SHELN", SHELN , envir = env)
  assign("SWIDOT", SWIDOT , envir = env)
  assign("WSHIDT", WSHIDT , envir = env)
  assign("NSDDS", NSDDS , envir = env)
  assign("NSDDL", NSDDL , envir = env)
  assign("NSDDM", NSDDM , envir = env)
  assign("WSDDS", WSDDS , envir = env)
  assign("WSDDL", WSDDL , envir = env)
  assign("WSDDM", WSDDM , envir = env)
  assign("TSDNOS", TSDNOS , envir = env)
  assign("TSDNOL", TSDNOL , envir = env)
  assign("TSDNOM", TSDNOM , envir = env)
  assign("TSDWTS", TSDWTS , envir = env)
  assign("TSDWTL", TSDWTL , envir = env)
  assign("TSDWTM", TSDWTM , envir = env)
  assign("SDDES", SDDES , envir = env)
  assign("NSHDS", NSHDS , envir = env)
  assign("NSHDS", NSHDS , envir = env)
  assign("NSHDL", NSHDL , envir = env)
  assign("NSHDM", NSHDM , envir = env)
  assign("WSHDS", WSHDS , envir = env)
  assign("WSHDL", WSHDL , envir = env)
  assign("WSHDM", WSHDM , envir = env)
  assign("TSHNOS", TSHNOS , envir = env)
  assign("TSHNOL", TSHNOL , envir = env)
  assign("TSHNOM", TSHNOM , envir = env)
  assign("TSHWTS", TSHWTS , envir = env)
  assign("TSHWTL", TSHWTL , envir = env)
  assign("TSHWTM", TSHWTM , envir = env)
  assign("SDIDOT", SDIDOT , envir = env)
  assign("SHIDOT", SHIDOT , envir = env)
  assign("CSDM", CSDM     , envir = env)
  assign("CSDN", CSDN     , envir = env)
  assign("SDWDES", SDWDES , envir = env)
  assign("SDNDES", SDNDES , envir = env)
  assign("SHWDES", SHWDES , envir = env)
  assign("SHNDES", SHNDES , envir = env)
  assign("CSHM", CSHM     , envir = env)
  assign("CSHN", CSHN     , envir = env)
  return()
  #END   # SUBROUTINE FOR_SEEDDM
}

#=======================================================================
#  FOR_ROOTDM, Subroutine
#  Calculates root damage due to pests.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/90 WDB Written
#  03/02/98 CHP Modified for PEST Module
#  01/12/99 GH  Incorporated into CROPGRO
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:     None
#=======================================================================
ROOTDM <- function (DYNAMIC) { 
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    CRLV = 0
    CRLF = 0
    CRTM = 0
    
    #-----------------------------------------------------------------------
    #     Compute soil profile depth (cm)
    #-----------------------------------------------------------------------
    CUMDEP = 0.0
    for (L in 1:NLAYR) {
      CUMDEP = CUMDEP + DLAYR[L]
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if (DYNAMIC == "RATE") {
    #-----------------------------------------------------------------------
    RLVDOT = 0.0
    RLFDOT = 0.0
    WRIDOT = 0.0
    #-----------------------------------------------------------------------
    #     Compute total root length volume
    #-----------------------------------------------------------------------
    TRLV = 0.0
    for (L in 1:NLAYR) {                          
      TRLV = TRLV + RLV[L]* DLAYR[L]          
    }                                     
    if (TRLV <= 0.0001) return()  
    
    #     Calculate fraction of root mass in each soil layer
    #     This will be used to distribute RLV changes
    RTFRAC <- rep(0, NLAYR)
    for (L in 1:NLAYR) {
      RTFRAC[L] = RLV[L]*DLAYR[L]/TRLV
    }
    
    #-----------------------------------------------------------------------
    #     Percent root mass destroyed
    #-----------------------------------------------------------------------
    if (PRTMD > 0.0) {
      WRIDOT = WRIDOT + (PRTMD/100.) * RTWT 
      RLVDOT = RLVDOT + (PRTMD/100.) * (TRLV/CUMDEP)
    }
    #-----------------------------------------------------------------------
    #     Totol root mass destroyed  g/m2/day
    #-----------------------------------------------------------------------
    if (WRTMD > 0.0) {
      WRTMD = min(WRTMD,RTWT)
      WRIDOT = WRIDOT + WRTMD 
      RLVDOT = RLVDOT + (WRTMD/RTWT) * (TRLV/CUMDEP) 
    }
    #-----------------------------------------------------------------------
    #     Percent root length volume damaged (cm/cm3)
    #-----------------------------------------------------------------------
    if (PRTLV > 0.0) {
      WRIDOT = WRIDOT + (PRTLV/100.) * RTWT 
      RLVDOT = RLVDOT + (PRTLV/100.) * (TRLV/CUMDEP)
    }
    #-----------------------------------------------------------------------
    #     Total root length volume damaged (cm/cm3)
    #-----------------------------------------------------------------------
    if (TRTLV > 0.0) {
      TRTLV = min(TRTLV,TRLV)
      WRIDOT = WRIDOT + RTWT * TRTLV/(TRLV/CUMDEP)
      RLVDOT = RLVDOT + TRTLV 
    }
    #-----------------------------------------------------------------------
    #   Percent root length flux (cm/cm2) destroyed
    #-----------------------------------------------------------------------
    if (PRTLF > 0.0) {
      WRIDOT = WRIDOT + (PRTLF/100.) * RTWT 
      RLVDOT = RLVDOT + (PRTLF/100.) * (TRLV/CUMDEP)
    }
    #-----------------------------------------------------------------------
    #   Total root length flux, cm/cm2 destroyed
    #-----------------------------------------------------------------------
    if (TRTLF > 0.0 & TRLV > 0.0) {
      WRIDOT = WRIDOT + (TRTLF/TRLV) * RTWT
      RLVDOT = RLVDOT + (TRTLF/CUMDEP) 
    }
    
    WRIDOT = max(0.0, min(WRIDOT, RTWT))
    RLVDOT = max(0.0, min(RLVDOT, TRLV/CUMDEP))
    RLFDOT = RLVDOT * CUMDEP
    
    #-----------------------------------------------------------------------
    #     When population is reduced in ceres-maize, root mass/m2 is reduced
    #     through reduction in population directly. The following allows
    #     root length volume to be adjusted separately
    
    #     Percent root length volume damaged (cm/cm3)
    #-----------------------------------------------------------------------
    if (PRLV > 0.0) {
      #        WRIDOT = WRIDOT + (PRTLV/100.) * RTWT 
      RLVDOT = RLVDOT + (PRLV/100.) * (TRLV/CUMDEP)
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    #-----------------------------------------------------------------------
    #     RLVDOT is distributed to layers by root mass fraction
    #     (RTFRAC) regardless of pest type or method.
    if (RLVDOT > 0.0) {
      for (L in 1:NLAYR) {
        RLV[L] = RLV[L] - RLVDOT * CUMDEP * RTFRAC[L] / DLAYR[L]
        #CUMDEP converts from per volume to per unit area 
        #RTFRAC(L) proportions among layers
        #Division by DLAYR(L) converts back to per volume basis for
        #  each soil layer
        RLV[L] = max(0.0, RLV[L])
      }
    }
    
    #-----------------------------------------------------------------------
    #     Compute cumulative root length mass, voluem, and flux
    #     and daily root length flux (cm root / cm2 ground) damage
    #-----------------------------------------------------------------------
    CRTM = CRTM + WRIDOT
    CRLV = CRLV + RLVDOT
    CRLF = CRLF + RLFDOT
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("WRTMD", WRTMD     , envir = env)
  assign("TRTLV", TRTLV     , envir = env)
  assign("RLV", RLV     , envir = env)
  assign("RLVDOT", RLVDOT, envir = env)
  assign("CRLV", CRLV  , envir = env)
  assign("RLFDOT", RLFDOT, envir = env)
  assign("CRLF", CRLF  , envir = env)
  assign("CRTM", CRTM  , envir = env)
  assign("WRIDOT", WRIDOT, envir = env)
  return() 
  #END   # SUBROUTINE FOR_ROOTDM
}

#=======================================================================
#  FOR_VEGDM, Subroutine
#  Calculates the reduction in vegetative mass in
#       state and rate variables due to pest damage.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1990 WDB Written
#  02/23/1998 CHP Modified for PEST Module
#  01/12/1999 GH  Incorporated into CROPGRO
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:     None
#=======================================================================
VEGDM <- function (DYNAMIC) { 
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    CLSEN  = 0.0
    CLFRZ  = 0.0
    CLAI   = 0.0
    CLFM   = 0.0
    CSTEM  = 0.0
    CSTRM  = 0.0
    CSFRZ  = 0.0
    CSRFRZ = 0.0
    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
  } else if (DYNAMIC == "RATE") {
    #-----------------------------------------------------------------------
    WSIDOT = 0.0
    WLIDOT = 0.0
    LAIDOT = 0.0
    DISLA  = 0.0
    DISLAP = 0.0
    LDAM   = 0.0
    LAIDAM = 0.0
    SDAM   = 0.0
    WSRIDOT = 0.0
    #-----------------------------------------------------------------------
    #     Desired observed cumulative stem damage
    #-----------------------------------------------------------------------
    #     When stem damage is reported as percent reduction of total 
    #     cumulative stem mass produced, use this section.  The total stem
    #     mass produced is CSW.  Part of the observed damage comes from 
    #     senescence and part from pests.  
    #-----------------------------------------------------------------------
    if (PCSTMD > 0.0) {
      #     Desired stem mass DSTEM after cumulative damage
      DSTEM = CSW * (1.0 - PCSTMD / 100.0)
      if ((STMWT - SSDOT - WSFDOT) > DSTEM) {
        SDAM = STMWT - SSDOT - WSFDOT - DSTEM
      } else {
        SDAM = 0.0
      }
      WSIDOT = WSIDOT + SDAM
    }
    
    #-----------------------------------------------------------------------
    #     Percent daily stem damage
    #-----------------------------------------------------------------------
    if (PSTMD > 0.0) {
      SDAM = PSTMD*STMWT/100.0
      WSIDOT = WSIDOT + SDAM
    }
    #-----------------------------------------------------------------------
    #     Absolute daily amount of stem mass damaged
    #-----------------------------------------------------------------------
    if(WSTMD > 0.0) {
      SDAM = min(WSTMD, STMWT)
      WSIDOT = WSIDOT + SDAM
    }
    
    WSIDOT = max(0.,WSIDOT)
    WSIDOT = min(WSIDOT, STMWT)
    
    
    
    
    #-----------------------------------------------------------------------
    #     Desired observed cumulative storage organ damage
    #-----------------------------------------------------------------------
    #     When storage organ damage is reported as percent reduction of total 
    #     cumulative storage organ mass produced, use this section.  The total
    #     storage organ mass produced is CSRW.  Part of the observed damage 
    #     comes from senesence and part from pests.  
    #-----------------------------------------------------------------------
    
    if (PCSTRD > 0.0) {
      #     Desired storage organ mass DSTOR after cumulative damage
      DSTOR = CSRW * (1.0 - PCSTRD / 100.0)
      if ((STRWT - SSRDOT - WSRFDOT) > DSTOR) {
        SRDAM = (STRWT - SSRDOT - WSRFDOT) - DSTOR
      } else {
        SRDAM = 0.0
      }
      WSRIDOT = WSRIDOT + SRDAM
    }
    
    #-----------------------------------------------------------------------
    #     Percent daily storage organ damage
    #-----------------------------------------------------------------------
    if (PSTRD > 0.0) {
      SRDAM = PSTRD*STRWT/100.0
      WSRIDOT = WSRIDOT + SRDAM
    }
    #-----------------------------------------------------------------------
    #     Absolute daily amount of storage organ mass damaged
    #-----------------------------------------------------------------------
    if(WSTRD > 0.0) {
      SRDAM = min(WSTRD, STRWT)
      WSRIDOT = WSRIDOT + SRDAM
    }
    
    WSRIDOT = max(0.,WSRIDOT)
    WSRIDOT = min(WSRIDOT, STRWT)
    
    
    #-----------------------------------------------------------------------
    #     Absolute daily leaf damage
    #-----------------------------------------------------------------------
    if (TLFAD > 0.0 & SLA > 0.0) {
      LAIDAM = min(TLFAD, WTLF * SLA / 10000.0)
      LDAM = LAIDAM * 10000.0 / SLA
      WLIDOT = WLIDOT + LDAM
      LAIDOT = LAIDOT + LAIDAM
    }
    
    if (TLFMD > 0.0) {
      LDAM = min(TLFMD,WTLF)
      LAIDAM = LDAM * SLA / 10000.
      WLIDOT = WLIDOT + LDAM
      LAIDOT = LAIDOT + LAIDAM
    }
    #-----------------------------------------------------------------------
    #     Percent daily leaf damage
    #-----------------------------------------------------------------------
    if (PLFAD > 0.0) {
      LDAM = WTLF*(PLFAD/100.0)
      LAIDAM = LDAM * SLA / 10000.0
      WLIDOT = WLIDOT + LDAM
      LAIDOT= LAIDOT + LAIDAM
    }
    
    if (PLFMD > 0.0) {
      LDAM = WTLF*(PLFMD/100.0)
      LAIDAM = LDAM * SLA / 10000.0
      WLIDOT = WLIDOT + LDAM
      LAIDOT= LAIDOT + LAIDAM
    }
    
    #-----------------------------------------------------------------------
    #     Desired observed percent leaf damage
    #-----------------------------------------------------------------------
    #  When damage is reported as percent reduction of total cumulative
    #  leaf mass produced, use this section.  The total leaf mass produced is
    #  CLW.  Part of the observed damage comes from senesence and freezing 
    #  and part comes from pests.  This section does not require you to 
    #  distinguish whether damage came from senesence or pests.  PWTLF is
    #  the potential available leaf mass after the observed leaf damage is
    #  applied.  
    #----------------------------------------------------------------
    if (PCLMT >= 0.0001) {
      PWTLF = CLW * (1.0 - PCLMT / 100.0)
      if ((WTLF - SLDOT - WLFDOT) > PWTLF) {
        LDAM = (WTLF - SLDOT - WLFDOT) - PWTLF
      } else {
        LDAM = 0.0
      }
      #--------------------------------------------------------------
      if (LDAM > 0.0) {
        WLIDOT = WLIDOT + LDAM
        LAIDAM = LDAM * SLA / 10000.0
        LAIDOT= LAIDOT + LAIDAM
      }
    }
    
    #-----------------------------------------------------------------------
    #     Where is PCLMA?? chp 3/23/01
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    #     Percent diseased leaf area; DISLA and TDLA are in units of cm2/m2
    #     This code came from GROW subroutine -- chp
    #-----------------------------------------------------------------------
    if (PDLA > 0.0) {
      DISLA = WTLF * SLA * PDLA/100.0
    }
    
    if (TDLA > 0.0) {
      DISLA = DISLA + TDLA
    }
    
    if (AREALF > 0.) {
      DISLAP = DISLA / AREALF * 100.0
    } else {
      DISLAP = 0.0
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    #-----------------------------------------------------------------------
    #     Maintain cumulative values for senesenced and frozen leaf tissue.
    #-----------------------------------------------------------------------
    if (SLDOT  > 0.0) CLSEN = CLSEN + SLDOT
    if (WLFDOT > 0.0) CLFRZ = CLFRZ + WLFDOT
    if (WSFDOT > 0.0) CSFRZ = CSFRZ + WSFDOT
    if (WSRFDOT > 0.0) CSRFRZ = CSRFRZ + WSRFDOT
    #-----------------------------------------------------------------------
    #     Maintain cumulative leaf, leaf area, and stem damage variables.
    #-----------------------------------------------------------------------
    CLFM  = CLFM  + WLIDOT
    CLAI  = CLAI  + LAIDOT
    CSTEM = CSTEM + WSIDOT
    CSTRM  = CSTRM + WSRIDOT
    #-----------------------------------------------------------------------
    #     Percent V-stage damage
    #-----------------------------------------------------------------------
    if (PVSTGD > 0.0)  {
      VSTAGE = VSTAGE * (1.0 - PVSTGD/100.)
    }
    
    #-----------------------------------------------------------------------
    #     Absolute daily amount of VSTAGE damaged
    #-----------------------------------------------------------------------
    if (VSTGD > 0.0) {
      VSTAGE = VSTAGE - VSTGD
    }
    VSTAGE = max(0.,VSTAGE)
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("WLIDOT",  WLIDOT  , envir = env)
  assign("VSTAGE",  VSTAGE  , envir = env)
  assign("WSRIDOT", WSRIDOT , envir = env)
  assign("CLSEN", CLSEN , envir = env)
  assign("CLAI", CLAI  , envir = env)
  assign("CLFM", CLFM  , envir = env)
  assign("LAIDOT", LAIDOT, envir = env)
  assign("DISLA", DISLA , envir = env)
  assign("DISLAP", DISLAP, envir = env)
  assign("CLFRZ", CLFRZ , envir = env)
  assign("CSTEM", CSTEM , envir = env)
  assign("CSFRZ", CSFRZ , envir = env)
  assign("WSIDOT", WSIDOT, envir = env)
  assign("CSRFRZ", CSRFRZ, envir = env)
  assign("CSTRM", CSTRM , envir = env)
  assign("DSTOR", DSTOR , envir = env)
  assign("SRDAM", SRDAM , envir = env)
  return()
  #END  # SUBROUTINE FOR_VEGDM
}

#=======================================================================
#  ASMDM, Subroutine
#  Calculates assimilative damage due to pests.
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  03/20/98 CHP Written based on assimilative damage calculations in
#               PLANT subroutine.
#  01/13/98 GH  Incorporated into CROPGRO
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:     None
#=======================================================================
ASMDM  <- function (DYNAMIC, PGAVL) {
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    #      Initialize cumulative assimilative damage variable.
    #-----------------------------------------------------------------------
    CASM = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    #***********************************************************************
    ASMDOT = 0.0
    
    if (TPSR > PGAVL) {
      TPSR = max(0.,PGAVL)
    } else {
      TPSR = max(0.,TPSR)
    }
    
    if (PPSR > 100.) {
      PPSR = 100.
    } else {
      PPSR = max(0.,PPSR)
    }
    
    TPSR = TPSR + PPSR*PGAVL/100.
    if (TPSR > PGAVL) {
      TPSR = max(0.,PGAVL)
    } else {
      TPSR = max(0.,TPSR)
    }
    
    ASMDOT = ASMDOT + TPSR
    CASM = CASM + TPSR
    
    #***********************************************************************
  }
  #***********************************************************************
  #***********************************************************************
  #     END OF DYNAMIC IF CONSTRUCT
  #***********************************************************************
  assign("ASMDOT", ASMDOT, envir = env)
  assign("CASM", CASM, envir = env)
  return()
  #END   # SUBROUTINE ASMDM
}

#=======================================================================
#  DORMANCY Subroutine 6/20/03 SJR
#  Fall Dormancy with cold hardening for perennial grasses and legumes
#  Separate functions for effects on partitioning, photosynthesis, mobilization
#  Generate a reduction factor for each for use in appropriate modules
#  Factor is 0-1 value adjusted for cultivar sensitivity to daylength
#  Cold hardening lowers the minimum survivable temperature for the crop
#  with increased exposure to low temperatures.
#  This subroutine also provides a death rate to allow partial
#  or total depletion of the stand by a freeze event
#----------------------------------------------------------------------
#  Called by: CROPGRO
#  Calls    : None
#=======================================================================

DORMANCY <- function (DYNAMIC) {
  
  params <- plantList$forage$params
  
  TYPPGD <- params$TYPPGD
  TYPPTD <- params$TYPPTD
  TYPPMD <- params$TYPPMD
  TYPHRD <- params$TYPHRD
  TYPDHD <- params$TYPDHD
  ECOTYP <- params$ECOTYP
  ECONO  <- params$ECONO
  FILEC  <- params$FILEC
  FILEE  <- params$FILEE
  ECONAM <- params$ECONAM
  PATHCR <- params$PATHCR
  PATHEC <- params$PATHEC
  RDRMT  <- params$RDRMT
  RDRMG  <- params$RDRMG
  RDRMM  <- params$RDRMM
  FNPGD  <- params$FNPGD
  FNPTD  <- params$FNPTD
  FNPMD  <- params$FNPMD
  FRZHRD <- params$FRZHRD
  FRZDHD <- params$FRZDHD
  RCHDP  <- params$RCHDP
  FRZDC  <- params$FRZDC
  HARD1  <- params$HARD1
  HARD2  <- params$HARD2
  
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  #***********************************************************************
  #    Initialize yesterdays daylength to that passed in from PLANT
  #***********************************************************************
  
  if (DYNAMIC == "SEASINIT") {
    
    PPGFAC <- 0.0
    PPTFAC <- 0.0
    PPMFAC <- 0.0
    DRMST  <- 'NODORM'
    DAYLY  <- 0.0
    RDRMT  <- 1.0
    RDRMG  <- 1.0
    RDRMM  <- 1.0
    
    DAYLY  <- DAYL
    
    #-----------------------------------------------------------------------
    #      All cultivars share the same freeze-killing temperature before
    #      cold hardening
    #
    #      Minimum survivable temperature after hardening varies with the
    #      cold hardening potential of the cultivar
    #-----------------------------------------------------------------------
    FREEZ2  <- HARD1
    HARD2   <- HARD1-(HARD1-HARD2)*RCHDP
    #-----------------------------------------------------------------------
    #
    #   THE FOLLOWING GENERATES HEADINGS FOR NEW OUTPUT FILE DORMANT.OUT
    #
    #-----------------------------------------------------------------------
    #      OPEN(UNIT = NOUTDT, FILE = OUTT, STATUS = 'UNKNOWN')
    #
    #      IF (IDETL == 'Y') {
    #
    #
    #-----------------------------------------------------------------------
    #     Variable heading for DORMANT.OUT
    #-----------------------------------------------------------------------
    #
    #        WRITE (NOUTDT,2202) NREP,TITLET,
    #     &    MODEL,CROPD,EXPER,CROP,ENAME,TRTNO,TITLET, ECOTYP, ECONAM
    # 2202   FORMAT (/,'*RUN ',I3,8X,': ',A25,/,
    #     &    1X,'MODEL',10X,':',1X,A8,' - ',A10,/,
    #     &    1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
    #     &    1X,'TREATMENT',I3, 3X,':',1X,A25,/,
    #     &        1X,'ECOTYPE',8X,':',1X,A6,1X,A16,/)
    #
    #
    #        WRITE (NOUTDT,2203)
    # 2203   FORMAT('@DATE',
    #     &  ' DAYL  DRMST  PPGFA#   PRTFA#  WTLF  WCRLF  LFDM    STMWT',
    #     &  '  WCRST  STDM    STRWT   WCRSR  SRDM   RTWT  WCRRT   RTDM')
    #      ENDIF
    
    
  } else if (DYNAMIC == "EMERG") {
    #      NONE
    #***********************************************************************
    #     Daily Rate Calculations
    #***********************************************************************
    
    
  } else if (DYNAMIC == "RATE") {
    
    #      NONE
    #***********************************************************************
    #     Daily Integration
    #***********************************************************************
    
  } else if (DYNAMIC == "INTEGR") {
    
    #***********************************************************************
    #     Calculate cold-hardening status for day
    #     Killing freeze temperature decreases as cold hardening proceeds.
    #      Cold hardening is reversible while days are getting shorter.
    #      Maximum rate of hardening (degrees C decrease in FREEZ2 per day)
    #      occurs at FRZHRD(1) with fractional rates between FRZHRD(1) and FRZHRD(2)
    #      Cold hardening is reversed between FRZHRD(2) and FRZHRD(3).
    #      Dehardening will not occur until daylength begins to increase.
    #      Dehardening is not reversible.
    #      FRZDC is the rate of plant/tissue death per degree C below FREEZ2.
    #      This allows gradual killing of the stand with increased rate at
    #      lower temperatures.
    #      Adapted from ALFACOLD model (Kanneganti et al., 1998, Agron J. 90:687-697)
    #      Note: this routine allows hardening and dehardening to occur on the
    #      same day with reverse hardening and dehardening combining to
    #      accelerate dehardening at higher temperatures.
    #***********************************************************************
    
    if (DAYL <= DAYLY) {
      
      HARDR  <- CURV(TYPHRD,FRZHRD[1],FRZHRD[2],FRZHRD[3],FRZHRD[4],TMIN)
      DHARDR <- 0.0
      
    }else{
      
      HARDR  <- CURV(TYPHRD,FRZHRD[1],FRZHRD[2],FRZHRD[3],FRZHRD[4],TMIN)
      DHARDR <- CURV(TYPDHD,FRZDHD[1],FRZDHD[2],FRZDHD[3],FRZDHD[4],TMIN)
      
    }
    
    FREEZ2 <- FREEZ2 - (HARDR - DHARDR)*RCHDP
    FREEZ2 <- min(HARD1, FREEZ2)
    FREEZ2 <- max(FREEZ2, HARD2)
    
    
    #***********************************************************************
    #     Calculate Partitioning, Pg, and mobilization reduction
    #     factors and dormancy state for day
    #***********************************************************************
    PPTFAC  <-  CURV(TYPPTD,FNPTD[1],FNPTD[2],FNPTD[3],FNPTD[4],DAYL)
    PPTFAC  <-  RDRMT * PPTFAC
    PPTFAC  <-  min(PPTFAC,1.0)
    
    #            FNPGD(4) = RDRMG * FNPGD(4)
    PPGFAC  <-  CURV(TYPPGD,FNPGD[1],FNPGD[2],FNPGD[3],FNPGD[4],DAYL)
    PPGFAC  <-   PPGFAC/RDRMG
    PPGFAC  <-  min (PPGFAC,1.0)
    
    #            FNPMD(4) = RDRMM * FNPMD(4)
    PPMFAC   <-  CURV(TYPPMD,FNPMD[1],FNPMD[2],FNPMD[3],FNPMD[4],DAYL)
    PPMFAC   <-  PPMFAC/RDRMM
    PPMFAC   <-  min (PPMFAC,1.0)
    
    if (PPTFAC > 0.0 | PPGFAC < 1.0 | PPMFAC < 1.0) {
      DRMST  <-  'DORM'
    } else {
      DRMST  <-  'NODORM'
    }
    
    DAYLY   <-  DAYL
    
  } else if (DYNAMIC == "OUTPUT") {
    #-----------------------------------------------------------------------
    #     Calculate new variables for DORMANCY.OUT
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    #     Sent daily growth and partitioning detail to DORMANCY.OUT
    #-----------------------------------------------------------------------
    
    
    
    #        IF (IDETL == 'Y') {
    #-----------------------------------------------------------------------
    #     Print out dormancy parameters - TEMPORARY
    #-----------------------------------------------------------------------
    #
    #            WRITE (NOUTDT,401) YRDOY, DAYL, DRMST, PPGFAC,
    #     &              PPTFAC, WTLF, WCRLF, LFDM,
    #     &              STMWT, WCRST, STDM, STRWT, WCRSR, SRDM,
    #     &              RTWT, WCRRT, RTDM
    # 401      FORMAT (1X,I5,1X,F5.2,1X,A6,1X,F5.3,1X,F5.3,1X,F6.1,
    #     &              1X,F6.1,1X,F6.1,1X,F7.1,1X,F6.1,1X,F6.1,1X,F7.1,
    #     &              1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1)
    #        ENDIF
    
    #***********************************************************************
    #     SEASEND
    #***********************************************************************
    
  } else if (DYNAMIC == "SEASEND") {
    #-----------------------------------------------------------------------
    #     Close DORMANCY.OUT
    #-----------------------------------------------------------------------
    
  }
  assign("FREEZ2",FREEZ2, envir = env)
  assign("PPGFAC",PPGFAC, envir = env)
  assign("DRMST",DRMST, envir = env)
  assign("PPMFAC",PPMFAC, envir = env)
  assign("PPTFAC",PPTFAC, envir = env)
  assign("DAYLY",DAYLY, envir = env)
  return()
  #END    #SUBROUTINE DORMANCY
}

#=======================================================================
#  FOR_LINDM, Subroutine
#----------------------------------------------------------------------
#  This subroutine linearly interpolates the between pest observations
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  01/01/90 WDB
#  02/25/98 CHP Modified for PEST Module
#  01/12/99 GH  Incorporated into CROPGRO
#-----------------------------------------------------------------------
#  Called by: PEST
#  Calls:     None
#=======================================================================
LINDM <- function (DAP){
  
  # PCN <- ?? # TODO: Descobrir o valor de PCN!!!!!!!!!  Tem relação com arquivo de leitura de parametros da peste
  # IDAP<- ?? # TODO: Descobrir o valor de IDAP!!!!!!!!!  
  # YPL(6,200)<- ?? # TODO: Descobrir o valor de YPL!!!!!!!!!  
  
  
  
  #-----------------------------------------------------------------------
  #     Initialize variables
  #-----------------------------------------------------------------------
  if (DAP <= 1) {
    for (I in 1:6){
      PL[I]  <- 0
      ROW[I] <- 1
    }
  }
  #-----------------------------------------------------------------------
  #     Linearly interpolate pest levels daily
  #-----------------------------------------------------------------------
  for (I in 1:PCN){
    #-----------------------------------------------------------------------
    #     Increment Row Counter
    #-----------------------------------------------------------------------
    if (IDAP(I,ROW[I]) < 0) {
      ROW[I] <- ROW[I] + 1
    }
    
    if (DAP > IDAP(I,ROW[I]) & IDAP(I,ROW[I]) > 0) ROW[I] = ROW[I] + 1
    #-----------------------------------------------------------------------
    #    Set beginning of interpolation to zero
    #-----------------------------------------------------------------------
    if (DAP < IDAP(I,1) ) {
      PL[I] <- 0
      #-----------------------------------------------------------------------
      #     Set First Interpolated Value if idap(I,1) = 1
      #-----------------------------------------------------------------------
    } else if (DAP == IDAP(I,1)) {
      PL[I]  <- YPL(I,ROW[I])
      ROW[I] <- ROW[I] + 1
      #-----------------------------------------------------------------------
      #   Set first interpolated value if idap < 0, that is
      #   there is some pest data before planting date
      #-----------------------------------------------------------------------
    } else if (ROW[I] > 1 & IDAP(I,ROW[I]-1) < 0 & DAP <= 1) {
      RISE  <-  YPL(I,ROW[I]) - YPL(I,ROW[I]-1)
      RUN   <- IDAP(I,ROW[I]) - IDAP(I,ROW[I]-1)
      SLOPE <- RISE/RUN
      PL[I] <- YPL(I,ROW[I]-1) + SLOPE * (DAP - IDAP(I,ROW[I]-1))
      #-----------------------------------------------------------------------
      # Linear Interpolation
      #-----------------------------------------------------------------------
    } else if (DAP <= IDAP(I,ROW[I])) {
      RISE   <- YPL(I,ROW[I]) - YPL(I,ROW[I]-1)
      RUN    <- IDAP(I,ROW[I]) - IDAP(I,ROW[I]-1)
      SLOPE  <- RISE/RUN
      PL[I]  <- PL[I] + SLOPE * 1
      #-----------------------------------------------------------------------
      # Set all damage to zero after last user entered
      # value for damage
      #-----------------------------------------------------------------------
      #        ELSEIF (DAP > IDAP(I,ROW(I))) {
      #          PL(I) = 0
    }
    
    if (DAP > IDAP(I,ROW[I])) {
      PL[I] <- 0
    }
  }
  
  assign("PL", PL, envir = env)
  assign("ROW", ROW, envir = env)
  return()
  #END   # SUBROUTINE FOR_LINDM
}

#=======================================================================
#  FOR_SENMOB Subroutine, S.J. Rymph, K.J. Boote, J.W. Jones and G. Hoogenboom
#  Calculates leaf, stem, root, and STOR senescence only for tissues lost
#  at "final" N concentration (due to natural aging, light stress, and
#  physiological maturity).  Also consolidates the calculation of
#  potential mining of C and N in one location.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  09/23/2005 SJR Created from parts of CROPGRO, DEMAND, ROOTS, SENES
#-----------------------------------------------------------------------
#  Called : CROPGRO
#  Calls  : ERROR, FIND, IGNORE
#========================================================================
SENMOB <- function (DYNAMIC, DAS, ISWWAT, PAR) {
  
  params    <- plantList$forage$params
  
  TYPLMOB   <- params$TYPLMOB
  TYPNMOB   <- params$TYPNMOB
  LFSEN     <- params$LFSEN
  ICMP      <- params$ICMP
  KCAN      <- params$KCAN
  PORPT     <- params$PORPT
  SENDAY    <- params$SENDAY
  SENRT2    <- params$SENRT2
  SENRTE    <- params$SENRTE
  TCMP      <- params$TCMP
  SENMAX    <- params$SENMAX
  SENPOR    <- params$SENPOR
  XSENMX    <- params$XSENMX
  XSTAGE    <- params$XSTAGE
  SENSR     <- params$SENSR
  SENCLV    <- params$SENCLV
  SENCRV    <- params$SENCRV
  SENCSV    <- params$SENCSV
  SENCSRV   <- params$SENCSRV
  SENNLV    <- params$SENNLV
  SENNRV    <- params$SENNRV
  SENNSV    <- params$SENNSV
  SENNSRV   <- params$SENNSRV
  PORMIN    <- params$PORMIN
  RTSDF     <- params$RTSDF
  RTEXF     <- params$RTEXF
  LRMOB     <- params$LRMOB
  NRMOB     <- params$NRMOB
  PCHOLFF   <- params$PCHOLFF
  PCHORTF   <- params$PCHORTF
  PCHOSRF   <- params$PCHOSRF
  PCHOSTF   <- params$PCHOSTF
  PROLFF    <- params$PROLFF
  PRORTF    <- params$PRORTF
  PROSRF    <- params$PROSRF
  PROSTF    <- params$PROSTF
  RFAC1     <- params$RFAC1
  RLDSM     <- params$RLDSM
  RTSEN     <- params$RTSEN
  CMOBMX    <- params$CMOBMX
  CMOBSRN   <- params$CMOBSRN
  CMOBSRX   <- params$CMOBSRX
  NMOBMX    <- params$NMOBMX
  NMOBSRN   <- params$NMOBSRN
  NMOBSRX   <- params$NMOBSRX
  NVSMOB    <- params$NVSMOB
  
  WSLOSS    <- 0  # TODO: nao sei como inicializar essa variável - local mas esta sem inicialização, Vem da Senes, mas pra Forage nao é chamado portanto o valor da variável é ZERO
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == "SEASINIT") {
    #-----------------------------------------------------------------------
    SRDOT   <- 0.0
    SRNDOT  <- 0.0
    RLV     <- 0.0
    #      RTDEP = 0.0
    #      SENRT = 0.0
    SUMEX   <- 0.0
    SUMRL   <- 0.0
    
    LFNSEN  <- 0.0
    
    LFSNMOB <- 0.0
    RTSNMOB <- 0.0
    SRSNMOB <- 0.0
    STSNMOB <- 0.0
    TSNMOB  <- 0.0
    
    CMINELF <- 0.0
    CMINEP  <- 0.0
    CMINERT <- 0.0
    CMINESH <- 0.0
    CMINESR <- 0.0
    CMINEST <- 0.0
    CMOBSR  <- 0.0
    LFCMINE <- 0.0
    LFSCMOB <- 0.0
    LFSNMOB <- 0.0
    LTSEN   <- 0.0
    NMINELF <- 0.0
    NMINEP  <- 0.0
    NMINERT <- 0.0
    NMINESR <- 0.0
    NMINEST <- 0.0
    NMOBR   <- 0.0
    NMOBSR  <- 0.0
    RLSEN   <- 0.0
    RTCMINE <- 0.0
    RTSCMOB <- 0.0
    RTSNMOB <- 0.0
    SHCMINE <- 0.0
    SHNMINE <- 0.0
    SLMDOT  <- 0.0
    SRCMINE <- 0.0
    SRMDOT  <- 0.0
    SRSCMOB <- 0.0
    SRSNMOB <- 0.0
    SSMDOT  <- 0.0
    SSRMDOT <- 0.0
    STCMINE <- 0.0
    STSCMOB <- 0.0
    STSNMOB <- 0.0
    TSCMOB  <- 0.0
    TSNMOB  <- 0.0
    
    SSDOT   <- 0.0
    SLDOT   <- 0.0
    SLNDOT  <- 0.0
    SSNDOT  <- 0.0
    RATTP   <- 1.0
    SSRDOT  <- 0.0
    SSRNDOT <- 0.0
    DAYL_1  <- -1.0
    DAYL_2  <- -2.0
    
    for (I in 1:5){
      SWFCAB[I] <- 1.0
    }
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    #-----------------------------------------------------------------------
    # DAS   <- max(0,TIMDIF(YRSIM,YRDOY)) # TODO: DAS VEM POR PARÃMETRO NECESSÁRIO ESSA ATRIBUIÇÃO?? LEANDRO 03/11/2020 
    
    #Update value of RATTP.
    NSWAB  <- 5
    for (I in NSWAB:2){ #TODO Leandro
      SWFCAB[I] <- SWFCAB[I-1]
    }
    #      DO I=NSWAB,2,-1
    #       WSWTLF(I)=WSWTLF(I-1)
    #      END DO
    SWFCAB[1] <- SWFAC
    #      WSWTLF(1)=WTLF
    RATTP <- SWFCAB[NSWAB]
    
    SSDOT   <- 0.0
    SLDOT   <- 0.0
    SLNDOT  <- 0.0
    
    SSNDOT  <- 0.0
    
    SSRDOT  <- 0.0
    SSRNDOT <- 0.0
    
    LFNSEN  <- 0.0
    SLMDOT  <- 0.0
    SRMDOT  <- 0.0
    SSRMDOT <- 0.0
    SSMDOT  <- 0.0
    
    LFSCMOB <- 0.0
    RTSCMOB <- 0.0
    SRSCMOB <- 0.0
    STSCMOB <- 0.0
    TSCMOB  <- 0.0
    
    LFSNMOB <- 0.0
    RTSNMOB <- 0.0
    SRSNMOB <- 0.0
    STSNMOB <- 0.0
    TSNMOB  <- 0.0
    
    CMINELF <- 0.0
    CMINERT <- 0.0
    CMINESR <- 0.0
    CMINEST <- 0.0
    
    LFCMINE <- 0.0
    RTCMINE <- 0.0
    SHCMINE <- 0.0
    SRCMINE <- 0.0
    STCMINE <- 0.0
    
    CMINEP  <- 0.0
    CMINEO  <- 0.0
    
    NMINELF <- 0.0
    NMINERT <- 0.0
    NMINESR <- 0.0
    NMINEST <- 0.0
    
    LFNMINE <- 0.0
    RTNMINE <- 0.0
    SHNMINE <- 0.0
    SRNMINE <- 0.0
    STNMINE <- 0.0
    
    NMINEP  <- 0.0
    NMINEO  <- 0.0
    
    #-----------------------------------------------------------------------
    #     Calculate root senescence
    #-----------------------------------------------------------------------
    #     Calculate root length per cm2 soil and initiate growth,
    #     respiration and senescence by layer
    #-----------------------------------------------------------------------
    TRTDY   <- 0.0
    TRLSEN  <- 0.0
    TRLNSEN <- 0.0
    # NL = 20
    RNDOT <- rep(0.0,20)
    
    for (L in 1:NLAYR){
      L1 <- L
      TRTDY <- TRTDY + RLV[L] * DLAYR[L]
      RLSEN[L] <- 0.0
      
      
      RNDOT[L]  <- 0.0
      RLNSEN[L] <- 0.0
      
    }
    
    SRNDOT <- 0.0
    #      TRLDF  = 0.0
    SUMEX <- 0.0
    SUMRL <- 0.0
    
    if (RTWT >= 0.0001) {
      #       RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
      #       RTWT has not yet been updated today, so use yesterday's
      #       value and don't subtract out today's growth - chp 11/13/00
      RFAC3 <- TRTDY * 10000.0 / RTWT
    } else {
      RFAC3 <- RFAC1
    }
    
    for (L in 1:L1){
      
      RLSEN[L] <- RLV[L] * RTSEN * DTX
      SWDF     <- 1.0
      SWEXF    <- 1.0
      
      #-----------------------------------------------------------------------
      #     Calculate water-stress factors only when H2O optionis "on"
      #-----------------------------------------------------------------------
      
      if (ISWWAT == 'Y') {
        if (SAT[L]-SW[L] < PORMIN) {
          SWEXF <- (SAT[L] - SW[L]) / PORMIN
          SWEXF <- min(SWEXF, 1.0)
        }
        
        #        SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMEX  <- SUMEX + DLAYR[L]*(RLV[L] - RLSEN[L])*(1.0 - SWEXF)
        #       SUMRL  = SUMRL + DLAYR(L)*RLV(L)
        SUMRL  <- SUMRL + DLAYR[L]*(RLV[L] - RLSEN[L])
        
        ESW    <- rep(0, NL)
        #     Need to calculate ESW where used. CHP 10/15/01
        ESW[L] <- DUL[L] - LL[L]
        if (SW[L] - LL[L] < 0.25*ESW[L]) {
          SWDF <- (SW[L] - LL[L]) / (0.25*ESW[L])
          SWDF <- max(SWDF, 0.0)
        }
      }
      
      #-----------------------------------------------------------------------
      
      RTSURV <- min(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
      #-----------------------------------------------------------------------
      if ((RLV[L] - RLSEN[L]) > RLDSM) {
        #            RNDOT(L) = RLV(L) * (1 - RTSURV)
        RNDOT[L] <- (RLV[L] - RLSEN[L]) * (1 - RTSURV)
        #          RLV(L) = RLV(L) * RTSURV
      }
      RLNSEN[L] <- RNDOT[L]
      TRLNSEN   <- TRLNSEN + RLNSEN[L] * DLAYR[L]
      
      TRLSEN <- TRLSEN + RLSEN[L] * DLAYR[L]
      
    }
    
    #-----------------------------------------------------------------------
    #     Calculate root senescence, growth, maintenance and growth
    #     respiration, and update root length density for each layer.
    #-----------------------------------------------------------------------
    
    #     SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3
    #     Sum RLSEN for total root senescence today. chp 11/13/00
    SRMDOT <- TRLSEN / RFAC3 * 10000.     #g/m2
    SRNDOT <- TRLNSEN / RFAC3 * 10000. #g/m2
    SRDOT  <- SRMDOT + SRNDOT
    
    #-----------------------------------------------------------------------
    #     This section calculates natural senescence of storage organ tissue
    #      Thought about moving this below the IF...{ line but did not
    #      Don't want to senesce if seedling but do want to senesce mature
    #      stand after it has been cut or frozen back
    #-----------------------------------------------------------------------
    SSRMDOT <- STRWT *  SENSR * DTX
    SSRMDOT <- min(SSRMDOT,STRWT)
    #-----------------------------------------------------------------------
    #     Calculate STOR water-stress senescence.
    #-----------------------------------------------------------------------
    SSRNDOT <- 0.0
    #-----------------------------------------------------------------------
    #     Calculate STOR senescence.
    #-----------------------------------------------------------------------
    SSRDOT <- SSRMDOT + SSRNDOT
    SSRDOT <- min(STRWT,SSRDOT)
    
    if (DAS <= NR7 & VSTAGE >= 1.0) {
      #-----------------------------------------------------------------------
      #     This section calculates natural senescence prior to the
      #     beginning of seed growth
      #-----------------------------------------------------------------------
      #        IF (VSTAGE >= 5.0) {
      #          PORLFT = 1.0 - TABEX(SENPOR,XSTAGE,VSTAGE,4)
      #          IF ((WTLF * ( 1.0 - RHOL)) > CLW*PORLFT) {
      #            SLDOT = WTLF * ( 1.0 - RHOL) - CLW * PORLFT
      #          }
      #        }
      #-----------------------------------------------------------------------
      #     09/18/05 SJR - Modify for forages Short harvest cycles prevent
      #     VSTAGE>5.0.  CLW gets too large relative to WTLF to be useful.
      #      Revise and simplify using one rate all the time, adjusted for
      #      temperature.  Rate is now a proportion of tissue senesced
      #      per physiological day.
      #-----------------------------------------------------------------------
      #      IF (WTLF > WTLF * (1 - RHOL) * LFSEN * DTX *
      #     &  (1-EXP(-KCAN * XLAI))) {
      #            LFNSEN = WTLF * (1 - RHOL) * LFSEN * DTX *
      if (WTLF > WTLF * LFSEN * DTX) {
        LFNSEN <- WTLF * LFSEN * DTX
      } else {
        LFNSEN <- WTLF
      }
      SLMDOT <- LFNSEN
      #-----------------------------------------------------------------------
      #     This section calculates senescence due to low light in lower
      #     canopy.  First compute LAI at which light compensation is reached
      #     { allow LAI above this amount to be senesced over TCMP thermal
      #     days.
      #-----------------------------------------------------------------------
      LTSEN <- 0.0
      if (PAR > 0.) {
        LCMP  <- -(1. / KCAN) * log(ICMP / PAR)
        LTSEN <- DTX * (XLAI - LCMP) / TCMP
        LTSEN <- max(0.0, LTSEN)
      }
      #-----------------------------------------------------------------------
      #     8/3/05 SJR Change LTSEN from leaf area senesced to the equivalent
      #      leaf mass senesced.  Moved conversion from SLDOT update equation.
      #      For ease of use in calculating DM, CH2O, and N lost in GROW
      #      subroutine
      #-----------------------------------------------------------------------
      LTSEN <- LTSEN * 10000. / SLAAD
      #-----------------------------------------------------------------------
      #     Convert area loss to biomass(m2 *10000cm2/m2)/(cm2/g)=g/m2
      #-----------------------------------------------------------------------
      #        SLDOT = SLDOT + LTSEN * 10000. / SLAAD
      SLDOT <- LFNSEN + LTSEN
      SLDOT <- min(WTLF,SLMDOT)
      
      #-----------------------------------------------------------------------
      #     Calculate senescence due to water stress.
      #-----------------------------------------------------------------------
      #        IF (WTLF >= WSWTLF(5)) {
      #          WSLOSS = SENDAY * (1. - RATTP) * WTLF
      #        } else if (SENDAY*(1.-RATTP) > WSWTLF(5)-WTLF) {
      #          WSLOSS=SENDAY*(1.-RATTP)*(WSWTLF(5)-WTLF/WSWTLF(5))
      #        ELSE
      #          WSLOSS=0
      #        }
      
      if (WSLOSS > 0.0) {
        PORLFT <- 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
        WSLOSS <- min(WSLOSS, WTLF - CLW * PORLFT)
        WSLOSS <- max(WSLOSS, 0.0)
        SLNDOT <- WSLOSS
      }
      
      SLDOT <- SLDOT + SLNDOT
      SLDOT <- min(WTLF,SLDOT)
      
      #-----------------------------------------------------------------------
      #     Calculate Stem senescence.
      #-----------------------------------------------------------------------
      SSMDOT <- SLMDOT * PORPT
      SSMDOT <- min(SSMDOT,0.1*STMWT)
      
      #-----------------------------------------------------------------------
      #     10/04/05 SJR Link to SENMOB where natural senescence was already
      #                         calculated.  Calculate SSDOT in same way as SLDOT.
      #-----------------------------------------------------------------------
      SSDOT   <- SSMDOT
      SSDOT   <- SSDOT + LFSENWT * PORPT
      SSDOT   <- min(SSDOT,0.1*STMWT)
      STSENWT <- SSDOT - SSMDOT
      
      SSDOT   <- SSDOT + LTSEN * PORPT
      SSDOT   <- min(SSDOT,0.1*STMWT)
      STLTSEN <- SSDOT - (SSMDOT + STSENWT)
      
      SSNDOT  <- SLNDOT * PORPT
      SSDOT   <- SSDOT + SSNDOT
      SSDOT   <- min(SSDOT, 0.1 * STMWT)
      SSNDOT  <- SSDOT - (SSMDOT + STSENWT + STLTSEN)
      
      #-----------------------------------------------------------------------
      #     This section calculates senescence of leaves and petioles
      #     after R7.
      #-----------------------------------------------------------------------
    } else if (DAS > NR7) {
      if(WTLF > 0.0001) {
        #          SLMDOT = WTLF * SENRT2
        #          SLNDOT = SLDOT
        SLMDOT <- 0.0
        #          SSMDOT = SLMDOT * PORPT
        #          SSNDOT = SSDOT
        SSMDOT <- 0.0
      } else {
        SLMDOT <- 0.0
        SSMDOT <- 0.0
        #          SLNDOT = 0.0
        #          SSNDOT = 0.0
      }
      #        IF (STMWT < 0.0001) {
      #          SLNDOT = 0.0
      #          SSNDOT = 0.0
      #        }
    }
    
    #-----------------------------------------------------------------------
    #     Calculate N available from today's senescence.
    #      Only Age, low-light and N-mobilization-based senescece are lost at
    #      less than current N and CH2O concentration.
    #      Senesced tissues will contain N and CH2O concentrations between
    #      "final" and "current" levels - calculated as
    #      PROLFF * 0.16+( SENNxV * PCNL/100 - PROLFF * 0.16)
    #      so mobilization would be the difference between "current" N or C
    #      concentration and this calculated level at senescence.
    #-----------------------------------------------------------------------
    LFSNMOB <- SLMDOT * (PCNL/100 - (SENNLV * (PCNL / 100 - PROLFF*0.16) + PROLFF*0.16)) + LTSEN * (PCNL / 100 - PROLFF * 0.16)
    STSNMOB <- SSMDOT * (PCNST/100 - (SENNSV * (PCNST / 100 - PROSTF*0.16) + PROSTF*0.16)) + STLTSEN * (PCNST / 100 - PROSTF * 0.16)
    
    SRSNMOB <- SSRMDOT * (PCNSR / 100 - (SENNSRV * (PCNSR / 100 - PROSRF*0.16) + PROSRF*0.16))
    RTSNMOB <- SRMDOT * (PCNRT / 100 - (SENNRV * (PCNRT / 100 - PRORTF*0.16) + PRORTF*0.16))
    
    #     TAKE OUT FROM HERE TO
    #        LFSENWT = SENRTE * LFSNMOB / 0.16
    #        LFSENWT = MIN(WTLF,LFSENWT)
    #        SLDOT = SLDOT + LFSENWT
    #        SLDOT = MIN(WTLF,SLDOT)
    
    #        STSENWT = LFSENWT * PORPT
    #        SSDOT = SSDOT + STSENWT
    #        SSDOT = MIN(STMWT, SSDOT)
    
    #            LFSNMOB = LFSNMOB + LFSENWT * (PCNL/100 -
    #     &              (SENNLV * (PCNL / 100 - PROLFF*0.16) + PROLFF*0.16))
    
    #            STSNMOB = STSNMOB + STSENWT * (PCNST/100 -
    #     &              (SENNSV * (PCNST / 100 - PROSTF*0.16) + PROSTF*0.16))
    
    #      HERE
    #            TSNMOB = LFSNMOB + STSNMOB + SRSNMOB + RTSNMOB
    
    #-----------------------------------------------------------------------
    #     Calculate CH2O available from today's senescence.
    #-----------------------------------------------------------------------
    #            LFSCMOB = (SLMDOT + LTSEN + LFSENWT) * (WCRLF / WTLF -
    #     &              (SENCLV * (WCRLF / WTLF - PCHOLFF) + PCHOLFF))
    #            STSCMOB = (SSMDOT + STLTSEN + STSENWT) * (WCRST / STMWT -
    #     &              (SENCSV * (WCRST / STMWT - PCHOSTF) + PCHOSTF))
    #            SRSCMOB = SSRMDOT * (WCRSR / STRWT -
    #     &              (SENCSRV * (WCRSR / STRWT - PCHOSRF) + PCHOSRF))
    #            RTSCMOB = SRMDOT * (WCRRT / RTWT -
    #     &              (SENCRV * (WCRRT / RTWT - PCHORTF) + PCHORTF))
    
    #            LFSCMOB = SLMDOT * ((WCRLF / WTLF) - PCHOLFF)
    #            STSCMOB = SSMDOT * ((WCRST / STMWT) - PCHOSTF)
    #            SRSCMOB = SSRMDOT * ((WCRSR / STRWT) - PCHOSRF)
    #            RTSCMOB = SRMDOT * ((WCRRT / RTWT) - PCHORTF)
    
    TSCMOB <- LFSCMOB + STSCMOB + SRSCMOB + RTSCMOB
    
    #-----------------------------------------------------------------------
    # DSSAT4 code for CMINEP
    #      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)
    #-----------------------------------------------------------------------
    # New code to deal with forages and dormancy
    # Boosts mobilization from storage organs after harvests (LAI<CLAIT)
    # Lowers mobilization during dormancy (when PPMFAC>0.0)
    #-----------------------------------------------------------------------
    #      IF (XLAI <= CLAIT) {
    #      CMOBSR=CMOBSRX*(PPMFAC)
    #      NMOBSR=NMOBSRX*(PPMFAC)
    #      ELSE
    #      CMOBSR=CMOBSRN*(PPMFAC)
    #      NMOBSR=NMOBSRN*(PPMFAC)
    #      }
    
    LAIMOBR <- CURV(TYPLMOB,LRMOB[1],LRMOB[2],LRMOB[3], LRMOB[4], min(XLAI,LRMOB[4]))
    
    #-----------------------------------------------------------------------
    #      Increase mobilization from storage if N status of plant is high.
    #-----------------------------------------------------------------------
    
    VEGNCNT <- PCNL/100*WTLF + PCNST/100*STMWT +  PCNRT/100*RTWT + PCNSR/100*STRWT
    VEGNCMX <- FNINL*WTLF + FNINS*STMWT + FNINR*RTWT + FNINSR*STRWT
    VNSTAT  <- min((VEGNCNT / VEGNCMX),  1.0)
    
    VNMOBR  <- CURV(TYPNMOB,NRMOB[1],NRMOB[2],NRMOB[3],NRMOB[4],VNSTAT)
    #-----------------------------------------------------------------------
    #      Set N mobilization rate from storage
    #      Default to NMOBSRN under most conditions
    #      set to NMOBSRX (max rate) after harvest or severe damage
    #      Reduce from either level depending on degree of dormancy
    #      Mobilization from storage is unaffected by water or N stress
    #      but is accelerated by low N status.
    #-----------------------------------------------------------------------
    #      Tried various equations - a straight multiplicative equation
    #      doesn't work because LAIMOBR is usually 0.0 which forces
    #      mobilization to the minimum rate * PPMFAC.
    #-----------------------------------------------------------------------
    #      NMOBSR = (NMOBSRN + ((NMOBSRX-NMOBSRN)*VNMOBR * LAIMOBR)) * PPMFAC
    #-----------------------------------------------------------------------
    #      Additive equation would give the highest rate of mobilization.
    #-----------------------------------------------------------------------
    #      NMOBSR = (NMOBSRN + (NMOBSRX-NMOBSRN)*VNMOBR)
    #      NMOBSR = (NMOBSR + (NMOBSRX-NMOBSR)*LAIMOBR)*PPMFAC
    #-----------------------------------------------------------------------
    #      Let the maximum of the two modifiers set the rate of mobilization.
    #-----------------------------------------------------------------------
    NMOBSR <- (NMOBSRN + max(VNMOBR,LAIMOBR) * (NMOBSRX - NMOBSRN)) * PPMFAC
    
    #-----------------------------------------------------------------------
    #      Set C mobilization rate from storage
    #      Default to CMOBSRN under most conditions
    #      set to CMOBSRX (max rate) after harvest or severe damage
    #      Reduce from either level depending on degree of dormancy
    #      Mobilization from storage is unaffected by water or N stress
    #-----------------------------------------------------------------------
    #      CMOBSR = (CMOBSRN + ((CMOBSRX-CMOBSRN)*VNMOBR * LAIMOBR)) * PPMFAC
    
    #      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*VNMOBR)
    #      CMOBSR = (CMOBSR + (CMOBSRX-CMOBSR)*LAIMOBR)*PPMFAC
    
    #      CMOBSR = (CMOBSRN + MAX(VNMOBR,LAIMOBR) *
    #     &              (CMOBSRX - CMOBSRN)) * PPMFAC
    
    CMOBSR <- (CMOBSRN + LAIMOBR * (CMOBSRX - CMOBSRN)) * PPMFAC
    
    #-----------------------------------------------------------------------
    #      Calculate potential N mobilization for the day
    #-----------------------------------------------------------------------
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
    
    NMOBR  <- NVSMOB * NMOBMX * TDUMX
    if (DAS > NR5) {
      NMOBR <- NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC)) * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB) * max(XPOD,DXR57**2.))
    }
    
    NMINELF <- NMOBR * WNRLF
    LFNMINE <- LFSNMOB + NMINELF
    LFSNMOB <- LFNMINE
    
    NMINEST <- NMOBR * WNRST
    STNMINE <- STSNMOB + NMINEST
    STSNMOB <- STNMINE
    
    NMINERT <- NMOBR * PPMFAC * WNRRT
    RTNMINE <- RTSNMOB + NMINERT
    RTSNMOB <- RTNMINE
    
    NMINESR <- NMOBSR * WNRSR
    SRNMINE <- SRSNMOB + NMINESR
    SRSNMOB <- SRNMINE
    
    SHNMINE <- NMOBR * WNRSH
    
    NMINEP  <- LFNMINE + STNMINE + RTNMINE + SRNMINE + SHNMINE
    NMINEO  <- NMINELF + NMINEST + NMINERT + NMINESR + SHNMINE
    
    TSNMOB  <- LFSNMOB + STSNMOB + SRSNMOB + RTSNMOB
    #      TSNMOB = LFNMINE + STNMINE + RTNMINE + SRNMINE + SHNMINE
    #      ADDITIONAL DM LOSS DUE TO N MOBILIZATION? SENRTE
    
    LFSENWT <- SENRTE * NMINELF / 0.16
    LFSENWT <- min(WTLF,LFSENWT)
    SLDOT   <- SLDOT + LFSENWT
    SLDOT   <- min(WTLF,SLDOT)
    
    STSENWT <- LFSENWT * PORPT
    SSDOT   <- SSDOT + STSENWT
    SSDOT   <- min(STMWT, SSDOT)
    
    #-----------------------------------------------------------------------
    #      Calculate potential CH2O mobilization for the day
    #-----------------------------------------------------------------------
    CMINELF <- CMOBMX * (DTX + DXR57)* (WCRLF - WTLF * PCHOLFF)
    LFCMINE <- max(LFSCMOB, CMINELF)
    
    CMINEST <- CMOBMX * (DTX + DXR57)* (WCRST - STMWT * PCHOSTF)
    STCMINE <- max(STSCMOB, CMINEST)
    
    CMINERT <- CMOBMX * (DTX + DXR57)* PPMFAC * (WCRRT - RTWT * PCHORTF)
    RTCMINE <- max(RTSCMOB, CMINERT)
    
    CMINESR <- CMOBSR * (DTX + DXR57)* (WCRSR - STRWT * PCHOSRF)
    SRCMINE <- max(SRSCMOB, CMINESR)
    
    SHCMINE <- CMOBMX * (DTX + DXR57)* WCRSH
    
    CMINEP  <- LFCMINE + STCMINE + RTCMINE + SRCMINE + SHCMINE
    CMINEO  <- CMINELF + CMINEST + CMINERT + CMINESR + SHCMINE
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("SLDOT",SLDOT  , envir = env)
  assign("SSDOT",SSDOT  , envir = env)
  assign("SRDOT",SRDOT  , envir = env)
  assign("LTSEN",LTSEN  , envir = env)
  assign("SWFCAB",SWFCAB  , envir = env) 
  assign("SRNDOT",SRNDOT  , envir = env)
  assign("SSRDOT",SSRDOT  , envir = env)
  assign("SSRNDOT",SSRNDOT  , envir = env)
  assign("STLTSEN",STLTSEN  , envir = env)
  assign("STSENWT",STSENWT  , envir = env)
  assign("LFSENWT",LFSENWT  , envir = env)
  assign("SSNDOT",SSNDOT  , envir = env)
  assign("RLNSEN",RLNSEN  , envir = env)          # TODO: atenção com essa Variável!!!!
  assign("SLMDOT",SLMDOT  , envir = env)
  assign("SRMDOT",SRMDOT  , envir = env)
  assign("SSMDOT",SSMDOT  , envir = env)
  assign("SSRMDOT",SSRMDOT  , envir = env)
  assign("LFSCMOB",LFSCMOB  , envir = env)
  assign("RTSCMOB",RTSCMOB  , envir = env)
  assign("SRSCMOB",SRSCMOB  , envir = env)
  assign("STSCMOB",STSCMOB  , envir = env)
  assign("TSCMOB",TSCMOB  , envir = env)
  assign("LFSNMOB",LFSNMOB  , envir = env)
  assign("RTSNMOB",RTSNMOB  , envir = env)
  assign("SRSNMOB",SRSNMOB  , envir = env)
  assign("STSNMOB",STSNMOB  , envir = env)
  assign("TSNMOB",TSNMOB  , envir = env)
  assign("CMINEP",CMINEP  , envir = env)
  assign("LFCMINE",LFCMINE  , envir = env)
  assign("RTCMINE",RTCMINE  , envir = env)
  assign("SHCMINE",SHCMINE  , envir = env)
  assign("SRCMINE",SRCMINE  , envir = env)
  assign("STCMINE",STCMINE  , envir = env)
  assign("NMINEP", NMINEP , envir = env)  
  assign("NMOBR",NMOBR , envir = env)
  assign("VNMOBR",VNMOBR , envir = env)
  assign("RLSEN",RLSEN  , envir = env)
  assign("CMINELF",CMINELF, envir = env)
  assign("CMINERT",CMINERT, envir = env)
  assign("CMINESH",CMINESH, envir = env)
  assign("CMINESR",CMINESR, envir = env)
  assign("CMINEST",CMINEST, envir = env)
  assign("NMINELF",NMINELF, envir = env)
  assign("NMINERT",NMINERT, envir = env)
  assign("NMINESR",NMINESR, envir = env)
  assign("NMINEST",NMINEST, envir = env)
  assign("SHNMINE",SHNMINE, envir = env)
  assign("CMOBSR",CMOBSR , envir = env)
  assign("NMOBSR",NMOBSR , envir = env)
  assign("LAIMOBR",LAIMOBR, envir = env)
  return()
  #END # SUBROUTINE FOR_SENMOB
}
#***********************************************************************
#     SENES VARIABLE DEFINITIONS:
#-----------------------------------------------------------------------
# CHAR      Contains the contents of last record read
# CMINELF   Potential mobile CH2O avaialable today from leaf (g [CH2O] m-2)
# CMINEO        DSSAT4 potential CH2O mobilization from storage (g[CH2O] / m2 / d)
# CMINEP        Potential whole-plant CH2O mobilization from storage (g[CH2O] / m2 / d)
# CMINERT   Potential mobile CH2O avaialable today from root (g [CH2O] m-2)
# CMINESR   Potential mobile CH2O avaialable today from STOR (g [CH2O] m-2)
# CMINEST   Potential mobile CH2O avaialable today from stem (g [CH2O] m-2)
# DAS       Days after start of simulation (days)
# DAYL      Current daylength (hours)
# DAYL_1    Yesterdays daylength (hours)
# DAYL_1    Daylength two days ago (hours)
# DLAYR(L)  Soil Depth in layer L (cm)
# DTX       Thermal time that occurs in a real day based on vegetative
#             development temperature function (thermal days / day)
# DYNAMI#   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG,
#             INTEGR, OUTPUT, or SEASEND
# ERR       Error code for file operation
# FILEC#    Path plus filename for species file (*.spe)
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
# LFCMINE        Today's maximum potential CH2O mobilization from leaf (g [CH2O] m-2)
# LFNMINE   Today's maximum potential N mobilization from leaf (g [N] m-2)
# LFNSEN        Defoliation from natural senescence (g [DM] m-2 d-1)
# LFSEN     Maximum rate of natural leaf senescence per physiological day
# LFSCMOB   Mass of leaf CH2O mobilized from tissue lost to natural
#              senescence (g [CH2O] m-2 d-1)
# LFSNMOB   Mass of leaf N mobilized from tissue lost to natural
#              senescence (g [N] m-2 d-1)
# LNUM      Current line number of input file
# LTSEN     Senescence of lower leaves due to self-shading of canopy
#             (1/day) 8/3/05 now is g [DM] m-2 d-1
# LUNCRP    Logical unit number for FILEC (*.spe file)
# NL        Maximum number of soil layers = 20
# NLAYR     Number of soil layers
# NMINELF   Potential mobile N avaialable today from leaf (g [N] m-2)
# NMINEO        DSSAT4 potential N mobilization from storage (g[N] / m2 / d)
# NMINEP        Potential whole-plant N mobilization from storage (g[N] / m2 / d)
# NMINERT   Potential mobile N avaialable today from root (g [N] m-2)
# NMINESR   Potential mobile N avaialable today from STOR (g [N] m-2)
# NMINEST   Potential mobile N avaialable today from stem (g [N] m-2)
# NR7       Day when 50% of plants first have yellowing or maturing pods
#             (days)
# PAR       Daily photosynthetically active radiation or photon flux
#             density (moles[quanta]/m2-d)
# PORLFT    Proportion of leaf weight grown which will have been senesced
#             if no water stress has occurred prior to this V-stage
# PORPT     Ratio of petiole to leaf weight
# RATTP     Factor used in determining increased senescence due to water
#             stress
# RFAC1     Root length per unit  root weight. (cm/g)
# RFAC3     Ratio of root length to root weight at the current time (cm/g)
# RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# RTCMINE        Today's maximum potential CH2O mobilization from root (g [CH2O] m-2)
# RTNMINE   Today's maximum potential N mobilization from root (g [N] m-2)
# RTSEN     Fraction of existing root length which can be senesced per
#             physiological day. (fraction / ptd)
# RTSCMOB   Mass of root CH2O mobilized from tissue lost to natural senescence
#              (g [CH2O] m-2 d-1)
# RTSNMOB   Mass of root N mobilized from tissue lost to natural senescence
#              (g [N] m-2 d-1)
# RTWT      Dry mass of root tissue, including C and N
#             (g[root] / m2[ground])
# SECTION   Section name in input file
# SENDAY    Maximum fraction of existing leaf weight which can be senesced
#             on day N as a function of severe water stress 4 days earlier.
# SENMAX(I) Maximum proportion of total leaf weight as a function of
#             V-stage (XSENMX(I)) which can be senesced due to water stress.
# SENCLV     Proportion used to calculate amount of CHO mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENCRV     Proportion used to calculate amount of CHO mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENCSV     Proportion used to calculate amount of CHO mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENCSRV     Proportion used to calculate amount of CHO mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENNLV     Proportion used to calculate amount of N mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENNRV     Proportion used to calculate amount of N mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENNSV     Proportion used to calculate amount of N mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
# SENNSRV     Proportion used to calculate amount of N mobilized from leaves lost to
#               natural senescence, low-light senescence, and N-mobilization senescence
#               Is a fraction of the difference between PCNL (current N concentration and
#               PROLFI*0.16 or "final" N concentration.
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
# SENSR        Constant for senescence of storage organ tissue
#                  (proportion of cumulative storage weight lost / physiological day)
# SENWT     Leaf senescence due to N mobilization (g[leaf] / m2[ground])
# SHCMINE   Potential mobile CH2O avaialable today from shell (g [CH2O] m-2)
# SHNMINE   Potential mobile N avaialable today from shell (g [N] m-2)
# SLAAD     Specific leaf area, excluding weight of C stored in leaves
#             (cm2[leaf] / g[leaf])
# SLMDOT    Defoliation due to daily leaf senescence that is lost at PROLFF,
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SRCMINE        Today's maximum potential CH2O mobilization from STOR (g [CH2O] m-2)
# SRMDOT    Daily root senescence that is lost at PRORTF,
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SRNMINE   Today's maximum potential N mobilization from STOR (g [N] m-2)
# SRSCMOB   Mass of STOR CH2O mobilized from tissue lost to natural senescence
#              (g [CH2O] m-2 d-1)
# SRSNMOB   Mass of STOR N mobilized from tissue lost to natural senescence
#              (g [N] m-2 d-1)
# SSMDOT    Daily petiole senescence that is lost at PROSTF,
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SSRMDOT   Daily STOR senescence that is lost at PROSRF,
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# STCMINE        Today's maximum potential CH2O mobilization from stem (g [CH2O] m-2)
# STMWT     Dry mass of stem tissue, including C and N
#             (g[stem] / m2[ground)
# STNMINE   Today's maximum potential N mobilization from stem (g [N] m-2)
# STRWT     Dry mass of storage organ tissue, including C and N
# STSCMOB   Mass of petiole CH2O mobilized from tissue lost to natural senescence
#              (g [CH2O] m-2 d-1)
# STSNMOB   Mass of petiole N mobilized from tissue lost to natural senescence
#              (g [N] m-2 d-1)
# TABEX     Function subroutine - Lookup utility
# TCMP      Time constant for senescence of lower leaves because of
#             excessive self-shading by crop canopy (thermal days)
# TIMDIF    Integer function which calculates the number of days between
#             two Julian dates (da)
# TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
# TRTDY     Total root length per square cm soil yesterday
#             (cm[root]/cm2[soil])
# TSCMOB    Total plant CH2O mobilized from tissue lost to natural and
#              low-light senescence (g [CH2O] m-2 d-1)
# TSNMOB    Total plant N mobilized from tissue lost to natural and
#              low-light senescence (g [N] m-2 d-1)
# VSTAGE    Number of nodes on main stem of plant
# WRDOTN    Dry weight growth rate of new root tissue including N but not C
#             reserves (g[root] / m2[ground]-d)
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
#     END SUBROUTINE FOR_SENMOB
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
#     Variables definitions
#-----------------------------------------------------------------------
# DAP       Number of days after planting (d)
# IDAP(I,J) Day of pest damage for pest I, observation J
#             (days after planting)
# PCN       Number of pests in FILET
# PL(I)     Pest level from pest progress file (FILET) for pest I (varies)
# RISE      Change in pest damage level between two observations for linear
#             interpolation
# ROW(I)    Counter which points to current row of data for each pest
#             progress curve (I)
# RUN       Change in date between two observations for linear
#             interpolation
# SLOPE     Slope of pest level curve (RISE/RUN) between two observations
# YPL(I,J)  Array for storage of pest data for pest or damage type I,
#             observation J
#-----------------------------------------------------------------------
#     End Subroutine FOR_LINDM
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
#     DORMANT VARIABLES
#-----------------------------------------------------------------------
# BLANK         ' '
# C255       255 character record
# DAYL         Current daylength (hours)
# DAYLY         Yesterdays daylength (hours)
# DRMST         Dormancy status (NODORM=not dormant, DORM=dormant - reversible,
# DYNAMI#         Controls run sequence: DYNAMIC =RUNINIT, SEASINIT, RATE,
#             EMERG, INTEGR, OUTPUT, or SEASEND
# ECONAM     Ecotype name - not used
# ECONO      Used to match ECOTYP in .ECO file
# ECOTYP     Ecotype code
# ERR
# ERRKEY
# ERRNUM
# FILEC, FILEE   Filenames for Crop and Species files
# FILECC, FILEGC File+pathname for Crop and Eco files
# FILEIO    Filename for Input file
# FNPGD(1)  Base daylength for CURV function for daylength effect on Pg
#              for short-day dormancy (daylength when dormancy is maximum)
# FNPGD(2)  Daylength threshold where dormancy effect begins
#              for daylength effect on Pg (for short-day dormancy)
# FNPGD(3)  Longest daylength threshold where there is no dormancy effect
#              for daylength effect on Pg (for long-day dormancy)
# FNPGD(4)  Daylength when dormancy effect is maximum
#              for daylength effect on Pg (long-day dormancy)
# FNPMD(1)  Base daylength for CURV function for daylength effect on mobilization
#              for short-day dormancy (daylength when dormancy is maximum)
# FNPMD(2)  Daylength threshold where dormancy effect begins
#              for daylength effect on mobilization (for short-day dormancy)
# FNPMD(3)  Longest daylength threshold where there is no dormancy effect
#              for daylength effect on mobilization (for long-day dormancy)
# FNPMD(4)  Daylength when dormancy effect is maximum
#              for daylength effect on mobilization (long-day dormancy)
# FNPTD(1)  Ignored for short-day dormancy
# FNPTD(2)  Daylength threshold where dormancy effect is maximum
#              for daylength effect on partitioning (for short-day dormancy)
# FNPTD(3)  Shortest daylength threshold where there is no dormancy effect
#              for daylength effect on partitioning (for short-day dormancy)
# FNPTD(4)  Minimum relative effect of dormancy when crop is non-dormant (set to 0.0)
# FREEZ2    Temperature below which plant growth stops completely. (�C)
# FRZD#        Freezing death coefficient  - percentage tissue/population death per day per degree below FREEZ2)
# FRZDHD(1) Minimum temperature at which dehardening begins (relative rate=0)
# FRZDHD(2) Temperature at which dehardening reaches maximum rate (relative rate=1)
# FRZDHD(3) Not used
# FRZDHD(4) Maximum (absolute) rate of dehardening (degrees C increase above HARD2 per day)n of STRWT and PLNTPOP)
# FRZHRD(1) Temperature at which cold hardening reaches maximum rate (relative rate=1)
# FRZHRD(2) Temperature below which cold hardening begins (relative rate=0)
# FRZHRD(3) Temperature at which hardening is reversed at maximum rate (relative rate=-1)
# FRZHRD(4) Maximum (absolute) rate of cold hardening (degrees C decrease towards HARD2 per day)
# HARD1        Killing low temperature before cold hardening (begins killing storage organ)
# HARD2        Killing low temperature after cold hardening (begins killing storage organ)
# ISECT
# LNUM
# LUNECO    Logical unit number for ECO files
# LUNIO     Input file logical unit no.
# PATHL
# PPGFA#        Reduction in photosynthetic rate due to dormancy
# PPMFA#        Reduction in mobilization rate due to dormancy
# PPTFA#        Reduction in partitioning to vegetative tissues during dormancy
# RCHDP        Ecotype relative cold hardening potential (0-1)
# RDRMG     Relative sensitivity of ecotype to daylength/dormancy effects on Pg
# RDRMM     Relative sensitivity of ecotype to daylength/dormancy effects on
#              mobilization
# RDRMT     Relative sensitivity of ecotype to daylength/dormancy effects on
#           partitioning to perenniating tissues
# SECTION   Heading name in input files
# TMIN        Daily average temperature
# TYPDHD        Response type for cold dehardening
# TYPHRD         Response type for cold hardening
# TYPPGD        Type of response curve for effect of daylength/dormancy on Pg
# TYPPMD    Type of response curve for effect of daylength/dormancy on
#           mobilization
# TYPPTD    Type of response curve for effect of daylength/dormancy on
#           partitioning to perenniating organ
#-----------------------------------------------------------------------
#     END SUBROUTINE DORMANT
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
#     Variable definitions
#-----------------------------------------------------------------------
# ASMDOT  Daily assimilative damage (g[CH2O] /m2 / d)
# CASM    Cumulative assimilate damage (g[CH2O] / m2)
# DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR,
#           OUTPUT, or SEASEND
# PGAVL   Total available CH2O available for growth & respiration
#           (g[CH2O] / m2)
# PPSR    Assimilate damage in daily percent photosynthesis reduction (%/d)
# TPSR    Daily absolute assimilate damage (g[CH2O]/m2/d)
#-----------------------------------------------------------------------
#     End Subroutine ASMDM
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
#     Variable definitions
#-----------------------------------------------------------------------
# AREALF  Area of leaves (one side) per unit ground area
#           (cm2[leaf] / m2[ground])
# CLAI    Cumulative leaf area index destroyed (m2/m2)
# CLFM    Cumulative leaf mass destroyed  (g/m2)
# CLFRZ   Cumulative frozen leaf tissue (g[leaf]/m2)
# CLSEN   Cumulative leaf senescence (g/m2)
# CLW     Cumulative leaf growth (g[leaf]/m2)
# CSFRZ      Cumulative frozen stem tissue (g[stem]/m2)
# CSRFRZ      Cumulative frozen storage organ tissue (g[storage]/m2)
# CSRW      Cumulative storage organ growth (g[storage]/m2)
# CSTEM   Cumulative stem mass destroyed (g/m2)
# CSTRM      Cumulative storage organ mass destroyed (g/m2)
# CSW     Cumulative stem growth (g[stem]/m2)
# DISLA   Diseased leaf area (cm2[leaf]/m2[ground]/d)
# DISLAP  Percent diseased leaf area (%/d)
# DSTEM   Desired stem mass (g/m2/d)
# DSTOR   Desired storage organ mass (g/m2/d)
# DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
#           OUTPUT, or SEASEND 
# LAIDAM  Change in leaf area index due to current pest
#           (m2[leaf]/m2[ground])
# LAIDOT  Daily change in leaf area index due to pest damage (m2/m2/d)
# LDAM    Daily leaf damage (g/m2/d)
# PCLMA   Percent observed leaf mass (WTLF) damage (%)
# PCLMT   Percent of total leaf mass (WTLF + senescence) destroyed (%)
# PCSTMD  Observed cumulative percentage stem mass damage (%)
# PCSTRD  Observed cumulative percentage storage organ mass damage (%)
# PDLA    Percent diseased leaf area (%)
# PLFAD   Daily percent leaf area damage (%/d)
# PLFMD   Percent leaf mass damage (%/d)
# PSTMD   Daily percent stem mass damage (%)
# PSTRD   Daily percent storage organ mass damage (%)
# PVSTGD  Percent V-stage damage (%)
# PWTLF   Potential available leaf mass after the observed leaf damage is 
#           applied (g[leaf]/m2)
# SDAM    Calculated stem damage (g/m2/d)
# SLA     Specific leaf area (cm2[leaf] / m2[ground])
# SLDOT   Defoliation due to daily leaf senescence (g/m2/day)
# SRDAM   Calculated storage organ damage (g/m2/d)
# SSDOT   Daily senescence of petioles (g / m2 / d)
# SSRDOT      Daily senescence of storage organ(g / m2 / d)
# STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
# STRWT   Dry mass of storage organ, including C and N (g[storage] / m2[ground)
# TDLA    Total diseased leaf area (cm2/m2)
# TLFAD   Total leaf area damage (cm2/cm2/d)
# TLFMD   Total leaf mass damage (g/m2/day)
# VSTAGE  Number of nodes on main stem of plant (nodes)
# VSTGD   Absolute daily V-stage damage (nodes/day)
# WLFDOT  Leaf weight losses due to freezing (g[leaf]/m2-d)
# WLIDOT  Daily pest or freeze damage to leaf mass (g/m2/day)
# WSFDOT        Stem weight losses due to freezing (g[stem]/m2-d)
# WSIDOT  Daily pest damage to stem mass (g/m2/day)
# WSRFDOT      Storage organ weight losses due to freezing (g[storage]/m2-d)
# WSRIDOT      Daily pest damage to storage organ mass (g/m2/day)
# WSTMD   Daily absolute stem damage (g/m2/day)
# WSTRD   Daily absolute storage organ damage (g/m2/day)
# WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
#-----------------------------------------------------------------------
#     End Subroutine FOR_VEGDM
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#     Variable definitions
#-----------------------------------------------------------------------
# CRLF      Cumulative root length flux (cm [root] / cm2 [ground])
# CRLV      Cumulative root length density (cm [root] / cm3 [soil])
# CRTM      Cumulative root mass (g/m2)
# CUMDEP    Cumulative depth of soil profile (cm)
# DLAYR(L)  Soil Depth in layer L (cm)
# DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
#             INTEGR, OUTPUT, or SEASEND 
# NL        Maximum number of soil layers = 20 
# NLAYR     Actual number of soil layers 
# PRLV      Percent reductin in root length volume in each layer, %
# PRTLF     Percent of root flux destroyed (%/d)
# PRTLV     Daily percent reduction in root length volume  (%/d)
# PRTMD     Daily percent root mass damage (%/d)
# RLFDOT    Daily root length flux damage (cm root / cm2 ground)
# RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# RLVDOT    Daily root length damage (cm root / cm3 soil)
# RTFRAC(L) Fraction of total root mass in soil layer L 
# RTWT      Dry mass of root tissue, including C and N
#             (g[root] / m2[ground])
# TRLV      Total root length per square cm soil today
#             (cm[root] / cm2[ground)
# TRTLF     Total root flux destroyed (cm/cm2/d)
# TRTLV     Daily absolute reduction in root length volume  (cm/cm3/d)
# WRIDOT    Daily pest damage to root mass (g/m2/day)
# WRTMD     Daily absolute root mass reduction  (g/m2/day)
#-----------------------------------------------------------------------
#     End Subroutine FOR_ROOTDM
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#     Variable Definitions
#-----------------------------------------------------------------------
# CSDM     Cumulative seed mass destroyed (g/m2)
# CSDN     Cumulative number of seeds destroyed (#/m2)
# CSHM     Cumulative shell mass destroyed (g/m2)
# CSHN     Cumulative number of shells destroyed (#/m2)
# DAS      Days after start of simulation (d)
# DEST     Percentage of shells destroyed 
# DYNAMI#  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
#            INTEGR, OUTPUT, or SEASEND 
# LAGSD    Time required between shell growth and seed growth, per cohort
#            (Photo-thermal days)
# LNGPEG   Time between start of peg (full flower) and shell formation (for 
#            peanuts only).  Defines slow growth period. (Photo-thermal days)
# NPP      Cohort number used as index in loops 
# NR2      Day when 50% of plants have one peg (peanuts only) (d)
# NSDDL    Daily number of large seeds damaged (#/m2/d)
# NSDDM    Daily number of mature seeds damaged (#/m2/d)
# NSDDS    Daily number of small seeds damaged (#/m2/d)
# NSHDL    Daily number of large shells damaged (#/m2/d)
# NSHDM    Daily number of mature shells damaged (#/m2/d)
# NSHDS    Daily number of small shells damaged (#/m2/d)
# PAGE     Photothermal age of each cohort  (Photo-thermal days)
# PHTHRS8  Threshold time that must accumulate in phase 8 for the next 
#            stage to occur.  Equivalent to PHTHRS(8) in Subroutine PHENOLOG.
#            (photothermal days)
# PHTIM    Cumulative photothermal time ages of seeds and shells 
# PSDDL    Percent large seed mass damage (%/d)
# PSDDM    Percent mature seed mass damage (%/d)
# PSDDS    Percent small seed mass damage (%/d)
# PSHDL    Percent large shell mass damage (%/d)
# PSHDM    Percent mature shell mass damage (%/d)
# PSHDS    Percent small shell mass damage (%/d)
# SDDES(J) Number of seeds destroyed today in cohort J when shells are 
#            not destroyed (#/m2/d)
# SDIDOT   Number of seeds destroyed on the current day (#/m2/d)
# SDNDES(J)Number of seeds destroyed in cohort J by pests or disease
#            (#/m2/day)
# SDNO(J)  Number of seeds for cohort J (#/m2)
# SDWT     Seed weight, g/m2
# SDWDES(J)Mass of seeds in cohort J destroyed today by pest or disease 
#            (g/m2/d)
# SHELN(J) Number of shells for cohort J (#/m2)
# SHIDOT   Number of shells destroyed on current day (#/m2/d)
# SWIDOT   Daily seed mass damage (g/m2/day)
# TSDNO    Total seed number in all cohorts (#/m2)
# TSDNOL   Total number of large seeds (#/m2)
# TSDNOM   Total number of mature seeds (#/m2)
# TSDNOS   Total number of small seeds (#/m2)
# TSDWT    Total seed mass in all cohorts (g/m2)
# TSDWTL   Seed mass for large seeds (g/m2)
# TSDWTM   Seed mass for mature seeds (g/m2)
# TSDWTS   Seed mass for small seeds (g/m2)
# TSHNO    Total shell number in all cohorts (#/m2)
# TSHNOL   Number shells with large seeds (#/m2)
# TSHNOM   Number shells with mature seeds (#/m2)
# TSHNOS   Number shells with small seeds (#/m2)
# TSHWT    Total shell mass in all cohorts (g/m2)
# TSHWTL   Shell mass with large seeds (g/m2)
# TSHWTM   Shell mass with mature seeds (g/m2)
# TSHWTS   Shell mass with small seeds (g/m2)
# WPS      Number seeds per gram (#/g)
# WSDDL    Daily mass of large seed damaged (g/m2/day)
# WSDDM    Daily mass of mature seed damaged (g/m2/day)
# WSDDS    Daily mass of small seed damaged (g/m2/day)
# WSHDL    Daily mass of large shell damaged (g/m2/day)
# WSHDM    Daily mass of mature shell damaged (g/m2/day)
# WSHDS    Daily mass of small shell damaged (g/m2/day)
# WSHIDT   Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WTSD(J)  Seed mass  for cohort J (g/m2)
# WTSHE(J) Shell mass  for cohort J (g/m2)
#-----------------------------------------------------------------------
#     End Subroutine FOR_SEEDDM
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#     FOR_PESTCP Variable Definitions
#-----------------------------------------------------------------------
# CPPLTD     Cumulative percent of plants destroyed (%)
# DAM        Daily absolute damage 
# DDAM       Potential pest damage for damage types with competition 
# DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
#              INTEGR, OUTPUT, or SEASEND 
# FSM        Food source mass or number  
# HPDAM         Harvest removal (proportion of TOPWT)
# LAIW       Weed leaf area (m2/m2)
# NPLTD      Number of plants destroyed (#/m2/d)
# NSDDL      Daily number of large seeds damaged (#/m2/d)
# NSDDM      Daily number of mature seeds damaged (#/m2/d)
# NSDDS      Daily number of small seeds damaged (#/m2/d)
# NSHDL      Daily number of large shells damaged (#/m2/d)
# NSHDM      Daily number of mature shells damaged (#/m2/d)
# NSHDS      Daily number of small shells damaged (#/m2/d)
# PCLMA      Percent observed leaf mass (WTLF) damage (%)
# PCLMT      Percent of total leaf mass (WTLF + senescence) destroyed (%)
# PCN        Number of pests in FILET 
# PCPID(I,J) Pest coupling points identification code for pest I, coupling 
#              point J 
# PCSTMD     Observed cumulative percentage stem mass damage (%)
# PCSTRD     Observed cumulative percentage storage organ mass damage (%)
# PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
#              daily damage rate, (2) percent observed damage, (3) daily 
#              percent damage rate, (4) absolute daily damage rate w/ 
#              preference and competition. 
# PDAM       Pest damage which may be applied to subsequent coupling points
# PDCF1(I,J) Pest damage coefficient associated with pest I, coupling point 
#              J 
# PDLA       Percent diseased leaf area (%)
# PL(I)      Pest level from pest progress file (FILET) for pest I (varies)
# PLAIW      Percent weed leaf area = LAIW/(LAIW+LAI crop).  Calculated in 
#              FOR_PESTCP, but not used anywhere. (%)
# PLFAD      Daily percent leaf area damage (%/d)
# PLFMD      Percent leaf mass damage (%/d)
# PLTPOP     Plant population (# plants / m2)
# PNO(I)     Pest number from File P for pest I 
# PPLTD      Percent plants destroyed  (%/m2/d)
# PPSR       Assimilate damage in daily percent photosynthesis reduction
#              (%/d)
# PRLV       Percent of root length volume destroyed
# PRTLF      Percent of root flux destroyed (%/d)
# PRTLV      Daily percent reduction in root length volume  (%/d)
# PRTMD      Daily percent root mass damage (%/d)
# PSDD       Percent of seed mass destroyed
# PSDDL      Percent large seed mass damage (%/d)
# PSDDM      Percent mature seed mass damage (%/d)
# PSDDS      Percent small seed mass damage (%/d)
# PSHDL      Percent large shell mass damage (%/d)
# PSHDM      Percent mature shell mass damage (%/d)
# PSHDS      Percent small shell mass damage (%/d)
# PSTMD      Daily percent stem mass damage (%)
# PSTRD      Daily percent storage organ mass damage (%)
# PVSTGD     Percent V-stage damage (%)
# REMDAM     Remainder of damage for multiple pest coupling points 
# RTWT       Dry mass of root tissue, including C and N
#              (g[root] / m2[ground])
# SLA        Specific leaf area (cm2[leaf] / m2[ground])
# STMWT      Dry mass of stem tissue, including C and N
#              (g[stem] / m2[ground)
# TDLA       Total diseased leaf area (cm2/m2)
# TLFAD      Total leaf area damage (cm2/cm2/d)
# TLFMD      Total leaf mass damage (g/m2/day)
# TOPWT      Total above ground biomass (g/m2)
# TPSR       Daily absolute assimilate damage (g[CH2O]/m2/d)
# TRTLF      Total root flux destroyed (cm/cm2/d)
# TRTLV      Daily absolute reduction in root length volume  (cm/cm3/d)
# TSDNOL     Total number of large seeds (#/m2)
# TSDNOM     Total number of mature seeds (#/m2)
# TSDNOS     Total number of small seeds (#/m2)
# TSDWTL     Seed mass for large seeds (g/m2)
# TSDWTM     Seed mass for mature seeds (g/m2)
# TSDWTS     Seed mass for small seeds (g/m2)
# TSHNOL     Number shells with large seeds (#/m2)
# TSHNOM     Number shells with mature seeds (#/m2)
# TSHNOS     Number shells with small seeds (#/m2)
# TSHWTL     Shell mass with large seeds (g/m2)
# TSHWTM     Shell mass with mature seeds (g/m2)
# TSHWTS     Shell mass with small seeds (g/m2)
# VSTAGE     Number of nodes on main stem of plant (nodes)
# VSTGD      Absolute daily V-stage damage (nodes/day)
# WRTMD      Daily absolute root mass reduction  (g/m2/day)
# WSDD       Daily mass of seeds damaged (g/m2/d)
# WSDDL      Daily mass of large seed damaged (g/m2/day)
# WSDDM      Daily mass of mature seed damaged (g/m2/day)
# WSDDS      Daily mass of small seed damaged (g/m2/day)
# WSHDL      Daily mass of large shell damaged (g/m2/day)
# WSHDM      Daily mass of mature shell damaged (g/m2/day)
# WSHDS      Daily mass of small shell damaged (g/m2/day)
# WSTMD      Daily absolute stem damage (g/m2/day)
# WSTRD      Daily absolute storage mass damage (g/m2/day)
# WTLF       Dry mass of leaf tissue including C and N
#              (g[leaf] / m2[ground])
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#     PEST Variable Definitions
#-----------------------------------------------------------------------
# AREALF     Area of leaves (one side) per unit ground area
#              (cm2[leaf] / m2[ground])
# ASMDOT     Daily assimilative damage (g[CH2O] /m2 / d)
# CASM       Cumulative assimilate damage (g[CH2O] / m2)
# CLAI       Cumulative leaf area index destroyed (m2/m2)
# CLFM       Cumulative leaf mass destroyed  (g/m2)
# CLW        Cumulative leaf growth (g[leaf]/m2)
# CPPLTD     Cumulative percent of plants destroyed (%)
# CRLF       Cumulative root length flux (cm root / cm2 ground)
# CRLV       Cumulative root length density (cm root / cm3 soil)
# CROPD      Name of crop 
# CRTM       Cumulative root mass (g/m2)
# CSDM       Cumulative seed mass destroyed (g/m2)
# CSDN       Cumulative number of seeds destroyed (#/m2)
# CSFRZ      Cumulative frozen stem tissue (g[stem]/m2)
# CSHM       Cumulative shell mass destroyed (g/m2)
# CSHN       Cumulative number of shells destroyed (#/m2)
# CSRW         Cumulative storage organ growth (g[storage]/m2)
# CSTEM      Cumulative stem mass destroyed (g/m2)
# CSTRM         Cumulative storage organ mass destroyed (g/m2)      
# CSW        Cumulative stem growth (g[stem]/m2)
# DAP        Number of days after planting (d)
# DAS        Days after start of simulation (d)
# DISLA      Diseased leaf area (cm2[leaf]/m2[ground]/d)
# DISLAP     Percent diseased leaf area (%/d)
# DLAYR(L)   Soil Depth in layer L (cm)
# DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
#              INTEGR, OUTPUT, or SEASEND 
# FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
# FILEP      Filename for pest coefficient file 
# FILET      Pest time series file 
# HPDAM         Harvest removal (proportion of TOPWT)
# IDAP(I,J)  Day of pest damage for pest I, observation J
#              (days after planting)
# IDETD      Switch to generate PEST.OUT file (Y or N) 
# LAGSD      Time required between shell growth and seed growth, per cohort
#              (Photo-thermal days)
# LAIDOT     Daily change in leaf area index due to pest damage (m2/m2/d)
# LNGPEG     Time between start of peg (full flower) and shell formation 
#              (for peanuts only).  Defines slow growth period.
#              (Photo-thermal days)
# LUNIO      Logical unit number for FILEIO 
# MULTI      Current simulation year (=1 for first or single simulation, 
#              =NYRS for last seasonal simulation) 
# NL         Maximum number of soil layers = 20 
# NLAYR      Actual number of soil layers 
# NPEST      Number of pest or damage types in FILEP 
# NPLTD      Number of plants destroyed (#/m2/d)
# NR2        Day when 50% of plants have one peg (peanuts only) (d)
# NSDDL      Daily number of large seeds damaged (#/m2/d)
# NSDDM      Daily number of mature seeds damaged (#/m2/d)
# NSDDS      Daily number of small seeds damaged (#/m2/d)
# NSHDL      Daily number of large shells damaged (#/m2/d)
# NSHDM      Daily number of mature shells damaged (#/m2/d)
# NSHDS      Daily number of small shells damaged (#/m2/d)
# OUTD       File name for pest damage output file (e.g., PEST.OUT) 
# PATHPE     Path name for pest information file 
# PCLMA      Percent observed leaf mass (WTLF) damage (%)
# PCLMT      Percent of total leaf mass (WTLF + senescence) destroyed (%)
# PCN        Number of pests in FILET 
# PCPID(I,J) Pest coupling points identification code for pest I, coupling 
#              point J 
# PCSTMD     Observed cumulative percentage stem mass damage (%)
# PCSTRD  Observed cumulative percentage storage organ mass damage (%)
# PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
#              daily damage rate, (2) percent observed damage, (3) daily 
#              percent damage rate, (4) absolute daily damage rate w/ 
#              preference and competition. 
# PDCF1(I,J) Pest damage coefficient associated with pest I, coupling point 
#              J 
# PDLA       Percent diseased leaf area (%)
# PGAVL      Total available CH2O available for growth & respiration
#              (g[CH2O] / m2)
# PHTHRS8    Threshold time that must accumulate in phase 8 for the next 
#              stage to occur.  Equivalent to PHTHRS(8) in Subroutine 
#              PHENOLOG. (photothermal days)
# PHTIM      Cumulative photothermal time ages of seeds and shells 
# PID(I)     Pest identification header from FILEP (max 200) 
# PL(I)      Pest level from pest progress file (FILET) for pest I (varies)
# PLFAD      Daily percent leaf area damage (%/d)
# PLFMD      Percent leaf mass damage (%/d)
# PLTPOP     Plant population (# plants / m2)
# PNO(I)     Pest number from File P for pest I 
# POBS(I)    Number of observations for pest I 
# PPLTD      Percent plants destroyed  (%/m2/d)
# PPSR       Assimilate damage in daily percent photosynthesis reduction
#              (%/d)
# PRLV       Percent of root length volume destroyed, %
# PRTLF      Percent of root flux destroyed (%/d)
# PRTLV      Daily percent reduction in root length volume  (%/d)
# PRTMD      Daily percent root mass damage (%/d)
# PSDD       Percent of seed mass destroyed (%/d)
# PSDDL      Percent large seed mass damage (%/d)
# PSDDM      Percent mature seed mass damage (%/d)
# PSDDS      Percent small seed mass damage (%/d)
# PSHDL      Percent large shell mass damage (%/d)
# PSHDM      Percent mature shell mass damage (%/d)
# PSHDS      Percent small shell mass damage (%/d)
# PSTHD(I)   Column heading for each pest progress curve in FILET (max 6) 
# PSTMD      Daily percent stem mass damage (%)
# PVSTGD     Percent V-stage damage (%)
# RLFDOT     Daily root length flux damage (cm root / cm2 ground)
# RLV(L)     Root length density for soil layer L (cm[root] / cm3[soil])
# RLVDOT     Daily root length damage (cm root / cm3 soil)
# RTWT       Dry mass of root tissue, including C and N
#              (g[root] / m2[ground])
# SDDES(J)   Number of seeds destroyed today in cohort J when shells are 
#              not destroyed (#/m2/day)
# SDIDOT     Number of seeds destroyed on the current day (#/m2/d)
# SDNO(J)    Number of seeds for cohort J (#/m2)
# SDWT       Seed weight, g/m2
# SHELN(J)   Number of shells for cohort J (#/m2)
# SHIDOT     Number of shells destroyed on current day (#/m2/d)
# SLA        Specific leaf area (cm2[leaf] / m2[ground])
# SLDOT      Defoliation due to daily leaf senescence (g/m2/day)
# SSDOT      Daily senescence of petioles (g / m2 / d)
# SSRDOT         Daily senescence of storage organ (g / m2 / d)
# STRWT      Dry mass of storage organ tissue, including C and N
#              (g[storage] / m2[ground)
# STMWT      Dry mass of stem tissue, including C and N
#              (g[stem] / m2[ground)
# SWIDOT     Daily seed mass damage (g/m2/day)
# TDLA       Total diseased leaf area (cm2/m2)
# TIMDIF     Integer function which calculates the number of days between 
#              two Julian dates (da)
# TITLET     Description of treatment for this simulation 
# TLFAD      Total leaf area damage (cm2/cm2/d)
# TLFMD      Total leaf mass damage (g/m2/day)
# TOPWT      Total above ground biomass (g/m2)
# TPSR       Daily absolute assimilate damage (g[CH2O]/m2/d)
# TRTLF      Total root flux destroyed (cm/cm2/d)
# TRTLV      Daily absolute reduction in root length volume  (cm/cm3/d)
# TRTNO      Treatment number being simulated (from FILEX) 
# TSDNOL     Total number of large seeds (#/m2)
# TSDNOM     Total number of mature seeds (#/m2)
# TSDNOS     Total number of small seeds (#/m2)
# TSDWTL     Seed mass for large seeds (g/m2)
# TSDWTM     Seed mass for mature seeds (g/m2)
# TSDWTS     Seed mass for small seeds (g/m2)
# TSHNOL     Number shells with large seeds (#/m2)
# TSHNOM     Number shells with mature seeds (#/m2)
# TSHNOS     Number shells with small seeds (#/m2)
# TSHWTL     Shell mass with large seeds (g/m2)
# TSHWTM     Shell mass with mature seeds (g/m2)
# TSHWTS     Shell mass with small seeds (g/m2)
# VSTAGE     Number of nodes on main stem of plant (nodes)
# VSTGD      Absolute daily V-stage damage (nodes/day)
# WLFDOT     Leaf weight losses due to freezing (g[leaf]/m2-d)
# WLIDOT     Daily pest or freeze damage to leaf mass (g/m2/day)
# WRIDOT     Daily pest damage to root mass (g/m2/day)
# WRTMD      Daily absolute root mass reduction  (g/m2/day)
# WSDD       Daily weight of seeds destroyed (g/m2/d)
# WSDDL      Daily percent of large seed damaged (g/m2/day)
# WSDDM      Daily percent of mature seed damaged (g/m2/day)
# WSDDS      Daily percent of small seed damaged (g/m2/day)
# WSFDOT        Stem weight losses due to freezing (g[stem]/m2-d)
# WSHDL      Daily mass of large shell damaged (g/m2/day)
# WSHDM      Daily mass of mature shell damaged (g/m2/day)
# WSHDS      Daily mass of small shell damaged (g/m2/day)
# WSHIDT     Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WSIDOT     Daily pest damage to stem mass (g/m2/day)
# WSRFDOT    Storage organ weight losses due to freezing (g[storage]/m2-d)
# WSRIDOT    Daily pest damage to storage organ mass (g/m2/day)
# WSTMD      Daily absolute stem damage (g/m2/day)
# WTLF       Dry mass of leaf tissue including C and N
#              (g[leaf] / m2[ground])
# WTSD(J)    Seed mass  for cohort J (g/m2)
# WTSHE(J)   Shell mass  for cohort J (g/m2)
# YPL(I,J)   Array for storage of pest data for pest or damage type I, 
#              observation J 
# YRDOY      Current day of simulation (YYDDD)
# YRPLT      Planting date (YYDDD)
# YRSIM      Start of simulation date (YYDDD)
#-----------------------------------------------------------------------
#     End Subroutine PEST
#-----------------------------------------------------------------------

