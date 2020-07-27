# Contem as subrotinas: (aqui como 'function's)
  # 1) VSTAGES (VSTAGES.txt com a programacao em fortran) para os estagios vegetativos
  # 2) RSTAGES (RSTAGES.txt com a programacao em fortran) para os estagios reprodutivos

# os comentarios com 'R' na frente são para controle e serão removidos posteriormente
# 'C' ou '!' sao comentarios do codigo original em fortran, portanto, nao usados pelo CROPGRO
# checar 'ifelse's e 'inputs'
# checar GO TO's e IF's sem THEN

# fortran to R
  # .LT.  meaning <
  # .LE.  meaning <=
  # .GT.  meaning >
  # .GE.  meaning >=
  # .EQ.  meaning =
  # .NE.  meaning !=
  # .OR.  meaning |
  # .AND. meaning &
  # DO/ENDDO   meaning for()
  # ELSE IF    meaning else if
  # AMAX1 & AMIN1  meaning max and min

#### Subrotina: VSTAGES ####
{
  
  #=======================================================================
  #  VSTAGES, Subroutine
  #  Calculates V-stages
  #-----------------------------------------------------------------------
  #  REVISION HISTORY
  #  07/24/98 CHP Pulled code from PHENOL subroutine
  #-----------------------------------------------------------------------
  #     Called from:    PHENOL
  #     Calls:          None
  #=======================================================================
  
  VSTAGES <- function (DAS, DTX, EVMODC, MNEMV1, NDVST,                #R !Input
                       NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             #R !Input
                       TURFAC, XPOD, YRDOY, YRPLT,                     #R !Input
                       RVSTGE, VSTAGE,                                 #R !Output
                       DYNAMIC) {                                      #R !Control
    
    #-----------------------------------------------------------------------
    #R CHARACTER*1 PLME
    #R INTEGER DYNAMIC
    #R INTEGER DAS, NVEG0, NVEG1, NDVST
    #R INTEGER YRPLT, YRDOY
    #R REAL VSTAGE, RVSTGE, VSTGED, VSTAGP
    #R REAL MNEMV1, TRIFOL, EVMODC, EVMOD, DTX
    #R REAL TURFAC, XPOD
    #R REAL PHZACC(20)
    
    PLME <- list("")
    PHZACC <- rep(0,20)
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    if (DAS == 1) { # IF (DYNAMIC .EQ. SEASINIT) THEN
      
      #-----------------------------------------------------------------------
      VSTAGE = 0.0
      RVSTGE = 0.0
      VSTGED = 0.0
      VSTAGP = 0.0
      RVSTGE = 0.0
      
      #***********************************************************************
      #***********************************************************************
      #     Daily Rate Calculations
      #***********************************************************************
    } else if (DYNAMIC == RATE) { # todo: check dynamic and rate
      #-----------------------------------------------------------------------
      #    Calculate rate of V-stage change for height and width determination
      #-----------------------------------------------------------------------
      RVSTGE = 0.
      
      if (DAS > DAS > NVEG0 & DAS <= NDVST + ANINT[VSTGED]) {
        if (DAS > NDVST & VSTGED > 0.001) {
          RVSTGE = 1. / VSTGED
          } else {
            RVSTGE = VSTAGE - VSTAGP
          }
      }
      
      #***********************************************************************
      #***********************************************************************
      #     Daily Integration
      #***********************************************************************
      #-----------------------------------------------------------------------
    } else if (DYNAMIC == INTEGR) {
      #     V-STAGE between emergence and unifoliate (V-1) is determined
      #     by the physiological accumulator and the minimum number of
      #     days between emergence and V-1 under optimum temperature (MNEMV1)
      #     V-STAGE after V-1 is determined by the leaf appearance rate
      #     (TRIFOL), a water stress factor (TURFAC) and physiological units
      #     for today (DTX).
      #-----------------------------------------------------------------------
      if (RVSTGE > 1.E-6) {
        VSTGED = 1. / RVSTGE
      } else {
        VSTGED = 0.0
      }
      
      VSTAGP = VSTAGE
      
      #-----------------------------------------------------------------------
      #     V-Stage for transplants
      #-----------------------------------------------------------------------
      if (PLME == "T" & YRPLT == YRDOY) {
        VSTAGE = 1. + (PHZACC[2] - MNEMV1) * TRIFOL
      }
      
      #-----------------------------------------------------------------------
      if (DAS >= NVEG0 & DAS <= NDVST) {
        if (DAS < NVEG1) {
          VSTAGE  = PHZACC[2]/MNEMV1
        } else {
          EVMOD = 1.0 + (abs(EVMODC)- VSTAGE) / EVMODC
          EVMOD = min(2.0,EVMOD)
          EVMOD = max(0.0,EVMOD)
        } else {
          EVMOD = 1.0
        }
      }
      VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD*TURFAC*(1.0-XPOD)
    }
    
    #***********************************************************************
    #***********************************************************************
    #     End of DYNAMIC IF construct
    #***********************************************************************
    #***********************************************************************
    #R  RETURN
    #todo return (x), or assign (usar)?
  }
  #R END SUBROUTINE VSTAGES
} # para facilitar a programacao

#### Subrotina: RSTAGES ####
{
  
  #************************************************************************
  #************************************************************************
  
  RSTAGES <- function (CONTROL,
                       FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    # Input
                       PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      # Input
                       JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     # Output
                       NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       # Output
                       RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      # Output
                       YRNR1, YRNR2, YRNR3, YRNR5, YRNR7) {            # Output
    
    #-----------------------------------------------------------------------
    #R CHARACTER*1 ISIMI, PLME
    #R INTEGER DYNAMIC
    #R INTEGER I, J, NVALP0, DAS, YRDOY, YRPLT, YRSIM
    #R INTEGER NDLEAF,  NDSET, NDVST, JPEND  #R !, TIMDIF
    #R INTEGER RSTAGE,  NVEG0, NVEG1, NR0, NR1, NR2, NR3, NR5, NR7
    #R INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE, YREMRG
    #R INTEGER NPRIOR(20), STGDOY(20), NVALPH(20)
    #R REAL PHTEM, SDEPTH
    #R REAL FT(20), FUDAY(20), FSW(20), FNSTR(20), FPSTR(20), PHTHRS(20)
    #R REAL PHZACC(20), PROG(20), REM(20)
    #R REAL SeedFrac, VegFrac, VegTime
    
    NPRIOR <- rep(0,20); STGDOY <- rep(0,20); NVALPH(20) <- rep(0,20)
    FT <- rep(0,20); FUDAY <- rep(0,20); FSW <- rep(0,20); FNSTR <- rep(0,20); FPSTR <- rep(0,20); PHTHRS(20) <- rep(0,20)
    PHZACC <- rep(0,20); PROG <- rep(0,20); REM <- rep(0,20)
    
    TYPE (ControlType) CONTROL #todo
    DYNAMIC = CONTROL % DYNAMIC #todo
    DAS     = CONTROL % DAS #todo
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    if (DYNAMIC == SEASINIT) { #IF (DYNAMIC == SEASINIT) THEN
      #-----------------------------------------------------------------------
      NVALP0 = 10000
      RSTAGE = 0
      
      for (I in 1:20) {
        PHZACC[I] = 0.0
        NVALPH[I] = NVALP0
        STGDOY[I] = 9999999
        PROG[I] = 0.
      }
      
      NVALPH[1]  = 1
      STGDOY[14] = YRSIM
      STGDOY[15] = YRPLT
      
      NVEG0  = NVALP0
      NVEG1  = NVALP0
      JPEND  = NVALP0
      NR0    = NVALP0
      NR1    = NVALP0
      NR2    = NVALP0
      NR3    = NVALP0
      NR5    = NVALP0
      NDLEAF = NVALP0
      NDVST  = NVALP0
      NDSET  = NVALP0
      NR7    = NVALP0
      YRNR1  = -99
      YRNR2  = -99
      YRNR3  = -99
      YRNR5  = -99
      YRNR7  = -99
      MDATE  = -99
      YREMRG = -99
      PHTEM = 0.0
      PROG  = 0.0
      # For P module:
      SeedFrac = 0.0
      VegFrac  = 0.0
      VegTime = PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHTHRS[8]
      
      #***********************************************************************
      #***********************************************************************
      #     Daily Integration
      #***********************************************************************
    } else if (DYNAMIC == INTEGR) {#ELSE IF (DYNAMIC .EQ. INTEGR) THEN
      #-----------------------------------------------------------------------
      for(J in 1:20){
        REM[J] = 1.0
      }
      
      if (YRDOY == YRPLT) {
        STGDOY[15] = YRPLT
      }
      
      #-----------------------------------------------------------------------
      #     Transplants
      #-----------------------------------------------------------------------
      if (PLME == "T" & YRPLT == YRDOY) {
        NVEG0 = DAS
        NVALPH[2] = NVEG0
        YREMRG    = YRDOY
        if (PHZACC[2] - PHTHRS[2] > -1.E-6) {
          NVEG1 = DAS
          NVALPH[3] = NVEG1
          PHZACC[3] = PHZACC[2] - PHTHRS[2]
          if (PHZACC[3] - PHTHRS[3] > -1.E-6) {
            JPEND = DAS
            NVALPH[4] = JPEND
            PHZACC[4] = PHZACC(3) - PHTHRS(3)
            if (PHZACC[4] - PHTHRS[4] > -1.E-6) {
              NR0 = DAS
              NVALPH[5] = NR0
              RSTAGE    = 0
              PHZACC[5] = PHZACC[4] - PHTHRS[4]
              if (PHZACC[5] - PHTHRS[5] > -1.E-6) {
                NR1 = DAS
                NVALPH[6] = NR1
                YRNR1     = YRDOY
                RSTAGE    = 1
                PHZACC[6] = PHZACC[5] - PHTHRS[5]
              }
            }
          }
        }
      }
      
      #-----------------------------------------------------------------------
      #     Check for emergence, if NVEG0 has been set to less than its
      #         initial value
      #-----------------------------------------------------------------------
      if (NVEG0 >= NVALP0) {
        PHTEM = PHTHRS[1] + SDEPTH * 0.6
        PROG[1] = FT[1] * FUDAY[1] * min(FSW[1],FNSTR[1],FPSTR[1])
        PHZACC[1] = PHZACC[1] + PROG[1]
        
        if ((PHZACC[1] - PHTEM) > -1.E-6 | (ISIMI == "E")) {
          
          #-----------------------------------------------------------------------
          #       Emergence, next stage, occurs on day DAS
          #-----------------------------------------------------------------------
          NVEG0 = DAS
          NVALPH[2] = NVEG0
          YREMRG    = YRDOY
          STGDOY[1] = YRDOY
          #-----------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          if (ISIMI != "E") {
            REM[2] = (PHZACC[1] - PHTEM)/(PROG[1] + 0.00001)
          }
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for veg stage, V1
      #-------------------------------------------------------------------------------
      #     Skip accumulator section if not time to start accumulating for phase 2
      #          also skips this accumulator on day when stage 2 occurred, with
      #          initial value of PHZACC(2) already computed
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[2])) & (NVEG1 >= NVALP0)) {
        #-------------------------------------------------------------------------------
        #     Skip section if stage 3 has already occurred
        #-------------------------------------------------------------------------------
        PROG[2] = FT[2] * FUDAY[2] * min(FSW[2],FNSTR[2],FPSTR[2]) * REM(NPRIOR[2])
        PHZACC[2] = PHZACC[2] + PROG[2]
        
        if (PHZACC[2] - PHTHRS[2] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       V1 occurs on day DAS
          #-------------------------------------------------------------------------------
          NVEG1 = DAS
          NVALPH[3] = NVEG1
          STGDOY[2] = YRDOY
          REM[3] = (PHZACC[2] - PHTHRS[2]) / (PROG[2] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for end of juvenile phase
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[3])) & (JPEND >= NVALP0)) {
        PROG[3] = FT[3] * FUDAY[3] * min(FSW[3],FNSTR[3],FPSTR[3]) * REM(NPRIOR[3])
        PHZACC[3] = PHZACC[3] + PROG[3]
        
        if (VegTime > 0) {
          VegFrac = PHZACC[3] / VegTime
        } else {
          VegFrac = 0.0
        }
        
        if(PHZACC[3] - PHTHRS[3] > -1.E-6) {
          
          #-------------------------------------------------------------------------------
          #       End of juvenile phase occurs on day DAS
          #-------------------------------------------------------------------------------
          JPEND = DAS
          NVALPH[4] = JPEND
          STGDOY[3] = YRDOY
          REM[4] = (PHZACC[3] - PHTHRS[3])/(PROG[3] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for floral induction, end of induction phase
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[4])) &. (NR0 >= NVALP0)) {
        PROG[4] = FT[4] * FUDAY[4] * min(FSW[4],FNSTR[4],FPSTR[4]) * REM(NPRIOR[4])
        PHZACC[4] = PHZACC[4] + PROG[4]
        VegFrac = (PHTHRS[3] + PHZACC[4]) / VegTime
        
        if(PHZACC[4] - PHTHRS[4] > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Floral induction occurs on day DAS, end of phase 4
          #-------------------------------------------------------------------------------
          NR0 = DAS
          NVALPH(5) = NR0
          RSTAGE    = 0
          STGDOY[4] = YRDOY
          REM[5] = (PHZACC[4] - PHTHRS[4])/(PROG[4] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for first flower, stage 6, end of phase 5
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[5])) & (NR1 >= NVALP0)) {
        PROG[5] = FT[5] * FUDAY[5] * min(FSW[5],FNSTR[5],FPSTR[5]) * REM(NPRIOR[5])
        PHZACC[5] = PHZACC[5] + PROG[5]
        VegFrac = (PHTHRS[3] + PHTHRS[4] + PHZACC[5]) / VegTime
        
        if(PHZACC[5] - PHTHRS[5] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       First flower occurs on day DAS
          #-------------------------------------------------------------------------------
          NR1 = DAS
          STGDOY[5] = YRDOY
          NVALPH[6] = NR1
          YRNR1     = YRDOY
          RSTAGE    = 1
          REM[6] = (PHZACC[5] - PHTHRS[5])/(PROG[5] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for beginning ovule (peg), stage 7, end of phase 6
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[6])) & (NR2 >= NVALP0)) {
        PROG[6] = FT[6] * FUDAY[6] * min(FSW[6],FNSTR[6],FPSTR[6]) * REM(NPRIOR[6])
        PHZACC[6] = PHZACC[6] + PROG[6]
        
        if(PHZACC[6] - PHTHRS[6] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       First peg occurs on day DAS
          #-------------------------------------------------------------------------------
          NR2 = DAS
          STGDOY[6] = YRDOY
          NVALPH[7] = NR2
          YRNR2     = YRDOY
          RSTAGE    = 2
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[7] = (PHZACC[6] - PHTHRS[6])/(PROG[6] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage beginning shell, stage 8, end of phase 7
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[7])) & (NR3 >= NVALP0)) {
        PROG[7] = FT[7] * FUDAY[7] * min(FSW[7],FNSTR[7],FPSTR[7]) * REM(NPRIOR[7])
        PHZACC[7] = PHZACC[7] + PROG[7]
        
        if(PHZACC[7] - PHTHRS[7] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage R3 occurs on day DAS
          #-------------------------------------------------------------------------------
          NR3 = DAS
          STGDOY[7] = YRDOY
          if (STGDOY[7] == STGDOY[6]) STGDOY[7] = 9999999
          NVALPH[8] = NR3
          YRNR3     = YRDOY
          RSTAGE    = 3
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[8] = (PHZACC[7] - PHTHRS[7])/(PROG[7] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage beginning seed (R5), stage 9, end of phase 8
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[8])) & (NR5 >= NVALP0)) {
        PROG[8] = FT[8] * FUDAY[8] * min(FSW[8],FNSTR[8],FPSTR[8]) * REM(NPRIOR[8])
        PHZACC[8] = PHZACC[8] + PROG[8]
        VegFrac =(PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHZACC[8])/VegTime
        
        if(PHZACC[8] - PHTHRS[8] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage R5 occurs on day DAS
          #-------------------------------------------------------------------------------
          NR5 = DAS
          STGDOY[8] = YRDOY
          NVALPH[9] = NR5
          YRNR5     = YRDOY
          RSTAGE    = 5
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[9] = (PHZACC[8] - PHTHRS[8])/(PROG[8] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage NDSET, stage 10, end of phase 9
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[9])) & (NDSET >= NVALP0)) {
        PROG[9] = FT[9] * FUDAY[9] * max(FSW[9],FNSTR[9],FPSTR[9]) * REM(NPRIOR[9]) #MAX ao inves de MIN
        PHZACC[9] = PHZACC[9] + PROG[9]
        
        if(PHZACC[9] - PHTHRS[9] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NDSET occurs on day DAS
          #-------------------------------------------------------------------------------
          NDSET = DAS
          STGDOY[9] = YRDOY
          NVALPH[10] = NDSET
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[10] = (PHZACC[9] - PHTHRS[9])/(PROG[9] + 0.00001)
        }
      }
      #-------------------------------------------------------------------------------
      #     Check for stage NR7, stage 11, end of phase 10
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[10])) & (NR7 >= NVALP0)) {
        PROG[10] = FT[10] * FUDAY[10]*max(FSW[10],FNSTR[10],FPSTR[10]) * REM(NPRIOR[10]) #MAX ao inves de MIN
        PHZACC[10] = PHZACC[10] + PROG[10]
        SeedFrac = PHZACC[10] / PHTHRS[10]
        
        if(PHZACC[10] - PHTHRS[10] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NR7, physiological maturity, occurs on day DAS
          #-------------------------------------------------------------------------------
          NR7 = DAS
          STGDOY[10] = YRDOY
          NVALPH[11] = NR7
          YRNR7      = YRDOY
          RSTAGE     = 7
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[11] = (PHZACC[10] - PHTHRS[10])/(PROG[10] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NR8, stage 12, end of phase 11
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[11])) & (MDATE <= YRSIM)) {
        PROG[11] = FT[11] * FUDAY[11]*min(FSW[11],FNSTR[11],FPSTR[11]) * REM(NPRIOR[11])
        PHZACC[11] = PHZACC[11] + PROG[11]
        
        if(PHZACC[11] - PHTHRS[11] > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NR8, harvest maturity, occurs on day DAS
          #-------------------------------------------------------------------------------
          STGDOY[11] = YRDOY
          NVALPH[12] = DAS
          MDATE      = YRDOY
          RSTAGE     = 8
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[12] = (PHZACC[11] - PHTHRS[11])/(PROG[11] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[12])) & (NDVST >= NVALP0)) {
        PROG[12] = FT[12] * FUDAY[12]*min(FSW[12],FNSTR[12],FPSTR[12]) * REM(NPRIOR[12])
        PHZACC[12] = PHZACC[12] + PROG[12]
        
        if(PHZACC[12] - PHTHRS[12] > 1.E-6) {
          #-------------------------------------------------------------------------------
          #       Stage NDVST, end of V-stage addition, occurs on day DAS
          #-------------------------------------------------------------------------------
          NDVST = DAS
          STGDOY[12] = YRDOY
          NVALPH[13] = NDVST
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[13] = (PHZACC[12] - PHTHRS[12])/(PROG[12] + 0.00001)
        }
      }
      
      #-------------------------------------------------------------------------------
      #     Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
      #-------------------------------------------------------------------------------
      if ((DAS >= NVALPH(NPRIOR[13])) & (NDLEAF >= NVALP0)) {
        PROG[13] = FT[13] * FUDAY[13]*min(FSW[13],FNSTR[13],FPSTR[13]) * REM(NPRIOR[13])
        PHZACC[13] = PHZACC[13] + PROG[13]
        
        if(PHZACC[13] - PHTHRS[13] > -1.E-6) {
          #-------------------------------------------------------------------------------
          #      Stage NDLEAF, end of leaf growth, occurs on day DAS
          #-------------------------------------------------------------------------------
          NDLEAF = DAS
          STGDOY[13] = YRDOY
          NVALPH[14] = NDLEAF
          #-------------------------------------------------------------------------------
          #       Account for the part of today that contributes to the next phase(s)
          #-------------------------------------------------------------------------------
          REM[14] = (PHZACC[13] - PHTHRS[13])/(PROG[13] + 0.00001)
        }
      }
      
      #************************************************************************
      #************************************************************************
      #     End of DYNAMIC IF construct
      #************************************************************************
    }
    #************************************************************************
    #R  RETURN
    # todo
    #R END SUBROUTINE RSTAGES
  }
  
} # para facilitar a programacao

#### Subrotina: IPPHENOL ####
{
  
  #************************************************************************
  #************************************************************************
  
  IPPHENOL(CONTROL,
           ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, CROP,    # Output
           CTMP, DLTYP, EVMODC, NPRIOR, NSENP, OPTBI,      # Output
           PHTHRS, PLME, PSENP, SDAGE, SDEPTH, SLOBI,      # Output
           THVAR, TRIFOL, TSELC, TB, TO1, TO2, TM, WSENP) {# Output
    
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #to do
    #R CHARACTER*1   PLME, BLANK
    #R CHARACTER*2   CROP
    #R CHARACTER*3   CTMP(20), DLTYP(20)
    #R CHARACTER*6   SECTION, ECOTYP, ECONO, ERRKEY
    #R CHARACTER*12  FILEC, FILEE
    #R CHARACTER*16  ECONAM
    #R CHARACTER*30  FILEIO
    #R CHARACTER*80  CHAR, PATHCR, PATHEC
    #R CHARACTER*92  FILECC, FILEGC
    #R CHARACTER*255 C255
    #R 
    #R INTEGER LUNIO, NPHS
    #R INTEGER LUNCRP, LUNECO, ISECT, PATHL
    #R INTEGER I, J, K
    #R INTEGER IVRGRP, IVRTEM, ERR, LINC, LNUM, FOUND
    #R INTEGER NPRIOR(20), TSELC(20)
    #R 
    #R PARAMETER (BLANK = ' ')
    #R PARAMETER (ERRKEY = 'IPPHEN')
    #R PARAMETER (NPHS = 13)
    #R 
    #R REAL ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, EVMODC
    #R REAL OPTBI
    #R REAL PPSEN, PH2T5, R1PPO, PM06, PM09
    #R REAL SDEPTH, SDAGE, SLOBI, THVAR, TRIFOL
    #R REAL TB(5), TO1(5), TO2(5), TM(5)
    #R REAL WSENP(20), NSENP(20)
    #R REAL PHTHRS(20), PSENP(20)
    
    #R !-----------------------------------------------------------------------
    #R !     Define constructed variable types based on definitions in
    #R !     ModuleDefs.for.
    
    # The variable "CONTROL" is of type "ControlType".
    TYPE (ControlType) CONTROL
    
    # Transfer values from constructed data types into local variables.
    FILEIO  = CONTROL % FILEIO
    LUNIO   = CONTROL % LUNIO
    
    #-----------------------------------------------------------------------
    #     Read in values from temporary file, which were previously input
    #       in Subroutine IPIBS.
    #-----------------------------------------------------------------------
    OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
    if (ERR != 0) {
      print *, "!!! ERROR !!!", ERRKEY,ERR,FILEIO,0
      *      CALL ERROR(ERRKEY,ERR,FILEIO,0)
      
      READ (LUNIO,100,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
      100   FORMAT(//////,15X,A12,1X,A80)
      
      if (ERR != 0) {
        print *, "!!! ERROR !!!!",ERRKEY,ERR,FILEIO,LNUM
        *      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        
        READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
        105   FORMAT(15X,A12,1X,A80)
        
        if (ERR != 0) { print *, "!!! ERROR !!!!",ERRKEY,ERR,FILEIO,LNUM
          *      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          
          #-----------------------------------------------------------------------
          #     Subroutine FIND finds appropriate SECTION in a file by
          #     searching for the specified 6-character string at beginning
          #     of each line.
          #-----------------------------------------------------------------------
          #  SECTION = '*SIMUL'
          #  CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          #  IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
          #  READ(LUNIO,'(31X,A1)',IOSTAT=ERR) ISIMI; LNUM = LNUM + 1
          #  IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          
          #-----------------------------------------------------------------------
          #     Find and read Cultivar Section
          #-----------------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          if (FOUND == 0) {
            print *, "!!! ERROR !!!!",SECTION, 42, FILEIO, LNUM
            *      CALL ERROR (SECTION, 42, FILEIO,LNUM)
            READ(LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
            
            if (ERR .NE. 0) {
              print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
              *      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
              
              #-----------------------------------------------------------------------
              if (CROP != "FA") {
                #-----------------------------------------------------------------------
                #     Find and Read Planting Details Section
                #-----------------------------------------------------------------------
                SECTION = '*PLANT'
                CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
                if (FOUND == 0) {
                  print *, "!!! ERROR !!!",SECTION, 42, FILEIO, LNUM
                  *        CALL ERROR (SECTION, 42, FILEIO,LNUM)
                  READ(LUNIO,140,IOSTAT=ERR) PLME, SDEPTH, SDAGE, ATEMP
                  140     FORMAT(35X,A1,19X,F5.1,6X,2(1X,F5.0))
                  LNUM = LNUM + 1
                  
                  if (ERR != 0) {
                    print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
                    *        CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
                    
                    #-----------------------------------------------------------------------
                    #     Find and read Cultivar Section
                    #-----------------------------------------------------------------------
                    SECTION = '*CULTI'
                    CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
                    if (FOUND == 0) {
                      print *, "!!! ERROR !!!",SECTION, 42, FILEIO, LNUM
                      *        CALL ERROR (SECTION, 42, FILEIO,LNUM)
                      READ(LUNIO,165,IOSTAT=ERR) ECONO, CSDVAR, PPSEN, PH2T5,
                      &              PHTHRS(6), PHTHRS(8), PHTHRS(10), PHTHRS(13)
                      165     FORMAT(24X,A6,7F6.0)
                      LNUM = LNUM + 1
                      if (ERR != 0) {
                        print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
                        *        CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
                      }
                      
                      CLOSE (LUNIO)
                      
                      #-----------------------------------------------------------------------
                      #     Open FILEC
                      #-----------------------------------------------------------------------
                      if (CROP != "FA") {
                        PATHL  = INDEX(PATHCR,BLANK)
                        if (PATHL <= 1) {
                          FILECC = FILEC
                        } else {
                          FILECC = PATHCR(1:(PATHL-1)) // FILEC
                        }
                        
                        CALL GETLUN('FILEC', LUNCRP)
                        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
                        if (ERR != 0) {
                          print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,0
                          *        CALL ERROR(ERRKEY,ERR,FILECC,0)
                          LNUM = 0
                          #-----------------------------------------------------------------------
                          #     Find Leaf Growth Parameters from FILEC and read EVMODC value
                          #-----------------------------------------------------------------------
                          SECTION = '!*LEAF'
                          CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
                          if (FOUND == 0) {
                            print *, "!!! ERROR !!!",SECTION, 42, FILECC, LNUM
                            *        CALL ERROR (SECTION, 42, FILECC,LNUM)
                            CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
                            READ(CHAR,'(24X,F6.1)',IOSTAT=ERR) EVMODC
                            
                            if (ERR != 0) {
                              print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
                              
                              #-----------------------------------------------------------------------
                              #     Find Phenology Section in FILEC and read
                              #-----------------------------------------------------------------------
                              SECTION = '!*PHEN'
                              CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
                              if (FOUND == 0) {
                                print *, "!!! ERROR !!!",SECTION, 42, FILECC, LNUM
                                *        CALL ERROR (SECTION, 42, FILECC,LNUM)
                                CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
                                READ(CHAR,250,IOSTAT=ERR) TB(1), TO1(1), TO2(1), TM(1)
                                250     FORMAT(13F6.1)
                                
                                if (ERR != 0) {
                                  print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
                                  *        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
                                  
                                  CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
                                  READ(CHAR,250,IOSTAT=ERR) TB(2), TO1(2), TO2(2), TM(2)
                                  if (ERR != 0) {
                                    print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
                                    *        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
                                    
                                    CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
                                    READ(CHAR,250,IOSTAT=ERR) TB(3), TO1(3), TO2(3), TM(3)
                                    
                                    if (ERR != 0) {
                                      print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
                                      *        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
                                      
                                      for (I in 1:NPHS) {
                                        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
                                        READ(CHAR,270,IOSTAT=ERR) J, NPRIOR(J), DLTYP(J), CTMP(J), TSELC(J), WSENP(J), NSENP(J), PSENP(J)
                                        270       FORMAT(I3,I3,2(2X,A3),1X,I2,3(1X,F5.2))
                                        if (ERR != 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC, LNUM
                                        *          CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
                                      }
                                      #todo: checar 'J' v 'I'
                                      CLOSE (LUNCRP)
                                      
                                      #-----------------------------------------------------------------------
                                      #     Open FILEE
                                      #-----------------------------------------------------------------------
                                      LNUM = 0
                                      PATHL  = INDEX(PATHEC,BLANK)
                                      if (PATHL <= 1) {
                                        FILEGC = FILEE
                                      } else {
                                        FILEGC = PATHEC(1:(PATHL-1)) // FILEE #todo: checar //
                                      }
                                      
                                      #-----------------------------------------------------------------------
                                      #    Read Ecotype Parameter File
                                      #-----------------------------------------------------------------------
                                      CALL GETLUN('FILEE', LUNECO)
                                      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
                                      if (ERR != 0) {
                                        print *, "!!! ERROR !!!",ERRKEY,ERR,FILEGC,0
                                        *        CALL ERROR(ERRKEY,ERR,FILEGC,0)
                                        ECOTYP = '      '
                                        LNUM = 0
                                        
                                        DO WHILE (ECOTYP != ECONO) #todo: DO WHILE é igual ao while?
                                        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
                                        if (ISECT == 1 .AND. C255(1:1) != ' ' & C255(1:1) != '*') {
                                          READ (C255,3100,IOSTAT=ERR) ECOTYP, ECONAM, IVRGRP, IVRTEM, THVAR, (PHTHRS(K), K=1,4), PM06, PM09,
                                          (PHTHRS(K),K=11,12), TRIFOL, R1PPO, OPTBI, SLOBI
                                          3100        FORMAT (A6, 1X, A16, 1X, 2(1X,I2), 7(1X,F5.0), 6X,
                                                              &          3(1X,F5.0), 2(6X), 3(1X,F5.0))
                                          if (ERR != 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEGC, LNUM
                                          *            CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
                                          if (ECOTYP == ECONO) {
                                            EXIT #todo: break?
                                          }
                                          
                                        } else if (ISECT == 0) {
                                          if (ECONO == 'DFAULT') print *, "!!! ERROR !!!",ERRKEY,35, FILEGC,LNUM
                                          *            CALL ERROR(ERRKEY,35,FILEGC,LNUM)
                                          ECONO = 'DFAULT'
                                          REWIND(LUNECO)
                                          LNUM = 0
                                        }
                                      }
                                      
                                      CLOSE (LUNECO)
                                      
                                      PHTHRS(5) = MAX(0.,PH2T5 - PHTHRS(3) - PHTHRS(4))
                                      PHTHRS(7) = PHTHRS(6) + MAX(0.,(PHTHRS(8) - PHTHRS(6))* PM06)
                                      PHTHRS(9) = MAX(0.,PHTHRS(10) * PM09)
                                      
                                      if (PPSEN >== 0.0) {
                                        CLDVAR = CSDVAR + (1.-THVAR)/max(PPSEN,0.000001)
                                      } else if (PPSEN .LT. 0.0) {
                                        CLDVAR = CSDVAR + (1.-THVAR)/min(PPSEN,-0.000001)
                                      }
                                      
                                      CSDVRR = CSDVAR - R1PPO
                                      CLDVRR = CLDVAR - R1PPO
                                      
                                    }
                                    
                                    
                                    
                                    # RETURN
                                    #R END  SUBROUTINE IPPHENOL
                                  }
                                  #************************************************************************
                                  #************************************************************************
                                  
                                } # para facilitar a programacao