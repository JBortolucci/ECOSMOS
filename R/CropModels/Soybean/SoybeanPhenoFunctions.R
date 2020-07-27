# Contem as subrotinas: (aqui como 'function's)
  # 1) VSTAGES (VSTAGES.txt com a programacao em fortran) para os estagios vegetativos
  # 2) RSTAGES (RSTAGES.txt com a programacao em fortran) para os estagios reprodutivos

# os comentarios com 'R' na frente são para controle e serão removidos posteriormente
 # 'C' ou '!' sao comentarios do codigo original em fortran, portanto, nao usados pelo CROPGRO
# checar 'ifelse's and 'inputs'

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
  # ELSE IF    meaning ifelse ???

#### Subrotina: VSTAGES ####
{
  
#R C=======================================================================
#R #R C  VSTAGES, Subroutine
#R #R C  Calculates V-stages
#R C-----------------------------------------------------------------------
#R #R C  REVISION HISTORY
#R #R C  07/24/98 CHP Pulled code from PHENOL subroutine
#R C-----------------------------------------------------------------------
#R !     Called from:    PHENOL
#R !     Calls:          None
#R C=======================================================================
  
  #SUBROUTINE VSTAGES(
  #  &    DAS, DTX, EVMODC, MNEMV1, NDVST,                #R !Input
  #  &    NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             #R !Input
  #  &    TURFAC, XPOD, YRDOY, YRPLT,                     #R !Input
  #  &    RVSTGE, VSTAGE,                                 #R !Output
  #  &    DYNAMIC)                                        #R !Control


VSTAGES <- function (DAS, DTX, EVMODC, MNEMV1, NDVST,                #R !Input
                     NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             #R !Input
                     TURFAC, XPOD, YRDOY, YRPLT,                     #R !Input
                     RVSTGE, VSTAGE,                                 #R !Output
                     DYNAMIC){                                       #R !Control

#R !-----------------------------------------------------------------------
#R  USE ModuleDefs     #R !Definitions of constructed variable types,
#R ! which contain control information, soil
#R ! parameters, hourly weather data.
#R IMPLICIT NONE
#R SAVE

#R CHARACTER*1 PLME
#R INTEGER DYNAMIC
#R INTEGER DAS, NVEG0, NVEG1, NDVST
#R INTEGER YRPLT, YRDOY
#R REAL VSTAGE, RVSTGE, VSTGED, VSTAGP
#R REAL MNEMV1, TRIFOL, EVMODC, EVMOD, DTX
#R REAL TURFAC, XPOD
#R REAL PHZACC(20)

PHZACC <- rep(0,20)
                       
#R !***********************************************************************
#R !***********************************************************************
#R !     Seasonal initialization - run once per season
#R !***********************************************************************
#  IF (DYNAMIC .EQ. SEASINIT) THEN
if (DYNAMIC == SEASINIT) { # todo ?? DAS == 1 ??

#R !-----------------------------------------------------------------------
  VSTAGE = 0.0
  RVSTGE = 0.0
  VSTGED = 0.0
  VSTAGP = 0.0
  RVSTGE = 0.0

#R !***********************************************************************
#R !***********************************************************************
#R C     Daily Rate Calculations
#R !***********************************************************************
#R  ELSEIF (DYNAMIC .EQ. RATE) THEN
#R !-----------------------------------------------------------------------
#R !    Calculate rate of V-stage change for height and width determination
#R !-----------------------------------------------------------------------
#R  RVSTGE = 0.

#R IF (DAS .GE. NVEG0 .AND. DAS .LE. NDVST + ANINT(VSTGED)) THEN
#R IF (DAS .GT. NDVST .AND. VSTGED .GT. 0.001) THEN
#R RVSTGE = 1. / VSTGED
#R ELSE
#R RVSTGE = VSTAGE - VSTAGP
#R ENDIF
#R ENDIF

  ifelse(DYNAMIC == RATE, # todo ?? ELSE IF (fortran) = ifelse (R) ??
         RVSTGE = 0.,
         if (DAS > DAS > NVEG0 & DAS <= NDVST + ANINT[VSTGED]) {
           if (DAS > NDVST & VSTGED > 0.001) {
             RVSTGE = 1. / VSTGED } else {
               RVSTGE = VSTAGE - VSTAGP
             }
           } )
  
#R !***********************************************************************
#R !***********************************************************************
#R !     Daily Integration
#R !***********************************************************************
#R  ELSE IF (DYNAMIC .EQ. INTEGR) THEN
#R !-----------------------------------------------------------------------
#R C     V-STAGE between emergence and unifoliate (V-1) is determined
#R C     by the physiological accumulator and the minimum number of
#R C     days between emergence and V-1 under optimum temperature (MNEMV1)
#R C     V-STAGE after V-1 is determined by the leaf appearance rate
#R C     (TRIFOL), a water stress factor (TURFAC) and physiological units
#R C     for today (DTX).
#R C-----------------------------------------------------------------------
#R IF (RVSTGE .GT. 1.E-6) THEN
#R VSTGED = 1. / RVSTGE
#R ELSE
#R VSTGED = 0.0
#R ENDIF
#R VSTAGP = VSTAGE
  ifelse(DYNAMIC == INTEGR, # ?? ELSE IF (fortran) = ifelse (R) ??
         
         if (RVSTGE > 1.E-6) {
           VSTGED = 1. / RVSTGE
           } else {
             VSTGED = 0.0
             }
         
         VSTAGP = VSTAGE,
  
#R !-----------------------------------------------------------------------
#R !     V-Stage for transplants
#R !-----------------------------------------------------------------------
#E IF (PLME .EQ. 'T' .AND. YRPLT .EQ. YRDOY) THEN
#E VSTAGE = 1. + (PHZACC(2) - MNEMV1) * TRIFOL
#E ENDIF
          if (PLME == "T" & YRPLT == YRDOY) {
            VSTAGE = 1. + (PHZACC[2] - MNEMV1) * TRIFOL
          }

#R !-----------------------------------------------------------------------
#R IF (DAS .GE. NVEG0 .AND. DAS .LE. NDVST) THEN
#R IF (DAS .LT. NVEG1) THEN
#R VSTAGE  = PHZACC(2)/MNEMV1
#R ELSE
#R IF (VSTAGE .LT. ABS(EVMODC) .AND.
#R     &        ABS(EVMODC) .GT. 0.0001) THEN
#R EVMOD = 1.0 + (ABS(EVMODC)- VSTAGE) / EVMODC
#R EVMOD = AMIN1(2.0,EVMOD)
#R EVMOD = AMAX1(0.0,EVMOD)
#R ELSE
#R EVMOD = 1.0
#R ENDIF
#R VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD*TURFAC*(1.0-XPOD)
#R ENDIF
#R ENDIF

          if (DAS >= NVEG0 & DAS <= NDVST) {
            if (DAS < NVEG1) {
              VSTAGE  = PHZACC[2]/MNEMV1
              } else {
                EVMOD = 1.0 + (abs(EVMODC)- VSTAGE) / EVMODC
                EVMOD = AMIN1(2.0,EVMOD) #todo check AMIN1
                EVMOD = AMAX1(0.0,EVMOD) #todo check AMAX1
                } else {
                  EVMOD = 1.0
                }
            }
            VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD*TURFAC*(1.0-XPOD) )

#R !***********************************************************************
#R !***********************************************************************
#R !     End of DYNAMIC IF construct
#R !***********************************************************************
#R  END IF
  }
#R !***********************************************************************
#R  RETURN
  #todo return, or assign?
}
#R END SUBROUTINE VSTAGES
} # para facilitar a programacao


#### Subrotina: RSTAGES ####
{
  
#R !************************************************************************
#R !************************************************************************
  
  #SUBROUTINE RSTAGES(CONTROL,
  #                   &    FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    #R !Input
  #                   &    PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      #R !Input
  #                   &    JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     #R !Output
  #                   &    NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       #R !Output
  #                   &    RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      #R !Output
  #                   &    YRNR1, YRNR2, YRNR3, YRNR5, YRNR7)              #R !Output

RSTAGES <- function (CONTROL,
                     FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    #R !Input
                     PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      #R !Input
                     JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     #R !Output
                     NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       #R !Output
                     RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      #R !Output
                     YRNR1, YRNR2, YRNR3, YRNR5, YRNR7){              #R !Output
  
#R !-----------------------------------------------------------------------
  #R USE ModuleDefs     #R !Definitions of constructed variable types,
#R ! which contain control information, soil
#R ! parameters, hourly weather data.
#R IMPLICIT NONE
#R SAVE

#R CHARACTER*1 ISIMI, PLME
#R 
#R INTEGER DYNAMIC
#R INTEGER I, J, NVALP0, DAS, YRDOY, YRPLT, YRSIM
#R INTEGER NDLEAF,  NDSET, NDVST, JPEND  #R !, TIMDIF
#R INTEGER RSTAGE,  NVEG0, NVEG1, NR0, NR1, NR2, NR3, NR5, NR7
#R INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE, YREMRG
#R INTEGER NPRIOR(20), STGDOY(20), NVALPH(20)
#R 
#R REAL PHTEM, SDEPTH
#R REAL FT(20), FUDAY(20), FSW(20), FNSTR(20), FPSTR(20), PHTHRS(20)
#R REAL PHZACC(20), PROG(20), REM(20)
#R REAL SeedFrac, VegFrac, VegTime

  NPRIOR <- rep(0,20); STGDOY <- rep(0,20); NVALPH(20) <- rep(0,20)
  FT <- rep(0,20); FUDAY <- rep(0,20); FSW <- rep(0,20); FNSTR <- rep(0,20); FPSTR <- rep(0,20); PHTHRS(20) <- rep(0,20)
  PHZACC <- rep(0,20); PROG <- rep(0,20); REM <- rep(0,20)

#R !     Note: Writing RSTAGES.OUT eliminates the debug vs. release problem
#R !       for chickpea.  Will have to investigate why#R !#R !#R ! chp 05/13/2004
#R !     Follow-up -- 01/24/2007 CHP revised statements that compared real #'s
#R !       OLD --> IF (PHZACC(3) .GE. PHTHRS(3)) THEN
#R !       NEW --> IF (PHZACC(3) - PHTHRS(3) > -1.E-6) THEN
#R !     This seems to have eliminated the debug-release problem in chickpea
#R !       and possibly other crops.

#R !     For output file:
  #R !      CHARACTER*11 OUTRSTG
#R !      CHARACTER*30 FILEIO
#R !      INTEGER DAP, DOY, ERRNUM, LUN, RUN, YEAR
#R !      LOGICAL FEXIST, FOPEN

TYPE (ControlType) CONTROL #todo
DYNAMIC = CONTROL % DYNAMIC #todo
DAS     = CONTROL % DAS #todo
#R !      FILEIO  = CONTROL % FILEIO
#R !      RUN     = CONTROL % RUN

#R !***********************************************************************
  #R !***********************************************************************
  #R !     Seasonal initialization - run once per season
#R !***********************************************************************
  #IF (DYNAMIC == SEASINIT) THEN
  if (DYNAMIC == SEASINIT) { #todo
#R !-----------------------------------------------------------------------
    NVALP0 = 10000
    RSTAGE = 0
    
    #DO I = 1,20
    for (I in 1:20) {
    PHZACC[I] = 0.0
    NVALPH[I] = NVALP0
    STGDOY[I] = 9999999
    PROG[I] = 0.
    #ENDDO
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
    #R !      NR8    = NVALP0
    YRNR1  = -99
    YRNR2  = -99
    YRNR3  = -99
    YRNR5  = -99
    YRNR7  = -99
    MDATE  = -99
    YREMRG = -99
    
    PHTEM = 0.0
    PROG  = 0.0
    
    #R !     For P module:
    SeedFrac = 0.0
    VegFrac  = 0.0
    VegTime = PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHTHRS[8]

#R !     Output file:
#R !      OUTRSTG = 'Rstages.OUT'
#R !      CALL GETLUN('OUTRSTG', LUN)
#R !
#R !      INQUIRE (FILE = OUTRSTG, EXIST = FEXIST)
#R !      INQUIRE (FILE = OUTRSTG, OPENED = FOPEN)
#R !      IF (FEXIST) THEN
#R !        IF (.NOT. FOPEN) THEN
#R !          OPEN (UNIT = LUN, FILE = OUTRSTG, STATUS = 'OLD',
                 #R !     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
#R !        ELSE
#R !          INQUIRE (FILE = OUTRSTG, NUMBER = LUN)
#R !        ENDIF
#R !      ELSE
#R !        OPEN (UNIT = LUN, FILE = OUTRSTG, STATUS = 'NEW',
               #R !     &    IOSTAT = ERRNUM)
#R !        WRITE(LUN,'("*RSTAGES OUTPUT FILE")')
#R !      ENDIF
#R !
#R !      #R !For sequenced run, use replicate
#R !      #R ! number instead of run number in header.
#R !      CALL HEADER(SEASINIT, LUN, RUN)
#R !
  #R !      WRITE (LUN,120)
#R !  120 FORMAT('@YEAR DOY   DAS   DAP  RSTG  PHTEM',
              #R !     &      '   PHAC1   PHAC2   PHAC3   PHAC4   PHAC5',
              #R !     &      '   PHAC6   PHAC7   PHAC8   PHAC9  PHAC10',
              #R !     &      '  PHAC11  PHAC12  PHAC13  PHAC14  PHAC15',
              #R !     &      '  PHAC16  PHAC17  PHAC18  PHAC19  PHAC20')

#R C***********************************************************************
#R C***********************************************************************
#R C     Daily Integration
#R C***********************************************************************
  #R ELSE IF (DYNAMIC .EQ. INTEGR) THEN
  ifelse(DYNAMIC == INTEGR,
#R !-----------------------------------------------------------------------
#R !     DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
#R DO  J = 1,20
#R REM(J) = 1.0
#R ENDDO
  for(J in 1:20){
    REM[J] = 1.0
  } ,

#IF (YRDOY .EQ. YRPLT) THEN
#STGDOY(15) = YRPLT
#ENDIF
  if (YRDOY == YRPLT) {
    STGDOY[15] = YRPLT
  } )


#R C-----------------------------------------------------------------------
#R C     Transplants
#R C-----------------------------------------------------------------------
#R IF (PLME .EQ. 'T' .AND. YRPLT .EQ. YRDOY) THEN
#R NVEG0 = DAS
#R NVALPH(2) = NVEG0
#R YREMRG    = YRDOY
#R #R !        IF (PHZACC(2) .GE. PHTHRS(2)) THEN
#R IF (PHZACC(2) - PHTHRS(2) > -1.E-6) THEN
#R NVEG1 = DAS
#R NVALPH(3) = NVEG1
#R PHZACC(3) = PHZACC(2) - PHTHRS(2)
#R #R !          IF (PHZACC(3) .GE. PHTHRS(3)) THEN
#R IF (PHZACC(3) - PHTHRS(3) > -1.E-6) THEN
#R JPEND = DAS
#R NVALPH(4) = JPEND
#R PHZACC(4) = PHZACC(3) - PHTHRS(3)
#R #R !            IF (PHZACC(4) .GE. PHTHRS(4)) THEN
#R IF (PHZACC(4) - PHTHRS(4) > -1.E-6) THEN
#R NR0 = DAS
#R NVALPH(5) = NR0
#R RSTAGE    = 0
#R PHZACC(5) = PHZACC(4) - PHTHRS(4)
#R #R !              IF (PHZACC(5) .GE. PHTHRS(5)) THEN
#R IF (PHZACC(5) - PHTHRS(5) > -1.E-6) THEN
#R NR1 = DAS
#R NVALPH(6) = NR1
#R YRNR1     = YRDOY
#R RSTAGE    = 1
#R PHZACC(6) = PHZACC(5) - PHTHRS(5)
#R ENDIF
#R ENDIF
#R ENDIF
#R ENDIF
#R ENDIF
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

#R C-----------------------------------------------------------------------
#R C     Check for emergence, if NVEG0 has been set to less than its
#R !         initial value
#R C-----------------------------------------------------------------------
#R IF (NVEG0 .GE. NVALP0) THEN
#R PHTEM = PHTHRS(1) + SDEPTH * 0.6
#R PROG(1) = FT(1) * FUDAY(1) * MIN(FSW(1),FNSTR(1),FPSTR(1))
#R PHZACC(1) = PHZACC(1) + PROG(1)
##R !        IF ((PHZACC(1) .GE. PHTEM) .OR. (ISIMI .EQ. 'E')) THEN
#IF ((PHZACC(1) - PHTEM) > -1.E-6 .OR. (ISIMI .EQ. 'E')) THEN
#
##R C-----------------------------------------------------------------------
##R C       Emergence, next stage, occurs on day DAS
##R C-----------------------------------------------------------------------
#NVEG0 = DAS
#NVALPH(2) = NVEG0
#YREMRG    = YRDOY
#STGDOY(1) = YRDOY
##R C-----------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#IF (ISIMI .NE. 'E') THEN
#REM(2) = (PHZACC(1) - PHTEM)/(PROG(1) + 0.00001)
#ENDIF
#ENDIF
#ENDIF
  if (NVEG0 >= NVALP0) {
    PHTEM = PHTHRS[1] + SDEPTH * 0.6
    PROG[1] = FT[1] * FUDAY[1] * min(FSW[1],FNSTR[1],FPSTR[1])
    PHZACC[1] = PHZACC[1] + PROG[1]
    
  if ((PHZACC(1) - PHTEM) > -1.E-6 | (ISIMI == "E")) {
#R C-----------------------------------------------------------------------
#R C       Emergence, next stage, occurs on day DAS
#R C-----------------------------------------------------------------------
    NVEG0 = DAS
    NVALPH[2] = NVEG0
    YREMRG    = YRDOY
    STGDOY[1] = YRDOY
#R C-----------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    if (ISIMI != "E") {
      REM[2] = (PHZACC[1] - PHTEM)/(PROG[1] + 0.00001)
    }
  }
}

##R C-------------------------------------------------------------------------------
##R C     Check for veg stage, V1
##R C-------------------------------------------------------------------------------
##R C     Skip accumulator section if not time to start accumulating for phase 2
##R C          also skips this accumulator on day when stage 2 occurred, with
##R C          initial value of PHZACC(2) already computed
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(2))) .AND. (NVEG1 .GE. NVALP0)) THEN
##R C-------------------------------------------------------------------------------
##R  C     Skip section if stage 3 has already occurred
##R C-------------------------------------------------------------------------------
#PROG(2) = FT(2) * FUDAY(2) * MIN(FSW(2),FNSTR(2),FPSTR(2))
#&      * REM(NPRIOR(2))
#PHZACC(2) = PHZACC(2) + PROG(2)
##R !        IF (PHZACC(2) .GE. PHTHRS(2)) THEN
#IF (PHZACC(2) - PHTHRS(2) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       V1 occurs on day DAS
##R C-------------------------------------------------------------------------------
#NVEG1 = DAS
#NVALPH(3) = NVEG1
#STGDOY(2) = YRDOY
#REM(3) = (PHZACC(2) - PHTHRS(2)) / (PROG(2) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for veg stage, V1
#R C-------------------------------------------------------------------------------
#R C     Skip accumulator section if not time to start accumulating for phase 2
#R C          also skips this accumulator on day when stage 2 occurred, with
#R C          initial value of PHZACC(2) already computed
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[2])) & (NVEG1 >= NVALP0)) {
#R C-------------------------------------------------------------------------------
#R  C     Skip section if stage 3 has already occurred
#R C-------------------------------------------------------------------------------
  PROG[2] = FT[2] * FUDAY[2] * min(FSW[2],FNSTR[2],FPSTR[2]) * REM(NPRIOR[2])
  PHZACC[2] = PHZACC[2] + PROG[2]
  if (PHZACC[2] - PHTHRS[2] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       V1 occurs on day DAS
#R C-------------------------------------------------------------------------------
    NVEG1 = DAS
    NVALPH[3] = NVEG1
    STGDOY[2] = YRDOY
    REM[3] = (PHZACC[2] - PHTHRS[2]) / (PROG[2] + 0.00001)
  }
}

##R C-------------------------------------------------------------------------------
##R C     Check for end of juvenile phase
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(3))) .AND. (JPEND .GE. NVALP0)) THEN
#PROG(3) = FT(3) * FUDAY(3) * MIN(FSW(3),FNSTR(3),FPSTR(3))
#&      * REM(NPRIOR(3))
#PHZACC(3) = PHZACC(3) + PROG(3)
#
##R !CHP added for Plant P - 9/20/2004
#IF (VegTime > 0) THEN
#VegFrac = PHZACC(3) / VegTime
#ELSE
#VegFrac = 0.0
#ENDIF
##R !        IF(PHZACC(3) .GE. PHTHRS(3)) THEN
#IF(PHZACC(3) - PHTHRS(3) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       End of juvenile phase occurs on day DAS
##R C-------------------------------------------------------------------------------
#JPEND = DAS
#NVALPH(4) = JPEND
#STGDOY(3) = YRDOY
#REM(4) = (PHZACC(3) - PHTHRS(3))/(PROG(3) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for end of juvenile phase
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[3])) & (JPEND >= NVALP0)) {
  PROG[3] = FT[3] * FUDAY[3] * min(FSW[3],FNSTR[3],FPSTR[3]) * REM(NPRIOR[3])
  PHZACC[3] = PHZACC[3] + PROG[3]
# !CHP added for Plant P - 9/20/2004
  if (VegTime > 0) {
    VegFrac = PHZACC[3] / VegTime
    ELSE
    VegFrac = 0.0
    }
  if(PHZACC[3] - PHTHRS[3] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       End of juvenile phase occurs on day DAS
#R C-------------------------------------------------------------------------------
    JPEND = DAS
    NVALPH[4] = JPEND
    STGDOY[3] = YRDOY
    REM[4] = (PHZACC[3] - PHTHRS[3])/(PROG[3] + 0.00001)
    }
}

##R C-------------------------------------------------------------------------------
##R C     Check for floral induction, end of induction phase
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(4))) .AND. (NR0 .GE. NVALP0)) THEN
#PROG(4) = FT(4) * FUDAY(4) * MIN(FSW(4),FNSTR(4),FPSTR(4))
#&      *REM(NPRIOR(4))
#PHZACC(4) = PHZACC(4) + PROG(4)
#VegFrac = (PHTHRS(3) + PHZACC(4)) / VegTime
##R !        IF(PHZACC(4) .GE. PHTHRS(4)) THEN
#IF(PHZACC(4) - PHTHRS(4) > 1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Floral induction occurs on day DAS, end of phase 4
##R C-------------------------------------------------------------------------------
#NR0 = DAS
#NVALPH(5) = NR0
#RSTAGE    = 0
#STGDOY(4) = YRDOY
#REM(5) = (PHZACC(4) - PHTHRS(4))/(PROG(4) + 0.00001)
#ENDIF
#ENDIF
  
#R C-------------------------------------------------------------------------------
#R C     Check for floral induction, end of induction phase
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[4])) &. (NR0 >= NVALP0)) {
  PROG[4] = FT[4] * FUDAY[4] * min(FSW[4],FNSTR[4],FPSTR[4]) * REM(NPRIOR[4])
  PHZACC[4] = PHZACC[4] + PROG[4]
  VegFrac = (PHTHRS[3] + PHZACC[4]) / VegTime
  if(PHZACC[4] - PHTHRS[4] > 1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Floral induction occurs on day DAS, end of phase 4
#R C-------------------------------------------------------------------------------
    NR0 = DAS
    NVALPH(5) = NR0
    RSTAGE    = 0
    STGDOY[4] = YRDOY
    REM[5] = (PHZACC[4] - PHTHRS[4])/(PROG[4] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for first flower, stage 6, end of phase 5
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(5))) .AND. (NR1 .GE. NVALP0)) {
#PROG(5) = FT(5) * FUDAY(5) * MIN(FSW(5),FNSTR(5),FPSTR(5))
#&      * REM(NPRIOR(5))
#PHZACC(5) = PHZACC(5) + PROG(5)
#VegFrac = (PHTHRS(3) + PHTHRS(4) + PHZACC(5)) / VegTime
##R !        IF(PHZACC(5) .GE. PHTHRS(5)) THEN
#IF(PHZACC(5) - PHTHRS(5) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       First flower occurs on day DAS
##R C-------------------------------------------------------------------------------
#NR1 = DAS
#STGDOY(5) = YRDOY
#NVALPH(6) = NR1
#YRNR1     = YRDOY
#RSTAGE    = 1
#REM(6) = (PHZACC(5) - PHTHRS(5))/(PROG(5) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for first flower, stage 6, end of phase 5
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[5])) & (NR1 >= NVALP0)) {
  PROG[5] = FT[5] * FUDAY[5] * min(FSW[5],FNSTR[5],FPSTR[5]) * REM(NPRIOR[5])
  PHZACC[5] = PHZACC[5] + PROG[5]
  VegFrac = (PHTHRS[3] + PHTHRS[4] + PHZACC[5]) / VegTime
  if(PHZACC[5] - PHTHRS[5] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       First flower occurs on day DAS
#R C-------------------------------------------------------------------------------
    NR1 = DAS
    STGDOY[5] = YRDOY
    NVALPH[6] = NR1
    YRNR1     = YRDOY
    RSTAGE    = 1
    REM[6] = (PHZACC[5] - PHTHRS[5])/(PROG[5] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for beginning ovule (peg), stage 7, end of phase 6
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(6))) .AND. (NR2 .GE. NVALP0)) THEN
#PROG(6) = FT(6) * FUDAY(6) * MIN(FSW(6),FNSTR(6),FPSTR(6))
#&      * REM(NPRIOR(6))
#PHZACC(6) = PHZACC(6) + PROG(6)
##R !        IF(PHZACC(6) .GE. PHTHRS(6)) THEN
#IF(PHZACC(6) - PHTHRS(6) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       First peg occurs on day DAS
##R C-------------------------------------------------------------------------------
#NR2 = DAS
#STGDOY(6) = YRDOY
#NVALPH(7) = NR2
#YRNR2     = YRDOY
#RSTAGE    = 2
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(7) = (PHZACC(6) - PHTHRS(6))/(PROG(6) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for beginning ovule (peg), stage 7, end of phase 6
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[6])) & (NR2 >= NVALP0)) {
  PROG[6] = FT[6] * FUDAY[6] * min(FSW[6],FNSTR[6],FPSTR[6]) * REM(NPRIOR[6])
  PHZACC[6] = PHZACC[6] + PROG[6]
  if(PHZACC[6] - PHTHRS[6] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       First peg occurs on day DAS
#R C-------------------------------------------------------------------------------
    NR2 = DAS
    STGDOY[6] = YRDOY
    NVALPH[7] = NR2
    YRNR2     = YRDOY
    RSTAGE    = 2
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[7] = (PHZACC[6] - PHTHRS[6])/(PROG[6] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage beginning shell, stage 8, end of phase 7
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(7))) .AND. (NR3 .GE. NVALP0)) THEN
#PROG(7) = FT(7) * FUDAY(7) * MIN(FSW(7),FNSTR(7),FPSTR(7))
#&      * REM(NPRIOR(7))
#PHZACC(7) = PHZACC(7) + PROG(7)
##R !        IF(PHZACC(7) .GE. PHTHRS(7)) THEN
#IF(PHZACC(7) - PHTHRS(7) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage R3 occurs on day DAS
##R C-------------------------------------------------------------------------------
#NR3 = DAS
#STGDOY(7) = YRDOY
#IF (STGDOY(7) .EQ. STGDOY(6)) STGDOY(7) = 9999999
#NVALPH(8) = NR3
#YRNR3     = YRDOY
#RSTAGE    = 3
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(8) = (PHZACC(7) - PHTHRS(7))/(PROG(7) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage beginning shell, stage 8, end of phase 7
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[7])) & (NR3 >= NVALP0)) {
  PROG[7] = FT[7] * FUDAY[7] * min(FSW[7],FNSTR[7],FPSTR[7]) * REM(NPRIOR[7])
  PHZACC[7] = PHZACC[7] + PROG[7]
  if(PHZACC[7] - PHTHRS[7] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage R3 occurs on day DAS
#R C-------------------------------------------------------------------------------
    NR3 = DAS
    STGDOY[7] = YRDOY
    if (STGDOY[7] == STGDOY[6]) STGDOY[7] = 9999999
    NVALPH[8] = NR3
    YRNR3     = YRDOY
    RSTAGE    = 3
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[8] = (PHZACC[7] - PHTHRS[7])/(PROG[7] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage beginning seed (R5), stage 9, end of phase 8
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(8))) .AND. (NR5 .GE. NVALP0)) THEN
#PROG(8) = FT(8) * FUDAY(8) * MIN(FSW(8),FNSTR(8),FPSTR(8))
#&      * REM(NPRIOR(8))
#PHZACC(8) = PHZACC(8) + PROG(8)
##R !STGRATIO8 = PHZACC(8) / PHTHRS(8)
#VegFrac =(PHTHRS(3) + PHTHRS(4) + PHTHRS(5) + PHZACC(8))/VegTime
##R !        IF(PHZACC(8) .GE. PHTHRS(8)) THEN
#IF(PHZACC(8) - PHTHRS(8) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage R5 occurs on day DAS
##R C-------------------------------------------------------------------------------
#NR5 = DAS
#STGDOY(8) = YRDOY
#NVALPH(9) = NR5
#YRNR5     = YRDOY
#RSTAGE    = 5
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
#REM(9) = (PHZACC(8) - PHTHRS(8))/(PROG(8) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage beginning seed (R5), stage 9, end of phase 8
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[8])) & (NR5 >= NVALP0)) {
PROG[8] = FT[8] * FUDAY[8] * min(FSW[8],FNSTR[8],FPSTR[8]) * REM(NPRIOR[8])
PHZACC[8] = PHZACC[8] + PROG[8]
VegFrac =(PHTHRS[3] + PHTHRS[4] + PHTHRS[5] + PHZACC[8])/VegTime
if(PHZACC[8] - PHTHRS[8] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage R5 occurs on day DAS
#R C-------------------------------------------------------------------------------
NR5 = DAS
STGDOY[8] = YRDOY
NVALPH[9] = NR5
YRNR5     = YRDOY
RSTAGE    = 5
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
REM[9] = (PHZACC[8] - PHTHRS[8])/(PROG[8] + 0.00001)
}
}

##R C-------------------------------------------------------------------------------
##R C     Check for stage NDSET, stage 10, end of phase 9
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(9))) .AND. (NDSET .GE. NVALP0)) THEN
#PROG(9) = FT(9) * FUDAY(9) * MAX(FSW(9),FNSTR(9),FPSTR(9))
#&      * REM(NPRIOR(9))
#PHZACC(9) = PHZACC(9) + PROG(9)
##R !        IF(PHZACC(9) .GE. PHTHRS(9)) THEN
#IF(PHZACC(9) - PHTHRS(9) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage NDSET occurs on day DAS
##R C-------------------------------------------------------------------------------
#NDSET = DAS
#STGDOY(9) = YRDOY
#NVALPH(10) = NDSET
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(10) = (PHZACC(9) - PHTHRS(9))/(PROG(9) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage NDSET, stage 10, end of phase 9
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[9])) & (NDSET >= NVALP0)) {
  PROG[9] = FT[9] * FUDAY[9] * max(FSW[9],FNSTR[9],FPSTR[9]) * REM(NPRIOR[9]) #MAX ao inves de MIN
  PHZACC[9] = PHZACC[9] + PROG[9]
  if(PHZACC[9] - PHTHRS[9] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage NDSET occurs on day DAS
#R C-------------------------------------------------------------------------------
    NDSET = DAS
    STGDOY[9] = YRDOY
    NVALPH[10] = NDSET
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[10] = (PHZACC[9] - PHTHRS[9])/(PROG[9] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage NR7, stage 11, end of phase 10
##R C-------------------------------------------------------------------------------
##R IF ((DAS .GE. NVALPH(NPRIOR(10))) .AND. (NR7 .GE. NVALP0)) THEN
#PROG(10) = FT(10) * FUDAY(10)*MAX(FSW(10),FNSTR(10),FPSTR(10))
#&      * REM(NPRIOR(10))
#PHZACC(10) = PHZACC(10) + PROG(10)
#SeedFrac = PHZACC(10) / PHTHRS(10)
##R !        IF(PHZACC(10) .GE. PHTHRS(10)) THEN
#IF(PHZACC(10) - PHTHRS(10) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage NR7, physiological maturity, occurs on day DAS
##R C-------------------------------------------------------------------------------
#NR7 = DAS
#STGDOY(10) = YRDOY
#NVALPH(11) = NR7
#YRNR7      = YRDOY
#RSTAGE    = 7
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(11) = (PHZACC(10) - PHTHRS(10))/(PROG(10) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage NR7, stage 11, end of phase 10
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[10])) & (NR7 >= NVALP0)) {
  PROG[10] = FT[10] * FUDAY[10]*max(FSW[10],FNSTR[10],FPSTR[10]) * REM(NPRIOR[10]) #MAX ao inves de MIN
  PHZACC[10] = PHZACC[10] + PROG[10]
  SeedFrac = PHZACC[10] / PHTHRS[10]
  if(PHZACC[10] - PHTHRS[10] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage NR7, physiological maturity, occurs on day DAS
#R C-------------------------------------------------------------------------------
    NR7 = DAS
    STGDOY[10] = YRDOY
    NVALPH[11] = NR7
    YRNR7      = YRDOY
    RSTAGE     = 7
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[11] = (PHZACC[10] - PHTHRS[10])/(PROG[10] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage NR8, stage 12, end of phase 11
##R C-------------------------------------------------------------------------------
##R IF ((DAS .GE. NVALPH(NPRIOR(11))) .AND. (MDATE .LE. YRSIM)) THEN
#PROG(11) = FT(11) * FUDAY(11)*MIN(FSW(11),FNSTR(11),FPSTR(11))
#&      * REM(NPRIOR(11))
#PHZACC(11) = PHZACC(11) + PROG(11)
##R !        IF(PHZACC(11) .GE. PHTHRS(11)) THEN
#IF(PHZACC(11) - PHTHRS(11) > 1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage NR8, harvest maturity, occurs on day DAS
##R C-------------------------------------------------------------------------------
#  #R !          NR8 = DAS
#STGDOY(11) = YRDOY
##R !          NVALPH(12) = NR8
#NVALPH(12) = DAS
#MDATE      = YRDOY
#RSTAGE    = 8
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(12) = (PHZACC(11) - PHTHRS(11))/(PROG(11) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage NR8, stage 12, end of phase 11
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[11])) & (MDATE <= YRSIM)) {
  PROG[11] = FT[11] * FUDAY[11]*min(FSW[11],FNSTR[11],FPSTR[11]) * REM(NPRIOR[11])
  PHZACC[11] = PHZACC[11] + PROG[11]
  if(PHZACC[11] - PHTHRS[11] > 1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage NR8, harvest maturity, occurs on day DAS
#R C-------------------------------------------------------------------------------
    STGDOY[11] = YRDOY
    NVALPH[12] = DAS
    MDATE      = YRDOY
    RSTAGE     = 8
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[12] = (PHZACC[11] - PHTHRS[11])/(PROG[11] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(12))) .AND. (NDVST .GE. NVALP0)) THEN
#PROG(12) = FT(12) * FUDAY(12)*MIN(FSW(12),FNSTR(12),FPSTR(12))
#&      * REM(NPRIOR(12))
#PHZACC(12) = PHZACC(12) + PROG(12)
##R !        IF(PHZACC(12) .GE. PHTHRS(12)) THEN
#IF(PHZACC(12) - PHTHRS(12) > 1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R C       Stage NDVST, end of V-stage addition, occurs on day DAS
##R C-------------------------------------------------------------------------------
#NDVST = DAS
#STGDOY(12) = YRDOY
#NVALPH(13) = NDVST
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(13) = (PHZACC(12) - PHTHRS(12))/(PROG(12) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[12])) & (NDVST >= NVALP0)) {
  PROG[12] = FT[12] * FUDAY[12]*min(FSW[12],FNSTR[12],FPSTR[12]) * REM(NPRIOR[12])
  PHZACC[12] = PHZACC[12] + PROG[12]
  if(PHZACC[12] - PHTHRS[12] > 1.E-6) {
#R C-------------------------------------------------------------------------------
#R C       Stage NDVST, end of V-stage addition, occurs on day DAS
#R C-------------------------------------------------------------------------------
    NDVST = DAS
    STGDOY[12] = YRDOY
    NVALPH[13] = NDVST
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[13] = (PHZACC[12] - PHTHRS[12])/(PROG[12] + 0.00001)
  }
}
  
##R C-------------------------------------------------------------------------------
##R C     Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
##R C-------------------------------------------------------------------------------
#IF ((DAS .GE. NVALPH(NPRIOR(13))) .AND. (NDLEAF .GE. NVALP0)) THEN
#PROG(13) = FT(13) * FUDAY(13)*MIN(FSW(13),FNSTR(13),FPSTR(13))
#&      * REM(NPRIOR(13))
#PHZACC(13) = PHZACC(13) + PROG(13)
##R !        IF(PHZACC(13) .GE. PHTHRS(13)) THEN
#IF(PHZACC(13) - PHTHRS(13) > -1.E-6) THEN
##R C-------------------------------------------------------------------------------
##R        Stage NDLEAF, end of leaf growth, occurs on day DAS
##R C-------------------------------------------------------------------------------
##R NDLEAF = DAS
#STGDOY(13) = YRDOY
#NVALPH(14) = NDLEAF
##R C-------------------------------------------------------------------------------
##R C       Account for the part of today that contributes to the next phase(s)
##R C-------------------------------------------------------------------------------
#REM(14) = (PHZACC(13) - PHTHRS(13))/(PROG(13) + 0.00001)
#ENDIF
#ENDIF

#R C-------------------------------------------------------------------------------
#R C     Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
#R C-------------------------------------------------------------------------------
if ((DAS >= NVALPH(NPRIOR[13])) & (NDLEAF >= NVALP0)) {
  PROG[13] = FT[13] * FUDAY[13]*min(FSW[13],FNSTR[13],FPSTR[13]) * REM(NPRIOR[13])
  PHZACC[13] = PHZACC[13] + PROG[13]
  if(PHZACC[13] - PHTHRS[13] > -1.E-6) {
#R C-------------------------------------------------------------------------------
#R        Stage NDLEAF, end of leaf growth, occurs on day DAS
#R C-------------------------------------------------------------------------------
    NDLEAF = DAS
    STGDOY[13] = YRDOY
    NVALPH[14] = NDLEAF
#R C-------------------------------------------------------------------------------
#R C       Account for the part of today that contributes to the next phase(s)
#R C-------------------------------------------------------------------------------
    REM[14] = (PHZACC[13] - PHTHRS[13])/(PROG[13] + 0.00001)
  }
}
  
#R !      #R !Daily printout
#R !      #R !Note: just print PHTEM will get rid of debug vs. release problems
#R !      DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))
#R !      CALL YR_DOY(YRDOY, YEAR, DOY)
#R !
  #R !      WRITE (LUN,300) YEAR, DOY, DAS, DAP, RSTAGE, PHTEM,
#R !     &                        (PHZACC(I),I=1,20)
#R !  300 FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I5,1X,F6.2,20(1X,F7.3))

#R !************************************************************************
  #R !************************************************************************
  #R !     End of DYNAMIC IF construct
#R !************************************************************************
  #R END IF
  }
#R !************************************************************************
#R  RETURN
# todo
#R END SUBROUTINE RSTAGES

}

} # para facilitar a programacao

#### Subrotina: IPPHENOL ####
{

#R #R !************************************************************************
#R #R !************************************************************************
  
  SUBROUTINE IPPHENOL(CONTROL,
                      &    ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, CROP,    #R !Output
                      &    CTMP, DLTYP, EVMODC, NPRIOR, NSENP, OPTBI,      #R !Output
                      &    PHTHRS, PLME, PSENP, SDAGE, SDEPTH, SLOBI,      #R !Output
                      &    THVAR, TRIFOL, TSELC, TB, TO1, TO2, TM, WSENP)  #R !Output

#R !-----------------------------------------------------------------------
#R USE ModuleDefs     #R !Definitions of constructed variable types,
#R ! which contain control information, soil
#R ! parameters, hourly weather data.
#R IMPLICIT NONE
#R !-----------------------------------------------------------------------
CHARACTER*1   PLME, BLANK
CHARACTER*2   CROP
CHARACTER*3   CTMP(20), DLTYP(20)
CHARACTER*6   SECTION, ECOTYP, ECONO, ERRKEY
CHARACTER*12  FILEC, FILEE
CHARACTER*16  ECONAM
CHARACTER*30  FILEIO
CHARACTER*80  CHAR, PATHCR, PATHEC
CHARACTER*92  FILECC, FILEGC
CHARACTER*255 C255

INTEGER LUNIO, NPHS
INTEGER LUNCRP, LUNECO, ISECT, PATHL
INTEGER I, J, K
INTEGER IVRGRP, IVRTEM, ERR, LINC, LNUM, FOUND
INTEGER NPRIOR(20), TSELC(20)

PARAMETER (BLANK = ' ')
PARAMETER (ERRKEY = 'IPPHEN')
PARAMETER (NPHS = 13)

REAL ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, EVMODC
REAL OPTBI
REAL PPSEN, PH2T5, R1PPO, PM06, PM09
REAL SDEPTH, SDAGE, SLOBI, THVAR, TRIFOL
REAL TB(5), TO1(5), TO2(5), TM(5)
REAL WSENP(20), NSENP(20)
REAL PHTHRS(20), PSENP(20)

#R !-----------------------------------------------------------------------
#R !     Define constructed variable types based on definitions in
#R !     ModuleDefs.for.

#R !     The variable "CONTROL" is of type "ControlType".
TYPE (ControlType) CONTROL

#R !     Transfer values from constructed data types into local variables.
FILEIO  = CONTROL % FILEIO
LUNIO   = CONTROL % LUNIO

#R !-----------------------------------------------------------------------
#R !     Read in values from temporary file, which were previously input
#R !       in Subroutine IPIBS.
#R !-----------------------------------------------------------------------
OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
IF (ERR .NE. 0) print *, "!!! ERROR !!!", ERRKEY,ERR,FILEIO,0
*      CALL ERROR(ERRKEY,ERR,FILEIO,0)

READ (LUNIO,100,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
100   FORMAT(//////,15X,A12,1X,A80)
IF (ERR .NE. 0) print *, "!!! ERROR !!!!",ERRKEY,ERR,FILEIO,LNUM
*      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
105   FORMAT(15X,A12,1X,A80)
IF (ERR .NE. 0) print *, "!!! ERROR !!!!",ERRKEY,ERR,FILEIO,LNUM
*      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

#R !#R !-----------------------------------------------------------------------
#R !#R !     Subroutine FIND finds appropriate SECTION in a file by
#R !#R !     searching for the specified 6-character string at beginning
#R !#R !     of each line.
#R !#R !-----------------------------------------------------------------------
#R !      SECTION = '*SIMUL'
#R !      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#R !      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
#R !      READ(LUNIO,'(31X,A1)',IOSTAT=ERR) ISIMI; LNUM = LNUM + 1
#R !      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

#R !-----------------------------------------------------------------------
#R !     Find and read Cultivar Section
#R !-----------------------------------------------------------------------
SECTION = '*CULTI'
CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
IF (FOUND .EQ. 0) print *, "!!! ERROR !!!!",SECTION, 42, FILEIO,
& LNUM
*      CALL ERROR (SECTION, 42, FILEIO,LNUM)
READ(LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
*      CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

#R !-----------------------------------------------------------------------
IF (CROP .NE. 'FA') THEN
#R !-----------------------------------------------------------------------
#R !     Find and Read Planting Details Section
#R !-----------------------------------------------------------------------
SECTION = '*PLANT'
CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
IF (FOUND .EQ. 0) print *, "!!! ERROR !!!",SECTION, 42, FILEIO,
& LNUM
*        CALL ERROR (SECTION, 42, FILEIO,LNUM)
READ(LUNIO,140,IOSTAT=ERR) PLME, SDEPTH, SDAGE, ATEMP
140     FORMAT(35X,A1,19X,F5.1,6X,2(1X,F5.0))
LNUM = LNUM + 1
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
*        CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

#R !-----------------------------------------------------------------------
#R !     Find and read Cultivar Section
#R !-----------------------------------------------------------------------
  SECTION = '*CULTI'
CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
IF (FOUND .EQ. 0) print *, "!!! ERROR !!!",SECTION, 42, FILEIO,
& LNUM
*        CALL ERROR (SECTION, 42, FILEIO,LNUM)
READ(LUNIO,165,IOSTAT=ERR) ECONO, CSDVAR, PPSEN, PH2T5,
&              PHTHRS(6), PHTHRS(8), PHTHRS(10), PHTHRS(13)
165     FORMAT(24X,A6,7F6.0)
LNUM = LNUM + 1
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEIO,LNUM
*        CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
ENDIF

CLOSE (LUNIO)

#R !-----------------------------------------------------------------------
#R !     Open FILEC
#R !-----------------------------------------------------------------------
IF (CROP .NE. 'FA') THEN
PATHL  = INDEX(PATHCR,BLANK)
IF (PATHL .LE. 1) THEN
FILECC = FILEC
ELSE
FILECC = PATHCR(1:(PATHL-1)) // FILEC
ENDIF

CALL GETLUN('FILEC', LUNCRP)
OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,0
*        CALL ERROR(ERRKEY,ERR,FILECC,0)
LNUM = 0
#R !-----------------------------------------------------------------------
#R !     Find Leaf Growth Parameters from FILEC and read EVMODC value
#R !-----------------------------------------------------------------------
  SECTION = '!*LEAF'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
IF (FOUND .EQ. 0) print *, "!!! ERROR !!!",SECTION, 42, FILECC,
& LNUM
*        CALL ERROR (SECTION, 42, FILECC,LNUM)
CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
READ(CHAR,'(24X,F6.1)',IOSTAT=ERR) EVMODC
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
#R !-----------------------------------------------------------------------
#R !     Find Phenology Section in FILEC and read
#R !-----------------------------------------------------------------------
  SECTION = '!*PHEN'
CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
IF (FOUND .EQ. 0) print *, "!!! ERROR !!!",SECTION, 42, FILECC,
& LNUM
*        CALL ERROR (SECTION, 42, FILECC,LNUM)
CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
READ(CHAR,250,IOSTAT=ERR) TB(1), TO1(1), TO2(1), TM(1)
250     FORMAT(13F6.1)
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
*        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
READ(CHAR,250,IOSTAT=ERR) TB(2), TO1(2), TO2(2), TM(2)
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
*        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
READ(CHAR,250,IOSTAT=ERR) TB(3), TO1(3), TO2(3), TM(3)
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,LNUM
*        CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

DO I = 1,NPHS
CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
READ(CHAR,270,IOSTAT=ERR) J, NPRIOR(J), DLTYP(J), CTMP(J),
&        TSELC(J), WSENP(J), NSENP(J), PSENP(J)
270       FORMAT(I3,I3,2(2X,A3),1X,I2,3(1X,F5.2))
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILECC,
& LNUM
*          CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
ENDDO

CLOSE (LUNCRP)

#R C-----------------------------------------------------------------------
#R C     Open FILEE
#R C-----------------------------------------------------------------------
LNUM = 0
PATHL  = INDEX(PATHEC,BLANK)
IF (PATHL .LE. 1) THEN
FILEGC = FILEE
ELSE
FILEGC = PATHEC(1:(PATHL-1)) // FILEE
ENDIF

#R C-----------------------------------------------------------------------
#R C    Read Ecotype Parameter File
#R C-----------------------------------------------------------------------
CALL GETLUN('FILEE', LUNECO)
OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEGC,0
*        CALL ERROR(ERRKEY,ERR,FILEGC,0)
ECOTYP = '      '
LNUM = 0

DO WHILE (ECOTYP .NE. ECONO)
CALL IGNORE(LUNECO, LNUM, ISECT, C255)
IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
    &          C255(1:1) .NE. '*') THEN
READ (C255,3100,IOSTAT=ERR) ECOTYP, ECONAM, IVRGRP,
&          IVRTEM, THVAR, (PHTHRS(K), K=1,4), PM06, PM09,
&          (PHTHRS(K),K=11,12), TRIFOL, R1PPO, OPTBI, SLOBI
3100        FORMAT (A6, 1X, A16, 1X, 2(1X,I2), 7(1X,F5.0), 6X,
                    &          3(1X,F5.0), 2(6X), 3(1X,F5.0))
IF (ERR .NE. 0) print *, "!!! ERROR !!!",ERRKEY,ERR,FILEGC,
& LNUM
*            CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
IF (ECOTYP .EQ. ECONO) THEN
EXIT
ENDIF

ELSE IF (ISECT .EQ. 0) THEN
IF (ECONO .EQ. 'DFAULT') print *, "!!! ERROR !!!",ERRKEY,35,
& FILEGC,LNUM
*            CALL ERROR(ERRKEY,35,FILEGC,LNUM)
ECONO = 'DFAULT'
REWIND(LUNECO)
LNUM = 0
ENDIF
ENDDO

CLOSE (LUNECO)

PHTHRS(5) = MAX(0.,PH2T5 - PHTHRS(3) - PHTHRS(4))
PHTHRS(7) = PHTHRS(6) + MAX(0.,(PHTHRS(8) - PHTHRS(6))* PM06)
PHTHRS(9) = MAX(0.,PHTHRS(10) * PM09)

IF (PPSEN .GE. 0.0) THEN
CLDVAR = CSDVAR + (1.-THVAR)/MAX(PPSEN,0.000001)
ELSE IF (PPSEN .LT. 0.0) THEN
CLDVAR = CSDVAR + (1.-THVAR)/MIN(PPSEN,-0.000001)
ENDIF

CSDVRR = CSDVAR - R1PPO
CLDVRR = CLDVAR - R1PPO

ENDIF

RETURN
END  SUBROUTINE IPPHENOL

#R !************************************************************************
#R !************************************************************************

} # para facilitar a programacao