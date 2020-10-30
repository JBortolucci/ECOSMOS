forage_harvest <- function(){
  
  SUBROUTINE forage_harvest(CONTROL,FILECC,
                            &                RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,!Input
                            &                WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST, !Input/Output
                            &                WTNLF,WTNST,WNRLF,WNRST,WTNCAN,     !Input/Output
                            &                AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
                            &                fhtot,FHTOTN, fhpctlf,fhpctn,FREQ,CUHT,MOWC,RSPLC,
                            &                DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
                            &                WTCO, WTLO, WTSO)
  
  
  #----------------------------------------------------------------------------
  # TODO: Trazer como parametros ??
  # YRDOY  = CONTROL % YRDOY
  # crop   = control % crop
  # trtno  = control % trtnum
  # run    = control % run
  # ename  = control % ename
  # mowfile = control % filex
  # mowfile(10:12) = 'MOW'
  # ERRKEY = 'FRHARV'
  # --------------------------------------------------------------------------
  
  !Daily Senescence
  DWTCO <- WTCO - PWTCO
  DWTLO <- WTLO - PWTLO
  DWTSO <- WTSO - PWTSO       
  A
  
  # --------------------------------------------------------------------------
  # Leitura de parâmetros MOW
  # IF (.NOT.ALLOCATED(MOW)) THEN
  # 
  # CALL GETLUN('FILEC', LUNCRP)
  # OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
  # IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
  # LNUM = 1
  # SECTION = '!*PLAN'
  # CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
  # IF (FOUND .EQ. 0) THEN
  # CALL ERROR(ERRKEY, 1, FILECC, LNUM)
  # ELSE
  # CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80)
  # READ(MOW80,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR)
  # &                    PROLFF, PROSTF
  # do j=1,5; CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80); end do
  # READ(MOW80,'(2f6.0)',IOSTAT=ERR)
  # &                    pliglf, pligst
  # CLOSE(LUNCRP)
  # IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  # END IF
  # 
  # MOWLUN=999
  # 
  # OPEN (UNIT=MOWLUN,FILE=MOWFILE,STATUS='OLD',IOSTAT=ERR)
  # IF (ERR .NE. 0) CALL ERROR(ERRKEY,29,MOWFILE,LNUM)
  # REWIND(MOWLUN)
  # 
  # ISECT = 0
  # MOWCOUNT = 0
  # write(trtchar,'(i6)') trtno
  # DO WHILE (ISECT.EQ.0)
  # READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
  # IF (MOW80(1:1).NE."@"
  #     &       .AND.MOW80(1:1).NE."!"
  #     &       .AND.MOW80(1:20).NE."                    "
  #     &       .and.mow80(1:6)==trtchar
  #     &       .AND.ISECT.EQ.0)THEN
  # MOWCOUNT = MOWCOUNT + 1
  # END IF
  # END DO
  # REWIND(MOWLUN)
  # 
  # 
  # IF (MOWCOUNT.GT.0) THEN
  # ALLOCATE(TRNO(MOWCOUNT),DATE(MOWCOUNT),MOW(MOWCOUNT))
  # ALLOCATE(RSPLF(MOWCOUNT),MVS(MOWCOUNT),rsht(mowcount))
  # ELSE
  # C         MOW file has no data for this treatment
  # CALL ERROR(ERRKEY,2,MOWFILE,0)
  # ALLOCATE(MOW(1))
  # MOW (1) = -99
  # RETURN
  # END IF
  # 
  # I = 0
  # ISECT = 0
  # DO WHILE (ISECT.EQ.0)
  # READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
  # IF (MOW80(1:1).NE."@"
  #     &       .AND.MOW80(1:1).NE."!"
  #     &       .AND.MOW80(1:20).NE."                    "
  #     &       .and.mow80(1:6)==trtchar
  #     &       .AND.ISECT.EQ.0)THEN
  # I = I + 1
  # READ (MOW80,'(2I6,4F6.0)',IOSTAT=ISECT)
  # &                TRNO(I),DATE(I),MOW(I),RSPLF(I),MVS(I),rsht(i)
  # CALL Y2K_DOY(DATE(I))
  # END IF
  # END DO
  # 
  # END IF
  # --------------------------------------------------------------------------
  
  # Alocar tamanho no vetor de valores MOW e trocar SIZE(MOW)
  for (I in length(MOW) ){
    
    if(date[i] == yrdoy) {
      if ((MOW[I] >= 0) && (trno[i] == trtno)) {
        if(mow[i]/10 < topwt) {
          FHLEAF <- 0
          FHSTEM <- 0
          FHVSTG <- 0
          if(RSPLF[I] >= 0){
            FHLEAF <- WTLF-(MOW[I]/10)*RSPLF[I]/100
            FHSTEM <- STMWT-(MOW[I]/10)*(1.0-RSPLF[I]/100)
          } else {
            FHLEAF <- WTLF-(MOW[I]/10)*WTLF/(WTLF+STMWT)
            FHSTEM <- STMWT-(MOW[I]/10)*STMWT/(WTLF+STMWT)
          }
          FHLEAF <- MAX(FHLEAF,0.0)
          FHSTEM <- MAX(FHSTEM,0.0)
          FHVSTG <- MAX(MVS[I],0.0)
          canht  <- max(rsht(i)/100,0.0)
          #              canht=max(rsht[i],0.0)     #enter rsht in cm
          
          
          fhtot <- fhleaf+fhstem
          
          fhlfn <- fhleaf*pcnl/100
          fhstn <- fhstem*pcnst/100
          fhtotn <- fhlfn+fhstn
          
          fhcrlf <- fhleaf*rhol
          fhcrst <- fhstem*rhos
          
          fhpctn <- fhtotn/fhtot*100
          fhplig <- (fhleaf*pliglf+fhstem*pligst)/fhtot*100
          fhpcho <- (fhcrlf+fhcrst)/fhtot*100
          fhpctlf <- fhleaf/fhtot*100
          
          WTLF  <- WTLF - FHLEAF
          STMWT <- STMWT - FHSTEM
          TOPWT <- TOPWT - FHLEAF - FHSTEM
          TOTWT <- TOTWT - FHLEAF - FHSTEM
          
          WCRLF <- WTLF*RHOL
          WCRST <- STMWT*RHOS
          
          WTNLF  <- WTLF*PCNL/100.
          WTNST  <- STMWT*PCNST/100.
          WTNCAN <- WTNCAN - FHLEAF*PCNL/100. - FHSTEM*PCNST/100.
          
          if ((WTLF - WCRLF) > 0.0) {
            WNRLF <- MAX (WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
          } else {
            WNRLF <- 0.0
          }
          
          if ((STMWT - WCRST) > 0.0) {
            WNRST <- MAX (WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
          } else {
            WNRST <- 0.0
          }
          
          AREALF <- WTLF*SLA
          XLAI   <- AREALF/10000.
          XHLAI  <- XLAI
          
          VSTAGE <- FHVSTG     
          vstagp <- vstage
          
        } else {
          
          fhtot  <- 0
          
          fhlfn   <- 0
          fhstn   <- 0
          fhtotn  <- 0
          
          fhcrlf  <- 0
          fhcrst  <- 0
          
          fhpctn  <- 0
          fhplig  <- 0
          fhpcho  <- 0
          
          fhpctlf <- 0
          
          
        }
        
        # CALL GETLUN('FORHARV', fhlun)
        # 
        # INQUIRE(file=FHOUT,EXIST=EXISTS)
        # IF (exists.and.(run/=1.or.i/=1)) THEN
        # OPEN(FILE=FHOUT,UNIT=FHLUN,POSITION='APPEND')
        # ELSE
        # call date_and_time(values=date_time)
        # OPEN(FILE=FHOUT,UNIT=FHLUN)
        # rewind(fhlun)
        # fhoutfmt = 
        #   &     "('*Forage Model Harvest Output: ',A8,A2,1X,A,1X,"//
        #   &     "'DSSAT Cropping System Model Ver. '"//
        #   &     ",I1,'.',I1,'.',I1,'.',"//
        #   &     "I3.3,1X,A10,4X,"//
        #   &     "A3,' ',I2.2,', ',I4,'; ',I2.2,':',I2.2,':',I2.2/)"
        # WRITE (fhlun,fhoutfmt) mowfile(1:8),crop,trim(ename),
        # &             Version,VBranch,
        # &             MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
        # &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        # WRITE(fhlun,'(a)')
        # &           '@RUN FILEX    CR TRNO FHNO YEAR DOY'//
        #   &           ' RCWAH RLWAH RSWAH RSRWH RRTWH RLAIH'//
        #   &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H'
        # !     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H FHAGE IVOMD'//
        #   !     &           '   MOWC RSPLC'
        # end if
        # call yr_doy(yrdoy,year,doy)
        # write(fhoutfmt,'(a)') '(i4,x,a8,a3,2(i5),i5,i4,'//
        #   &            '5(i6),f6.2,2(i6),3(f6.2),f6.1,i6,f6.1,x,f8.0,F6.1)'
        # WRITE(fhlun,fhoutfmt)
        # &           run,mowfile(1:8),crop,trtno,i,year,doy,
        # &           Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
        # &           Nint(strwt*10.),Nint(rtwt*10.),xlai,
        # &           Nint(fhtot*10.),Nint(fhtotn*10.),
        # &           fhpctn,fhpcho,fhplig,fhpctlf
        # !     &           -99,-99.0,MOWC,RSPLC
        # close(fhlun)
        
        if((date[i] == yrdoy) && (trno[i] == trtno)) {
          PWTCO <- WTCO 
          PWTLO <- WTLO
          PWTSO <- WTSO
          DWTCO <- WTCO - PWTCO
          DWTLO <- WTLO - PWTLO
          DWTSO <- WTSO - PWTSO
        }
        if(i == length(MOW)) 
          return
      }
    }
  }
}