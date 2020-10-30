# ----------------------------------------------------------------
# --------------------------CANOPY--------------------------------

# ADD
CHARACTER*80  C80
CHARACTER*80 PATHCR,CHAR

INTEGER I, II, LUNCRP, LUNECO, ERR, LNUM, ISECT

REAL CUMNHT
REAL HNHGT
REAL NSTRES
REAL NRATIO
REAL NHGT                           
REAL SLAMAX
REAL SLAMIN
REAL SLAPAR
REAL TURSLA
REAL NSLA

# REMOVE 
INTEGER LINC

# --------------------------END CANOPY----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------DEMAND--------------------------------

# ADD
CHARACTER*1 PLME

CHARACTER*80  C80
CHARACTER*6   SECTION
CHARACTER*255 C255
CHARACTER*6   ERRKEY

INTEGER YRSIM, YRDOY

INTEGER LUNCRP, LUNIO, LUNECO, ERR, LNUM, FOUND, ISECT

REAL NRATIO

REAL NFSL
REAL NSLA
REAL CUMNSF
REAL NHGT
REAL SDAGPL

REAL AGRSTR, FNINSR, FRSTR, WLIDOT 
REAL FRSTRF, FRSTRM, FRSTRMX, NMOBSR, NMOBSRN, NMOBSRX
REAL NVSTSR, PCNSR, PPMFAC, PPTFAC, PROSRF, PROSRI
REAL STRWT, TFRLF, TFRSTM, TFRSTR, TFRRT, WCRSR, WNRSR
REAL XLAI, XSTR, YSTOR(8)

CHARACTER*3 TYPLMOB, TYPNMOB 
REAL LRMOB(4), NRMOB(4)
REAL PHZACC(20), SDLEST
REAL XLFEST(8), YLFEST(8), YSREST(8), YSTEST(8)

REAL CDMOLD, CHOPRO, FROLDA, KCOLD
REAL SLDOT, SRDOT, SSDOT, SSRDOT
REAL LFSCMOB, LFSNMOB, RTSCMOB, RTSNMOB, SRSCMOB, SRSNMOB, STSCMOB, STSNMOB
REAL WTNLF, WTNRT, WTNSR, WTNST
REAL PROLFR, PROSTR, PRORTR, PROSRR

# REMOVE 
CHARACTER*78 MSG(2)
INTEGER YREND
# --------------------------END DEMAND----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------FREEZE--------------------------------
# ADD
CHARACTER*30 FILEIO

INTEGER RUN

REAL  FRZDC, FRZDL, NRUSSR, NRUSST, PSRSRFL, PSRLYR1, PSRSRFD, 
REAL  PSRLYRD, SRLYRD,SRFTEMP, SRSRFD, SSRDOT, SSDOT, STMWT,
REAL  STRWT, VSTAGE, WSFDOT, WSRFDOT, FREEZ1
REAL, DIMENSION(NL) :: ST

# REMOVE
# --------------------------END FREEZE----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------GROW----------------------------------
# ADD
CHARACTER*92  FILEGC
CHARACTER*78 MSG(2)
CHARACTER*6   SECTION
CHARACTER(len=6) trtchar
character(len=60) ename
CHARACTER*78 MESSAGE(2)

INTEGER RUN
INTEGER MOWLUN,ISECT,ERR, I
INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
INTEGER LUNCRP,fhlun, LNUM, FOUND
INTEGER MOWCOUNT,j
INTEGER TRTNO

REAL PORPT, TNCFAC

REAL DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO
REAL LTSEN

REAL EODLFC, EODLFN, EODSTC, EODSTN
REAL EODRTC, EODRTN, EODSRC, EODSRN
REAL LFCADDM, LFNADDM, RTCADDM, RTNADDM
REAL SRCADDM, SRNADDM, STCADDM, STNADDM
REAL LFNADFDM, STNADFDM, SRNADFDM
REAL SLCADDOT, SLNADDOT, SRCADDOT, SRNADDOT
REAL SSCADDOT, SSNADDOT, SSRCADDOT, SSRNADDOT 

REAL,ALLOCATABLE,DIMENSION(:) :: MOW,RSPLF,MVS,rsht
REAL CADRT, CADSH, NADSH

REAL CADSR, CRUSSR, FRSTR, NADSR, NGRSR
REAL NRUSSR, SSRDOT, SSRNDOT, WSRDOTN, WSRIDOT
REAL WSRDOT, WSRFDOT
REAL CSRW, PCNSR, PLIGSR, RHOSR, STRWT, WCRSR
REAL WNRSR, WTNSR, WTNSRA, WTNSRO, WTSRO
REAL CPFSTR, NSRDOT, ALPHSR
REAL PCARSR, PLIPSR, PMINSR
REAL POASR, PROSRI, PROSRF, WSRI, WRCSRDT, NSROFF, NSRALL
REAL PSRLYRD, PSRSRFD, PSRSRFL, PSRLYR1, STRSRFL
REAL STRLYR1, TPSRSRFL, TPSRLYR1

REAL CLOFF, CROFF, CSOFF, CSROFF
REAL PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF

REAL PSTCLSMOB, PSTCLSTRES

REAL LFSCMOB, LFSENWT, LFSNMOB, RTSCMOB, RTSNMOB, SRSCMOB
REAL SRSNMOB, STSCMOB, STSNMOB, TSCMOB
REAL SLMDOT, SRMDOT, SRNDOT, SSMDOT, SSRMDOT, STLTSEN, STSENWT 
REAL WSFDOT  

REAL FHLEAF,FHSTEM,FHVSTG
REAL VSTAGE  

REAL SENCLV, SENCRV, SENCSV, SENCSRV
REAL SENNLV, SENNRV, SENNSV, SENNSRV

# REMOVE
REAL NLPEST
REAL ADD
REAL ShutMob, RootMob, ShelMob
# --------------------------END GROW------------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------STRESS--------------------------------
# ADD
CHARACTER*30 FILEIO
INTEGER RUN
REAL STRWT

# REMOVE
# --------------------------END STRESS----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------INCOMP--------------------------------
# ADD
CHARACTER*6 ECONO, ECOTYP
CHARACTER*92 FILEGC
CHARACTER*255 C255

INTEGER LUNECO
INTEGER I

REAL PROSRI, PLIPSR, PLIGSR, POASR, PMINSR, PCARSR
REAL AGRSTR, FRSTR

# REMOVE
CHARACTER*30 FILEIO
INTEGER LUNIO, LINC
# --------------------------END INCOMP----------------------------
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# --------------------------MOBIL---------------------------------
# ADD
REAL NRUSSR, WNRSR, NMOBSR, PPMFAC
REAL NRUSTOT, PNMLF, PNMST, PNMRT, PNMSR, PNMSH
REAL ANMINELF, ANMINERT, ANMINESH, ANMINESR, ANMINEST, LFSNMOB
REAL NMINELF, NMINERT, NMINESR, NMINEST, RTSNMOB, SHNMINE
REAL SRSNMOB, STSNMOB, TSNMOB 

# REMOVE
# --------------------------END MOBIL-----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------NFIX----------------------------------
# ADD
REAL SDWNOD
INTEGER YRDOY, YRSIM
INTEGER TIMDIF

# REMOVE
INTEGER LINC
INTEGER SDWNOD
# --------------------------END NFIX------------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------NUPTAK--------------------------------
# ADD
REAL BD(NL), FAC(NL)
REAL PGAVL, PRSPNH4, PRSPNO3, PTRNH4U, PTRNO3U, RNH4C, RNO3C
REAL NUPNH4(NL), NUPNO3(NL)
REAL TSNMOB
# REMOVE
REAL KG2PPM(NL)
# --------------------------END NUPTAK----------------------------
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# --------------------------PHENOL--------------------------------
# ADD
REAL TGROAV
REAL vstagp
# REMOVE
REAL PSENP(20)
REAL PStres2
REAL SeedFrac, VegFrac
# --------------------------END PHENOL----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------VSTAGES-------------------------------
# ADD
# REMOVE
# --------------------------END VSTAGES---------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------PODDET--------------------------------
# ADD
# REMOVE
INTEGER LINC
# --------------------------END PODDET----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------PODS----------------------------------
# ADD
INTEGER YRSIM
INTEGER RUN
# REMOVE
INTEGER LINC, YRPLT
REAL PStres2, CPSTRES
# --------------------------END PODS------------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------PODCOMP-------------------------------
# ADD
# REMOVE
INTEGER LINC
# --------------------------END PODCOMP---------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------RESPIR--------------------------------
# ADD
CHARACTER*1 MRSWITCH, TRSWITCH
CHARACTER*3 TRSTYP
REAL LFMRC, mft, RTMRC, SDMRC, SHELMRC, STMMRC, STRMRC
REAL TRST(4), WTNLF, WTNRT, WTNSD, WTNSH, WTNST, WTNSR
REAL CURV
# REMOVE
REAL SCLTS
# --------------------------END RESPIR----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------ROOTS---------------------------------
# ADD
CHARACTER*1 PLME
CHARACTER*30 FILEIO
REAL TRTDY
REAL CADRT, NADRT, SRCADDOT, SRNADDOT
REAL RNDOT(NL), RLNSEN(NL), TRLNSEN, TRLSEN, TRLGRW, SENRT(NL)
REAL SRMDOT, SRNDOT
# REMOVE
REAL RLV_WS(NL)
REAL TRLV_MIN, RLSENTOT, FACTOR, RTWTMIN
REAL TotRootMass, CumRootMass
# --------------------------END ROOTS-----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------INROOT--------------------------------
# ADD
CHARACTER*1 PLME
REAL RLDSM, RLRATIO, RTWT, TRLCAP, RLCAP(NL)
# REMOVE
# --------------------------END INROOT----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------RStages-------------------------------
# ADD
# REMOVE
REAL SeedFrac, VegFrac, VegTime
# --------------------------END RStages---------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------SDCOMP--------------------------------
# ADD
# REMOVE
# --------------------------END SDCOMP----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------VEGGR---------------------------------
# ADD
INTEGER TIMDIF, YRSIM
REAL AGRSTR, CADSR, CADSRF, CLAIT, CMOBSR, CMOBSRN
REAL CMOBSRX, CRUSSR, FNINSR, FNINSRG, FRSTR
REAL NADSR, NGRSR, NGRSRG, PPMFAC, PROSRG
REAL PROSRI, PROSRT, STRWT, WCRSR, WSRDOTN

REAL NADSH, NRUSTOT, PNMLF, PNMRT, PNMSH, PNMSR, PNMST
REAL CADRT, CADSH, RPRO

REAL LFSCMOB, RTSCMOB, SRSCMOB, STSCMOB, TSCMOB, TSNMOB
REAL ACMINELF, ACMINERT, ACMINESH, ACMINESR, ACMINEST
REAL CMINELF, CMINER, CMINERT, CMINESH, CMINESR, CMINEST
REAL LFCMINE, RTCMINE, SHCMINE, SRCMINE, STCMINE
REAL SLDOT, SRDOT, SSDOT, SSRDOT
REAL LFSNMOB, RTSNMOB, SRSNMOB, STSNMOB
REAL PROLFF, PRORTF, PROSRF, PROSTF
REAL WTNLF, WTNRT, WTNSR, WTNST



REAL NVSTL, NVSTR, NVSTS, NVSTSR, PCNL, PCNST, PCNRT, PCNSR
REAL CWST, MXWST, PWLF, PWST, PWRT, PWSR, RTWT
REAL PROLFR, PROSTR, PRORTR, PROSRR

INTEGER L, NLAYR
REAL AGRVGI, AGRVGPI, DNADRAT, NLKCOST, NLKGROW
REAL NRFRESP, PNTVG, NUPNH4(NL), NUPNO3(NL), ONDMOLD
REAL PNUPNH4,PNUPNO3, RFIXN, RNH4C, RNNU, RNO3C, TRNH4U
REAL TRNO3U, UNH4(NL), UNO3(NL), XTVEGM

REAL NLKCHK, TNLKCHK

REAL CDEBIT, LFCDEBT, LSTSR, RTCDEBT, SRCDEBT, STCDEBT

REAL ALPHL, ALPHR, ALPHS, ALPHSR
REAL PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF
REAL CADVG, CHORECOVER, NLKSPENT, NLKNUSED

REAL ANMINELF, ANMINERT, ANMINESR, ANMINEST, NRUSLF
REAL NRUSRT, NRUSSR, NRUSST, NSTFAC

REAL AAA, BBB, CCC, DDD,ZZZ, XXX
# REMOVE
INTEGER LINC
REAL PStres2, KCAN
# --------------------------END VEGGR-----------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# --------------------------NLKDIST-------------------------------
# ADD
REAL AGRLF, AGRRT, AGRSTM, AGRSTR, FNINL, FNINR
REAL FNINS, FNINSR, FRLF, FRRT, FRSTM, FRSTR
REAL NLEAK, PROLFI, PRORTI, PROSRI, PROSTI
REAL RNO3C, RNH4C, RPRO, TSNMOB
REAL ANMINELF, ANMINERT, ANMINEST, ANMINESR, NGRLF
REAL NGRRT, NGRST, NGRSR,NMINEA, NRUSLF, NRUSRT, NRUSSR
REAL NRUSST, TRNH4U, TRNO3U, TRNU, WLDOTN
REAL WRDOTN, WSDOTN, WSRDOTN
REAL CHORECOVER, NLKSPENT, NLKNUSED

REAL AGRVG, AGRVGI, AGRVGPI, ANMINETOT
REAL NLEAK2, NLKNG1
REAL NLKNG2, NLKNG3, NLKRTRN1, NLKRTRN2, NLKRTRN3
REAL OCH2OCOST, PNUPNH4, PNUPNO3, PNTVG, RNNU, TRNURTRN 

INTEGER L, NLAYR
REAL UNH4(NL), UNO3(NL)
# --------------------------END NLKDIST---------------------------
# ----------------------------------------------------------------
