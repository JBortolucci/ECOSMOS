#=======================================================================
#  COPYRIGHT 1998-2014 DSSAT Foundation
#                      University of Florida, Gainesville, Florida
#                      International Fertilizer Development Center
#                      Washington State University
#  ALL RIGHTS RESERVED
#=======================================================================
#=======================================================================
#  CROPGRO, Subroutine, G. Hoogenboom, J.W. Jones, K.J. Boote, C. Porter
#-----------------------------------------------------------------------
#  CROPGRO template plant growth subroutine.
#  Computes plant development and growth.
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/01/1990 GH  Written.
#  04/24/1994 NBP Changed TAIRHR to TGRO.
#  07/14/1995 JWH Modified to reserve some C for nodule growth
#  08/21/1995 GH  Added seed composition.
#  01/13/1999 CHP Adapted for modular system
#  05/11/1999 GH  Further adaptation for modular system
#  03/16/2000 GH  Further adaptation for modular system
#  09/17/2001 CHP Removed CANNAA, RO and RP from output list.
#  09/21/2001 CHP Added KCAN, PORMIN, RWUEP1, RWUMX to output list.
#  12/03/2001 GH  Changed name to CROPGRO to reflect template approach.
#  02/03/2003 CHP EORATIO and KEP now exported for ET routines.
#  06/19/2003 CHP Added KTRANS for export to transpiration routines instead of KEP.
#  07/08/2003 CHP Added KSEVAP for export to soil evaporation routines.
#  12/16/2004 KJB, LAH, CHP Revise harvest variables, call to HRes_CRGRO
#  02/21/2005 SJR Moved ISWWAT condition to ROOTS routine to allow computation
#                 of root senescence even when water not simulated.
#  10/24/2005 CHP Put weather variables in constructed variable. 
#  11/07/2005 CHP Replaced FAC with SOILPROP variable KG2PPM
#  06/06/2006 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
#  07/13/2006 CHP Added P model
#  06/11/2007 CHP PStres1 affects photosynthesis, PStres2 affects growth
#=======================================================================

CROPGRO <- function(CONTROL, ISWITCH, 
                    EOP, HARVFRAC, NH4, NO3, SOILPROP, SPi_AVAIL,   #Input
                    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           #Input
                    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, MDATE, #Output
                    NSTRES, PSTRES1,                                #Output
                    PUptake, PORMIN, RLV, RWUMX, SENESCE,           #Output
                    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI) {       #Output
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #TODO verificar essas strings ou opções Y/N que governam chamadas também
  #CHARACTER*1 DETACH, IDETO, ISWNIT, ISWSYM, ISWWAT, ISWDIS, ISWPHO, MEPHO, RNMODE
  #CHARACTER*2 CROP
  #CHARACTER*6 ECONO
  
  #     Transfer values from constructed data types into local variables.
  #TODO apenas para orientar a conexão com o padrão do ECOSMOS
  #CROP    = CONTROL % CROP
  #DYNAMIC = CONTROL % DYNAMIC
  #FILEIO  = CONTROL % FILEIO
  #YRDOY   = CONTROL % YRDOY
  #DAS     = CONTROL % DAS
  #RUN     = CONTROL % RUN
  #RNMODE  = CONTROL % RNMODE
  #
  #DLAYR  = SOILPROP % DLAYR  
  #DS     = SOILPROP % DS     
  #DUL    = SOILPROP % DUL    
  #KG2PPM = SOILPROP % KG2PPM  
  #LL     = SOILPROP % LL     
  #NLAYR  = SOILPROP % NLAYR  
  #SAT    = SOILPROP % SAT    
  #SLPF   = SOILPROP % SLPF  
  #WR     = SOILPROP % WR     
  #
  #IDETO  = ISWITCH % IDETO
  #ISWDIS = ISWITCH % ISWDIS
  #ISWNIT = ISWITCH % ISWNIT
  #ISWSYM = ISWITCH % ISWSYM
  #ISWPHO = ISWITCH % ISWPHO
  #ISWWAT = ISWITCH % ISWWAT
  #MEPHO  = ISWITCH % MEPHO
  #
  #CO2    = WEATHER % CO2   
  #DAYL   = WEATHER % DAYL  
  #PAR    = WEATHER % PAR   
  #TAVG   = WEATHER % TAVG  
  #TDAY   = WEATHER % TDAY  
  #TGRO   = WEATHER % TGRO  
  #TGROAV = WEATHER % TGROAV
  #TMIN   = WEATHER % TMIN  
  
  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  #TODO mover as coisas do RUNINIT para o SEASINIT
  if (DYNAMIC == RUNINIT) {
    #-----------------------------------------------------------------------
    #     Call input routine for CROPGRO module parameters
    #-----------------------------------------------------------------------
    KTRANS = KEP
    KSEVAP = -99.   #Defaults to old method of light
    #  extinction calculation for soil evap.
    
    if (CROP != 'FA' & MEPHO == 'C') {
      PHOTO(CONTROL, 
            BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
            NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
            SWFAC, TDAY, XHLAI, XPOD,                       #Input
            AGEFAC, PG)                                     #Output
    }
    
    #-----------------------------------------------------------------------
    #TODO usando a 'PHENOL' como exemplo para limpeza/organização dos argumentos da função
    PHENOL (iyear, iyear0, jday, DAS,DYNAMIC) 
    
    #-----------------------------------------------------------------------
    #TODO comentar as subrotinas que não usaremos, como pestes, fósforo (P) e outputs, por hora
    # Todavia, acho válido deixar aqui para futuras implementações, caso necessário/desejável pelo ECOSMOS
    if (ISWDIS == 'Y') {
      PEST(CONTROL, ISWITCH, 
           AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    #Input
           PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      #Input
           SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       #Input
           RLV, SDNO, SHELN, SWIDOT,                       #Input/Output
           VSTAGE, WSHIDT, WTSD, WTSHE,                    #Input/Output
           ASMDOT, DISLA, NPLTD, PPLTD,                    #Output
           SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  #Output
    }
    
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      DEMAND(RUNINIT, CONTROL,
             AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
             FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
             LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
             PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
             SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
             TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
             WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
             WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
             AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, Fnew, FNINL,  #Output
             FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
             GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
             NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
             POTLIP, SDGR, TURADD, XFRT, YREND)                #Output
      
      #-----------------------------------------------------------------------
      #    Call plant COMPosition INitialization (for data input)
      #-----------------------------------------------------------------------
      INCOMP(RUNINIT, 
             FILECC, FILEIO, FRLF, FRRT, FRSTM,              #Input
             AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   #Output
             AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR)          #Output
      
      #-----------------------------------------------------------------------
      NUPTAK(RUNINIT,
             DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,#Input
             NH4, NO3, NLAYR, RLV, SAT, SW,                 #Input
             TRNH4U, TRNO3U, TRNU, UNH4, UNO3)              #Output
      
      #-----------------------------------------------------------------------
      if (ISWSYM == 'Y') {
        NFIX(RUNINIT,
             AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,         #Input
             FILECC, FILEIO, NLAYR, NR7, PLTPOP,           #Input
             SAT, ST, SW, TURFAC,                          #Input
             CNOD, DWNOD, DWNODA, NDTH, NFIXN,             #Output
             NODGR, WTNFX, SENNOD)                         #Output
      }
      
      #-----------------------------------------------------------------------
      PODS(RUNINIT, 
           AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
           FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
           GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
           NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
           PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
           TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
           PStres2, YRPLT,                                 #Input
           AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
           PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
           SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
           WTSHE, WTSHMT, FLWN)                            #Output
      
      #-----------------------------------------------------------------------
      if (DETACH == 'Y') {
        PODDET(
          FILECC, TGRO, WTLF, YRDOY, YRNR2,             #Input
          PODWTD, SDNO, SHELN, SWIDOT,                  #Output
          WSHIDT, WTSD, WTSHE,                          #Output
          RUNINIT)                                      #Control
      }
      
      #-----------------------------------------------------------------------
      VEGGR (RUNINIT, 
             AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #Input
             DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #Input
             FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #Input
             NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #Input
             PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #Input
             TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #Input
             WCRST, WTLF, XLAI, YRDOY, YREMRG,               #Input
             AGRVG, FRLF, FRRT, FRSTM,                       #I/O
             CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #Output
             CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #Output
             NADST, NGRLF, NGRRT, NGRST, NSTRES,             #Output
             TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 #Output
      
      #-----------------------------------------------------------------------
      #     Call leaf senescence routine for initialization
      #-----------------------------------------------------------------------
      SENES(RUNINIT, 
            FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       #Input
            RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  #Input
            SLDOT, SLNDOT, SSDOT, SSNDOT)                   #Output
      
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS(RUNINIT,
            AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #Input
            ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #Input
            SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #Input
            RLV, RTDEP, SATFAC, SENRT, SRDOT)               #Output
    }
    
    #-----------------------------------------------------------------------
    GROW(CONTROL, ISWITCH, RUNINIT, SOILPROP, 
         AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #Input
         CRUSST, DISLA, Fnew, FILECC, FRLF, FRSTM,            #Input
         NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #Input
         NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #Input
         NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #Input
         POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #Input
         SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #Input
         SSNDOT, TRNH4U, TRNO3U, TRNU,                     #Input
         TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #Input
         WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #Input
         MDATE, YRPLT,                                     #Input
         SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             #Input/Output
         AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    #Output
         DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      #Output
         PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         #Output
         PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   #Output
         PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         #Output
         ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                #Output
         SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             #Output
         SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  #Output
         WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          #Output
         WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  #Output
         WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       #Output
         WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        #Output
         WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      #Output
         WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      #Output
         ShutMob, RootMob, ShelMob)                        #Output
    
    OPGROW(CONTROL, ISWITCH, SoilProp, 
           CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
           GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
           PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
           PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
           RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
           SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
           TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
           WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 
    
    #     Initialize Overview.out file.
    OPHARV(CONTROL, ISWITCH, 
           AGEFAC, CANHT, CANNAA, CANWAA, CROP,            #Input
           HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   #Input
           PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   #Input
           SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           #Input
           VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     #Input
           XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      #Input
           YRNR7, YRPLT,                                   #Input
           SDWTAH)                                         #Output
    
    #     If this is not a sequenced run, dont use any previously calculated
    #       harvest residue.
    #TODO checar esse 'index'
    if (RUN == 1 | INDEX('QF',RNMODE) <= 0) {
      #HARVRES % RESWT  = 0.0
      #HARVRES % RESLIG = 0.0
      #HARVRES % RESE   = 0.0
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  } else if (DYNAMIC == SEASINIT) {
    #***********************************************************************
    #     The following statements came from INPLNT
    #-----------------------------------------------------------------------
    CMINEP = 0.0
    CNOD   = 0.0
    CNODMN = 0.0
    CTONOD = 0.0
    MAINR  = 0.0
    NAVL   = 0.0
    PGAVL  = 0.0
    RO     = 0.0
    RP     = 0.0
    RPROAV = RFIXN
    
    TURFAC = 1.0
    SWFAC  = 1.0
    
    RSPNO3 = 0.0
    RSPNH4 = 0.0
    
    KSTRES = 1.0
    
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      if (MEPHO == 'L') {
        #Retrieve AGEFAC and PG from ETPHOT routine.
        # call's comentados
        #CALL GET('SPAM', 'AGEFAC', AGEFAC)
        #CALL GET('SPAM', 'PG'    , PG)
      } else if (MEPHO == 'C') {
        PHOTO(CONTROL, 
              BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
              NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
              SWFAC, TDAY, XHLAI, XPOD,                       #Input
              AGEFAC, PG)                                     #Output
      }
    }
    
    #-----------------------------------------------------------------------
    PHENOL(CONTROL, ISWITCH, 
           DAYL, NSTRES, PStres2, SOILPROP, ST,            #Input
           SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     #Input
           DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        #Output
           NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       #Output
           RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        #Output
           TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         #Output
           YRNR2, YRNR3, YRNR5, YRNR7)                     #Output
    
    #-----------------------------------------------------------------------
    #     Initialize pest coupling point and damage variables
    #     Need to initialize even if pests are not being modelled this
    #     season - zero out values from last simulation.
    #-----------------------------------------------------------------------
    PEST(CONTROL, ISWITCH, 
         AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    #Input
         PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      #Input
         SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       #Input
         RLV, SDNO, SHELN, SWIDOT,                       #Input/Output
         VSTAGE, WSHIDT, WTSD, WTSHE,                    #Input/Output
         ASMDOT, DISLA, NPLTD, PPLTD,                    #Output
         SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  #Output
    
    #-----------------------------------------------------------------------
    #     Initialization call to DEMAND must preceed initialization calls
    #         to INCOMP and GROW (need to initialize values of F, FRLF,
    #         FRRT, and FRSTM for use in those routines)  chp 9/22/98
    #-----------------------------------------------------------------------
    DEMAND(SEASINIT, CONTROL,
           AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
           FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
           LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
           PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
           SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
           TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
           WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
           WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
           AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, Fnew, FNINL,  #Output
           FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
           GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
           NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
           POTLIP, SDGR, TURADD, XFRT, YREND)                #Output
    
    #-----------------------------------------------------------------------
    #     Call plant COMPosition INitialization
    #     This call must preceed initialization call to GROW (need to
    #         initialize value of SDPROR for use in that routine) chp 9/22/98
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      INCOMP(SEASINIT, 
             FILECC, FILEIO, FRLF, FRRT, FRSTM,              #Input
             AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   #Output
             AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR)          #Output
    }
    
    #-----------------------------------------------------------------------
    GROW(CONTROL, ISWITCH, SEASINIT, SOILPROP, 
         AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #Input
         CRUSST, DISLA, Fnew, FILECC, FRLF, FRSTM,            #Input
         NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #Input
         NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #Input
         NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #Input
         POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #Input
         SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #Input
         SSNDOT, TRNH4U, TRNO3U, TRNU,                     #Input
         TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #Input
         WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #Input
         MDATE, YRPLT,                                     #Input
         SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             #Input/Output
         AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    #Output
         DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      #Output
         PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         #Output
         PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   #Output
         PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         #Output
         ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                #Output
         SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             #Output
         SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  #Output
         WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          #Output
         WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  #Output
         WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       #Output
         WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        #Output
         WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      #Output
         WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      #Output
         ShutMob, RootMob, ShelMob)                        #Output
    
    #-----------------------------------------------------------------------
    NUPTAK(SEASINIT, 
           DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT, #Input
           NH4, NO3, NLAYR, RLV, SAT, SW,                  #Input
           TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               #Output
    
    #     Plant phosphorus module initialization
    #P_CGRO (DYNAMIC, ISWITCH, 
    #         CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       #Input
    #         RootMob, RTDEP, RTWT, SDWT, SeedFrac,           #Input
    #         ShelMob, SHELWT, ShutMob, SOILPROP,             #Input
    #         SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      #Input
    #         WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            #Input
    #         SENESCE,                                        #I/O
    #         PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, #Output
    #         PStres1, PStres2, PUptake, FracRts)             #Output
    
    #-----------------------------------------------------------------------
    MOBIL(SEASINIT,
          NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              #Input
          WNRLF, WNRRT, WNRSH, WNRST,                     #Input
          NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST)         #Output
    
    #-----------------------------------------------------------------------
    NFIX(SEASINIT,
         AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           #Input
         FILECC, FILEIO, NLAYR, NR7, PLTPOP,             #Input
         SAT, ST, SW, TURFAC,                            #Input
         CNOD, DWNOD, DWNODA, NDTH, NFIXN,               #Output
         NODGR, WTNFX, SENNOD)                           #Output
    
    #-----------------------------------------------------------------------
    PODS(SEASINIT, 
         AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
         FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
         GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
         NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
         PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
         TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
         PStres2, YRPLT,                                 #Input
         AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
         PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
         SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
         WTSHE, WTSHMT, FLWN)                            #Output
    
    #-----------------------------------------------------------------------
    VEGGR (SEASINIT, 
           AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #Input
           DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #Input
           FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #Input
           NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #Input
           PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #Input
           TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #Input
           WCRST, WTLF, XLAI, YRDOY, YREMRG,               #Input
           AGRVG, FRLF, FRRT, FRSTM,                       #I/O
           CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #Output
           CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #Output
           NADST, NGRLF, NGRRT, NGRST, NSTRES,             #Output
           TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 #Output
    
    #-----------------------------------------------------------------------
    #     Call leaf senescence routine for initialization
    #-----------------------------------------------------------------------
    SENES(SEASINIT, 
          FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       #Input
          RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  #Input
          SLDOT, SLNDOT, SSDOT, SSNDOT)                   #Output
    
    #-----------------------------------------------------------------------
    #     Call to root growth and rooting depth routine
    #-----------------------------------------------------------------------
    ROOTS(SEASINIT,
          AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #Input
          ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #Input
          SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #Input
          RLV, RTDEP, SATFAC, SENRT, SRDOT)               #Output
    
    #-----------------------------------------------------------------------
    #     Write headings to output file GROWTH.OUT
    #-----------------------------------------------------------------------
    OPGROW(CONTROL, ISWITCH, SoilProp,  
           CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
           GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
           PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
           PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
           RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
           SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
           TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
           WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 
    
    OPHARV (CONTROL, ISWITCH, 
            AGEFAC, CANHT, CANNAA, CANWAA, CROP,            #Input
            HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   #Input
            PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   #Input
            SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           #Input
            VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     #Input
            XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      #Input
            YRNR7, YRPLT,                                   #Input
            SDWTAH)                                         #Output
    
    # Zero the value of HARVRES composite variable here 
    #     NOTE: At this time, the variable has already been used to 
    #     initialize soil properties for this season.
    #TODO ver essa questão aqui
    #HARVRES % RESWT  = 0.0
    #HARVRES % RESLIG = 0.0
    #HARVRES % RESE   = 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE CALCULATIONS
    #***********************************************************************
  } else if (DYNAMIC == RATE) {
    #***********************************************************************
    if (YRDOY > YREMRG & YREMRG > 0 & ISWWAT == 'Y') {
      #       Calculate daily water stess factors (from SWFACS)
      #       EOP in mm/d
      #       TRWUP and EP1 in cm/d
      SWFAC  = 1.0
      TURFAC = 1.0
      if (EOP > 0.001) {
        EP1 = EOP * 0.1
        if (TRWUP / EP1 < RWUEP1) {
          TURFAC = (1./RWUEP1) * TRWUP / EP1
        }
        if (EP1 >= TRWUP) {
          SWFAC = TRWUP / EP1
        }
      }
    }
    
    #-----------------------------------------------------------------------
    #     CALL vegetative and reproductive development subroutine
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      PHENOL(CONTROL, ISWITCH, 
             DAYL, NSTRES, PStres2, SOILPROP, ST,            #Input
             SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     #Input
             DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        #Output
             NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       #Output
             RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        #Output
             TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         #Output
             YRNR2, YRNR3, YRNR5, YRNR7)                     #Output
    }
    
    #----------------------------------------------------------------------
    if (ISWDIS == 'Y') {
      PEST(CONTROL, ISWITCH, 
           AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    #Input
           PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      #Input
           SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       #Input
           RLV, SDNO, SHELN, SWIDOT,                       #Input/Output
           VSTAGE, WSHIDT, WTSD, WTSHE,                    #Input/Output
           ASMDOT, DISLA, NPLTD, PPLTD,                    #Output
           SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             #Output
    }
    
    if (CROP != 'FA' & DAS > NVEG0) {
      if (MEPHO == 'L') {
        #TODO usar PHOTO.R ou trazer AGEFAC and PG do DSSAT/Fortran
        #Retrieve AGEFAC and PG from ETPHOT routine.
        #CALL GET('SPAM', 'AGEFAC', AGEFAC)
        #CALL GET('SPAM', 'PG'    , PG)
      } else if (MEPHO == 'C') {
        PHOTO(CONTROL, 
              BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
              NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
              SWFAC, TDAY, XHLAI, XPOD,                       #Input
              AGEFAC, PG)                                     #Output
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY INTEGRATION 
    #***********************************************************************
  } else if (DYNAMIC == INTEGR & CROP != 'FA') {
    #***********************************************************************
    #-----------------------------------------------------------------------
    #     Move PHENOL integration up here.
    #     Need to set NVEG0 before test for DAS = NVEG0, otherwise,
    #     initialization on day of emergence will never occur.
    #-----------------------------------------------------------------------
    PHENOL(CONTROL, ISWITCH, 
           DAYL, NSTRES, PStres2, SOILPROP, ST,            #Input
           SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     #Input
           DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        #Output
           NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       #Output
           RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        #Output
           TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         #Output
           YRNR2, YRNR3, YRNR5, YRNR7)                     #Output
    
    #-----------------------------------------------------------------------
    if (DAS == NVEG0) {
      #----------------------------------------------------------------------
      #     On day of emergence, initialize:
      #-----------------------------------------------------------------------
      GROW(CONTROL, ISWITCH, EMERG, SOILPROP, 
           AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #Input
           CRUSST, DISLA, Fnew, FILECC, FRLF, FRSTM,            #Input
           NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #Input
           NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #Input
           NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #Input
           POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #Input
           SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #Input
           SSNDOT, TRNH4U, TRNO3U, TRNU,                     #Input
           TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #Input
           WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #Input
           MDATE, YRPLT,                                     #Input
           SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             #Input/Output
           AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    #Output
           DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      #Output
           PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         #Output
           PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   #Output
           PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         #Output
           ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                #Output
           SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             #Output
           SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  #Output
           WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          #Output
           WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  #Output
           WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       #Output
           WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        #Output
           WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      #Output
           WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      #Output
           ShutMob, RootMob, ShelMob)                        #Output
      
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS(EMERG,
            AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #Input
            ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #Input
            SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #Input
            RLV, RTDEP, SATFAC, SENRT, SRDOT)               #Output
      
      #-----------------------------------------------------------------------
      #       DYNAMIC = EMERG (not INTEGR) here
      DEMAND(EMERG, CONTROL, 
             AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
             FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
             LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
             PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
             SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
             TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
             WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
             WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
             AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, Fnew, FNINL,  #Output
             FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
             GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
             NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
             POTLIP, SDGR, TURADD, XFRT, YREND)                #Output
      
      #-----------------------------------------------------------------------
      PODS(EMERG, 
           AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
           FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
           GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
           NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
           PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
           TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
           PStres2, YRPLT,                                 #Input
           AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
           PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
           SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
           WTSHE, WTSHMT, FLWN)                            #Output
      
      #-----------------------------------------------------------------------
      VEGGR(EMERG, 
            AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #Input
            DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #Input
            FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #Input
            NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #Input
            PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #Input
            TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #Input
            WCRST, WTLF, XLAI, YRDOY, YREMRG,               #Input
            AGRVG, FRLF, FRRT, FRSTM,                       #I/O
            CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #Output
            CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #Output
            NADST, NGRLF, NGRRT, NGRST, NSTRES,             #Output
            TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 #Output
      
      if (ISWPHO == 'Y' | ISWPHO == 'H') {
        #       Plant phosphorus module initialization at plant emergence
        P_CGRO (EMERG, ISWITCH, 
                CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       #Input
                RootMob, RTDEP, RTWT, SDWT, SeedFrac,           #Input
                ShelMob, SHELWT, ShutMob, SOILPROP,             #Input
                SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      #Input
                WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            #Input
                SENESCE,                                        #I/O
                PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, #Output
                PStres1, PStres2, PUptake, FracRts)             #Output
      }
      
      #-----------------------------------------------------------------------
    }
    #----------------------------------------------------------------------
    if (DETACH == 'Y' & DAS <= NVEG0+1) {
      PODDET(
        FILECC, TGRO, WTLF, YRDOY, YRNR2,               #Input
        PODWTD, SDNO, SHELN, SWIDOT,                    #Output
        WSHIDT, WTSD, WTSHE,                            #Output
        EMERG)                                          #Control
    }
    
    #***********************************************************************
    #     Skip growth processes and N and C balances before plants emerge
    #-----------------------------------------------------------------------
    if (DAS >= NVEG0) {
      #-----------------------------------------------------------------------
      #     Initialize available N and C for beginning of daily calcs.
      #-----------------------------------------------------------------------
      NAVL = 0.0
      PGAVL = 0.0
      
      #-----------------------------------------------------------------------
      #    Initialize variables that represent N and C availability during a day
      #    Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
      #    PGAVL is the total available CH2O available for growth & respiration
      #
      #    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
      #    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
      #    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
      #
      #-----------------------------------------------------------------------
      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)
      PGAVL = PG + CMINEP
      #-----------------------------------------------------------------------
      #       Compute maintenance respiration and subtract from available CH2O
      #-----------------------------------------------------------------------
      RESPIR(
        PG, R30C2, RES30C, TGRO, WTMAIN,                #Input
        RO, RP,                                         #Input/Output
        MAINR)                                          #Output
      
      if (MAINR > PGAVL) {
        PGAVL  = 0.0
      } else {
        PGAVL = PGAVL - MAINR
      }
      #-----------------------------------------------------------------------
      #     Reduce PGAVL if pest damage occurs to C assimilation
      #     Moved to PEST module - chp
      #-----------------------------------------------------------------------
      #     Call PEST Module for INTEGRATION calculations
      #-----------------------------------------------------------------------
      if (ISWDIS=='Y') {
        PEST(CONTROL, ISWITCH, 
             AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    #Input
             PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      #Input
             SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       #Input
             RLV, SDNO, SHELN, SWIDOT,                       #Input/Output
             VSTAGE, WSHIDT, WTSD, WTSHE,                    #Input/Output
             ASMDOT, DISLA, NPLTD, PPLTD,                    #Output
             SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  #Output
        
        if (ASMDOT > 1.E-4) {
          PGAVL = PGAVL - ASMDOT
          PGAVL = max(0.0,PGAVL)
        }
      }
      
      #-----------------------------------------------------------------------
      #    Call Subroutine to calculate Nitrogen and Carbon Demand for new growth
      #-----------------------------------------------------------------------
      DEMAND(INTEGR, CONTROL, 
             AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #Input
             FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #Input
             LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #Input
             PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #Input
             SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #Input
             TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #Input
             WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #Input
             WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #Input
             AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, Fnew, FNINL,  #Output
             FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #Output
             GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #Output
             NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #Output
             POTLIP, SDGR, TURADD, XFRT, YREND)                #Output
      
      if (YRDOY == YREND) RETURN
      
      #-----------------------------------------------------------------------
      #    Compute N Available From Seed, During Early Growth
      #     chp - this takes much longer than 7 ptd to deplete seed N.
      #-----------------------------------------------------------------------
      if (SDNPL > 0.0001) {
        NAVL = max(SDNPL * DTX / 7. , 0.0)
        SDNPL = SDNPL - NAVL
      } else {
        SDNPL = 0.0
        NAVL = 0.0
      }
      
      #-----------------------------------------------------------------------
      #    If ISWNIT = Y - Call soil N routines. Balance Available C and N
      #    If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
      #-----------------------------------------------------------------------
      if (ISWNIT == 'Y') {
        NUPTAK(INTEGR, 
               DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT, #Input
               NH4, NO3, NLAYR, RLV, SAT, SW,                  #Input
               TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               #Output
        
        #-----------------------------------------------------------------------
        #    Account for C Used to reduce N Uptake to protein
        #-----------------------------------------------------------------------
        RSPNO3 = TRNO3U/0.16 * RNO3C
        RSPNH4 = TRNH4U/0.16 * RNH4C
        if (PGAVL < (RSPNO3+RSPNH4)) {
          PGAVL = 0.0
        } else {
          PGAVL = PGAVL - (RSPNO3 + RSPNH4)
        }
        #-----------------------------------------------------------------------
        #       Accumulate nitrogen for today's growth, NAVL
        #-----------------------------------------------------------------------
        NAVL = NAVL + TRNU
      }
      #-----------------------------------------------------------------------
      #    CALL Nitrogen mobilization subroutine
      #    to compute availability of N from other tissue (NMINEA)
      #-----------------------------------------------------------------------
      MOBIL(INTEGR,
            NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              #Input
            WNRLF, WNRRT, WNRSH, WNRST,                     #Input
            NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST)         #Output
      
      #-----------------------------------------------------------------------
      #       Plant phosphorus module
      #-----------------------------------------------------------------------
      if (ISWPHO == 'Y' | ISWPHO == 'H') {
        P_CGRO (DYNAMIC, ISWITCH, 
                CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       #Input
                RootMob, RTDEP, RTWT, SDWT, SeedFrac,           #Input
                ShelMob, SHELWT, ShutMob, SOILPROP,             #Input
                SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      #Input
                WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            #Input
                SENESCE,                                        #I/O
                PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, #Output
                PStres1, PStres2, PUptake, FracRts)             #Output
      }
      
      #-----------------------------------------------------------------------
      #    Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
      #-----------------------------------------------------------------------
      if (PGAVL > NMINEA/0.16*RPRO) {
        PGAVL = PGAVL - NMINEA/0.16*RPRO
      } else {
        PGAVL = 0.0
      }
      NAVL   = NAVL + NMINEA
      #-----------------------------------------------------------------------
      #     Allow some of today's PG to be used for N fixation, depending
      #     on N uptake and mining, and on demand for N.
      #     NAVLV = N available for veg growth from uptake and mining
      #     CAVVEG = C available for veg growth
      #     NDMVEG = N required for veg growth if all PGAVL is used as computed
      #     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
      #     PROVEG = average protein composition of growing tissue today
      #     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
      #-----------------------------------------------------------------------
      CTONODR = max(0.0, (NDMREP-NAVL)*RFIXN/0.16)
      CTONODR = min(CTONODR,PGAVL)
      CTONOD = 0.0
      CAVVEG = max(0.,(PGAVL - CDMREP))
      NAVLV = max(0.,(NAVL-NDMREP))
      CNDFX = max(0.,(RFIXN/0.16)*(NDMVEG-NAVLV))
      if (CAVVEG > 1.E-4 & CNDFX > 1.E-4) {
        PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
        CTONOD = CAVVEG - (CAVVEG + (NAVLV*RFIXN/0.16))*AGRVG/(AGRVG+PROVEG*RFIXN)
      }
      
      #-----------------------------------------------------------------------
      #     Reserve for nodule growth an amount of C equivalent to a fixed
      #     fraction (FRCNOD) of C allocated to root growth.  JWH 7/11/95
      #-----------------------------------------------------------------------
      if (DAS < NR2) {
        CNODMN = CAVVEG * FRRT * FRCNOD
      } else {
        CNODMN = 0.0
      }
      CTONOD = min(CNODMN + max(0.0, CTONOD), CAVVEG) + CTONODR
      
      #-----------------------------------------------------------------------
      #     Call nitrogen fixation routine if ISWSYM is set to Y
      #     and if thermal time exceeds the lag phase for n-fixation
      #-----------------------------------------------------------------------
      if (ISWNIT == 'Y' & ISWSYM == 'Y') {
        if (VSTAGE > TTFIX) {
          NFIX(INTEGR, 
               AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           #Input
               FILECC, FILEIO, NLAYR, NR7, PLTPOP,             #Input
               SAT, ST, SW, TURFAC,                            #Input
               CNOD, DWNOD, DWNODA, NDTH, NFIXN,               #Output
               NODGR, WTNFX, SENNOD)                           #Output
        }
      }
      #-----------------------------------------------------------------------
      #       If ISWSYM = U, then N-FIXATION is assumed to occur at a rate
      #       that carbon will allow, and nodules are not grown explicitely
      #-----------------------------------------------------------------------
      if ((ISWNIT == 'Y') & (ISWSYM == 'U') | (ISWNIT != 'Y')) {
        NFIXN = max(0.0,NDMREP + NDMVEG - NAVL)
        CNOD = RFIXN * NFIXN/0.16
      }
      #-----------------------------------------------------------------------
      #    Accumulate NAVL for growth, reduce PGAVL by cost to fix N
      #-----------------------------------------------------------------------
      if (PGAVL > CNOD) {
        PGAVL = PGAVL - CNOD
      } else {
        PGAVL = 0.0
      }
      NAVL = NAVL + NFIXN
      #-----------------------------------------------------------------------
      #     Call routine to compute actual seed and shell growth
      #-----------------------------------------------------------------------
      PODS(INTEGR, 
           AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
           FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
           GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
           NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
           PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
           TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
           PStres2, YRPLT,                                 #Input
           AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
           PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
           SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
           WTSHE, WTSHMT, FLWN)                            #Output
      
      #-----------------------------------------------------------------------
      #     Call specific routines for peanut to determine
      #         Seed size
      #         Pod color
      #         Pod Detachment
      #-----------------------------------------------------------------------
      if (DETACH == 'Y' & DAS >= NR1) {
        PODDET(
          FILECC, TGRO, WTLF, YRDOY, YRNR2,               #Input
          PODWTD, SDNO, SHELN, SWIDOT,                    #Output
          WSHIDT, WTSD, WTSHE,                            #Output
          INTEGR)                                         #Control
      }
      
      #-----------------------------------------------------------------------
      #     Compute carbon required for seed (CGRSD) and shell (CGRSH) growth
      #-----------------------------------------------------------------------
      CGRSD = WSDDTN * AGRSD3
      CGRSH = WSHDTN * AGRSH1
      #-----------------------------------------------------------------------
      #     Reduce PGAVL by C used for seed growth
      #     Also reduce NAVL by N used for seed growth
      #-----------------------------------------------------------------------
      if (PGAVL > (CGRSD + CGRSH)) {
        PGAVL = PGAVL - CGRSD - CGRSH
      } else {
        PGAVL = 0.0
      }
      NAVL   = NAVL - (NGRSD + NGRSH)
      PGAVL  = max(0.0,PGAVL)
      NAVL   = max(0.0,NAVL)
      
      #-----------------------------------------------------------------------
      #     CSAVEV is a faction of PG for vegetative growth that is stored
      #     as CH2O.  Increase this as plant moves from R1 into seed fill.
      #  These two statements came from the VEGGR subroutine - chp
      #-----------------------------------------------------------------------
      CSAVEV = CADPR1 * PGAVL * FRACDN
      PGAVL = PGAVL - CSAVEV
      
      #-----------------------------------------------------------------------
      #     Call routine to compute actual vegetative growth, C to mine or add
      #-----------------------------------------------------------------------
      VEGGR(INTEGR, 
            AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #Input
            DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #Input
            FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #Input
            NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #Input
            PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #Input
            TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #Input
            WCRST, WTLF, XLAI, YRDOY, YREMRG,               #Input
            AGRVG, FRLF, FRRT, FRSTM,                       #I/O
            CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #Output
            CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #Output
            NADST, NGRLF, NGRRT, NGRST, NSTRES,             #Output
            TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 #Output
      
      #-----------------------------------------------------------------------
      #     Compute C required for LF, ST, and RT growth, and remaining C and N
      #-----------------------------------------------------------------------
      PGAVL = PGAVL - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
      NAVL = NAVL - (NGRLF + NGRST + NGRRT)
      NAVL = NAVL - (NADLF + NADST + NADRT)
      PGAVL = PGAVL - (CADST + CADLF) * PCH2O
      #-----------------------------------------------------------------------
      #     Call leaf senescence routine to compute leaf loss variables
      #-----------------------------------------------------------------------
      SENES(INTEGR, 
            FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       #Input
            RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  #Input
            SLDOT, SLNDOT, SSDOT, SSNDOT)                   #Output
      
      #-----------------------------------------------------------------------
      #     Call freeze damage routine if TMIN is less than FREEZ1 deg C
      #-----------------------------------------------------------------------
      if (TMIN < FREEZ1) {
        FREEZE(
          FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,           #Input
          TMIN, WTLF, YRDOY,  YRPLT,                      #Input
          MDATE,                                          #Input/Output
          WLFDOT)                                         #Output
      } else {
        WLFDOT = 0.0
      }
      #-----------------------------------------------------------------------
      #     Call to root growth and rooting depth routine
      #-----------------------------------------------------------------------
      ROOTS(INTEGR,
            AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #Input
            ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #Input
            SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #Input
            RLV, RTDEP, SATFAC, SENRT, SRDOT)               #Output
      
      #-----------------------------------------------------------------------
      #     Compute total C cost for growing seed, shell, and vegetative tissue
      #     for tomorrow's potential growth calculations
      #-----------------------------------------------------------------------
      #       Calculate the respiration required for seed, shell, and veg tissue
      #       depending on the source of N uptake
      #-----------------------------------------------------------------------
      if ((TRNU + NFIXN + NMINEA) > 1.E-4) {
        RPROAV = ((RSPNO3 + RSPNH4) * 0.16 + NFIXN * RFIXN + NMINEA * RPRO) / (TRNU + NFIXN + NMINEA)
      } else {
        RPROAV = (RNO3C + RNH4C) / 2.
      }
      #-----------------------------------------------------------------------
      #     AGRSD2 = SDPRO*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
      #    &         + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
      #-----------------------------------------------------------------------
      AGRSD2 = FNINSD*6.25*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA   +  POTLIP*RLIP + POTCAR*RCH2O
      AGRSH2 =  PROSHI*RPROAV + PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA + PMINSH*RMIN + PCARSH*RCH2O
      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RPROAV
      #-----------------------------------------------------------------------
      #     Call routine to integrate growth and damage
      #-----------------------------------------------------------------------
      GROW(CONTROL, ISWITCH, INTEGR, SOILPROP, 
           AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #Input
           CRUSST, DISLA, Fnew, FILECC, FRLF, FRSTM,            #Input
           NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #Input
           NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #Input
           NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #Input
           POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #Input
           SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #Input
           SSNDOT, TRNH4U, TRNO3U, TRNU,                     #Input
           TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #Input
           WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #Input
           MDATE, YRPLT,                                     #Input
           SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             #Input/Output
           AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    #Output
           DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      #Output
           PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         #Output
           PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   #Output
           PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         #Output
           ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                #Output
           SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             #Output
           SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  #Output
           WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          #Output
           WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  #Output
           WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       #Output
           WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        #Output
           WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      #Output
           WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      #Output
           ShutMob, RootMob, ShelMob)                        #Output
      
      
      if ((WTLF+STMWT)> 0.0001) {
        PCNVEG = (WTNLF+WTNST)/(WTLF+STMWT)*100.
      } else {
        #       #PCNVEG = -99.    #Wait for GBuild fix for -99''s
        PCNVEG = 0.
      }
      
      #-----------------------------------------------------------------------
      #     End of DAS > NVEG0 if construct
      #-----------------------------------------------------------------------
    }
    #-----------------------------------------------------------------------
    
    #***********************************************************************
    #***********************************************************************
    #-----------------------------------------------------------------------
    #     OUTPUT / SEASEND section
    #-----------------------------------------------------------------------
    #TODO ver necessidade do 'output' e 'seasend'
  } else if (DYNAMIC == OUTPUT | DYNAMIC == SEASEND) {
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      if (YRDOY == YREND & DYNAMIC == OUTPUT) {
        STGDOY(16) = YREND
      }
      
      if (ISWDIS=='Y') {
        PEST(CONTROL, ISWITCH, 
             AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    #Input
             PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      #Input
             SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       #Input
             RLV, SDNO, SHELN, SWIDOT,                       #I/O
             VSTAGE, WSHIDT, WTSD, WTSHE,                    #I/O
             ASMDOT, DISLA, NPLTD, PPLTD,                    #Output
             SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             #Output
      }
      
      PODS(DYNAMIC, 
           AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
           FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
           GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
           NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
           PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
           TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
           PStres2, YRPLT,                                 #Input
           AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
           PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
           SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
           WTSHE, WTSHMT, FLWN)                            #Output
      
      OPGROW(CONTROL, ISWITCH, SoilProp, 
             CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
             GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
             PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
             PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
             RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
             SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
             TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
             WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 
      
      if (ISWPHO == 'Y' | ISWPHO == 'H') {
        P_CGRO (DYNAMIC, ISWITCH, 
                CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       #Input
                RootMob, RTDEP, RTWT, SDWT, SeedFrac,           #Input
                ShelMob, SHELWT, ShutMob, SOILPROP,             #Input
                SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      #Input
                WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            #Input
                SENESCE,                                        #I/O
                PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, #Output
                PStres1, PStres2, PUptake, FracRts)             #Output
      }
    }
    
    #     Write to Overview.out and summary.out files.
    OPHARV (CONTROL, ISWITCH, 
            AGEFAC, CANHT, CANNAA, CANWAA, CROP,            #Input
            HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   #Input
            PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   #Input
            SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           #Input
            VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     #Input
            XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      #Input
            YRNR7, YRPLT,                                   #Input
            SDWTAH)                                         #Output
    
    #     Call PlantNBal only for seasonal output.
    if (DYNAMIC == SEASEND) {
      if (CROP != 'FA') {
        PlantNBal (CONTROL, ISWITCH, 
                   SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,     #Input
                   WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,      #Input
                   WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,    #Input
                   WTNSHO, WTNSO, WTNST, WTNUP)                    #Input
      }
      #-----------------------------------------------------------------------
      #     Calculate harvest residue left in field
      HRes_CGRO(CONTROL,
                CROP, DLAYR, DWNOD, HARVFRAC, NLAYR, PConc_Shut,#Input
                PConc_Root, PConc_Shel, PConc_Seed, PLIGLF,     #Input
                PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST, RLV,    #Input
                RTWT, SDWT, SENESCE, SHELWT, STMWT, WTLF,       #Input
                WTNLF,WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,       #Input
                HARVRES)                                        #Output
      #TODO ver essa questão aqui
      #SENESCE % ResWt  = 0.0
      #SENESCE % ResLig = 0.0
      #SENESCE % ResE   = 0.0
    }
    
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #     Store plant module data for use in ETPHOT.
  # call's comentados
  #Call PUT('PLANT', 'CANHT',  CANHT)
  #Call PUT('PLANT', 'CANWH',  CANWH)
  #Call PUT('PLANT', 'DXR57',  DXR57)
  #Call PUT('PLANT', 'EXCESS', EXCESS)
  #Call PUT('PLANT', 'NR5',    NR5)   
  #Call PUT('PLANT', 'PLTPOP', PLTPOP)
  #Call PUT('PLANT', 'RNITP',  RNITP) 
  #Call PUT('PLANT', 'SLAAD',  SLAAD) 
  #Call PUT('PLANT', 'XPOD',   XPOD)
  
  #RETURN
  #END SUBROUTINE CROPGRO
}
#=======================================================================

#***********************************************************************
#     Variable listing (updated 25 Feb 2004)
#***********************************************************************
# AGEFAC    Relative effect of current leaf N on canopy photosynthesis 
#             (0-1) (fraction)
# AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
# AGRNOD    CH2O requirement for nodule growth (g[CH2O] / g[nodule])
# AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
# AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
#             content (g[CH2O] / g[seed])
# AGRSD2    CH2O requirement for seed growth, including cost for protein 
#             content (g[CH2O] / g[seed])
# AGRSD3    CH2O requirement for seed growth, with reduced N content
#            (g[CH2O] / g[seed])
# AGRSH1    CH2O required for shell growth, excluding cost for protein 
#             content (g[CH2O] / g[shell])
# AGRSH2    CH2O requirement for shell growth, including cost for protein 
#             content (g[CH2O] / g[shell])
# AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
# AGRVG     Mass of CH2O required for vegetative tissue growth including 
#             stoichiometry and respiration (g[CH2O] / g[tissue])
# AGRVG2    Total mass of CH2O required for vegetative tissue growth
#            (g[CH2O] / g[tissue])
# AREALF    Area of leaves (one side) per unit ground area
#            (cm2[leaf] / m2[ground])
# ASMDOT    Daily assimilative damage (g[CH2O] /m2 / d)
# BETN      Spacing between plants along a row (m / plant)
# BWAH      Weight of by-product not harvested (top weight minus seed 
#             weight) (g/m2)
# CADLF     Mass of CH2O added to leaf reserves after growth
#            (g[CH2O] / m2 / d)
# CADPR1    Maximum fraction of stem growth after flowering that can be 
#             allocated to carbohydrate storage just before a full seed 
#             load is set. (fraction)
# CADST     Mass of CH2O added to stems (g[CH2O] / m2 / d)
# CANHT     Canopy height (m)
# CANNAA    Weight of N in total plant at flowering (g[N] / m2)
# CANWAA    Canopy weight at flowering stage (g[plant] / m2)
# CANWH     Canopy width normal to row (m)
# CAVVEG    C available for vegetative tissue growth (g[CH2O] / m2 / d)
# CDMREP    Total CH2O needed for potential reproductive growth
#            (g[CH2O] / m2 / d)
# CGRSD     Carbon required for seed growth (g[CH2O] / m2 / d)
# CGRSH     Carbon required for shell growth (g[CH2O] / m2 / d)
# CLW       Cumulative leaf growth (g[leaf]/m2)
# CMINEA    Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
# CMINEP    Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
# CMOBMX    Maximum C pool mobilization rate (g[CH2O] / m2 / d)
# CNDFX     Carbon needed to fix N needed but not supplied by uptake or 
#             mining (g[CH2O] / m2 / d)
# CNOD      C used in N-Fixation and nodule growth (including respiration 
#             costs) today (g[CH2O] / m2 / d)
# CNODMN    Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
# CO2       Atmospheric carbon dioxide concentration (ï¿½mol[CO2] / mol[air])
# CONTROL   Composite variable containing variables related to control 
#             and/or timing of simulation.    See Appendix A. 
# CROP      Crop identification code 
# CRUSLF    C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
# CRUSRT    C mobilized from root tissue in a day (g[CH2O] / m2 / d)
# CRUSSH    C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
# CRUSST    C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
# CSAVEV    Fraction of PG for VEG that is stored as CH2O 
# CSW       Cumulative stem growth (g[stem]/m2)
# CTONOD    C to allocate to nodules to fix N needed for reproductive and 
#             vegetative growth (g[CH2O] / m2 / d)
# CTONODR   CH2O allocated to nodules for fixing N needed for reproductive 
#             growth (g[CH2O] / m2 / d)
# DAS       Days after start of simulation (d)
# DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
# DETACH    Switch to determine if pod detachment will be simulated (Y or 
#             N) 
# DISLA     Diseased leaf area (cm2[leaf]/m2[ground]/d)
# DLAYR(L)  Thickness of soil layer L (cm)
# DRPP      Photoperiod days which occur in a real day
#            (photoperiod days / day)
# DS(L)     Cumulative depth in soil layer L (cm)
# DTX       Thermal time that occurs in a real day based on vegetative 
#             development temperature function (thermal days / day)
# DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
#             layer L (cm3[water]/cm3[soil])
# DWNOD     Current nodule mass (g[nodule] / m2)
# DWNODA    Cumulative nodule growth (g[nodule] / m2)
# DXR57     Relative time between first seed (NR5) and physiological 
#             maturity (NR7) (fraction)
# ECONO     Ecotype code - used to match ECOTYP in .ECO file 
# EOP       Potential plant transpiration rate (mm/d)
# EORATIO   Ratio of increase in potential evapotranspiration with increase 
#             in LAI (up to LAI=6.0) for use with FAO-56 Penman reference 
#             potential evapotranspiration. 
# EP1       Actual plant transpiration rate (cm/d)
# EXCESS    Factor based on excess PG used to affect tomorrows PG 
#             calculation 
# F         Specific leaf area of new leaf tissue growth, including N
#            (cm2[leaf] / g[leaf])
# FILECC    Path plus filename for species file (*.spe) 
# FILEGC    Pathname plus filename for ECO file 
# FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
# FLWN(J)   Number of flowers added for cohort J (# / m2)
# FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
# FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
# FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
# FNINSD    Maximum fraction of N for growing seed tissue based on 
#             temperature (g[N] / g[seed])
# FNINSH    Maximum fraction of N for growing shell tissue
#            (g[N] / g[shell])
# FRACDN    Relative time between flowering (NR1) and last leaf appearance 
#             (NDLEAF) 
# FRCNOD    Fraction of new root dry matter allocation that is diverted to 
#             nodule growth 
# FREEZ1    Temperature below which plant loses all leaves, but development 
#             continues (ï¿½C)
# FREEZ2    Temperature below which plant growth stops completely. (ï¿½C)
# FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
#             day (g[leaf] / g[veg])
# FRRT      Fraction of vegetative tissue growth that goes to roots on a 
#             day (g[root] / g[veg])
# FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
#             day (g[stem] / g[veg])
# GDMSD     Seed growth demand based on temperature and photoperiod
#            (g[seed] / m2 / d)
# GROWTH    Total new growth of plant tissue on a day (g[tissue] / m2 / d)
# GRRAT1    Maximum growth per individual shell (g / shell / d)
# GRWRES    Growth respiration (g[CH2O]/m2-d)
# HARVFRAC  Two-element array containing fractions of (1) yield harvested 
#             and (2) by-product harvested (fraction)
# HARVRES   Composite variable containing harvest residue amounts for total 
#             dry matter, lignin, and N amounts.  Structure of variable is 
#             defined in ModuleDefs.for. 
# IDETO     Switch for printing OVERVIEW.OUT file 
# ISWDIS    Pest damage simulation switch (Y or N) 
# ISWITCH   Composite variable containing switches which control flow of 
#             execution for model.  The structure of the variable 
#             (SwitchType) is defined in ModuleDefs.for. 
# ISWNIT    Nitrogen simulation switch (Y or N) 
# ISWSYM    Nitrogen fixation simulation switch (Y = simulate nodule 
#             growth, N = no nodule growth, U = N-fixation occurs at a rate 
#             that carbon will allow, and nodules are not grown explicitly) 
# ISWWAT    Water simulation control switch (Y or N) 
# KCAN      Canopy light extinction coefficient for daily PAR, for 
#             equidistant plant spacing, modified when in-row and between 
#             row spacing are not equal 
# KEP       Energy extinction coefficient for partitioning EO to EP 
# KSEVAP    Light extinction coefficient used for computation of soil 
#             evaporation 
# KTRANS    Light extinction coefficient used for computation of plant 
#             transpiration 
# LAGSD     Time required between shell growth and seed growth, per cohort
#            (Photo-thermal days)
# LAIMX     Maximum leaf area index this season (m2[leaf] / m2[ground])
# LL(L)     Volumetric soil water content in soil layer L at lower limit
#            (cm3 [water] / cm3 [soil])
# LNGPEG    Time between start of peg (full flower) and shell formation 
#             (for peanuts only).  Defines slow growth period.
#             (Photo-thermal days)
# MAINR     Maintenance respiration (g[CH2O] / m2 / d)
# MDATE     Harvest maturity date (YYYYDDD)
# MEPHO     Method for photosynthesis computation ('C'=Canopy or daily, 
#             'L'=hedgerow or hourly) 
# NADLF     N added to leaf N reserves (g[N] / m2 / d)
# NADRT     N added to root N reserves (g[N] / m2 / d)
# NADST     N added to stem N reserves (g[N] / m2 / d)
# NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
# NAVLV     N available for vegetative growth (g[N] / m2 / d)
# NDLEAF    Day when leaf expansion ceased (d)
# NDMNEW    Total N demand for new growth (g[N] / m2 / d)
# NDMOLD    N demand for old tissue (g[N] / m2 / d)
# NDMREP    Total N needed for potential reproductive growth
#            (g[N] / m2 / d)
# NDMSDR    Amount of Mobilized N which can be used for seed growth
#            (g[N] / m2 / d)
# NDMTOT    Total N demand (g[N] / m2 / d)
# NDMVEG    N required for vegetative growth if all PGAVL is used as 
#             computed (g[N] / m2 / d)
# NDSET     Normal time by which a pod load (full number) should be 
#             achieved with no water stress (d)
# NDTH      Nodule death rate (g[nodule] / m2 / d)
# NFIXN     Amount of N fixed during the day (g[N] / m2 / d)
# NGRLF     Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
# NGRRT     Maximum N demand for root growth (g[root N] / m2[ground] / d)
# NGRSD     Rate of N accumulation in new seeds (g[N] / m2 / d)
# NGRSH     Rate of N accumulation in new shells (g[N] / m2 / d)
# NGRST     Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
# NH4(L)    Ammonium N in soil layer L (ï¿½g[N] / g[soil])
# NLAYR     Actual number of soil layers 
# NMINEA    Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
# NMOBR     Stage-dependent potential N mining rate expressed as a fraction 
#             of the maximum rate (NMOBMX) 
# NO3(L)    Nitrate in soil layer L (ï¿½g[N] / g[soil])
# NODGR     New nodule growth (g[nod] / m2 / d)
# NOUTDO    Logical unit for OVERVIEW.OUT file 
# NPLTD     Number of plants destroyed (#/m2/d)
# NR1       Day when 50% of plants have at least one flower (d)
# NR2       Day when 50% of plants have one fruit (pod or peg) (d)
# NR5       Day when 50% of plants have pods with beginning seeds (d)
# NR7       Day when 50% of plants first have yellowing or maturing pods
#            (d)
# NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
# NRUSRT    N actually mobilized from roots in a day (g[N]/m2-d)
# NRUSSH    N actually mobilized from shells in a day (g[N]/m2-d)
# NRUSST    N actually mobilized from stems in a day (g[N]/m2-d)
# NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
# NVEG0     Day of emergence (d)
# PAR       Daily photosynthetically active radiation or photon flux 
#             density (moles[quanta]/m2-d)
# PCARSH    Proportion of shell tissue that is carbohydrate (fraction)
# PCCSD     Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
# PCH2O     Respiration loss due to storage/mobilization of CH2O
#            (g[CH2O] / g[CH2O])
# PCLSD     Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
# PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
# PCNRT     Percent N in root tissue (100 g[N] / g[root])
# PCNSD     Percentage of N in seed tissue (100 g[N] / g[seed])
# PCNSH     Percentage of N in shell tissue (100 g[N] / g[shell])
# PCNST     Percent N in stem tissue (100 g[N] / g[stem])
# PCTMAT    Fraction of pods that are mature (seed are 90% of final size) 
# PG        Daily gross photosynthesis (g[CH2O] / m2 - d)
# PGAVL     Total available CH2O available for growth & respiration
#            (g[CH2O] / m2)
# PHTHRS(I) Threshold time that must accumulate in phase I for the next 
#             stage to occur (thermal or photothermal days)
# PHTIM(I)  Cumulative photothermal time ages of seeds and shells 
# PLIGLF    Proportion of leaf tissue that is lignin (fraction)
# PLIGNO    Proportion of nodule tissue that is lignin (fraction)
# PLIGRT    Proportion of root tissue that is lignin (fraction)
# PLIGSD    Proportion of seed tissue that is lignin (fraction)
# PLIGSH    Proportion of shell tissue that is lignin (fraction)
# PLIGST    Proportion of stem tissue that is lignin (fraction)
# PLIPSH    Proportion of shell tissue that is lipid (fraction)
# PLTPOP    Plant population (# plants / m2)
# PMINSD    Proportion of seed tissue that is mineral (fraction)
# PMINSH    Proportion of shell tissue that is mineral (fraction)
# PNTIM(I)  Photothermal days from first flower when flowers in age group I 
#             formed (p-t-d)
# POASD     Proportion of seed tissue that is organic acid (fraction)
# POASH     Proportion of shell tissue that is organic acid (fraction)
# PODNO     Total number of pods (#/m2)
# PODWT     Dry mass of seeds plus shells, including C and N
#            (g[pods] / m2[ground])
# PODWTD    Mass of detached pods (g[pods] / m2[ground])
# PORMIN    Minimum pore space required for supplying oxygen to roots for 
#             optimal growth and function (cm3/cm3)
# POTCAR    Potential carbohydrate composition of seed based on temperature
#            (fraction)
# POTLIP    Potential lipid composition of seed based on temperature
#            (fraction)
# PPLTD     Percent plants destroyed (%/m2/d)
# PROLFI    Maximum protein composition in leaves during growth with 
#             luxurious supply of N (g[protein] / g[leaf tissue])
# PRORTI    Maximum protein composition in roots during growth with 
#             luxurious supply of N (g[protein] / g[root])
# PROSHI    Maximum protein composition in shells during growth with 
#             luxurious supply of N (g[protein] / g[shell tissue])
# PROSTI    Maximum protein composition in stems during growth with 
#             luxurious supply of N (g[protein] / g[stem])
# PROVEG    Average protein composition of growing tissue today
#            (g[protein] / g[veget. tissue])
# PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
# PUNCTR    Cumulative puncture damage (not yet implemented) 
# R30C2     Respiration coefficient that depends on total plant mass, value 
#             at 30C (g[CH2O] used / g[CH2O] fixed / hr)
# RCH2O     Respiration required for synthesizing CH2O structure
#            (g[CH2O] / g[tissue])
# RES30C    Respiration coefficient that depends on gross photosynthesis, 
#             value at 30C (g CH2O/g DW/hr)
# RFIXN     CH2O required for biological N fixation (g[CH2O] / g[protein])
# RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# RHOS      Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
# RLIG      Respiration required for synthesizing lignin structure
#            (g[CH2O] / g[lignin])
# RLIP      Respiration required for synthesizing lipid structure
#            (g[CH2O] / g[lipid])
# RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# RMIN      Respiration required for synthesizing mineral structure
#            (g[CH2O] / g[mineral])
# RNH4C     CH2O required for protein synthesis when source of N is 
#             ammonium uptake (g[CH2O] / g[protein])
# RNITP     True nitrogen concentration in leaf tissue for photosynthesis 
#             reduction. (%)
# RNMODE    Simulation run mode (I=Interactive, A=All treatments, B=Batch 
#             mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence) 
# RNO3C     Respiration required for reducing NO3 to protein
#            (g[CH2O] / g[protein])
# RO        Respiration coefficient that depends on total plant mass
#            (g[CH2O] / g[tissue])
# ROA       Respiration required for synthesizing organic acids
#            (g[CH2O] / g[product])
# ROWSPC    Row spacing (m)
# RP        Proportion of the day's photosynthesis which is respired in the 
#             maintenance process 
# RPRO      Respiration required for re-synthesizing protein from mobilized 
#             N (g[CH2O] / g[protein])
# RPROAV    Respiration required for protein synthesis, average based on 
#             sources of N (g[CH2O] / g[protein])
# RSPNH4    Respiration required for reducing NH4 to protein
#            (g[CH2O] / m2 / d)
# RSPNO3    Respiration required for reducing NO3 to protein
#            (g[CH2O] / m2 / d)
# RSTAGE    Number of RSTAGES which have occurred. 
# RTDEP     Root depth (cm)
# RTWT      Dry mass of root tissue, including C and N
#            (g[root] / m2[ground])
# RUN       Change in date between two observations for linear 
#             interpolation 
# RVSTGE    Rate of VSTAGE change (nodes/day)
# RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of 
#             TRWU/EP1 (total potential daily root water uptake/ actual 
#             transpiration) 
# RWUMX     Maximum water uptake per unit root length, constrained by soil 
#             water (cm3[water] / cm [root])
# SAT(L)    Volumetric soil water content in layer L at saturation
#            (cm3 [water] / cm3 [soil])
# SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
#             stress; 1 = saturated stress ) 
# SDDES(J)  Number of seeds destroyed today in cohort J when shells are not 
#             destroyed (#/m2/d)
# SDGR      Potential growth rate per seed (g / seed / d)
# SDIDOT    Number of seeds destroyed on the current day (#/m2/d)
# SDNO(J)   Number of seeds for cohort J (#/m2)
# SDNPL     Seed N (g[N] / m2)
# SDPROR    Ratio to adjust lipid and carbohydrate proportions when seed 
#             protein differs from protein composition of standard cultivar 
#             (SDPROS) 
# SDRATE    Seeding rate, mass of seed sown (g[seed] / m2[ground])
# SDVAR     Maximum cultivar-dependent seed growth rate, per seed
#            (g / seed / d)
# SDWT      Dry mass of seed tissue, including C and N
#            (g[seed] / m2[ground])
# SDWTAH    Actual seed weight harvested (g[seed] / m2[ground])
# SEEDNI    Seed or transplant N at planting (g[N] / m2)
# SEEDNO    Total number of seeds (#/m2)
# SENESCE   Composite variable containing data about daily senesced plant 
#             matter. Structure of variable is defined in ModuleDefs.for 
# SENNOD(L) Daily senesced matter from nodules in soil layer L
#            (kg[dry matter]/ha)
# SENRT(L)  Daily senesced matter from roots in soil layer L
#            (kg[dry matter]/ha)
# SHELN(J)  Number of shells for cohort J (#/m2)
# SHELWT    Total mass of all shells (g / m2)
# SHVAR     Shell growth rate during its rapid growth phase, per shell
#            (g / shell / d)
# SLA       Specific leaf area (cm2[leaf] / m2[ground])
# SLAAD     Specific leaf area, excluding weight of C stored in leaves
#            (cm2[leaf] / g[leaf])
# SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
# SLNDOT    Leaf senescence due to water stress (g/m2/day)
# SLPF      Soil photosynthesis factor, 0 to 1 scale 
# SOILPROP  Composite variable containing soil properties including bulk 
#             density, drained upper limit, lower limit, pH, saturation 
#             water content.  Structure defined in ModuleDefs. 
# SRDOT     Daily root senescence (g / m2 / d)
# SSDOT     Daily senescence of petioles (g / m2 / d)
# SSNDOT    Petiole senescence due to water stress (g/m2/day)
# ST(L)     Soil temperature in soil layer L (ï¿½C)
# STGDOY(I) Day when plant stage I occurred (YYYYDDD)
# STMWT     Dry mass of stem tissue, including C and N
#            (g[stem] / m2[ground)
# SW(L)     Volumetric soil water content in layer L
#            (cm3 [water] / cm3 [soil])
# SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
#             0.0=max stress 
# SWIDOT    Daily seed mass damage (g/m2/day)
# TAVG      Average daily temperature (ï¿½C)
# TDAY      Average temperature during daylight hours (ï¿½C)
# TDUMX     Photo-thermal time that occurs in a real day based on early 
#             reproductive development temperature function
#             (photo-thermal days / day)
# TDUMX2    Photo-thermal time that occurs in a real day based on late 
#             reproductive development temperature function
#             (photo-thermal days / day)
# TGRO(I)   Hourly canopy temperature (ï¿½C)
# TGROAV    Average daily canopy temperature (ï¿½C)
# TMIN      Minimum daily temperature (ï¿½C)
# TNLEAK    Total nitrogen leak (g[N] / m2 / d)
# TOPWT     Total weight of above-ground portion of crop, including pods
#            (g[tissue] / m2)
# TOTWT     Total weight of crop (g[tissue] / m2)
# TRNH4U    Total N uptake in ammonium form in a day (kg[N] / ha / d)
# TRNO3U    Total N uptake in nitrate form in a day (kg[N] / ha / d)
# TRNU      Total N uptake in a day (kg[N] / ha / d)
# TRWUP     Potential daily root water uptake over soil profile (cm/d)
# TTFIX     Physiological days delay in nodule initiation
#            (photo-thermal days / day)
# TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
#             pod addition.  Stress is defined to INCREASE growth and 
#             addition. 
# TURFAC    Water stress factor for expansion (0 - 1) 
# UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
#            (kg [N] / ha - d)
# UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
# VSTAGE    Number of nodes on main stem of plant (nodes)
# WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
# WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
# WCRSH     Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
# WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
# WLDOTN    Dry weight growth rate of new leaf tissue including N but not C 
#             reserves (g[leaf] / m2[ground]-d)
# WLFDOT    Leaf weight losses due to freezing (g[leaf]/m2-d)
# WLIDOT    Daily pest or freeze damage to leaf mass (g/m2/day)
# WNRLF     N available for mobilization from leaves above lower limit of 
#             mining (g[N] / m2)
# WNRRT     N available for mobilization from roots above lower limit of 
#             mining (g[N] / m2)
# WNRSH     N available for mobilization from shells above lower limit of 
#             mining (g[N] / m2)
# WNRST     N available for mobilization from stems above lower limit of 
#             mining (g[N] / m2)
# WR(L)     Root hospitality factor, used to compute root distribution 
# WRDOTN    Dry weight growth rate of new root tissue including N but not C 
#             reserves (g[root] / m2[ground]-d)
# WRIDOT    Daily pest damage to root mass (g/m2/day)
# WSDDTN    New seed growth today (g[seed] / m2 / d)
# WSDOTN    Dry weight growth rate of new stem tissue including N but not C 
#             reserves (g[stem] / m2[ground]-d)
# WSHDTN    New shell growth today (g[shell] / m2 / d)
# WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WSIDOT    Daily pest damage to stem mass (g/m2/day)
# WTABRT    Weight of shells aborted on a day (g[shell] / m2 / d)
# WTCO      Cumulative losses of plant tissue (g[tissue] / m2)
# WTLF      Dry mass of leaf tissue including C and N
#            (g[leaf] / m2[ground])
# WTLO      Cumulative leaf losses (g[leaf] / m2)
# WTMAIN    Mass of tissue assumed to require maintenance (g[tissue] / m2)
# WTNCAN    Mass of N in canopy (g[N] / m2[ground])
# WTNEW     Initial mass of seedling or seed (g / plant)
# WTNFX     Cumulative weight of N fixed (g[N] / m2)
# WTNLA     Cumulative N added to leaves (g[N]/m2-d)
# WTNLF     Mass of N in leaves (g[leaf N] / m2[ground])
# WTNLO     Cumulative N loss from leaves (g[N] / m2)
# WTNNA     Cumulative N added to nodules (g[N]/m2-d)
# WTNNAG    Total accumulated N used in nodule growth (g[N] / m2)
# WTNNO     Cumulative N loss from nodules (g[N] / m2)
# WTNNOD    Mass of N in nodules (g[N] / m2[ground])
# WTNOO     Cumulative nodule losses (g[nodule] / m2)
# WTNRA     Cumulative N added to roots (g[N]/m2-d)
# WTNRO     Cumulative N loss from roots (g[N] / m2)
# WTNRT     Mass of N in roots (g[root N] / m2[ground])
# WTNSA     Cumulative N added to stems (g[N]/m2-d)
# WTNSD     Mass of N in seeds (g[N] / m2[ground])
# WTNSDA    Cumulative N added to seeds (g[N]/m2-d)
# WTNSDO    Cumulative N loss from seeds (g[N] / m2)
# WTNSH     Mass of N in shells (g[N] / m2[ground])
# WTNSHA    Cumulative N added to shells (g[N]/m2-d)
# WTNSHO    Cumulative N loss from shells (g[N] / m2)
# WTNSO     Cumulative N loss from stems (g[N] / m2)
# WTNST     Mass of N in stems (g[stem N] / m2[ground])
# WTNUP     Cumulative N uptake (g[N] / m2)
# WTRO      Cumulative root losses (g[root] / m2)
# WTSD(J)   Seed mass  for cohort J (g/m2)
# WTSDO     Cumulative seed losses (g[seed] / m2)
# WTSHE(J)  Shell mass  for cohort J (g/m2)
# WTSHMT    Cohorts that reach THRESH today (g/m2)
# WTSHO     Cumulative shell losses (g[shell] / m2)
# WTSO      Cumulative stem losses (g[stem] / m2)
# XFRT      Current day's partitioning to reproductive growth (0-1)
#            (g[fruit] / g[plant])
# XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
# XLAI      Leaf area (one side) per unit of ground area
#            (m2[leaf] / m2[ground])
# XPOD      Growth partitioning to pods which slows node appearance
#            (fraction)
# YRDOY     Current day of simulation (YYYYDDD)
# YREMRG    Day of emergence (YYYYDDD)
# YREND     Date for end of season (usually harvest date) (YYYYDDD)
# YRNR1     Day when 50% of plants have at least one flower (YYYYDDD)
# YRNR2     Day when 50% of plants have one fruit (pod or peg) (YYYYDDD)
# YRNR3     Day when 50% of plants have at least one beginning pod
#            (YYYYDDD)
# YRNR5     Day when 50% of plants have pods with beginning seeds (YYYYDDD)
# YRNR7     Day when 50% of plants first have yellowing or maturing pods
#            (YYYYDDD)
# YRPLT     Planting date (YYYYDDD)
#***********************************************************************
#      END SUBROUTINE CROPGRO
#=======================================================================