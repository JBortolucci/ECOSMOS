


simDataVars$TGRO     <- rep(1.,24)



GROWTH <- function (iyear, iyear0, jday,DAS,DYNAMIC){
  
  
  environment(GROW)             <- env


GROW <- function (CONTROL, ISWITCH, EMERG, SOILPROP, 
        AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     # #!Input
        CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            # #!Input
        NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   # #!Input
        NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       # #!Input
        NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            # #!Input
        POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            # #!Input
        SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       # #!Input
        SSNDOT, TRNH4U, TRNO3U, TRNU,                     # #!Input
        TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   # #!Input
        WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    # #!Input
        MDATE, YRPLT,                                     # #!Input
        SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             # #!Input/Output
        AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    # #!Output
        DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      # #!Output
        PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         # #!Output
        PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   # #!Output
        PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         # #!Output
        ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                # #!Output
        SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             # #!Output
        SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  # #!Output
        WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          # #!Output
        WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  # #!Output
        WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       # #!Output
        WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        # #!Output
        WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      # #!Output
        WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      # #!Output
        ShutMob, RootMob, ShelMob)  {                      # #!Output
  
}

ROOTS <- function(EMERG,
          AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #!Input
          ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #!Input
          SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #!Input
          RLV, RTDEP, SATFAC, SENRT, SRDOT)  {             #!Output

  
}

DEMAND <- function(RUNINIT, CONTROL,
         AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  #!Input
         FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    #!Input
         LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  #!Input
         PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      #!Input
         SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   #!Input
         TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, #!Input
         WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   #!Input
         WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     #!Input
         AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  #!Output
         FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   #!Output
         GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  #!Output
         NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      #!Output
         POTLIP, SDGR, TURADD, XFRT, YREND)  {              #!Output

}
  
PODS <- function(EMERG, 
         AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #!Input
         FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #!Input
         GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #!Input
         NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #!Input
         PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #!Input
         TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #!Input
         PStres2, YRPLT,                                 #!Input
         AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #!Output
         PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #!Output
         SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #!Output
         WTSHE, WTSHMT, FLWN)         {                   #!Output

}
  
#!-----------------------------------------------------------------------
   VEGGR <- function(EMERG, 
                 AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      #!Input
                 DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     #!Input
                 FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              #!Input
                 NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      #!Input
                 PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           #!Input
                 TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      #!Input
                 WCRST, WTLF, XLAI, YRDOY, YREMRG,               #!Input
                 AGRVG, FRLF, FRRT, FRSTM,                       #!I/O
                 CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     #!Output
                 CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   #!Output
                 NADST, NGRLF, NGRRT, NGRST, NSTRES,             #!Output
                 TNLEAK, WLDOTN, WRDOTN, WSDOTN)      {           #!Output

     
   }

PODDET <- function(
      FILECC, TGRO, WTLF, YRDOY, YRNR2,               #!Input
      PODWTD, SDNO, SHELN, SWIDOT,                    #!Output
      WSHIDT, WTSD, WTSHE,                            #!Output
      EMERG)  {
  
  
}


SENES <- function(INTEGR, 
          FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       #!Input
          RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  #!Input
          SLDOT, SLNDOT, SSDOT, SSNDOT) {
  
  
}

FREEZE <- function(
      FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,           #!Input
      TMIN, WTLF, YRDOY,  YRPLT,                      #!Input
      MDATE,                                          #!Input/Output
      WLFDOT){
  
  
}   




}




