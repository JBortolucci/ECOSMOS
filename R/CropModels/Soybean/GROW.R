#=======================================================================
#  GROW, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
#-----------------------------------------------------------------------
#  Governing Model Equations
#  Integrates all growth related variables
#-----------------------------------------------------------------------
#  REVISION       HISTORY
#  01/09/1989 GH  Written.
#  01/04/1996 GH  Added seed composition routine.
#  01/19/1996 KJB NMOBR
#  04/02/1996 KJB Modified
#  05/30/1998 CHP moved diseased leaf area calculations to PEST module.
#  07/16/1998 CHP Modified for modular format
#  05/11/1999 GH  Incorporated in CROPGRO
#  03/29/2000 CHP Added cumulative senescence variables
#  06/19/2001 GH  Add checks for negative state variables
#  06/11/2002 GH  Modified for Y2K
#  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
#                   as defined in ModuleDefs.for
#  03/24/2004 CHP Added P component of senesced matter
#  01/19/2006 CHP N in senesced roots lost at actual N%, not minimum.
#-----------------------------------------------------------------------
#  Called by:  PLANT
#  Calls:      IPGROW, STRESS
#              ERROR
#=======================================================================

simDataVars$SWIDOT  <-  0 #input/output
simDataVars$WLFDOT  <-  0 #input/output
simDataVars$WSHIDT  <-  0 #input/output
simDataVars$WTNFX  <-  0 #input/output
simDataVars$XHLAI  <-  0 #input/output
simDataVars$AREALF  <-  0
simDataVars$BETN  <-  0
simDataVars$CANNAA  <-  0
simDataVars$CANWAA  <-  0
simDataVars$CLW  <-  0
simDataVars$CSW  <-  0
simDataVars$DWNOD  <-  0
simDataVars$DWNODA  <-  0
simDataVars$GROWTH  <-  0
simDataVars$GRWRES  <-  0
simDataVars$LAIMX  <-  0
simDataVars$PCCSD  <-  0
simDataVars$PCLSD  <-  0
simDataVars$PCNL  <-  0
simDataVars$PCNRT  <-  0
simDataVars$PCNSD  <-  0
simDataVars$PCNSH  <-  0
simDataVars$PCNST  <-  0
simDataVars$PLTPOP  <-  0
simDataVars$PLIGLF  <-  0
simDataVars$PLIGNO  <-  0
simDataVars$PLIGRT  <-  0
simDataVars$PLIGSD  <-  0
simDataVars$PLIGSH  <-  0
simDataVars$PLIGST  <-  0
simDataVars$PODWT  <-  0
simDataVars$PUNCSD  <-  0
simDataVars$PUNCTR  <-  0
simDataVars$RHOL  <-  0
simDataVars$RHOS  <-  0
simDataVars$RNITP  <-  0
simDataVars$ROWSPC  <-  0
simDataVars$RTWT  <-  0
simDataVars$SDNPL  <-  0
simDataVars$SDRATE  <-  0
simDataVars$SDWT  <-  0
simDataVars$SEEDNI  <-  0
simDataVars$SEEDNO  <-  0
simDataVars$SENESCE  <-  0
simDataVars$SHELWT  <-  0
simDataVars$SLA  <-  0
simDataVars$SLAAD  <-  0
simDataVars$STMWT  <-  0
simDataVars$TOPWT  <-  0
simDataVars$TOTWT  <-  0
simDataVars$WCRLF  <-  0
simDataVars$WCRRT  <-  0
simDataVars$WCRSH  <-  0
simDataVars$WCRST  <-  0
simDataVars$WNRLF  <-  0
simDataVars$WNRRT  <-  0
simDataVars$WNRSH  <-  0
simDataVars$WNRST  <-  0
simDataVars$WTCO  <-  0
simDataVars$WTLF  <-  0
simDataVars$WTLO  <-  0
simDataVars$WTMAIN  <-  0
simDataVars$WTNCAN  <-  0
simDataVars$WTNEW  <-  0
simDataVars$WTNLA  <-  0
simDataVars$WTNLF  <-  0
simDataVars$WTNLO  <-  0
simDataVars$WTNNA  <-  0
simDataVars$WTNNAG  <-  0
simDataVars$WTNNO  <-  0
simDataVars$WTNNOD  <-  0
simDataVars$WTNOO  <-  0
simDataVars$WTNRA  <-  0
simDataVars$WTNRO  <-  0
simDataVars$WTNRT  <-  0
simDataVars$WTNSA  <-  0
simDataVars$WTNSD  <-  0
simDataVars$WTNSDA  <-  0
simDataVars$WTNSDO  <-  0
simDataVars$WTNSH  <-  0
simDataVars$WTNSHA  <-  0
simDataVars$WTNSHO  <-  0
simDataVars$WTNSO  <-  0
simDataVars$WTNST  <-  0
simDataVars$WTNUP  <-  0
simDataVars$WTRO  <-  0
simDataVars$WTSDO  <-  0
simDataVars$WTSHO  <-  0
simDataVars$WTSO  <-  0
simDataVars$XLAI  <-  0
simDataVars$XPOD  <-  0
simDataVars$ShutMob  <-  0
simDataVars$RootMob  <-  0
simDataVars$ShelMob  <-  0

GROW <- function (CONTROL, ISWITCH, DYNAMIC, EMERG, SOILPROP, 
                  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #!Input
                  CRUSST, DISLA, Fnew, FILECC, FRLF, FRSTM,         #!Input  #*** Fnew is 'F' in the original file. Changed because F is logical in R. ***
                  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #!Input
                  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #!Input
                  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #!Input
                  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #!Input
                  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #!Input
                  SSNDOT, TRNH4U, TRNO3U, TRNU,                     #!Input
                  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #!Input
                  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #!Input
                  MDATE, YRPLT,                                     #!Input
                  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             #!Input/Output
                  AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    #!Output
                  DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      #!Output
                  PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         #!Output
                  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   #!Output
                  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         #!Output
                  ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                #!Output
                  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             #!Output
                  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  #!Output
                  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          #!Output
                  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  #!Output
                  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       #!Output
                  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        #!Output
                  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      #!Output
                  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      #!Output
                  ShutMob, RootMob, ShelMob)  {                     #!Output
  
  GROW <- 0
  
  #TODO conectar com padrao ECOSMOS 
  #CHARACTER*1  ISWSYM, ISWNIT, IDETO, IHARI, PLME
  #DYNAMIC, NOUTDO, L, NLAYR
  #YRDOY, YRNR1, MDATE
  #YRPLT
  PLME   <- 'S' # equivalente ao [.SBX] *PLANTING DETAILS: PLME  
  IHARI  <- 'M' # TODO VERIFICAR (provável que pertença ao '[.SBX] *HARVEST DETAILS')
  PLTPOP <- 40  # equivalente ao [.SBX] *PLANTING DETAILS: PPOE
  ROWSPC <- 0.5 # equivalente ao [.SBX] *PLANTING DETAILS: TPLRS
  SDWTPL <- -99 # equivalente ao [.SBX] *PLANTING DETAILS: PLDS
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- 0.200 # Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  SDPRO <- 0.400 # Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  WTPSD <- 0.19  # Maximum weight per seed (g)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PROLFF  <- 0.112
  PROSTF  <- 0.035
  PRORTF  <- 0.056
  PROSHF  <- 0.050
  PCARLF  <- 0.405000001
  PCARNO  <- 0.479999989
  PCARRT  <- 0.711000025
  PCARSD  <- 0.314999998
  PCARSH  <- 0.379999995
  PCARST  <- 0.663999975
  PLIGLF  <- 7.00000003E-02
  PLIGNO  <- 7.00000003E-02
  PLIGRT  <- 7.00000003E-02
  PLIGSD  <- 1.99999996E-02
  PLIGSH  <- 0.280000001
  PLIGST  <- 7.00000003E-02
  PLIPLF  <- 2.50000004E-02
  PLIPNO  <- 5.00000007E-02
  PLIPRT  <- 1.99999996E-02
  PLIPSH  <- 1.99999996E-02
  PLIPST  <- 1.99999996E-02
  PMINLF  <- 9.39999968E-02
  PMINNO  <- 5.00000007E-02
  PMINRT  <- 5.70000000E-02
  PMINSD  <- 2.50000004E-02
  PMINSH  <- 2.99999993E-02
  PMINST  <- 4.60000001E-02
  POALF   <- 5.00000007E-02
  POANO   <- 5.00000007E-02
  POART   <- 5.00000007E-02
  POASD   <- 3.99999991E-02
  POASH   <- 3.99999991E-02
  POAST   <- 5.00000007E-02
  PROLFI  <- 0.356000006
  PRONOD  <- 0.300000012
  PRORTI  <- 9.20000002E-02
  PROSTI  <- 0.150000006
  #!*CARBON AND NITROGEN MINING PARAMETERS
  ALPHL  <- 0.04
  ALPHS  <- 0.08
  ALPHR  <- 0.04
  ALPHSH <- 0.08
  #!*VEGETATIVE PARTITIONING PARAMETERS
  WTFSD  <- 0.55
  #!*ROOT PARAMETERS
  RMIN   <- 0.05
  
  #TODO: trazer NUPTAKE.for e NFIX.for para 'TRNO3U' 'TRNH4U' 'NFIXN' 'NDTH' 'DWNOD' 'TRNU' 'NODGR'

  #     Surface and soil residue due to daily senescence of plant matter
  NL       = 20  #!Maximum number of soil layers
  
  #TODO ajustar para padrão ECOSMOS?
  SENRT(NL)
  SENNOD(NL)
  SenWt(0:NL)        #kg[dry matter]/ha
  SenLig(0:NL)       #kg[lignin]/ha
  SenE(0:NL,NELEM)   #kg[E]/ha (E=N, P, S,...)
  NLPEST
  NLAYR
  
  # danos por pestes...
  DISLA  <- 1 #TODO VERIFICAR PEST.for (Diseased leaf area (cm2[leaf]/m2[ground]/d))
  PPLTD  <- 0 #TODO VERIFICAR PEST.for (Percent plants destroyed (%/m2/d))
  NPLTD  <- 0 #TODO VERIFICAR PEST.for (Number of plants destroyed (#/m2/d))
  WRIDOT <- 0 #TODO VERIFICAR PEST.for & ROOTDM.for (Daily pest damage to root mass (g/m2/day))
  WSIDOT <- 0 #TODO VERIFICAR PEST.for & VEGDM.for (Daily pest damage to stem mass (g/m2/day))
  SDIDOT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Number of seeds destroyed on the current day (#/m2/d))
  WLIDOT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Daily pest or freeze damage to leaf mass (g/m2/day))
  WSHIDT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Weight of shell tissue consumed by pests today (g[shell]/m2-d))
  
  

  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  if (DYNAMIC == RUNINIT) {
    #-----------------------------------------------------------------------
    #CALL IPGROW(
    #  &  FILEIO, FILECC,  CROP,                                  !Input
    #  &  ALPHL,  ALPHR,  ALPHS,  ALPHSH,                         !Output
    #  &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,         !Output
    #  &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,         !Output
    #  &  PLIPLF, PLIPST, PLIPRT, PLIPSH,                 PLIPNO, !Output
    #  &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,         !Output
    #  &  POALF,  POAST,  POART,  POASH,  POASD,  POANO,          !Output
    #  &  PROLFF, PROSTF, PRORTF, PROSHF,                 PRONOD, !Output
    #  &  PROLFI, PROSTI, PRORTI,                                 !Output
    #  &  PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,                 !Output
    #  &  SDLIP,  SDPRO,  WTFSD,  WTPSD,   XPODF)                 !Output
    
    if (CROP != 'FA') {
      #-----------------------------------------------------------------------
      #       Copied from IPIBS
      #-----------------------------------------------------------------------
      ROWSPC = ROWSPC / 100.
      if (ROWSPC * PLTPOP > 1.E-4) {
        BETN = 1 / (ROWSPC*PLTPOP)
      } else {
        BETN = 0
      }
      
      #-----------------------------------------------------------------------
      #     Newest version of CROPGRO has variable seed N, oil, CH2O,
      #     which may make this tougher
      #-----------------------------------------------------------------------
      CPFLF  = 30./44.*(PLIPLF*1.720 + PLIGLF*0.659 + POALF*(-0.011) + PMINLF*RMIN + PCARLF*0.170)
      CPFSTM = 30./44.*(PLIPST*1.720 + PLIGST*0.659 + POAST*(-0.011) + PMINST*RMIN + PCARST*0.170)
      CPFRT  = 30./44.*(PLIPRT*1.720 + PLIGRT*0.659 + POART*(-0.011) + PMINRT*RMIN + PCARRT*0.170)
      CPFNOD = 30./44.*(PLIPNO*1.720 + PLIGNO*0.659 + POANO*(-0.011) + PMINNO*RMIN + PCARNO*0.170)
      CPFSH1 = 30./44.*(PLIPSH*1.720 + PLIGSH*0.659 + POASH*(-0.011) + PMINSH*0.073 + PCARSH*0.170)
      CPFSD1 = 30./44.*(PMINSD*0.073 + PLIGSD*0.659 + POASD*(-0.011) + (SDLIP*1.720 + PCARSD*0.170)*(1. - SDPROR))
      #-----------------------------------------------------------------------
      #     CALCULATE PERCENT NITROGEN IN LEAVES AT END OF SEASON
      PCNMIN = PROLFF * 16.0            #Moved from INCOMP
      #-----------------------------------------------------------------------
    }
    
    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
  } else if (DYNAMIC == SEASINIT) {
    #-----------------------------------------------------------------------
    ALFDOT = 0.0
    AREALF = 0.0
    AREAH  = 0.0
    CANWAA = 0.0
    CANNAA = 0.0
    GROWTH = 0.0
    GRWRES = 0.0
    LAIMX  = 0.0
    NLDOT  = 0.0
    NSDOT  = 0.0
    NRDOT  = 0.0
    NSDDOT = 0.0
    NSHDOT = 0.0
    NTOVR  = 0.0
    PCCSD  = 0.0
    PCLSD  = 0.0
    PCNL   = 0.0
    PCNRT  = 0.0
    PCNSD  = 0.0
    PCNSH  = 0.0
    PCNST  = 0.0
    PODWT  = 0.0
    RHOL   = 0.0
    RHOR   = 0.0
    RHOS   = 0.0
    RHOSH  = 0.0
    RTWT   = 0.0
    SDRATE = 0.0
    SDWT   = 0.0
    SDWTAM = 0.0
    SEEDNI = 0.0
    SEEDNO = 0.0
    
    SenWt  = 0.0
    SenLig = 0.0
    SenE   = 0.0
    
    # VERIFICAR: Atribuição de uma operação?
    #SENESCE % ResWt  = 0.0
    #SENESCE % ResLig = 0.0
    #SENESCE % ResE   = 0.0
    #SENESCE % CumResE[N] = 0.0
    
    SHELWT = 0.0
    SLAAD  = 0.0
    STMWT  = 0.0
    TGROW  = 0.0
    TOPWT  = 0.0
    TOTWT  = 0.0
    WCRLF  = 0.0
    WCRRT  = 0.0
    WCRSH  = 0.0
    WCRST  = 0.0
    WNRLF  = 0.0
    WNRRT  = 0.0
    WNRSH  = 0.0
    WNRST  = 0.0
    WSDDOT = 0.0
    WSHDOT = 0.0
    WTCO   = 0.0
    WTCSD  = 0.0
    WTLF   = 0.0
    WTLO   = 0.0
    WTLSD  = 0.0
    WTMAIN = 0.0
    WTNCAN = 0.0
    WTNLA  = 0.0
    WTNLF  = 0.0
    WTNLO  = 0.0
    WTNMOB = 0.0
    WTNNA  = 0.0
    WTNNAG = 0.0
    WTNNO  = 0.0
    WTNNOD = 0.0
    WTNOO  = 0.0
    WTNRA  = 0.0
    WTNRO  = 0.0
    WTNRT  = 0.0
    WTNSA  = 0.0
    WTNSD  = 0.0
    WTNSDA = 0.0
    WTNSDO = 0.0
    WTNSH  = 0.0
    WTNSHA = 0.0
    WTNSHO = 0.0
    WTNSO  = 0.0
    WTNST  = 0.0
    WTNTOT = 0.0
    WTNUP  = 0.0
    WTRO   = 0.0
    WTSDO  = 0.0
    WTSHO  = 0.0
    WTSO   = 0.0
    XLAI   = 0.0
    XHLAI  = 0.0
    XPOD   = 0.0
    
    CLW = 0.0   #Cumulative leaf growth
    CSW = 0.0   #Cumulative stem growth
    
    ShutMob = 0.0
    RootMob = 0.0
    ShelMob = 0.0
    
    SDPDOT = 0.0    #CHP - not used
    PUNDOT = 0.0    #CHP - not used
    
    NLPEST = 0.0    #CHP - N loss due to pest damage
    
    #-----------------------------------------------------------------------
    # KJB 02/12/03 Set SLA to zero until emergence
    # JWJ 09/16/03 Set SLA to -99 until emergence
    # CHP 09/17/03 Do this for output only - SLA is used in calculations!
    SLA = 0.0
    #     Need to get initial value of F from DEMAND first  chp 9/14/98
    #      if (CROP != 'FA') {
    #        SLA    = F                 
    #      }
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == EMERG) {
    #-----------------------------------------------------------------------
    #     Net growth rates
    WLDOT  = 0.0
    WSDOT  = 0.0
    WRDOT  = 0.0
    WNDOT  = 0.0
    WPDOT  = 0.0
    WLFDOT = 0.0
    
    #     Initial seedling or transplant weight
    SDRATE  = WTPSD * PLTPOP / 0.8 * 10
    
    if (PLME != 'T') {
      WTNEW  = WTFSD * WTPSD
      TOTWT  = WTNEW * PLTPOP
    } else {
      TOTWT = SDWTPL / 10.
      WTNEW = TOTWT / PLTPOP
    }
    
    #     Initial mass of plant components
    WTLF  = FRLF * TOTWT
    STMWT = FRSTM * TOTWT
    TOPWT = WTLF + STMWT
    RTWT  = TOTWT - TOPWT
    
    WLFI   = WTLF
    WSTI   = STMWT
    WRTI   = RTWT
    
    #     Initialize cumulative variables
    CLW   = WTLF
    CSW   = STMWT
    
    #     CH2O reserves
    WCRLF = ALPHL * WTLF
    WCRST = ALPHS * STMWT
    WCRRT = ALPHR * RTWT
    
    #     Initialize cumulative C variables
    #      WCRLA = WCRLF
    #      WCRSA = WCRST
    #      WCRRA = WCRRT
    
    #     Carbohydrate composition of plant components (fraction)
    RHOL   = ALPHL
    RHOR   = ALPHR
    RHOS   = ALPHS
    RHOSH  = ALPHSH
    
    #     Net carbon addition variables
    WRCLDT = 0.0
    WRCSDT = 0.0
    WRCRDT = 0.0
    WRCSHD = 0.0
    
    #     Compute N in plant components
    WTNLF  = WLFI * PROLFI * 0.16
    WTNST  = WSTI * PROSTI * 0.16
    WTNRT  = WRTI * PRORTI * 0.16
    WTNSH  = 0.0
    WTNSD  = 0.0
    WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD
    
    #     Seed or transplant N at planting
    SDNPL  = WTPSD * SDPRO * 0.16 * 0.75 * PLTPOP - (WTNLF + WTNST + WTNRT)
    SDNPL  = max(SDNPL,0.0)
    SEEDNI = WTNLF + WTNST + WTNRT + SDNPL
    
    #     Initialize cumulative N variables
    WTNLA  = WTNLF
    WTNSA  = WTNST
    WTNRA  = WTNRT
    
    #     Percent N in plant components
    PCNL   = WTNLF / WLFI * 100.
    RNITP  = PCNL
    PCNST  = WTNST / WSTI * 100.
    PCNRT  = WTNRT / WRTI * 100.
    
    # VERIFICAR: Mudar F.
    #     Intialize leaf area.  Value of F initialized in DEMAND.
    AREALF = WTLF * Fnew  #*** Fnew is 'F' in the original file. Changed because F is logical in R. ***
    AREAH  = AREALF
    SLA    = AREALF / WTLF
    SLAAD  = AREALF / (WTLF - WCRLF)
    XLAI   = AREALF / 10000.
    XHLAI  = XLAI
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == INTEGR) {
    #-----------------------------------------------------------------------
    NLPEST = 0.0    #CHP - N loss due to pest damage
    
    GROWTH = WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
    
    GRWRES = WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN*CPFRT + NODGR*CPFNOD + WSHDTN*CPFSH1 + WSDDTN*CPFSD1 + 30./44 *(TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 + NFIXN/0.16 * 1.798)
    
    #-----------------------------------------------------------------------
    #    Use 1982 Penning de Vries book, p. 125, composition of tissue,
    #    fraction C for six categories, convert by 30/12 to CH20.
    #-----------------------------------------------------------------------
    #     Account for N added to existing plant tissue, e.g., NADLF, that
    #     is damaged by insects, freezing, or senesced.  Otherwise, could
    #     get increase in tissue N composition when tissue is aborted.  Need
    #     to account for mass, N and C lost this way in sections below
    #-----------------------------------------------------------------------
    #       WLDOT = Net leaf growth rate
    #-----------------------------------------------------------------------
    WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT - NRUSLF/0.16 - CRUSLF
    
    #     ShutMob is amount of leaf mass lost due to N and C mobilization
    #     A positive value represents leaf mass lost. (kg/ha)
    ShutMob = (NRUSLF/0.16 + CRUSLF) * 10.      #kg/ha
    
    if (WTLF > 1.E-4) {
      WLDOT = WLDOT + (CADLF+NADLF/0.16) * (1. - min(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ADD = (CADLF+NADLF/0.16) * (1. - min(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ShutMob = ShutMob - ADD * 10.             #kg/ha
    } else {
      ADD = 0.0
    }
    if (WLDOT < 0.0) {
      WLDOT = max(WLDOT, -WTLF)
    }
    
    #-----------------------------------------------------------------------
    #       WSDOT = Net stem growth rate
    #-----------------------------------------------------------------------
    WSDOT = WSDOTN - SSDOT - WSIDOT - NRUSST / 0.16 - CRUSST
    ShutMob = ShutMob + (NRUSST / 0.16 + CRUSST) * 10.      #kg/ha
    
    if (STMWT > 1.E-4) {
      WSDOT = WSDOT + (CADST+NADST/0.16) * (1. - min(1.0,(SSDOT+WSIDOT)/STMWT))
      ADD = (CADST+NADST/0.16) * (1. - min(1.0,(SSDOT+WSIDOT)/STMWT))
      ShutMob = ShutMob - ADD * 10.                         #kg/ha
    } else {
      ADD = 0.
    }
    if (WSDOT < 0.0) {
      WSDOT = max(WSDOT, -STMWT)
    }
    #-----------------------------------------------------------------------
    #     Net root growth rate
    #-----------------------------------------------------------------------
    WRDOT = WRDOTN - SRDOT - NRUSRT/0.16 - CRUSRT - WRIDOT
    RootMob = (NRUSRT/0.16 + CRUSRT) * 10.      #kg/ha
    
    if (RTWT > 1.E-4) {
      WRDOT = WRDOT + (NADRT/0.16) * (1. - min(1.0,(SRDOT+WRIDOT)/RTWT))
      ADD = (NADRT/0.16) * (1. - min(1.0,(SRDOT+WRIDOT)/RTWT))
      RootMob = RootMob - ADD * 10.             #kg/ha
    } else {
      ADD = 0.0
    }
    
    if (WRDOT < 0.0) {
      WRDOT = max(WRDOT, -RTWT)
    }
    #-----------------------------------------------------------------------
    #     Net shell growth rate
    #-----------------------------------------------------------------------
    WSHIDT = min(WSHIDT,SHELWT)     # pest damage to shells
    WSHDOT = WSHDTN - WSHIDT - WTABRT - NRUSSH / 0.16 - CRUSSH
    ShelMob = (NRUSSH / 0.16 + CRUSSH) * 10.    #kg/ha
    
    #-----------------------------------------------------------------------
    #     Net seed growth rate
    #-----------------------------------------------------------------------
    SWIDOT = min(SWIDOT,SDWT)       # pest damage to seeds
    WSDDOT = WSDDTN - SWIDOT        
    WTLSD  = WTLSD + WSDDOT * POTLIP    #lipids in seed
    WTCSD  = WTCSD + WSDDOT * POTCAR    #carbohydrates in seed
    
    #-----------------------------------------------------------------------
    #     Net nodule growth rate
    #-----------------------------------------------------------------------
    WNDOT  = NODGR - NDTH            
    
    #-----------------------------------------------------------------------
    #     Net pod growth rate
    #-----------------------------------------------------------------------
    WPDOT = WSHDOT + WSDDOT          
    
    #-----------------------------------------------------------------------
    #     Total Net plant growth rate
    #-----------------------------------------------------------------------
    WDOT = WLDOT + WSDOT + WRDOT + WPDOT + WNDOT 
    
    #-----------------------------------------------------------------------
    if (GROWTH > 1.E-4) {
      if (XPODF == 'PD') {
        XPOD = 0.17 * (WSDDTN+WSHDTN)/GROWTH + 0.83 * XPOD
      } else if (XPODF == 'SD') {
        XPOD = 0.17 * (WSDDTN)/GROWTH + 0.83 * XPOD
      } else {
        #CALL ERROR(ERRKEY,1,'      ',0)
      }
    }
    #-----------------------------------------------------------------------
    #    Integration, Add Today's Net Growth to Existing Weights
    #-----------------------------------------------------------------------
    TOTWT  = TOTWT  + WDOT
    TOPWT  = TOPWT  + WSDOT + WLDOT + WPDOT
    WTLF   = WTLF   + WLDOT
    STMWT  = STMWT  + WSDOT
    SDWT   = SDWT   + WSDDOT
    SHELWT = SHELWT + WSHDOT
    RTWT   = RTWT   + WRDOT
    PODWT  = PODWT  + WPDOT
    DWNOD  = DWNOD  + WNDOT
    TGROW  = TGROW  + GROWTH
    
    #-----------------------------------------------------------------------
    #     Cumulative leaf and stem growth
    #-----------------------------------------------------------------------
    CLW = CLW + WLDOTN      
    CSW = CSW + WSDOTN      
    
    #-----------------------------------------------------------------------
    #     Store values of TOPWT at flowering and SDWT at maturity
    #-----------------------------------------------------------------------
    if (YRDOY == YRNR1) {
      CANWAA = TOPWT
    } else if (YRDOY == MDATE) {
      SDWTAM = SDWT
    }
    #---------------------------------------------------------------------
    #     Compute Seed Weight for Which there is Maintenance Costs.
    #-----------------------------------------------------------------------
    WSDMAN = min(SDWT,SHELWT)
    WTMAIN = TOTWT - SDWT + WSDMAN
    
    #-----------------------------------------------------------------------
    #     Carbon Reserves:  Net Growth Rates for Mobile Carbohydrates
    #-----------------------------------------------------------------------
    #     Account for N added to existing plant tissue, e.g., NADLF, that
    #     is damaged by insects, freezing, or senesced.  Otherwise, could
    #     get increase in tissue N composition when tissue is aborted.  Need
    #     to account for mass, N and C lost this way in sections below
    #-----------------------------------------------------------------------
    WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLNDOT+WLIDOT+WLFDOT)
    if (WTLF > 1.E-4) {
      WRCLDT = WRCLDT + CADLF * (1. - min(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
    }
    
    WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT
    if (STMWT > 1.E-4) {
      WRCSDT = WRCSDT + CADST * (1. - min(1.0,(SSDOT+WSIDOT)/STMWT))
    }
    
    WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT
    WRCSHD = ALPHSH * WSHDOT - CRUSSH - RHOSH*(WTABRT+WTSHMT+WSHIDT)
    #-----------------------------------------------------------------------
    #     Update C Storage, Concentrations in Leaves, Stems, Roots, and Shells
    #-----------------------------------------------------------------------
    WCRLF = WCRLF + WRCLDT
    WCRST = WCRST + WRCSDT
    WCRRT = WCRRT + WRCRDT
    WCRSH = WCRSH + WRCSHD
    
    if (WCRLF <= 1.0E-30) WCRLF = 0.0
    if (WCRST <= 1.0E-30) WCRST = 0.0
    if (WCRRT <= 1.0E-30) WCRRT = 0.0
    if (WCRSH <= 1.0E-30) WCRSH = 0.0
    
    #-----------------------------------------------------------------------
    #     Compute Cumulative C loss During Season
    #-----------------------------------------------------------------------
    WTLO  = WTLO  + SLDOT  + WLIDOT + WLFDOT
    WTSO  = WTSO  + SSDOT  + WSIDOT
    WTRO  = WTRO  + SRDOT  + WRIDOT
    WTSHO = WTSHO + WTABRT + WSHIDT
    WTSDO = WTSDO + SWIDOT
    WTNOO = WTNOO + NDTH
    WTCO  = WTLO  + WTSO   + WTSHO  + WTSDO
    
    #-----------------------------------------------------------------------
    #     Compute CH20 fractions
    #-----------------------------------------------------------------------
    if (WTLF > 0.0001) {
      RHOL =  WCRLF/WTLF
    } else {
      RHOL = 0.0
    }
    
    if (STMWT > 0.0001) {
      RHOS =  WCRST/STMWT
    } else {
      RHOS = 0.0
    }
    
    if (RTWT > 0.0001) {
      RHOR =  WCRRT/RTWT
    } else {
      RHOR = 0.0
    }
    
    if (SHELWT > 0.0001) {
      RHOSH =  WCRSH/SHELWT
    } else {
      RHOSH = 0.0
    }
    
    #=======================================================================
    #    Nitrogen Balance:  Net Growth Rates for Nitrogen Components
    #=======================================================================
    #     Account for N added to existing plant tissue, e.g., NADLF, that
    #     is damaged by insects, freezing, or senesced.  Otherwise, could
    #     get increase in tissue N composition when tissue is aborted.  Need
    #     to account for mass, N and C lost this way in sections below
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    #     Leaf nitrogen senescence and pest damage loss
    #-----------------------------------------------------------------------
    #              senesc. + pest   + freeze  
    NLOFF  = (SLNDOT + WLIDOT + WLFDOT) * (PCNL/100.) + (SLDOT-SLNDOT) * PROLFF * 0.16
    NLPEST = NLPEST + WLIDOT * PCNL/100.
    
    # ALTERADO: .LT. to <
    if (NLOFF < 0.0) {
      NLOFF = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the leaves
    #-----------------------------------------------------------------------
    NLDOT = NGRLF - NLOFF - NRUSLF
    if (WTLF > 1.E-4) {
      NLDOT = NLDOT + NADLF * (1. - min(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
    }
    
    #-----------------------------------------------------------------------
    #     Stem nitrogen senescence and pest damage loss
    #-----------------------------------------------------------------------
    #              senesce + pest
    NSOFF  = (SSNDOT + WSIDOT) * (PCNST/100.) + (SSDOT - SSNDOT) * PROSTF * 0.16
    NLPEST = NLPEST + WSIDOT * PCNST/100.
    if (NSOFF < 0.0) {
      NSOFF = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the stems
    #-----------------------------------------------------------------------
    NSDOT = NGRST - NSOFF - NRUSST
    if (STMWT > 1.E-4) {
      NSDOT = NSDOT + NADST * (1. - min(1.0,(SSDOT + WSIDOT) / STMWT))
    }
    
    #-----------------------------------------------------------------------
    #     Root nitrogen senescence and pest damage loss
    #-----------------------------------------------------------------------
    NROFF  = (SRDOT+WRIDOT) * (PCNRT/100.)
    if (NROFF < 0.0) {
      NROFF = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the roots
    #-----------------------------------------------------------------------
    NRDOT = NGRRT - NROFF - NRUSRT
    if (RTWT > 1.E-4) {
      NRDOT = NRDOT + NADRT * (1. - min(1.0,(SRDOT + WRIDOT) / RTWT))
    }
    
    #-----------------------------------------------------------------------
    #     Shell nitrogen senescence, abortion and pest damage loss
    #-----------------------------------------------------------------------
    NSHOFF = (WTABRT+WSHIDT) * (PCNSH/100.)
    NLPEST = NLPEST + WSHIDT * PCNSH/100.
    if (NSHOFF < 0.0) {
      NSHOFF = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in shells
    #-----------------------------------------------------------------------
    NSHDOT = NGRSH - NSHOFF - NRUSSH
    
    #-----------------------------------------------------------------------
    #     Seed nitrogen senescence, abortion and pest damage loss
    #-----------------------------------------------------------------------
    NSDOFF = SWIDOT * PCNSD/100.
    if (NSDOFF < 0.0) {
      NSDOFF = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in seeds
    #-----------------------------------------------------------------------
    NSDDOT = NGRSD - NSDOFF
    
    #-----------------------------------------------------------------------
    #   Integration, Update N in Each Component by Adding Today's Growth
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the leaves
    #-----------------------------------------------------------------------
    # ALTERADO: ABS to abs(). Se repetia, mas só comentei aqui pra não duplicar.
    if ((NLDOT < 0.0) & (abs(NLDOT) > WTNLF)) {
      NLDOT = - WTNLF
    }
    WTNLF = WTNLF + NLDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the stems
    #-----------------------------------------------------------------------
    if ((NSDOT < 0.0) & (abs(NSDOT) > WTNST)) {
      NSDOT = - WTNST
    }
    WTNST = WTNST + NSDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the roots
    #-----------------------------------------------------------------------
    if ((NRDOT < 0.0) & (abs(NRDOT) > WTNRT)) {
      NRDOT = - WTNRT
    }
    WTNRT = WTNRT + NRDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the nodules
    #-----------------------------------------------------------------------
    WTNNOD = DWNOD * 0.16 * PRONOD
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the shells
    #-----------------------------------------------------------------------
    if ((NSHDOT < 0.0) & (abs(NSHDOT) > WTNSH)) {
      NSHDOT = - WTNSH
    }
    WTNSH = WTNSH + NSHDOT
    
    if ((NSDDOT < 0.0) & (abs(NSDDOT) > WTNSD)) {
      NSDDOT = - WTNSD
    }
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the seed
    #-----------------------------------------------------------------------
    WTNSD = WTNSD + NSDDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the plant
    #-----------------------------------------------------------------------
    WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNNOD
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the canopy (all above ground components)
    #-----------------------------------------------------------------------
    WTNCAN = WTNLF + WTNST + WTNSH + WTNSD
    
    #-----------------------------------------------------------------------
    #     Save total nitrogen in the canopy at the start of flowering
    #-----------------------------------------------------------------------
    if (YRDOY == YRNR1) {
      CANNAA = WTNCAN
    }
    
    #-----------------------------------------------------------------------
    #     Compute Cumulative N added during season
    #-----------------------------------------------------------------------
    #   CHP - working on Plant N balance:
    #   Add adjustment factors for N addition.  
    #     These factors are used in computation of net addition to 
    #     tissue, but previously not here.  Adding these statements
    #     forces cumulative N additions to equal 'current N' plus 
    #     'senesced N'.  
    
    #   However . . .  by making this change, cumulative tissue N no longer
    #     matches 'Seed N' plus 'N2 fixed' plus 'N uptake'.  Why???
    
    #     Leaf
    NLALL = NGRLF - NRUSLF 
    if (WTLF > 1.E-4) {
      NLALL = NLALL + NADLF * (1. - min(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
    }
    
    #     Stem
    NSALL = NGRST - NRUSST 
    if (STMWT > 1.E-4) {
      NSALL = NSALL + NADST * (1. - min(1.0,(SSDOT + WSIDOT) / STMWT))
    }
    
    #     Root
    NRALL = NGRRT - NRUSRT
    if (RTWT > 1.E-4) {
      NRALL = NRALL + NADRT * (1. - min(1.0,(SRDOT + WRIDOT) / RTWT))
    }
    
    #     Shell and seed
    NSHALL = NGRSH - NRUSSH
    NSDALL = NGRSD
    
    WTNLA = WTNLA + NLALL
    WTNSA = WTNSA + NSALL
    WTNRA = WTNRA + NRALL
    WTNSHA = WTNSHA + NSHALL
    WTNSDA = WTNSDA + NSDALL
    #-----------------------------------------------------------------------
    #    Nodules
    #-----------------------------------------------------------------------
    if (ISWNIT == 'Y' & ISWSYM == 'Y') {
      DWNODA = DWNODA + NODGR
      WTNFX = WTNFX + NFIXN + (NODGR * 0.16 * PRONOD) - NTOVR
      WTNNAG = DWNODA * 0.16 * PRONOD
    }
    #-----------------------------------------------------------------------
    #     Compute Cumulative N loss During Season
    #-----------------------------------------------------------------------
    WTNLO  = WTNLO  + NLOFF
    WTNSO  = WTNSO  + NSOFF
    WTNRO  = WTNRO  + NROFF
    WTNSHO = WTNSHO + NSHOFF
    WTNSDO = WTNSDO + NSDOFF
    if (ISWNIT == 'Y' & ISWSYM == 'Y') {
      NNOFF  = NDTH * 0.16 * PRONOD
      WTNNO  = WTNNO  + NNOFF
    }
    #-----------------------------------------------------------------------
    #     N Balance Components for Mobilized, Uptake, and Fixed N
    #-----------------------------------------------------------------------
    WTNMOB = WTNMOB + NMINEA
    WTNUP = WTNUP + TRNU
    WTNNA = WTNNOD + WTNNO
    #-----------------------------------------------------------------------
    #     Compute Percentage N in each Plant Component
    #-----------------------------------------------------------------------
    if ((WTLF > 1.E-4) & (WTLF > WTNLF) & (WTNLF > 1.E-5)) {
      PCNL = WTNLF / WTLF * 100.0
    } else {
      PCNL = 0.0
    }
    
    if ((STMWT > 1.E-4) & (WTNST > 1.E-5)) {
      PCNST = WTNST / STMWT * 100.0
    } else {
      PCNST = 0.0
    }
    
    if ((RTWT > 1.E-4) & (WTNRT > 1.E-5)) {
      PCNRT = WTNRT / RTWT * 100.0
    } else {
      PCNRT = 0.0
    }
    
    if ((SHELWT > 1.E-4) & (WTNSH > 1.E-5)) {
      PCNSH = WTNSH / SHELWT * 100.0
    } else {
      PCNSH = 0.0
    }
    #-----------------------------------------------------------------------
    #     Compute Percentage Seed Composition
    #-----------------------------------------------------------------------
    if (SDWT > 0.01) {
      PCNSD = WTNSD / SDWT * 100.0
      PCLSD = WTLSD / SDWT * 100.0
      PCCSD = WTCSD / SDWT * 100.0
    } else {
      PCNSD = 0.0
      PCLSD = 0.0
      PCCSD = 0.0
    }
    #-----------------------------------------------------------------------
    #    Calculate true nitrogen concentration in leaf tissue for
    #     photosynthesis reduction.
    #-----------------------------------------------------------------------
    if ((WTLF - WCRLF) > 1.E-4) {
      RNITP = 100.*WTNLF/(WTLF-WCRLF)
    } else {
      RNITP = PCNMIN
    }
    #-----------------------------------------------------------------------
    #     Calculate Remaining N in Shells, Leaves, Stems, and Roots
    #     That can be Mined (Plant N-Balance).
    #-----------------------------------------------------------------------
    if ((WTLF - WCRLF) > 1.E-4) {
      WNRLF = max(WTNLF - PROLFF * 0.16 * (WTLF-WCRLF), 0.0)
    } else {
      WNRLF = 0.0
    }
    
    if ((STMWT - WCRST) > 1.E-4) {
      WNRST = max(WTNST - PROSTF * 0.16 * (STMWT-WCRST), 0.0)
    } else {
      WNRST = 0.0
    }
    
    if (RTWT > 1.E-4) {
      WNRRT = max(WTNRT - PRORTF * 0.16 * RTWT, 0.0)
    } else {
      WNRRT = 0.0
    }
    
    if (SHELWT > 1.E-4) {
      WNRSH = max(WTNSH - PROSHF * 0.16 * SHELWT, 0.0)
    } else {
      WNRSH = 0.0
    }
    
    #-----------------------------------------------------------------------
    #     This section added to provide senescence parameters to the 
    #     Soil N routines (senescence and freeze values are added to soil
    #     residue; pest damage components are lost from the system)
    #-----------------------------------------------------------------------
    #     Surface carbon includes all senescence and freeze variables for
    #      leaf (SLDOT, WLFDOT), stem (SSDOT), and shell (WTABRT)
    #       At this time, the model does not senesce seed.   
    #     Convert from biomass to C with a factor 0.40.   
    #-----------------------------------------------------------------------
    SenWt[0] = (SLDOT + WLFDOT + SSDOT + WTABRT)
    #     Convert from g/m2 to kg/ha with a factor of 10.
    SenWt[0] = max(SenWt[0], 0.) * 10.0    #kg[dry matter]/ha
    
    #-----------------------------------------------------------------------
    #     Surface nitrogen includes nitrogen losses computed above minus the
    #       pest damage components. 
    #-----------------------------------------------------------------------
    # VERIFICAR: SenE é uma variável? Se sim, deve usar []
    SenE(0,1) = NLOFF + NSOFF + NSHOFF - NLPEST  
    #     Convert from g/m2 to kg/ha with a factor of 10.
    SenE(0,1) = max(SenE(0,1),0.) * 10.0             #kg[N]/ha
    
    #-----------------------------------------------------------------------
    #     Surface phosphorus senescence.  - do this is P routine
    #-----------------------------------------------------------------------
    #      SenE(0,2) = SenWt(0) * PConc_Shut             !kg[N]/ha
    
    #-----------------------------------------------------------------------
    #     Contribution of lignin to surface litter from senesced and frozen 
    #       plant matter
    #-----------------------------------------------------------------------
    SenLig[0] = (SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST + WTABRT * PLIGSH
    #     Convert from g/m2 to kg/ha with a factor of 10.
    SenLig[0] = max(SenLig[0],0.) * 10.0             #kg[lig]/ha
    
    #-----------------------------------------------------------------------
    #     Senescence of roots and nodules (kg/ha)
    #-----------------------------------------------------------------------
    for (L in 1:NLAYR) {
      SenWt[L]  = SENRT[L] + SENNOD[L]            #kg[dry matter]/ha
      SenLig[L] = SENRT[L] * PLIGRT + SENNOD[L] * PLIGNO   #kg[lig]/ha
      #SenE(L,1) = (SENRT[L]* PRORTF + SENNOD[L] * PRONOD) * 0.16 
      #       01/19/2006 CHP Root senescence at current N% (per KJB and JWJ)
      SenE[L,1] = SENRT[L] * PCNRT / 100. + SENNOD[L] * PRONOD * 0.16 
    }
    
    #-----------------------------------------------------------------------
    #     Leaf Area, Specific Leaf Area Calculations
    #     Calculate Area Growth rate and Update Leaf Area (AREALF, XLAI)
    #-----------------------------------------------------------------------
    # VERIFICAR: variável F
    ALFDOT = WLDOTN*F - (SLDOT+WLIDOT+WLFDOT+NRUSLF/0.16)*SLA
    if (WTLF > 1.E-4) {
      ALFDOT = ALFDOT + SLA * (NADLF/0.16) * (1. - min(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
    }
    AREALF = AREALF + ALFDOT
    XLAI   = AREALF / 10000.
    if (AREALF > 0.00001 & WTLF > 1.E-4) {
      SLA    = AREALF / WTLF
      SLAAD  = AREALF / (WTLF - WCRLF)
      if (SLA > 999.) SLA = 0.0
      if (SLAAD > 999.) SLAAD = -99.
    }
    #-----------------------------------------------------------------------
    #     Remember XLAI
    #-----------------------------------------------------------------------
    LAIMX = max(XLAI,LAIMX)
    #-----------------------------------------------------------------------
    #     Calculate "Healthy" or Non-Diseased Leaf Area Index
    #-----------------------------------------------------------------------
    #     AREAH  = AREALF - 2. * DISLA
    #     KJB Remove 2. factor
    AREAH  = AREALF - DISLA
    AREAH  = max(0.,AREAH)
    XHLAI  = AREAH / 10000.
    #-----------------------------------------------------------------------
    #     Integrate Pest Damage to Seeds
    #-----------------------------------------------------------------------
    PUNCSD = PUNCSD + SDPDOT           #chp not used
    SEEDNO = SEEDNO - SDIDOT
    PUNCTR = PUNCTR + PUNDOT           #chp not used
    #-----------------------------------------------------------------------
    #     Loss in Plants due to Pests, Adjust Plant Spacing Also
    #-----------------------------------------------------------------------
    if (NPLTD > 1.E-5) {
      PLTPOP = PLTPOP - NPLTD
      PLTPOP = max(0.,PLTPOP)
      if (PLTPOP*ROWSPC > 1.E-4) {
        BETN = 1.0/(ROWSPC*PLTPOP)
      }
    }
    
    if (PPLTD > 1.E-4) {
      PLTPOP = PLTPOP - PPLTD*PLTPOP/100.
      PLTPOP = max(0.,PLTPOP)
      if(PLTPOP*ROWSPC > 1.E-4) {
        BETN = 1.0/(ROWSPC*PLTPOP)
      }
    }
    
    #-----------------------------------------------------------------------
    #     Terminate growth if stress causes extremely low plant weights
    #-----------------------------------------------------------------------
    if (TOPWT < 0.00001 | STMWT < 0.00001) {
      STRESS(
             AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,     #!Input
             RTWT, SDWT, SHELWT, STMWT, TOPWT,               #!Input
             TOTWT, TURFAC, WTLF, YRDOY, YRPLT,              #!Input
             MDATE)                                          #!Output
      return()
    }
    
    if (IHARI != 'R' & IHARI != 'D') {
      if (RTWT  < 0.00001 | WTLF  < 0.00001) {
        STRESS(
          AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,     #!Input
          RTWT, SDWT, SHELWT, STMWT, TOPWT,               #!Input
          TOTWT, TURFAC, WTLF, YRDOY, YRPLT,              #!Input
          MDATE)   
        
        # ALTERADO: RETURN to return()
        return()
      }
    }
    
    # VERIFICAR: Novamente expressão com operação a esquerda.
    #SENESCE % ResWt  = SenWt
    #SENESCE % ResLig = SenLig
    
    # VERIFICAR: Começa indice començando com 0 e expressão a esquerda. No R o indexamento começa sempre do 1.
    for (L in 0:NLAYR) {
      #SENESCE % ResE[L,N]  = SenE[L,N] #TODO verificar sintaxe e conexao com demais subrotinas
      #        SENESCE % ResE(L,P)  = SenE(L,P)
      
      #        This is being done in OpSoilOrg:
      #        SENESCE % CumResWt   = SENESCE % CumResWt + SenWt(L)
      #        SENESCE % CumResE(N) = SENESCE % CumResE(N) + SenE(L,N)
      
      #        Do this in P module:
      #        SENESCE % CumResE(P) = SENESCE % CumResE(P) + SenE(L,P)
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #-----------------------------------------------------------------------
  assign("SWIDOT", SWIDOT, envir = env)
  assign("WLFDOT", WLFDOT, envir = env)
  assign("WSHIDT", WSHIDT, envir = env)
  assign("WTNFX", WTNFX, envir = env)
  assign("XHLAI", XHLAI, envir = env)
  assign("AREALF", AREALF, envir = env)
  assign("BETN", BETN, envir = env)
  assign("CANNAA", CANNAA, envir = env)
  assign("CANWAA", CANWAA, envir = env)
  assign("CLW", CLW, envir = env)
  assign("CSW", CSW, envir = env)
  assign("DWNOD", DWNOD, envir = env)
  assign("DWNODA", DWNODA, envir = env)
  assign("GROWTH", GROWTH, envir = env)
  assign("GRWRES", GRWRES, envir = env)
  assign("LAIMX", LAIMX, envir = env)
  assign("PCCSD", PCCSD, envir = env)
  assign("PCLSD", PCLSD, envir = env)
  assign("PCNL", PCNL, envir = env)
  assign("PCNRT", PCNRT, envir = env)
  assign("PCNSD", PCNSD, envir = env)
  assign("PCNSH", PCNSH, envir = env)
  assign("PCNST", PCNST, envir = env)
  assign("PLTPOP", PLTPOP, envir = env)
  assign("PLIGLF", PLIGLF, envir = env)
  assign("PLIGNO", PLIGNO, envir = env)
  assign("PLIGRT", PLIGRT, envir = env)
  assign("PLIGSD", PLIGSD, envir = env)
  assign("PLIGSH", PLIGSH, envir = env)
  assign("PLIGST", PLIGST, envir = env)
  assign("PODWT", PODWT, envir = env)
  assign("PUNCSD", PUNCSD, envir = env)
  assign("PUNCTR", PUNCTR, envir = env)
  assign("RHOL", RHOL, envir = env)
  assign("RHOS", RHOS, envir = env)
  assign("RNITP", RNITP, envir = env)
  assign("ROWSPC", ROWSPC, envir = env)
  assign("RTWT", RTWT, envir = env)
  assign("SDNPL", SDNPL, envir = env)
  assign("SDRATE", SDRATE, envir = env)
  assign("SDWT", SDWT, envir = env)
  assign("SEEDNI", SEEDNI, envir = env)
  assign("SEEDNO", SEEDNO, envir = env)
  assign("SENESCE", SENESCE, envir = env)
  assign("SHELWT", SHELWT, envir = env)
  assign("SLA", SLA, envir = env)
  assign("SLAAD", SLAAD, envir = env)
  assign("STMWT", STMWT, envir = env)
  assign("TOPWT", TOPWT, envir = env)
  assign("TOTWT", TOTWT, envir = env)
  assign("WCRLF", WCRLF, envir = env)
  assign("WCRRT", WCRRT, envir = env)
  assign("WCRSH", WCRSH, envir = env)
  assign("WCRST", WCRST, envir = env)
  assign("WNRLF", WNRLF, envir = env)
  assign("WNRRT", WNRRT, envir = env)
  assign("WNRSH", WNRSH, envir = env)
  assign("WNRST", WNRST, envir = env)
  assign("WTCO", WTCO, envir = env)
  assign("WTLF", WTLF, envir = env)
  assign("WTLO", WTLO, envir = env)
  assign("WTMAIN", WTMAIN, envir = env)
  assign("WTNCAN", WTNCAN, envir = env)
  assign("WTNEW", WTNEW, envir = env)
  assign("WTNLA", WTNLA, envir = env)
  assign("WTNLF", WTNLF, envir = env)
  assign("WTNLO", WTNLO, envir = env)
  assign("WTNNA", WTNNA, envir = env)
  assign("WTNNAG", WTNNAG, envir = env)
  assign("WTNNO", WTNNO, envir = env)
  assign("WTNNOD", WTNNOD, envir = env)
  assign("WTNOO", WTNOO, envir = env)
  assign("WTNRA", WTNRA, envir = env)
  assign("WTNRO", WTNRO, envir = env)
  assign("WTNRT", WTNRT, envir = env)
  assign("WTNSA", WTNSA, envir = env)
  assign("WTNSD", WTNSD, envir = env)
  assign("WTNSDA", WTNSDA, envir = env)
  assign("WTNSDO", WTNSDO, envir = env)
  assign("WTNSH", WTNSH, envir = env)
  assign("WTNSHA", WTNSHA, envir = env)
  assign("WTNSHO", WTNSHO, envir = env)
  assign("WTNSO", WTNSO, envir = env)
  assign("WTNST", WTNST, envir = env)
  assign("WTNUP", WTNUP, envir = env)
  assign("WTRO", WTRO, envir = env)
  assign("WTSDO", WTSDO, envir = env)
  assign("WTSHO", WTSHO, envir = env)
  assign("WTSO", WTSO, envir = env)
  assign("XLAI", XLAI, envir = env)
  assign("XPOD", XPOD, envir = env)
  assign("ShutMob", ShutMob, envir = env)
  assign("RootMob", RootMob, envir = env)
  assign("ShelMob", ShelMob, envir = env)
  # ALTERADO: RETURN to return()
  return()
}  # SUBROUTINE GROW
#=======================================================================

#=======================================================================
#  STRESS, Subroutine, from code in GROW subroutine
#-----------------------------------------------------------------------
#  Plant death due to stress
#-----------------------------------------------------------------------
#  REVISION        HISTORY
#  09/18/1998 CHP  Written based on code in GROW subroutine
#-----------------------------------------------------------------------
#  Called by:  GROW
#  Calls:      None
#=======================================================================
# SUBROUTINE STRESS(
#&  AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,       !Input
#&  RTWT, SDWT, SHELWT, STMWT, TOPWT,                 !Input
#&  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,                !Input
#&  MDATE)                                            !Output

simDataVars$MDATE  <-  0

STRESS <- function(
  AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,       #!Input
  RTWT, SDWT, SHELWT, STMWT, TOPWT,                 #!Input
  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,                #!Input
  MDATE) {                                           #!Output
  #-----------------------------------------------------------------------
  #IMPLICIT NONE
  #SAVE
  #-----------------------------------------------------------------------
  #TODO VERIFICAR
  #CHARACTER*1  IDETO, IHARI
  #INTEGER NOUTDO, YRDOY, YRPLT, MDATE, DAP, TIMDIF
  #INTEGER YR, DOY

  #-----------------------------------------------------------------------
  #     Set Ending Plant Weights, Dates of Stages if Plants Died
  #-----------------------------------------------------------------------
  TOTWT  = max(0.,TOTWT)
  TOPWT  = max(0.,TOPWT)
  WTLF   = max(0.,WTLF)
  STMWT  = max(0.,STMWT)
  SDWT   = max(0.,SDWT)
  SHELWT = max(0.,SHELWT)
  RTWT   = max(0.,RTWT)
  PODWT  = max(0.,PODWT)
  DWNOD  = max(0.,DWNOD)
  
  if (MDATE < 0) {
    #        NR8   = max(0,TIMDIF(YRSIM,YRDOY))
    MDATE = YRDOY
  }
  #-----------------------------------------------------------------------
  #if (IHARI == 'M') {
  #  DAP   = max(0,TIMDIF(YRPLT,YRDOY)) #TODO tradução timdif 
  #  
  #  !Message to WARNING.OUT
  #  CALL YR_DOY(YRDOY, YR, DOY)
  #  WRITE(MESSAGE(1), 100) DAP
  #  WRITE(MESSAGE(2), 110) YR, DOY, TURFAC, AGEFAC
  #  CALL WARNING(2,'CRPGRO', MESSAGE)
  #  100 FORMAT('Plant died due to extreme stress at ', I3, ' days after planting.')
  #  110 FORMAT('(DAY : ',I4,1X,I3,'; TURFAC : ',F5.2,'; AGEFAC : ', F5.2,'  )')
  #  
  #  #       Message to screen
  #  #       WRITE (*,275) MESSAGE(1), MESSAGE(2)
  #  #       Message to Overview.out
  #  if (IDETO == 'Y') {
  #    WRITE (NOUTDO,275) MESSAGE(1), MESSAGE(2)
  #  }
  #  275   FORMAT(/,2X,A78,/,2X,A78)
  #}
  #-----------------------------------------------------------------------
  assign("MDATE", MDATE, envir = env)
  # ALTERADO: RETURN to return()
  return()
}  # SUBROUTINE STRESS
#=======================================================================

#=======================================================================
#  IPGROW, Subroutine, C. H. Porter
#-----------------------------------------------------------------------
#  Reads input data for GROW routines.
#----------------------------------------------------------------------
#  REVISION HISTORY
#  09/15/1998 CHP Written
#  08/12/2003 CHP Added I/O error checking
#  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
#-----------------------------------------------------------------------
#  Called : GROW
#  Calls  : FIND, ERROR, IGNORE
#=======================================================================
#SUBROUTINE IPGROW(
#  &  FILEIO, FILECC,  CROP,                            !Input
#  &  ALPHL,  ALPHR,  ALPHS,  ALPHSH,                   !Output
#  &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,   !Output
#  &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,   !Output
#  &  PLIPLF, PLIPST, PLIPRT, PLIPSH,         PLIPNO,   !Output
#  &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,   !Output
#  &  POALF,  POAST,  POART,  POASH,  POASD,  POANO,    !Output
#  &  PROLFF, PROSTF, PRORTF, PROSHF,         PRONOD,   !Output
#  &  PROLFI, PROSTI, PRORTI,                           !Output
#  &  PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,           !Output
#  &  SDLIP,  SDPRO,  WTFSD,  WTPSD,   XPODF)           !Output
#
##-----------------------------------------------------------------------
#IMPLICIT NONE
#SAVE
##-----------------------------------------------------------------------
#CHARACTER*1 PLME, UPCASE
#CHARACTER*2 XPODF, CROP
#CHARACTER*6 ERRKEY
#PARAMETER (ERRKEY = 'IPGROW')
#CHARACTER*6 SECTION, ECONO  !, ECOTYP
#CHARACTER*30 FILEIO
#CHARACTER*80 C80
#CHARACTER*92 FILECC
##      CHARACTER*255 C255
#
#INTEGER LUNCRP, LUNIO, I    !, LUNECO
#INTEGER ERR, LINC, LNUM, FOUND, ISECT
#
#REAL ROWSPC, RMIN, PLTPOP, WTFSD, WTPSD,
#&  SDPRO, SDLIP, SDWTPL,
#&  ALPHL,  ALPHS,  ALPHR,  ALPHSH,
#&  PROLFF, PROSTF, PRORTF, PROSHF,                 PRONOD,
#&  PROLFI, PROSTI, PRORTI,
#&  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,
#&  PLIPLF, PLIPST, PLIPRT, PLIPSH,                 PLIPNO,
#&  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,
#&  POALF,  POAST,  POART,  POASH,  POASD,  POANO,
#&  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
#
##-----------------------------------------------------------------------
#CALL GETLUN('FILEIO', LUNIO)
#OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
#if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
#LNUM = 0
##-----------------------------------------------------------------------
##    Find and read Cultivar Section
##-----------------------------------------------------------------------
#SECTION = '*CULTI'
#CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#if (FOUND == 0) {
#  CALL ERROR(SECTION, 42, FILEIO, LNUM)
#} else {
#  READ (LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
#  if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
#}
#
#if (CROP != 'FA') {
#  #-----------------------------------------------------------------------
#  #    Read Planting Details Section
#  #-----------------------------------------------------------------------
#  SECTION = '*PLANT'
#  CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILEIO, LNUM)
#  } else {
#    READ(LUNIO,'(24X,F6.1,5X,A1,6X,F6.0,12X,F6.0)',IOSTAT=ERR) PLTPOP, PLME, ROWSPC, SDWTPL ; LNUM = LNUM + 1
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
#  }
#  
#  #-----------------------------------------------------------------------
#  #    Read Cultivar Section
#  #-----------------------------------------------------------------------
#  SECTION = '*CULTI'
#  CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILEIO, LNUM)
#  } else {
#    READ(LUNIO,'(24X,A6,66X,F6.0,24X,2F6.0)',IOSTAT=ERR)  ECONO, WTPSD, SDPRO, SDLIP
#    LNUM = LNUM + 1
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
#  }
#  
#}
#
#CLOSE (LUNIO)
#
#if (CROP != 'FA') {
#  #-----------------------------------------------------------------------
#  #     Read in values from input file, which were previously input
#  #       in Subroutine IPCROP.
#  #-----------------------------------------------------------------------
#  CALL GETLUN('FILEC', LUNCRP)
#  OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
#  if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
#  LNUM = 0
#  #-----------------------------------------------------------------------
#  #    Find and Read Respiration Section
#  #-----------------------------------------------------------------------
#  #     Subroutine FIND finds appropriate SECTION in a file by
#  #     searching for the specified 6-character string at beginning
#  #     of each line.
#  #-----------------------------------------------------------------------
#  SECTION = '#*RESP'
#  CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILECC, LNUM)
#  } else {
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(24X,F6.0)',IOSTAT=ERR) RMIN
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#  }
#  
#  SECTION = '#*PLAN'
#  CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILECC, LNUM)
#  } else {
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR) PROLFI, PROLFF, PROSTI, PROSTF
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(F6.0,6X,F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PRORTF, PROSHF
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(12X,F6.0)',IOSTAT=ERR) PRONOD
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(6F6.0)',IOSTAT=ERR) PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(5F6.0)',IOSTAT=ERR) PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(6F6.0)',IOSTAT=ERR) PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(6F6.0)',IOSTAT=ERR) POALF, POAST, POART, POASH, POASD, POANO
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(6F6.0)',IOSTAT=ERR) PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#  }
#  
#  SECTION = '#*CARB'
#  CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILECC, LNUM)
#  } else {
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(4X,A2)',IOSTAT=ERR) XPODF
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#    
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(4F6.0)',IOSTAT=ERR) ALPHL, ALPHS, ALPHR, ALPHSH
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#  }
#  
#  for (I in 1:2) {
#    XPODF(I:I) = UPCASE(XPODF(I:I))
#  }
#  
#  #-----------------------------------------------------------------------
#  SECTION = '#*VEGE'
#  CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
#  if (FOUND == 0) {
#    CALL ERROR(SECTION, 42, FILECC, LNUM)
#  } else {
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
#    READ(C80,'(F6.0)',IOSTAT=ERR) WTFSD
#    if (ERR != 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
#  }
#  
#  CLOSE (LUNCRP)
#  
#}
##-----------------------------------------------------------------------
#return()
#}  # SUBROUTINE IPGROW
#=======================================================================
#}
#-----------------------------------------------------------------------
#       Variable definitions: updated 27 Feb 04
#-----------------------------------------------------------------------
# AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
#           (fraction)
# ALFDOT   Rate of leaf expansion (cm2[leaf] / m2[ground] / d)
# ALPHL    Fraction of new leaf growth that is mobile C (fraction)
# ALPHR    Fraction of new root growth that is mobile C (fraction)
# ALPHS    Fraction of new seed growth that is mobile C (fraction)
# ALPHSH   Fraction of new shell growth that is mobile C (fraction)
# AREAH    Area of healthy leaves (cm2[leaf] / m2[ground])
# AREALF   Area of leaves (one side) per unit ground area
#           (cm2[leaf] / m2[ground])
# BETN     Spacing between plants along a row (m / plant)
# C255     255-character record read from file 
# C80      80-character record read from file 
# CADLF    Mass of CH2O added to leaf reserves after growth
#           (g[CH2O] / m2 / d)
# CADST    Mass of CH2O added to stems (g[CH2O] / m2 / d)
# CANNAA   Weight of N in total plant at flowering (g[N] / m2)
# CANWAA   Canopy weight at flowering stage (g[plant] / m2)
# CLW      Cumulative leaf growth (g[leaf]/m2)
# CONTROL  Composite variable containing variables related to control 
#            and/or timing of simulation.    See Appendix A. 
# CPFLF    Respiration requirement for net leaf growth
#           (g[CH20] / g[tissue])
# CPFNOD   Respiration requirement for net nodule growth
#           (g[CH20] / g[tissue])
# CPFRT    Respiration requirement for net root growth
#           (g[CH20] / g[tissue])
# CPFSD1   Respiration requirement for net seed growth
#           (g[CH20] / g[tissue])
# CPFSH1   Respiration requirement for net leaf growth
#           (g[CH20] / g[tissue])
# CPFSTM   Respiration requirement for net stem growth
#           (g[CH20] / g[tissue])
# CROP     Crop identification code 
# CRUSLF   C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
# CRUSRT   C mobilized from root tissue in a day (g[CH2O] / m2 / d)
# CRUSSH   C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
# CRUSST   C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
# CSW      Cumulative stem growth (g[stem]/m2)
# DAP      Number of days after planting (d)
# DISLA    Diseased leaf area (cm2[leaf]/m2[ground]/d)
# DOY      Current day of simulation (d)
# DWNOD    Current nodule mass (g[nodule] / m2)
# DWNODA   Cumulative nodule growth (g[nodule] / m2)
# ECONO    Ecotype code - used to match ECOTYP in .ECO file 
# ECOTYP   Ecotype code for this simulation 
# ERR      Error code for file operation 
# F        Specific leaf area of new leaf tissue growth, including N
#           (cm2[leaf] / g[leaf])
# FILECC   Path plus filename for species file (*.spe) 
# FILEGC   Pathname plus filename for ECO file 
# FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
# FOUND    Indicator that good data was read from file by subroutine FIND 
#            (0 - End-of-file encountered, 1 - NAME was found) 
# FRLF     Fraction of vegetative tissue growth that goes to leaves on a 
#            day (g[leaf] / g[veg])
# FRSTM    Fraction of vegetative tissue growth that goes to stems on a day
#           (g[stem] / g[veg])
# GROWTH   Total new growth of plant tissue on a day (g[tissue] / m2 / d)
# GRWRES   Growth respiration (g[CH2O]/m2-d)
# IDETO    Switch for printing OVERVIEW.OUT file 
# IHARI    Harvest type code: M=at harvest maturity, R=on specified day of 
#            year (HDATE), D= on specified day after planting (HDATE), G= 
#            at specified growth stage (HSTG), A= within specified window 
#            when SW conditions are met 
# ISECT    Indicator of completion of IGNORE routine: 0 - End of file 
#            encountered, 1 - Found a good line to read, 2 - End of Section 
#            in file encountered denoted by * in column 1. 
# ISWITCH  Composite variable containing switches which control flow of 
#            execution for model.  The structure of the variable 
#            (SwitchType) is defined in ModuleDefs.for. 
# ISWNIT   Nitrogen simulation switch (Y or N) 
# ISWSYM   Nitrogen fixation simulation switch (Y = simulate nodule growth, 
#            N = no nodule growth, U = N-fixation occurs at a rate that 
#            carbon will allow, and nodules are not grown explicitly) 
# LAIMX    Maximum leaf area index this season (m2[leaf] / m2[ground])
# LINC     Line number of input file 
# LNUM     Current line number of input file 
# LUNCRP   Logical unit number for FILEC (*.spe file) 
# LUNECO   Logical unit number for FILEE (*.eco file) 
# LUNIO    Logical unit number for FILEIO 
# MDATE    Harvest maturity date (YYYYDDD)
# MESSAGE  Text array containing information to be written to WARNING.OUT 
#            file. 
# NADLF    N added to leaf N reserves (g[N] / m2 / d)
# NADRT    N added to root N reserves (g[N] / m2 / d)
# NADST    N added to stem N reserves (g[N] / m2 / d)
# NDTH     Nodule death rate (g[nodule] / m2 / d)
# NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
# NGRLF    Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
# NGRRT    Maximum N demand for root growth (g[root N] / m2[ground] / d)
# NGRSD    Rate of N accumulation in new seeds (g[N] / m2 / d)
# NGRSH    Rate of N accumulation in new shells (g[N] / m2 / d)
# NGRST    Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
# NLALL    N added to leaves today (g[N]/m2-d)
# NLAYR    Actual number of soil layers 
# NLDOT    Net N addition for leaves (g[leaf N] / m2[ground] / d)
# NLOFF    N loss from leaves in a day (g[N]/m2-d)
# NMINEA   Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NNOFF    N loss from nodules in a day (g[N]/m2-d)
# NODGR    New nodule growth (g[nod] / m2 / d)
# NOUTDO   Logical unit for OVERVIEW.OUT file 
# NPLTD    Number of plants destroyed (#/m2/d)
# NRALL    N added to roots today (g[N]/m2-d)
# NRDOT    Net N addition for roots (g[N] / m2[ground] / d)
# NROFF    N loss from roots in a day (g[N]/m2-d)
# NRUSLF   N actually mobilized from leaves in a day (g[N]/m2-d)
# NRUSRT   N actually mobilized from roots in a day (g[N]/m2-d)
# NRUSSH   N actually mobilized from shells in a day (g[N]/m2-d)
# NRUSST   N actually mobilized from stems in a day (g[N]/m2-d)
# NSALL    N added to stems today (g[N]/m2-d)
# NSDALL   N added to seeds today (g[N]/m2-d)
# NSDDOT   Net N addition for seeds (g[N] / m2[ground] / d)
# NSDOFF   N loss from seeds in a day (g[N]/m2-d)
# NSDOT    Net N addition for stems (g[N] / m2[ground] / d)
# NSHALL   N added to shells today (g[N]/m2-d)
# NSHDOT   Net N addition for shells (g[N] / m2[ground] / d)
# NSHOFF   N loss from shells in a day (g[N]/m2-d)
# NSOFF    N loss from stems in a day (g[N]/m2-d)
# NTOVR    (Not used) 
# PCARLF   Proportion of leaf tissue that is carbohydrate (fraction)
# PCARNO   Proportion of nodule tissue that is carbohydrate (fraction)
# PCARRT   Proportion of root tissue that is carbohydrate (fraction)
# PCARSD   Proportion of seed tissue that is carbohydrate
#           (g[CH2O] / g[seed])
# PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
# PCARST   Proportion of stem tissue that is carbohydrate (fraction)
# PCCSD    Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
# PCLSD    Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
# PCNL     Percentage of N in leaf tissue (100 g[N] / g[leaf])
# PCNMIN   Minimum percentage of leaf N composition after N mining
#           (100 g[N] / g[leaf])
# PCNRT    Percent N in root tissue (100 g[N] / g[root])
# PCNSD    Percentage of N in seed tissue (100 g[N] / g[seed])
# PCNSH    Percentage of N in shell tissue (100 g[N] / g[shell])
# PCNST    Percent N in stem tissue (100 g[N] / g[stem])
# PLIGLF   Proportion of leaf tissue that is lignin (fraction)
# PLIGNO   Proportion of nodule tissue that is lignin (fraction)
# PLIGRT   Proportion of root tissue that is lignin (fraction)
# PLIGSD   Proportion of seed tissue that is lignin (fraction)
# PLIGSH   Proportion of shell tissue that is lignin (fraction)
# PLIGST   Proportion of stem tissue that is lignin (fraction)
# PLIPLF   Proportion of leaf tissue that is lipid (fraction)
# PLIPNO   Proportion of nodule tissue that is lipid (fraction)
# PLIPRT   Proportion of root tissue that is lipid (fraction)
# PLIPSH   Proportion of shell tissue that is lipid (fraction)
# PLIPST   Proportion of stem tissue that is lipid (fraction)
# PLME     Planting method; T = transplant, S = seed, P = pre-germinated 
#            seed, N = nursery 
# PLTPOP   Plant population (# plants / m2)
# PMINLF   Proportion of leaf tissue that is mineral (fraction)
# PMINNO   Proportion of nodule tissue that is mineral (fraction)
# PMINRT   Proportion of root tissue that is mineral (fraction)
# PMINSD   Proportion of seed tissue that is mineral (fraction)
# PMINSH   Proportion of shell tissue that is mineral (fraction)
# PMINST   Proportion of stem tissue that is mineral (fraction)
# POALF    Proportion of leaf tissue that is organic acid (fraction)
# POANO    Proportion of nodule tissue that is organic acid (fraction)
# POART    Proportion of root tissue that is organic acid (fraction)
# POASD    Proportion of seed tissue that is organic acid (fraction)
# POASH    Proportion of shell tissue that is organic acid (fraction)
# POAST    Proportion of stem tissue that is organic acid (fraction)
# PODWT    Dry mass of seeds plus shells, including C and N
#           (g[pods] / m2[ground])
# POTCAR   Potential carbohydrate composition of seed based on temperature
#           (fraction)
# POTLIP   Potential lipid composition of seed based on temperature
#           (fraction)
# PPLTD    Percent plants destroyed (%/m2/d)
# PROLFF   Minimum leaf protein composition after N mining
#           (g[protein] / g[leaf])
# PROLFI   Maximum protein composition in leaves during growth with 
#            luxurious supply of N (g[protein] / g[leaf tissue])
# PRONOD   Protein composition in nodules (g[protein] / g[nodule])
# PRORTF   Minimum root protein composition after N mining
#           (g[protein] / g[root])
# PRORTI   Maximum protein composition in roots during growth with 
#            luxurious supply of N (g[protein] / g[root])
# PROSHF   Minimum shell protein composition after N mining
#           (g[protein] / g[shell])
# PROSTF   Minimum stem protein composition after N mining
#           (g[protein] / g[stem])
# PROSTI   Maximum protein composition in stems during growth with 
#            luxurious supply of N (g[protein] / g[stem])
# PUNCSD   Cumulative puncture damage to seed (not yet implemented) 
# PUNCTR   Cumulative puncture damage (not yet implemented) 
# PUNDOT   Daily puncture damage (not yet implemented) 
# RHOL     Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# RHOR     Fraction of root which is carbohydrate (g [CH2O] / g[root])
# RHOS     Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
# RHOSH    Fraction of shell which is carbohydrate (g [CH2O] / g[shell])
# RMIN     Respiration required for synthesizing mineral structure
#           (g[CH2O] / g[mineral])
# RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
#            reduction. (%)
# ROWSPC   Row spacing (m)
# RTWT     Dry mass of root tissue, including C and N
#           (g[root] / m2[ground])
# SDIDOT   Number of seeds destroyed on the current day (#/m2/d)
# SDLIP    Maximum lipid composition in seed (fraction)
# SDNPL    Seed N (g[N] / m2)
# SDPDOT   Daily seed puncture damage (not yet implemented) 
# SDPRO    Seed protein fraction at 25ï¿½C (g[protein] / g[seed])
# SDPROR   Ratio to adjust lipid and carbohydrate proportions when seed 
#            protein differs from protein composition of standard cultivar 
#            (SDPROS) 
# SDRATE   Seeding rate, mass of seed sown (g[seed] / m2[ground])
# SDWT     Dry mass of seed tissue, including C and N
#           (g[seed] / m2[ground])
# SDWTAM   Seed weight at maturity (g / m2)
# SDWTPL   Initial planting material dry weight (kg / ha)
# SECTION  Section name in input file 
# SEEDNI   Seed or transplant N at planting (g[N] / m2)
# SEEDNO   Total number of seeds (#/m2)
# SENESCE  Composite variable containing data about daily senesced plant 
#            matter. Structure of variable is defined in ModuleDefs.for 
# SENWT    Leaf senescence due to N mobilization (g[leaf] / m2[ground])
# SHELWT   Total mass of all shells (g / m2)
# SLA      Specific leaf area (cm2[leaf] / m2[ground])
# SLAAD    Specific leaf area, excluding weight of C stored in leaves
#           (cm2[leaf] / g[leaf])
# SLDOT    Defoliation due to daily leaf senescence (g/m2/day)
# SLNDOT   Leaf senescence due to water stress (g/m2/day)
# SOILPROP Composite variable containing soil properties including bulk 
#            density, drained upper limit, lower limit, pH, saturation 
#            water content.  Structure defined in ModuleDefs. 
# SRDOT    Daily root senescence (g / m2 / d)
# SSDOT    Daily senescence of petioles (g / m2 / d)
# SSNDOT   Petiole senescence due to water stress (g/m2/day)
# STMWT    Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
# SWIDOT   Daily seed mass damage (g/m2/day)
# TGROW    Cumulative growth of plant tissue (g[tissue] / m2)
# TOPWT    Total weight of above-ground portion of crop, including pods
#           (g[tissue] / m2)
# TOTWT    Total weight of crop (g[tissue] / m2)
# TRNH4U   Total N uptake in ammonium form in a day (kg[N] / ha / d)
# TRNO3U   Total N uptake in nitrate form in a day (kg[N] / ha / d)
# TRNU     Total N uptake in a day (kg[N] / ha / d)
# TURFAC   Water stress factor for expansion (0 - 1) 
# WCRLF    Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
# WCRRT    Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
# WCRSH    Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
# WCRST    Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
# WDOT     Net growth rate for the entire crop (g[tissue] / m2 / d)
# WLDOT    Net leaf growth rate (g[leaf] / m2 / d)
# WLDOTN   Dry weight growth rate of new leaf tissue including N but not C 
#            reserves (g[leaf] / m2[ground]-d)
# WLFDOT   Leaf weight losses due to freezing (g[leaf]/m2-d)
# WLFI     Initial weight of leaves (g[leaf] / m2)
# WLIDOT   Daily pest or freeze damage to leaf mass (g/m2/day)
# WNDOT    Net nodule growth rate (g[nodule] / m2 / d)
# WNRLF    N available for mobilization from leaves above lower limit of 
#            mining (g[N] / m2)
# WNRRT    N available for mobilization from roots above lower limit of 
#            mining (g[N] / m2)
# WNRSH    N available for mobilization from shells above lower limit of 
#            mining (g[N] / m2)
# WNRST    N available for mobilization from stems above lower limit of 
#            mining (g[N] / m2)
# WPDOT    Net pod growth rate (g[pod] / m2 / d)
# WRCLDT   Net C addition for leaves (g[CH2O] / m2 /d)
# WRCRDT   Net C addition for roots (g[CH2O] / m2 /d)
# WRCSDT   Net C addition for stems (g[CH2O] / m2 /d)
# WRCSHD   Net C addition for shells (g[CH2O] / m2 /d)
# WRDOT    Net root growth rate (g[root] / m2 / d)
# WRDOTN   Dry weight growth rate of new root tissue including N but not C 
#            reserves (g[root] / m2[ground]-d)
# WRIDOT   Daily pest damage to root mass (g/m2/day)
# WRTI     Initial weight of roots (g[root] / m2)
# WSDDOT   Net seed growth rate (g[seed] / m2 / d)
# WSDDTN   New seed growth today (g[seed] / m2 / d)
# WSDMAN   Mass of seed requiring maintenance (g[seed] / m2)
# WSDOT    Net stem growth rate (g[stem] / m2 / d)
# WSDOTN   Dry weight growth rate of new stem tissue including N but not C 
#            reserves (g[stem] / m2[ground]-d)
# WSHDOT   Net shell growth rate (g[shell] / m2 / d)
# WSHDTN   New shell growth today (g[shell] / m2 / d)
# WSHIDT   Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WSIDOT   Daily pest damage to stem mass (g/m2/day)
# WSTI     Initial weight of stems (g[stem] / m2)
# WTABRT   Weight of shells aborted on a day (g[shell] / m2 / d)
# WTCO     Cumulative losses of plant tissue (g[tissue] / m2)
# WTCSD    Cumulative carbohydrate added to seeds (g[C] / m2 / d)
# WTFSD    Relative weight of seed compared to maximum (fraction)
# WTLF     Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
# WTLO     Cumulative leaf losses (g[leaf] / m2)
# WTLSD    Cumulative lipids added to seeds (g[lipid] / m2 / d)
# WTMAIN   Mass of tissue assumed to require maintenance (g[tissue] / m2)
# WTNCAN   Mass of N in canopy (g[N] / m2[ground])
# WTNEW    Initial mass of seedling or seed (g / plant)
# WTNFX    Cumulative weight of N fixed (g[N] / m2)
# WTNLA    Cumulative N added to leaves (g[N]/m2-d)
# WTNLF    Mass of N in leaves (g[leaf N] / m2[ground])
# WTNLO    Cumulative N loss from leaves (g[N] / m2)
# WTNMOB   Cumulative mobilized N (g[N] / m2)
# WTNNA    Cumulative N added to nodules (g[N]/m2-d)
# WTNNAG   Total accumulated N used in nodule growth (g[N] / m2)
# WTNNO    Cumulative N loss from nodules (g[N] / m2)
# WTNNOD   Mass of N in nodules (g[N] / m2[ground])
# WTNOO    Cumulative nodule losses (g[nodule] / m2)
# WTNRA    Cumulative N added to roots (g[N]/m2-d)
# WTNRO    Cumulative N loss from roots (g[N] / m2)
# WTNRT    Mass of N in roots (g[root N] / m2[ground])
# WTNSA    Cumulative N added to stems (g[N]/m2-d)
# WTNSD    Mass of N in seeds (g[N] / m2[ground])
# WTNSDA   Cumulative N added to seeds (g[N]/m2-d)
# WTNSDO   Cumulative N loss from seeds (g[N] / m2)
# WTNSH    Mass of N in shells (g[N] / m2[ground])
# WTNSHA   Cumulative N added to shells (g[N]/m2-d)
# WTNSHO   Cumulative N loss from shells (g[N] / m2)
# WTNSO    Cumulative N loss from stems (g[N] / m2)
# WTNST    Mass of N in stems (g[stem N] / m2[ground])
# WTNTOT   Total plant N content (g[N] / m2[ground])
# WTNUP    Cumulative N uptake (g[N] / m2)
# WTPSD    Maximum weight per seed under non-limiting substrate (g / seed)
# WTRO     Cumulative root losses (g[root] / m2)
# WTSDO    Cumulative seed losses (g[seed] / m2)
# WTSHMT   Cohorts that reach THRESH today (g/m2)
# WTSHO    Cumulative shell losses (g[shell] / m2)
# WTSO     Cumulative stem losses (g[stem] / m2)
# XHLAI    Healthy leaf area index (m2[leaf] / m2[ground])
# XLAI     Leaf area (one side) per unit of ground area
#           (m2[leaf] / m2[ground])
# XPOD     Growth partitioning to pods which slows node appearance
#           (fraction)
# XPODF    Input parameter which determines the method of calculating XPOD 
# YR       Year portion of date 
# YRDOY    Current day of simulation (YYYYDDD)
# YRNR1    Day when 50% of plants have at least one flower (YYYYDDD)
# YRPLT    Planting date (YYYYDDD)
#=======================================================================
#       END SUBROUTINES GROW and IPGROW
#=======================================================================