simDataVars$TGRO     <- rep(1.,24)

#------------------GROW VARS---------------------
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
simDataVars$PODWT  <-  0
simDataVars$PUNCSD  <-  0
simDataVars$PUNCTR  <-  0
simDataVars$RHOL  <-  0
simDataVars$RHOS  <-  0
simDataVars$RNITP  <-  0
# simDataVars$ROWSPC  <-  50 #TODO: Mudar para parametro fixo
simDataVars$RTWT  <-  0
simDataVars$SDNPL  <-  0
simDataVars$SDRATE  <-  0
simDataVars$SDWT  <-  0
simDataVars$SEEDNI  <-  0
simDataVars$SEEDNO  <-  0
# simDataVars$SENESCE  <-  0
# SENESCE
# NL = 20 
simDataVars$SENESCE_ResWt   <-  rep(0,20)
simDataVars$SENESCE_ResLig  <-  rep(0,20)
simDataVars$SENESCE_ResE    <-  matrix(0,20,3)
simDataVars$SENESCE_CumResWt  <-  0
simDataVars$SENESCE_CumResE   <-  c()
# HARVRES
simDataVars$HARVRES_ResWt   <-  rep(0,20)
simDataVars$HARVRES_ResLig  <-  rep(0,20)
simDataVars$HARVRES_ResE    <-  matrix(0,20,3)
simDataVars$HARVRES_CumResWt  <-  0
simDataVars$HARVRES_CumResE   <-  c()

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
simDataVars$MDATE  <-  0

simDataVars$CPFLF   <-  0
simDataVars$CPFSTM  <-  0
simDataVars$CPFRT   <-  0
simDataVars$CPFNOD  <-  0
simDataVars$CPFSH1  <-  0
simDataVars$CPFSD1  <-  0
simDataVars$PCNMIN  <-  0
simDataVars$WTLSD  <-  0
simDataVars$WTCSD  <-  0
simDataVars$ALFDOT <- 0
simDataVars$AREAH  <- 0
simDataVars$NLDOT  <- 0
simDataVars$NSDOT  <- 0
simDataVars$NRDOT  <- 0
simDataVars$NSDDOT <- 0
simDataVars$NSHDOT <- 0
simDataVars$NTOVR  <- 0
simDataVars$RHOR   <- 0
simDataVars$RHOSH  <- 0
simDataVars$SDWTAM <- 0
simDataVars$TGROW  <- 0
simDataVars$WSDDOT <- 0
simDataVars$WSHDOT <- 0
simDataVars$WTCSD  <- 0
simDataVars$WTLSD  <- 0
simDataVars$WTNMOB <- 0
simDataVars$WTNNA  <- 0
simDataVars$SDPDOT  <- 0
simDataVars$PUNDOT  <- 0


simDataVars$WSFDOT  <- 0
simDataVars$SRNDOT  <- 0
simDataVars$DWTCO   <- 0
simDataVars$DWTLO   <- 0
simDataVars$DWTSO   <- 0
simDataVars$PWTCO   <- 0
simDataVars$PWTLO   <- 0
simDataVars$PWTSO   <- 0
simDataVars$LTSEN   <- 0
simDataVars$CADRT   <- 0
simDataVars$CADSH   <- 0
simDataVars$NADSH   <- 0
simDataVars$CADSR   <- 0
simDataVars$CRUSSR  <- 0
simDataVars$NADSR   <- 0
simDataVars$NGRSR   <- 0
simDataVars$NRUSSR   <- 0
simDataVars$SSRDOT   <- 0
simDataVars$SSRNDOT   <- 0
simDataVars$WSRDOTN   <- 0
simDataVars$WSRIDOT   <- 0
simDataVars$WSRDOT   <- 0
simDataVars$WSRFDOT   <- 0
simDataVars$CSRW   <- 0
simDataVars$RHOSR   <- 0
simDataVars$WTNSRA   <- 0
simDataVars$WTNSRO   <- 0
simDataVars$WTSRO   <- 0
simDataVars$CPFSTR   <- 0
simDataVars$NSRDOT   <- 0
simDataVars$WSRI   <- 0
simDataVars$WRCSRDT <- 0
simDataVars$NSROFF <- 0
simDataVars$NSRALL <- 0
simDataVars$PSRSRFL <- 0
simDataVars$PSRLYR1 <- 0
simDataVars$TPSRSRFL <- 0
simDataVars$TPSRLYR1 <- 0
simDataVars$LFSENWT <- 0
simDataVars$SLMDOT <- 0
simDataVars$SRMDOT <- 0
simDataVars$SSMDOT <- 0
simDataVars$SSRMDOT <- 0
simDataVars$STLTSEN <- 0
simDataVars$STSENWT <- 0

# harv
simDataVars$FHLEAF <- 0
simDataVars$FHSTEM <- 0
simDataVars$FHVSTG <- 0

#----------------END GROW VARS-------------------

#-----------------ROOTS VARS---------------------
simDataVars$SATFAC <-  0
simDataVars$SENRT  <- rep(0, 20) # rep(0, NL)
simDataVars$SRDOT  <-  0

# TODO: NL = 20
simDataVars$RLV    <- rep(0, 20) # rep(0, NL)
simDataVars$RTDEP  <-  0

simDataVars$TRLV  <-  0
simDataVars$CumRootMass  <-  0

simDataVars$SRCADDOT  <-  0
simDataVars$SRNADDOT  <-  0
simDataVars$RLNSEN  <-  rep(0, 20)
simDataVars$RLSEN  <-  rep(0, 20)
simDataVars$RO  <-  0
simDataVars$RP  <-  0


#----------------END ROOTS VARS------------------

#-----------------DEMAND VARS--------------------
simDataVars$AGRSD1  <-  0
simDataVars$AGRSD2  <-  0
simDataVars$AGRVG2  <-  0
simDataVars$CDMREP  <-  0
simDataVars$Fnew    <-  0 
simDataVars$FNINL   <-  0
simDataVars$FNINR   <-  0
simDataVars$FNINS   <-  0
simDataVars$FNINSD  <-  0
simDataVars$GDMSD   <-  0
simDataVars$GRRAT1  <-  0
simDataVars$NDMNEW  <-  0
simDataVars$NDMOLD  <-  0
simDataVars$NDMREP  <-  0
simDataVars$NDMSDR  <-  0
simDataVars$NDMTOT  <-  0
simDataVars$NDMVEG  <-  0
simDataVars$NMINEP  <-  0
simDataVars$NMOBR   <-  0
simDataVars$PHTIM   <- rep(0, 300)
simDataVars$PNTIM   <- rep(0, 300)
simDataVars$POTCAR  <-  0
simDataVars$POTLIP  <-  0
simDataVars$SDGR    <-  0
simDataVars$TURADD  <-  0
simDataVars$XFRT    <-  0
simDataVars$REDSHL  <-  0
simDataVars$TMPFAC  <-  1.0
simDataVars$CDMSD   <-  0
simDataVars$SLAMN   <-  0
simDataVars$DUMFAC  <- 0
simDataVars$FVEG    <- 0
simDataVars$SLAMX   <- 0
simDataVars$GROMAX  <- 0
simDataVars$SIZRAT  <- 0
simDataVars$YY  <- 0
simDataVars$XX  <- 0
simDataVars$FRLFM  <- 0
simDataVars$FRSTMM <- 0

simDataVars$ADDSHL <- 0
simDataVars$TURXFR <- 0
simDataVars$NDMSD  <- 0
simDataVars$GDMSDR <- 0
simDataVars$CDMSDR <- 0
simDataVars$NAGE   <- 0
simDataVars$NDMSH  <- 0

simDataVars$YVGROW <- rep(0,6)

# -------new---------------
simDataVars$CUMNSF   <- 0
simDataVars$AGRSTR   <- 0 
simDataVars$FNINSR   <- 0 
simDataVars$FRSTR    <- 0
simDataVars$WLIDOT   <- 0 
simDataVars$FRSTRM   <- 0 
simDataVars$PCNSR    <- 0
simDataVars$PPTFAC   <- 0 
simDataVars$STRWT    <- 0
simDataVars$WCRSR    <- 0
simDataVars$WNRSR    <- 0
simDataVars$XSTR     <- 0
simDataVars$SSRDO    <- 0
simDataVars$LFSCMOB  <- 0 
simDataVars$LFSNMOB  <- 0 
simDataVars$RTSCMOB  <- 0 
simDataVars$RTSNMOB  <- 0 
simDataVars$SRSCMOB  <- 0 
simDataVars$SRSNMOB  <- 0 
simDataVars$STSCMOB  <- 0 
simDataVars$STSNMOB  <- 0 
simDataVars$WTNSR    <- 0
#----------------END DEMAND VARS------------------

#-------------------PODS VARS---------------------
simDataVars$AGRSD3  <-  0
simDataVars$LAGSD   <-  0
simDataVars$LNGPEG  <-  0
simDataVars$NGRSD   <-  0
simDataVars$NGRSH   <-  0
simDataVars$PCTMAT  <-  0
simDataVars$PODNO   <-  0
# simDataVars$POTCAR  <-  0
# simDataVars$POTLIP  <-  0
simDataVars$SDNO    <- rep(0, 300)
simDataVars$SDVAR   <-  0
# simDataVars$SEEDNO  <-  0
simDataVars$SHELN   <- rep(0, 300)
simDataVars$SHVAR   <-  0
simDataVars$WSDDTN  <-  0
simDataVars$WSHDTN  <-  0
simDataVars$WTABRT  <-  0
simDataVars$WTSD    <- rep(0, 300)
simDataVars$WTSHE   <- rep(0, 300)
simDataVars$WTSHMT  <-  0
simDataVars$FLWN    <- rep(0, 300)
# simDataVars$AGRSD3  <-  0
simDataVars$ANINSD  <-  0
simDataVars$CUMSIG  <-  0 
simDataVars$RSD     <-  0
simDataVars$PGAVLR     <-  0

simDataVars$ACCAGE   <-  0
simDataVars$AFLW     <-  0
simDataVars$CNSTRES  <-  1
simDataVars$FNINSH   <-  0
simDataVars$FLWRDY   <-  0
simDataVars$PODADD   <-  0
simDataVars$SHMINE   <-  0
simDataVars$TEMPOD   <-  0
simDataVars$TRIGGR   <-  0
simDataVars$WTSHM    <-  0
simDataVars$NAVPOD <- 0
simDataVars$NR2TIM <- 0
simDataVars$PGNPOD <- 0
simDataVars$RPRPUN <- 1

simDataVars$SUPDE  <- rep(0, 300)
simDataVars$AVTEM  <- rep(0, 300)
simDataVars$SDDES  <- rep(0, 300)

simDataVars$MNESPM  <- 0
simDataVars$SDVAR   <- 0
simDataVars$SHVAR   <- 0
#-----------------END PODS VARS-------------------

#-------------------VEGGR VARS--------------------
simDataVars$AGRVG  <-  0 #TODO: VERIFICAR I/O
simDataVars$FRLF  <-  0  #TODO: VERIFICAR I/O
simDataVars$FRRT  <-  0  #TODO: VERIFICAR I/O
simDataVars$FRSTM  <-  0 #TODO: VERIFICAR I/O
simDataVars$CADLF  <-  0
simDataVars$CADST  <-  0
simDataVars$CANHT  <-  0
simDataVars$CANWH  <-  0
simDataVars$CMINEA  <-  0
simDataVars$CRUSLF  <-  0
simDataVars$CRUSRT  <-  0
simDataVars$CRUSSH  <-  0
simDataVars$CRUSST  <-  0
simDataVars$EXCESS  <-  0
simDataVars$NADLF  <-  0
simDataVars$NADRT  <-  0
simDataVars$NADST  <-  0
simDataVars$NGRLF  <-  0
simDataVars$NGRRT  <-  0
simDataVars$NGRST  <-  0
simDataVars$NSTRES  <-  1
simDataVars$TNLEAK  <-  0
simDataVars$WLDOTN  <-  0
simDataVars$WRDOTN  <-  0
simDataVars$WSDOTN  <-  0

simDataVars$CUMTUR  <-  0
simDataVars$FNINLG  <-  0
simDataVars$FNINRG  <-  0
simDataVars$FNINSG  <-  0

#canopy
# simDataVars$CANHT   <-  0
simDataVars$CUMNHT    <-  0
simDataVars$PNMLF     <-  0
simDataVars$PNMST     <-  0
simDataVars$PNMRT     <-  0
simDataVars$PNMSH     <-  0
simDataVars$PNMSR     <-  0
simDataVars$ANMINELF  <-  0
simDataVars$ANMINERT  <-  0
simDataVars$ANMINESH  <-  0
simDataVars$ANMINESR  <-  0
simDataVars$ANMINEST  <-  0



simDataVars$TSCMOB  <-  0
simDataVars$LFCMINE  <-  0
simDataVars$RTCMINE  <-  0
simDataVars$SHCMINE  <-  0
simDataVars$SRCMINE  <-  0
simDataVars$STCMINE  <-  0
simDataVars$TNLKCHK  <-  0
simDataVars$LFCDEBT  <-  0
simDataVars$RTCDEBT  <-  0
simDataVars$SRCDEBT  <-  0
simDataVars$STCDEBT  <-  0
simDataVars$CADVG  <-  0
simDataVars$CHORECOVER  <-  0
simDataVars$NLKSPENT  <-  0
simDataVars$NLKNUSED  <-  0
simDataVars$NLEAK  <-  0


#-----------------END VEGGR VARS------------------

#-------------------PODDET VARS-------------------
simDataVars$PODWTD  <-  1
# NCOHORTS = 300
# simDataVars$SDNO    <- rep(0, 300)
# simDataVars$SHELN   <- rep(0, 300)
# simDataVars$SWIDOT  <-  0
# simDataVars$WSHIDT  <-  0
# simDataVars$WTSD    <- rep(0, 300)
# simDataVars$WTSHE   <- rep(0, 300)
#-----------------END PODDET VARS-----------------

#-------------------SENES VARS--------------------
simDataVars$SLDOT  <- 0
simDataVars$SLNDOT <- 0
simDataVars$SSDOT  <- 0
simDataVars$SSNDOT <- 0
simDataVars$SWFCAB <- rep(0,5)
#-----------------END SENES VARS------------------

#-------------------FREEZE VARS-------------------
# simDataVars$MDATE  <- 0  #VERIFICAR INPUT/OUTPUT não se encaixa aqui
# simDataVars$WLFDOT  <-  0
simDataVars$frost    <-  F
simDataVars$PSRSRFD  <-  0
simDataVars$PSRLYRD  <-  0
#-----------------END FREEZE VARS-----------------

#-------------------INCOMP VARS-------------------
simDataVars$AGRLF   <-  0
simDataVars$AGRNOD  <-  0
simDataVars$AGRRT   <-  0
# simDataVars$AGRSD1  <-  0
# simDataVars$AGRSD2  <-  0
simDataVars$AGRSH1  <-  0
simDataVars$AGRSH2  <-  0
simDataVars$AGRSTM  <-  0
# simDataVars$AGRVG   <-  0
# simDataVars$AGRVG2  <-  0
simDataVars$SDPROR  <-  0
#-----------------END INCOMP VARS-----------------

#-------------------NUPTAK VARS-------------------
simDataVars$TRNH4U <- 0
# TODO: nsoilay : RNH4U <- rep(0, nsoilay) - RNO3U <- rep(0, 20)
simDataVars$RNH4U  <- rep(0, 20)
simDataVars$TRNO3U <- 0
simDataVars$RNO3U  <- rep(0, 20)
simDataVars$TRNU   <- 0
simDataVars$UNH4   <- 0
simDataVars$UNO3   <- 0

simDataVars$NUPNH4 <- rep(0, 20)
simDataVars$NUPNO3 <- rep(0, 20)
simDataVars$TSNMOB <- 0
#-----------------END NUPTAK VARS-----------------

#--------------------MOBIL VARS-------------------
simDataVars$NMINEA  <-  0
simDataVars$NRUSLF  <-  0
simDataVars$NRUSRT  <-  0
simDataVars$NRUSSH  <-  0
simDataVars$NRUSST  <-  0
#------------------END MOBIL VARS-----------------

#--------------------NFIX VARS--------------------
simDataVars$CNOD   <-  0
# simDataVars$DWNOD  <-  0
# simDataVars$DWNODA <-  0
simDataVars$NDTH   <-  0
simDataVars$NFIXN  <-  0
simDataVars$NODGR  <-  0
# simDataVars$WTNFX  <-  0
simDataVars$SDWNOD <-  0
simDataVars$SENNOD    <- rep(0,20)
simDataVars$SWMEM     <- rep(0, 9)

simDataVars$KG2PPM   <- rep(0,20)
simDataVars$SNO3     <- rep(0,20)
simDataVars$SNH4     <- rep(0,20)
#-------------------END NFIX VARS-----------------


#------------------RESPIR VARS---------------------
simDataVars$MAINR  <-  0 #output
#-----------------END RESPIR VARS------------------



#----------------GROW FUNCTION--------------------
GROW <- function (DYNAMIC,iyear,jday, ISWNIT,ISWSYM)  {
  
  environment(STRESS) <- env
  params <- plantList$forage$params
  
  # TODO: Nitrogen
  N = 1
  
  YRDOY <- paste0(iyear,sprintf("%03d", jday))
  
  PLME   <- 'T' # equivalente ao [.SBX] *PLANTING DETAILS: PLME  
  IHARI  <- 'M' # TODO VERIFICAR (provável que pertença ao '[.SBX] *HARVEST DETAILS')
  PLTPOP <- config$plant1$plantPop  #40  # equivalente ao [.SBX] *PLANTING DETAILS: PPOE
  
  # TODO: ROWSPC - precisa dividir por 100?
  ROWSPC <- config$plant1$rowSpacing / 100 # 0.5 # equivalente ao [.SBX] *PLANTING DETAILS: TPLRS
  
  
  SDWTPL <- 2000 # equivalente ao [.SBX] *PLANTING DETAILS: PLWT
  
  TNCFAC   <- params$TNCFAC
  PLIGSR   <- params$PLIGSR
  ALPHSR   <- params$ALPHSR
  PCARSR   <- params$PCARSR
  PLIPSR   <- params$PLIPSR
  PMINSR   <- params$PMINSR
  POASR    <- params$POASR
  PROSRI   <- params$PROSRI
  PROSRF   <- params$PROSRF
  STRSRFL  <- params$STRSRFL
  STRLYR1  <- params$STRLYR1
  PCHOLFF  <- params$PCHOLFF
  PCHORTF  <- params$PCHORTF
  PCHOSRF  <- params$PCHOSRF
  PCHOSTF  <- params$PCHOSTF
  SENCLV   <- params$SENCLV
  SENCRV   <- params$SENCRV
  SENCSV   <- params$SENCSV
  SENCSRV  <- params$SENCSRV
  SENNLV   <- params$SENNLV
  SENNRV   <- params$SENNRV
  SENNSV   <- params$SENNSV
  SENNSRV  <- params$SENNSRV
  
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- params$SDLIP  #0.200 # Fraction oil in seeds (g(oil)/g(seed)) [from VAR# BR0001]
  SDPRO <- params$SDPRO  #0.400 # Fraction protein in seeds (g(protein)/g(seed)) [from VAR# BR0001]
  WTPSD <- params$WTPSD  #0.19  # Maximum weight per seed (g)
  
  
  
  
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PROLFF  <- params$PROLFF  #0.112
  PROSTF  <- params$PROSTF  #0.035
  PRORTF  <- params$PRORTF  #0.056
  PROSHF  <- params$PROSHF  #0.050
  PCARLF  <- params$PCARLF  #0.405
  PCARNO  <- params$PCARNO  #0.480
  PCARRT  <- params$PCARRT  #0.711
  PCARSD  <- params$PCARSD  #0.315
  PCARSH  <- params$PCARSH  #0.380
  PCARST  <- params$PCARST  #0.664
  PLIGLF  <- params$PLIGLF  #0.070
  PLIGNO  <- params$PLIGNO  #0.070
  PLIGRT  <- params$PLIGRT  #0.070
  PLIGSD  <- params$PLIGSD  #0.020
  PLIGSH  <- params$PLIGSH  #0.280
  PLIGST  <- params$PLIGST  #0.070
  PLIPLF  <- params$PLIPLF  #0.025
  PLIPNO  <- params$PLIPNO  #0.050
  PLIPRT  <- params$PLIPRT  #0.020
  PLIPSH  <- params$PLIPSH  #0.020
  PLIPST  <- params$PLIPST  #0.020
  PMINLF  <- params$PMINLF  #0.094
  PMINNO  <- params$PMINNO  #0.050
  PMINRT  <- params$PMINRT  #0.057
  PMINSD  <- params$PMINSD  #0.025
  PMINSH  <- params$PMINSH  #0.030
  PMINST  <- params$PMINST  #0.046
  POALF   <- params$POALF   #0.050
  POANO   <- params$POANO   #0.050
  POART   <- params$POART   #0.050
  POASD   <- params$POASD   #0.040
  POASH   <- params$POASH   #0.040
  POAST   <- params$POAST   #0.050
  PROLFI  <- params$PROLFI  #0.356
  PRONOD  <- params$PRONOD  #0.300
  PRORTI  <- params$PRORTI  #0.092
  PROSTI  <- params$PROSTI  #0.150
  #!*CARBON AND NITROGEN MINING PARAMETERS
  ALPHL  <- params$ALPHL   #0.04
  ALPHS  <- params$ALPHS   #0.08
  ALPHR  <- params$ALPHR   #0.04
  ALPHSH <- params$ALPHSH  #0.08
  XPODF  <- params$XPODF  #'SD'
  #!*VEGETATIVE PARTITIONING PARAMETERS
  WTFSD  <- params$WTFSD  #0.55
  #!*ROOT PARAMETERS
  RMIN   <- params$RMIN  #0.05
  
  
  #     Surface and soil residue due to daily senescence of plant matter
  NL       <- 20  #!Maximum number of soil layers
  
  SenWt <- rep(0,NL)        #kg[dry matter]/ha
  SenLig <- rep(0,NL)       #kg[lignin]/ha
  NELEM <- 3 #Numero de elementos usados na simulacao do Century! 
  SenE <- matrix(0,NL,NELEM)   #kg[E]/ha (E=N, P, S,...)
  NLAYR <- nsoilay
  
  # danos por pestes...
  # DISLA  <- 1 #TODO VERIFICAR PEST.for (Diseased leaf area (cm2[leaf]/m2[ground]/d))
  PPLTD  <- 0 #TODO VERIFICAR PEST.for (Percent plants destroyed (%/m2/d))
  NPLTD  <- 0 #TODO VERIFICAR PEST.for (Number of plants destroyed (#/m2/d))
  WRIDOT <- 0 #TODO VERIFICAR PEST.for & ROOTDM.for (Daily pest damage to root mass (g/m2/day))
  WSIDOT <- 0 #TODO VERIFICAR PEST.for & VEGDM.for (Daily pest damage to stem mass (g/m2/day))
  SDIDOT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Number of seeds destroyed on the current day (#/m2/d))
  WLIDOT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Daily pest or freeze damage to leaf mass (g/m2/day))
  # WSHIDT <- 0 #TODO VERIFICAR PEST.for & SEDDM.for (Weight of shell tissue consumed by pests today (g[shell]/m2-d))
  
  #***********************************************************************
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    # veio do RUNINIT
    if (CROP != 'FA') {
      #-----------------------------------------------------------------------
      #       Copied from IPIBS
      #-----------------------------------------------------------------------
      # TODO: ROWSPC - precisa dividir por 100?
      # Removi o assign()
      # ROWSPC = ROWSPC / 100.
      
      
      if ((ROWSPC > 0.0) && (PLTPOP > 0.0)) {
        BETN <- 1 / (ROWSPC*PLTPOP)
      } else {
        BETN <- 0
      }
      
      #-----------------------------------------------------------------------
      #     Newest version of CROPGRO has variable seed N, oil, CH2O,
      #     which may make this tougher
      #-----------------------------------------------------------------------
      CPFLF  <- 30./44.*(PLIPLF*1.720 + PLIGLF*0.659 + POALF*(-0.011) + PMINLF*RMIN + PCARLF*0.170)
      CPFSTM <- 30./44.*(PLIPST*1.720 + PLIGST*0.659 + POAST*(-0.011) + PMINST*RMIN + PCARST*0.170)
      CPFSTR <- 30./44.*(PLIPSR*1.720 + PLIGSR*0.659 + POASR*(-0.011) + PMINSR*RMIN + PCARSR*0.170)
      CPFRT  <- 30./44.*(PLIPRT*1.720 + PLIGRT*0.659 + POART*(-0.011) + PMINRT*RMIN + PCARRT*0.170)
      CPFNOD <- 30./44.*(PLIPNO*1.720 + PLIGNO*0.659 + POANO*(-0.011) + PMINNO*RMIN + PCARNO*0.170)
      CPFSH1 <- 30./44.*(PLIPSH*1.720 + PLIGSH*0.659 + POASH*(-0.011) + PMINSH*0.073 + PCARSH*0.170)
      CPFSD1 <- 30./44.*(PMINSD*0.073 + PLIGSD*0.659 + POASD*(-0.011) + (SDLIP*1.720 + PCARSD*0.170)*(1. - SDPROR))
      #-----------------------------------------------------------------------
      #     CALCULATE PERCENT NITROGEN IN LEAVES AT END OF SEASON
      PCNMIN <- PROLFF * 16.0            #Moved from INCOMP
      #-----------------------------------------------------------------------
    }
    
    ALFDOT <- 0.0
    AREALF <- 0.0
    AREAH  <- 0.0
    CANWAA <- 0.0
    CANNAA <- 0.0
    GROWTH <- 0.0
    GRWRES <- 0.0
    LAIMX  <- 0.0
    NLDOT  <- 0.0
    NSDOT  <- 0.0
    NRDOT  <- 0.0
    NSDDOT <- 0.0
    NSHDOT <- 0.0
    NTOVR  <- 0.0
    PCCSD  <- 0.0
    PCLSD  <- 0.0
    PCNL   <- 0.0
    PCNRT  <- 0.0
    PCNSD  <- 0.0
    PCNSH  <- 0.0
    PCNST  <- 0.0
    PODWT  <- 0.0
    RHOL   <- 0.0
    RHOR   <- 0.0
    RHOS   <- 0.0
    RHOSH  <- 0.0
    RTWT   <- 0.0
    SDWT   <- 0.0
    SDWTAM <- 0.0
    SEEDNI <- 0.0
    SEEDNO <- 0.0
    
    SenWt  <-  rep(0,20)
    SenLig <-  rep(0,20)
    SenE   <- matrix(0,NL,NELEM)
    
    #TODO: VERIFICAR
    simDataVars$SENESCE_ResWt   <-  SenWt
    simDataVars$SENESCE_ResLig  <-  SenLig
    simDataVars$SENESCE_ResE    <-  SenE
    
    SHELWT <- 0.0
    SLAAD  <- 0.0
    STMWT  <- 0.0
    TGROW  <- 0.0
    TOPWT  <- 0.0
    TOTWT  <- 0.0
    WCRLF  <- 0.0
    WCRRT  <- 0.0
    WCRSH  <- 0.0
    WCRST  <- 0.0
    WNRLF  <- 0.0
    WNRRT  <- 0.0
    WNRSH  <- 0.0
    WNRST  <- 0.0
    WSDDOT <- 0.0
    WSHDOT <- 0.0
    WTCO   <- 0.0
    DWTCO  <- 0.0
    DWTLO  <- 0.0
    DWTSO  <- 0.0
    PWTCO  <- 0.0
    PWTLO  <- 0.0
    PWTSO  <- 0.0
    WTCSD  <- 0.0
    WTLF   <- 0.0
    WTLO   <- 0.0
    WTLSD  <- 0.0
    WTMAIN <- 0.0
    WTNCAN <- 0.0
    WTNLA  <- 0.0
    WTNLF  <- 0.0
    WTNLO  <- 0.0
    WTNMOB <- 0.0
    WTNNA  <- 0.0
    WTNNAG <- 0.0
    WTNNO  <- 0.0
    WTNNOD <- 0.0
    WTNOO  <- 0.0
    WTNRA  <- 0.0
    WTNRO  <- 0.0
    WTNRT  <- 0.0
    WTNSA  <- 0.0
    WTNSD  <- 0.0
    WTNSDA <- 0.0
    WTNSDO <- 0.0
    WTNSH  <- 0.0
    WTNSHA <- 0.0
    WTNSHO <- 0.0
    WTNSO  <- 0.0
    WTNST  <- 0.0
    WTNTOT <- 0.0
    WTNUP  <- 0.0
    WTRO   <- 0.0
    WTSDO  <- 0.0
    WTSHO  <- 0.0
    WTSO   <- 0.0
    XLAI   <- 0.0
    XHLAI  <- 0.0
    XPOD   <- 0.0
    
    CLW    <- 0.0   #Cumulative leaf growth
    CSW    <- 0.0   #Cumulative stem growth
    CSRW   <- 0.0   #Cumulative storage organ growth
    
    
    SDPDOT <- 0.0    #CHP - not used
    PUNDOT <- 0.0    #CHP - not used
    
    
    NSRDOT <- 0.0
    PCNSR  <- 0.0
    RHOSR  <- 0.0
    STRWT  <- 0.0
    WCRSR  <- 0.0
    WNRSR  <- 0.0
    WTNSRA <- 0.0
    WTSRO  <- 0.0
    WTNSR  <- 0.0
    WTNSRO <- 0.0
    
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
  } else if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    #     Net growth rates
    WLDOT   <- 0.0
    WSDOT   <- 0.0
    WRDOT   <- 0.0
    WNDOT   <- 0.0
    WPDOT   <- 0.0
    WLFDOT  <- 0.0
    WSFDOT  <- 0.0
    WSRDOT  <- 0.0
    WSRFDOT <- 0.0
    
    #     Initial seedling or transplant weight
    SDRATE  <- WTPSD * PLTPOP / 0.8 * 10
    
    if (PLME != 'T') {
      WTNEW  <- WTFSD * WTPSD
      TOTWT  <- WTNEW * PLTPOP
    } else {
      TOTWT <- SDWTPL / 10.
      WTNEW <- TOTWT / PLTPOP
    }
    
    #     Initial mass of plant components
    WTLF  <- FRLF * TOTWT
    STMWT <- FRSTM * TOTWT
    TOPWT <- WTLF + STMWT
    STRWT <- FRSTR * TOTWT
    RTWT  <- TOTWT - TOPWT - STRWT
    
    WLFI  <- WTLF
    WSTI  <- STMWT
    WRTI  <- RTWT
    WSRI  <- STRWT
    
    #     Initialize cumulative variables
    CLW   <- WTLF
    CSW   <- STMWT
    CSRW  <- STRWT
    
    #     CH2O reserves
    WCRLF <- ALPHL * WTLF
    WCRST <- ALPHS * STMWT
    WCRRT <- ALPHR * RTWT
    WCRSR <- ALPHSR * STRWT
    
    #     Initialize cumulative C variables
    #      WCRLA = WCRLF
    #      WCRSA = WCRST
    #      WCRRA = WCRRT
    
    #     Carbohydrate composition of plant components (fraction)
    RHOL   <- ALPHL
    RHOR   <- ALPHR
    RHOS   <- ALPHS
    RHOSH  <- ALPHSH
    RHOSR  <- ALPHSR
    
    #     Net carbon addition variables
    WRCLDT  <- 0.0
    WRCSDT  <- 0.0
    WRCRDT  <- 0.0
    WRCSHD  <- 0.0
    WRCSRDT <- 0.0
    
    #     Compute N in plant components
    WTNLF  <- WLFI * PROLFI * 0.16
    WTNST  <- WSTI * PROSTI * 0.16
    WTNRT  <- WRTI * PRORTI * 0.16
    WTNSR  <- WSRI * PROSRI * 0.16
    WTNSH  <- 0.0
    WTNSD  <- 0.0
    WTNTOT <- WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNSR
    
    #     Seed or transplant N at planting
    SDNPL  <- WTPSD * SDPRO * 0.16 * 0.75 * PLTPOP - (WTNLF + WTNST + WTNRT + WTNSR)
    SDNPL  <- max(SDNPL,0.0)
    SEEDNI <- WTNLF + WTNST + WTNRT + SDNPL + WTNSR
    
    #     Initialize cumulative N variables
    WTNLA  <- WTNLF
    WTNSA  <- WTNST
    WTNRA  <- WTNRT
    WTNSRA <- WTNSR
    
    #     Percent N in plant components
    PCNL   <- WTNLF / WLFI * 100.
    RNITP  <- PCNL
    PCNST  <- WTNST / WSTI * 100.
    PCNRT  <- WTNRT / WRTI * 100.
    PCNSR  <- WTNSR / WSRI * 100
    
    #     Intialize leaf area.  Value of F initialized in DEMAND.
    AREALF <- WTLF * Fnew  #*** Fnew is 'F' in the original file. Changed because F is logical in R. ***
    AREAH  <- AREALF
    SLA    <- AREALF / WTLF
    SLAAD  <- AREALF / (WTLF - WCRLF)
    XLAI   <- AREALF / 10000.
    XHLAI  <- XLAI
    
    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    
    GROWTH <- WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
    
    GRWRES <- WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN*CPFRT + NODGR*CPFNOD + WSHDTN*CPFSH1 + WSDDTN*CPFSD1 + WSRDOTN*CPFSTR + 30./44 *(TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 + NFIXN/0.16 * 1.798)
    
    
    
    # ----------------------------------------------------------------------
    #       8/2/05 SJR create new variables representing N refill and CH2O 
    #     refill lost to pest and freeze damage - previously was calculated 
    #     in several equations.  NADLF and CADLF lost with senesced tissues
    #     was calculated and included as part of SxDOT in the SENES and 
    #     ROOTS subroutines
    #     10/06/05 SJR Subtracted SxmDOT from organ mass because no longer 
    #     add C of N to today's naturally senesced tissues.
    # -----------------------------------------------------------------------
    if (WTLF-SLDOT > 0.0) {
      LFCADDM <- CADLF * ( min(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
      LFNADDM <- NADLF * ( min(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
    } else {
      LFCADDM <- 0
      LFNADDM <- 0
    }
    if (RTWT-SRDOT > 0.0){
      RTCADDM <- CADRT * ( min(1.0,WRIDOT/(RTWT - SRDOT)))
      RTNADDM <- NADRT * ( min(1.0,WRIDOT/(RTWT - SRDOT)))
    } else {
      RTCADDM <- 0
      RTNADDM <- 0
    }
    if (STRWT-SSRDOT > 0.0){
      SRCADDM <- CADSR * ( min(1.0,(WSRIDOT+WSRFDOT)/(STRWT-SSRDOT)))
      SRNADDM <- NADSR * ( min(1.0,(WSRIDOT+WSRFDOT)/(STRWT-SSRDOT)))
    } else {
      SRCADDM <- 0
      SRNADDM <- 0
    }
    if (STMWT-SSDOT > 0.0){
      STCADDM <- CADST * ( min(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
      STNADDM <- NADST * ( min(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
    } else {
      STCADDM <- 0
      STNADDM <- 0
    }
    
    #-----------------------------------------------------------------------
    #      8/2/05 SJR create new variables representing End-of-Day N and CH2O
    #      concentrations.  Differs from PCNx as this includes NADxx and 
    #      CADxx which are added prior to senescing tissues.  Simplifies 
    #      several calculations.
    #-----------------------------------------------------------------------
    #      10/11/05 SJR Modified equations to accomodate new mobilization
    #      and senescence scheme.  "Natural senescence" is calculated at the
    #      beginning of the day and no C or N refill is added.  These tissues 
    #      remain at yesterday's C and N concentrations.  The rest of the
    #      tissues may have C annd N refill and, thus, have a different C
    #      and N concentration.
    #-----------------------------------------------------------------------
    # EODLFC = (WCRLF + CADLF) / (WTLF + NADLF / 0.16 + CADLF)
    EODLFC <- (WCRLF + CADLF - LFSCMOB) / (WTLF + NADLF / 0.16 + CADLF - SLMDOT)
    # EODLFN = (WTNLF + NADLF) / (WTLF + NADLF / 0.16 + CADLF)
    EODLFN <- (WTNLF + NADLF - LFSNMOB) / (WTLF + NADLF / 0.16 + CADLF - SLMDOT)
    
    # EODSTC = (WCRST + CADST) / (STMWT + NADST / 0.16 + CADST)
    EODSTC <- (WCRST + CADST - STSCMOB) / (STMWT + NADST / 0.16 + CADST - SSMDOT)
    # EODSTN = (WTNST + NADST) / (STMWT + NADST / 0.16 + CADST)
    EODSTN <- (WTNST + NADST - STSNMOB) / (STMWT + NADST / 0.16 + CADST - SSMDOT)
    
    # EODRTC = (WCRRT + CADRT) / (RTWT + NADRT / 0.16 + CADRT)
    EODRTC <- (WCRRT + CADRT - RTSCMOB) / (RTWT + NADRT / 0.16 +  CADRT - SRMDOT)
    # EODRTN = (WTNRT + NADRT) / (RTWT + NADRT / 0.16 + CADRT)
    EODRTN <- (WTNRT + NADRT - RTSNMOB) / (RTWT + NADRT / 0.16 +  CADRT - SRMDOT)
    
    
    # EODSRC = (WCRSR + CADSR) / (STRWT + NADSR / 0.16 + CADSR)
    EODSRC <- (WCRSR + CADSR - SRSCMOB) / (STRWT + NADSR / 0.16 + CADSR - SSRMDOT)
    # EODSRN = (WTNSR + NADSR) / (STRWT + NADSR / 0.16 + CADSR)
    EODSRN <- (WTNSR + NADSR - SRSNMOB) / (STRWT + NADSR / 0.16 + CADSR - SSRMDOT)
    
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
    #      WLDOT = WLDOTN - (SLDOT - SLCADDOT - SLNADDOT / 0.16) - WLIDOT - 
    #     &        WLFDOT - NRUSLF/0.16 - CRUSLF
    #-----------------------------------------------------------------------
    #     01/01/06 SJR Revised calculation of WxDOT - was missing corrections
    #                        for LFSENWT, reorganized to make it easier to follow.
    #-----------------------------------------------------------------------
    #      WLDOT = WLDOTN - (SLDOT - SLCADDOT - SLNADDOT / 0.16) - WLIDOT - 
    #     &        WLFDOT - (NRUSLF - LFSNMOB) / 0.16 - (CRUSLF - LFSCMOB)
    
    #      IF (WTLF .GT. 0.0) THEN
    #         WLDOT = WLDOT + CADLF - SLCADDOT - LFCADDM + 
    #     &        NADLF / 0.16 - SLNADDOT / 0.16 - LFNADDM / 0.16
    #      ENDIF
    #-----------------------------------------------------------------------
    #      Correct WLDOTN for mobilization (xRUSLF)
    #      Subtract senesced DM but add back that part which was mobilized from natural senescence
    #      prior to abscission as that was included in xRUSLF
    #      Add back the part that was mobilized from senescence due to N-mobilization prior to abscission
    #      Subtract losses to pest and freeze damage
    #-----------------------------------------------------------------------
    WLDOT <- WLDOTN - CRUSLF - NRUSLF/0.16 - SLDOT - WLIDOT - WLFDOT
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if(FHLEAF > 0){
      WLDOT <- WLDOT-FHLEAF
    }
    #      IF (FHLEAF .GT. 0) THEN
    #        IF (WTLF .GT. 0) THEN
    #          WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT
    #     &    - (FHLEAF - CADLF + LFCADDM - (NADLF-LFNADDM)/0.16)
    #        ELSE
    #          WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT - FHLEAF
    #        ENDIF
    #      ELSE
    #        WLDOT = WLDOTN - CRUSLF - NRUSLF/0.16
    #     &        - SLDOT - WLIDOT - WLFDOT
    #      ENDIF
    #--------------------------------------------
    
    # ----------------------------------------------------------------------
    #     If started the day with some leaf mass, add CADLF and NADLF, corrected for parts lost with 
    #     pest and freeze-damaged tissue and also mobilization (from other than natural senescence 
    #      and low-light senescence.
    # ----------------------------------------------------------------------
    if((WTLF < 0.0) && (FHLEAF == 0)) {
      WLDOT <- WLDOT + CADLF - LFCADDM + (NADLF - LFNADDM)/0.16
    }
    
    if(WLDOT < 0.0) {
      WLDOT <- max(WLDOT, -WTLF)
    }
    
    
    
    
    #-----------------------------------------------------------------------
    #       WSDOT = Net stem growth rate
    #-----------------------------------------------------------------------
    #      WSDOT = WSDOTN - (SSDOT - SSCADDOT - SSNADDOT / 0.16) - WSIDOT - 
    #     &        WSFDOT - NRUSST / 0.16 - CRUSST 
    #      WSDOT = WSDOTN - (SSDOT - SSCADDOT - SSNADDOT / 0.16) - WSIDOT - 
    #     &        WSFDOT - (NRUSST - STSNMOB) / 0.16 - (CRUSST - STSCMOB)
    #      IF (STMWT .GT. 0.0) THEN
    #           WSDOT = WSDOT + CADST - SSCADDOT - STCADDM + 
    #     &              NADST/0.16 - SSNADDOT / 0.16 - STNADDM /0.16
    #      ENDIF
    
    
    WSDOT <- WSDOTN - CRUSST - NRUSST/0.16 - SSDOT  - WSIDOT - WSFDOT
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if(FHSTEM > 0){
      WSDOT <- WSDOT - FHSTEM
    }
    
    #      IF (FHSTEM .GT. 0) THEN
    #        IF (STMWT .GT. 0) THEN
    #          WSDOT = WSDOTN - SSDOT - WSIDOT - WSFDOT
    #     &    - (FHSTEM - CADST + STCADDM - (NADST-STNADDM)/0.16)
    #        ELSE
    #          WSDOT = WSDOTN - SSDOT - WSIDOT - WSFDOT - FHSTEM
    #        ENDIF
    #      ELSE
    #        WSDOT = WSDOTN - CRUSST - NRUSST/0.16
    #     &        - SSDOT - WSIDOT - WSFDOT
    #      ENDIF
    #--------------------------------------------
    
    if ((STMWT > 0.0) && (FHSTEM == 0)) {
      WSDOT <- WSDOT + CADST - STCADDM + (NADST - STNADDM) / 0.16
    }
    
    #--------------------------------------------
    
    if (WSDOT < 0.0) {
      WSDOT = max(WSDOT, -STMWT)
    }
    
    
    
    #-----------------------------------------------------------------------
    #       WSRDOT = Net storage tissue growth rate
    #-----------------------------------------------------------------------
    #      WSRDOT = WSRDOTN - (SSRDOT - SSRCADDOT - SSRNADDOT / 0.16) - 
    #     &              WSRIDOT - WSRFDOT - NRUSSR / 0.16 - CRUSSR 
    
    #      WSRDOT = WSRDOTN - (SSRDOT - SSRCADDOT - SSRNADDOT / 0.16) - 
    #     &              WSRIDOT - WSRFDOT - (NRUSSR - SRSNMOB) / 0.16 - 
    #     &              (CRUSSR - SRSCMOB) 
    
    
    #      IF (STRWT .GT. 0.0) THEN
    #         WSRDOT = WSRDOT + CADSR - SSRCADDOT - SRCADDM + 
    #     &              NADSR/0.16 - SSRNADDOT / 0.16 - SRNADDM / 0.16
    #      ENDIF
    
    WSRDOT <- WSRDOTN - CRUSSR - NRUSSR/0.16 - SSRDOT - WSRIDOT - WSRFDOT 
    
    if (STRWT > 0.0) {
      WSRDOT <- WSRDOT + CADSR - SRCADDM + (NADSR - SRNADDM)/0.16
    }
    
    if (WSRDOT < 0.0) {
      WSRDOT <- max(WSRDOT, -STRWT)
    }
    
    #-----------------------------------------------------------------------
    #     Allocate net growth to above and below ground storage organ tissue
    #      using initial partitioning strategy from species file.
    #      Allocate freeze losses according to the proportions calculated in FREEZE
    #      Allocate all othe losses using current "actual" proportions.
    #      If losses for one portion are greater than available mass
    #      the excess loss is taken from the other portion
    #      NOTE: using TPSRLYR1 and TPSRSRFL to hold intermediate values for 
    #      themselves before calculating actual new proportions.
    #-----------------------------------------------------------------------
    TPSRLYR1 <- (WSRDOTN * STRLYR1) + (WSRDOT - WSRDOTN + WSRFDOT) * PSRLYR1 - WSRFDOT * PSRLYRD
    
    TPSRSRFL <- (WSRDOTN * STRSRFL) + (WSRDOT - WSRDOTN + WSRFDOT) * PSRSRFL - WSRFDOT * PSRSRFD
    
    #      TPSRLYR1 = (WSRDOTN * STRLYR1) - ((SSRDOT - SSRCADDOT - 
    #     &              SSRNADDOT / 0.16) + WSRIDOT + NRUSSR / 0.16 + CRUSSR)
    #     &               * PSRLYR1 - WSRFDOT * PSRLYRD
    
    #      TPSRSRFL = (WSRDOTN * STRSRFL) - ((SSRDOT - SSRCADDOT -  
    #     &              SSRNADDOT / 0.16) + WSRIDOT + NRUSSR / 0.16 + CRUSSR)
    #     &               * PSRSRFL - WSRFDOT * PSRSRFD
    
    if (STRWT > 0.0) {
      #      TPSRLYR1 = TPSRLYR1 + CADSR * STRLYR1 - (SSRCADDOT + SRCADDM) * 
      #     &              PSRLYRD + NADSR/0.16 * STRLYR1 - (SSRNADDOT / 0.16 +
      #     &              SRNADDM / 0.16) * PSRLYR1 
      
      #      TPSRSRFL = TPSRSRFL + CADSR * STRSRFL - (SSRCADDOT + SRCADDM) * 
      #     &              PSRSRFD + NADSR/0.16 * STRSRFL - (SSRNADDOT / 0.16 +
      #     &              SRNADDM / 0.16) * PSRSRFL
      #-----------------------------------------------------------------------
      #     Check to make sure not losing more mass from above or below ground
      #      portions than exist in each.  If so, allocate excess loss to the
      #      other portion.  If total loss is more than storage organ mass,
      #      limit loss to actual mass of storage organ present
      #      NOTE: must be done even when net gain is positive because freeze 
      #      damage could result in a net loss of above ground tissue and a net 
      #      gain of total and below ground tissue.
      #-----------------------------------------------------------------------
      
      
      if (-TPSRSRFL > STRWT*PSRSRFL) {
        TPSRLYR1 <- TPSRLYR1 + (TPSRSRFL+(STRWT*PSRSRFL))
        TPSRSRFL <- -STRWT * PSRSRFL
        if (-TPSRLYR1 >= STRWT*PSRLYR1) {
          TPSRLYR1 <- -STRWT * PSRLYR1
          TPSRSRFL <- -STRWT * PSRSRFL
        }
      }
      
      if (-TPSRLYR1 > STRWT*PSRLYR1) {
        TPSRSRFL <- TPSRSRFL + (TPSRLYR1+(STRWT*PSRLYR1))
        TPSRLYR1 <- -STRWT * PSRLYR1
        if (-TPSRSRFL >= STRWT*PSRSRFL) {
          TPSRLYR1 <- -STRWT * PSRLYR1
          TPSRSRFL <- -STRWT * PSRSRFL
        }
      }
    }
    
    #-----------------------------------------------------------------------
    #     Calculate proportions of below and above ground storage organ 
    #      tissue at end of day - with new growth and losses.
    #      Still need current days proportions for allocating senesced
    #      nutrients to appropriate soil layers for CENTURY
    #-----------------------------------------------------------------------
    # 2019-05-30 CHECK FOR ZERO DIVIDE CHP
    if (STRWT + WSRDOT > .001) {
      TPSRLYR1 <- (TPSRLYR1 + (STRWT*PSRLYR1))/(STRWT+ WSRDOT)
      TPSRSRFL <- (TPSRSRFL + (STRWT*PSRSRFL))/(STRWT+ WSRDOT)
    } else {
      TPSRLYR1 <- (TPSRLYR1 + (STRWT*PSRLYR1))/(.001)
      TPSRSRFL <- (TPSRSRFL + (STRWT*PSRSRFL))/(.001)
    }
    
    #-----------------------------------------------------------------------
    #     Net root growth rate
    #-----------------------------------------------------------------------
    #      WRDOT = WRDOTN - (SRDOT - SRCADDOT - SRNADDOT / 0.16) - WRIDOT - 
    #     &        NRUSRT/0.16 - CRUSRT 
    #
    #      WRDOT = WRDOTN - (SRDOT - SRCADDOT - SRNADDOT / 0.16) - WRIDOT - 
    #     &        (NRUSRT - RTSNMOB) / 0.16 - (CRUSRT - RTSCMOB) 
    #
    #
    #      IF (RTWT .GT. 0.0) THEN
    #         WRDOT = WRDOT + CADRT - SRCADDOT - RTCADDM + 
    #     &        NADRT/0.16 - SRNADDOT / 0.16 - RTNADDM / 0.16
    #      ENDIF
    
    WRDOT <- WRDOTN - CRUSRT - NRUSRT/0.16 - SRDOT - WRIDOT  
    
    if (RTWT > 0.0) {
      WRDOT <- WRDOT + CADRT - RTCADDM + (NADRT - RTNADDM)/0.16
    }
    
    if (WRDOT < 0.0) {
      WRDOT <- max(WRDOT, -RTWT)
    }
    #-----------------------------------------------------------------------
    #     Net shell growth rate
    #-----------------------------------------------------------------------
    WSHIDT <- min(WSHIDT,SHELWT)     # pest damage to shells
    WSHDOT <- WSHDTN - WSHIDT - WTABRT - NRUSSH / 0.16 - CRUSSH
    
    if (SHELWT > 0.0) {
      WSHDOT <- WSHDOT + (CADSH + NADSH/0.16) * (1. - min(1.0,WTABRT/SHELWT))
    }
    
    #-----------------------------------------------------------------------
    #     Net seed growth rate
    #-----------------------------------------------------------------------
    SWIDOT <- min(SWIDOT,SDWT)       # pest damage to seeds
    WSDDOT <- WSDDTN - SWIDOT        
    WTLSD  <- WTLSD + WSDDOT * POTLIP    #lipids in seed
    WTCSD  <- WTCSD + WSDDOT * POTCAR    #carbohydrates in seed
    
    #-----------------------------------------------------------------------
    #     Net nodule growth rate
    #-----------------------------------------------------------------------
    WNDOT  <- NODGR - NDTH            
    
    #-----------------------------------------------------------------------
    #     Net pod growth rate
    #-----------------------------------------------------------------------
    WPDOT  <- WSHDOT + WSDDOT          
    
    #-----------------------------------------------------------------------
    #     Total Net plant growth rate
    #-----------------------------------------------------------------------
    WDOT <- WLDOT + WSDOT + WRDOT + WPDOT + WNDOT + WSRDOT
    
    #-----------------------------------------------------------------------
    if (GROWTH > 0.0) {
      if (XPODF == 'PD') {
        XPOD <- 0.17 * (WSDDTN+WSHDTN)/GROWTH + 0.83 * XPOD
      } else if (XPODF == 'SD') {
        XPOD <- 0.17 * (WSDDTN)/GROWTH + 0.83 * XPOD
      } else {
        # TODO (?): CALL ERROR(ERRKEY,1,'      ',0)
        # Mostrar mensagem de erro 
      }
    }
    #-----------------------------------------------------------------------
    #    Integration, Add Today's Net Growth to Existing Weights
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #      SJR 5/12/04 reduce VSTAGE for forage model
    #    Lower VSTAGE for leaf lost to freezing
    #     PDA 5/6/2010  Added FHLEAF for forage harvest 
    #-----------------------------------------------------------------------
    if ((SLDOT + WLDOT) >= WTLF) {
      VSTAGE <- 0.0
    } else {
      #       SJR 5/19/04 - drop leaf loss due to senescence - per discussion
      #       with KJB - decided that it was a bad idea
      #            VSTAGE = ((WTLF-(SLDOT+WLFDOT))/WTLF) * VSTAGE
      VSTAGE <- ((WTLF-WLFDOT)/WTLF) * VSTAGE
      #--------------------------------------------
      # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
      #--------------------------------------------
      if (FHLEAF > 0) VSTAGE <- FHVSTG
      #--------------------------------------------
    }
    #-----------------------------------------------------------------------
    
    TOTWT  <- TOTWT  + WDOT
    TOPWT  <- TOPWT  + WSDOT + WLDOT + WPDOT
    WTLF   <- WTLF   + WLDOT
    STMWT  <- STMWT  + WSDOT
    SDWT   <- SDWT   + WSDDOT
    SHELWT <- SHELWT + WSHDOT
    RTWT   <- RTWT   + WRDOT
    PODWT  <- PODWT  + WPDOT
    DWNOD  <- DWNOD  + WNDOT
    TGROW  <- TGROW  + GROWTH
    STRWT  <- STRWT + WSRDOT
    
    #-----------------------------------------------------------------------
    #     Cumulative leaf and stem growth
    #-----------------------------------------------------------------------
    CLW  <- CLW + WLDOTN      
    CSW  <- CSW + WSDOTN 
    CSRW <- CSRW + WSRDOTN
    
    #-----------------------------------------------------------------------
    #     Store values of TOPWT at flowering and SDWT at maturity
    #-----------------------------------------------------------------------
    if (YRDOY == YRNR1) {
      CANWAA <- TOPWT
    } else if (YRDOY == MDATE) {
      SDWTAM <- SDWT
    }
    #---------------------------------------------------------------------
    #     Compute Seed Weight for Which there is Maintenance Costs.
    #-----------------------------------------------------------------------
    WSDMAN <- min(SDWT,SHELWT)
    WTMAIN <- TOTWT - SDWT + WSDMAN
    
    #-----------------------------------------------------------------------
    #     Carbon Reserves:  Net Growth Rates for Mobile Carbohydrates
    #-----------------------------------------------------------------------
    #     Account for N added to existing plant tissue, e.g., NADLF, that
    #     is damaged by insects, freezing, or senesced.  Otherwise, could
    #     get increase in tissue N composition when tissue is aborted.  Need
    #     to account for mass, N and C lost this way in sections below
    #-----------------------------------------------------------------------
    
    
    #      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLNDOT+WLIDOT+WLFDOT)
    #      Changed to
    #      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLDOT+WLIDOT+WLFDOT)
    #      SJR - 6/5/05 When added minimum CH2O concentration after 
    #      abscission, replaced the last line listed above with
    #      CLOFF = (SLNDOT + WLIDOT +WLFDOT) * RHOL + 
    #     &        PCHOLFF*(SLDOT - SLNDOT)
    #-----------------------------------------------------------------------
    #     7/27/05 SJR Revised to be compatible with NLOFF equation
    #      RHOL is yesterday's CH2O concentration - no allowance for CADLF 
    #      or NADLF.  
    #      CH2O lost = CH2O lost from low-light senescence, water stress, 
    #      pest and freeze damage at "current" CH2O concentration (adjusted 
    #      for CADLF and NADLF) + Remainder of senesced leaf exclusing NADLF
    #      lost at "final" CH2O concentration.
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #      8/2/05 SJR - Simplified and revised equation using new variables.
    #      Easier to read and understand. 
    #-----------------------------------------------------------------------
    #      10/06/05 SJR - Revised CxOFF equations for because not refilling 
    #      SxMDOT tissues any more. 
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #      02/01/06 SJR      Rearrange WRCLDT calculation so easier to follow.
    #      Subtract CH2O lost with abscission of natural senescence and tissue 
    #      loss due to N mobilization at "final" CH2O concentration.
    #      Subtract CH2O lost to low-light and H2O stress at "current" concentration
    #      Subtract CH2O lost to pest and freeze damage at "current" concentration
    #      Add CH2O refill corrected for the amount lost with senesced tissues other than 
    #      those lost to natural senescence and the amount of C refill lost 
    #      with pest and freeze damaged tissue.
    #-----------------------------------------------------------------------
    CLOFF <- (SLMDOT + LTSEN + LFSENWT) * (SENCLV * (RHOL - PCHOLFF) + PCHOLFF) + (SLNDOT + WLIDOT + WLFDOT) * RHOL
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHLEAF > 0) {
      if (WLDOTN > 0) {
        CLOFF <- CLOFF + (FHLEAF - WLDOTN) * RHOL + WLDOTN * ALPHL
      } else {
        CLOFF <- CLOFF + FHLEAF * RHOL
      }
      #        IF (WTLF .GT. 0)THEN
      #          CLOFF=CLOFF
      #     &  +(FHLEAF-WLDOTN-CADLF+LFCADDM-(NADLF-LFNADDM)/0.16)*RHOL
      #        ELSE
      #          CLOFF=CLOFF+(FHLEAF-WLDOTN)*RHOL
      #        ENDIF
      #        IF (WLDOTN.GT.0)THEN
      #          CLOFF = CLOFF+WLDOTN*ALPHL
      #        ENDIF
    }
    #--------------------------------------------
    
    if (CLOFF < 0.0) {
      CLOFF <- 0.0
    }
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHLEAF > 0) {
      WRCLDT <- -CRUSLF - (CLOFF - WLDOTN * ALPHL)
    } else { 
      WRCLDT <- WLDOTN * ALPHL - CRUSLF - CLOFF
    }
    
    #      IF (WTLF .GT. 0.0) THEN
    #        IF (FHLEAF.GT.0)THEN
    #          IF (WLDOTN.GT.0)WRCLDT=-CRUSLF-CLOFF
    #          CLOFF  = CLOFF+CADLF-LFCADDM
    #          WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
    #        ELSE
    #          WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
    #     &     + CADLF - LFCADDM
    #        ENDIF
    #      ENDIF
    #--------------------------------------------
    
    if ((WTLF > 0.0) && (FHLEAF == 0.0)) {
      WRCLDT <- WRCLDT + CADLF - LFCADDM
    }
    
    #      CLOFF = (SLMDOT + LFSENWT) * PCHOLFF +
    #     &        (LTSEN + SLNDOT) * RHOL + 
    #     &        (WLIDOT + WLFDOT) * RHOL 
    #
    #      IF (CLOFF. LT. 0.0) THEN
    #        CLOFF = 0.0
    #      ENDIF
    #
    #      WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
    #      IF (WTLF .GT. 0.0) THEN
    #         WRCLDT = WRCLDT + CADLF - SLCADDOT - LFCADDM
    #      ENDIF
    
    
    #-----------------------------------------------------------------------
    #      Original code commented out
    #      Appears to be double accounting for C in new C reserves in stem
    #      Changed WSDOT to WSDOTN to fix this/be consistent with leaf code
    #      E-mailed Dr. Boote, waiting for comment
    #-----------------------------------------------------------------------
    #      WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT
    #      WRCSDT = ALPHS * WSDOTN - CRUSST - RHOS * (SSDOT+WSFDOT+WSIDOT)
    #      IF (STMWT .GT. 0.0) THEN
    #         WRCSDT = WRCSDT + CADST *
    #     &     (1. - MIN(1.0,(SSDOT+WSIDOT+WSFDOT)/STMWT))
    #      ENDIF
    
    CSOFF <- (SSMDOT + STLTSEN + STSENWT) * (SENCSV * (RHOS - PCHOSTF) + PCHOSTF) + (SSNDOT + WSIDOT + WSFDOT) * RHOS  
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHSTEM > 0.0) {
      if (WSDOTN > 0.0) {
        CSOFF <- CSOFF + (FHSTEM - WSDOTN) * RHOS + WSDOTN * ALPHS
      } else {
        CSOFF <- CSOFF + FHSTEM * RHOS
      }
    }
    
    #      IF (FHSTEM.GT.0)THEN
    #        IF (STMWT .GT. 0)THEN
    #          CSOFF=CSOFF
    #     &  +(FHSTEM-WSDOTN-CADST+STCADDM-(NADST-STNADDM)/0.16)*RHOS
    #        ELSE
    #          CSOFF=CSOFF+(FHSTEM-WSDOTN)*RHOS
    #        ENDIF
    #        IF (WSDOTN.GT.0)THEN
    #          CSOFF = CSOFF+WSDOTN*ALPHS
    #        ENDIF
    #      ENDIF
    
    #--------------------------------------------
    
    if (CSOFF < 0.0) {
      CSOFF <- 0.0
    }
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHSTEM > 0.0) {
      WRCSDT <- -CRUSST - (CSOFF - WSDOTN * ALPHS)
    } else {
      WRCSDT <- WSDOTN * ALPHS - CRUSST - CSOFF
    }
    
    #      IF (STMWT .GT. 0.0) THEN
    #        IF (FHSTEM.GT.0)THEN
    #          CSOFF  = CSOFF+CADST-STCADDM
    #          WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
    #        ELSE
    #          WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
    #     &     + CADST - STCADDM
    #        ENDIF
    #      ENDIF
    #--------------------------------------------
    
    #      WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
    if ((STMWT > 0.0) && (FHSTEM == 0.0)) {
      WRCSDT <- WRCSDT + CADST - STCADDM
    }
    
    
    #      WRCSRDT = ALPHSR * WSRDOT - CRUSSR - RHOSR * (SSRNDOT
    #     &              +WSRIDOT + WSRFDOT)
    
    #      WRCSRDT = ALPHSR * WSRDOTN - CRUSSR - RHOSR * (SSRDOT
    #     &              +WSRIDOT + WSRFDOT)
    #      IF (STRWT .GT. 0.0) THEN
    #         WRCSRDT = WRCSRDT + CADSR *
    #     &     (1. - MIN(1.0,(SSRDOT+WSRIDOT+WSRFDOT)/STRWT))
    #      ENDIF
    
    CSROFF <- SSRMDOT * (SENCSRV * (RHOSR - PCHOSRF) + PCHOSRF) + (SSRNDOT + WSRIDOT) * RHOSR
    
    if (CSROFF < 0.0) {
      CSROFF <- 0.0
    }
    
    WRCSRDT <- WSRDOTN * ALPHSR - CRUSSR - CSROFF
    if (STRWT > 0.0) {
      WRCSRDT <- WRCSRDT + CADSR - SRCADDM
    }
    
    
    #-----------------------------------------------------------------------
    #      Original code commented out
    #      Appears to be double accounting for C in new C reserves in stem
    #      Changed WSDOT to WSDOTN to fix this/be consistent with leaf code
    #      E-mailed Dr. Boote, waiting for comment
    #-----------------------------------------------------------------------
    #      WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT
    #      WRCRDT = ALPHR * WRDOTN - CRUSRT - RHOR * (SRDOT + WRIDOT)
    CROFF <- (SRMDOT + SRNDOT) * (SENCRV * (RHOR - PCHORTF) + PCHORTF) + (SRNDOT + WRIDOT) * RHOR 
    
    if (CROFF < 0.0) {
      CROFF <- 0.0
    }
    
    WRCRDT <- WRDOTN * ALPHR - CRUSRT - CROFF
    if (RTWT > 0.0) {
      WRCRDT <- WRCRDT + CADRT - RTCADDM
    }
    
    
    #      WRCSHD = ALPHSH * WSHDOT - CRUSSH - RHOSH*(WTABRT+WTSHMT+WSHIDT)
    WRCSHD <- ALPHSH * WSHDTN - CRUSSH - RHOSH * (WTABRT + WTSHMT + WSHIDT)
    
    #-----------------------------------------------------------------------
    #     Update C Storage, Concentrations in Leaves, Stems, Roots, and Shells
    #-----------------------------------------------------------------------
    WCRLF <- WCRLF + WRCLDT
    WCRST <- WCRST + WRCSDT
    WCRRT <- WCRRT + WRCRDT
    WCRSH <- WCRSH + WRCSHD
    WCRSR <- WCRSR + WRCSRDT
    
    if (WCRLF <= 1.0E-30) WCRLF <- 0.0
    if (WCRST <= 1.0E-30) WCRST <- 0.0
    if (WCRRT <= 1.0E-30) WCRRT <- 0.0
    if (WCRSH <= 1.0E-30) WCRSH <- 0.0
    if (WCRSR <= 1.0E-30) WCRSR <- 0.0
    
    #-----------------------------------------------------------------------
    #     Compute Cumulative C loss During Season
    #-----------------------------------------------------------------------
    WTLO   <- WTLO  + SLDOT  + WLIDOT + WLFDOT
    WTSO   <- WTSO  + SSDOT  + WSIDOT + WSFDOT
    WTRO   <- WTRO  + SRDOT  + WRIDOT
    WTSHO  <- WTSHO + WTABRT + WSHIDT
    WTSDO  <- WTSDO + SWIDOT
    WTNOO  <- WTNOO + NDTH
    WTCO   <- WTLO  + WTSO   + WTSHO  + WTSDO
    WTSRO  <- WTSRO  + SSRDOT  + WSRIDOT + WSRFDOT
    
    #-----------------------------------------------------------------------
    #     Compute CH20 fractions
    #-----------------------------------------------------------------------
    if (WTLF > 0.0001) {
      RHOL <-  WCRLF/WTLF
    } else {
      RHOL <- 0.0
    }
    
    if (STMWT > 0.0001) {
      RHOS <-  WCRST/STMWT
    } else {
      RHOS <- 0.0
    }
    
    if (STRWT > 0.0001) {
      RHOSR <-  WCRSR/STRWT
    } else {
      RHOSR <- 0.0
    }
    
    if (RTWT > 0.0001) {
      RHOR <-  WCRRT/RTWT
    } else {
      RHOR <- 0.0
    }
    
    if (SHELWT > 0.0001) {
      RHOSH <-  WCRSH/SHELWT
    } else {
      RHOSH <- 0.0
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
    #      SJR 7/7/05 some controversy over what N concentration tissues are 
    #      senesced at depending on the reason for senescence.  Quick or 
    #      stress-related losses should be lost at current N concentration but 
    #      slow, normal senescence should be lost at final N concentration.  
    #      Existing code uses PCNL to calculate N lost to quick senescence.  
    #      This does not account for the increase in N content and concentration 
    #      from NADLF added today.  Fixed by calculating a temporary N 
    #      concentration from WTNLF, NADLF, and WTLF at the start of the day.
    #-----------------------------------------------------------------------
    #      NLOFF  = (LTSEN + SLNDOT + WLIDOT + WLFDOT) * (PCNL/100.) +
    #     &     (SLDOT-(SLNDOT + LTSEN)) * PROLFF * 0.16
    #-----------------------------------------------------------------------
    #      02/01/06 SJR      Rearrange NLDOT calculation so easier to follow.
    #      Subtract CH2O lost with abscission of natural senescence and tissue 
    #      loss due to N mobilization at "final" N concentration.
    #      Subtract N lost to low-light and H2O stress at "current" concentration
    #      Subtract N lost to pest and freeze damage at "current" concentration
    #      Add N refill corrected for the amount lost with senesced tissues other than 
    #      those lost to natural senescence and the amount of N refill lost 
    #      with pest and freeze damaged tissue.
    #-----------------------------------------------------------------------
    #      NLOFF  = (SLMDOT + LFSENWT) * PROLFF * 0.16 +
    #     &        (LTSEN + SLNDOT) * PCNL/100 + 
    #     &        (WLIDOT + WLFDOT) * PCNL/100  
    #      
    #      IF (NLOFF. LT. 0.0) THEN
    #        NLOFF = 0.0
    #      ENDIF
    
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the leaves
    #-----------------------------------------------------------------------
    #      NLDOT = NGRLF - NRUSLF - NLOFF
    #      IF (WTLF .GT. 0.0) THEN
    #         NLDOT = NLDOT + NADLF - SLNADDOT - LFNADDM
    #      ENDIF
    
    
    
    NLOFF <- SLMDOT * (SENNLV * (PCNL/100 - PROLFF * 0.16) + PROLFF * 0.16) + (LTSEN + LFSENWT) * PROLFF *0.16 + (SLNDOT + WLIDOT + WLFDOT) * PCNL/100  
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHLEAF > 0.0) {
      if (WLDOTN > 0.0) {
        NLOFF <- NLOFF + (FHLEAF - WLDOTN) * PCNL / 100 + NGRLF
      } else {
        NLOFF <- NLOFF + FHLEAF * PCNL / 100
      }
    }
    #      IF (FHLEAF.GT.0)THEN
    #        IF (WTLF .GT. 0)THEN
    #          NLOFF=NLOFF
    #     &     +(FHLEAF-WLDOTN-CADLF+LFCADDM-(NADLF-LFNADDM)/0.16)*PCNL/100
    #        ELSE
    #          NLOFF=NLOFF+(FHLEAF-WLDOTN)*PCNL/100
    #        ENDIF
    #        IF (WLDOTN.GT.0)THEN
    #          NLOFF = NLOFF+NGRLF
    #        ENDIF
    #      ENDIF
    #--------------------------------------------
    
    
    # ALTERADO: .LT. to <
    if (NLOFF < 0.0) {
      NLOFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the leaves
    #-----------------------------------------------------------------------
    #      NLDOT = NGRLF - NRUSLF - NLOFF
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHLEAF > 0.0) {
      NLDOT <- -NRUSLF - (NLOFF - NGRLF)
    } else {
      NLDOT <- NGRLF - NRUSLF - NLOFF
    }
    #      IF (WTLF .GT. 0.0) THEN
    #        IF (FHLEAF.GT.0)THEN
    #          NLOFF = NLOFF + NADLF - LFNADDM
    #          NLDOT = NGRLF - NRUSLF - NLOFF
    #        ELSE
    #          NLDOT = NGRLF - NRUSLF - NLOFF + NADLF - LFNADDM
    #        ENDIF
    #      ENDIF
    #--------------------------------------------
    if (WTLF > 0.0 && FHLEAF == 0.0)  {
      NLDOT <- NLDOT + NADLF - LFNADDM
    }
    
    #-----------------------------------------------------------------------
    #     Stem nitrogen senescence and pest damage loss
    #-----------------------------------------------------------------------
    #     NSOFF  = (LTSEN * PORPT + SSNDOT + WSIDOT + WSFDOT) * EODSTN +
    #    &   (SSMDOT + - SSNDOT - LTSEN * PORPT - SSNADDOT / 0.16) * 
    #    &     PROSTF * 0.16 
    
    NSOFF <- SSMDOT * (SENNSV * (PCNST/100 - PROSTF * 0.16) + PROSTF * 0.16) + (STLTSEN + STSENWT) * PROSTF *0.16 + (SSNDOT + WSIDOT + WSFDOT) * PCNST/100  
    
    if (NSOFF < 0.0) {
      NSOFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the stem
    #-----------------------------------------------------------------------
    #      NSDOT = NGRST - NRUSST - NSOFF
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHSTEM > 0.0) {
      NSDOT <- -NRUSST - (NSOFF - NGRST)
    } else {
      NSDOT <- NGRST - NRUSST - NSOFF
    }
    #      IF (FHSTEM.GT.0)THEN
    #        IF (STMWT .GT. 0)THEN
    #          NSOFF=NSOFF
    #     &     +(FHSTEM-WSDOTN-CADST+STCADDM-(NADST-STNADDM)/0.16)*PCNST/100
    #        ELSE
    #          NSOFF=NSOFF+(FHSTEM-WSDOTN)*PCNST/100
    #        ENDIF
    #        IF (WSDOTN.GT.0)THEN
    #          NSOFF = NSOFF+NGRST
    #        ENDIF
    #      ENDIF
    
    #--------------------------------------------
    
    if (STMWT > 0.0 && FHSTEM == 0.0) {
      NSDOT <- NSDOT + NADST - STNADDM
    }
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    #      IF (STMWT .GT. 0.0) THEN
    #        IF (FHSTEM.GT.0)THEN
    #         NSOFF = NSOFF + NADST - STNADDM
    #         NSDOT = NGRST - NRUSST - NSOFF
    #        ELSE
    #         NSDOT = NGRST - NRUSST - NSOFF + NADST - STNADDM
    #        ENDIF
    #      ENDIF
    #--------------------------------------------
    # 
    #-----------------------------------------------------------------------
    #     Storage tissue nitrogen senescence and pest damage loss
    #      Using different strategy - Stolon not abscised like leaf 
    #      therefore, are assuming senesced tissue lost at average 
    #      N concentration
    #-----------------------------------------------------------------------
    #      NSROFF  = (SSRNDOT + WSRIDOT + WSRFDOT) * (PCNSR/100.) +
    #     &     (SSRDOT-SSRNDOT) * PROSRF * 0.16
    
    
    NSROFF <- SSRMDOT  * (SENNSRV * (PCNSR/100 - PROSRF * 0.16) + PROSRF * 0.16) + (SSRNDOT + WSRIDOT + WSRFDOT) * PCNSR/100  
    
    if (NSROFF < 0.0) {
      NSROFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the STOR
    #-----------------------------------------------------------------------
    NSRDOT <- NGRSR - NRUSSR - NSROFF
    if (STRWT > 0.0) {
      NSRDOT <- NSRDOT + NADSR - SRNADDM
    }
    
    #-----------------------------------------------------------------------
    #     Root nitrogen senescence and pest damage loss
    #-----------------------------------------------------------------------
    NROFF <- SRMDOT  * (SENNRV * (PCNRT/100 - PRORTF * 0.16) + PRORTF * 0.16) + (SRNDOT + WRIDOT) * PCNRT / 100
    
    if (NROFF < 0.0) {
      NROFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in the roots
    #-----------------------------------------------------------------------
    NRDOT <- NGRRT - NRUSRT - NSROFF
    if (RTWT > 0.0) {
      NRDOT <- NRDOT + NADRT - RTNADDM
    }
    
    #-----------------------------------------------------------------------
    #     Shell nitrogen senescence, abortion and pest damage loss
    #-----------------------------------------------------------------------
    NSHOFF <- (WTABRT + WSHIDT) * (PCNSH / 100.)
    if (NSHOFF < 0.0) {
      NSHOFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in shells
    #-----------------------------------------------------------------------
    NSHDOT <- NGRSH - NSHOFF - NRUSSH
    if (SHELWT > 0.0) {
      NSHDOT <- NSHDOT + NADSH * (1. - min(1.0,(WTABRT+WSHIDT) / SHELWT))
    }
    
    #-----------------------------------------------------------------------
    #     Seed nitrogen senescence, abortion and pest damage loss
    #-----------------------------------------------------------------------
    NSDOFF <- SWIDOT * PCNSD/100.
    if (NSDOFF < 0.0) {
      NSDOFF <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     Net growth rate of nitrogen in seeds
    #-----------------------------------------------------------------------
    NSDDOT <- NGRSD - NSDOFF
    
    #-----------------------------------------------------------------------
    #   Integration, Update N in Each Component by Adding Today's Growth
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the leaves
    #-----------------------------------------------------------------------
    # ALTERADO: ABS to abs(). Se repetia, mas só comentei aqui pra não duplicar.
    if ((NLDOT < 0.0) & (abs(NLDOT) > WTNLF)) {
      NLDOT <- - WTNLF
    }
    WTNLF <- WTNLF + NLDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the stems
    #-----------------------------------------------------------------------
    if ((NSDOT < 0.0) & (abs(NSDOT) > WTNST)) {
      NSDOT <- - WTNST
    }
    WTNST <- WTNST + NSDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the storage tissues
    #-----------------------------------------------------------------------
    if ((NSRDOT < 0.0) && (abs(NSRDOT) > WTNSR)) {
      NSRDOT <- - WTNSR
    }
    WTNSR <- WTNSR + NSRDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the roots
    #-----------------------------------------------------------------------
    if ((NRDOT < 0.0) & (abs(NRDOT) > WTNRT)) {
      NRDOT <- - WTNRT
    }
    WTNRT <- WTNRT + NRDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the nodules
    #-----------------------------------------------------------------------
    WTNNOD <- DWNOD * 0.16 * PRONOD
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the shells
    #-----------------------------------------------------------------------
    if ((NSHDOT < 0.0) & (abs(NSHDOT) > WTNSH)) {
      NSHDOT <- - WTNSH
    }
    WTNSH <- WTNSH + NSHDOT
    
    if ((NSDDOT < 0.0) & (abs(NSDDOT) > WTNSD)) {
      NSDDOT <- - WTNSD
    }
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the seed
    #-----------------------------------------------------------------------
    WTNSD <- WTNSD + NSDDOT
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the plant
    #-----------------------------------------------------------------------
    WTNTOT <- WTNLF + WTNST + WTNSR + WTNRT + WTNSH + WTNSD + WTNNOD
    
    #-----------------------------------------------------------------------
    #     Total nitrogen in the canopy (all above ground components)
    #-----------------------------------------------------------------------
    WTNCAN = WTNLF + WTNST + WTNSH + WTNSD
    
    #-----------------------------------------------------------------------
    #     Save total nitrogen in the canopy at the start of flowering
    #-----------------------------------------------------------------------
    if (YRDOY == YRNR1) {
      CANNAA <- WTNCAN
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
    
    SLNADDOT  <- 0.0
    SSNADDOT  <- 0.0
    SSRNADDOT <- 0.0
    
    
    #     Leaf
    NLALL <- NGRLF - NRUSLF 
    if (WTLF > 0.0) {
      NLALL <- NLALL + NADLF - SLNADDOT - LFNADDM # TODO: Não consegui descobrir o valor da variável SLNADDOT, valor zero, descobrir onde ela é alterada 
    }
    
    #     Stem
    NSALL <- NGRST - NRUSST 
    if (STMWT > 0.0) {
      NSALL <- NSALL + NADST - SSNADDOT - STNADDM # TODO: Não consegui descobrir o valor da variável SSNADDOT, valor zero, descobrir onde ela é alterada 
    }
    
    #     Storage tissues
    NSRALL <- NGRSR - NRUSSR 
    if (STRWT > 0.0) {
      NSRALL <- NSRALL + NADSR - SSRNADDOT - SRNADDM # TODO: Não consegui descobrir o valor da variável SSRNADDOT, valor zero, descobrir onde ela é alterada 
    }
    
    #     Root
    NRALL <- NGRRT - NRUSRT
    if (RTWT > 0.0) {
      NRALL <- NRALL + NADRT - SRNADDOT - RTNADDM
    }
    
    #     Shell and seed
    NSHALL <- NGRSH - NRUSSH
    #-----------------------------------------------------------------------
    #      Added these lines after adding NADSH variable
    #-----------------------------------------------------------------------
    
    if (SHELWT > 0.0) {
      NSHALL <- NSHALL + NADSH * (1. - min(1.0,(WTABRT+WSHIDT) / SHELWT))
    }
    
    NSDALL <- NGRSD
    
    WTNLA <- WTNLA + NLALL
    WTNSA <- WTNSA + NSALL
    WTNRA <- WTNRA + NRALL
    WTNSHA <- WTNSHA + NSHALL
    WTNSDA <- WTNSDA + NSDALL
    WTNSRA <- WTNSRA + NSRALL
    #-----------------------------------------------------------------------
    #    Nodules
    #-----------------------------------------------------------------------
    if (ISWNIT == 'Y' && ISWSYM == 'Y') {
      DWNODA <- DWNODA + NODGR
      WTNFX <- WTNFX + NFIXN + (NODGR * 0.16 * PRONOD) - NTOVR
      WTNNAG <- DWNODA * 0.16 * PRONOD
    }
    #-----------------------------------------------------------------------
    #     Compute Cumulative N loss During Season
    #-----------------------------------------------------------------------
    WTNLO  <- WTNLO  + NLOFF
    WTNSO  <- WTNSO  + NSOFF
    WTNRO  <- WTNRO  + NROFF
    WTNSHO <- WTNSHO + NSHOFF
    WTNSDO <- WTNSDO + NSDOFF
    WTNSRO <- WTNSRO + NSROFF
    if (ISWNIT == 'Y' & ISWSYM == 'Y') {
      NNOFF  <- NDTH * 0.16 * PRONOD
      WTNNO  <- WTNNO  + NNOFF
    }
    #-----------------------------------------------------------------------
    #     N Balance Components for Mobilized, Uptake, and Fixed N
    #-----------------------------------------------------------------------
    WTNMOB <- WTNMOB + NMINEA
    WTNUP  <- WTNUP + TRNU
    WTNNA  <- WTNNOD + WTNNO
    #-----------------------------------------------------------------------
    #     Compute Percentage N in each Plant Component
    #-----------------------------------------------------------------------
    if ((WTLF > 0.0) & (WTLF > WTNLF) & (WTNLF > 0.0)) {
      PCNL <- WTNLF / WTLF * 100.0
    } else {
      PCNL <- 0.0
    }
    
    if ((STMWT > 0.0) & (WTNST > 0.0)) {
      PCNST <- WTNST / STMWT * 100.0
    } else {
      PCNST <- 0.0
    }
    
    if ((STRWT > 0.0) && (STRWT > WTNSR) && (WTNSR > 0.0)) {
      PCNSR <- WTNSR / STRWT * 100.0
    } else {
      PCNSR <- 0.0
    }
    
    if ((RTWT > 0.0) & (WTNRT > 0.0)) {
      PCNRT <- WTNRT / RTWT * 100.0
    } else {
      PCNRT <- 0.0
    }
    
    if ((SHELWT > 0.0) & (WTNSH > 0.0)) {
      PCNSH <- WTNSH / SHELWT * 100.0
    } else {
      PCNSH <- 0.0
    }
    #-----------------------------------------------------------------------
    #     Compute Percentage Seed Composition
    #-----------------------------------------------------------------------
    if (SDWT > 0.01) {
      PCNSD <- WTNSD / SDWT * 100.0
      PCLSD <- WTLSD / SDWT * 100.0
      PCCSD <- WTCSD / SDWT * 100.0
    } else {
      PCNSD <- 0.0
      PCLSD <- 0.0
      PCCSD <- 0.0
    }
    #-----------------------------------------------------------------------
    #    Calculate true nitrogen concentration in leaf tissue for
    #     photosynthesis reduction.
    #-----------------------------------------------------------------------
    if ((WTLF - WCRLF) > 0.0) {
      RNITP <- 100. * WTNLF / (WTLF - TNCFAC * WCRLF)
      #-----------------------------------------------------------------------
      #  PDA 5/26/2010  ADDED CODE TO ALLOW LEAF CH2O TO AFFECT PHOTOSYNTHESIS
      #      IF (WTLF .GT. 0.0.AND.(WTLF - WCRLF) .GT. 0.0) THEN
      #         IF (WCRLF/WTLF.GT.ALPHL) THEN
      #           RNITP = 100*WTNLF/(WTLF-ALPHL/(1-ALPHL)*(WTLF-WCRLF))      
      #         ELSE
      #           RNITP = 100*WTNLF/(WTLF-WCRLF)
      #         ENDIF
      #-----------------------------------------------------------------------
    } else {
      RNITP <- PCNMIN
    }
    #-----------------------------------------------------------------------
    #     Calculate Remaining N in Shells, Leaves, Stems, and Roots
    #     That can be Mined (Plant N-Balance).
    #-----------------------------------------------------------------------
    if ((WTLF - WCRLF) > 0.0) {
      WNRLF <- max(WTNLF - PROLFF * 0.16 * (WTLF-WCRLF), 0.0)
    } else {
      WNRLF <- 0.0
    }
    
    if ((STMWT - WCRST) > 0.0) {
      WNRST <- max(WTNST - PROSTF * 0.16 * (STMWT-WCRST), 0.0)
    } else {
      WNRST <- 0.0
    }
    
    if ((STRWT - WCRSR) > 0.0) {
      WNRSR <- max(WTNSR - PROSRF * 0.16 * (STRWT-WCRSR), 0.0)
    } else {
      WNRSR <- 0.0
    }
    
    if (RTWT > 0.0) {
      WNRRT <- max(WTNRT - PRORTF * 0.16 * RTWT, 0.0)
    } else {
      WNRRT <- 0.0
    }
    
    if (SHELWT > 0.0) {
      WNRSH <- max(WTNSH - PROSHF * 0.16 * SHELWT, 0.0)
    } else {
      WNRSH <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #     This section added to provide senescence parameters to the 
    #     Soil N routines (senescence and freeze values are added to soil
    #     residue; pest damage components are lost from the system)
    #-----------------------------------------------------------------------
    #     Surface carbon includes all senescence and freeze variables for
    #      leaf (SLDOT, WLFDOT), stem (SSDOT), storage organ (SSRDOT, WSRFDOT)
    #       and shell (WTABRT). At this time, the model does not senesce seed.   
    #     Convert from biomass to C with a factor 0.40.   
    #      NOTE storage organ senescence is apportioned to both surface layer 
    #      and soil layer1 according to proportions PSRSRFD and PSRLYRD from FREEZE
    #      Also replaced PCNx with EODxxN.  PCNx is yesterday's ending N 
    #      concentration and doesn't include NADLF and CADLF from today.
    #-----------------------------------------------------------------------
    #        LFNADFDM = LFNADDM * WLFDOT / (WLIDOT + WLFDOT)
    #        STNADFDM = STNADDM * WSFDOT / (WSIDOT + WSFDOT) 
    #        SRNADFDM = SRNADDM * WSRFDOT / (WSRIDOT + WSRFDOT)
    #      SENCLN(0,1) = (SLDOT + WLFDOT + SSDOT + WTABRT) * 0.40
    SenWt[1] <- (SLDOT + WLFDOT + SSDOT + WSFDOT + WTABRT + SSRDOT *PSRSRFL) + WSRFDOT * PSRSRFD
    #
    #     Convert from g/m2 to kg/ha with a factor of 10.
    #      SENCLN(0,1) = AMAX1(SENCLN(0,1), 0.) * 10.0     !kg[C]/ha
    SenWt[1] <- max(SenWt[1], 0.) * 10.0    #kg[dry matter]/ha
    
    #-----------------------------------------------------------------------
    #     Surface nitrogen includes nitrogen losses computed above minus the
    #       pest damage components. 
    #-----------------------------------------------------------------------
    #      SENCLN(0,3) = (NLOFF - WLIDOT * PCNL / 100.) +     
    #      SenE(0,1) = (NLOFF - WLIDOT * PCNL  / 100.) +                  !Leaf
    #     &        (NSOFF - WSIDOT * PCNST / 100.) +                  !Stem
    #     &        (NSHOFF- WSHIDT * PCNSH / 100.) +                  !Shell
    #     &              (NSROFF- (WSRIDOT + WSRFDOT) * PCNSR / 100.*PSRSRFL)
    #     &              + WSRFDOT * PCNSR / 100 * PSRSRFD                  !Storage
    
    SenE[1,2] <- (NLOFF - WLIDOT * EODLFN) +                         #Leaf
      (NSOFF - WSIDOT * EODSTN) +                         #Stem
      (NSHOFF- WSHIDT * PCNSH / 100.) +                   #Shell
      (NSROFF- (WSRIDOT + WSRFDOT) * EODSRN * PSRSRFL)
    + WSRFDOT * EODSRN * PSRSRFD                        #Storage
    
    #     Convert from g/m2 to kg/ha with a factor of 10.
    #      SENCLN(0,3) = AMAX1 (SENCLN(0,3),0.) * 10.0        !kg[N]/ha
    SenE[1,2] <- max(SenE[1,2],0.) * 10.0             #kg[N]/ha
    
    #-----------------------------------------------------------------------
    #     Contribution of lignin to surface litter from senesced and frozen 
    #       plant matter
    #-----------------------------------------------------------------------
    #      SENCLN(0,2) = (SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST +
    SenLig[1] <- (SLDOT + WLFDOT) * PLIGLF + (SSDOT+WSFDOT) * PLIGST +  WTABRT * PLIGSH + (SSRDOT*PSRSRFL + WSRFDOT*PSRSRFD)* PLIGSR 
    #     Convert from g/m2 to kg/ha with a factor of 10.
    SenLig[1] <- max(SenLig[1],0.) * 10.0             #kg[lig]/ha
    
    #-----------------------------------------------------------------------
    #     Senescence of roots, nodules, and subsurface storage organs (kg/ha)
    #-----------------------------------------------------------------------
    for (L in 1:NLAYR) {
      if (L == 1) { 
        #       SENCLN(L,1) = (SENRT(L) + SENNOD(L)) * 0.40   !kg[C]/ha
        SenWt[L]  <- SENRT[L] + SENNOD[L] + (SSRDOT*PSRLYR1 + WSRFDOT*PSRLYRD)                                 # kg[dry matter]/ha
        #       SENCLN(L,2) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO !kg[lig]/ha
        SenLig[L] <- SENRT[L] * PLIGRT + SENNOD[L] * PLIGNO + (SSRDOT*PSRLYR1 + WSRFDOT*PSRLYRD)* PLIGSR       # kg[lig]/ha
        #       SENCLN(L,3) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 
        SenE[L,1] <- (SENRT[L]* PRORTF + SENNOD[L] * PRONOD) * 0.16 + (NSROFF- (WSRIDOT + WSRFDOT) * EODSRN * PSRLYR1) + WSRFDOT * EODSRN * PSRLYRD
      } else {
        #       SENCLN(L,1) = (SENRT(L) + SENNOD(L)) * 0.40   !kg[C]/ha
        SenWt[L] <- (SENRT[L] + SENNOD[L])                                                                    # kg[dry matter]/ha
        #       SENCLN(L,2) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO !kg[lig]/ha
        SenLig[L] <- SENRT[L] * PLIGRT + SENNOD[L] * PLIGNO                                                    # kg[lig]/ha
        # TODO: Verificar se comeca do 1 ou 2 
        #SenE(L,1) = (SENRT[L]* PRORTF + SENNOD[L] * PRONOD) * 0.16 
        #       01/19/2006 CHP Root senescence at current N% (per KJB and JWJ)
        SenE[L,1] <- (SENRT[L]* PRORTF + SENNOD[L] * PRONOD) * 0.16 
      }
    }
    
    #-----------------------------------------------------------------------
    #     Update distribution of storage tissue above and below soil surface
    #-----------------------------------------------------------------------
    PSRLYR1 <- TPSRLYR1
    PSRSRFL <- TPSRSRFL 
    
    #-----------------------------------------------------------------------
    #     Soil nitrogen includes nitrogen losses computed above minus the
    #       pest damage components of root and nodule. 
    #-----------------------------------------------------------------------
    #SENCLN(1,3) = NROFF - WRIDOT * EODRTN. + NNOFF
    #SENCLN(1,3) = AMAX1 (SENCLN(1,3), 0.) * 10.0    !kg[N]/ha
    #-----------------------------------------------------------------------
    #     Contribution of lignin to soil from senesced roots and nodules
    #-----------------------------------------------------------------------
    #SENCLN(1,2) = (SRDOT * PLIGRT + NDTH * PLIGNO) * 10.0
    #kg[lig]/ha
    #-----------------------------------------------------------------------
    #     Leaf Area, Specific Leaf Area Calculations
    #     Calculate Area Growth rate and Update Leaf Area (AREALF, XLAI)
    #-----------------------------------------------------------------------
    #      SJR - 6/17/05 Per e-mails with KJB - the amount of protein lost to 
    #            leaf N mobilization and refill should not affect leaf area.  
    #            Fix by not adding NRUSLF/0.16 to senescence and pest losses.  
    #            This prevents N mobilization from affecting SLA. Next, eliminate  
    #            the second line adding NADLF.  This still allows SENWT (skeleton 
    #            lost with mobilized N) to reduce leaf area.
    #      Original Code
    #      ALFDOT = WLDOTN*F - (SLDOT+WLIDOT+WLFDOT+NRUSLF/0.16)*SLA
    #      IF (WTLF .GT. 0.0) THEN
    #         ALFDOT = ALFDOT + SLA * (NADLF/0.16) *
    #     &   (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
    #      ENDIF
    #      Revised into one line
    #-----------------------------------------------------------------------
    # 
    #      ALFDOT = WLDOTN * F - (SLDOT - SLNADDOT + WLIDOT + WLFDOT) * SLA
    
    #--------------------------------------------
    # PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
    #--------------------------------------------
    if (FHLEAF > 0.0){
      ALFDOT <- -(SLDOT + WLIDOT + WLFDOT) * SLA - (FHLEAF - WLDOTN) * SLA
    } else {
      ALFDOT <- WLDOTN * Fnew - (SLDOT - SLNADDOT + WLIDOT + WLFDOT) * SLA
    }
    AREALF <- AREALF + ALFDOT
    
    #-----------------------------------------------------------------------
    #     New line for Forage model to prevent negative LAI's in winter
    #-----------------------------------------------------------------------
    
    AREALF <- max(AREALF, 0.0)
    
    XLAI   <- AREALF / 10000.
    if ((AREALF > 0.00001) && (WTLF > 0.0)) {
      SLA    <- AREALF / WTLF
      SLAAD  <- AREALF / (WTLF - WCRLF)
      if (SLA > 999.) SLA     <- -99.  # TODO: Vericar o valor -99 
      if (SLAAD > 999.) SLAAD <- -99.  # TODO: Vericar o valor -99 
    }
    #-----------------------------------------------------------------------
    #     Remember XLAI
    #-----------------------------------------------------------------------
    LAIMX <- max(XLAI,LAIMX)
    #-----------------------------------------------------------------------
    #     Calculate "Healthy" or Non-Diseased Leaf Area Index
    #-----------------------------------------------------------------------
    #     AREAH  = AREALF - 2. * DISLA
    AREAH  <- AREALF - DISLA          # KJB correction beta factor covers this
    
    AREAH  <- max(0.,AREAH)
    XHLAI  <- AREAH / 10000.
    #-----------------------------------------------------------------------
    #     Integrate Pest Damage to Seeds
    #-----------------------------------------------------------------------
    PUNCSD <- PUNCSD + SDPDOT           #chp not used
    SEEDNO <- SEEDNO - SDIDOT
    PUNCTR <- PUNCTR + PUNDOT           #chp not used
    #-----------------------------------------------------------------------
    #     Loss in Plants due to Pests, Adjust Plant Spacing Also
    #-----------------------------------------------------------------------
    if (NPLTD > 0.0) {
      PLTPOP <- PLTPOP - NPLTD
      PLTPOP <- max(0.,PLTPOP)
      if (PLTPOP*ROWSPC > 0.0) {
        BETN <- 1.0/(ROWSPC*PLTPOP)
      }
    }
    
    if (PPLTD > 0.0) {
      PLTPOP <- PLTPOP - PPLTD*PLTPOP/100.
      PLTPOP <- max(0.,PLTPOP)
      if(PLTPOP*ROWSPC > 0.0) {
        BETN <- 1.0/(ROWSPC*PLTPOP)
      }
    }
    
    #-----------------------------------------------------------------------
    #     Terminate growth if stress causes extremely low plant weights
    #-----------------------------------------------------------------------
    if (STRWT < 0.00001) {
      if (TOPWT < 0.00001 || STMWT < 0.00001) {
        STRESS(AGEFAC, iyear, jday)
        return()
      }
      
      if (IHARI != 'R' && IHARI != 'D') {
        if ((RTWT < 0.00001) || (WTLF < 0.00001)) {
          STRESS(AGEFAC, iyear, jday)
          return()
        }
      }
    }
    
    simDataVars$SENESCE_ResWt   <-  SenWt
    simDataVars$SENESCE_ResLig  <-  SenLig
    simDataVars$SENESCE_ResE    <-  SenE
  }
  
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
  assign("PODWT", PODWT, envir = env)
  assign("PUNCSD", PUNCSD, envir = env)
  assign("PUNCTR", PUNCTR, envir = env)
  assign("RHOL", RHOL, envir = env)
  assign("RHOS", RHOS, envir = env)
  assign("RNITP", RNITP, envir = env)
  assign("RTWT", RTWT, envir = env)
  assign("SDNPL", SDNPL, envir = env)
  assign("SDRATE", SDRATE, envir = env)
  assign("SDWT", SDWT, envir = env)
  assign("SEEDNI", SEEDNI, envir = env)
  assign("SEEDNO", SEEDNO, envir = env)
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
  assign("SENESCE_ResWt", SENESCE_ResWt, envir = env)   
  assign("SENESCE_ResLig", SENESCE_ResLig, envir = env)  
  assign("SENESCE_ResE", SENESCE_ResE, envir = env)    
  assign("SENESCE_CumResWt", SENESCE_CumResWt, envir = env)
  assign("SENESCE_CumResE", SENESCE_CumResE, envir = env) 
  
  assign("CPFLF",  CPFLF , envir = env) 
  assign("CPFSTM", CPFSTM, envir = env) 
  assign("CPFRT",  CPFRT , envir = env) 
  assign("CPFNOD", CPFNOD, envir = env) 
  assign("CPFSH1", CPFSH1, envir = env) 
  assign("CPFSD1", CPFSD1, envir = env) 
  assign("PCNMIN", PCNMIN, envir = env) 
  assign("WTLSD", WTLSD, envir = env) 
  assign("WTCSD", WTCSD, envir = env) 
  assign("ALFDOT",ALFDOT, envir = env)
  assign("AREAH",AREAH , envir = env)
  assign("NLDOT",NLDOT , envir = env)
  assign("NSDOT",NSDOT , envir = env)
  assign("NRDOT",NRDOT , envir = env)
  assign("NSDDOT",NSDDOT, envir = env)
  assign("NSHDOT",NSHDOT, envir = env)
  assign("NTOVR",NTOVR , envir = env)
  assign("RHOR",RHOR  , envir = env)
  assign("RHOSH",RHOSH , envir = env)
  assign("SDWTAM",SDWTAM, envir = env)
  assign("TGROW",TGROW , envir = env)
  assign("WSDDOT",WSDDOT, envir = env)
  assign("WSHDOT",WSHDOT, envir = env)
  assign("WTCSD",WTCSD , envir = env)
  assign("WTLSD",WTLSD , envir = env)
  assign("WTNMOB",WTNMOB, envir = env)
  assign("WTNNA",WTNNA , envir = env)
  assign("SDPDOT",SDPDOT , envir = env)
  assign("PUNDOT",PUNDOT , envir = env)
  assign("DWTCO",DWTCO , envir = env)
  assign("DWTLO",DWTLO , envir = env)
  assign("DWTSO",DWTSO , envir = env)
  assign("PWTCO",PWTCO , envir = env)
  assign("PWTLO",PWTLO , envir = env)
  assign("PWTSO",PWTSO , envir = env)
  assign("WSRDOT",WSRDOT , envir = env)
  assign("WSRFDOT",WSRFDOT , envir = env)
  assign("CSRW",CSRW , envir = env)
  assign("PCNSR",PCNSR , envir = env)
  assign("RHOSR",RHOSR , envir = env)
  assign("STRWT",STRWT , envir = env)
  assign("WNRSR",WNRSR , envir = env)
  assign("WTNSR",WTNSR , envir = env)
  assign("WTNSRA",WTNSRA , envir = env)
  assign("WTNSRO",WTNSRO , envir = env)
  assign("WTSRO",WTSRO , envir = env)
  assign("CPFSTR",CPFSTR , envir = env)
  assign("NSRDOT",NSRDOT , envir = env)
  assign("WSRI",WSRI , envir = env)
  assign("WRCSRDT",WRCSRDT , envir = env)
  assign("NSROFF",NSROFF , envir = env)
  assign("NSRALL",NSRALL , envir = env)
  assign("PSRSRFL",PSRSRFL , envir = env)
  assign("PSRLYR1",PSRLYR1 , envir = env)
  assign("TPSRSRFL",TPSRSRFL , envir = env)
  assign("TPSRLYR1",TPSRLYR1 , envir = env)
  
  return()
}   

STRESS <- function(AGEFAC, iyear, jday) {
  
  #-----------------------------------------------------------------------
  #     Set Ending Plant Weights, Dates of Stages if Plants Died
  #-----------------------------------------------------------------------
  TOTWT  <- max(0.,TOTWT)
  TOPWT  <- max(0.,TOPWT)
  WTLF   <- max(0.,WTLF)
  STMWT  <- max(0.,STMWT)
  SDWT   <- max(0.,SDWT)
  SHELWT <- max(0.,SHELWT)
  RTWT   <- max(0.,RTWT)
  PODWT  <- max(0.,PODWT)
  DWNOD  <- max(0.,DWNOD)
  STRWT  <- max(0.,STRWT)
  
  YRDOY <- paste0(iyear,sprintf("%03d", jday))
  
  if (MDATE < 0) {
    #        NR8   = max(0,TIMDIF(YRSIM,YRDOY))
    MDATE <- YRDOY
  }
  
  #DAP = idpp
  #tem uma mensagem de erro aqui: 'Plant died due to extreme stress at 'xx' days after planting.')
  
  assign("MDATE", MDATE, envir = env)
  assign("STRWT", STRWT, envir = env)
  
  return()
}  
#--------------END GROW FUNCTION-----------------

#---------------ROOTS FUNCTION-------------------
ROOTS <- function(DYNAMIC, CROP, PG, ISWWAT) { 
  
  environment(INROOT) <- env
  params <- plantList$forage$params
  
  NLAYR <- nsoilay
  
  PLME      <- 'T'      # equivalente ao [.SBX]  # TODO: Levar para o template??? Leandro 12/11/2020
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RFAC1  <- params$RFAC1   #7500.0
  RLDSM  <- params$RLDSM   #0.1
  RTSDF  <- params$RTSDF   #0.015 
  RTSEN  <- params$RTSEN   #0.020
  PORMIN <- params$PORMIN  #0.02
  RTEXF  <- params$RTEXF   #0.10
  RTDEPI <- params$RTDEPI  #20.0
  XRTFAC <- params$XRTFAC  #c(0.00, 3.00, 6.00, 30.00)
  YRTFAC <- params$YRTFAC  #c(2.50, 2.50, 2.60, 2.60)
  
  NL       <- 20  #!Maximum number of soil layers 
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #---------------------------------------------------------------------
    SRDOT    <- 0.0
    SRNDOT   <- 0.0   
    RLV      <- 0.0
    RTDEP    <- 0.0       
    SENRT    <- rep(0, 20)
    SUMEX    <- 0.0
    SUMRL    <- 0.0
    SRCADDOT <- 0.0
    SRNADDOT <- 0.0
    SATFAC   <- 0.0
    
    #-----------------------------------------------------------------------
    #     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
    #-----------------------------------------------------------------------
    # if (CROP != 'FA' & ISWWAT != 'N') {
    if ((CROP != 'FA') && (ISWWAT != 'N')) {
      RFAC2 <- TABEX(YRTFAC,XRTFAC,0.0,4)
    }
    
    #***********************************************************************
    #***********************************************************************
    # EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    #   Call INROOT for initialization of root variables on
    #   day of emergence.  (GROW emergence initialization
    #   must preceed call to INROOT.)
    #-----------------------------------------------------------------------
    INROOT()
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    
    #-----------------------------------------------------------------------
    #     Calculate Root Depth Rate of Increase, Physiological Day (RFAC2)
    #-----------------------------------------------------------------------
    RFAC2 <- TABEX(YRTFAC, XRTFAC, VSTAGE, 4)
    RLNEW <- WRDOTN * RFAC1 / 10000.
    CGRRT <- AGRRT * WRDOTN
    
    #-----------------------------------------------------------------------
    #     Calculate root length per cm2 soil and initiate growth,
    #     respiration and senescence by layer
    #-----------------------------------------------------------------------
    TRTDY  <- 0.0
    
    RLGRW  <- rep(0, NL)
    RRLF   <- rep(0, NL)
    MRESPR <- rep(0, NL)
    GRESPR <- rep(0, NL)
    RESPS  <- rep(0, NL)
    for (L in 1:NLAYR) {
      TRTDY      <- TRTDY + RLV[L] * DLAYR[L]
      RRLF[L]    <- 0.0
      # RLSEN[L]  <- 0.0
      RLGRW[L]   <- 0.0
      MRESPR[L]  <- 0.0
      GRESPR[L]  <- 0.0
      RESPS [L]  <- 0.0
      
    }
    
    #-----------------------------------------------------------------------
    #     Move calculation of "yesterday's" RFAC here so have RFAC3 to use 
    # in calculating SENWT(L) - had been using RFAC1.
    #-----------------------------------------------------------------------
    
    if (RTWT - WRDOTN >= 0.0001) {
      #  RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
      #  RTWT has not yet been updated today, so use yesterday's
      #  value and don't subtract out today's growth - chp 11/13/00
      RFAC3 <- TRTDY * 10000.0 / RTWT
    } else {
      RFAC3 <- RFAC1
    }
    
    
    #-----------------------------------------------------------------------
    SRNDOT <- 0.0
    TRLDF  <- 0.0
    CUMDEP <- 0.0
    SUMEX  <- 0.0
    SUMRL  <- 0.0
    
    #Fortran
    ESW    <- rep(0, NL)
    RLDF   <- rep(0, NL)
    
    for (L in 1:NLAYR) {
      
      L1 <- L
      CUMDEP <- CUMDEP + DLAYR[L]
      SWDF   <- 1.0
      SWEXF  <- 1.0
      
      #-----------------------------------------------------------------------
      #      2/21/05 - SJR - move conditional call for water stress from CROPGRO 
      #      to FOR_ROOTS.  Allows root senescence when Water dynamics option is 
      #      turned off.  Water stress options set to no stress levels.  This 
      #      also allows output of root growth dynamics without limimiting 
      #      water or N uptake. Moved calculation of SUMEX and SUMRL after 
      #      calculation of SWDF so that both excess soil water stress (SWEXF) 
      #      and water deficit stress (SWDF) could be included in a single 
      #      conditional clause.
      #-----------------------------------------------------------------------
      
      if (ISWWAT == 'Y') {
        if (SAT[L]-SW[L] < PORMIN) {
          SWEXF <- (SAT[L] - SW[L]) / PORMIN
          SWEXF <- min(SWEXF, 1.0)
        }
        
        #        SUMEX  <- SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMEX  <- SUMEX + DLAYR[L]*(RLV[L] - RLSEN[L])*(1.0 - SWEXF)
        #        SUMRL  <- SUMRL + DLAYR(L)*RLV(L)
        SUMRL  <- SUMRL + DLAYR[L]*(RLV[L] - RLSEN[L])
        
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
      #      10/3/05 SJR
      #      Calculate losses to water-stress senescence but do not update RLV.
      #      Instead update at end of routine after calculating natural 
      #      senescence and maintenance respiration.
      #      6/21/06 SJR Moved water-stress senescence to SENMOB along with 
      #      all other types of senescence
      #-----------------------------------------------------------------------
      #        IF (RLV(L) .GT. RLDSM) THEN
      #        IF ((RLV(L) - RLSEN(L)) .GT. RLDSM) THEN
      #            RNDOT(L) = RLV(L) * (1 - RTSURV)
      #            RNDOT(L) = (RLV(L) - RLSEN(L)) * (1 - RTSURV)
      #          RLV(L) = RLV(L) * RTSURV
      #        ENDIF
      #-----------------------------------------------------------------------
      RLDF[L] <- WR[L] * DLAYR[L] * min(SWDF,SWEXF)
      if (CUMDEP < RTDEP) {
        TRLDF <- TRLDF + RLDF[L]
      } else {
        if (WR[L] > 0.0 & RLNEW > 0.0) {
          if (L == 1) {
            RTDEP <- RTDEP + DTX * RFAC2
          } else {
            RTDEP <- RTDEP + DTX * RFAC2 * min(SWDF,SWEXF) * (1. + 0.25 * (1. - max(SWFAC,0.40)))
            #-----------------------------------------------------------------------
            #-KJB  DO NOT WANT TO DECREASE ROOT DEPTH WITH STRESS.  IF PG TO ROOTS
            # IS LOW BECAUSE OF SEED GROWTH OR IF WATER DEFICIT CAUSES LOW PG TO ROOTS
            # DESPITE INCREASED PARTITIONING TO ROOTS, { RLV WILL NOT INCREASE
            # SO THERE WILL BE NO EFFECTIVE INCREASE IN WATER EXTRACTION.  IDEALLY THE
            # DECISION SHOULD BE BASED ON AMOUNT OF ROOT GROWTH VS NORMAL UNSTRESS.
            # ACCELERATE FROM 1.0 TO 0.5, STAY FLAT, SHOULD DROP AGAIN, 0.5 TO 0.0
            # AS THE OTHER FUNCTION ACTS.  NOTE:  SWFAC*2.0 WAS NOT USED IN ALL 40 CASES
            # EXCEPT 1985-RAINFED WHERE DELETING INCREASED YIELD 2764 TO 2770 KG/HA.
            # NOW ACCELERATING ROOT GROWTH BY ABOUT 12-13% AT SWFA!=0.50.  THIS
            # HELPS IOWA 88 AND VEG STRESS TRTS IN 1981 AND 1985. INCR SEED AND BIO.
            #-----------------------------------------------------------------------
          }
          # VERIFICAR: DEPMAX estava sendo calculado no RUNINIT
          DEPMAX <- DS[NLAYR]
          if (RTDEP > DEPMAX) {
            RTDEP <- DEPMAX
          }
        }
        RLDF[L] <- RLDF[L] * (1. - (CUMDEP - RTDEP) / DLAYR[L])
        TRLDF <- TRLDF + RLDF[L]
        
        break()
      }
    }
    #-----------------------------------------------------------------------
    #     Calculate root senescence, growth, maintenance and growth
    #     respiration, and update root length density for each layer.
    #-----------------------------------------------------------------------
    
    if (SUMRL > 0.0) {
      SATFAC <- SUMEX/SUMRL
    } else {
      SATFAC <- 0.0
    }
    
    TRLV     <- 0.0
    TRLSEN   <- 0.0
    TRLNSEN  <- 0.0
    TRLGRW   <- 0.0
    
    for (L in 1:L1) {
      if (TRLDF < 0.00001) {
        RRLF[L] <- 1.0
      } else {
        RRLF[L] <- RLDF[L]/TRLDF
      }
      #-----------------------------------------------------------------------
      #       MRESPR, GRESPR, and RESPS are not used anywhere
      #                       chp 9/22/98
      #-----------------------------------------------------------------------
      # TODO: Verificar Isso Leandro 23/10/2020
      # MRESPR[L] = (RLV[L]/RFAC1*RO*DLAYR[L]*100.0 +RRLF[L]*FRRT*PG*RP) * 44.0 / 30.0
      MRESPR[L] <- ((RLV[L] - RLSEN[L])/RFAC1*RO*DLAYR[L]*100.0 + RRLF[L]*FRRT*PG*RP) * 44.0 / 30.0
      GRESPR[L] <- RRLF[L] * (CGRRT-WRDOTN) * 44.0 /30.0
      RESPS [L] <- MRESPR[L] + GRESPR[L]
      #-----------------------------------------------------------------------
      RLGRW[L]  <- RRLF[L] * RLNEW / DLAYR[L] #cm[root]/cm3[ground]
      
      #-----------------------------------------------------------------------
      #      10/3/05 SJR Track water-stress senescence (RLNSEN, TRLNSEN, SRNDOT)
      #       separately from natural senescence so each can be lost at
      #        proper N concentration.
      #      10/4/05 SJR moved calculation of RFAC 3 to top of integration step
      #      and replaced RFAC1 in calculation of SENRT with RFAC3.
      #-----------------------------------------------------------------------
      
      #        RLNSEN(L) = RLV(L) * RNDOT(L)
      #        RLNSEN(L) = RNDOT(L)
      #        RLSEN(L) = RLV(L) * RTSEN * DTX
      SENRT[L] <- RLSEN[L] * DLAYR[L] / RFAC3 * 10000. * 10. + RLNSEN[L] * DLAYR[L] / RFAC3 * 10000. * 10. #kg/ha
      RLV[L]   <- RLV[L] - RLNSEN[L] - RLSEN[L] + RLGRW[L]
      
      TRLGRW   <- TRLGRW + RLGRW[L] * DLAYR[L]
      TRLNSEN  <- TRLNSEN + RLNSEN[L] * DLAYR[L]
      TRLSEN   <- TRLSEN + RLSEN[L] * DLAYR[L]
      
      TRLV <- TRLV + RLV[L] * DLAYR[L]
      
    }
    
    #     SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3
    #     Sum RLSEN for total root senescence today. chp 11/13/00  
    #      SRNDOT = TRLNSEN / RFAC3 * 10000. !g/m2
    #      SRDOT = SRMDOT + SRNDOT     
    #-----------------------------------------------------------------------
    #     7/27/05 SJR Calculate today's NADRT lost to senescence.
    #     8/2/05 SJR SRDOT is lost at minimum composition/concentration
    #                        so all CADRT and NADRT are left in the N & CH2O pools
    #      10/04/05 SJR - Required  only for N and C lost with roots senesced
    #                        due to water stress. 
    #      6/21/06 SJR No longer required after moving senescence to SENMOB
    #-----------------------------------------------------------------------
    #      SRCADDOT = CADRT * MIN(1.0,(SRNDOT) / (RTWT - SRMDOT))
    #      SRNADDOT = NADRT * MIN(1.0,(SRNDOT) / (RTWT - SRMDOT))
    #      SRDOT = SRDOT + SRNADDOT / 0.16 + SRCADDOT 
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  assign("RLV", RLV, envir = env)
  assign("RTDEP", RTDEP, envir = env)
  assign("SATFAC", SATFAC, envir = env)
  assign("SENRT", SENRT, envir = env)
  assign("SRDOT", SRDOT, envir = env)
  
  assign("TRLV", TRLV, envir = env)
  assign("SRCADDOT", SRCADDOT, envir = env)
  assign("SRNADDOT", SRNADDOT, envir = env)
  assign("SRNDOT", SRNDOT, envir = env)
  
  return()
} 

INROOT <- function (){  
  
  params <- plantList$forage$params
  
  NL        <- 20  #!Maximum number of soil layers 
  
  PLME      <- 'T'      # equivalente ao [.SBX]  # TODO: Levar para o template??? Leandro 12/11/2020
  RLDSM     <- params$RLDSM
  
  #!*ROOT PARAMETERS
  RTDEPI  <- params$RTDEPI # 20.0
  RFAC1   <- params$RFAC1   #7500.0
  
  # 
  #***********************************************************************
  #     INITIALIZE ROOT DEPTH AT EMERGENCE
  #-----------------------------------------------------------------------
  TRLCAP <- 0.0
  CUMDEP <- 0.
  
  RLCAP <- rep(0,NLAYR)
  for (L in 1:NLAYR) {
    RLV[L]   <- 0.0
    RLCAP[L] <- 0.0
  }
  
  if (PLME == 'T') {
    
    RLINIT <- RTWT / 10000 * RFAC1
    
    for (L in 1:NLAYR) {
      CUMDEP   <- CUMDEP + DLAYR[L]
      RLCAP[L] <- RLDSM * DLAYR[L]
      TRLCAP   <- TRLCAP + RLCAP[L]
    }
    
    if (RLINIT <= TRLCAP) {
      RLRATIO <- 1.0
      RTDEP   <- RLINIT / RLDSM
    } else {
      RLRATIO <- RLINIT / TRLCAP
      RTDEP   <- CUMDEP
    }
    
    for (L in 1:NLAYR) {
      if (RLINIT >= RLCAP[L]) {
        RLV[L] <- RLCAP[L] * RLRATIO /DLAYR[L]
        RLINIT <- RLINIT - (RLCAP[L] * RLRATIO)
      } else {
        RLV[L] <- RLINIT / DLAYR[L]
        RLINIT <- 0.0
      }
    }
  } else {
    
    
    RTDEP <- RTDEPI
    #-----------------------------------------------------------------------
    #     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
    #     RTDEPTI (ROOT DEPTH AT EMERGENCE)
    #-----------------------------------------------------------------------
    
    
    for (L in 1:NLAYR) {
      DEP <- min(RTDEP - CUMDEP, DLAYR[L])
      RLINIT <- WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP * 10000 )
      
      CUMDEP <- CUMDEP + DEP
      RLV[L] <- RLINIT / DLAYR[L]
      if (CUMDEP >= RTDEP) {
        break()
      }
    }
  }
  
  assign("RLV", RLV, envir = env)
  assign("RTDEP", RTDEP, envir = env)
  return()
}
#--------------END ROOTS FUNCTION----------------

#---------------DEMAND FUNCTION------------------
DEMAND <- function(DYNAMIC, DAS, CROP, PAR, PGAVL, RPROAV, TAVG) {
  
  environment(SDCOMP) <- env
  params <- plantList$forage$params
  
  TS <- 24
  
  XLFEST    <- params$XLFEST
  YLFEST    <- params$YLFEST 
  YSREST    <- params$YSREST
  YSTEST    <- params$YSTEST
  SDLEST    <- params$SDLEST  
  FRSTRF    <- params$FRSTRF  
  FRSTRMX   <- params$FRSTRMX 
  LRMOB     <- params$LRMOB   
  NRMOB     <- params$NRMOB   
  NMOBSRN   <- params$NMOBSRN 
  NMOBSRX   <- params$NMOBSRX 
  PLME      <- 'T'      # encontrado *PLANTING DETAILS  # TODO: Levar para o template??? Leandro 12/11/2020
  PROLFR    <- params$PROLFR  
  PRORTR    <- params$PRORTR  
  PROSRF    <- params$PROSRF  
  PROSRI    <- params$PROSRI  
  PROSRR    <- params$PROSRR  
  PROSTR    <- params$PROSTR  
  SDAGPL    <- params$SDAGPL  
  TYPLMOB   <- params$TYPLMOB 
  TYPNMOB   <- params$TYPNMOB 
  YSTOR     <- params$YSTOR   
  KCOLD     <- params$KCOLD  
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP  <- params$SDLIP   #0.200 # Fraction oil in seeds (g(oil)/g(seed))
  SDPRO  <- params$SDPRO   #0.400 # Fraction protein in seeds (g(protein)/g(seed))
  SLAVAR <- params$SLAVR  #370   # Specific leaf area of cultivar under standard growth conditions (cm2/g)
  SIZELF <- params$SIZLF  #200   # Maximum size of full leaf (three leaflets) (cm2)
  THRESH <- params$THRSH  #78    # Threshing percentage. The maximum ratio of (seed/(seed+shell)) at maturity. Causes seeds to stop growing as their dry weight
  XFRUIT <- params$XFRT  #1.000  # Maximum fraction of daily growth that is partitioned to seed + shell 
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  LNGSH <- params$LNGSH  #10.0  # Time required for growth of individual shells (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  # !*VEGETATIVE PARTITIONING PARAMETERS
  XLEAF   <- params$XLEAF   #c( 0.0,  1.5,   3.3,   5.0,  7.8,  10.5,  30.0,  40.0)
  YLEAF   <- params$YLEAF   #c(0.41, 0.42,  0.42,  0.41, 0.36,  0.32,  0.31,  0.31)
  YSTEM   <- params$YSTEM   #c(0.09, 0.13,  0.21,  0.29, 0.37,  0.49,  0.49,  0.49)
  FRLFMX  <- params$FRLFMX  #0.70
  FRLFF   <- params$FRLFF   #0.24
  FRSTMF  <- params$FRSTMF  #0.55
  
  #!*LEAF GROWTH PARAMETERS
  FINREF <- params$FINREF  #180.
  SLAREF <- params$SLAREF  #350.
  SIZREF <- params$SIZREF  #171.40
  VSSINK <- params$VSSINK  #5.0
  SLAMAX <- params$SLAMAX  #950.
  SLAMIN <- params$SLAMIN  #250.0
  SLAPAR <- params$SLAPAR  #-0.048
  TURSLA <- params$TURSLA  #1.50
  XVGROW <- params$XVGROW  #c( 0.0,  1.0,  2.0,  3.0,  4.0,  5.0)
  YVREF  <- params$YVREF   #c( 0.0, 20.0, 55.0,110.0,200.0,320.0)
  # YVGROW <- rep(0,6) #preenchido com uma função de interpolacao/lookup (TABEX)
  XSLATM <- params$XSLATM  #c(-50.0,  00.0,  12.0,  22.0,  60.0)         
  YSLATM <- params$YSLATM  #c( 0.25,  0.25,  0.25,  1.00,   1.0)
  #!*SEED  COMPOSITION VALUES 
  CARMIN <- params$CARMIN  #0.180
  LIPOPT <- params$LIPOPT  #23.65 
  LIPTB  <- params$LIPTB   #7.16
  SLOSUM <- params$SLOSUM  #9.08000022E-03
  
  #!*SEED AND SHELL GROWTH PARAMETERS
  FNSDT  <- params$FNSDT   #c(6.0,  21.0,  23.5,  41.0) #+ QDR in .SPE
  TYPSDT <- params$TYPSDT  #"QDR"
  SHLAG  <- params$SHLAG   #0
  SRMAX  <- params$SRMAX   #0.300
  XFRMAX <- params$XFRMAX  #0
  XXFTEM <- params$XXFTEM  #c(0.00, 5.00, 20.00, 35.00, 45.00, 60.00)
  YXFTEM <- params$YXFTEM  #c(1.00, 1.00, 1.00 ,  1.00,  0.00,  0.00)
  XTRFAC <- params$XTRFAC  #c(0.00,  0.50,  0.75,  1.00)              
  YTRFAC <- params$YTRFAC  #c(0.00,  0.00,  0.00,  0.00)
  
  #!*CARBON AND NITROGEN MINING PARAMETERS
  NMOBMX <- params$NMOBMX  #0.090
  NRCVR  <- params$NRCVR   #0.15
  NVSMOB <- params$NVSMOB  #0.35
  #!*PLANT COMPOSITION VALUES
  PLIGSD <- params$PLIGSD   #0.020
  PMINSD <- params$PMINSD   #0.025
  POASD  <- params$POASD    #0.040
  PROLFF <- params$PROLFF   #0.112
  PROLFI <- params$PROLFI   #0.356
  PRORTF <- params$PRORTF   #0.056
  PRORTI <- params$PRORTI   #0.092
  PROSTF <- params$PROSTF   #0.035
  PROSTI <- params$PROSTI   #0.150
  #!*RESPIRATION PARAMETERS
  RCH2O  <- params$RCH20  #1.242
  RLIG   <- params$RLIG   #2.174
  RLIP   <- params$RLIP   #3.106
  RMIN   <- params$RMIN   #0.050
  RNO3C  <- params$RNO3C  #2.556
  ROA    <- params$ROA    #0.929
  RPRO   <- params$RPRO   #0.360
  
  NCOHORTS <- 300 #from line 51 in ModuleDefs.for NCOHORTS = 300, !Maximum number of cohorts
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    CDMSDR <- 0.0
    GDMSDR <- 0.0
    FNINSD <- 0.0
    NDMNEW <- 0.0
    NDMREP <- 0.0
    NDMSD  <- 0.0
    NDMSH  <- 0.0
    NDMSDR <- 0.0
    NDMVEG <- 0.0
    # NMOBR = 0.0
    SDGR   <- 0.0
    FNINL  <- 0.0
    FNINS  <- 0.0
    FNINR  <- 0.0
    NMINEP <- 0.0
    
    RPRPUN <- 1.0 
    TMPFAC <- 1.0
    CUMNSF <- 1.0
    
    FNINSR <- 0.0
    
    #-----------------------------------------------------------------------
    #     SET VARIETY SPECIFIC LEAF PARAMETERS
    #-----------------------------------------------------------------------
    if (CROP != 'FA') {
      DUMFAC <- SLAVAR / SLAREF
      Fnew   <- DUMFAC * FINREF 
      FVEG   <- DUMFAC * SLAMAX
      SLAMN  <- DUMFAC * SLAMIN
      SLAMX  <- DUMFAC * SLAMAX
      GROMAX <- 0.0
      SIZRAT <- SIZELF / SIZREF
      
      for (I in 1:6){
        YVGROW[I] <- SIZRAT * YVREF[I]
      }
      
      #-----------------------------------------------------------------------
      #     INITIALIZE PARTITIONING PARAMETERS - seedlings
      #-----------------------------------------------------------------------
      FRLF  <- TABEX(YLEAF,XLEAF,0.0,8)
      FRSTM <- TABEX(YSTEM,XLEAF,0.0,8)
      FRSTR <- TABEX(YSTOR,XLEAF,0.0,8)
      FRRT  <- 1.0 - FRLF - FRSTM - FRSTR
      
    }
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    XFRT   <- XFRUIT
    ADDSHL <- 0.0
    TURXFR <- 0.0
    GDMSD  <- 0.0
    CDMSD  <- 0.0
    NDMSD  <- 0.0
    GDMSDR <- 0.0
    CDMSDR <- 0.0
    NDMSDR <- 0.0
    CDMREP <- 0.0
    NAGE   <- 0
    for (NPP in 1:365) {
      PHTIM[NPP] <- 0.
      PNTIM[NPP] <- 0.
    }
    FNINSD <- SDPRO * 0.16   
    FNINL  <- PROLFI * 0.16  
    FNINS  <- PROSTI * 0.16  
    FNINR  <- PRORTI * 0.16 
    FNINSR <- PROSRI * 0.16
    NVSTL  <- FNINL
    NVSTS  <- FNINS
    NVSTR  <- FNINR
    NVSTSR <- FNINSR
    
    #-----------------------------------------------------------------------
    #     INITIALIZE PARTITIONING PARAMETERS for transplants adjusted for 
    #      SDAGE and growth temperature )
    #-----------------------------------------------------------------------
    
    if (PLME == 'T') {
      if (PHZACC[4] >= SDLEST) { 
        FRLF  <- TABEX(YLFEST,XLFEST,VSTAGE,8)
        FRSTM <- TABEX(YSTEST,XLFEST,VSTAGE,8)
        FRSTR <- TABEX(YSREST,XLFEST,VSTAGE,8)
      } else {
        FRLF  <- TABEX(YLEAF,XLEAF,VSTAGE,8)
        FRSTM <- TABEX(YSTEM,XLEAF,VSTAGE,8)
        FRSTR <- TABEX(YSTOR,XLEAF,VSTAGE,8)
      }
      FRRT <- 1.0 - FRLF - FRSTM - FRSTR
    }
    
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    # DAS = MAX(0,TIMDIF(YRSIM,YRDOY))  # TODO: DAS VEM POR PARÃMETRO NECESSÁRIO ESSA ATRIBUIÇÃO?? LEANDRO 03/11/2020 
    #-----------------------------------------------------------------------
    #     Compute max N mining, NMINEP, based on stage-dependent mining
    #     rate, NMOBR
    #-----------------------------------------------------------------------
    #     Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day
    #     NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
    #-----------------------------------------------------------------------
    #    9/27/95 ACCELERATE N MOBILIZATION AFTER R5, FUNCTION OF (1-SWFAC)
    #     ALLOWS ACCELERATING BY 50% IF MAX DEFICIT.
    #     2/6/96 SOMETIMES SEEDS FILL, XPOD IS LOW, THEN N MOBILIZATION SLOWS
    #     I DON'T REALLY WANT THAT, LATE IN CYCLE.  KJB
    #     NOW, DXR57 HITS CLOSE TO 1 AT MATURITY AND PREVENTS THAT
    #-----------------------------------------------------------------------
    
    #      NMOBR  = NVSMOB * NMOBMX * TDUMX
    #      IF (DAS .GT. NR5) THEN
    #        NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC))
    #     &  * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB)
    #     &  * MAX(XPOD,DXR57**2.))
    #      ENDIF
    
    #-----------------------------------------------------------------------
    #      DSSAT4 equation
    #-----------------------------------------------------------------------
    #      NMINEP = NMOBR * (WNRLF + WNRST + WNRRT + WNRSH)
    
    #-----------------------------------------------------------------------
    #      Add N from storage to N available for potential mobilization
    #-----------------------------------------------------------------------
    #        NMINEP = NMOBR * (WNRLF + WNRST + WNRSH) +
    #     &              NMOBR * PPMFAC * WNRRT +
    #     &              NMOBSR * WNRSR
    
    #-----------------------------------------------------------------------
    
    #-----------------------------------------------------------------------
    if (DAS >= NR1) {
      #-----------------------------------------------------------------------
      #     Accumulate physiological age of flower (PNTIM) and pod (PHTIM) cohorts
      #-----------------------------------------------------------------------
      
      # VERIFICAR: Nao seria DAS <= NR1 ? Pois caso DAS seja menor, a linha a seguir dara erro por ser indice negativo.
      if (DAS == NR1) {
        PNTIM[1] <- 0.0
      } else {
        PNTIM[DAS - NR1 + 1] = PNTIM[DAS - NR1] + TDUMX
      }
      
      if (DAS <= NR2) {
        PHTIM[1] <- 0.0
      } else {
        PHTIM[DAS - NR2 + 1] = PHTIM[DAS - NR2] + TDUMX
      }
      
      #-----------------------------------------------------------------------
      #     Calculate function for modifying seed growth rate with temperature
      #-----------------------------------------------------------------------
      TMPFAC <- 0
      TMPFCS <- 0
      for (I in 1:24) {
        # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
        # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
        
        TMPFAC <- CURV(TYPSDT,FNSDT[1], FNSDT[2], FNSDT[3], FNSDT[4], TGRO[I])
        TMPFCS <- TMPFCS + TMPFAC
      }
      
      TMPFAC <- TMPFCS / 24.
      
      #-----------------------------------------------------------------------
      #       Calculate reduction in seed growth due to insect punctures
      #-----------------------------------------------------------------------
      if (PUNCSD > 0.001) {
        REDPUN <- 1.0 - (PUNCTR/PUNCSD) * RPRPUN
        REDPUN <- max(0.0,REDPUN)
      } else {
        REDPUN <- 1.0
      }
      #-----------------------------------------------------------------------
      #       Water stress factor (TURADD) effect on reproductive growth and
      #       pod addition.  Stress is defined to INCREASE growth and addition.
      #-----------------------------------------------------------------------
      TURADD <- TABEX (YTRFAC,XTRFAC,TURFAC,4)
      #-----------------------------------------------------------------------
      #     Calculate maximum growth per seed based on temp and seed punctures
      #-----------------------------------------------------------------------
      SDGR <- SDVAR * TMPFAC * REDPUN * (1.-(1.-DRPP)*SRMAX) * (1. + TURADD)
      #-----------------------------------------------------------------------
      #     Initialize Seed Growth Demands and CH2O and N required for seed
      #       growth
      #-----------------------------------------------------------------------
      GDMSD  <- 0.0
      CDMSD  <- 0.0
      NDMSD  <- 0.0
      GDMSDR <- 0.0
      CDMSDR <- 0.0
      NDMSDR <- 0.0
      #-----------------------------------------------------------------------
      if (DAS > NR2) {
        for (NPP in 1:(DAS - NR2)) { 
          #-----------------------------------------------------------------------
          #     Calculate physiol age of seed cohort.  Do not allow seed to grow
          #     until shells are greater than LAGSD physiol age.
          #-----------------------------------------------------------------------
          PAGE <- PHTIM[DAS - NR2 + 1] - PHTIM[NPP]
          if (PAGE >= LAGSD) {
            #-----------------------------------------------------------------------
            #     Allow cohort growth until threshing limit (seed wt./pod wt) occurs
            #     taking into account damage by pests to seed and shells
            #-----------------------------------------------------------------------
            REDSHL <- 0
            if (SDDES[NPP] > 0) {
              REDSHL <- WTSHE[NPP] * SDDES[NPP] / (SDDES[NPP] + SDNO[NPP])
            }
            SDMAX <- (WTSHE[NPP] - REDSHL) * THRESH / (100 - THRESH) - WTSD[NPP]
            SDMAX <- max(0, SDMAX)
            #-----------------------------------------------------------------------
            #     Compute Seed Growth Demand, GDMSD, and N required for seed, NDMSD
            #-----------------------------------------------------------------------
            GDMSD  <- GDMSD  + min(SDGR * SDNO[NPP] * REDPUN, SDMAX)
          }
        }
        #-----------------------------------------------------------------------
        #     Call seed composition routine
        #-----------------------------------------------------------------------
        SDCOMP(TAVG)
        
        NDMSD  <- FNINSD * GDMSD
        #-----------------------------------------------------------------------
        #     Calculate Amount of Mobilized N Which Can be Used for Seed Growth,
        #     NDMSDR, potential seed growth from this source of N, GDMSDR,
        #     and CH2O required for this seed growth from mobilized N, CDMSDR
        #-----------------------------------------------------------------------
        if (NDMSD > NMINEP) {
          NDMSDR <- NMINEP
        } else {
          NDMSDR <- NDMSD
        }
        GDMSDR <- NDMSDR / FNINSD
        CDMSDR <- GDMSDR * (AGRSD1 + FNINSD * 6.25 * RPRO)
        #-----------------------------------------------------------------------
        #    Compute Total CH2O Demand to Grow GDMSD g Tissue
        #-----------------------------------------------------------------------
        CDMSD <- (max(0, (GDMSD - GDMSDR))) * AGRSD2 + CDMSDR
      }
    }
    #-----------------------------------------------------------------------
    #     Compute max growth per shell, depending on temp, daylength
    #-----------------------------------------------------------------------
    GRRAT1 <- SHVAR * TMPFAC * (1- (1-DRPP) * SRMAX) * (1.0 + TURADD)
    #-----------------------------------------------------------------------
    #     Initialize Shell Growth Demand, N (NDMSH) and C (CDMSH) needed for growth
    #-----------------------------------------------------------------------
    GDMSH <- 0.0
    NDMSH <- 0.0
    CDMSH <- 0.0
    #-----------------------------------------------------------------------
    #     Compute growth demand for shells, GDMSH, allowing slow growth
    #     until LNGPEG age, then potential growth until LNGSH
    #-----------------------------------------------------------------------
    if (DAS > NR2) {
      for (NPP in 1:(DAS - NR2)) {  
        NAGE <- DAS - NR2 + 1 - NPP  #NAGE not used - chp
        PAGE <- PHTIM[DAS - NR2 + 1] - PHTIM[NPP]
        if (PAGE <= LNGSH & SHELN[NPP] >= 0.001 & GRRAT1 >= 0.001) {
          if (PAGE >= LNGPEG) {
            #Shells between LNGPEG and LNGSH
            ADDSHL <- GRRAT1 * SHELN[NPP]
          } else {
            #Shells < LNGPEG
            ADDSHL <- GRRAT1 * SHELN[NPP] * SHLAG
          }
        }
        GDMSH  <- GDMSH + ADDSHL
      }
      #-----------------------------------------------------------------------
      #     Compute CH2O required for the potential shell growth
      #-----------------------------------------------------------------------
      CDMSH <- GDMSH * AGRSH2
    }
    #-----------------------------------------------------------------------
    #     Compute TEMXFR, the temp effect on partitioning to pods
    #     High temp would increase fraction growth to vegetative tissue
    #-----------------------------------------------------------------------
    TEMXFR <- 0.
    for (I in 1:24) {
      # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
      # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
      
      TEMXFR <- TEMXFR + TABEX(YXFTEM,XXFTEM,TGRO[I],6)
    }
    TEMXFR <- TEMXFR/24.
    # 24 changed to TS by Bruce Kimball on 3Jul17
    
    #-----------------------------------------------------------------------
    #     Partitioning to pods is increased under drought stress conditions
    #        depending on XFRMAX, an input parameter
    #-----------------------------------------------------------------------
    TURXFR <- XFRMAX * (1 - TURFAC)
    TURXFR <- min(TURXFR, 1)
    TURXFR <- max(TURXFR, 0)
    #-----------------------------------------------------------------------
    #     Night length and temperature are multiplicative
    #     but turgor effect adds to the partitioning
    #-----------------------------------------------------------------------
    XFRT <- XFRUIT * TEMXFR + XFRUIT * TURXFR
    #     XFRT = XFRUIT * RNIT * TEMXFR   #NEED TO FIX FOR DAYLENGTH EFFECT
    XFRT <- min(XFRT,1.0)
    XFRT <- max(XFRT,0.0)
    #-----------------------------------------------------------------------
    #    Total Potential Available CH2O for Reprod Growth (CAVTOT)
    #    and total CH2O needed for potential reproductive growth (CDMREP)
    #-----------------------------------------------------------------------
    CAVTOT <- PGAVL * XFRT
    CDMREP <- CDMSH + CDMSD
    #-----------------------------------------------------------------------
    #    Adjust C-Demand for New Growth if C-Available is Less than C Demand
    #    Also adjust tissue growth demand for seeds and shells
    #-----------------------------------------------------------------------
    GDMSDO <- GDMSD
    if (CDMREP > CAVTOT) {
      if (CDMSD > CAVTOT) {
        CDMSH <- 0.0
        GDMSH <- 0.0
        CDMSD <- CAVTOT
        if (CDMSDR > CAVTOT) {
          CDMSDR <- CAVTOT
        }
        GDMSD  <- (max(0.0,(CDMSD-CDMSDR)))/ AGRSD2 + CDMSDR / (AGRSD1 + FNINSD * 6.25 * RPRO)
        NDMSDR <- GDMSDR * FNINSD
      } else {
        CDMSH <- CAVTOT - CDMSD
        GDMSH <- CDMSH/AGRSH2
      }
      CDMREP <- CDMSD + CDMSH
    }
    #-----------------------------------------------------------------------
    #     Compute N demand for seed, shell, and total reproductive growth
    #-----------------------------------------------------------------------
    NDMSD  <- GDMSD * FNINSD
    NDMSH  <- GDMSH * FNINSH
    NDMREP <- NDMSD + NDMSH
    
    #-----------------------------------------------------------------------
    #     Vegetative partitioning factors and demand for C and N for new
    #     growth before VSSINK, assume leaf expansion is fixed, compute
    #     SLA based on function of light, temp, etc, then compute
    #     FRLF (leaf partitioning), then FRRT, FRSTM
    #-----------------------------------------------------------------------
    #     Check to See if New Vegetative Tissue Can Be Grown, Using PGAVL
    #-----------------------------------------------------------------------
    CDMVEG <- max(0.0,(1.-XFRT)*PGAVL)
    NDMVEG <- 0.0
    CDMVEG <- (PGAVL * XFRT - CDMREP) + CDMVEG
    
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
      FRLFM  <- TABEX (YLFEST, XLFEST, VSTAGE, 8)
      FRSTMM <- TABEX (YSTEST, XLFEST, VSTAGE, 8)
      FRSTRM <- TABEX (YSREST, XLFEST, VSTAGE, 8)
      YY   <- FRLFM - FRLFF 
      XX   <- FRSTMM - FRSTMF
      XSTR <- FRSTRM - FRSTRF
    }
    #-----------------------------------------------------------------------
    if (DAS < NR1) {
      #-----------------------------------------------------------------------
      #     Calculate Pattern of Vegetative Partitioning, a function of V-STAGE
      #-----------------------------------------------------------------------
      if (PHZACC[4] >= SDLEST) { 
        FRLF  <- TABEX(YLFEST,XLFEST,VSTAGE,8)
        FRSTM <- TABEX(YSTEST,XLFEST,VSTAGE,8)
        FRSTR <- TABEX(YSREST,XLFEST,VSTAGE,8)
      } else {
        FRLF  <- TABEX(YLEAF,XLEAF,VSTAGE,8)
        FRSTM <- TABEX(YSTEM,XLEAF,VSTAGE,8)
        FRSTR <- TABEX(YSTOR,XLEAF,VSTAGE,8)
      }
    } else {
      #-----------------------------------------------------------------------
      #     Partitioning between vegetative tissues depends on development
      #     as expressed by FRACDN, the relative development between R1 and NDLEAF
      #-----------------------------------------------------------------------
      FRLF    <- FRLFM - YY * FRACDN
      FRSTM   <- FRSTMM - XX * FRACDN
      FRSTR   <- FRSTRM - XSTR * FRACDN
      if ( DAS >= NDLEAF) {
        FRLF  <- FRLFF
        FRSTM <- FRSTMF
        FRSTR <- FRSTRF
      }
    }
    
    FRRT <- 1. - FRLF - FRSTM - FRSTR
    
    #      Location to insert code to increase partitioning to leaf
    #      in spring and summer but not fall.  Per Dr. Boote.
    
    if (PPTFAC > 0.0) {
      FRSTR  <- (FRSTRMX - FRSTR) * PPTFAC + FRSTR
      TFRLF  <- FRLF/(FRLF + FRSTM + FRRT) * (1-FRSTR)
      TFRSTM <- FRSTM/(FRLF + FRSTM + FRRT) * (1-FRSTR)
      TFRRT  <- FRRT/(FRLF + FRSTM + FRRT) * (1-FRSTR)
      FRLF   <- TFRLF
      FRSTM  <- TFRSTM
      FRRT   <- 1.0 - (FRLF + FRSTM + FRSTR)
    }
    
    #-----------------------------------------------------------------------
    #     Compute F, specific leaf area for new leaf weight
    #-----------------------------------------------------------------------
    TPHFAC <- 0
    for (I in 1:24){
      # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
      # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
      
      TPHFAC <- TPHFAC + TABEX (YSLATM,XSLATM,TGRO[I],5)
    }
    TPHFAC <- TPHFAC/24.
    #-----------------------------------------------------------------------
    PARSLA <- (SLAMN+(SLAMX-SLAMN) * exp(SLAPAR*PAR)) / SLAMX
    TURFSL <- max(0.1, (1.0 - (1.0 - TURFAC)*TURSLA))
    
    #Nitrogen effect added by Diego
    #-----------------------------------------------------------------------
    #    Find and Read Leaf Growth Section
    #-----------------------------------------------------------------------
    
    #---------------------------------------------------------------- added by Diego      
    # CALL GETLUN('FILEC', LUNCRP)      
    # OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
    # SECTION = '!*LEAF'
    # CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
    # CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
    # CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
    # READ(C255,'(24X,F6.0)') NSLA
    # !        WRITE(1000,'(I7,F10.3)') YRDOY,NSLA
    # CLOSE (LUNCRP)
    
    #-----------------------------------------------------------------
    NSLA      <- params$NSLA
    
    if (NSLA > 1.2) {                              #To limit NSLA to 1.2
      NSLA <- 1.2 
    }
    NFSL   <- max(0.1, (1.0 - (1.0 - NSTRES) * NSLA))       
    CUMNSF <- 0.75*CUMNSF + 0.25*NFSL  
    #-----------------------------------------------------------------------
    #     Compute overall effect of TMP, PAR, water stress on SLA (F), first
    #     for veg stages, then transition to rep stage from R1 to end leaf
    #     effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
    #-----------------------------------------------------------------------
    #* NFSL !NFSL added by Diego
    FFVEG <- FVEG * TPHFAC * PARSLA * TURFSL * CUMNSF 
    #-----------------------------------------------------------------------
    #     Compute overall effect of TMP, PAR, water stress on SLA (F), first
    #     for veg stages, then transition to rep stage from R1 to end leaf
    #     effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
    #-----------------------------------------------------------------------
    FFVEG <- FVEG * TPHFAC * PARSLA * TURFSL
    Fnew  <- FFVEG 
    if (XFRT*FRACDN >= 0.05) { 
      Fnew <- FFVEG * (1.0 - XFRT * FRACDN) 
    }
    #-----------------------------------------------------------------------
    #     For determinate plants (XFRUIT=1.) leaf expansion stops at NDLEAF
    #-----------------------------------------------------------------------
    if (XFRUIT > 0.9999 & DAS >= NDLEAF) { 
      Few <- 0.0 
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
      GROYES <- GROMAX
      GROMAX <- TABEX(YVGROW,XVGROW,VSTAGE,6) * SIZELF/SIZREF
      GAINNW <- (GROMAX - GROYES) * PLTPOP
      #-----------------------------------------------------------------------
      #     CALCULATE MINIMUM WEIGHT NEEDED TO ADD GAINNW LEAF AREA/M2,
      #     AND AMOUNT OF LEAF WEIGHT WHICH CAN BE GROWN WITH PG AVAILABLE
      #-----------------------------------------------------------------------
      if (Fnew > 0.0) {
        GAINWT <- GAINNW/Fnew
      } else {
        GAINWT <- 0.0
      }
      #-----------------------------------------------------------------------
      #     Compute fraction of C partitioned to leaves, based on F, VSSINK
      #     Limit leaf pertitioning to FRLFMX (i.e., FRLFMX = 0.7)
      #-----------------------------------------------------------------------
      FRLF <- (AGRLF*GAINWT)/(CDMVEG + 0.0001)
      if (FRLF > FRLFMX) {
        GAINWT <- (CDMVEG/AGRLF) * FRLFMX
        # VERIFICAR: F novamente. Mudar
        GAINNW <- GAINWT * Fnew
        FRLF   <- FRLFMX
      }
      #-----------------------------------------------------------------------
      #     Recompute FRSTM, FRSTR, and FRRT based on FRLF
      #-----------------------------------------------------------------------
      FRSTM <- (1. - FRLF) * FRSTM / (FRSTM + FRRT + FRSTR)
      FRSTR <- (1. - FRLF) * FRSTR / (FRSTM + FRRT + FRSTR)
      FRRT  <- 1. - FRLF - FRSTM - FRSTR
      #-----------------------------------------------------------------------
    }
    #-----------------------------------------------------------------------
    #     Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
    #     and total CH2O cost per g of veg tissue (AGRVG2)
    #-----------------------------------------------------------------------
    AGRVG  <- AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM + AGRSTR * FRSTR
    AGRVG2 <- AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI + FRSTR * PROSRI) * RPROAV
    #-----------------------------------------------------------------------
    #    Compute N Demand for New Tissue, including reproductive and vegetative
    #-----------------------------------------------------------------------
    NDMVEG <- (CDMVEG/AGRVG2) * (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
    NDMNEW <- NDMREP + NDMVEG
    #-----------------------------------------------------------------------
    #    Check to See if Any C is Left After Reproductive Growth for
    #    Reducing N to Re-Fill Old Tissue, if N Can Be Taken up by Roots
    #-----------------------------------------------------------------------
    CNOLD  <- max(0.0,PGAVL-CDMREP)
    NDMOLD <- 0.0
    #-----------------------------------------------------------------------
    #    Nitrogen Demand for Old Tissue
    #-----------------------------------------------------------------------
    if (DAS > NVEG0 & DAS < NR7 & CNOLD > 0.0) {
      NVSTL  <- PROLFR*0.16
      NVSTS  <- PROSTR*0.16
      NVSTR  <- PRORTR*0.16
      NVSTSR <- PROSRR*0.16
      if (DXR57 > 0.0) {
        FRNLFT <- (NRCVR + (1. - NRCVR) * (1. - DXR57^2))
        NVSTL  <- PROLFF * 0.16 + ((PROLFR - PROLFF) * 0.16) * FRNLFT
        NVSTS  <- PROSTF * 0.16 + ((PROSTR - PROSTF) * 0.16) * FRNLFT
        NVSTR  <- PRORTF * 0.16 + ((PRORTR - PRORTF) * 0.16) * FRNLFT
        NVSTSR <- PROSRF * 0.16 + ((PROSRR - PROSRF) * 0.16) * FRNLFT
      }
      # NDMOLD = (WTLF  - WCRLF) * max(0.0,(NVSTL - PCNL /100.)) + (STMWT - WCRST) * max(0.0,(NVSTS - PCNST/100.)) + (RTWT  - WCRRT) * max(0.0,(NVSTR - PCNRT/100.))
      #-----------------------------------------------------------------------
      #    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
      #                        to natural senescence
      #-----------------------------------------------------------------------
      
      NDMOLD <-   max(0.0,(WTLF - SLDOT - WCRLF) * PROLFR * 0.16 - WTNLF) 
      + max(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16 - WTNST)
      + max(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 - WTNRT)
      + max(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16 - WTNSR)
      
      if (NDMOLD > (CNOLD/RNO3C*0.16)) {
        NDMOLD <- CNOLD/RNO3C*0.16
      }
    }
    
    #-----------------------------------------------------------------------
    #      KJB/SJR New code to minimize/fix situation of overspending PGAVL on
    #      N demand.  Original code uses all PGAVL for CDMREP and CDMVEG 
    #      then goes ahead and calculates NDMOLD using CDMVEG again.
    #      Get to N uptake and can potentially take up more N than have CHO
    #      to reduce.  Contributes to NLEAK.
    #-----------------------------------------------------------------------
    if (NDMOLD > 0.0) {
      
      CHOPRO <- NDMOLD * 6.25 * RNO3C
      
      FROLDA <- 1 - exp(-KCOLD * (CDMVEG / CHOPRO))
      
      CDMOLD <- CHOPRO * FROLDA
      
      NDMOLD <- FROLDA * NDMOLD
      
      CDMVEG <- CDMVEG - CDMOLD
      
      NDMVEG <- (CDMVEG/AGRVG2) * (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
      
      NDMNEW <- NDMREP + NDMVEG
      
    }
    #-----------------------------------------------------------------------
    #      KJB/SJR End new code.  Now CDMTOT=PGAVL, not more.
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #    Total N Demand
    #-----------------------------------------------------------------------
    NDMTOT <- NDMREP + NDMVEG + NDMOLD
    #-----------------------------------------------------------------------
    #    Compute Total Demand for C, and Max. C that Could be Mined
    #     CDMTOT not used - chp
    #-----------------------------------------------------------------------
    CDMTOT <- CDMREP + CDMVEG + NDMOLD*RNO3C/0.16 
    GDMSD  <-  GDMSDO
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
  
  assign("AGRSD2", AGRSD2, envir = env)
  assign("AGRVG", AGRVG, envir = env)
  assign("AGRVG2", AGRVG2, envir = env)
  assign("CDMREP", CDMREP, envir = env)
  assign("Fnew", Fnew, envir = env)
  assign("FNINL", FNINL, envir = env)
  assign("FNINR", FNINR, envir = env)
  assign("FNINS", FNINS, envir = env)
  assign("FNINSD", FNINSD, envir = env)
  assign("FRLF", FRLF, envir = env)
  assign("FRRT", FRRT, envir = env)
  assign("FRSTM", FRSTM, envir = env)
  assign("GDMSD", GDMSD, envir = env)
  assign("GRRAT1", GRRAT1, envir = env)
  assign("NDMNEW", NDMNEW, envir = env)
  assign("NDMOLD", NDMOLD, envir = env)
  assign("NDMREP", NDMREP, envir = env)
  assign("NDMSDR", NDMSDR, envir = env)
  assign("NDMTOT", NDMTOT, envir = env)
  assign("NDMVEG", NDMVEG, envir = env)
  assign("NMINEP", NMINEP, envir = env)
  assign("NMOBR", NMOBR, envir = env)
  assign("PHTIM", PHTIM, envir = env)
  assign("PNTIM", PNTIM, envir = env)
  assign("POTLIP", POTLIP, envir = env)
  assign("SDGR", SDGR, envir = env)
  assign("TURADD", TURADD, envir = env)
  assign("XFRT", XFRT, envir = env)
  # TODO: Verificar assign REDSHL e TMPFAC, CDMSD, SLAMN
  assign("REDSHL", REDSHL, envir = env)
  assign("TMPFAC", TMPFAC, envir = env)
  assign("CDMSD", CDMSD, envir = env)
  assign("SLAMN", SLAMN, envir = env)
  assign("DUMFAC", DUMFAC, envir = env)
  assign("FVEG", FVEG, envir = env)
  assign("SLAMX", SLAMX, envir = env)
  assign("GROMAX", GROMAX, envir = env)
  assign("SIZRAT", SIZRAT, envir = env)
  assign("YY", YY, envir = env)
  assign("XX", XX, envir = env)
  assign("FRLFM", FRLFM , envir = env)
  assign("FRSTMM", FRSTMM, envir = env)
  
  assign("ADDSHL", ADDSHL, envir = env)
  assign("TURXFR", TURXFR, envir = env)
  assign("NDMSD",  NDMSD, envir = env)
  assign("GDMSDR", GDMSDR, envir = env)
  assign("CDMSDR", CDMSDR, envir = env)
  assign("NAGE",   NAGE, envir = env)
  assign("NDMSH",NDMSH, envir = env)
  assign("SDDES",SDDES, envir = env)
  assign("YVGROW",YVGROW, envir = env)
  
  assign("CUMNSF",CUMNSF  , envir = env)
  assign("AGRSTR",AGRSTR  , envir = env)
  assign("FNINSR",FNINSR  , envir = env)
  assign("FRSTR",FRSTR    , envir = env)
  assign("WLIDOT",WLIDOT  , envir = env)
  assign("FRSTRM",FRSTRM  , envir = env)
  assign("PCNSR",PCNSR    , envir = env)
  assign("PPTFAC",PPTFAC  , envir = env)
  assign("STRWT",STRWT    , envir = env)
  assign("WCRSR",WCRSR    , envir = env)
  assign("WNRSR",WNRSR    , envir = env)
  assign("XSTR",XSTR      , envir = env)
  assign("SSRDO",SSRDO    , envir = env)
  assign("LFSCMOB",LFSCMOB, envir = env)
  assign("LFSNMOB",LFSNMOB, envir = env)
  assign("RTSCMOB",RTSCMOB, envir = env)
  assign("RTSNMOB",RTSNMOB, envir = env)
  assign("SRSCMOB",SRSCMOB, envir = env)
  assign("SRSNMOB",SRSNMOB, envir = env)
  assign("STSCMOB",STSCMOB, envir = env)
  assign("STSNMOB",STSNMOB, envir = env)
  assign("WTNSR",WTNSR    , envir = env)
  return()
}

SDCOMP <- function (TAVG) {
  params <- plantList$forage$params
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- params$SDLIP   #0.200 #Fraction oil in seeds (g(oil)/g(seed)) 
  SDPRO <- params$SDPRO   #0.400 #Fraction protein in seeds (g(protein)/g(seed)) 
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*SEED  COMPOSITION VALUES 
  CARMIN <- params$CARMIN  #0.180
  LIPOPT <- params$LIPOPT  #23.65 
  LIPTB  <- params$LIPTB   #7.16
  SLOSUM <- params$SLOSUM  #9.08000022E-03
  #!*PLANT COMPOSITION VALUES
  PLIGSD <- params$PLIGSD  #0.020 
  PMINSD <- params$PMINSD  #0.025
  POASD  <- params$POASD   #0.040
  #!*RESPIRATION PARAMETERS
  RCH2O  <- params$RCH20   #1.242
  RLIG   <- params$RLIG    #2.174
  RLIP   <- params$RLIP    #3.106
  RMIN   <- params$RMIN    #0.050
  RNO3C  <- params$RNO3C   #2.556
  ROA    <- params$ROA     #0.929
  RPRO   <- params$RPRO    #0.360
  
  #***********************************************************************
  #***********************************************************************
  #     The quadratic plateau for predicting percentage lipids
  #-----------------------------------------------------------------------
  if (TAVG >= LIPOPT) {
    LIPTEM <- 1.0
  } else if ((TAVG < LIPOPT) & (TAVG > LIPTB)) {
    LIPTEM <- 1.0 - ((LIPOPT - TAVG) / (LIPOPT - LIPTB))^2
  } else {
    LIPTEM <- 0.0
  }
  POTLIP <- SDLIP * LIPTEM
  
  
  #-----------------------------------------------------------------------
  #     Determination of protein percentage
  #-----------------------------------------------------------------------
  GENSUM <- (SDPRO*100.0) + (SDLIP*100.0) * (1.0 - (((LIPOPT - 25.) / (LIPOPT - LIPTB))^2))
  SUMTEM <- 1.0 + SLOSUM * (TAVG - 25.0)
  PSUMSD <- GENSUM * SUMTEM / 100.0
  POTPRO <- PSUMSD - POTLIP
  
  #-----------------------------------------------------------------------
  #     Determination of carbohydrate percentage
  #-----------------------------------------------------------------------
  POTCAR <- 1.0 - POTLIP - POTPRO
  if (POTCAR < CARMIN) {
    POTCAR <-  CARMIN
  }
  
  TOTAL  <- POTLIP + POTPRO + POTCAR
  #      IF (TOTAL .NE. 1.0) THEN
  if (abs(TOTAL) - 1.0 > 0.0005) {
    POTPRO <- POTPRO / TOTAL
    POTLIP <- POTLIP / TOTAL
    POTCAR <- POTCAR / TOTAL
    #        Note:  POTCAR will fall below CARMIN again, if adusted.
    #        Should only POTPRO and POTLIP be adusted here? -chp
    #        Check logic - GH
    #        Check PODDETACH - GH
    TOTAL  <- POTLIP + POTPRO + POTCAR
  }
  
  POTCAR <- POTCAR - PMINSD - POASD - PLIGSD
  AGRSD1 <- PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA + POTLIP*RLIP + POTCAR*RCH2O
  AGRSD2 <- AGRSD1 + RNO3C*POTPRO
  FNINSD <- POTPRO / 6.25
  
  assign("AGRSD1", AGRSD1, envir = env)
  assign("AGRSD2", AGRSD2, envir = env)
  assign("FNINSD", FNINSD, envir = env)
  assign("POTCAR", POTCAR, envir = env)
  assign("POTLIP", POTLIP, envir = env)
  
  return()
}
#--------------END DEMAND FUNCTION---------------

#----------------PODS FUNCTION-------------------
PODS <- function(DYNAMIC, DAS, NAVL, ISWWAT, iyear, jday, PGAVL) {
  
  environment(PODCOMP) <- env
  params <- plantList$forage$params
  
  TS <- 24
  
  NLAYR <- nsoilay
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  # XFRT    <- 1.000 # Maximum fraction of daily growth that is partitioned to seed + shell
  SDPDVR  <- params$SDPDV  #2.05 # ***SDPDV no .CUL*** Average seed per pod under standard growing conditions (#/pod)
  PODUR   <- params$PODUR   #10.0  # Time required for cultivar to reach final pod load under optimal conditions (photothermal days)
  THRESH  <- params$THRSH  #78    # Threshing percentage. The maximum ratio of (seed/(seed+shell)) at maturity. Causes seeds to stop growing as their dry weight
  WTPSD   <- params$WTPSD   #0.19  # Maximum weight per seed (g)
  SFDUR   <- params$SFDUR   #21.0  # Seed filling duration for pod cohort at standard growth conditions (photothermal days)
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  LNGSH <- params$LNGSH  #10.0 #Time required for growth of individual shells (photothermal days)
  
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*SEED AND SHELL GROWTH PARAMETERS
  DSWBAR  <- params$DSWBAR  #15.0
  SETMAX  <- params$SETMAX  #0.60
  RFLWAB  <- params$RFLWAB  #0.0
  XMPAGE  <- params$XMPAGE  #100.0
  #TODO verificar esses vetores
  FNPDT   <- params$FNPDT    #c(14.0,21.0,26.5,40.0) # + QDR no .SPE
  TYPPDT  <- params$TYPPDT   #"QDR"
  XSWBAR  <- params$XSWBAR   #c(0.00,  0.01,  0.25,  1.00,  1.00) 
  YSWBAR  <- params$YSWBAR   #c(1.00,  1.00,  1.00,  1.00,  1.00) 
  XSWFAC  <- params$XSWFAC   #c(0.00,  0.50,  1.00,  1.00) 
  YSWFAC  <- params$YSWFAC   #c(0.00,  1.00,  1.00,  1.00)
  #!*PLANT COMPOSITION VALUES
  PROSHI  <- params$PROSHI  #0.250
  PROLFF  <- params$PROLFF  #0.112
  PROSHF  <- params$PROSHF  #0.050
  
  
  #TODO ver como está sendo usado no ECOSMOS, Santiago
  YRDOY  <- paste0(iyear,sprintf("%03d", jday))
  YRPLT  <- YRDOY #TODO ver como está sendo usado no ECOSMOS
  
  
  # TODO: Verificar Stress
  # NSTRES  <- 1 # N stress factor (1=no stress, 0=max stress) [verificar de onde vem no ECOSMOS se formos usar]
  # SWFAC   <- 1 # water stress factor (verificar de onde vem no ECOSMOS)
  
  NCOHORTS <- 300 #from line 51 in ModuleDefs.for NCOHORTS = 300, !Maximum number of cohorts
  
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    PODCOMP(DYNAMIC, NAVL)
    
    #-----------------------------------------------------------------------
    #     Set minimum days for phenological events under optimum conditions
    #     (temperature and short photoperiod)
    #-----------------------------------------------------------------------
    #     Number of days from end pod set to physiological maturity
    MNESPM <- PHTHRS[10] - PHTHRS[9]
    
    #-----------------------------------------------------------------------
    #     Number of days between start of peg (full flower) and shell
    #     formation
    #     Only used in peanut to define slow growth period.
    LNGPEG <- PHTHRS[7] - PHTHRS[6]
    
    #-----------------------------------------------------------------------
    #     Number of days between start of shell and seed formation of a pod
    LAGSD  <- PHTHRS[8] - PHTHRS[6]
    
    #-----------------------------------------------------------------------
    #     Compute reproductive rates from cultivar and ecotype coefficients
    #-----------------------------------------------------------------------
    SDVAR <- WTPSD / SFDUR
    SHVAR <- WTPSD * SDPDVR * ((100.-THRESH)/THRESH)/((LNGSH-.85*LNGPEG)*((1.-PROSHI)/(1.-PROSHF)))
    
    
    FNINSH <- 0.0   
    NAVPOD <- 0.0
    NGRSD  <- 0.0   
    NGRSH  <- 0.0   
    NR2TIM <- 0
    PCTMAT <- 0.0   
    PGNPOD <- 0.0
    PODNO  <- 0.0   
    WSDDTN <- 0.0   
    WSHDTN <- 0.0   
    WTABRT <- 0.0   
    WTSHM  <- 0.0   
    WTSHMT <- 0.0   
    WTSD   <- rep(0, 300)
    WTSHE  <- rep(0, 300)
    
    
    # TODO: Verificar atribuições de vetores!!!!!
    RPRPUN <- 1.0 
    PGAVLR <- 0.0
    SDNO   <- rep(0, 300)
    AGRSD3 <- AGRSD1
    SHELN  <- rep(0, 300)
    FLWN   <- rep(0, 300)
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    ACCAGE  <- 0.0
    AFLW    <- 0.0
    CNSTRES <- 1.0
    
    FNINSH  <- PROSHI * 0.16         
    FLWRDY  <- 0.0
    PCTMAT  <- 0.0
    PODADD  <- 0.0
    SHMINE  <- 0.0
    TEMPOD  <- 0.0
    TRIGGR  <- 0
    WTSHM   <- 0.0
    
    for (NPP in 1:NCOHORTS) {
      SHELN[NPP] <- 0.0
      WTSHE[NPP] <- 0.0
      WTSD[NPP] <- 0.0
      SDNO[NPP] <- 0.0
      FLWN[NPP] <- 0.0
      SUPDE[NPP] <- 0.0
      AVTEM[NPP] <- 0.0
    }
    
    PODCOMP(DYNAMIC, NAVL)
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    #     Daily Initialization.
    #-----------------------------------------------------------------------
    PODMAT <- 0.0
    WSDDTN <- 0.0
    WSHDTN <- 0.0
    NGRSD  <- 0.0
    NGRSH  <- 0.0
    NLEFT  <- 0.0
    PGLEFT <- 0.0
    
    # DAS   = max(0,TIMDIF(YRSIM,YRDOY))    # TODO: verificar se vai utilizar essa!!! Leandro 26/10/2020
    
    
    #***********************************************************************
    #     Seed growth section
    #-----------------------------------------------------------------------
    if (YRNR1 != -99){
      yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
      yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
      
      yrnr1 <- as.character(paste0(substr(YRNR1,1,4),'-01-01'))
      yrnr1 <- as.Date(yrnr1)+as.numeric(substr(YRNR1,5,7))-1
      # if (YRDOY >= YRNR1 & YRNR1 > 0) {
      if (yrdoy >= yrnr1 & yrnr1 > 0) {
        
        # TODO: adaptação para funcao timdif 
        
        NR1TIM <- max(as.numeric(yrdoy-yrnr1),0)
        
        # NR1TIM <- max(TIMDIF(YRNR1,YRDOY),0) #TODO tradução timdif 
        #-----------------------------------------------------------------------
        PGAVLR <- PGAVL * XFRT
        assign("PGAVLR",PGAVLR, envir = env)
        
        #-----------------------------------------------------------------------
        #     Nitrogen stress; 8-Day moving average.
        #     Slow response to change in N stress.
        #-----------------------------------------------------------------------
        CNSTRES <- 0.875 * CNSTRES + 0.125 * NSTRES
        #-----------------------------------------------------------------------
        #     Calculate insect feeding and puncture damage
        #-----------------------------------------------------------------------
        REDPUN <- 1.0
        if (PUNCSD > 0.0) {
          REDPUN <- REDPUN - (PUNCTR/PUNCSD) * RPRPUN
          REDPUN <- max(0.0,REDPUN)
        } else {
          REDPUN <- 1.0
        }
        
        if (YRNR2 != -99){
          # TODO: adaptação para funcao timdif 
          yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
          yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
          
          yrnr2 <- as.character(paste0(substr(YRNR2,1,4),'-01-01'))
          yrnr2 <- as.Date(yrnr2)+as.numeric(substr(YRNR2,5,7))-1
          # if (YRDOY > YRNR2 & YRNR2 > 0) {
          if (yrdoy > yrnr2 & yrnr2 > 0) {
            
            NR2TIM <- max(as.numeric(yrdoy-yrnr2),0)
            
            # NR2TIM <- max(TIMDIF(YRNR2,YRDOY),0) #TODO tradução timedif 
            #-----------------------------------------------------------------------
            #     Remember yesterdays mature shell weight, WTSHMY
            #-----------------------------------------------------------------------
            WTSHMY <- WTSHM
            WTSHM <- 0.0
            for (NPP in 1:NR2TIM) { 
              if (NPP > NCOHORTS) {
                # TODO: Escrever mensagem de aviso
              }
              #-----------------------------------------------------------------------
              #     Compute physiological age of cohort
              #-----------------------------------------------------------------------
              PAGE <- PHTIM[NR2TIM + 1] - PHTIM[NPP]
              #-----------------------------------------------------------------------
              #     Prevent seeds from growing until they are older than LAGSD p-t-d
              #-----------------------------------------------------------------------
              #           if (PAGE < LAGSD) GO TO 800
              if (PAGE >= LAGSD) {
                #-----------------------------------------------------------------------
                #     Prevent cohort from exceeding threshing limit, considering
                #     damaged seed (SDDES) that were damaged without shell loss ***wdb??
                #-----------------------------------------------------------------------
                if (SDDES[NPP] > 0.0) {
                  REDSHL <- WTSHE[NPP]*SDDES[NPP]/(SDDES[NPP]+SDNO[NPP])
                } else {
                  REDSHL <- 0.
                }
                SDMAX <- (WTSHE[NPP]-REDSHL)*THRESH/(100.-THRESH)-WTSD[NPP]
                SDMAX <- max(0.0,SDMAX)
                #-----------------------------------------------------------------------
                #     Compute shell wt of cohorts that are full
                #-----------------------------------------------------------------------
                if (SDMAX <= 0.0) WTSHM <- WTSHM + WTSHE[NPP]
              }
            }
            #-----------------------------------------------------------------------
            #     Compute cohorts of shell wt. that reach THRESH today
            #-----------------------------------------------------------------------
            WTSHMT <- WTSHM - WTSHMY
            
            #-----------------------------------------------------------------------
            #     Modification of seed composition
            #-----------------------------------------------------------------------
            #       This section of code provides an alternative to calling PODCOMP
            #       routine.  Currently, both are done.
            #-----------------------------------------------------------------------
            RSD <- 1.0
            if (GDMSD > 0.0001) {
              CRSD <- min(PGAVLR / (GDMSD*AGRSD1), 1.0)
              NREQ <- FNINSD * min(PGAVLR/AGRSD1, GDMSD)
              if (NREQ > 0.0) {
                NRSD <- NAVL / NREQ
              } else {
                NRSD <- 1.0
              }
              RSD <- min(CRSD, NRSD, 1.0)
            }
            
            AGRSD3 <- AGRSD1
            ANINSD <- FNINSD
            
            #-----------------------------------------------------------------------
            #     Detailed seed composition calculations
            #-----------------------------------------------------------------------
            
            PODCOMP(DYNAMIC, NAVL)
            
            #-----------------------------------------------------------------------
            #     Grow seed cohorts
            #-----------------------------------------------------------------------
            for (NPP in 1:NR2TIM) { 
              PAGE <- PHTIM[NR2TIM + 1] - PHTIM[NPP]
              #           if (PAGE < LAGSD) GO TO 1300
              if (PAGE >= LAGSD) {
                if (SDDES[NPP] > 0.0) {
                  REDSHL <- WTSHE[NPP]*SDDES[NPP]/(SDDES[NPP]+SDNO[NPP])
                } else {
                  REDSHL <- 0.
                }
                SDMAX <- (WTSHE[NPP]-REDSHL)*THRESH/(100.-THRESH)-WTSD[NPP]
                SDMAX <- max(0.0,SDMAX) * (1. + TURADD)
                WTSD[NPP] <- WTSD[NPP]+RSD*min(SDGR*SDNO[NPP]*REDPUN,SDMAX)
                #-----------------------------------------------------------------------
                #     New Seed Tissue Growth, for updating crop seed mass
                #         in GROW, N Required
                #-----------------------------------------------------------------------
                WSDDTN <- WSDDTN + RSD * min(SDGR*SDNO[NPP]*REDPUN,SDMAX)
                NGRSD <- NGRSD+ANINSD*RSD*min(SDGR*SDNO[NPP]*REDPUN,SDMAX)
              }
            }
            #-----------------------------------------------------------------------
          }
        }#End of YRDOY>YRNR2 Seed growth section
        
        #***********************************************************************
        #     Shell section
        #-----------------------------------------------------------------------
        PGLEFT <- max(0.0,(PGAVLR - WSDDTN*AGRSD3))
        NLEFT  <- max(0.0,(NAVL - NGRSD))
        PGNPOD <- PGLEFT
        NAVPOD <- NLEFT
        #-----------------------------------------------------------------------
        #     Calculate function for modifying pod setting with temperature
        #-----------------------------------------------------------------------
        TEMPOD <- 0.
        for (I in 1:24) {
          # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
          # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
          
          TEMPOD <- TEMPOD + CURV(TYPPDT,FNPDT[1],FNPDT[2],FNPDT[3],FNPDT[4],TGRO[I])
        }
        
        # ALTERADO: REAL(TS) é conversão para tipo REAL, não é necessário aqui.
        TEMPOD <- TEMPOD / 24.
        
        #-----------------------------------------------------------------------
        #     Avg soil water (SWBAR) over DSWBAR depth to affect flower,
        #         pod addition
        #-----------------------------------------------------------------------
        if (ISWWAT == 'Y') {
          ACTSW <- 0.0
          POTSW <- 0.0
          DSW <- 0.0
          for (I in 1:NLAYR) {
            DSW <- DSW + DLAYR[I]
            FLAYR <- 1.0
            if (DSW > DSWBAR) {
              FLAYR <- (DSWBAR-(DSW-DLAYR[I]))/DLAYR[I]
            }
            ACTSW <- ACTSW + (SW[I] - LL[I]) * DLAYR[I] * FLAYR
            POTSW <- POTSW + (DUL[I] - LL[I]) * DLAYR[I] * FLAYR
            
            # ALTERADO: GOTO era a forma do fortran parar o laço de repetição, no R usa-se break
            if ( FLAYR < 1.0 ) break() # GO TO 401 #TODO
            
          }
          
          SWBAR <- ACTSW / POTSW
          SWBAR <- min(SWBAR,1.0)
          SWBAR <- max(SWBAR,0.0)
          
        } else {
          SWBAR <- 1.0
        }
        #-----------------------------------------------------------------------
        #     Soil water factor (SWADD1), and Water stress factor (SWADD2)
        #-----------------------------------------------------------------------
        SWADD1 <- TABEX (YSWBAR,XSWBAR,SWBAR,5)
        SWADD2 <- TABEX (YSWFAC,XSWFAC,SWFAC,4)
        #-----------------------------------------------------------------------
        SHMAXG <- SHVAR
        #-----------------------------------------------------------------------
        #     This section calculates shell growth after first pod (NR2)
        #-----------------------------------------------------------------------
        if (YRNR2 != -99){
          # TODO: adaptação para funcao timdif 
          yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
          yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
          
          yrnr2 <- as.character(paste0(substr(YRNR2,1,4),'-01-01'))
          yrnr2 <- as.Date(yrnr2)+as.numeric(substr(YRNR2,5,7))-1
          
          if (yrdoy > yrnr2 & yrnr2 > 0) {
            for (NPP in 1:NR2TIM) { 
              NAGE <- NR2TIM + 1 - NPP
              PAGE <- PHTIM[NR2TIM + 1] - PHTIM[NPP]
              ADDSHL <- 0.0
              SUPDAY <- 1.0
              if (PAGE <= LNGSH) {
                if (SHELN[NPP] >= 0.001 & GRRAT1 >= 0.001) {
                  if (PAGE >= LNGPEG) {
                    ADDSHL <- min(PGLEFT/AGRSH1,GRRAT1 * SHELN[NPP], NLEFT/(FNINSH*CNSTRES^0.5))
                    SUPDAY <- min((PGLEFT/AGRSH1)/(GRRAT1*SHELN[NPP]), (NLEFT/(FNINSH*CNSTRES^0.5))/(GRRAT1 * SHELN[NPP]), SWADD1)
                    if (SUPDAY >= 1.0) SUPDAY = 1.0
                  } else {
                    if (SHLAG < 0.001) SHLAG = 0.001
                    ADDSHL <- min(PGLEFT/AGRSH1 ,GRRAT1*SHELN[NPP]*SHLAG, NLEFT/(FNINSH*CNSTRES^0.5))
                    SUPDAY <- min((PGLEFT/AGRSH1)/(GRRAT1*SHELN[NPP]*SHLAG),
                                  (NLEFT/(FNINSH*CNSTRES^0.5))/(GRRAT1*SHELN[NPP]*SHLAG), SWADD1)
                    if (SUPDAY >= 1.0) SUPDAY = 1.0
                  }
                  #-----------------------------------------------------------------------
                  #     Compute running avg ratio supply to demand for shell grwoth
                  #-----------------------------------------------------------------------
                }
                if (NAGE <= 1) {
                  SUPDE[NPP] <- SUPDAY
                  AVTEM[NPP] <- TEMPOD
                } else {
                  SUPDE[NPP] <- (SUPDE[NPP] * (NAGE-1) + SUPDAY)/NAGE
                  AVTEM[NPP] <- (AVTEM[NPP] * (NAGE-1) + TEMPOD)/NAGE
                }
                #-----------------------------------------------------------------------
                #     Compute overall growth of all shells, total N required
                #     and the remaining C (PGLEFT) and N (LEFT)
                #-----------------------------------------------------------------------
                WSHDTN <- WSHDTN + ADDSHL
                NGRSH <- NGRSH + ADDSHL * PROSHI * 0.16 * CNSTRES^0.5
                if (PGLEFT < 1.0E-6) PGLEFT=0.0          #NBP
                if (ADDSHL < 1.0E-6) ADDSHL=0.0          #NBP
                PGLEFT <- max(0.0,(PGLEFT - ADDSHL * AGRSH1))
                NLEFT  <- max(0.0,(NLEFT - ADDSHL * (FNINSH*CNSTRES^0.5)))
              }
              #-----------------------------------------------------------------------
              #     Grow shells if greater than 1 day old
              #-----------------------------------------------------------------------
              SHMINE <- 0.0
              if (SDDES[NPP] > 0.0) {
                REDSHL <- WTSHE[NPP]*SDDES[NPP]/(SDDES[NPP]+SDNO[NPP])
              } else {
                REDSHL <- 0.
              }
              SDMAXX <- (WTSHE[NPP]-REDSHL) * THRESH/(100. - THRESH)
              if (SHELWT-WTSHM > 0.0 & SDMAXX >= WTSD[NPP]) {
                SHMINE <- NRUSSH/0.16 * WTSHE[NPP]/(SHELWT - WTSHM)
              }
              WTSHE[NPP] <- WTSHE[NPP] + ADDSHL - max(SHMINE,0.0)
            }
            #-----------------------------------------------------------------------
            #     Set seeds based on ratio of supply to demand for shells,
            #     average temperature and night length effect
            #     between (LAGSD) and (LAGSD+TDUMX) p-t-d age
            #-----------------------------------------------------------------------
            WTABRT <- 0.0
            for (NPP in 1:NR2TIM) { 
              PAGE <- PHTIM[NR2TIM + 1] - PHTIM[NPP]
              if (PAGE >= LAGSD & PAGE < LAGSD + TDUMX & SDNO[NPP] <= 0.0) {
                #-----------------------------------------------------------------------
                #     Physiol age to set seeds
                #-----------------------------------------------------------------------
                if (SUPDE[NPP] >= SETMAX) {
                  SHRAT <- 1.0
                } else {
                  SHRAT <- SUPDE[NPP]/SETMAX
                }
                SDNO[NPP] <- min(SHRAT, AVTEM[NPP]*(DRPP^1.0)) * SHELN[NPP]* SDPDVR + SDNO[NPP]
                #-----------------------------------------------------------------------
                #     Abort shells that do not form seed; abort (1-SHRAT) fraction
                #-----------------------------------------------------------------------
                WTABR <- 0.0
                START <- SHELN[NPP]
                SHELN[NPP] <- SHELN[NPP]*min(SHRAT, AVTEM[NPP]*(DRPP^1.0))
                if (START > 0.) {
                  WTABR <- (START-SHELN[NPP])*WTSHE[NPP]/START
                }
                WTSHE[NPP] <- WTSHE[NPP] - WTABR
                WTABRT <- WTABRT + WTABR
              }
            }
            #-----------------------------------------------------------------------
          }  
        }#End of DAS>NR2 Shell growth section
        #***********************************************************************
        #     Add new pods and flowers
        #     RFLWAB is relative rate of flower abortion per day because
        #     daylength is not optimum.  The flowers that survive "NR2"
        #     PHOTOTHERMAL days is equal to FLWRDY which can limit pod addition
        #-----------------------------------------------------------------------
        AFLW <- RFLWAB * (1.0 - TDUMX) * (1.0 - SWADD2)
        FLWRDY <- 0.
        #NR1TIM >= NPP, pois indice nao pode ser menor que 1 FLWN[NPP]
        if (NR1TIM >= 1){
          for (NPP in 1:NR1TIM) { 
            if (FLWN[NPP] > 0.0001) {
              PNAGE <- PNTIM[NR1TIM + 1] - PNTIM[NPP]
              FLWN[NPP] <- FLWN[NPP] * (1.0 - AFLW)
              if (PNAGE >= PHTHRS[6]) {
                #-----------------------------------------------------------------------
                #     Allow flowers in each cohort to make pods over 2-3 days
                #-----------------------------------------------------------------------
                FLWFRC <- 0.
                if (TDUMX > 0.0001) FLWFRC <- (PNAGE-PHTHRS[6])/TDUMX
                FLWFRC <- min(FLWFRC,1.0)
                FLWFRC <- max(FLWFRC,0.0)
                FLWRDY <- FLWFRC*FLWN[NPP] + FLWRDY
                FLWN[NPP] <- (1.0-FLWFRC)*FLWN[NPP]
              }
            }
          }
        }
        
        PMAX <- PGAVLR/(SDVAR*AGRSD1*SDPDVR)*(1./PODUR)
        
        if (YRNR2 != -99){
          # TODO: adaptação para funcao timdif 
          yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
          yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
          
          yrnr2 <- as.character(paste0(substr(YRNR2,1,4),'-01-01'))
          yrnr2 <- as.Date(yrnr2)+as.numeric(substr(YRNR2,5,7))-1
          
          if (yrdoy >= yrnr2 & yrnr2 > 0) {
            FLADD <- FLWRDY * TEMPOD *(DRPP^1.3)* min(SWADD1,SWADD2) *XFRT
            if (DAS > NDSET & MNESPM > 0.) {
              ACCAGE <- ACCAGE + TEMPOD * DRPP * SWFAC / MNESPM
              ACCAGE <- min(1.0,ACCAGE)
            }
            #-----------------------------------------------------------------------
            #    Reduce pod addition from END POD SET to physiological maturity
            #    DRPP**1.3 makes smaller and more sentitive to long days
            #    Scale pod addition to RNITP, leaf N for photo purporses
            #-----------------------------------------------------------------------
            RNITPD <- (RNITP*0.01-PROLFF*0.16)/(FNINL-PROLFF*0.16)
            RNITPD <- min(1.1,RNITPD)
            RNITPD <- max(0.1,RNITPD)
            PODADD <- PMAX * TEMPOD * (DRPP^1.3) * min(SWADD1,SWADD2,RNITPD) * max((1.0 - ACCAGE),0.0)
            #    &       * max((1.0 - ACCAGE),0.0) * (1.0 + TURADD)
            SHELN[NR2TIM + 1] = min(PODADD, PGNPOD/(SHMAXG*AGRSH1), FLADD, NAVPOD/(SHMAXG*(FNINSH*CNSTRES^0.5)))
            #-----------------------------------------------------------------------
            #    KJB ADDED 1/27/96.  2 CONDITIONS: NDSET AND TRIGGER (CUMSIG >.98)
            #    MUST BE MET TO STOP POD ADDITION.  THUS, IF WE ARE THRU THE WINDOW
            #    AND FULL LOAD IS SET, { NO LATE PODS AND FLOWERS CAN BE ADDED
            #    PRESENTLY NDSET WAS PLACED TO R7 IN SOYBEAN.  MOVE IT EARLIER
            #    SO WE CAN PREVENT FUNNY LATE BUMPS (WMS-88, 84RF) OCCURRING AFTER
            #    FULL LOAD IS APPARENTLY SET, BUT DROUGHT IS RELEASED.
            #-----------------------------------------------------------------------
            if (TRIGGR == 0 & CUMSIG < 0.98) {
              TRIGGR <- 1
            }
            
            if (DAS >= NDSET & TRIGGR == 1) {
              SHELN[NR2TIM + 1] <- 0.0
            }
            #-----------------------------------------------------------------------
          }  
        }#End of DAS>NR2 Pod and flower growth section
        #-----------------------------------------------------------------------
        FLWADD <- 2. * PMAX * TEMPOD * (DRPP^1.3) * min(SWADD1,SWADD2,CNSTRES^0.5)
        #    &     min(SWADD1,SWADD2,CNSTRES**0.5) * (1.0 + TURADD)
        FLWN[NR1TIM + 1] <- min(FLWADD,PGNPOD/(SHMAXG*0.1*AGRSH1), NAVPOD/(SHMAXG*0.1*(FNINSH*CNSTRES^0.5)))
        if (DAS >= NDSET & TRIGGR == 1) {
          FLWN[NR1TIM + 1] <- 0.
        }
        #-----------------------------------------------------------------------
        #     Calculate number of pods, including those with and without seeds
        #-----------------------------------------------------------------------
        SEEDNO <- 0.0
        PODNO <- 0.0
        
        #-----------------------------------------------------------------------
        if (YRNR2 != -99){
          # TODO: adaptação para funcao timdif 
          yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
          yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
          
          yrnr2 <- as.character(paste0(substr(YRNR2,1,4),'-01-01'))
          yrnr2 <- as.Date(yrnr2)+as.numeric(substr(YRNR2,5,7))-1
          
          if (yrdoy >= yrnr2 & yrnr2 > 0) {
            
            #TODO VERIFICAR: aparentemente usado apenas para "snap bean"
            #CALL FreshWt(INTEGR, ISWFWT, NR2TIM, PHTIM, SDNO, SHELN, WTSD, WTSHE, YRPLT)
            
            for (NPP in 1:(NR2TIM + 1)) { 
              #-----------------------------------------------------------------------
              PAGE <- PHTIM[NR2TIM + 1] - PHTIM[NPP]
              #-----------------------------------------------------------------------
              if (PAGE > LNGPEG) {
                PODNO <- PODNO + SHELN[NPP]
                SEEDNO <- SEEDNO + SDNO[NPP]
                if (PAGE >= LAGSD) {
                  if (SDDES[NPP] > 0.0) {
                    REDSHL <- WTSHE[NPP] * SDDES[NPP] / (SDDES[NPP] + SDNO[NPP])
                  } else {
                    REDSHL <- 0.
                  }
                  SDMAXX <- (WTSHE[NPP]-REDSHL) * THRESH/(100. - THRESH)
                  if ((WTSD[NPP] >= 0.95 * SDMAXX) & (WTSHE[NPP] > 0.001) | (PAGE > XMPAGE)) {
                    PODMAT <- PODMAT + SHELN[NPP]
                  }
                }
              }
            }
          }
        }
        
        #-----------------------------------------------------------------------
        if (PODMAT > 0. & PODNO > 0.) {
          PCTMAT <- PODMAT*100./PODNO
        }
        
        #-----------------------------------------------------------------------
      } 
    }#End of section for YRDOY>YRNR1
    #-----------------------------------------------------------------------
    #    Leave PODS with : NGRSD  : New N Used for New Seed Growth
    #                      NGRSH  : New N Used for New Shell Growth
    #                      WSDDTN : New Seed Growth, g tissue/m2 d
    #                      WSHDTN : New Shell Growth, g tissue/m2 d
    #                      New pods, seeds, flowers, mature pods, % mature pods
    
    #***********************************************************************
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  
  assign("AGRSD3", AGRSD3, envir = env)
  assign("LAGSD", LAGSD, envir = env)
  assign("LNGPEG", LNGPEG, envir = env)
  assign("NGRSD", NGRSD, envir = env)
  assign("NGRSH", NGRSH, envir = env)
  assign("PCTMAT", PCTMAT, envir = env)
  assign("PODNO", PODNO, envir = env)
  assign("SDNO", SDNO, envir = env)
  assign("POTCAR", POTCAR, envir = env)
  assign("POTLIP", POTLIP, envir = env)
  
  assign("SDVAR", SDVAR, envir = env)
  assign("SEEDNO", SEEDNO, envir = env)
  assign("SHELN", SHELN, envir = env)
  assign("SHVAR", SHVAR, envir = env)
  assign("WSDDTN", WSDDTN, envir = env)
  assign("WSHDTN", WSHDTN, envir = env)
  assign("WTABRT", WTABRT, envir = env)
  assign("WTSD", WTSD, envir = env)
  assign("WTSHE", WTSHE, envir = env)
  assign("WTSHMT", WTSHMT, envir = env)
  assign("FLWN", FLWN, envir = env)
  assign("FNINSH",FNINSH, envir = env)
  assign("REDSHL",REDSHL, envir = env)
  # Verificar assign PGAVLR,
  assign("ACCAGE",ACCAGE  , envir = env)
  assign("AFLW",AFLW    , envir = env)
  assign("CNSTRES",CNSTRES , envir = env)
  assign("FNINSH",FNINSH  , envir = env)
  assign("FLWRDY",FLWRDY  , envir = env)
  assign("PODADD",PODADD  , envir = env)
  assign("SHMINE",SHMINE  , envir = env)
  assign("TEMPOD",TEMPOD  , envir = env)
  assign("TRIGGR",TRIGGR  , envir = env)
  assign("WTSHM",WTSHM   , envir = env)
  assign("NAVPOD",NAVPOD, envir = env)
  assign("NR2TIM",NR2TIM, envir = env)
  assign("PGNPOD",PGNPOD, envir = env)
  assign("RPRPUN",RPRPUN, envir = env)
  assign("SUPDE",SUPDE, envir = env)
  assign("AVTEM",AVTEM, envir = env)
  
  assign("SDDES",SDDES, envir = env)
  
  assign("ANINSD",ANINSD, envir = env)
  
  assign("MNESPM",MNESPM, envir = env)
  assign("LNGPEG",LNGPEG, envir = env)
  assign("LAGSD",LAGSD, envir = env)
  assign("SDVAR",SDVAR, envir = env)
  assign("SHVAR",SHVAR, envir = env)
  
  return() #PODS
}

PODCOMP <- function(DYNAMIC, NAVL) {
  params <- plantList$forage$params
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PLIGSD <- params$PLIGSD  #0.020 
  PMINSD <- params$PMINSD  #0.025
  POASD  <- params$POASD   #0.040
  PROMAX <- params$PROMAX  #0.080
  PROMIN <- params$PROMIN  #0.030
  THETA  <- params$THETA   #0.800
  #!*RESPIRATION PARAMETERS
  RCH2O  <- params$RCH20 #1.242
  RLIG   <- params$RLIG  #2.174
  RLIP   <- params$RLIP  #3.106
  RMIN   <- params$RMIN  #0.050
  ROA    <- params$ROA   #0.929
  
  
  #***********************************************************************
  #***********************************************************************
  #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
  #         or transplanting of plants
  #***********************************************************************
  if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    #     Initialize plant variables at emergence
    #-----------------------------------------------------------------------
    CUMSIG <- 1.0      
    RATION <- 1.0      
    RATIOC <- 1.0      
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    #     Daily initialize, each should change before seed cohorts section
    #-----------------------------------------------------------------------
    PNINSD <- FNINSD
    ANINSD <- FNINSD
    DTLIP <- 0.0
    DTCAR <- 0.0
    AGRSD3 <- AGRSD1
    RSD <- 1.0
    CRSD <- 1.0
    NRSD <- 1.0
    XRSD <- 1.0
    
    if (PGAVLR <= 0.00001) {
      RSD <- 0.0
    } else if (GDMSD <= 0.0001) {
      RSD <- 1.0
      
    } else {
      CRSD2 <- PGAVLR/(GDMSD*AGRSD1)
      CRSD  <- min(CRSD2,1.0)
      NREQ <- FNINSD*(min(PGAVLR/AGRSD1,GDMSD))
      NRSD <- NAVL/NREQ
      #-----------------------------------------------------------------------
      #     Full seed load defined for either of all N or all C
      #     being used for seed growth.
      #-----------------------------------------------------------------------
      CUMSIG <- 0.8 * CUMSIG + 0.2 * CRSD
      #-----------------------------------------------------------------------
      #     5-day moving average.  Does it work ?
      #
      #     Computing the possible seed N conc given the "total" NAVL
      #     relative to PG available or seed growth demand
      #-----------------------------------------------------------------------
      PNINSD <- NAVL/(min(PGAVLR/AGRSD1,GDMSD))
      #-----------------------------------------------------------------------
      #     Set ANINSD equal to FNINSD to allow nitrogen to go to vegetative
      #     parts when carbon is not limiting.  Note:  CRSD and NRSD are above
      #     1 and upto 30 during the time seed setting occurs.  This is why we
      #     can not let ANINSD = PNINSD or let RSD = CRSD during this phase.
      
      #-----------------------------------------------------------------------
      #-----------------------------------------------------------------------
      #     CASE 4: NRSD <= 1.0 - N supply limiting seed growth
      # -----------------------------------------------------------------------
      if (NRSD <= 1.0) {
        #-----------------------------------------------------------------------
        #   NOTE THAT WHEN NRSD < 1.0, { PNINSD < FNINSD.  IN THIS CASE,
        #   THE N SUPPLY IS INSUFFICIENT TO GROW SEED AT FNINSD.  N IS LIMITED
        #   RELATIVE TO C SUPPLY AND RELATIVE TO SEED DEMAND.  ALLOW SEED N CONC
        #   TO DECLINE AND RECOMPUTE RSD & AGRSD3.  THERE IS NO CASE OF EXCESS C
        #   SUPPLY HERE BECAUSE N STRESS BY THE DEFINED CURVE LIMITS SINGLE SEED
        #   GROWTH RATE.  N STRESS INCREASINGLY LIMITS SEED GROWTH RATE AS PNINSD
        #   DECLINES.  AS A RESULT OF "LIMITING" SEED GROWTH RATE, THE SEED N CONC
        #   IS  HELD UP A BIT MORE BECAUSE LESS C IS USED.  SO ANINSD WILL BE
        #   A BIT HIGHER THAN PNINSD HERE.  AGRSD3 AND RSD SHOULD BE RECOMPUTED.
        #
        #   MORE NOTES:  NOTE THAT NRSD AND PNINSD ALREADY CONSIDER CARBON SUPPLY
        #   (PGAVLR) IN THEIR EQUATIONS.  THUS, IF C SUPPLY IS HIGH, { PNINSD
        #   WILL BE LOW AND IT WILL BE CONSIDERED HERE.  IF C SUPPLY IS LOW, THE
        #   PNINSD WILL BE HIGH (ABOVE FNINSD) AND NRSD > 1, AND IT WILL BE
        #   CONSIDERED IN THE NEXT LOOP.
        #
        #        COMPUTE XRSD WITH RECT HYP EQ, WITH INIT SLOPE = 1/PROMIN
        #        MAX = 1.0 AT FNINSD, AND THE Y IS 0 TO 1.0 AND X IS
        #        INCOMING PNINSD FROM ZERO TO FNINSD.
        #        THIS "RSD" DESCRIBES N LIMIT ON SEED GROWTH RATE
        #        { COMPUTE "NEW" "ANINSD" BASED ON "N-LIMITED' SEED
        #        GROWTH RATE.  USE THAT AS THE ANINSD, { CHECKS FOR
        #        CARBOHYDRATE REQUIRED BASED ON NAVL AND PGAVLR
        #
        #-----------------------------------------------------------------------
        #          SCALAR = (FNINSD/PROMIN + 1.0 - ((FNINSD/PROMIN + 1.0)**2
        #     &          - 4*THETA*FNINSD/PROMIN*1.0)**0.5)/(2*THETA)
        #          XRSD = ((PNINSD/PROMIN + 1.0 - ((PNINSD/PROMIN + 1.0)**2
        #     &          - 4*THETA*PNINSD/PROMIN*1.0)**0.5)/(2*THETA))/SCALAR
        
        XRSD <- ((PNINSD/PROMIN + 1.0 - ((PNINSD/PROMIN + 1.0)^2 - 4*THETA*PNINSD/PROMIN*1.0)^0.5)/(2*THETA)) / ((FNINSD/PROMIN + 1.0 - ((FNINSD/PROMIN + 1.0)^2 - 4*THETA*FNINSD/PROMIN*1.0)^0.5)/(2*THETA))
        
        ANINSD <- NAVL/(XRSD*min(PGAVLR/AGRSD1,GDMSD))
        #-----------------------------------------------------------------------
        #  CHECK BECAUSE N CONC CAN GO ABOVE FNINSD IF PNINSD NEAR FNINSD
        #-----------------------------------------------------------------------
        ANINSD <- min(ANINSD,FNINSD)
        
        #-----------------------------------------------------------------------
        #-----------------------------------------------------------------------
        #     CASE 3: CRSD2 <= 1.0 - C supply is limiting seed growth.
        #-----------------------------------------------------------------------
      } else {
        #      HERE NRSD > 1.0 AND PNINSD > FININSD, SO ALLOW INCREASING
        #      SEED N CONC.  CONSIDER TWO OPTIONS, C DEFICIENT OR THAT C
        #      SUPPLY IS EXCESS.  A CRSD <= 1.0 IS THE SAME AS CRSD2 <= 1.0
        #      FIRST OPTION IS IF C SUPPLY IS DEFICIENT, BUT N SUPPLY/DEMAND
        #      RATIO GREATER THAN 1.
        if (CRSD2 <= 1.0) {
          #-----------------------------------------------------------------------
          #      DOES POSSIBLE N CONCENTRATION EXCEED MAX?
          #-----------------------------------------------------------------------
          #           if (PNINSD>PROMAX) {
          #             ANINSD = PROMAX
          #           } else {
          #-----------------------------------------------------------------------
          #     UNDER THIS CONDITION, CRSD < 1.0 AND NRSD > 1.0 AND PNINSD <PROMAX
          #     SO HERE WE LET SEED N CONC INCREASE, AND BE EQUAL TO PNINSD.
          #-----------------------------------------------------------------------
          #               ANINSD = PNINSD
          #            }
          ANINSD <- min(PNINSD, PROMAX)
          
          #-----------------------------------------------------------------------
          #      IT IS NOT POSSIBLE FOR PNINSD < FNINSD AND YET NRSD > 1.0
          #      IT IS NOT POSSIBLE FOR PNINSD > FNINSD AND YET NRSD < 1.0 EITHER
          #      SO NRSD CAN ONLY BE < 1.0 IF NAVL < NREQ, AND THAT IS ONLY
          #      IF PNINSD < FNINSD.  THIS WAS COVERED BEFORE
          
          #-----------------------------------------------------------------------
          #-----------------------------------------------------------------------
          #     CASE 2: Full seed load and neither N or C supply is limiting 
          #         seed growth
          #-----------------------------------------------------------------------
        } else if (CRSD2 > 1.0 & CUMSIG < 0.98) {
          #-----------------------------------------------------------------------
          #      UNDER THIS CONDITION, N IS EXCESS RELATIVE TO C SUPPLY OR SD
          #      DEMAND (NRSD > 1) AND C SUPPLY EXCEEDS SEED DEMAND (CRSD2 > 1).
          #      ALSO, RESTRICTED THIS ONE TO OCCUR ONLY AFTER A FULL SEED LOAD
          #      OCCURS.  THE "CUMSIG" MUST BE WORKED TO DROPPED BELOW 1.0 ONLY
          #      WHEN SEVERAL DAYS OF "FULL SEED" LOAD OCCURS.  RESULTS FROM
          #      SEVERAL DAYS OF CRSD OR CRSD2 < 1,  WE NEED THIS TO AVOID
          #      PROBLEMS DURING EARLY PODSET WHEN CRSD IS VERY LARGE AND NRSD
          #      ALSO LARGE.
          #-----------------------------------------------------------------------
          #      0.20 FRACTION OF EXCESS ASSIMILATE IS ALLOWED TO "PUSH" SINGLE
          #      SEED GROWTH RATE (I.E., VIA INCREASED RSD).  THIS WILL DECREASE
          #      AMOUNT OF STEM AND LEAF GROWTH DURING SEED FILL.
          #      LIMIT THE ACTUAL N CONC TO PROMAX OR THE NAVL/(GDMSD*RSD).  THIS
          #      ONE WILL NEED TO BE CHECKED CAREFULLY TO PREVENT REALLY HIGH OR
          #      LOW N.  WE INTEND FOR IT TO ALLOW N CONC LOWER THAN FNINSD, BUT
          #      WILL DO AN XRSD LIMIT ON SEED GROWTH RATE IF ANINSD < FNINSD
          #
          #      BECAUSE WE USE ONLY 20% OF EXCESS ASSIMILATE, I DON'T THINK WE
          #      NEED TO RE-COMPUTE RSD AFTER COMPUTING COMPOSITIONS AND COSTS.
          #
          #      GIVE UP ON "BOOSTING" SEED GROWTH RATE, TRY TO INFLUENCE
          #      COMPOSITION AS A RATIO OF N SUPPLY TO C SUPPLY, NORMALIZED
          #      TO FNINSD.  RECIPROCAL OF RATION IS RATIOC.  ASSUME SLOPE OF
          #      ONE-THIRD OF POSSIBLE CHANGE FROM FNINSD TO PROMAX OR PROMIN.
          #       9/25/95 KJB
          #-----------------------------------------------------------------------
          ANINSD <- FNINSD
          if (NAVL > 0.0) {
            RATION <- (NAVL/(PGAVLR/AGRSD1))/FNINSD
            RATIOC <- 1.0 / RATION
            if (RATION >= 1.0) {
              ANINSD <- min(FNINSD * (1.0 + (RATION - 1.0)/3.), PROMAX)
            } else {
              # VERIFICAR: Por que está com esse comentário no meio da expressão?
              ANINSD <- max(FNINSD * (1.0-(RATIOC-1.0)/3.), PROMIN)
            }
          }
          
          #-----------------------------------------------------------------------
          #-----------------------------------------------------------------------
          #     CASE 1: Do not have full seed load and carbon supply is not 
          #         limiiting sedd growth.  Do not change composition.
          #-----------------------------------------------------------------------
          
        } else if (CRSD2 > 1.0 & CUMSIG >= 0.98) {
          #-----------------------------------------------------------------------
          #  EVEN IF CRSD2 > 1 AND NRSD > 1, WE HAVE NOT REACHED A SEED LOAD
          #  SO HOLD CONCENTRATIONS AND COSTS UNCHANGED.
          #-----------------------------------------------------------------------
          ANINSD <- FNINSD
        }
      }
      
      #-----------------------------------------------------------------------
      #-----------------------------------------------------------------------
      #  For cases 2, 3 and 4, adjust lipids and carbohydrate concentrations
      #-----------------------------------------------------------------------
      if (NRSD <= 1.0 | CRSD2 <= 1.0 | CUMSIG < 0.98) {
        #-----------------------------------------------------------------------
        #     ADJUSTING SO LIPID AND CARBOHYDRATE TAKE UP DIFFERENCE
        #     MOST DATA SUGGEST THAT LIPID CHANGES 0.33 PER 1 PERCENT CHANGE
        #     IN PROTEIN.
        #-----------------------------------------------------------------------
        DTLIP <- 0.33*(FNINSD - ANINSD)*6.25
        DTCAR <- (FNINSD - ANINSD)*6.25  - DTLIP
        POTLIP <- POTLIP + DTLIP
        POTCAR <- POTCAR + DTCAR
        #     POTPRO = ANINSD*6.25
        TOTAL  <- POTLIP + ANINSD*6.25 + POTCAR + PMINSD + POASD + PLIGSD
        #             TOTAL not used - chp
        AGRSD3 <- PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA + POTLIP*RLIP + POTCAR*RCH2O
        #-----------------------------------------------------------------------
        #     THIS IS CORRECT, ABOVE BASED ON N LIMIT, NEXT ON C LIMIT
        #     CONSIDERING ANY SHIFT IN PROTEIN CONC.
        #-----------------------------------------------------------------------
        DMSDN <- NAVL / ANINSD
        DMSDC <- PGAVLR / AGRSD3
        RSD <- min(min(DMSDN,DMSDC)/GDMSD,1.0)
        RSD <- max(0.0,RSD)
      }
      #-----------------------------------------------------------------------
      #-----------------------------------------------------------------------
    }
    
  }
  
  assign("AGRSD3",AGRSD3, envir = env)
  assign("ANINSD",ANINSD, envir = env)
  assign("CUMSIG",CUMSIG, envir = env)
  assign("RSD",   RSD   , envir = env)
  assign("POTCAR",POTCAR, envir = env)
  assign("POTLIP",POTLIP, envir = env)
  
  return()
}
#--------------END PODS FUNCTION---------------

#---------------VEGGR FUNCTION-----------------
VEGGR <- function(DYNAMIC, DAS, iyear, jday, CMINEP, CSAVEV, NAVL, PAR, PG, PGAVL) {                 
  
  environment(CANOPY) <- env
  environment(NLKDIST) <- env
  params <- plantList$forage$params
  
  TS <- 24
  
  #-----------------------------------------------------------------------
  
  YRDOY   <- paste0(iyear,sprintf("%03d", jday))
  
  #TODO: AVISO verificar com Santiago padrão ECOSMOS -> YREMRG, NR1
  
  # TODO: ROWSPC - precisa dividir por 100?
  ROWSPC <- config$plant1$rowSpacing / 100 #0.50 #TODO: ARQUIVO DE MANEJO ROWSPC -> Row spacing (m)
  
  
  CADSRF  <- params$CADSRF
  CMOBSRN <- params$CMOBSRN
  CMOBSRX <- params$CMOBSRX
  PROSRG  <- params$PROSRG
  PROSRI  <- params$PROSRI
  RPRO    <- params$RPRO
  PROLFF  <- params$PROLFF
  PRORTF  <- params$PRORTF
  PROSRF  <- params$PROSRF
  PROSTF  <- params$PROSTF
  MXWST   <- params$MXWST
  PWLF    <- params$PWLF
  PWST    <- params$PWST
  PWRT    <- params$PWRT
  PWSR    <- params$PWSR
  PROLFR  <- params$PROLFR
  PROSTR  <- params$PROSTR
  PRORTR  <- params$PRORTR
  PROSRR  <- params$PROSRR
  RNH4C   <- params$RNH4C
  RNO3C   <- params$RNO3C
  PCHOLFF <- params$PCHOLFF
  PCHORTF <- params$PCHORTF
  PCHOSRF <- params$PCHOSRF
  PCHOSTF <- params$PCHOSTF
  NSTFAC  <- params$NSTFAC
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CMOBMX <- params$CMOBMX  #0.024
  #!*RESPIRATION PARAMETERS
  PCH2O  <- params$PCH2O  #1.13
  #!*PLANT COMPOSITION VALUES
  PROLFI <- params$PROLFI  #0.356 
  PRORTI <- params$PRORTI  #0.092
  PROSTI <- params$PROSTI  #0.150
  PROLFG <- params$PROLFG  #0.285
  PRORTG <- params$PRORTG  #0.064
  PROSTG <- params$PROSTG  #0.100
  #!*VEGETATIVE PARTITIONING PARAMETERS
  ATOP   <- params$ATOP  #1.00
  #!*CARBON AND NITROGEN MINING PARAMETERS
  CADSTF <- params$CADSTF  #0.75
  #TODO verificar
  #*NITROGEN STRESS PARAMETERS
  # NRATIO <- 1.00
  
  # TODO: Verificar Stress
  # NSTRES  <- 1 # N stress factor (1=no stress, 0=max stress) [verificar de onde vem no ECOSMOS se formos usar]
  # SWFAC   <- 1 # water stress factor (verificar de onde vem no ECOSMOS)
  
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #    Call CANOPY for input
    #-----------------------------------------------------------------------
    # CANOPY(DYNAMIC,DAS, PAR, TGRO)
    
    CADLF   <- 0.0  
    CADST   <- 0.0  
    CMINEA  <- 0.0  
    CRUSLF  <- 0.0  
    CRUSRT  <- 0.0  
    CRUSST  <- 0.0  
    CUMTUR  <- 1.0  
    EXCESS  <- 1.0  
    FNINLG  <- 0.0  
    FNINRG  <- 0.0  
    FNINSG  <- 0.0  
    NADLF   <- 0.0  
    NADRT   <- 0.0  
    NADST   <- 0.0  
    NGRLF   <- 0.0  
    NGRRT   <- 0.0  
    NGRST   <- 0.0  
    NSTRES  <- 1.0  
    PGLEFT  <- 0.0
    SUPPN   <- 0.0
    TNLEAK  <- 0.0  
    VGRDEM  <- 0.0
    WLDOTN  <- 0.0  
    WRDOTN  <- 0.0  
    WSDOTN  <- 0.0  
    
    CADSR   <- 0.0 
    CRUSSR  <- 0.0
    FNINSRG <- 0.0
    NADSR   <- 0.0
    NGRSR   <- 0.0
    WSRDOTN <- 0.0
    TNLKCHK <- 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    FNINLG  <- PROLFG * 0.16   
    FNINRG  <- PRORTG * 0.16   
    FNINSG  <- PROSTG * 0.16  
    FNINSRG <- PROSRG * 0.16
    CUMTUR  <- 1.0             
    
    CANOPY(DYNAMIC, DAS, PAR, TGRO)
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    # DAS   = max(0,TIMDIF(YRSIM,YRDOY))  # TODO: Nao poderia usar DAS que vem por parametro VERIFICAR!! Leandro 22/10/2020
    #-----------------------------------------------------------------------
    #     Partitioning is modified by water stress and nitrogen stress
    #-----------------------------------------------------------------------
    SUPPN  <- NFIXN + TRNU + NMINEA
    #    chp added check for YRDOY = YREMRG, but on the next day, it still
    #     shows N stress because there is little supply.  Force a lag time?
    NSTFAC <- min(NSTFAC,1.0) 
    NSTFAC <- max(NSTFAC,0.1)
    #      if (SUPPN < 0.70 * NDMNEW & NDMNEW > 0.) {
    
    
    # TODO: deixar NSTRES = 1, ajustar SUPPN
    if (SUPPN < NSTFAC * NDMNEW & NDMNEW > 0. & YRDOY != YREMRG) {
      #        NSTRES = MIN(1.0,SUPPN/(NDMNEW * 0.70))
      #        NSTRES = MIN(1.0,SUPPN/(NDMNEW * 0.70))
      NSTRES <- min(1.0,SUPPN/(NDMNEW * NSTFAC))
    } else {
      NSTRES <- 1.0
    }
    
    
    FRRT <- ATOP * (1.0 - ( min(TURFAC,NSTRES))) * (1.0 - FRRT) + FRRT
    #-----------------------------------------------------------------------
    #     Cumulative turgor factor that remembers veg drought stress
    #     to shift partitioning between leaf and stem toward leaf,
    #     especially after drought is released.
    #     Sort of 20-day rolling average
    #-----------------------------------------------------------------------
    CUMTUR <- 0.95*CUMTUR + 0.05*TURFAC
    #-----------------------------------------------------------------------
    #     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
    #     0.7 appears to be too much for peanut, but not for soybean.
    #-----------------------------------------------------------------------
    FRSTR <- (FRSTR / (FRLF + FRSTM + FRSTR)) * (1 - FRRT)
    
    FRLF  <- (1.0 + 0.6 * (1.0 - CUMTUR)) * (1. - FRRT - FRSTR) * FRLF / (FRLF + FRSTM)
    FRLF  <- min(FRLF, 0.90 * (1. - FRRT - FRSTR))
    
    FRSTM <- 1.0 - FRRT - FRLF - FRSTR
    #-----------------------------------------------------------------------
    #     To prevent negative partitioning to root and limit leaf plus
    #     stem to a maximum of 98 % of the vegetative partitioning
    #-----------------------------------------------------------------------
    FRLF  <- min(FRLF,FRLF * 0.98 / (max(0.001,FRLF + FRSTM + FRSTR)))
    FRSTM <- min(FRSTM,FRSTM * 0.98 / (max(0.001,FRLF + FRSTM + FRSTR)))
    FRSTR <- min(FRSTR,FRSTR * 0.98 / (max(0.001,FRLF + FRSTM + FRSTR)))
    FRRT  <- 1.0 - FRLF - FRSTM - FRSTR
    #-----------------------------------------------------------------------
    #     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
    #-----------------------------------------------------------------------
    AGRVG <- AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM + AGRSTR * FRSTR
    #-----------------------------------------------------------------------
    #     Calculate New Growth Rate of Leaves, Stems, and Roots
    #-----------------------------------------------------------------------
    VGRDEM  <- PGAVL / AGRVG
    WLDOTN  <- FRLF * VGRDEM
    WSDOTN  <- FRSTM * VGRDEM
    WRDOTN  <- FRRT * VGRDEM
    WSRDOTN <- FRSTR * VGRDEM
    #-----------------------------------------------------------------------
    #     Compute maximum N required for tissue growth
    #-----------------------------------------------------------------------
    NGRLF  <- WLDOTN * FNINL
    NGRST  <- WSDOTN * FNINS
    NGRRT  <- WRDOTN * FNINR
    NGRSR  <- WSRDOTN * FNINSR
    NGRVEG <- NGRLF + NGRST + NGRRT + NGRSR
    #-----------------------------------------------------------------------
    #     Compute minimum N required for tissue growth
    #-----------------------------------------------------------------------
    NGRLFG <- WLDOTN * FNINLG
    NGRSTG <- WSDOTN * FNINSG
    NGRRTG <- WRDOTN * FNINRG
    NGRSRG <- WSRDOTN * FNINSRG
    NGRVGG <- NGRLFG + NGRSTG + NGRRTG + NGRSRG
    
    NRATIO <- 1.0
    if (NAVL < NGRVGG) {
      #-----------------------------------------------------------------------
      #     Compute ratio for reducing leaf growth to prevent N conc of
      #       new tissue from being below the minimum for growth
      #-----------------------------------------------------------------------
      if (NGRVGG > 0.0) {
        NRATIO  <- NAVL / NGRVGG
        WLDOTN  <- WLDOTN * NRATIO
        WSDOTN  <- WSDOTN * NRATIO
        WRDOTN  <- WRDOTN * NRATIO
        WSRDOTN <- WSRDOTN * NRATIO
        NGRLF   <- NGRLFG * NRATIO
        NGRST   <- NGRSTG * NRATIO
        NGRRT   <- NGRRTG * NRATIO
        NGRSR   <- NGRSRG * NRATIO
        
        #-----------------------------------------------------------------------
        #     Adjust conversion costs to account for composition of tissue at
        #       lower N concentration
        #-----------------------------------------------------------------------
        AGRVG <- AGRLF * FRLF * (1.0 - (PROLFG - PROLFI)/(1.0 - PROLFI) )+ AGRRT * FRRT * (1.0 - (PRORTG - PRORTI)/(1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI)/ (1.0 - PROSTI)) + AGRSTR * FRSTR * (1.0 - (PROSRG - PROSRI) /(1.0 - PROSRI))
        
        #       IF (AGRVG * (WLDOTN + WSDOTN + WRDOTN + WSRDOTN) .GT. PGAVL) THEN
        #                              VGRDEM = PGAVL / AGRVG
        #            WLDOTN = FRLF * VGRDEM
        #            WSDOTN = FRSTM * VGRDEM
        #            WRDOTN = FRRT * VGRDEM
        #            WSRDOTN = FRSTR * VGRDEM
        #       ENDIF
      }
    } else {
      #-----------------------------------------------------------------------
      #     NAVL IS between lower and maximum N limit in this case,
      #       leaf expansion occurs as normal, but N concentration is reduced
      #-----------------------------------------------------------------------
      if (NGRVEG > 0.0 & NAVL < NGRVEG) {
        NGRLF <- min(NAVL * NGRLF / NGRVEG, NGRLF)
        NGRST <- min(NAVL * NGRST / NGRVEG, NGRST)
        NGRRT <- min(NAVL * NGRRT / NGRVEG, NGRRT)
        NGRSR <- min(NAVL * NGRSR / NGRVEG, NGRSR)
      }
      #-----------------------------------------------------------------------
      #     Compute protein fraction of new vegetative tissue growth
      #-----------------------------------------------------------------------
      if (WLDOTN > 0.0) {
        PROLFT <- NGRLF * (100./16.)/WLDOTN
      } else {
        PROLFT <- 0.0
      }
      if (WSDOTN > 0.0) {
        PROSTT <- NGRST * (100./16.)/WSDOTN
      } else {
        PROSTT <- 0.0
      }
      if (WRDOTN > 0.0) {
        PRORTT <- NGRRT * (100./16.)/WRDOTN
      } else {
        PRORTT <- 0.0
      }
      if (WSRDOTN > 0.0) {
        PROSRT <- NGRSR * (100./16.)/WSRDOTN
      } else {
        PROSRT <- 0.0
      }
      #-----------------------------------------------------------------------
      #     Recompute respiration costs if expansion occurs at low N-conc.,
      #       allow N dilution during growth of leaves, stems, and roots
      #-----------------------------------------------------------------------
      AGRVG <- AGRLF * FRLF * (1.0 - (PROLFT - PROLFI)/ (1.0-PROLFI)) + AGRRT * FRRT * (1.0 - (PRORTT - PRORTI)/ (1.0 - PRORTI)) + AGRSTM * FRSTM * (1.0 - (PROSTT - PROSTI)/(1.0 - PROSTI)) + AGRSTR * FRSTR * (1.0 - (PROSRT - PROSRI)/(1.0-PROSRI))
      
      #            VGRDEM = PGAVL / AGRVG
      #            WLDOTN = FRLF * VGRDEM
      #            WSDOTN = FRSTM * VGRDEM
      #            WRDOTN = FRRT * VGRDEM
      #            WSRDOTN = FRSTR * VGRDEM
      
    }
    #-----------------------------------------------------------------------
    #     Compute C and N remaining to add to reserves
    #-----------------------------------------------------------------------
    PGLEFT <- max(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN) * AGRVG))
    #-----------------------------------------------------------------------
    #     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
    #     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
    #     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
    #     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
    #     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
    #     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
    #     value 1.04 -0.04 also can not be greater than 1.0 or we get
    #     stimulation of photosynthesis and the sq root works differently.
    #-----------------------------------------------------------------------
    if (PG > 0.0001 & PGLEFT > 0.00001) {
      EXCESS <-  (1.20 - min(1.0, max(PGLEFT/PG,0.20)) )^0.5
    } else {
      EXCESS <- 1.00
    }
    
    ACMINELF <- 0.0
    ACMINERT <- 0.0
    ACMINESH <- 0.0
    ACMINESR <- 0.0
    ACMINEST <- 0.0
    
    
    CADST  <- 0.0
    CADLF  <- 0.0
    CADSR  <- 0.0
    CADRT  <- 0.0
    CADSH  <- 0.0
    CMINEA <- 0.0
    CRUSLF <- 0.0
    CRUSST <- 0.0
    CRUSRT <- 0.0
    CRUSSH <- 0.0
    CRUSSR <- 0.0
    #-----------------------------------------------------------------------
    #      SJR 10/16/03
    #      Set all NAD variables to 0.0
    #      NADSH never assigned a value other than 0.0 - not used anywhere
    #-----------------------------------------------------------------------
    
    #      NADRAT = 0.0
    #      NADST  = 0.0
    #      NADLF  = 0.0
    #      NADRT  = 0.0
    #      NADSR  = 0.0
    NADSH  <- 0.0
    
    #-----------------------------------------------------------------------
    #      SJR 10/20/03
    #      Set all NLEAK fix variables to 0.0
    #-----------------------------------------------------------------------
    NRFRESP <- 0.0
    NLKGROW <- 0.0
    NLKCOST <- 0.0
    
    #-----------------------------------------------------------------------
    #    Calculate Increase in Remobilizable C due to N shortage and
    #      add to Carbon Pool.  Distribute to Leaves and Stems.
    #-----------------------------------------------------------------------
    #    Want half as much accumulation in stem in veg phase
    #-----------------------------------------------------------------------
    # if (DAS < NR1) {
    #   LSTR = (1.-0.6*CADSTF)/(0.6*CADSTF)
    # } else {
    #   LSTR = (1.-CADSTF)/CADSTF
    # }
    #-----------------------------------------------------------------------
    #    5/11/04  KJB/SJR Add code to allocate excess CH2O to Stolon as well
    #      as to leaf and stem.  For forages chose to ignore different 
    #      partitioning for vegetative vs. reproductive stages.
    #-----------------------------------------------------------------------
    #      How CADSTF and CADSRF work to patition CAD between STOR, Stems and Leaves.
    #      CADSRF/(1-CADSRF) X as much CH2O will be allocated to each g of STOR 
    #      as to each g of Tops.
    #      Of the CH2O added to Tops, CADSTF/(1-CADSTF) as much CH2O will be  
    #      allocated to each g of stems as to each g of  leaves.
    #-----------------------------------------------------------------------
    #    02/02/06 SJR  Adjust CH2O refill partitioning for today's DM lost 
    #                        to natural senescence.
    #      Complicates the equation considerably with minimal impact on results.
    #      Only added for "correctness"
    #-----------------------------------------------------------------------
    LSTSR <- CADSRF/(1-CADSRF)
    LSTR  <- (1.-CADSTF)/CADSTF
    
    
    if (STMWT+WTLF > 0.0) {
      #      5/16/05 SJR added WTLF to calculation of LSTSR to be consistent 
      #            with the definition - ratio of CH2O to be added to stem 
      #            relative to that added to leaf + stem
      #         LSTSR = LSTSR*STRWT/(STRWT*LSTSR+STMWT)
      #         LSTSR = LSTSR * (STRWT - SSRMDOT) / ((STRWT-SSRMDOT) *
      #     &              LSTSR + STMWT - SSMDOT + WTLF - SLMDOT)
      #         LSTR = LSTR * (WTLF - SLMDOT) / 
      #     &              (STMWT - SSMDOT + (WTLF - SLMDOT) * LSTR)
      
      LSTSR <- LSTSR * (STRWT - SSRDOT ) / (LSTSR * (STRWT-SSRDOT) + (STMWT - SSDOT ) + (WTLF - SLDOT ))
      
      LSTR  <- LSTR * (WTLF - SLDOT ) / (LSTR * (WTLF - SLDOT ) + (STMWT - SSDOT) )
    }
    if (PGLEFT >= CMINEP) {
      CADSR <- (PGLEFT - CMINEP) / PCH2O * LSTSR
      CADLF <- (PGLEFT - CMINEP) / PCH2O * LSTR * (1 - LSTSR)
      CADST <- (PGLEFT - CMINEP) * (1. - LSTR) * (1 - LSTSR) / PCH2O
    } else {
      
      #-----------------------------------------------------------------------
      #    Calculate actual C used (CMINEA) , compute how much is taken
      #    from LF, ST, RT, and SH, which may be less than orig calc of CMINEP
      #
      #    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
      #    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
      #    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
      #
      #-----------------------------------------------------------------------
      #      7/2/03 SJR added (PPMFAC) to limit mobilization from roots 
      #      while dormant
      #-----------------------------------------------------------------------
      #        IF (CMINEP .GT. 0) THEN
      #          CMINEA = CMINEP - PGLEFT
      #          CRUSLF = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * 
      #     &              (WCRLF - WTLF*PCHOLFF) 
      #          CRUSST = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * 
      #     &              (WCRST - STMWT * PCHOSTF) 
      #          CRUSSH = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * WCRSH 
      #          CRUSRT = CMINEA / CMINEP * CMOBMX * (DTX + DXR57) * PPMFAC *
      #     &              (WCRRT - RTWT * PCHORTF)        
      #            CRUSSR = CMINEA / CMINEP * CMOBSR * (DTX + DXR57) * 
      #     &              (WCRSR - STRWT * PCHOSRF)
      #        ENDIF
      
      
      
      #if (CMINEP > 0) {
      
      CMINEA <- CMINEP - PGLEFT
      #        CMINEA = MAX(TSCMOB,CMINEP - PGLEFT)
      if (CMINEA > CMINEP) CMINEA <- CMINEP
      
      #-----------------------------------------------------------------------
      #      In this case, the remaining TSNMOB will stay inthe WTNxx pools
      #-----------------------------------------------------------------------
      if (CMINEA < TSCMOB) {
        LFSCMOB <- LFSCMOB * (CMINEA / TSCMOB)
        STSCMOB <- STSCMOB * (CMINEA / TSCMOB)
        RTSCMOB <- RTSCMOB * (CMINEA / TSCMOB)
        SRSCMOB <- SRSCMOB * (CMINEA / TSCMOB)
      } else {
        #-----------------------------------------------------------------------
        #      Otherwise all TSNMOB plus some portion of mobilized N will be used
        #-----------------------------------------------------------------------
        #            IF (CMINEP .GT.TSCMOB) THEN
        CMINER   <- (CMINEA - TSCMOB) / (CMINEP - TSCMOB)
        ACMINESH <- SHCMINE * CMINER
        ACMINELF <- (LFCMINE - LFSCMOB) * CMINER
        ACMINEST <- (STCMINE - STSCMOB) * CMINER
        ACMINERT <- (RTCMINE - RTSCMOB) * CMINER
        ACMINESR <- (SRCMINE - SRSCMOB) * CMINER
      }
      CRUSLF <- LFSCMOB + ACMINELF
      CRUSST <- STSCMOB + ACMINEST
      CRUSRT <- RTSCMOB + ACMINERT
      CRUSSR <- SRSCMOB + ACMINESR
      CRUSSH <- ACMINESH
    }
    #-----------------------------------------------------------------------
    #      "Original" Forage model modification to code for adding CSAVEV
    #      back to leaf, stem and STOR
    #-----------------------------------------------------------------------
    #        CADSR = CADSR + CSAVEV/PCH2O * LSTSR
    #      CADLF = CADLF + CSAVEV/PCH2O * LSTR*(1-LSTSR)
    #      CADST = CADST + CSAVEV * (1. - LSTR)*(1-LSTSR)/PCH2O
    #-----------------------------------------------------------------------
    #      5/31/05 SJR apportion CSAVEV to organs in proportion to their 
    #      level of CH2O "deficit" or debt.  When deficit is really 
    #      a surplus, dump all CSAVEV to STOR
    #-----------------------------------------------------------------------
    #      10/06/05 SJR Move calculation of xxDEBT to CH2OREF 
    #-----------------------------------------------------------------------
    #      10/31/05 SJR Created separate variable CADVG for CH2O reserved for
    #            refilling vegetative tissues prior to NR1.  Previously CSAVEV 
    #            had been used to carry both Pre-NR1 and Post-NR1 reserves.
    #            This change heps delineat the difference in purposes for these
    #            two periods.
    #            When this was done, the CSAVEV code was returned to the 
    #            original code from DSSAT4 for consistenc between the model
    #            versions.
    #-----------------------------------------------------------------------
    
    
    CADLF <- CADLF + CADVG/PCH2O * LFCDEBT
    CADST <- CADST + CADVG/PCH2O * STCDEBT 
    CADRT <- CADRT + CADVG/PCH2O * RTCDEBT
    CADSR <- CADSR + CADVG/PCH2O * SRCDEBT
    
    CADSR <- CADSR + CSAVEV/PCH2O * LSTSR
    CADLF <- CADLF + CSAVEV/PCH2O * LSTR*(1-LSTSR)
    CADST <- CADST + CSAVEV * (1. - LSTR)*(1-LSTSR)/PCH2O
    
    
    #-----------------------------------------------------------------------
    #    Calculate Increase in Remobilizable N Due to a C shortage,
    #      add to Nitrogen pool
    #-----------------------------------------------------------------------
    NLEFT  <- max(0.0,NAVL  -  (NGRLF  + NGRST  + NGRRT + NGRSR))
    
    if (NLEFT > 0.0) {
      #-----------------------------------------------------------------------
      #      Calculate capacity for refilling old tissue N if PGAVL were not
      #      limiting.  This is the original calculation with no modifiers.
      #      This may get tid of some N that would otherwise be NLEAK by 
      #      using ONDMOLD instead of NDMOLD whish may have been limited 
      #      by PGAVL
      #      10/03/05 SJR Add code to stop refilling tissue that will be 
      #      senesced today.
      #-----------------------------------------------------------------------
      # 
      #       ONDMOLD = (WTLF - SLMDOT - WCRLF) * MAX(0.0,(NVSTL - PCNL /100.))
      #     &     + (STMWT - SSMDOT - WCRST) * 
      #     &              MAX(0.0,(NVSTS - PCNST/100.))
      #     &     + (RTWT  - SRMDOT - WCRRT) * 
      #     &              MAX(0.0,(NVSTR - PCNRT/100.))
      #     &         + (STRWT - SSRMDOT - WCRSR) * 
      #     &              MAX(0.0,(NVSTSR - PCNSR/100.))
      #-----------------------------------------------------------------------
      #    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
      #                        to natural senescence.
      #      Refill capacity for each organ is the max potential N content - current N content
      #      Max potential N content = current organ mass (yesterday's mass 
      #      without CH2O minus losses to natural senescence, need to add back 
      #      C&N mobilized before senescence) X initial N concentration 
      #      Current N content is WTNxx minus N lost in today's natural senescence
      #-----------------------------------------------------------------------
      
      ONDMOLD <- max(0.0,(WTLF - SLDOT - WCRLF) * PROLFR * 0.16 - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 
      + max(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16 - (WTNST - (SSDOT * PCNST /100 - STSNMOB)))
      + max(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 - (WTNRT - (SRDOT * PCNRT / 100 - RTSNMOB)))
      + max(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16 - (WTNSR -(SSRDOT * PCNSR / 100 - SRSNMOB)))
      
      #-----------------------------------------------------------------------
      #       10/03/05 SJR Limit refill to N uptake + TSBNMOB to
      #      ensure that N mobilized form non-senescing tissues is not used 
      #      to refilll old tissue
      #-----------------------------------------------------------------------
      if (ONDMOLD > TRNU + TSNMOB) ONDMOLD <- TRNU + TSNMOB
      #-----------------------------------------------------------------------
      #      If total capacity for refilling N allows, put "NLEAK" into NLEFT
      #      Otherwise, allocate NLEAK to NLEFT to the amount allowed by ONDMOLD
      #-----------------------------------------------------------------------
      if (NLEFT > ONDMOLD) {
        NLEAK <- NLEFT - ONDMOLD
        NLEFT <- ONDMOLD
      } else {
        NLEAK <- 0.0
      }
      
      NDMOLD <- NLEFT
      #-----------------------------------------------------------------------
      #      SJR 10/20/03 Added code to fix/distribute NLEAK
      #      NLEAK caused by 3 scenarios
      #        I) Rounding/precision errors.
      #       II) Changing FRRT & FRLF in VEGGR.FOR due to H2O or N stress, 
      #             changes N demand.
      #      III) Low Pg - exhaust PGAVL on N uptake.  Have N available but no
      #             CH2O left for growth so N goes to refill old tissue.  
      #             Particularly a problem when LFWT is low.  Also causes N% to 
      #             exceed initial or maximum concentrations set in species file.
      #-----------------------------------------------------------------------
      #      SJR 10/16/03 Eliminate NLEAK values due to rounding errors
      #            see I) above.
      #-----------------------------------------------------------------------
      if (NLEAK < 0.0001) NLEAK <- 0.0
      
      
      #-----------------------------------------------------------------------
      #    Original code allocated N back to only leaf, stem and root.
      #    Uses FRxx which is the fraction of today's growth going to organ xx.
      #    This is not indicative of the original "source" of the mobilized N being returned.
      #-----------------------------------------------------------------------
      
      
      #-----------------------------------------------------------------------
      #      Allocate excess N uptake to new tissues
      #-----------------------------------------------------------------------
      #      Original DSSAT4 scheme - based on proportions of new growth
      #      but came from  proportions of existing tissue
      #-----------------------------------------------------------------------
      
      #        NDMOLD = (WTLF  - WCRLF) * MAX(0.0,(NVSTL - PCNL /100.))
      #     &     + (STMWT - WCRST) * MAX(0.0,(NVSTS - PCNST/100.))
      #     &     + (RTWT  - WCRRT) * MAX(0.0,(NVSTR - PCNRT/100.))
      #     &         + (STRWT - WCRSR) * MAX(0.0,(NVSTSR - PCNSR/100.))
      
      #
      #                    NADRAT = NLEFT / (FRLF*FNINL+FRSTM*FNINS+FRRT*FNINR
      #               &                               +FRSTR*FNINSR)
      #                    NADLF  = NADRAT * FRLF * FNINL
      #                    NADST  = NADRAT * FRSTM * FNINS
      #                    NADRT  = NADRAT * FRRT * FNINR
      
      #-----------------------------------------------------------------------
      #      sjr 10/16/03 Revised scheme based on existing growth proportions
      #      10/03/05 SJR Revised equation to be consistent with earlier change 
      #      preventing filling of tissues that will be senesced today.
      #-----------------------------------------------------------------------
      #      IF (NLEFT .GT. 0.0) THEN
      #-----------------------------------------------------------------------
      #      Allocate excess N uptake to refill old tissues
      #-----------------------------------------------------------------------
      #      11/04/05 SJR added MXWST and CWST to increase allocation of N to 
      #            stems during drought, to mimic increase in NO3- concentration.
      #-----------------------------------------------------------------------
      CWST <- PWST + (1 - TURFAC) * (MXWST - PWST)
      
      #        DNADRAT = PWLF * (WTLF - SLMDOT - WCRLF) * 
      #     &              MAX(0.0,(NVSTL - PCNL /100.))
      #     &    + CWST * (STMWT - SSMDOT - WCRST) * 
      #     &              MAX(0.0,(NVSTS - PCNST/100.))
      #     &    + PWRT * (RTWT  - SRMDOT - WCRRT) * 
      #     &              MAX(0.0,(NVSTR - PCNRT/100.))
      #     &        + PWSR * (STRWT - SSRMDOT - WCRSR) * 
      #     &              MAX(0.0,(NVSTSR - PCNSR/100.))
      
      #-----------------------------------------------------------------------
      #    02/01/06 SJR  Adjust N refill capacity for today's DM and N lost 
      #                        to natural senescence
      #-----------------------------------------------------------------------
      
      DNADRAT <-   PWLF * max(0.0,(WTLF - SLDOT - WCRLF)   * PROLFR * 0.16 - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 
      + PWST * max(0.0,(STMWT - SSDOT - WCRST)  * PROSTR * 0.16 - (WTNST -(SSDOT * PCNST/100 - STSNMOB))) 
      + PWRT * max(0.0,(RTWT  - SRDOT - WCRRT)  * PRORTR * 0.16 - (WTNRT -(SRDOT * PCNRT/100 - RTSNMOB))) 
      + PWSR * max(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16 - (WTNSR -(SSRDOT * PCNSR/100 - SRSNMOB))) 
      
      
      if (DNADRAT > 0.0) {
        NADRAT <- NLEFT / DNADRAT 
      } else {
        NADRAT <- 0.0
      }
      
      #                NADLF = NADRAT * PWLF * (WTLF - SLMDOT - WCRLF) * 
      #     &              MAX(0.0,(NVSTL - PCNL /100.))
      #
      #            NADST = NADRAT * CWST * (STMWT - SSMDOT - WCRST) * 
      #     &              MAX(0.0,(NVSTS - PCNST/100.))
      #
      #            NADRT = NADRAT * PWRT * (RTWT  - SRMDOT - WCRRT) * 
      #     &              MAX(0.0,(NVSTR - PCNRT/100.))
      #
      #            NADSR = NADRAT * PWSR * (STRWT - SSRMDOT - WCRSR) * 
      #     &              MAX(0.0,(NVSTSR - PCNSR/100.))
      
      NADLF <- NADRAT * PWLF * max(0.0,(WTLF - SLDOT - WCRLF) * PROLFR * 0.16 - (WTNLF -(SLDOT * PCNL/100 - LFSNMOB))) 
      
      NADST <- NADRAT * PWST * max(0.0,(STMWT - SSDOT - WCRST) * PROSTR * 0.16 - (WTNST -(SSDOT * PCNST/100 - STSNMOB))) 
      
      NADRT <- NADRAT * PWRT * max(0.0,(RTWT  - SRDOT - WCRRT) * PRORTR * 0.16 - (WTNRT -(SRDOT * PCNRT/100 - RTSNMOB))) 
      
      NADSR <- NADRAT * PWSR * max(0.0,(STRWT - SSRDOT - WCRSR) * PROSRR * 0.16 - (WTNSR -(SSRDOT * PCNSR/100 - SRSNMOB))) 
      
      
      
    } else {
      NADRAT <- 0.0
      NADLF  <- 0.0
      NADST  <- 0.0
      NADRT  <- 0.0
      NADSR  <- 0.0
      NLEAK  <- 0.0
    }
    
    #-----------------------------------------------------------------------
    #      SJR 10/20/03 Added code to fix/distribute NLEAK
    #      Fix for II) and III) above.  
    #      Basic strategy is to estimate how much growth could have been 
    #      achieved if CH2O that wAs used for N mobilization, fixation, or uptake
    #      was used for growth instead.  Then lower NLEAK and the appropriate
    #      source by the amount of N used in and to fuel this growth.
    #
    #      Considered using PGAVL in this as well but did not because, to date,
    #      NLEAK that would be addressed by this fix only occurs when PGAVL is 
    #      exhausted.  If there were more PGAVL, more growth would have occurred
    #      and there wouldn't be any NLEAK.
    #
    #      Strategy:
    #      1) Figure out where it came from (mined, fixed or uptake)
    #      2) Use appropriate respiration coefficient to calculate how much CH2O 
    #        was used to liberate that amount of N
    #      3) Calculate how much new growth could be generated if that amount 
    #        of CH2O was used for growth and liberation of just enough
    #        N for that new growth, no excess.
    #      4) Recalculate N source (NMINEA, NFIXN, or TRNU).  Recalculate NLEAK
    #      5) In case of TRNU, distrbute N to NO3 and NH4 in each soil layer
    #      6) Recalculate growth rate and N concentration
    #-----------------------------------------------------------------------
    
    CHORECOVER <- 0.0
    NLKSPENT   <- 0.0
    NLKNUSED   <- 0.0
    NLKCHK     <- NLEAK
    TNLKCHK    <- TNLKCHK + NLEAK
    
    if (NLEAK > 0.0) {
      
      NLKDIST(CHORECOVER, NLKSPENT, NLKNUSED)
      
      if (NLEAK < 0.0001) NLEAK <- 0.0
      
      TNLEAK <- TNLEAK + NLEAK
      
    }
    
    AAA <- FRLF *  FNINL / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
    BBB <- FRSTM * FNINS / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
    CCC <- FRRT *  FNINR / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
    DDD <- FRSTR * FNINSR / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR + FRSTR * FNINSR)
    ZZZ <- AAA + BBB + CCC + DDD
    
    XXX <- PNMLF + PNMST + PNMRT + PNMSR + PNMSH
    
    
    
    
    
    #-----------------------------------------------------------------------
    #     Subroutine CANOPY calculates height and width of the canopy as a
    #     function of VSTAGE, air temperature, drought stress (TURFAC),
    #     daylenght and radiation (PAR).
    #-----------------------------------------------------------------------
    CANOPY(DYNAMIC,DAS, PAR, TGRO)
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("AGRVG", AGRVG, envir = env)
  assign("FRLF", FRLF, envir = env)
  assign("FRRT", FRRT, envir = env)
  assign("FRSTM", FRSTM, envir = env)
  assign("CADLF", CADLF, envir = env)
  assign("CADST", CADST, envir = env)
  assign("CANHT", CANHT, envir = env)
  assign("CANWH", CANWH, envir = env)
  assign("CMINEA", CMINEA, envir = env)
  assign("CRUSLF", CRUSLF, envir = env)
  assign("CRUSRT", CRUSRT, envir = env)
  assign("CRUSSH", CRUSSH, envir = env)
  assign("CRUSST", CRUSST, envir = env)
  assign("EXCESS", EXCESS, envir = env)
  assign("NADLF", NADLF, envir = env)
  assign("NADRT", NADRT, envir = env)
  assign("NADST", NADST, envir = env)
  assign("NGRLF", NGRLF, envir = env)
  assign("NGRRT", NGRRT, envir = env)
  assign("NGRST", NGRST, envir = env)
  assign("NSTRES", NSTRES, envir = env)
  assign("TNLEAK", TNLEAK, envir = env)
  assign("WLDOTN", WLDOTN, envir = env)
  assign("WRDOTN", WRDOTN, envir = env)
  assign("WSDOTN", WSDOTN, envir = env)
  
  assign("CUMTUR", CUMTUR, envir = env)
  assign("FNINLG", FNINLG, envir = env)
  assign("FNINRG", FNINRG, envir = env)
  assign("FNINSG", FNINRG, envir = env)
  assign("CADSR", CADSR, envir = env)
  assign("CRUSSR", CRUSSR, envir = env)
  assign("FNINSR", FNINSR, envir = env)
  assign("FNINSRG", FNINSRG, envir = env)
  assign("FRSTR", FRSTR, envir = env)
  assign("NADSR", NADSR, envir = env)
  assign("NGRSR", NGRSR, envir = env)
  assign("WSRDOTN", WSRDOTN, envir = env)
  assign("NADSH", NADSH, envir = env)
  assign("CADRT", CADRT, envir = env)
  assign("CADSH", CADSH, envir = env)
  assign("LFSCMOB", LFSCMOB, envir = env)
  assign("RTSCMOB", RTSCMOB, envir = env)
  assign("SRSCMOB", SRSCMOB, envir = env)
  assign("STSCMOB", STSCMOB, envir = env)
  assign("TSNMOB", TSNMOB, envir = env)
  assign("TNLKCHK", TNLKCHK, envir = env)
  
  
  
  
  
  return()
}

#-----------------------------------------------------------------------
#     Subroutine NLKDIST for distributing NLEAK
#      For each potential source of NLEAK:
#      2) Use appropriate respiration coefficient (NRSPCST)to calculate 
#        how much CH2O was used to liberate that amount of N
#      3) Calculate how much new growth could be generated if that amount 
#        of CH2O was used for growth and liberation of just enough
#        N for that new growth, no excess.
#-----------------------------------------------------------------------
# TODO: Verificar Parêtros da Função
NLKDIST <- function (CHORECOVER, NLKSPENT, NLKNUSED) {
  
  params  <- plantList$forage$params
  
  NSTFAC  <- params$NSTFAC
  PROLFI  <- params$PROLFI
  PROLFI  <- params$PROLFI
  PRORTI  <- params$PRORTI
  PROSRI  <- params$PROSRI
  PROSTI  <- params$PROSTI
  RNH4C   <- params$RNH4C
  RNO3C   <- params$RNO3C
  RPRO    <- params$RPRO
  
  
  NLEAK2     <- 0.0
  NLKNG1     <- 0.0
  NLKNG2     <- 0.0
  NLKNG3     <- 0.0
  NLKRTRN1   <- 0.0
  NLKRTRN2   <- 0.0
  NLKRTRN3   <- 0.0
  CHORECOVER <- 0.0
  NLKSPENT   <- 0.0
  NLKNUSED   <- 0.0
  TRNURTRN   <- 0.0
  
  #      Calculate CH2O cost of new growth at initial N concentrations
  PNTVG   <-(FRLF * PROLFI + FRSTM * PROSTI + FRRT * PRORTI + FRSTR * PROSRI) * 0.16
  AGRVGI  <- FRLF * AGRLF  + FRSTM * AGRSTM + FRRT * AGRRT  + FRSTR * AGRSTR
  AGRVGPI <- FRLF * PROLFI + FRSTM * PROSTI + FRRT * PRORTI + FRSTR * PROSRI
  
  
  #      Calculate CH2O cost for today's TRNU
  if (TRNU > 0.0) {
    PNUPNO3 <- TRNO3U / TRNU
    PNUPNH4 <- TRNH4U / TRNU
    RNNU <- PNUPNO3 * RNO3C + PNUPNH4 * RNH4C
  }
  
  #      Allocate NLEAK to a source.  Allocate first to N mobilization 
  #      (NLKRTRN1), next to N uptake (NLKRTRN2), and lastly, to N from 
  #      natural senescence (NLKRTRN3).
  NLKRTRN1 <- min(NLEAK,NMINEA-TSNMOB)
  NLEAK2   <- NLEAK - NLKRTRN1
  NLKRTRN2 <- min(NLEAK2,TRNU)
  NLKRTRN3 <- NLEAK2 - NLKRTRN2
  
  #      For NLKRTRN1 and NLKRTRN2, "return" part of the excess N in 
  #      exchange for "recovering" the CH2O that was spent acquiring it.  
  #      Then use this CH2O and remaining N for new growth at "initial" N 
  #      concentrations.  N from natural senescence cannot be returned as 
  #      the tissue will be lost, so N is just added to the N pool.  
  #      Need to formally add to the N pool as it will be subtracted out 
  #      later as NRUSxx.
  
  #      Calculate amount of new growth (NLKNGx) from each source of NLEAK.
  #      Based on :
  #      N in new growth = NLKNGx * PNTVG (amount of new growth * proportion N 
  #      in new growth (assume to be "initial" N concentration.
  #      N to be returned to provide the CH2O required for new growth
  #      NLKNGx * CH2O cost/g new growth * 1/CH2O recivered/g N returned.
  #      NLKRTRNx = the sum of these two (N required for new growth + 
  #      N returned for CH2O). Set NLKRTRNx = sum of the two equations and 
  #      solve algebraically for NLKNGx to find how much new growth can be 
  #      generated from each component of NLEAK.  Note that no N is returned to 
  #      natural senescence so "new growth" = N/0.16.
  
  NLKNG1 <- NLKRTRN1 * 1/(PNTVG + AGRVGI / (RPRO * 0.16))
  if (NLKRTRN2 > 0.0) NLKNG2 <- NLKRTRN2 * 1 / (PNTVG + AGRVGI / (RNNU * 0.16))
  NLKNG3 <- NLKRTRN3 / 0.16
  #      Calculate total CH2O cost of growth before adding in NLEAK growth
  OCH2OCOST <- AGRVG * (WLDOTN + WSDOTN +WRDOTN + WSRDOTN)
  
  #      Add new growth to WxDOTN
  WLDOTN  <- WLDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRLF
  WSDOTN  <- WSDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRSTM
  WRDOTN  <- WRDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRRT
  WSRDOTN <- WSRDOTN + (NLKNG1 + NLKNG2 + NLKNG3) * FRSTR      
  
  #      Add N in new growth to NGRxx
  NGRLF <- NGRLF + (NLKNG1 + NLKNG2) * FRLF *FNINL + NLKRTRN3 * FRLF            
  NGRST <- NGRST + (NLKNG1 + NLKNG2) * FRSTM *FNINS + NLKRTRN3 * FRSTM            
  NGRRT <- NGRRT + (NLKNG1 + NLKNG2) * FRRT *FNINR + NLKRTRN3 * FRRT            
  NGRSR <- NGRSR + (NLKNG1 + NLKNG2) * FRSTR *FNINSR + NLKRTRN3 * FRSTR            
  
  #      Adjust NRUSxx for N "returned" for CH2O
  #      Adjust ANMINExx for N "returned" for CH2O
  ANMINETOT <- ANMINELF + ANMINEST + ANMINERT + ANMINESR
  if (ANMINETOT > 0.0) {
    ANMINELF <- ANMINELF - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINELF/ANMINETOT
    ANMINEST <- ANMINEST - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINEST/ANMINETOT
    ANMINERT <- ANMINERT - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINERT/ANMINETOT
    ANMINESR <- ANMINESR - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINESR/ANMINETOT
    
    ANMINETOT <- ANMINELF + ANMINEST + ANMINERT + ANMINESR
    
    NRUSLF <- NRUSLF - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINELF/ANMINETOT
    NRUSST <- NRUSST - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINEST/ANMINETOT
    NRUSRT <- NRUSRT - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINERT/ANMINETOT
    NRUSSR <- NRUSSR - NLKNG1 * (AGRVGI / (RPRO * 0.16)) * ANMINESR/ANMINETOT
  }
  
  #      Calculate how much NLEAK was used as N and how much was 
  #      returned for CH2O
  
  CHORECOVER <- NLKNG1 * AGRVGI + NLKNG2 * AGRVGI
  AGRVG <- (OCH2OCOST + CHORECOVER) / (WLDOTN + WSDOTN + WRDOTN + WSRDOTN)
  if (TRNU > 0.001) {
    NLKSPENT <- NLKNG1 * AGRVGI / (RPRO * 0.16) + NLKNG2 * AGRVGI / (RNNU * 0.16)
  } else {
    NLKSPENT <- NLKNG1 * AGRVGI / (RPRO * 0.16) 
  }
  
  NLKNUSED <- NLKNG1 * PNTVG + NLKNG2 * PNTVG + NLKRTRN3
  NLEAK <- NLEAK - (NLKSPENT + NLKNUSED)
  
  #      Return NLKSPENT to its sources
  NMINEA <- NMINEA - NLKNG1 * (AGRVGI / (RPRO * 0.16))  
  if (TRNU > 0.001) {
    TRNURTRN <- NLKNG2 * AGRVGI / (RNNU * 0.16)
    
    
    #      Allocate N "returned for respiration" (CH2O) to layers as NO3&NH4
    
    for (L in 1:NLAYR) {
      if (TRNO3U > 0.0) {
        UNO3[L] <- UNO3[L] - TRNURTRN * 10 * PNUPNO3 * UNO3[L]/TRNO3U
      }
      
      if (TRNH4U > 0.0) {
        UNH4[L] <- UNH4[L] - TRNURTRN * 10 * PNUPNH4 * UNH4[L]/TRNH4U
      }
      
    }
    
    TRNO3U <- TRNO3U - PNUPNO3 * TRNURTRN * 10
    TRNH4U <- TRNH4U - PNUPNH4 * TRNURTRN * 10
    TRNU   <- TRNU - TRNURTRN
    
  }
  #  assigns
  assign("TRNH4U", TRNH4U, envir = env)
  assign("TRNO3U", TRNO3U, envir = env)
  assign("UNH4", UNH4, envir = env)
  assign("UNO3", UNO3, envir = env)
  assign("ANMINELF", ANMINELF, envir = env)
  assign("ANMINERT", ANMINERT, envir = env)
  assign("ANMINESR", ANMINESR, envir = env)
  assign("ANMINEST", ANMINEST, envir = env)
  assign("NRUSLF", NRUSLF, envir = env)
  assign("NRUSRT", NRUSRT, envir = env)
  assign("NRUSSR", NRUSSR, envir = env)
  assign("NRUSST", NRUSST, envir = env)
  assign("NLEAK", NLEAK, envir = env)
  assign("NGRLF", NGRLF, envir = env)
  assign("CHORECOVER", CHORECOVER, envir = env)
  assign("NLKSPENT", NLKSPENT, envir = env)
  assign("NLKNUSED", NLKNUSED, envir = env)
  assign("NGRRT", NGRRT, envir = env)
  assign("NGRST", NGRST, envir = env)
  assign("NGRSR", NGRSR, envir = env)
  assign("NMINEA", NMINEA, envir = env)
  assign("TRNU", TRNU, envir = env)
  assign("WLDOTN", WLDOTN, envir = env)
  assign("WRDOTN", WRDOTN, envir = env)
  assign("WSDOTN", WSDOTN, envir = env)
  assign("WSRDOTN", WSRDOTN, envir = env)
  assign("AGRVG", AGRVG, envir = env)
  return()
}



CANOPY <- function (DYNAMIC, DAS, PAR, TGRO) {
  params <- plantList$forage$params
  
  #-----------------------------------------------------------------------
  # TODO: ROWSPC - precisa dividir por 100?
  ROWSPC <- config$plant1$rowSpacing / 100 #0.50 #TODO: ARQUIVO DE MANEJO ROWSPC -> Row spacing (m)
  
  TS <- 24
  
  NHGT <- params$NHGT
  NSLA <- params$NSLA
  
  #______________________________________________________________        
  # *SOYBEAN ECOTYPE COEFFICIENTS: CRGRO047 MODEL
  # ECO# SB0602
  RHGHT  <- params$RHGHT    #0.9
  RWIDTH <- params$RWDTH   #1.0 #RWDTH no .ECO
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN   <- params$KCAN   #0.67
  #!*CANOPY HEIGHT AND WIDTH GROWTH PARAMETERS
  # TODO verificar se são 5, 8 (.SPE) ou 10 (.for) posições no vetor 
  XHWPAR  <- params$XHWPAR    #c(0.00,  5.00,  7.50, 10.00, 15.00, 20.00, 30.00, 80.00)
  YHWPAR  <- params$YHWPAR    #c(4.00,  2.00,  1.50,  1.25,  1.05,  1.00,  1.00,  1.00)
  XHWTEM  <- params$XHWTEM    #c(-50.0,  00.0,  15.0,  26.0,  60.0)
  YHWTEM  <- params$YHWTEM    #c(0.40,  0.40,  0.50,  1.00,  1.00)
  XVSHT   <- params$XVSHT     #c(0.00,  1.00,  4.00,  6.00,  8.00, 10.00, 14.00, 16.00, 20.00, 40.00)
  YVSHT   <- params$YVSHT     #c(0.0300, 0.0530, 0.0630, 0.0660, 0.0690, 0.0660, 0.0620, 0.0510, 0.0340, 0.0060)
  YVSWH   <- params$YVSWH     #c(0.0300, 0.0510, 0.0620, 0.0640, 0.0660, 0.0630, 0.0590, 0.0460, 0.0250, 0.0010)
  #fim dos parametros de planta
  
  #***********************************************************************
  #***********************************************************************
  #     SEASONAL INITIALIZATION 
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    CANHT  <- 0.0
    CANWH  <- 0.0
    CUMNHT <- 0.0
    
    
    #***********************************************************************
    #***********************************************************************
    #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #         or transplanting of plants
    #***********************************************************************
  } else if  (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    CANHT  <- TABEX(YVSHT,XVSHT,VSTAGE,10)       
    CANWH  <- TABEX(YVSWH,XVSHT,VSTAGE,10)  
    #        CANHT  = TABEX(YVSHT*100,XVSHT,VSTAGE,10) !from m to cm      
    #        CANWH  = TABEX(YVSWH*100,XVSHT,VSTAGE,10) !from m to cm
    
    #        CANHT = MOWHT
    #        IF (MOWHT .GT. 0.0) THEN CANHT = MOWHT
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if  (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    #     Calculate effect of temperature on canopy expansion, HWTEM
    #-----------------------------------------------------------------------
    HWTEM <- 0.0
    for (I in 1:24) {
      # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
      # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
      
      HWTEM <- HWTEM + TABEX(YHWTEM,XHWTEM,TGRO[I],5)
    }
    HWTEM <- HWTEM /24.
    #       24 changed to TS on 5 July 2017 by Bruce Kimball
    
    #-----------------------------------------------------------------------
    #     Calculate effect of day's PAR on canopy expansion, HPAR.
    #     ASSUME THAT UPPER 30% OF CANOPY SHADES THE GROWING POINT
    #     WPAR IS EFFECT ON WIDTH.  SHADE DOES NOT MAKE MUCH WIDER. LOWER K?
    #-----------------------------------------------------------------------
    #     IF (XLAI .GT. 0.1) THEN
    #        PARNOD = PAR * EXP(-KCAN*0.3*(XLAI-0.1))
    #     ELSE
    #        PARNOD = PAR
    #     }
    #-----------------------------------------------------------------------
    PARNOD <- PAR * exp(-KCAN*(0.3*XLAI))
    HPAR   <- TABEX(YHWPAR,XHWPAR,PARNOD,8)
    WPAR   <- TABEX(YHWPAR,XHWPAR,PAR,8)
    
    
    if (NHGT > 1.4) {                              #To limit NSLA to 1.4
      NHGT <- 1.4 
    }
    HNHGT  <-  max(0.1, (1.0 - (1.0 - NSTRES)*NHGT))
    CUMNHT <- 0.75*CUMNHT + 0.25*HNHGT
    #-----------------------------------------------------------------------
    #     Calculate rate of increase in canopy height and update height, CANHT
    #-----------------------------------------------------------------------
    RCANHT <- RVSTGE * TABEX(YVSHT,XVSHT,VSTAGE,10) * HWTEM * TURFAC * HPAR * RHGHT * CUMNHT
    CANHT  <- CANHT + RCANHT
    
    
    #     Set minimum Canopy height based on lookup function
    CANHT  <- max(CANHT, TABEX(YVSHT,XVSHT, 0.0, 10))
    
    #-----------------------------------------------------------------------
    #     Calculate rate of increase in canopy width and update width, CANWH
    #     RWIDTH,RHGHT are used to normalize other crops to the values in tables
    #     Values of RHGHT and RWIDTH = 1.00 are for Florunner peanut variety
    #     1/22/03 KJB - Don't allow reduction in vstage to reduce canopy
    #       width.
    #-----------------------------------------------------------------------
    RCANWH <- max(0.0,RVSTGE) * TABEX(YVSWH,XVSHT,VSTAGE,10) * HWTEM * TURFAC * WPAR * RWIDTH * CUMNHT
    CANWH  <- CANWH + RCANWH
    
    #     Set minimum Canopy width based on lookup function
    CANWH  <- max(CANWH, TABEX(YVSWH, XVSHT, 0.0, 10))  
    CANWH  <- min(CANWH,ROWSPC)
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("CANHT", CANHT, envir = env)
  assign("CANWH", CANWH, envir = env)
  assign("CUMNHT", CUMNHT, envir = env)
  
  return()
}
#--------------END VEGGR FUNCTION--------------

#---------------PODDET FUNCTION----------------
PODDET <- function(DYNAMIC, iyear, jday) {         #Input
  params <- plantList$forage$params
  TS <- 24
  
  #-----------------------------------------------------------------------
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*POD LOSS PARAMETERS
  DWC    <- params$DWC  #6.0
  PR1DET <- params$PR1DET  #0.3961
  PR2DET <- params$PR2DET  #-0.865
  XP1DET <- params$XP1DET  #1.00
  XP2DET <- params$XP2DET  #0.00
  #!*PHENOLOGY PARAMETERS   
  TB	<- params$TB  #c( 7,  6, -15, 0, 0)
  TO1	<- params$TO1  #c(28, 26,  26, 0, 0)
  TO2	<- params$TO2  #c(35, 30,  34, 0, 0)
  TM	<- params$TM  #c(45, 45,  45, 0, 0)
  
  TDLM <- rep(0,20)
  NCOHORTS <- 300 #from line 51 in ModuleDefs.for NCOHORTS = 300, !Maximum number of cohorts
  # TODO: Verificar essas variaveis
  WPODY  <- rep(0, NCOHORTS)
  PDET   <- rep(0, NCOHORTS)
  DAYS   <- rep(0, NCOHORTS)
  MSHELN <- rep(0, NCOHORTS)
  DTC    <- rep(0, NCOHORTS)
  
  
  #***********************************************************************
  #***********************************************************************
  #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
  #         or transplanting of plants
  #***********************************************************************
  if (DYNAMIC == 'EMERG') {
    #-----------------------------------------------------------------------
    # ALTERADO: Provavelmente esse número é como o fortran referencia até onde o loop vai repetir.
    for (I in 1:NCOHORTS) {
      DTC[I]    <- 0.0
      MSHELN[I] <- 0.0
      WPODY[I]  <- 0.0
      DAYS[I]   <- 0.0
    }
    PODWTD <- 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    #     Compute thermal time using hourly predicted air temperature
    #     based on observed max and min temperature.
    #--------------------------------------------------------------------
    FT <- 0.0
    for (I in 1:24) {
      # TGRO[I] <-TGRO_T$V3[TGRO_T$V1==DAS & TGRO_T$V2==I]
      # TGRO[I] <- tl_h[I] - 273.15         # TGRO[I] <- ta_h[I] - 273.15
      
      FTHR <- CURV('LIN',TB[3],TO1[3],TO2[3],TM[3],TGRO[I])
      FT   <- FT + FTHR/24.
    }
    # -------------------------------------------------------------------
    #  Compute ratio of leaf area per pod cm2/pod
    #  and leaf mass per pod mass g/g
    # -------------------------------------------------------------------
    TPODM <- 0.0
    RLMPM <- 1.0
    
    # -------------------------------------------------------------------
    #      Compute 10 day running average of leaf mass and PGAVL
    # -------------------------------------------------------------------
    # TODO VERIFICAR: 10,2,-1 deve ser de 10 até 2 no passo -1 (10, 9, 8, 7...)
    for (I in seq(10, 2)) { #TODO: checar numero e sintate
      TDLM[I] <- TDLM[I-1]
    }
    TDLM[1] <- WTLF
    # -------------------------------------------------------------------
    #     Compute slope of leaf mass curve
    # -------------------------------------------------------------------
    SL10 <- (TDLM[1] - TDLM[10])/ 10.0
    
    #---------------------------------------------------------------------
    
    
    if (YRNR2 >= 0) {
      
      yrdoy <- as.character(paste0(substr(YRDOY,1,4),'-01-01'))
      yrdoy <- as.Date(yrdoy)+as.numeric(substr(YRDOY,5,7))-1
      
      yrnr2 <- as.character(paste0(substr(YRNR1,1,4),'-01-01'))
      yrnr2 <- as.Date(yrnr2)+as.numeric(substr(YRNR2,5,7))-1
      
      #---------------------------------------------------------------------
      # ALTERADO: Sempre usar parenteses no for
      for (NPP in 1:(yrdoy - yrnr2))  { 
        TPODM <- TPODM + WTSHE[NPP] + WTSD[NPP]
      }
      
      if (TPODM > 10.0) RLMPM = WTLF / TPODM
      #-------------------------------------------------------------------
      #     Main loop that cycles through detachment model
      #--------------------------------------------------------------------
      for (NPP in 1:(yrdoy - yrnr2)) { 
        #--------------------------------------------------------------------
        #     Determine maximum cohort shell mass and accumulate
        #     days without carbohydrate on a cohort basis
        #--------------------------------------------------------------------
        if (SHELN[NPP] > MSHELN[NPP]) {
          MSHELN[NPP] <- SHELN[NPP]
        }
        if (WTSD[NPP] + WTSHE[NPP] >= 0.01) {
          if (WTSD[NPP] + WTSHE[NPP] <= WPODY[NPP] &&  WTSD[NPP] > 0.0) {
            DAYS[NPP] <-  DAYS[NPP] + 1.
          }
          
          if (WTSD[NPP] + WTSHE[NPP] > WPODY[NPP]) {
            DAYS[NPP] <- 0
          }
          
          #-----------------------------------------------------------------------
          #     Accumulate pod detachment thermal time counter (DTC) based on
          #     ratio of LFM/PDM and 10 day average slope of the leaf mass curve
          #-----------------------------------------------------------------------
          #           if (RLMPM > PR1DET | SL10 > PR2DET) GOTO 700
          if (RLMPM <= PR1DET & SL10 <= PR2DET) {
            if ((SL10 <= PR2DET) | DAYS[NPP] > DWC | WTLF <= 10.) {
              DTC[NPP] <- DTC[NPP] + FT
            }
          } else {
            #           Accumulate DTC based on days without carbon before RLMPM < PR1DET
            #           and SL10 < PR2DET
            if (DAYS[NPP] > DWC | WTLF <= 10.) {
              DTC[NPP] <- DTC[NPP] + FT
            }
          }
          #-----------------------------------------------------------------------
        }
      }
      #--------------------------------------------------------------------
      #     Compute detachment for each cohort
      #--------------------------------------------------------------------
      for (NPP in 1:(yrdoy - yrnr2)) { 
        #       curve based on Drew control, disease and Lowman tag pod cohort study
        if (DTC[NPP] > 0) {
          
          # ALTERADO: EXP por exp
          XPD <- MSHELN[NPP] * (1.0 - XP1DET * exp(XP2DET*DTC[NPP])/100)
          XPD <- max(0.0,XPD)
          if (SHELN[NPP] > XPD) {
            if (SHELN[NPP] >= 0.01 & DTC[NPP] <= 34.) {
              PDET[NPP] <- SHELN[NPP] - XPD
              PDET[NPP] <- max(0.0,PDET[NPP])
              PODWTD    <- PODWTD + (WTSHE[NPP] + WTSD[NPP])*PDET[NPP] / SHELN[NPP]
              
              SDDAM <-  WTSD[NPP] * PDET[NPP] / SHELN[NPP]
              if (SDDAM > WTSD[NPP]) {
                SWIDOT <- SWIDOT + WTSD[NPP]
              } else {
                SWIDOT <- SWIDOT + SDDAM
              }
              
              SHDAM <- WTSHE[NPP] * PDET[NPP] / SHELN[NPP]
              if (SHDAM > WTSHE[NPP]) {
                WSHIDT <- WSHIDT + WTSHE[NPP]
              } else {
                WSHIDT <- WSHIDT + SHDAM
              }
              
              WTSD[NPP]  <- WTSD[NPP] * (1. - PDET[NPP] / SHELN[NPP])
              SDNO[NPP]  <- SDNO[NPP] * (1. - PDET[NPP] / SHELN[NPP])
              WTSHE[NPP] <- WTSHE[NPP]* (1. - PDET[NPP] / SHELN[NPP])
              SHELN[NPP] <- SHELN[NPP]* (1. - PDET[NPP] / SHELN[NPP])
              
              WTSHE[NPP] <- max(0.0,WTSHE[NPP])
              SHELN[NPP] <- max(0.0,SHELN[NPP])
              WTSD[NPP]  <- max(0.0,WTSD[NPP])
              SDNO[NPP]  <- max(0.0,SDNO[NPP])
            }
          }
        }
        WPODY[NPP] <- WTSD[NPP] + WTSHE[NPP]
      }
      
      SUMSD <- 0.0
      SUMSH <- 0.0
      for (NPP in 1:(yrdoy - yrnr2))  { 
        SUMSD <- SUMSD + WTSD[NPP]
        SUMSH <- SUMSH + WTSHE[NPP]
      }
    }
    
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  
  assign("PODWTD", PODWTD, envir = env)
  assign("SDNO", SDNO, envir = env)
  assign("SHELN", SHELN, envir = env)
  assign("SWIDOT", SWIDOT, envir = env)
  assign("WSHIDT", WSHIDT, envir = env)
  assign("WTSD", WTSD, envir = env)
  assign("WTSHE", WTSHE, envir = env)
  
  return()
}
#--------------END PODDET FUNCTION-------------

#---------------SENES FUNCTION-----------------
SENES <- function (DYNAMIC, DAS, PAR) {
  params <- plantList$forage$params
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF SENESCENCE FACTORS
  ICMP   <- params$ICMP    #0.80
  TCMP   <- params$TCMP    #10.0
  SENDAY <- params$SENDAY  #0.06
  SENRT2 <- params$SENRT2  #0.20
  SENRTE <- params$SENRTE  #0.80
  SENMAX <- params$SENMAX  #c(0.0, 0.2, 0.6 , 0.6)
  SENPOR <- params$SENPOR  #c(0.0, 0.0, 0.12, 0.12)
  XSENMX <- params$XSENMX  #c(3.0, 5.0, 10.0, 30.0)
  XSTAGE <- params$XSTAGE  #c(0.0, 5.0, 14.0, 30.0)
  #!*VEGETATIVE PARTITIONING PARAMETERS
  PORPT  <- params$PORPT  #0.58
  #!*PHOTOSYNTHESIS PARAMETERS
  KCAN   <- params$KCAN   #0.67
  # fim dos parametros de especie
  
  # SWFCAB <- rep(0,NSWAB)
  
  #TYPE (ControlType) CONTROL
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    SSDOT  <- 0.0
    SLDOT  <- 0.0
    SLNDOT <- 0.0
    SSNDOT <- 0.0
    # RATTP  = 1.0
    
    # for (I in 1:5) {
    #   SWFCAB[I] = 1.0
    # }
    SWFCAB <- rep(1,5)
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    #Update value of RATTP.
    
    NSWAB  <- 5
    
    # ALTERADO: NSWAB, 2, -1 to seq(NSWAV, 2)
    # TODO VERIFICAR: Se I começa em 1, pois no indice abaixo ele subtrai. Se começar em 1 o indice vai ficar 0.
    for (I in seq(NSWAB,2)) { 
      SWFCAB[I] <- SWFCAB[I-1]
    }
    SWFCAB[1] <- SWFAC
    RATTP <- SWFCAB[NSWAB]
    
    SSDOT  <- 0.0
    SLDOT  <- 0.0
    SLNDOT <- 0.0
    SSNDOT <- 0.0
    
    if (DAS <= NR7 & VSTAGE >= 1.0) {
      #-----------------------------------------------------------------------
      #     This section calculates natural senescence prior to the
      #     beginning of seed growth
      #-----------------------------------------------------------------------
      if (VSTAGE >= 5.0) {
        PORLFT <- 1.0 - TABEX(SENPOR,XSTAGE,VSTAGE,4)
        if ((WTLF * ( 1.0 - RHOL)) > CLW*PORLFT) {
          SLDOT <- WTLF * ( 1.0 - RHOL) - CLW * PORLFT
        }
      }
      #-----------------------------------------------------------------------
      #     This section calculates leaf senescence due to N mobilization.
      #     Concentration of N in stems and leaves must
      #     be recalculated since senescing leaves and petioles are assumed
      #     to contain no more mineable Protein--i.e., N content of senesced
      #     leaves and petioles is different from canopy average.
      #-----------------------------------------------------------------------
      LFSEN <- SENRTE * NRUSLF / 0.16
      LFSEN <- min(WTLF,LFSEN)
      SLDOT <- SLDOT + LFSEN
      SLDOT <- min(WTLF,SLDOT)
      #-----------------------------------------------------------------------
      #     This section calculates senescence due to low light in lower
      #     canopy.  First compute LAI at which light compensation is reached
      #     then allow LAI above this amount to be senesced over TCMP thermal
      #     days.
      #-----------------------------------------------------------------------
      LTSEN <- 0.0
      if (PAR > 0.) {
        LCMP  <- -(1. / KCAN) * log(ICMP / PAR)
        LTSEN <- DTX * (XLAI - LCMP) / TCMP
        LTSEN <- max(0.0, LTSEN)
      }
      #-----------------------------------------------------------------------
      #     Convert area loss to biomass(m2 *10000cm2/m2)/(cm2/g)=g/m2
      #-----------------------------------------------------------------------
      SLDOT <- SLDOT + LTSEN * 10000. / SLAAD
      #-----------------------------------------------------------------------
      #     Calculate senescence due to water stress.
      #-----------------------------------------------------------------------
      WSLOSS <- SENDAY * (1. - RATTP) * WTLF
      if (WSLOSS > 0.0) {
        PORLFT <- 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
        WSLOSS <- min(WSLOSS, WTLF - CLW * PORLFT)
        WSLOSS <- max(WSLOSS, 0.0)
        SLNDOT <- WSLOSS
      }
      SLDOT  <- SLDOT + SLNDOT
      SSDOT  <- SLDOT * PORPT
      SSDOT  <- min(SSDOT,0.1*STMWT)
      SSNDOT <- SLNDOT * PORPT
      SSNDOT <- min(SSDOT,SSNDOT)
      #-----------------------------------------------------------------------
      #     This section calculates senescence of leaves and petioles
      #     after R7.
      #-----------------------------------------------------------------------
    } else if (DAS > NR7) {
      if (WTLF > 0.0001) {
        SLDOT  <- WTLF * SENRT2
        SLNDOT <- SLDOT
        SSDOT  <- SLDOT * PORPT
        SSNDOT <- SSDOT
      } else {
        SLDOT  <- 0.0
        SSDOT  <- 0.0
        SLNDOT <- 0.0
        SSNDOT <- 0.0
      }
      if (STMWT < 0.0001) {
        SLNDOT <- 0.0
        SSNDOT <- 0.0
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  
  assign("SLDOT",SLDOT , envir = env)
  assign("SLNDOT",SLNDOT, envir = env)
  assign("SSDOT",SSDOT , envir = env)
  assign("SSNDOT",SSNDOT, envir = env)
  assign("SWFCAB",SWFCAB, envir = env)
  
  return()
}
#--------------END SENES FUNCTION--------------

#---------------FREEZE FUNCTION----------------
FREEZE <- function(TMIN, iyear, jday) {
  params <- plantList$forage$params
  
  FRZDC   <- params$FRZDC
  FREEZ1  <- params$FREEZ1
  
  #______________________________________________________________        
  # SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*LEAF SENESCENCE FACTORS
  FREEZ2 <- params$FREEZ2  #-5.00
  assign("FREEZ2",FREEZ2, envir = env)
  
  YRDOY   <- paste0(iyear,sprintf("%03d", jday))
  YRPLT   <- paste0(iyear,sprintf("%03d", jday))
  
  
  WSRFDOT <- 0.0
  
  PSRSRFD <- 0.0
  PSRLYRD <- 0.0
  #-----------------------------------------------------------------------
  #      Calculate proportion of above-ground plant mass that will
  #      be lost to frost.  Function of number of degrees below threshold 
  #      temperature multiplied by the proportion lost per degree below
  #      threshold.  For total kill (like DSSAT35) set FRZDC=1.0
  #-----------------------------------------------------------------------
  
  FRZDL  <- (FREEZ1 - TMIN) * FRZDC
  FRZDL  <- min(FRZDL,1.0)
  FRZDL  <- max(FRZDL,0.0)
  
  WLFDOT <- (WTLF - SLDOT - NRUSLF/0.16) * FRZDL
  WLFDOT <- min((WTLF - SLDOT - NRUSLF/0.16), WLFDOT)
  #-----------------------------------------------------------------------
  #      SJR 5/12/04 Moved VSTAGE adjustment to GROW
  #       to be compatible with adjustment for senescence
  #-----------------------------------------------------------------------
  #            VSTAGE = ((WTLF-WLFDOT)/WTLF) * VSTAGE
  
  WSFDOT <- (STMWT - SSDOT - NRUSST/0.16) * FRZDL
  WSFDOT <- min ((STMWT - SSDOT - NRUSST/0.16), WSFDOT)
  
  
  if (TMIN <= FREEZ2) {
    # TODO: Verificar se mata a planta e rebrota! Leandro 23/10/2020
    frost <- T #Henrique: criei para encerrar o ciclo devido à geada severa (2020-10-1)
    if (MDATE < 0) {
      MDATE <- YRDOY
    }
    WLFDOT <- WTLF - SLDOT - NRUSLF/0.16
    #-----------------------------------------------------------------------
    #      SJR 5/12/04 Moved VSTAGE adjustment to GROW
    #       to be compatible with adjustment for senescence
    #-----------------------------------------------------------------------
    #                  VSTAGE = 0.0
    WSFDOT <- STMWT - SSDOT - NRUSST/0.16
  }
  # DAP   = max(0,TIMDIF(YRPLT,YRDOY))    TODO: Adaptar a função timedif
  
  #    WRITE(MESSAGE(1),100) DAP
  #    WRITE(MESSAGE(2),110) YRDOY
  #    CALL WARNING(1, 'FREEZE', MESSAGE)
  #100 FORMAT('Freeze occurred at ',I4,' days after planting.')
  #110 FORMAT('  (DAY : ',I7,' )')
  #    WRITE (*,'(/,2X,A78,/,2X,A78)') MESSAGE(1), MESSAGE(2)
  #    if (IDETO == 'Y')  {
  #      WRITE (NOUTDO,'(/,5X,A78,/,5X,A78)') MESSAGE(1), MESSAGE(2)
  #    }
  print(paste0('Freeze occurred at ',idpp,' days after planting.'))
  
  #-----------------------------------------------------------------------
  assign("MDATE", MDATE, envir = env)
  assign("WLFDOT", WLFDOT, envir = env)
  assign("frost", frost, envir = env)
  assign("PSRSRFD", PSRSRFD, envir = env)
  assign("PSRLYRD", PSRLYRD, envir = env)
  assign("WSFDOT", WSFDOT, envir = env)
  
  return()
}
#--------------END FREEZE FUNCTION-------------

#---------------INCOMP FUNCTION----------------
INCOMP <- function(DYNAMIC) {
  params <- plantList$forage$params
  
  ECONO   <- params$ECONO
  ECOTYP  <- params$ECOTYP
  PLIGSR  <- params$PROSRI
  PROSRI  <- params$PLIPSR
  PLIPSR  <- params$PLIGSR
  POASR   <- params$POASR
  PMINSR  <- params$PMINSR
  PCARSR  <- params$PCARSR
  
  #______________________________________________________________        
  # *SOYBEAN GENOTYPE COEFFICIENTS: CRGRO047 MODEL
  SDLIP <- params$SDLIP  #0.200 #Fraction oil in seeds (g(oil)/g(seed))  
  SDPRO <- params$SDPRO  #0.400 #Fraction protein in seeds (g(protein)/g(seed))  
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*PLANT COMPOSITION VALUES
  PCARLF   <- params$PCARLF    #0.405
  PCARNO   <- params$PCARNO    #0.450
  PCARRT   <- params$PCARRT    #0.711
  PCARSD   <- params$PCARSD    #0.315
  PCARSH   <- params$PCARSH    #0.380
  PCARST   <- params$PCARST    #0.664
  PLIGLF   <- params$PLIGLF    #0.070
  PLIGNO   <- params$PLIGNO    #0.070
  PLIGRT   <- params$PLIGRT    #0.070
  PLIGSD   <- params$PLIGSD    #0.020
  PLIGSH   <- params$PLIGSH    #0.280
  PLIGST   <- params$PLIGST    #0.070
  PLIPLF   <- params$PLIPLF    #0.025
  PLIPNO   <- params$PLIPNO    #0.050
  PLIPRT   <- params$PLIPRT    #0.020
  PLIPSH   <- params$PLIPSH    #0.020
  PLIPST   <- params$PLIPST    #0.020
  PMINLF   <- params$PMINLF    #0.094
  PMINNO   <- params$PMINNO    #0.050
  PMINRT   <- params$PMINRT    #0.057
  PMINSD   <- params$PMINSD    #0.025
  PMINSH   <- params$PMINSH    #0.030
  PMINST   <- params$PMINST    #0.046
  POALF    <- params$POALF     #0.050
  POANO    <- params$POANO     #0.050
  POART    <- params$POART     #0.050
  POASD    <- params$POASD     #0.040
  POASH    <- params$POASH     #0.040
  POAST    <- params$POAST     #0.050
  PROLFI   <- params$PROLFI    #0.356
  PRORTI   <- params$PRORTI    #0.092
  PROSHI   <- params$PROSHI    #0.250
  PROSTI   <- params$PROSTI    #0.150
  SDPROS   <- params$SDPROS    #0.400
  #!*RESPIRATION PARAMETERS
  RCH2O    <- params$RCH20   #1.242
  RLIG     <- params$RLIG    #2.174
  RLIP     <- params$RLIP    #3.106
  RMIN     <- params$RMIN    #0.050
  RNO3C    <- params$RNO3C   #2.556
  ROA      <- params$ROA     #0.929
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    #     COMPUTE RESPIRATION COEFFICIENTS BASED ON PLANT COMPOSITION
    #-----------------------------------------------------------------------
    #
    AGRLF  <-  PLIPLF*RLIP + PLIGLF*RLIG + POALF*ROA + PMINLF*RMIN + PCARLF*RCH2O
    AGRSTM <-  PLIPST*RLIP + PLIGST*RLIG + POAST*ROA + PMINST*RMIN + PCARST*RCH2O
    AGRRT  <-  PLIPRT*RLIP + PLIGRT*RLIG + POART*ROA + PMINRT*RMIN + PCARRT*RCH2O
    AGRNOD <-  PLIPNO*RLIP + PLIGNO*RLIG + POANO*ROA + PMINNO*RMIN + PCARNO*RCH2O
    
    #-----------------------------------------------------------------------
    #     AGRSTR storage growth cost, AGRSR2 include protein component of  
    #     growth cost
    #-----------------------------------------------------------------------
    
    AGRSTR <- PLIPSR*RLIP + PLIGSR*RLIG + POASR*ROA + PMINSR*RMIN + PCARSR*RCH2O
    
    #-----------------------------------------------------------------------
    #     AGRVG2, AGRSH2, AGRSD2 include protein component of vegetative 
    #     growth cost
    #-----------------------------------------------------------------------
    AGRVG  <- AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM + AGRSTR * FRSTR
    
    AGRVG2 <- AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI +FRSTR*PROSRI)*RNO3C
    
    #-----------------------------------------------------------------------
    AGRSH1 <-  PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA  + PMINSH*RMIN + PCARSH*RCH2O
    AGRSH2 <-  AGRSH1 + PROSHI*RNO3C 
    
    #-----------------------------------------------------------------------
    SDPROR <- (SDPRO - SDPROS) / ( SDLIP + PCARSD )
    AGRSD1 <- PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
    AGRSD2 <- AGRSD1 + SDPRO*RNO3C 
    
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
  assign("AGRSTR", AGRSTR, envir = env)
  assign("FRSTR", FRSTR, envir = env)
  
  return()
}
#--------------END INCOMP FUNCTION-------------

#---------------NUPTAK FUNCTION----------------
NUPTAK <- function (DYNAMIC, PGAVL) {
  params <- plantList$forage$params
  
  RNH4C  <- params$RNH4C
  RNO3C  <- params$RNO3C
  
  #______________________________________________________________        
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*ROOT PARAMETERS
  RTNO3  <- params$RTNO3  #0.006
  RTNH4  <- params$RTNH4  #0.006
  # fim dos parametros de planta
  
  NL <- 20
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    # TODO: Verificar o uso do -99.000000
    SNO3   <- rep(0, NL)    #vem do INSOIL.for
    SNH4   <- rep(0, NL)    #vem do INSOIL.for
    INO3   <- c(1.10000002,1.10000002,1.10000002,1.10000002,1.10000002,1.10000002,1.10000002,1.10000002,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000)
    INH4   <- c(0.100000001,0.100000001,0.100000001,0.100000001,0.100000001,0.100000001,0.100000001,0.100000001,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000,-99.0000000)
    
    # TODO: Verificar A necessidade dessa Variável KG2PPM, No origiral era pra ser REMOVIDA!!!
    KG2PPM <- rep(0, NL)
    
    # TODO: Verificar o código abaixo KG2PPM!!
    for(L in 1:NL)
    {
      KG2PPM[L] <- 1.0/(BD[L] * 0.1 * DLAYR[L])
      SNO3[L] <- INO3[L] / KG2PPM[L]
      SNH4[L] <- INH4[L] / KG2PPM[L]
    }
    #linkar com variavel do ecosmos depois
    # NO3    <- rep(2, NL)    #vem do INSOIL.for
    # NH4    <- rep(2, NL)    #vem do INSOIL.for
    
    TRNO3U <- 0.0 
    TRNH4U <- 0.0 
    TRNU   <- 0.0 
    UNH4   <- rep(0, NLAYR)
    UNO3   <- rep(0, NLAYR)
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    FAC <- rep(0, NLAYR)
    
    #-----------------------------------------------------------------------
    #   Initialize variables
    #-----------------------------------------------------------------------
    TRNU     <- 0.0
    TRNO3U   <- 0.0
    TRNH4U   <- 0.0
    NUF      <- 0.0
    XMIN     <- 0.0
    
    PTRNO3U  <- 0.0
    PTRNH4U  <- 0.0
    for (L in 1:NLAYR) {
      RNO3U[L] <- 0.0
      RNH4U[L] <- 0.0
      UNH4[L]  <- 0.0
      UNO3[L]  <- 0.0
      
      FAC[L]   <- 10. / (BD[L] * DLAYR[L])
      SNO3[L]  <- NO3[L] / FAC[L]
      SNH4[L]  <- NH4[L] / FAC[L]
    }
    #-----------------------------------------------------------------------
    #   Determine crop N demand (kg N/ha), after subtracting mobilized N
    #-----------------------------------------------------------------------
    ANDEM <- (NDMTOT - max(TSNMOB ,NDMSDR)) * 10.0
    if (ANDEM > 0.0) {
      #-----------------------------------------------------------------------
      #   Calculate potential N uptake in soil layers with roots
      #-----------------------------------------------------------------------
      for (L in 1:NLAYR) {
        if (RLV[L] > 1.E-6) {
          FNH4 <- 1.0 - exp(-0.08 * NH4[L])
          FNO3 <- 1.0 - exp(-0.08 * NO3[L])
          if (FNO3 < 0.04) { FNO3 = 0.0 }
          if (FNO3 > 1.0)  { FNO3 = 1.0 }
          if (FNH4 < 0.04) { FNH4 = 0.0 }
          if (FNH4 > 1.0)  { FNH4 = 1.0 }
          
          SMDFR <- 2.0*((SW[L] - LL[L]) / (DUL[L] - LL[L]))
          if (SMDFR < 0.0) {
            SMDFR <- 0.0
          }
          
          if (SW[L] > DUL[L]) {
            SMDFR <- 2.0*(1.0 - (SW[L] - DUL[L]) / (SAT[L] - DUL[L]))
          }
          
          SMDFR <- max (SMDFR,0.0)
          SMDFR <- min (SMDFR,1.0)
          
          RFAC <- RLV[L] * SMDFR * SMDFR * DLAYR[L] * 100.0
          #-----------------------------------------------------------------------          
          #-----------------------------------------------------------------------
          #   Calculate potential N uptake in soil layers with roots
          #-----------------------------------------------------------------------
          #        DO L=1,NLAYR
          #        IF (RLV(L) .GT. 0.0) THEN
          #        FNH4 = 1.0 - EXP(-0.08 * NH4(L))
          #        FNO3 = 1.0 - EXP(-0.08 * NO3(L))
          #        IF (FNO3 .LT. 0.04) FNO3 = 0.0  
          #        IF (FNO3 .GT. 1.0)  FNO3 = 1.0
          #        IF (FNH4 .LT. 0.04) FNH4 = 0.0  
          #        IF (FNH4 .GT. 1.0)  FNH4 = 1.0
          #        
          #        SMDFR = 2.0*((SW(L) - LL(L)) / (DUL(L) - LL(L)))
          #        IF (SMDFR .LT. 0.0) THEN
          #        SMDFR = 0.0
          #        ENDIF
          #        
          #        IF (SW(L) .GT. DUL(L)) THEN
          #        SMDFR = 2.0*(1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L)))
          #        ENDIF
          #        SMDFR    = AMAX1 (SMDFR,0.0)
          #        SMDFR    = AMIN1 (SMDFR,1.0)
          #        
          #        RFAC = RLV(L) * SMDFR * DLAYR(L) * 100.0
          #         ENDIF
          #      END DO
          #            
          #            
          #-----------------------------------------------------------------------
          #  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
          #  DLAYR = Layer depth (cm)
          #  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
          #  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
          #  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
          #  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
          #-----------------------------------------------------------------------
          RNO3U[L] <- RFAC * FNO3 * RTNO3
          RNH4U[L] <- RFAC * FNH4 * RTNH4
          #-----------------------------------------------------------------------
          #      11/2/05 SJR Copied code limiting potential N uptake from the 
          #            "actual" uptake code below.  This prevents the situation where
          #            TRNU islarger than ANDEM, so TRNU is limited to TRNU but ends 
          #            up short of ANDEM because one or more layers is limited by 
          #            MXNO3U or MXNH4U.
          #-----------------------------------------------------------------------
          
          
          XMIN    <- 0.25 / FAC[L]
          MXNO3U  <- max(0.0,(SNO3[L] - XMIN))
          if (RNO3U[L] > MXNO3U) {
            RNO3U[L] <- MXNO3U
          }
          XMIN <- 0.5 / FAC[L]
          MXNH4U  <- max(0.0,(SNH4[L] - XMIN))
          if (RNH4U[L] > MXNH4U) {
            RNH4U[L] <- MXNH4U
          }
          
          RNO3U[L] <- max(0.0,RNO3U[L])
          RNH4U[L] <- max(0.0,RNH4U[L])
          TRNU <- TRNU + RNO3U[L] + RNH4U[L] #kg[N]/ha
          
          #-----------------------------------------------------------------------
          #     SJR 10/17/03 - Calculate potential total NO3 and NH4 uptake
          #-----------------------------------------------------------------------
          PTRNO3U  <- PTRNO3U + RNO3U[L]
          PTRNH4U  <- PTRNH4U + RNH4U[L]
          
        }
      }
      #-----------------------------------------------------------------------
      #   Calculate N uptake in soil layers with roots based on demand (kg/ha)
      #-----------------------------------------------------------------------
      
      #-----------------------------------------------------------------------
      #     SJR 10/17/03 - Calculate cost of uptake and reduction of CP 
      #      from potential NO3 and NH4
      #-----------------------------------------------------------------------
      
      PRSPNO3 <- (PTRNO3U/10)/0.16 * RNO3C  
      PRSPNH4 <- (PTRNH4U/10)/0.16 * RNH4C  
      
      #-----------------------------------------------------------------------
      #     SJR 10/17/03 - Check that cost of uptake and reduction of CP 
      #      from TRNU does not exceed PGAVL
      #-----------------------------------------------------------------------
      
      if (PGAVL < (PRSPNO3 + PRSPNH4)) {
        TRNU <- (PGAVL / (PRSPNO3+PRSPNH4)) * TRNU
      }
      
      
      if (ANDEM > TRNU) {
        ANDEM <- TRNU
      }
      #        IF (TRNU == 0.0) GO TO 600
      if (TRNU > 0.001) {
        #-----------------------------------------------------------------------
        #      10/31/05 SJR Replace TRNU with (PRSPNO3 + PRSPNH4) to prevent 
        #            excessive N-uptake when PGAVL limits TRNU to less than
        #            (PRSPNO3 + PRSPNH4), hence yielding a false high proportion
        #             of available N to be taken up - contributes to NLEAK.
        #-----------------------------------------------------------------------
        #          NUF = ANDEM / TRNU
        NUF <- ANDEM / (PTRNO3U + PTRNH4U)        
        for (L in 1:NLAYR) {
          if (RLV[L] > 0.0) {
            UNO3[L] <- RNO3U[L] * NUF
            UNH4[L] <- RNH4U[L] * NUF
            XMIN    <- 0.25 / FAC[L]
            MXNO3U  <- max(0.0,(SNO3[L] - XMIN))
            if (UNO3[L] > MXNO3U) {
              UNO3[L] <- MXNO3U
            }
            XMIN <- 0.5 / FAC[L]
            MXNH4U  <- max(0.0,(SNH4[L] - XMIN))
            if (UNH4[L] > MXNH4U) {
              UNH4[L] <- MXNH4U
            }
            TRNO3U  <- TRNO3U + UNO3[L]
            TRNH4U  <- TRNH4U + UNH4[L]
          }
        }
        
        #-----------------------------------------------------------------------
        #      Calculate proportion of TRNU coming from NO3 and NH4 in each layer
        #      for use in "returning" NLEAK
        #-----------------------------------------------------------------------
        for (L in 1:NLAYR) {
          if (TRNO3U > 0.0) {
            NUPNO3[L] <- UNO3[L] / TRNO3U
          }
          
          if (TRNH4U > 0.0) {
            NUPNH4[L] <- UNH4[L] / TRNH4U
            #        NUPNH4[L] = UNH4[L] / TRNO3U !kjb fixed 7/12/2017
          }
        }
        #-----------------------------------------------------------------------
        #   Convert uptake to g/m^2
        #-----------------------------------------------------------------------
        TRNO3U <- TRNO3U / 10.0
        TRNH4U <- TRNH4U / 10.0
        TRNU   <- TRNO3U + TRNH4U
        #-----------------------------------------------------------------------
      }
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("TRNH4U", TRNH4U, envir = env)
  assign("TRNO3U", TRNO3U, envir = env)
  assign("TRNU", TRNU, envir = env)
  assign("UNH4", UNH4, envir = env)
  assign("UNO3", UNO3, envir = env)
  
  assign("KG2PPM", KG2PPM, envir = env)
  assign("SNO3", SNO3, envir = env)
  assign("SNH4", SNH4, envir = env)
  
  return()
}
#--------------END NUPTAK FUNCTION-------------

#---------------- MOBIL FUNCTION---------------
MOBIL <- function(DYNAMIC) {
  params <- plantList$forage$params
  #!*RESPIRATION PARAMETERS (.SPE), mas não usado, aparentemente
  RPRO   <- params$RPRO   #0.360 
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    CNMINE <- 0.0         
    NMINEA <- 0.0         
    NRUSLF <- 0.0         #moved from INPLNT
    NRUSST <- 0.0         
    NRUSRT <- 0.0         
    NRUSSH <- 0.0
    NRUSSR <- 0.0
    PNMLF  <- 0.0
    PNMST  <- 0.0
    PNMRT  <- 0.0
    PNMSR  <- 0.0
    PNMSH  <- 0.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    #-----------------------------------------------------------------------
    CNMINE   <- 0.0
    NMINEA   <- 0.0
    NRUSLF   <- 0.0
    NRUSST   <- 0.0
    NRUSRT   <- 0.0
    NRUSSH   <- 0.0
    NRUSSR   <- 0.0
    
    ANMINELF <- 0.0
    ANMINEST <- 0.0
    ANMINERT <- 0.0
    ANMINESR <- 0.0
    ANMINESH <- 0.0
    
    #-----------------------------------------------------------------------
    #    Leave MOBIL with N Mined from Leaf, Stem,Root, Shell, and
    #    Total Plant Tissue, and CH2O used in the Re-synthesis of Protein
    #-----------------------------------------------------------------------
    #      IF (TRNU .LT. NDMNEW .AND. NMINEP .GT. 0.0) THEN
    #         NMINEA = NDMNEW - TRNU
    #         IF (NMINEA .GT. NMINEP) NMINEA = NMINEP
    
    
    #         NMINER = NMINEA/NMINEP * NMOBR
    #         NRUSLF = NMINER * WNRLF
    #         NRUSST = NMINER * WNRST
    #         NRUSSH = NMINER * WNRSH
    #         NRUSRT = NMINER * WNRRT * (PPMFAC)
    #           NRUSSR = NMINEA * WNRSR * NMOBSR / NMINEP
    
    #         CNMINE = NMINEA / 0.16 * RPRO        !Not used
    #      ENDIF
    NMINEA <- TSNMOB
    
    #     NMINELF SHOULD HAVE BEEN LFNMINE, ETC. IN EARLIER VERSION, NOW NMINEP=TSNMOB=NMINEA
    
    #      IF (TRNU .LT. NDMNEW .AND. NMINEP.GT. 0.0) THEN
    #         NMINEA = MAX(TSNMOB,NDMNEW - TRNU)
    #         IF (NMINEA .GT. NMINEP) NMINEA = NMINEP
    #
    #            IF (NMINEA .GT. TSNMOB) THEN
    #                  NMINER = (NMINEA - TSNMOB) / (NMINEP - TSNMOB)
    #                  ANMINESH = SHNMINE * NMINER
    #                  ANMINELF = (NMINELF - LFSNMOB) * NMINER
    #                  ANMINEST = (NMINEST - STSNMOB) * NMINER
    #                  ANMINERT = (NMINERT - RTSNMOB) * NMINER
    #                  ANMINESR = (NMINESR - SRSNMOB) * NMINER
    #            ENDIF
    #      ENDIF
    
    NRUSLF <- LFSNMOB + ANMINELF
    NRUSST <- STSNMOB + ANMINEST
    NRUSRT <- RTSNMOB + ANMINERT
    NRUSSR <- SRSNMOB + ANMINESR
    NRUSSH <- ANMINESH
    
    CNMINE <- NMINEA / 0.16 * RPRO        #Not used
    #-----------------------------------------------------------------------
    #    Calculate proportion of N Mined from Leaf, Stem,Root, Shell, and
    #      Storage - Use to put back excess mobilized N
    #-----------------------------------------------------------------------
    
    NRUSTOT <- NRUSLF+NRUSST+NRUSSH+NRUSRT+NRUSSR
    if (NRUSTOT > 0.0) {
      PNMLF <- NRUSLF / NRUSTOT
      PNMST <- NRUSST / NRUSTOT
      PNMRT <- NRUSRT / NRUSTOT
      PNMSR <- NRUSSR / NRUSTOT
      PNMSH <- NRUSSH / NRUSTOT
    } else {
      PNMLF <- 0.0
      PNMST <- 0.0
      PNMRT <- 0.0
      PNMSR <- 0.0
      PNMSH <- 0.0
      
    }
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  #  RETURN
  #END ! SUBROUTINE MOBIL
  assign("NMINEA", NMINEA, envir = env)
  assign("NRUSLF", NRUSLF, envir = env)
  assign("NRUSRT", NRUSRT, envir = env)
  assign("NRUSSH", NRUSSH, envir = env)
  assign("NRUSST", NRUSST, envir = env)
  assign("NRUSSR", NRUSSR, envir = env)
  assign("PNMLF", PNMLF, envir = env)
  assign("PNMST", PNMST, envir = env)
  assign("PNMRT", PNMRT, envir = env)
  assign("PNMSR", PNMSR, envir = env)
  assign("PNMSH", PNMSH, envir = env)
  assign("ANMINELF", ANMINELF, envir = env)
  assign("ANMINERT", ANMINERT, envir = env)
  assign("ANMINESH", ANMINESH, envir = env)
  assign("ANMINESR", ANMINESR, envir = env)
  assign("ANMINEST", ANMINEST, envir = env)
  
  return()
}
#----------------END MOBIL FUNCTION------------

#----------------NFIX FUNCTION-----------------
NFIX <- function(DYNAMIC, DAS, CNODMN, CTONOD) { #TODO Santiago # falta linkar, DLAYR, NLAYR,SAT, ST, SW 
  params <- plantList$forage$params
  #______________________________________________________________
  # *SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #!*NITROGEN FIXATION PARAMETERS
  TYPFXT <- params$TYPFXT  #'LIN'
  TYPNGT <- params$TYPNGT  #'LIN'
  TYPFXD <- params$TYPFXD  #'LIN'
  TYPFXW <- params$TYPFXW  #'LIN'
  TYPFXA <- params$TYPFXA  #'INL'
  FNFXT  <- params$FNFXT   #c(5.00,    20.0,  35.0,  44.0)
  FNNGT  <- params$FNNGT   #c(7.00,    22.0,  35.0,  44.0)
  FNFXD  <- params$FNFXD   #c(0.00,    0.85,  1.00,  10.0)
  FNFXW  <- params$FNFXW   #c(-0.02,  0.001,  1.00,  2.00)
  FNFXA  <- params$FNFXA   #c(0.00,    0.10,  1.00,  0.00)
  NDTHMX <- params$NDTHMX  #0.07
  NODRGM <- params$NODRGM  #0.170
  DWNODI <- params$DWNODI  #0.014
  SNACTM <- params$SNACTM  #0.045
  CNODCR <- params$CNODCR  #0.05
  #!*RESPIRATION PARAMETERS
  RFIXN  <- params$RFIXN  #2.830
  #!*PLANT COMPOSITION VALUES
  PRONOD <- params$PRONOD  #0.300
  
  #fim dos parametros de planta
  
  EFNFIX <- 1 # ICRE - equivalente ao [.SBX] *INITIAL CONDITIONS
  EFINOC <- 1 # ICRN - equivalente ao [.SBX] *INITIAL CONDITIONS
  EFINOC <- ifelse(EFINOC <= 0.0, 1.0, EFINOC) 
  EFNFIX <- ifelse(EFNFIX <= 0.0, 1.0, EFNFIX) 
  
  PLTPOP <- 40  # equivalente ao [.SBX] *PLANTING DETAILS: PPOE
  
  #TODO ver padrão ECOSMOS
  NLAYR <- nsoilay
  # DLAYR  <- rep(0, NL)
  # DLAYR <-  c(10.0000000, 10.0000000, 10.0000000, 10.0000000, 10.0000000 ,      10.0000000 ,      30.0000000 ,      30.0000000 ,      30.0000000    ,  -99.0000000   ,   -99.0000000   ,   -99.0000000    ,  -99.0000000    ,  -99.0000000    ,  -99.0000000    ,  -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000  ,    -99.0000000 )
  # SAT    <- rep(0, NL)
  # SAT   <-  c(0.360000014 ,   0.340000004 ,     0.330000013    ,   0.330000013   ,   0.319999993  ,    0.319999993  ,    0.319999993    ,  0.319999993  ,    0.319999993   ,   -99.0000000  ,    -99.0000000  ,    -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000   ,   -99.0000000  ,    -99.0000000  ,    -99.0000000  ,    -99.0000000 ,     -99.0000000)
  # SW     <- rep(0, NL)
  # SW    <-  c(0.219999999 ,    0.219999999  ,    0.213000000  ,    0.207000002  ,    0.200000003  ,    0.180000007   ,   0.180000007  ,    0.180000007    ,   0.00000000   ,    0.00000000  ,     0.00000000  ,     0.00000000   ,    0.00000000   ,    0.00000000   ,    0.00000000   ,    0.00000000  ,     0.00000000  ,     0.00000000  ,     0.00000000 ,      0.00000000)
  # NL = 20
  #TODO: linkar com ECOSMOS
  # ST<- rep(25,20)
  
  #***********************************************************************
  #***********************************************************************
  #     Seasonal initialization - run once per season
  #***********************************************************************
  if (DYNAMIC == 'SEASINIT') {
    #-----------------------------------------------------------------------
    CNOD   <- 0.0
    DWNOD  <- 0.0    
    DWNODA <- 0.0  
    NDTH   <- 0.0    
    NFIXN  <- 0.0    
    NODGR  <- 0.0    
    WTNFX  <- 0.0    
    SDWNOD <- 0.0
    SENNOD <- rep(0,20)
    
    DNOD   <- 30.0
    
    #***********************************************************************
    #***********************************************************************
    #     DAILY RATE/INTEGRATION
    #***********************************************************************
  } else if (DYNAMIC == 'INTEGR') {
    # DAS = mas(0,TIMDIF(YRSIM,YRDOY))   # TODO: Verificar de vai usar essa forma ou receber o DAS ja resolve??  Leandro 26/10/2020
    #-----------------------------------------------------------------------
    #   Set initial nodule mass to DWNODI as read from crop species file
    #-----------------------------------------------------------------------
    if (SDWNOD < 1.) {
      DWNOD  <- DWNODI * PLTPOP
      SDWNOD <- 1.0
      DWNODA <- DWNODI * PLTPOP
      WTNFX  <- DWNODA * 0.16 * PRONOD
      for (J in 1:8) {
        SWMEM[J] <- 1.0
      }
    }
    
    #-----------------------------------------------------------------------
    #   Initialize soil water and temperature factors (top DNOD cm of soil)
    #-----------------------------------------------------------------------
    SWFACT <- 1.0
    FLDACT <- 1.0
    ACSTF  <- 0.0
    ACSTG  <- 0.0
    DSW    <- 0.0
    FLDSUM <- 0.0
    #-----------------------------------------------------------------------
    #     Calculate carbon allocated per unit of nodule biomass:
    #     CNODCR = C requirement for nodule respiration (g C/g nodule/d)
    #-----------------------------------------------------------------------
    CNFACT <- 1.
    if (DWNOD > 0.) {
      FRCNM <- CTONOD/DWNOD
      if (FRCNM < CNODCR) { CNFACT = FRCNM / CNODCR }
    }
    #-----------------------------------------------------------------------
    #   Calculate soil water and temperature factors for each layer to DNOD
    #-----------------------------------------------------------------------
    # TODO: NL <- 20, LAYERFRAC <- rep(0,NL)
    LAYERFRAC <- rep(0,20)
    DSWP <- 0.0
    DNOD <- 50.0
    for (I in 1:NLAYR) {
      FLAYR <- 1.0
      DSW   <- DSW + DLAYR[I]
      if (DSW > DNOD) { FLAYR <- (DNOD-(DSW-DLAYR[I]))/DLAYR[I] }
      
      ACSTF <- ACSTF + DLAYR[I] * FLAYR * CURV(TYPFXT,FNFXT[1],FNFXT[2],FNFXT[3],FNFXT[4],ST[I])
      ACSTG <- ACSTG + DLAYR[I] * FLAYR * CURV(TYPNGT,FNNGT[1],FNNGT[2],FNNGT[3],FNNGT[4],ST[I])
      
      EPORS  <- max(SAT[I] - SW[I], 0.0)
      FLDSUM <- FLDSUM + DLAYR[I] * FLAYR * CURV(TYPFXW,FNFXW[1],FNFXW[2],FNFXW[3],FNFXW[4],EPORS)
      
      if (I == 1) {
        LAYERFRAC[1] <- DSW / DNOD
      } else {
        LAYERFRAC[I] <- (DSW - DSWP)*FLAYR / DNOD
      }
      DSWP <- DSW
      if ( FLAYR < 1.0 ) break()
      
    }
    #-----------------------------------------------------------------------
    #   Constraints due to soil water and T and average nodule age:
    #   TNFIX : soil T effect on N2 fixation
    #   TNGRO : soil T effect on nodule growth
    #   SWFACT: soil water deficit effect on N2 fixation and nodule growth
    #   FLDACT: soil water flooding effect on N2 fixation and nodule growth
    #   NFXAGE: average nodule age effect on nodule growth
    #-----------------------------------------------------------------------
    TNFIX  <- ACSTF / DNOD
    TNGRO  <- ACSTG / DNOD
    FLDACT <- FLDSUM / DNOD
    
    SWFACT <- CURV(TYPFXD,FNFXD[1],FNFXD[2],FNFXD[3],FNFXD[4],TURFAC)
    NFXAGE <- CURV(TYPFXA,FNFXA[1],FNFXA[2],FNFXA[3],FNFXA[4],DXR57)
    #-----------------------------------------------------------------------
    # DETERMINE MEMORY OF PREVIOUS EIGHT DAYS OF SOIL WATER DEFICITS
    #-----------------------------------------------------------------------
    # TODO VERIFICAR: 10,2,-1 deve ser de 10 até 2 no passo -1 (10, 9, 8, 7...) [from PODDET.for]
    for (J in seq(8, 2)) {  
      SWMEM[J] <- SWMEM[J-1]
    }
    SWMEM[1] <- SWFACT
    
    SWMEM8 <- 0.0
    for (J in 1:8) {
      SWMEM8 <- SWMEM8 + SWMEM[J]
    }
    SWMEM8 <- SWMEM8/8
    #-----------------------------------------------------------------------
    #     Reserve CNODMN for nodule growth.  JWH 7/9/95
    #-----------------------------------------------------------------------
    CLEFT <- CTONOD - CNODMN
    #-----------------------------------------------------------------------
    #    Compute Specific Nodule Activity taking into account the maximum
    #       activity of the nodules (SNACTM), and strain effects only.
    #    9/27/95 moved temp, water deficit, and soil water flooding effects
    #    below to the primary rate.  We are not getting proper stress effects.
    #-----------------------------------------------------------------------
    SNACT  <- SNACTM  * EFNFIX
    #-----------------------------------------------------------------------
    #       Compute nodule death rate as function of SW deficit, SW flooding,
    #                               and carbon deficit (chp)
    #-----------------------------------------------------------------------
    RNDTH <- NDTHMX * max((1.-FLDACT),(1.-SWFACT),(1.-CNFACT))
    NDTH  <- min(1.0,RNDTH) * DWNOD               #g/m2
    for (I in 1:NLAYR) {
      SENNOD[I] <- NDTH * LAYERFRAC[I] * 10.     #kg/ha
    }
    #-----------------------------------------------------------------------
    #    Compute N-Fixation
    #
    #-----------------------------------------------------------------------
    if (DAS < NR7) {
      PNFIXN <- min((CLEFT * 0.16 / RFIXN), (DWNOD * SNACT)) * TNFIX
      NFIXN  <- PNFIXN * min(SWFACT, SWMEM8, FLDACT)
    } else {
      PNFIXN <- 0.0
      NFIXN  <- 0.0
    }
    #-----------------------------------------------------------------------
    #    Compute C Used for N-Fixation
    #-----------------------------------------------------------------------
    PCSFIX <- (PNFIXN / 0.16) * RFIXN
    CUSFIX <- (NFIXN  / 0.16) * RFIXN
    CNOFIX <- PCSFIX - CUSFIX
    #-----------------------------------------------------------------------
    #     Compute C Left to Grow New Nodule Mass
    #     Includes minimum reserved for nodule growth (CNODMN) plus any C
    #     left after N fixation.  JWH 7/11/95
    #-----------------------------------------------------------------------
    CLEFT <- max(0.0,CLEFT - CUSFIX- 0.9*CNOFIX) + CNODMN
    #-----------------------------------------------------------------------
    #    Compute Potential Growth of Nodules (Demand)
    #    EFNFIX = strain efficiency
    #    EFINOC = inoculation effectiveness (or rhizobium density factor)
    #-----------------------------------------------------------------------
    if (DAS < NR7) {
      NODRGR <- NODRGM  * EFNFIX * EFINOC
    } else {
      NODRGR <- 0.0
    }
    #-----------------------------------------------------------------------
    #    Compute Nodule Growth, Limiting by Either Supply or Demand for C
    #-----------------------------------------------------------------------
    NODGR  <- min(CLEFT/AGRNOD,DWNOD*NODRGR) * TNGRO * min(SWFACT,FLDACT) * NFXAGE
    CNODGR <- NODGR * AGRNOD
    #-----------------------------------------------------------------------
    #    Compute C used in N-Fixation and Nodule Growth (Including
    #    Respiration Costs) Today
    #-----------------------------------------------------------------------
    CNOD <- CUSFIX + CNODGR
    
    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
  }
  #***********************************************************************
  assign("CNOD", CNOD, envir = env)
  assign("DWNOD", DWNOD, envir = env)
  assign("DWNODA", DWNODA, envir = env)
  assign("NDTH", NDTH, envir = env)
  assign("NFIXN", NFIXN, envir = env)
  assign("NODGR", NODGR, envir = env)
  assign("WTNFX", WTNFX, envir = env)
  assign("SENNOD", SENNOD, envir = env)
  assign("SDWNOD", SDWNOD, envir = env)
  assign("SWMEM", SWMEM, envir = env)
  
  return()
}
#----------------END NFIX FUNCTION---------------

#----------------RESPIR FUNCTION---------------
RESPIR <- function (DAS, PG) {
  params <- plantList$forage$params
  
  MRSWITCH  <- params$MRSWITCH
  TRSWITCH  <- params$TRSWITCH
  TRSTYP    <- params$TRSTYP
  TRST      <- params$TRST
  LFMRC     <- params$LFMRC
  mft       <- params$mft
  RTMRC     <- params$RTMRC
  SDMRC     <- params$SDMRC
  SHELMRC   <- params$SHELMRC
  STMMRC    <- params$STMMRC
  STRMRC    <- params$STRMRC
  
  #*SOYBEAN SPECIES COEFFICIENTS: CRGRO047 MODEL
  #*RESPIRATION PARAMETERS
  RES30C <- params$RES30C   #3.5E-04
  R30C2  <- params$R30C2   #.0040
  
  TS <- 24
  TRSFAC <- 0.0
  
  
  
  if ((TRSWITCH == "M") || (TRSWITCH == "m")){
    for (H in 1:24) {
      TRSFAC <- TRSFAC + 0.044+0.0019*TGRO[H]+0.001*TGRO[H]**2
    }
  } else { 
    for (H in 1:24) {
      TRSFAC <- TRSFAC+mft*CURV(TRSTYP,TRST[1],TRST[2],TRST[3], TRST[4],TGRO[H])
    }
  }
  #-----------------------------------------------------------------------
  #     Convert maintainence respiration to actual temperature. RES30C is
  #     the g CH2O/g DW/hr used in maintenance respiration at 30 C.
  #-----------------------------------------------------------------------
  #      01/17/05 SJR Convert from single maintenance respiration cost for all organs
  #                  to cost per unit crude protein for each organ
  #                  Estimate cost from Penning de Vries cost of 0.24g glucose/g protein 
  #                  then multiply by turnover rate/hour.  Use much lower turnover rate 
  #                  for storage organ than leaf.
  #      Used MRSWITCH to allow user to revert to original MR code.  
  #      IF MRSWITCH=0 then use original code, else use scheme correlating 
  #      maintenance respiration with organ protein content and turnover.  
  #      NOTE: still includes PG rate effect.
  #-----------------------------------------------------------------------
  if (MRSWITCH == "P") {
    RO <- WTNLF*6.25*LFMRC+WTNST*6.25*STMMRC+WTNRT*6.25*RTMRC+ WTNSR*6.25*STRMRC+ min((WTNSH*6.25*SHELMRC),(WTNSD*6.25*SDMRC))
    RO <- RO*TRSFAC
  } else {
    RO <- RES30C * TRSFAC
  }
  RP <- R30C2 * TRSFAC
  
  #      RP = R30C2 * 24
  
  if (MRSWITCH == "P") {
    MAINR <- RO + RP*PG
  } else{
    MAINR <- RO*WTMAIN + RP*PG
  }
  assign("MAINR", MAINR, envir = env)
  assign("RO", RO, envir = env)
  assign("RP", RP, envir = env)
  return()
}
#--------------END RESPIR FUNCTION-------------