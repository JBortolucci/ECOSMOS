simDataVars$CDEBIT  <-  0

CH2OREF <- function(DYNAMIC, PG, PGAVL){
  
  params <- plantList$forage$params
  
  TYPCREF   <- params$TYPCREF
  TYPLREF   <- params$TYPLREF
  TYPPREF   <- params$TYPPREF 
  FILEC     <- params$FILEC
  FILEE     <- params$FILEE
  PATHEC    <- params$PATHEC
  ALPHL     <- params$ALPHL
  ALPH      <- params$ALPH
  ALPHS     <- params$ALPHS
  ALPHR     <- params$ALPHR
  ALPHSH    <- params$ALPHSH
  ALPHSR    <- params$ALPHSR
  PHTMAX    <- params$PHTMAX
  CRREF     <- params$CRREF
  LRREF     <- params$LRREF
  PRREF     <- params$PRREF 
  LMXSTD    <- params$LFMAX # TODO: parametro com nome diferente na hjora da leitura LMXSTD = LFMAX
  CADPV     <- params$CADPV
  PGREF     <- params$PGREF
  PCHOLFF   <- params$PCHOLFF
  PCHORTF   <- params$PCHORTF
  PCHOSRF   <- params$PCHOSRF
  PCHOSTF   <- params$PCHOSTF
  
  #***********************************************************************
  #     Run Initialization - Called once per simulation
  #***********************************************************************
  
  if (DYNAMIC == "RUNINIT") {
    
    #-----------------------------------------------------------------------
    #     Initialize Refill variables
    #-----------------------------------------------------------------------
    CDEBIT  <- 0.0
    LFCDEBT <- 0.0
    STCDEBT <- 0.0
    RTCDEBT <- 0.0
    SRCDEBT <- 0.0
    CADVG   <- 0.0
    
    #***********************************************************************
    #     Daily Rate Calculations 
    #***********************************************************************
  } else if (DYNAMIC == "RATE") {
    #***********************************************************************
    #     Daily Integration 
    #***********************************************************************
  } else if (DYNAMIC == "INTEGR") {
    CDEBIT  <- 0.0
    LFCDEBT <- 0.0
    STCDEBT <- 0.0
    RTCDEBT <- 0.0
    SRCDEBT <- 0.0
    CADVG   <- 0.0
    #-----------------------------------------------------------------------
    #     Calculate current mobile CH2O status and deficit
    #-----------------------------------------------------------------------
    #      02/01/06 SJR Modified calculations of CSTATUS and xxCCAP for CH2O 
    #      mobilization and loss from natural senescence
    #-----------------------------------------------------------------------
    #-----------------------------------------------------------------------
    #      Max CH2O capacity for leaves is ALPHL * today's leaf mass without CH2O
    #      Today's leaf mass without CH2O is:
    #      WTLF minus today's DM lost to natural senescence minus today's CH2O content
    #      DM lost to natural senescence is SLMDOT corrected for N and CH2O 
    #      mobilized prior to abscission (LFSNMOB and LFSCMOB).
    #      Leaf CH2O content is WCRLF adjusted for CH2O lost with SLMDOT (SLMDOT*PCHOLFF)
    #      CH2O REFILL capacity is the Max CH2O capacity described above minus 
    #      today's CH2O content.
    #-----------------------------------------------------------------------
    LFMCCAP <- ALPHL * (WTLF - SLDOT - WCRLF)
    STMCCAP <- ALPHS * (STMWT - SSDOT - WCRST)
    RTMCCAP <- ALPHR * (RTWT - SRDOT - WCRRT)
    SRMCCAP <- ALPHSR * (STRWT  - SSRDOT - WCRSR)
    #-------------------------------------------------------------------
    #  PDA 6/3/2010 Corrected math so that ALPHX is actually the maximum
    #-------------------------------------------------------------------
    #      LFMCCAP = (ALPHL/(1-ALPHL)) * (WTLF - SLDOT - WCRLF)
    
    #      STMCCAP = (ALPHS/(1-ALPHS)) * (STMWT - SSDOT - WCRST)
    
    #      RTMCCAP = (ALPHR/(1-ALPHR)) * (RTWT - SRDOT - WCRRT)
    
    #      SRMCCAP = (ALPHSR/(1-ALPHSR)) * (STRWT  - SSRDOT - WCRSR)
    #-------------------------------------------------------------------
    
    #      SRMCCAP = ALPHSR * (STRWT  - SSRMDOT + SRSCMOB + SRSNMOB / 0.16 
    #               &                         - WCRSR - SSRMDOT * PCHOSRF)
    #-----------------------------------------------------------------------
    # Status is current CH2O concentration as a proportion of Max CH2O capacity
    #-----------------------------------------------------------------------
    CSTATUS <- (WCRLF + WCRRT + WCRSR + WCRST  - (SLDOT * WCRLF/RTWT - LFSCMOB) - (SSDOT * WCRST/STMWT - STSCMOB) - (SRDOT * WCRRT/RTWT - RTSCMOB) - (SSRDOT * WCRSR/STRWT - SRSCMOB)) / (LFMCCAP + STMCCAP + RTMCCAP + SRMCCAP)
    #-----------------------------------------------------------------------
    #      CH2O REFILL capacity is the Max CH2O capacity described above minus 
    #      today's CH2O content.
    #-----------------------------------------------------------------------
    LFCCAP <- LFMCCAP - WCRLF - (SLDOT * WCRLF/WTLF - LFSCMOB)
    if (LFCCAP <= 0.0) LFCCAP <- 0.0
    STCCAP <- STMCCAP - WCRST - (SSDOT * WCRST/STMWT - STSCMOB)
    if (STCCAP <= 0.0) STCCAP <- 0.0
    RTCCAP <- RTMCCAP - WCRRT - (SRDOT * WCRRT/RTWT - RTSCMOB)
    if (RTCCAP <= 0.0) RTCCAP <- 0.0
    SRCCAP <- SRMCCAP - WCRSR - (SSRDOT * WCRSR/STRWT - SRSCMOB)
    if (SRCCAP <= 0.0) SRCCAP <- 0.0
    #      CDEBIT = (ALPHL * WTLF + ALPHR * RTWT + ALPHS * STMWT + 
    #     &          ALPHSR *STRWT) - (WCRLF + WCRRT + WCRSR + WCRST)
    CDEBIT <- LFCCAP + STCCAP + RTCCAP + SRCCAP
    CDEBIT <- max(CDEBIT, 0.0)
    
    if (CDEBIT > 0.0) {
      LFCDEBT = LFCCAP / CDEBIT
      if (LFCDEBT <= 0.0) LFCDEBT = 0.0
      
      STCDEBT = STCCAP / CDEBIT
      if (STCDEBT <= 0.0) STCDEBT = 0.0
      
      RTCDEBT = RTCCAP / CDEBIT
      if (RTCDEBT <= 0.0) RTCDEBT = 0.0
      
      SRCDEBT = SRCCAP / CDEBIT
      if (SRCDEBT <= 0.0) SRCDEBT = 0.0
      
    } else { 
      LFCDEBT <- 0.0
      STCDEBT <- 0.0
      RTCDEBT <- 0.0
      SRCDEBT <- 1.0
    }
    
    
    #-----------------------------------------------------------------------
    #     Calculate mobile CH2O refill rate modifiers
    #-----------------------------------------------------------------------
    PGLFMX    <- (1. - exp(-1.6 * LMXSTD)) / (1. - exp(-1.6 * PGREF))
    PGSTATUS  <- PG / PHTMAX * PGLFMX 
    #      PGSTATUS = PG / PHTMAX * PGLFMX
    CFILL <- CURV(TYPCREF,CRREF[1],CRREF[2],CRREF[3],CRREF[4],CSTATUS)
    LFILL <- CURV(TYPLREF,LRREF[1],LRREF[2],LRREF[3],LRREF[4],XLAI)
    PFILL <- CURV(TYPPREF,PRREF[1],PRREF[2],PRREF[3],PRREF[4],PGSTATUS)
    #-----------------------------------------------------------------------
    #     Calculate amount of PGAVL allocated to mobile CH2O status refill
    #-----------------------------------------------------------------------
    CADVG <- CADPV * CFILL * LFILL * PFILL * min(PG,PGAVL)
    CADVG <- min(CADVG, CDEBIT)
    #***********************************************************************
    #     OUTPUT
    #***********************************************************************
  } else if (DYNAMIC == "OUTPUT") {
    #***********************************************************************
    #     SEASEND
    #***********************************************************************
  } else if (DYNAMIC == "SEASEND") {
  }
    
    assign("CDEBIT",CDEBIT , envir = env)
    assign("CADVG",CADVG , envir = env)
    assign("LFCDEBT",LFCDEBT, envir = env)
    assign("RTCDEBT",RTCDEBT, envir = env)
    assign("SRCDEBT",SRCDEBT, envir = env)
    assign("STCDEBT",STCDEBT, envir = env)
  return()
}
# CADPV    Maximum fraction of PGAVL for vegetative growth that can be 
#            allocated to mobile carbohydrate storage under non-stress   
#            conditions during vegetative growth stages. ( fraction)
# CADVG        CH2O to be reserved today for CH2O refill of vegetative 
#                  organs g [CH2O] m-2 d-1
# CDEBIT        Mobile CH2O deficit relative to mobile CH2O content 
#                  if plant was all "new" tissue
# CFILL        CH2O status effect on routine mobile CH2O refill rate 
# CRREF(4)  CURV response of routine refill of mobile CH2O during 
#                  vegetaive stages to current mobile CH2O status
# CSTATUS        Mobile CH2O content relative to mobile CH2O content 
#                  if plant were all "new" tissue
# LFCCAP    Leaf mobile CH2O deficit relative to mobile CH2O content 
#                  if leaf was all "new" tissue
# LFCDEBT        Proportion of total plant CH2O "deficit" attributed to leaves
# LFILL        LAI effect on routine mobile CH2O refill rate 
# LFMCCAP        Maximum CH2O content of leaves if tissues are at ALPHL concentration of CH2O
# LRREF(4)  CURV response of routine refill of mobile CH2O during 
#                  vegetaive stages to current LAI
# PCH2OLFF Proportion of mobile CH2O remaining in senesced leaf tissue
#             g [CH2O] / g [senesced leaf DM]
# PCH2ORTF Proportion of mobile CH2O remaining in senesced root tissue
#             g [CH2O] / g [senesced root DM]
# PCH2OSRF Proportion of mobile CH2O remaining in senesced STOR tissue
#             g [CH2O] / g [senesced STOR DM]
# PCH2OSTF Proportion of mobile CH2O remaining in senesced stem tissue
#             g [CH2O] / g [senesced leaf DM]
# PFILL        Canopy PG effect on routine mobile CH2O refill rate 
# PHTMAX   Maximum amount of CH20 which can be produced if 
#            photosynthetically active radiation (PAR) is very high (3 
#            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
# PRREF(4)  CURV response of routine refill of mobile CH2O during 
#                  vegetaive stages to today's canopy PG rate
# RTCCAP    Root mobile CH2O deficit relative to mobile CH2O content 
#                  if root was all "new" tissue
# RTCDEBT        Proportion of total plant CH2O "deficit" attributed to roots
# RTMCCAP        Maximum CH2O content of roots if tissues are at ALPHR concentration of CH2O
# SLMDOT    Defoliation due to daily leaf senescence that is lost at PROLFF, 
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SRCCAP    STOR mobile CH2O deficit relative to mobile CH2O content 
#                  if STOR was all "new" tissue
# SRCDEBT        Proportion of total plant CH2O "deficit" attributed to STOR
# SRMCCAP        Maximum CH2O content of STOR if tissues are at ALPHSR concentration of CH2O
# SRMDOT    Daily root senescence that is lost at PRORTF, 
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SSMDOT    Daily petiole senescence that is lost at PROSTF, 
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# SSRMDOT   Daily STOR senescence that is lost at PROSRF, 
#              hence, some fraction of the N content is subject to mobilization (g/m2/day)
# STCCAP    Stem mobile CH2O deficit relative to mobile CH2O content 
#                  if stem was all "new" tissue
# STCDEBT        Proportion of total plant CH2O "deficit" attributed to stem
# STMCCAP        Maximum CH2O content of Stems if tissues are at ALPHS concentration of CH2O
# TYPCREF        Shape of CURV response for refilling mobile CH2O to CH2O status
# TYPLREF        Shape of CURV response for refilling mobile CH2O to LAI
# TYPPREF        Shape of CURV response for refilling mobile CH2O to canopy PG rate
