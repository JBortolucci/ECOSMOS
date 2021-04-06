simDataVars$MOW        <- c()
simDataVars$RSPLF      <- c()
simDataVars$MVS        <- c() 
simDataVars$RSHT       <- c() 
simDataVars$DateMOW    <- c() 
simDataVars$DayMOW     <- c()
simDataVars$MonthMOW   <- c()
simDataVars$YearMOW    <- c()
simDataVars$fhtot      <- 0 # Henrique: added on 2020-12-18

forage_harvest <- function(day, month, year, FHTOTN, index){
  
  params <- plantList[[index]]$params
  
  # DATE     <- params$DATE
  # MOW      <- params$MOW
  # RSPLF    <- params$RSPLF
  # MVS      <- params$MVS
  # RSHT     <- params$RSHT
  PROLFF   <- params$PROLFF
  PROSTF   <- params$PROSTF
  PLIGLF   <- params$PLIGLF
  PLIGST   <- params$PLIGST
  
  #Daily Senescence
  DWTCO <- WTCO - PWTCO
  DWTLO <- WTLO - PWTLO
  DWTSO <- WTSO - PWTSO       
  # if (day == 14 )browser()
  for (I in 1:length(MOW) ){
    
    if((DayMOW[I] == day) & (MonthMOW[I] == month) & (YearMOW[I] == year)) {
      if ((MOW[I] >= 0)) {
        if(MOW[I]/10 < TOPWT) {
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
          FHLEAF <- max(FHLEAF,0.0)
          FHSTEM <- max(FHSTEM,0.0)
          FHVSTG <- max(MVS[I],0.0)
          CANHT  <- max(RSHT[I]/100,0.0)
          #              canht=max(rsht[i],0.0)     #enter rsht in cm
          
          fhtot <- FHLEAF+FHSTEM
          fhlfn <- FHLEAF*PCNL/100
          fhstn <- FHSTEM*PCNST/100
          fhtotn <- fhlfn+fhstn
          fhcrlf <- FHLEAF*RHOL
          fhcrst <- FHSTEM*RHOS
          
          fhpctn <- fhtotn/fhtot*100
          fhplig <- (FHLEAF*PLIGLF+FHSTEM*PLIGST)/fhtot*100
          fhpcho <- (fhcrlf+fhcrst)/fhtot*100
          fhpctlf <- FHLEAF/fhtot*100
          
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
            WNRLF <- max(WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
          } else {
            WNRLF <- 0.0
          }
          
          if ((STMWT - WCRST) > 0.0) {
            WNRST <- max(WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
          } else {
            WNRST <- 0.0
          }
          
          AREALF <- WTLF*SLA
          XLAI   <- AREALF/10000.
          XHLAI  <- XLAI
          
          VSTAGE <- FHVSTG     
          VSTAGP <- VSTAGE
          
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
        
        if((DayMOW[I] == day) & (MonthMOW[I] == month) & (YearMOW[I] == year)) {
          PWTCO <- WTCO 
          PWTLO <- WTLO
          PWTSO <- WTSO
          DWTCO <- WTCO - PWTCO
          DWTLO <- WTLO - PWTLO
          DWTSO <- WTSO - PWTSO
        }
      }
    }
  }
  assign("WTLF",   WTLF, envir = env)
  assign("STMWT",  STMWT, envir = env)
  assign("TOPWT",  TOPWT, envir = env)
  assign("TOTWT",  TOTWT, envir = env)
  assign("WCRLF",  WCRLF, envir = env)
  assign("WCRST",  WCRST, envir = env)
  assign("WTNLF",  WTNLF, envir = env)
  assign("WTNST",  WTNST, envir = env)
  assign("WNRLF",  WNRLF, envir = env)
  assign("WNRST",  WNRST, envir = env)
  assign("WTNCAN", WTNCAN, envir = env)
  assign("AREALF", AREALF, envir = env)
  assign("XLAI",   XLAI, envir = env)
  assign("XHLAI",  XHLAI, envir = env)
  assign("VSTAGE", VSTAGE, envir = env)
  assign("VSTAGP", VSTAGP, envir = env)
  assign("DWTCO",  DWTCO, envir = env)
  assign("DWTLO",  DWTLO, envir = env)
  assign("DWTSO",  DWTSO, envir = env)
  assign("PWTCO",  PWTCO, envir = env)
  assign("PWTLO",  PWTLO, envir = env)
  assign("PWTSO",  PWTSO, envir = env)
  assign("CANHT",  CANHT, envir = env)
  # assign("FHLEAF",FHLEAF , envir = env)
  # assign("FHSTEM",FHSTEM , envir = env)
  # assign("FHVSTG",FHVSTG , envir = env)
  assign("fhtot",fhtot , envir = env) # Harvested forage weight (leaf + stem) at forage harvest (kg/ha) (aka HERB in PlantGro.out or FHWAH in FORAGE.out) [2020-12-18]
  return()
}

ReadForageHarvData <- function(id, path = "UserFiles/Field/Mowing/",  simInstances = NULL) {
  
  # path = "UserFiles/Field/Mowing/BRSPCI_Rainfed_2018-2019.csv"
  
  #### Read csv file for the SimID (aka id) ####
  MowData   <- read.csv2(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
  # MowData   <- read.csv2(path, header = T, stringsAsFactors = F, dec = ".")
  if(ncol(MowData) == 1) {
    MowData <- read.csv(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
    # MowData <- read.csv(path, header = T, stringsAsFactors = F, dec = ".")
  }
  
  MOW       <- MowData$MOW
  RSPLF     <- MowData$RSPLF
  MVS       <- MowData$MVS  
  RSHT      <- MowData$RSHT 
  DateMOW   <- MowData$date 
  DayMOW    <- MowData$day
  MonthMOW  <- MowData$month
  YearMOW   <- MowData$year
  
  # Assign to global environment
  assign("MOW", MOW     , envir = simInstances)
  assign("RSPLF", RSPLF   , envir = simInstances)
  assign("MVS", MVS     , envir = simInstances)
  assign("RSHT", RSHT    , envir = simInstances)
  assign("DateMOW", DateMOW , envir = simInstances)
  assign("DayMOW", DayMOW  , envir = simInstances)
  assign("MonthMOW", MonthMOW, envir = simInstances)
  assign("YearMOW", YearMOW , envir = simInstances)  
}