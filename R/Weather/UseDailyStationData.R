# ---------------------------------------------------------------------
# subroutine rdstation2d(iday, imonth, iyear,iyear0 )
# ---------------------------------------------------------------------
# This subroutine reads in daily fields..
# ---------------------------------------------------------------------
# Globals:
#
# ndaypm
# npoi
# stinprecd
# stinqd
# stinrad
# stintd
# stintmax
# stintmin
# stinwindd
# --------------------------------------------------------------------

# Use daily station data. 
# Get the value by passing day, month and year.

UseDailyStationData <- function(day, month, year) {
  
 
                                      useYear <-   year
  if (year < min(data_station$year)) {useYear <-  min(data_station$year)}
  if (year > max(data_station$year)) {useYear <-  max(data_station$year)}
  
  useDay <- day
  if((year < min(data_station$year) || year > max(data_station$year)) && month == 2 && day == 29) {   useDay <- 28  } #Aplicável somente para os anos onde não temos dados de clima

  
  
  TN <- data_station$TMIN[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  TX <- data_station$TMAX[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  PR <- data_station$RAIN[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  RS <- data_station$SRAD[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  RH <- data_station$RHUM[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  U2 <- data_station$WIND[which(data_station$day == useDay &  data_station$month == month  & data_station$year == useYear)]
  
  
  if( is.na(TN)==T |  is.na(TX)==T | is.na(PR)==T | is.na(RS)==T | is.na(RH)==T | is.na(U2)==T ) { 
            print(paste0("No daily weather file ",day,"/",month,"/",year))
              stop()  }
  
  #### Set variable value for current point ####
  tmax    <- TX + 273.16
  tmin    <- TN + 273.16
  td      <- (tmax + tmin) / 2 
  precip  <- PR
  stinrad <- RS  
  ud      <- U2
  qd      <-  min (0.99,max (0.05, RH/100))

  
  # convert from relative humidity to specific humidity at
  # daily mean temperature

  psurf  <- 101325 * (td / (td + 0.0065 * xintopo)) ** (grav / rair / 0.0065)
  
  qd <- qd * qsat(esat(td), psurf)
  
  # Henrique & Leandro: irrigation feature [2020-11-06]
  if(irriON) {
    irrig <- inirrig[which(inirrig$day == useDay &  inirrig$month == month & inirrig$year == useYear),]
    irrig <- ifelse(as.numeric(nrow(irrig))==1,irrig$irrig,0)
    } else {
    irrig <- 0
  }
  precip <- precip + irrig

  # Assign to global environment
  assign("tmax", tmax, envir = env)
  assign("tmin", tmin, envir = env)
  assign("td", td, envir = env)
  assign("precip", precip, envir = env)
  assign("stinrad",   stinrad,   envir = env)
  assign("ud", ud, envir = env)
  assign("qd", qd, envir = env)
  assign("psurf", psurf, envir = env)
  assign("irrig", irrig, envir = env) # Henrique & Leandro: irrigation feature [2020-11-06]
  
}