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
  
  useDay <- day
  if((year < 1980 || year > 2015) && month == 2 && day == 29) {
    useDay <- 28
  }
  
  #### Retrieve variable value for current time from file ####
  
  useYear <- ifelse(year >= 1980 && year <= 2018, year, 2015) # Henrique: alterado de 2015 p/ 2018  para ler os dados medidos nas estações dos exp da soja (25/9/2020)
  
  
  TN <- intmin[which(intmin$day == useDay &  intmin$month == month & intmin$year == useYear),]
  TX <- intmax[which(intmax$day == useDay & intmax$month == month  & intmax$year == useYear),]
  PR <- inprec[which(inprec$day == useDay & inprec$month == month  & inprec$year == useYear),]
  RS <- insrad[which(insrad$day == useDay & insrad$month == month  & insrad$year == useYear),]
  RH <- inrh[which(inrh$day == useDay     & inrh$month == month    & inrh$year == useYear),]
  U2 <- inu2[which(inu2$day == useDay     & inu2$month == month    & inu2$year == useYear),]
  
  #### Set variable value for current point ####
  tmax    <- TX$var + 273.16
  tmin    <- TN$var + 273.16
  td      <- (tmax + tmin) / 2 
  precip  <- PR$var
  stinrad <- RS$var  
  ud      <- U2$var
  qd     <-  min (0.99,max (0.05, RH$var/100))
  
  # convert from relative humidity to specific humidity at
  # daily mean temperature

  psurf  <- 101325 * (td / (td + 0.0065 * xintopo)) ** (grav / rair / 0.0065)
  
  qd <- qd * qsat(esat(td), psurf)

  # Assign to global environment
  assign("tmax", tmax, envir = env)
  assign("tmin", tmin, envir = env)
  assign("td", td, envir = env)
  assign("precip", precip, envir = env)
  assign("stinrad",   stinrad,   envir = env)
  assign("ud", ud, envir = env)
  assign("qd", qd, envir = env)
  assign("psurf", psurf, envir = env)
  
  
}