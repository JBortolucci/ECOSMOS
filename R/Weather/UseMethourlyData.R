UseMethourlyData <- function(year, jday, time) {
  
  # Get variables for current time
  keyName <- as.character(paste(toString(year),toString(jday),time/3600,sep="-"))
  
  #me_current <- met_hourly[which(met_hourly$year == year & met_hourly$day == jday & met_hourly$time == time),]
  me_current <- met_hourly[keyName,]
  
  
  if(length(me_current) == 0){paste0(" NO HORLY DATA FOR  ",keyNames)
    stop()}
  
  # Calculate minimum and maximum temperature of the day
  if(time == 0) {
    
    me_day <- met_hourly[which(met_hourly$YEAR == year & met_hourly$JDAY == jday),]
    
    assign("tmin", array( min(me_day$TAIR + 273.16), 1), envir = env)
    assign("tmax", array( max(me_day$TAIR + 273.16), 1), envir = env)
    assign("td",   array(mean(me_day$TAIR + 273.16), 1), envir = env)
    
  }
  
  
  # if(time==0)print(paste0('Reading hourly data - ',year,'/', imonth,'/', jday))
  
  # rwork <- grav/rair/0.0065
  # set elevation for the arlington grid cells
  # based on elevation reading (meters) at morrisonville
  # elevm <- 552
  
  # calculate surface pressure in hPa
  assign("psurf", array(me_current$PSURF * 100, 1), envir = env)
  
  # precipitation (mm)
  assign("precip", array(me_current$RAIN * dtime / 3600), envir = env)
  
  # air temperature
  assign("ta", array(me_current$TAIR + 273.16, 1), envir = env)
  
  # solar radiation
  assign("sradh", array(max(me_current$SRAD, 0), 1), envir = env)
  
  # specific humidity
  assign("qd", array(me_current$RHUM / 100 * qsat(esat(me_current$TAIR + 273.16), psurf[1]), 1), envir = env)
  
  me_current$WIND <- max(me_current$WIND, 0.2)
  
  assign("ua", array(me_current$WIND, 1), envir = env)
  
  # TODO: fix imonth, iday, ihtime
  if(me_current$RHUM < 1)
    stop(paste("rh too low!", me_current$RHUM, "month, day, hour", imonth, iday, ihtime))
  
}

# Colunm  Variables
# 1 <-    Year
# 2 <-    Julian day
# 3 <-    hour (Min)
# 4 <-    var 1: precipitation: mm
# 5 <-    var 2: radiation: W/m2
# 6 <-    var 3: ar temperature : ºC
# 7 <-    var 4: Umidity : %
# 8 <-    var 5: Wind speed: m/s
# 9 <-    var 6: Ar pressure: hPa