# ---------------------------------------------------------------------
# subroutine methourly (iyear, jday, time, seed)
# ---------------------------------------------------------------------
# initializes surface meteorology from hourly data  
# avoids the need to go through much of subroutine 'daily' in weather.f 
# ---------------------------------------------------------------------
# Globals:
#
# dmetyear
# dtime
# grav
# imetyear
# npoi
# precip
# psurf
# qd
# rair
# ta
# td
# tmax
# tmin
# ua
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# initializes surface meteorology from hourly data  
# avoids the need to go through much of subroutine 'daily' in weather.f 
# ---------------------------------------------------------------------

UseMethourlyData <- function(year, jday, time) {
  
  # Get variables for current time
  keyName <- paste0(toString(year),toString(jday),toString(time))
  #me_current <- met_hourly[which(met_hourly$year == year & met_hourly$day == jday & met_hourly$time == time),]
  me_current <- met_hourly[keyName,]
  
  if(length(me_current) == 0)
    stop("")
  
  # Calculate minimum and maximum temperature of the day
  if(time == 0) {
    
    me_day <- met_hourly[which(met_hourly$year == year & met_hourly$day == jday),]
    
    assign("tmin", array( min(me_day$var3 + 273.16), 1), envir = env)
    assign("tmax", array( max(me_day$var3 + 273.16), 1), envir = env)
    assign("td", array(mean(me_day$var3 + 273.16), 1), envir = env)
    
    # csant - chamando com os dados da estacao mesmo, verificar depois o impacto  - dailymet(imonth, iday, seed, jday)
  }
  
  # if(time==0)print(paste0('Reading hourly data - ',year,'/', imonth,'/', jday))
  
  # rwork <- grav/rair/0.0065
  # set elevation for the arlington grid cells
  # based on elevation reading (meters) at morrisonville
  # elevm <- 552
  
  # calculate surface pressure in hPa
  assign("psurf", array(me_current$var6 * 100, 1), envir = env)
  
  # precipitation (mm)
  assign("precip", array(me_current$var1 * dtime / 3600), envir = env)
  
  # air temperature
  assign("ta", array(me_current$var3 + 273.16, 1), envir = env)
  
  # insolation
  assign("cloud", array(max(me_current$var2, 0), 1), envir = env)
  
  # specific humidity
  # usar psurf ou psurfi? usar psurf
  assign("qd", array(me_current$var4 / 100 * qsat(esat(me_current$var3 + 273.16), psurf[1]), 1), envir = env)
  
  me_current$var5 <- max(me_current$var5, 0.2)
  
  assign("ua", array(me_current$var5, 1), envir = env)
  
  # TODO: fix imonth, iday, ihtime
  if(me_current$var4 < 1)
    stop(paste("rh too low!", me_current$var4, "month, day, hour", imonth, iday, ihtime))
  
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