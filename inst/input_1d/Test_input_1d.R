
setwd("D:\\MODELOS\\Macro-Ecosystem-Simulator\\AgroIbisR-removed_nlpoint_loops\\inst\\input_1d")



metfile <- 'met_EUCAFLUX_60min.txt'

met_hourly <- read.table(
  metfile, 
  col.names = c("year", "day", "time", 
                "var1", "var2", "var3",
                "var4", "var5", "var6"))
for (iyear in 2008:2016){
  for (jday in 11:365){
    for (itime in 1:24){
      
    time=3600*(itime-1)
      


# Get variables for current time
me_current <- met_hourly[which(met_hourly$year == iyear &
                                 met_hourly$day == jday &
                                 met_hourly$time == time),]

print(paste0(iyear,'/',jday,'/',itime))

if(me_current$var4 < 10)  stop(paste("rh too low!",iyear,jday, itime))


}
  }
}

