# ---------------------------------------------------------------------
#  subroutine climanl  
# ---------------------------------------------------------------------
#  this subsroutine is only used to initialize growing degree days,
#  coldest temp, and warmest temp at very beginning - provides a
#  climate 'history' based on monthly mean values
# ---------------------------------------------------------------------
# Globals:
#
# baset
# deltat
# gdd0
# gdd0c
# gdd10
# gdd12
# gdd5
# gdd8
# gddsgcp
# ndaypm
# npoi
# pcm
# pmmin
# PPTavgann
# Tavgann
# tc
# tcmin
# tw
#
# ---------------------------------------------------------------------
climanl <- function() {
  
  # data_station$TMIN
  # data_station$TMAX
  # data_station$RAIN
  # data_station$SRAD
  # data_station$RHUM
  # data_station$WIND
  #______________________________________________________________________
  #  Find annual averages for vegetations existence
  #______________________________________________________________________
  # annual average precipitation taken from 30 year climate mean  
  assign("PPTavgann",  mean(data_station$RAIN), envir = env)
  
  # annual average minimum air temperature (deg C)
  assign("tminavgann", mean(data_station$TMIN), envir = env)

  # annual average air temperature taken from 30 year climate mean    
  assign("Tavgann",    mean((data_station$TMAX+data_station$TMIN)/2), envir = env)
  
  # to do: supostamente esse valores esta´correto, pois seria -> tcmin = tc +deltat | deltat=tmin_absuluta 
  #   coldest daily temperature of current year (C)
  assign("tcmin", min(data_station$TMIN), envir = env) #SVC set as - Absolute minimum temperature 
  
  
  
  td_m <- aggregate(data_station,by=list(month=data_station$month),FUN=mean)
  td_m$TM<-(td_m$TMAX+td_m$TMIN)/2

  # coldest monthly temperature (year 0) in deg c
  assign("tc", min(td_m$TM), envir = env)
  
  # warmest monthly temperature (year 0) in deg c
  assign("tw", max(td_m$TM), envir = env)
  

  # local dummy variables
  gdd0    <- 0 
  gdd0c   <- 0
  gdd5    <- 0
  gdd8    <- 0
  gdd10   <- 0
  gdd12   <- 0
  
# Agrega os dados de clima para dias me'dios, assim podemos fazer a media de GDD para qualquer periodo
  td_mj <- aggregate(data_station,by=list(jday=data_station$jday), FUN=mean)
  td_mj$TD <- (td_mj$TMAX+td_mj$TMIN)/2.

  i=0
  ii=0
  
  for(iimonth in 1:12) {
    
    for(iiday in 1:daypm(iimonth, 2001)) {
      
      i=i+1
      
      # PFT_UPDATE: Específico para cada tipo de planta?
      #             Se sim, deve ser alterado para realizar o calculo para cada tipo de planta.
      
      # if(pmmin[13] > pcm[13]) {
      #   if(iimonth>=pmmin[13] | iimonth<= pcm[13] ) {
      #     ii=ii+1
      #     gdd10 <- gdd10 + max(0, min(td_mj$var[i] + 273.16 - baset[13], 26))
      #   }
      # } else {
      #   if(iimonth>=pmmin[13] && iimonth<= pcm[13] ){
      #     gdd10 <- gdd10 + max(0, min(td_mj$var[i] + 273.16 - baset[13], 26))
      #   }
      # }
      # 
      # if(pmmin[14]>pcm[14]) {
      #   if(iimonth>=pmmin[14] | iimonth<= pcm[14] ){
      #     gdd8 <- gdd8 + max(0, min(td_mj$var[i] + 273.16 - baset[14], 26))
      #   }
      # } else {
      #   if(iimonth>=pmmin[14] && iimonth<= pcm[14] ){
      #     gdd8 <- gdd8 + max(0, min(td_mj$var[i] + 273.16 - baset[14], 26))
      #   }
      # }
      # 
      # if(pmmin[15]>pcm[15]){
      #   if(iimonth>=pmmin[15] | iimonth<= pcm[15] ){
      #     gdd0c <- gdd0c + max(0, min(td_mj$var[i] + 273.16 - baset[15], 26))
      #   }
      # } else {
      #   if(iimonth>=pmmin[15] && iimonth<= pcm[15] ){
      #     gdd0c <- gdd0c + max(0, min(td_mj$var[i] + 273.16 - baset[15], 26))
      #   }
      # }
      # 
      # gdd0  <- gdd0  + max(0, td_mj$var[i])
      # gdd5  <- gdd5  + max(0, td_mj$var[i] - 5.0)
      gdd12 <- gdd12 + max(0, min(td_mj$TD[i] + 273.16 - baset[1], 30))
      
    }
  }
  # PFT_UPDATE: criar um gdd para cada tipo. Susbtituir nos modelos
  gddsgcp  <- gdd12 * (mxmat[1]/365)
  gddsgcr  <- gdd12
  # gddsoym  <- gdd10
  # gddcornm <- gdd8 
  # gddwhm   <- 1.2*gdd8
  # gddwwh   <- gdd0c
  
  #  print(paste(ii,gddsoym,gddcornm,gddwhm,gddwwh,gddsgcp,gddsgcr,sep= " / "))
  
  #______________________________________________________________________
  
  # assign("gdd0", gdd0, envir = env)
  # assign("gdd5", gdd5, envir = env)
  # assign("gddsoym", gddsoym, envir = env)
  # assign("gddcornm", gddcornm, envir = env)
  # assign("gddwhm", gddwhm, envir = env)
  # assign("gddwwh", gddwwh, envir = env)
  assign("gddsgcp", gddsgcp, envir = env)
  assign("gddsgcr", gddsgcr, envir = env)
  
  environment(existence) <- env
  
  existence()
  
  
}
