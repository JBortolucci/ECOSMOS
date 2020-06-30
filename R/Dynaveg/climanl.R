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
  
  
  #______________________________________________________________________
  #  Find annual averages for vegetations existence
  #______________________________________________________________________
  # annual average precipitation taken from 30 year climate mean  
  assign("PPTavgann",  mean(inprec$var), envir = env)
  
  # annual average minimum air temperature (deg C)
  assign("tminavgann", mean(intmin$var), envir = env)
  # assign("tminavgann", 0, envir = env)
  
  intd$jday <- yday(intd$mydate)
  
  td_m <- aggregate(intd,by=list(month=intd$month),FUN=mean)
  
  # annual average air temperature taken from 30 year climate mean    
  assign("Tavgann",    mean(intd$var), envir = env)
  
  # coldest monthly temperature (year 0) in deg c
  assign("tc", min(td_m$var), envir = env)
  
  # warmest monthly temperature (year 0) in deg c
  assign("tw", max(td_m$var), envir = env)
  
  # to do: supostamente esse valores esta´correto, pois seria -> tcmin = tc +deltat | deltat=tmin_absuluta 
  #   coldest daily temperature of current year (C)
  assign("tcmin", min(intmin$var), envir = env) #SVC set as - Absolute minimum temperature 
  
  
  # local dummy variables
  gdd0    <- 0 
  gdd0c   <- 0
  gdd5    <- 0
  gdd8    <- 0
  gdd10   <- 0
  gdd12   <- 0
  
  
  td_mj <- aggregate(intd,by=list(jday=intd$jday), FUN=mean)
  
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
      gdd12 <- gdd12 + max(0, min(td_mj$var[i] + 273.16 - baset[1], 30))
      
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
