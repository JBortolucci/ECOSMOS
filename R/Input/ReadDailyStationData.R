# Load from csv files daily station data. 

# TODO - Alterar função para ler os dados de um ano passado por parâmetro?
# Isso pode ser necessário para a simulação rodando para muitos pontos, pois pode sobrecarregar a memória

ReadDailyStationData <- function(path = "inst/input_xavier/", lat = 0, lon = 0, instanceEnv = NULL) {
  
  # if(istyear < 1980) {
  # stop(paste(
  # "year has value", year, "and is not in the Xavier data!",
  # "Xavier data starts in 1980!"
  # ))
  # }
  
  # to do: Jair, verificar se essa inicializacao e necessaria
  
  stinrad   <- c() # daily Station Solar Radiation (MJ/m2/day)
  
  latc <-  lat + 0.125
  lonc <-  lon - 0.125
  
  lons<-substr(lonc*1000,2,5)
  
  if(latc*1000>0){
    if(latc*1000<1000){lats<-paste0("N0",substr(latc*1000,1,2))}else{lats<-paste0("N",substr(latc*1000,1,3)) }}else{
    if(latc*1000>(-10*1000)){lats<-paste0("0",substr(latc*1000,2,4))}else{lats<-substr(latc*1000,2,5)}
  }
  
  wth<- as.character(sprintf("%4s%4s",lats,lons))
  wth<-gsub(' ','0',wth)
  
  file_in<- paste0(path,wth,".csv")
  

  #### Read csv file for current coordinate ####
  
  data_station<-read.csv(file_in, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  
  
  
  DOYtoDate=function(DATE)
  {
    DATE=sprintf("%05d",as.numeric(DATE))
    YEAR<-as.numeric(substring(DATE,1,2))
    YEAR[YEAR>21]<-YEAR[YEAR>21]+1900;YEAR[YEAR<=21]<-YEAR[YEAR<=21]+2000
    DOY=as.numeric(substring(DATE,3,5))
    DATE=as.Date(DOY-1, origin = paste0(YEAR,"-01-01"))
    return(DATE)
  }
  
  
  data_station$mydate   <- DOYtoDate(data_station$DATE)
  data_station$mydate   <- as.Date(data_station$mydate)
  data_station$year     <- year(data_station$mydate)
  data_station$month    <- month(data_station$mydate)
  data_station$day      <- mday(data_station$mydate)
  data_station$jday     <- yday(data_station$mydate)
  
  
  
  assign("data_station", data_station, envir = instanceEnv)
  
}