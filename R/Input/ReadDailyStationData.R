# Load from csv files daily station data. 

# TODO - Alterar função para ler os dados de um ano passado por parâmetro?
# Isso pode ser necessário para a simulação rodando para muitos pontos, pois pode sobrecarregar a memória

ReadDailyStationData <- function(path = "inst/input_xavier/", lat = 0, lon = 0, instanceEnv = NULL) {
  
  
  #### Read csv file for current coordinate ####
  
  print(paste0("Weather Station -> ",path))
  data_station<-read.csv(path, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  
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