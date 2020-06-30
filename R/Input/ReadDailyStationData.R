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
  
  latc <- gsub("\\.", "", sprintf('%.3f', lat + 0.125))
  lonc <- gsub("\\.", "", sprintf('%.3f', lon - 0.125))
  
  #### Get csv filename for current coordinate ####
  file_wth_TN_nm <- paste0(path, "Xavier_Tmin_", lonc, latc, ".csv")
  file_wth_TX_nm <- paste0(path, "Xavier_Tmax_", lonc, latc, ".csv")
  file_wth_PR_nm <- paste0(path, "Xavier_prec_", lonc, latc, ".csv")
  file_wth_RS_nm <- paste0(path, "Xavier_Rs_", lonc, latc, ".csv")
  file_wth_RH_nm <- paste0(path, "Xavier_RH_", lonc, latc, ".csv")
  file_wth_U2_nm <- paste0(path, "Xavier_u2_", lonc, latc, ".csv")
  
  #### Read csv file for current coordinate ####
  intmin   <- read.csv(file_wth_TN_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(intmin$var) == 0) {
    intmin   <- read.csv(file_wth_TN_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  
  intmax   <- read.csv(file_wth_TX_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(intmax$var) == 0) {
    intmax   <- read.csv(file_wth_TX_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  
  inprec   <- read.csv(file_wth_PR_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(inprec$var) == 0) {
    inprec   <- read.csv(file_wth_PR_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  
  insrad   <- read.csv(file_wth_RS_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(insrad$var) == 0) {
    insrad   <- read.csv(file_wth_RS_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  
  inrh     <- read.csv(file_wth_RH_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(inrh$var) == 0) {
    inrh   <- read.csv(file_wth_RH_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  
  inu2     <- read.csv(file_wth_U2_nm, header = T, sep = ";", stringsAsFactors = F, dec = ".")
  if(length(inu2$var) == 0) {
    inu2   <- read.csv(file_wth_U2_nm, header = T, sep = ",", stringsAsFactors = F, dec = ".")
  }
  intd     <- intmin
  intd$var <- (intmin$var + intmax$var) / 2

  
  # Assign to global environment
  assign("intmin", intmin, envir = instanceEnv)
  assign("intmax", intmax, envir = instanceEnv)
  assign("intd", intd, envir = instanceEnv)
  assign("inprec", inprec, envir = instanceEnv)
  assign("insrad", insrad, envir = instanceEnv)
  assign("inrh", inrh, envir = instanceEnv)
  assign("inu2", inu2, envir = instanceEnv)
  
}