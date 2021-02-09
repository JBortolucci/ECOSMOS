# Henrique & Leandro: fertilization feature [2020-11-30]
# Read fertilization rates (kg Nutrient/day) to crop for its correspondent SimID

ReadDailyFertilizationData <- function(id, path = "UserFiles/Field/Fertilization/", instanceEnv = NULL) {

  assign("inifertnitroON",FALSE, envir = instanceEnv)
  
  #### Read csv file for the SimID (aka id) ####
  # Nitrogen (N)
  inifertnitro <- read.csv2(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
  if(ncol(inifertnitro) == 1) {
    inifertnitro <- read.csv(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
  }
  
  # Assign to global environment
  assign("inifertnitroON",TRUE, envir = instanceEnv)
  assign("inifertnitro", inifertnitro, envir = instanceEnv)

}