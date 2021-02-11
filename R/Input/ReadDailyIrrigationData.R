# Henrique & Leandro: irrigation feature [2020-11-06]
# Read irrigated water application rate (mm/day) to crop for its correspondent SimID

ReadDailyIrrigationData <- function(id, path = "UserFiles/Field/Irrigation/", instanceEnv = NULL) {

  # Making sure that at least 'irrig' = 0 is used in UseDailyStationData() 
  assign("irriON",FALSE, envir = instanceEnv)
  
  #### Read csv file for the SimID (aka id) ####
  inirrig   <- read.csv2(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
  if(ncol(inirrig) == 1) {
    inirrig <- read.csv(paste0(path,id,".csv"), header = T, stringsAsFactors = F, dec = ".")
    assign("irriON",TRUE, envir = instanceEnv)
  }
  
  # Assign to global environment
  assign("inirrig", inirrig, envir = instanceEnv)

}