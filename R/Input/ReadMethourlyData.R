# Read meteorology hourly data from file

ReadMethourlyData <- function(path) {
  
  # metfile <- paste0(path, "FAYS_279_2015_208_2017_Input_1hora.txt")
  metfile <- path
  #metfile <- paste0(path, "met_EUCAFLUX_60min.txt")
  
  
  met_hourly <- read.table( metfile, col.names = c("year", "day", "time", "var1", "var2", "var3", "var4", "var5", "var6"))
  
  imetyear   <- as.numeric(met_hourly[1,1])    # Year to start using hourly met data for climate
  dmetyear   <- as.numeric(met_hourly[1,2])    # Julian day to start using hourly met data for climate
  
  imetend    <- as.numeric(met_hourly[nrow(met_hourly),1])  # Year to end using hourly met data for climate
  dmetend    <- as.numeric(met_hourly[nrow(met_hourly),2])   # Julian day to end using hourly met data for climate
  
  
  keyNames <- numeric(nrow(met_hourly))
  for(i in 1:nrow(met_hourly)) {
    keyNames[i] <- paste0(toString(met_hourly[i,1]),toString(met_hourly[i,2]),toString(met_hourly[i,3]))
  }
  rownames(met_hourly) <- keyNames
  
  assign("met_hourly", met_hourly, envir = env)
  
  assign("imetyear", imetyear, envir = env)
  assign("dmetyear", dmetyear, envir = env)
  assign("imetend", imetend, envir = env)
  assign("dmetend", dmetend, envir = env)
}