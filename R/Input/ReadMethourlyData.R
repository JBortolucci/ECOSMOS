# Read meteorology hourly data from file

ReadMethourlyData <- function(path) {
  
  met_hourly <-  read.csv(file = path, header = T, stringsAsFactors = F, sep = ",")
  
  
  imetyear   <- as.numeric(met_hourly$YEAR[1])    # Year to start using hourly met data for climate
  dmetyear   <- as.numeric(met_hourly$JDAY[1])    # Julian day to start using hourly met data for climate
  
  imetend    <- as.numeric(met_hourly$YEAR[nrow(met_hourly)])  # Year to end using hourly met data for climate
  dmetend    <- as.numeric(met_hourly$JDAY[nrow(met_hourly)])   # Julian day to end using hourly met data for climate
  
  
  keyNames <- as.character(numeric(nrow(met_hourly)))
  
  for(i in 1:nrow(met_hourly)) {
    keyNames[i] <- as.character(paste(toString(met_hourly$YEAR[i]),toString(met_hourly$JDAY[i]),toString(met_hourly$HOUR[i]),sep="-"))
  }
  
  rownames(met_hourly) <- keyNames
  
  assign("met_hourly", met_hourly, envir = env)
  
  assign("imetyear", imetyear, envir = env)
  assign("dmetyear", dmetyear, envir = env)
  assign("imetend", imetend, envir = env)
  assign("dmetend", dmetend, envir = env)
  
}