UseDailyFertilizationData <- function(day, month, year, index) {
  
  # Henrique & Leandro: fertilization feature [2020-11-30]
  # Nitrogen (N)
  if(inifertnitroON) {
    fertnitroAux <- inifertnitro[which(inifertnitro$day == day &  inifertnitro$month == month & inifertnitro$year == year),]
    fertnitro[index] <- ifelse(as.numeric(nrow(fertnitroAux))==1,fertnitroAux$fertnitro,0)
  } 
  
  # Assign to global environment
  assign("fertnitro", fertnitro, envir = env) # Henrique & Leandro: fertilization feature [2020-11-30]
  
}