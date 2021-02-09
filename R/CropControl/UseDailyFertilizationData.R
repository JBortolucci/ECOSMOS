UseDailyFertilizationData <- function(day, month, year) {
  
  # Henrique & Leandro: fertilization feature [2020-11-30]
  # Nitrogen (N)
  if(inifertnitroON) {
    fertnitro <- inifertnitro[which(inifertnitro$day == day &  inifertnitro$month == month & inifertnitro$year == year),]
    fertnitro <- ifelse(as.numeric(nrow(fertnitro))==1,fertnitro$fertnitro,0)
    } else {
    fertnitro <- 0
  }

  # Assign to global environment
  assign("fertnitro", fertnitro, envir = env) # Henrique & Leandro: fertilization feature [2020-11-30]
  
}