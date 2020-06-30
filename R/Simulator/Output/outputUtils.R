createOutput <- function(simVars) {
  simVars$HOURLY                  <- 1      # Tipo de output Horário
  simVars$NO_HOURLY               <- 0      # Tipo de output Diário
  simVars$oHour                   <- 1      # Contador de Horas 
  simVars$outputHourlyList        <- getOutputList("R/Simulator/variablesForHourlyOutput.txt")
  simVars$outputDailyList         <- getOutputList("R/Simulator/variablesForDailyOutput.txt")
  getHourlyVariables(simVars, simVars$outputHourlyList)
  getDailyVariables(simVars, simVars$outputDailyList)
  simVars$outputCHourly           <- list()
  simVars$outputCDaily            <- list()
  simVars$outputCHourlyDataFrame  <- data.frame()
  simVars$outputCDailyDataFrame   <- data.frame()
  simVars$outputHourlyFileName    <-paste("output/", simVars$config$id, "_Hourly.rds", sep = "")
  simVars$outputDailyFileName     <-paste("output/", simVars$config$id, "_Daily.rds", sep = "")
  if(file.exists(simVars$outputHourlyFileName))
    file.remove(simVars$outputHourlyFileName)
  if(file.exists(simVars$outputDailyFileName))
    file.remove(simVars$outputDailyFileName)
}

getVariables <- function(simVars, outputNameVars) {
  nomes <- c("Date","Hour")
  for (nome in outputNameVars) {
    if(is.null(simVars[[nome]])){
      stop(paste("Variável ", nome, " desconhecida"))
    }
    if(length(simVars[[nome]]) > 1){
      if(is.matrix(simVars[[nome]])){
        for(i in 1:nrow(simVars[[nome]])){
          for(j in 1:ncol(simVars[[nome]])){
            nomes <- c(nomes, paste(nome,"[",i,",",j,"]",sep = ""))
          }
        }
      } else {
        for(a in 1:length(simVars[[nome]])){
          nomes <- c(nomes, paste(nome,"[",a,"]",sep = ""))
        }
      }
    } else {
      nomes <- c(nomes, nome)
    } 
  }
  variables <- c(nomes)
  return(variables)
}

getHourlyVariables <- function(simVars, hourlyOutputList) {
  hourlyVariables          <- getVariables(simVars, hourlyOutputList)
  assign("hourlyVariables", hourlyVariables, envir = simVars$env)
}

getDailyVariables <- function(simVars, dailyOutputList) {
  dailyVariables          <- getVariables(simVars, dailyOutputList)
  if ("Hour" %in% dailyVariables){
    dailyVariables <- dailyVariables[-match("Hour", dailyVariables)]
  }
  assign("dailyVariables", dailyVariables, envir = simVars$env)
}

initializeMeansVariables <- function (simVars, nameVars){
  for(variavel in nameVars){
    simVars[[paste(variavel, "Mean", sep = '')]] <- 0
  }
}

dataFrameGenerator <- function(simVars) {
  if (!is.null(simVars$outputHourlyList)) {
    simVars$outputCHourlyDataFrame <- do.call(rbind, simVars$outputCHourly)
    colnames(simVars$outputCHourlyDataFrame) <- simVars$hourlyVariables
    simVars$outputCHourlyDataFrame <- as.data.frame(simVars$outputCHourlyDataFrame, stringsAsFactors = FALSE)
    appendToFile(simVars$outputCHourlyDataFrame, simVars$outputHourlyFileName)
  }
  
  
  if (!is.null(simVars$outputDailyList)) {
    simVars$outputCDailyDataFrame <- do.call(rbind, simVars$outputCDaily)
    colnames(simVars$outputCDailyDataFrame) <- simVars$dailyVariables
    simVars$outputCDailyDataFrame <- as.data.frame(simVars$outputCDailyDataFrame, stringsAsFactors = FALSE)
    appendToFile(simVars$outputCDailyDataFrame, simVars$outputDailyFileName)
  }
  resetOutput(simVars)
}

appendToFile <- function(newRow, savedFile){
  if(file.exists(savedFile)){
    df <- readRDS(savedFile)
    df <- do.call("rbind", list(df, newRow))
    saveRDS(df, file = savedFile)
  } else {
    saveRDS(newRow, file = savedFile)
  }
}

resetOutput <- function(simVars) {
  simVars$outputCHourly           <- list()
  simVars$outputCDaily            <- list()
  simVars$outputCHourlyDataFrame  <- data.frame()
  simVars$outputCDailyDataFrame   <- data.frame()
}