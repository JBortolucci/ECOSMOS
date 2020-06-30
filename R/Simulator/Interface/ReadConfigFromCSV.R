#' ReadConfigFromCSV
#'
#' Função que lê de um arquivo csv (formato padrão), as configurações da simulação.
#' Retorna uma lista contendo as informações nomeadas.
#'
#' @param filePath Caminho do arquivo .CSV
#'
#' @examples
#' environment(VariablesInicialization) <- simVars
#' ret <- ReadConfigFromCSV("R/Interface/Input/Template_Simulation_Control_ECOSMOS.csv")

ReadConfigFromCSV <- function(filePath) {

  
  simulationList <- list()
  curData <- 1
  
  data <- read.csv(file = filePath, header = T, stringsAsFactors = F ,sep = ",")
  
  atualData <- data[curData,]
  aux <- 10
  
  repeat {
    
    sim <- list()
    sim$id          <- atualData[[1]]
    sim$coord       <- list(lat = lat  <- atualData[[2]], 
                            lon = lon  <- atualData[[3]])
    
    sim$startYear   <- atualData[[4]]
    sim$endYear     <- atualData[[5]]
    
    sim$soilId      <- atualData[[6]]
    sim$stationID   <- atualData[[7]]
    sim$soilcspin   <- atualData[[8]]
    sim$isimveg     <- atualData[[9]]
    sim$isimfire    <- atualData[[10]]
    sim$irrigate    <- atualData[[11]]
    sim$npft        <- 0
    
    for (i in seq(from = 12, to = length(atualData), by = 6)) {
      if(!is.na(atualData[[i]]) && !atualData[[i]] == "") {
        plant <- list()
        plant$name         <- atualData[[i]]
        plant$plantJday    <- atualData[[i+1]]
        plant$startYear    <- atualData[[i+2]]
        plant$cycleLength  <- atualData[[i+3]]
        plant$ncycles      <- atualData[[i+4]]
        plant$params       <- atualData[[i+5]]
        sim[paste("plant", (sim$npft+1), sep = "")] <- list(plant)
        sim$npft <- sim$npft + 1 
      }
    }
    
    simulationList[paste("simulation",curData, sep="")]  <- list(sim)
    curData <- curData + 1
    atualData <- data[curData,]
    if (is.na(atualData[[1]])) break()
  }
  
  return(simulationList)
}