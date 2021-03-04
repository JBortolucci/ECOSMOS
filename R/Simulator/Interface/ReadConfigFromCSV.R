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
    sim$id                  <- atualData[[1]]
    sim$coord               <- list(lat = lat  <- atualData[[2]], 
                                    lon = lon  <- atualData[[3]])
    # Henrique & Leandro
    # 1) foram inseridos DOYs para controle da janela de simulação [2020-10-XX]
    sim$startYear           <- atualData[[4]]
    sim$startOfSimulation   <- atualData[[5]]
    sim$endYear             <- atualData[[6]]
    sim$endOfSimulation     <- atualData[[7]]
            
    sim$soilId              <- atualData[[8]]
    sim$stationID           <- atualData[[9]]
    # 2) flag para uso ou não da funcionalidade de 'initial conditions' [2020-11-04]
    sim$soilic              <- atualData[[10]]
    
    sim$soilcspin           <- atualData[[11]]
    sim$isimveg             <- atualData[[12]]
    sim$isimfire            <- atualData[[13]]
    sim$irrigate            <- atualData[[14]]
    sim$npft                <- 0
    
    for (i in seq(from = 15, to = length(atualData), by = 9)) {
      if(!is.na(atualData[[i]]) && !atualData[[i]] == "") {
        plant <- list()
        plant$name          <- atualData[[i]]
        plant$plantJday     <- atualData[[i+1]]
        plant$startYear     <- atualData[[i+2]]
        plant$plantPop      <- atualData[[i+3]]
        plant$rowSpacing    <- atualData[[i+4]]
        plant$plantingDepht <- atualData[[i+5]]
        plant$cycleLength   <- atualData[[i+6]]
        plant$ncycles       <- atualData[[i+7]]
        plant$params        <- atualData[[i+8]]
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