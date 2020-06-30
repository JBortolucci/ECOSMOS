########################################################################
#
#  EXEMPLO
#
#  Esse arquivo é um exemplo de como configurar e executar uma simulação
#  utilizando o Ecosmos.
#
#
########################################################################


# Carrega e compila todas as funções do simulador.
source("./R/Simulator/RunSimulation.R")


# Carrega o modelo que será utilizado na simulação.
source("./R/CropModels/Eucalipto/EucaliptoModel.R")
# source("./R/CropModels/Sugarcane/SugarcaneModel.R")


# Adiciona no simulador o modelo previamente carregado.
CreateModel(nameArg           = "eucalipto",
            typeArg           = simDataVars$CROPS,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = EucaliptoModel)

# CreateModel(nameArg           = "sugarcane",
#             typeArg           = simDataVars$CROPS,
#             canopyArg         = simDataVars$LOWER,
#             carbonFixationArg = simDataVars$C3,
#             ModelArg          = SugarcaneModel)


# Faz a leitura do arquivo contendo os parâmetros globais da simulação.
ReadGlobalParamsFromFile("inst/global_params.csv")


# Função que prepara a simulação que será executada.

# Param1: template contendo as configurações de controle da simulação (abra o arquivo para detalhes).
# Param2: 
# Param3:
ConfigSimulationFromFile("inst/Template_Simulation_Control_ECOSMOS.csv", "inst/plant_params.csv", "D:/input_xavier/")


#################
# Run simulation. 
# Warning: The simulation will run for configuration previously defined, if no configuration is provided, the default config will be 
# setted automatically. Be aware, this can result in unexpected output, for security read the documetation for more details about 
# default configs.

HourlyStep <- function(simVars) {



}

plai <- numeric(3000)
i    <- 1
DailyStep <- function(simVars) {

  
  print(paste(simVars$year, simVars$jday, simVars$plai[1]))
  plai[i] <- simVars$plai[1]
  i <- i + 1
  
  assign("i", i, envir = globalenv())
  assign("plai", plai, envir = globalenv())
  
}

RunSimulation(parallel = F, compiled = T, OnEndDailyStep = DailyStep, OnEndHourlyStep = HourlyStep)


plot(plai, type = "l", col = "red")
# lines(plai2, type = "l", col = "green")


