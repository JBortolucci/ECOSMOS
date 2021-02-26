# install packages
# install.packages(c("Rcpp"))

# Package dependency
library(ncdf4)
library(tidyverse)
library(lubridate)
library(Rcpp)
library(compiler)

# Simulation Data (simData) is the container for all variables present in the simulator. 
# This environment keeps the variables definition (just a "fingerprint").
# TODO: Change name from 'simDataVars' to 'simData'
# TODO: Move it to an appropriate place
simDataVars <- new.env(parent = globalenv())

# Agrupa as variáveis por dimensão (npft, nsoilay, ndat etc)
varGroups <- list()

# # #
# # # Attribute of simDataVars
# # #

# PlantList is the data structure used by instance of simulation to access plant models.
simDataVars$plantList <- list()


# # #
# # # Globals attributes
# # #

# Estrutura que guarda as instancias da simulação. Cada ambiente dentro de simInstance é uma cópida de simDataVars.
# Cada instancia é referenciada pelo nome do id da simulação, definida no arquivo de configuração.
simInstances <- new.env(parent = globalenv())

# PlantBaseList variable is used to maintain the implementation of an extern model avaliable in the simulator.
# Private field - Do not be an attribute of simDataVars
basePlantList <- new.env(parent = globalenv())

simConfigs <- list()

# Initializes all functions
source("R/Simulator/FunctionsInicialization.R")


simDataVars$ndaypm <- array(data = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

# Indice da planta que deve iniciar a simular. Se não for especificado, simula na ordem passada no arquivo de configuração.
simDataVars$currentPlant <- 1

# TODO: Melhorar estrutura de controle de estados da simulação
# variáveis de controle da simulação
simDataVars$initialized   <- F
simDataVars$pauseEachStep <- T


# Eventos chamados dentro da simulação
simDataVars$OnEndDailyStep  <- NULL
simDataVars$OnEndHourlyStep <- NULL


# Initializes all variables
source("R/Simulator/VariablesInicialization.R")


# TODO: Chama função de inicializaçao de variável aqui.
VariablesInicialization()


CreateBuiltInNatVegModels()

# Run all simulations


RunSimulation <- function(parallel = F, compiled = F, OnEndDailyStep = NULL, OnEndHourlyStep = NULL) {
  
  setDefaultFunctions()
  if(compiled){ SetCompiledFunctions() } 
  
  if(parallel) {
    
    simInstancesNames <- as.list(ls(simInstances))
    
    mclapply(simInstancesNames, function(instance_name) {
      GeneralModel(simVars = simInstances[[instance_name]])
    })
    
  } else {
    
    len <- length(simInstances)
    for(i in seq(1,len)) {
      
      simId <- simConfigs[[i]]$id
      
      print(paste0("# "))
      print(paste0("# Starting simulation ", simId, "..."))
      print(paste0("# "))
      start_time <- Sys.time()
      
      # TODO: Continuar implementação depois. Teria que separar os loops em funções para chamada isolada.
      # if(stepControl) {
      #   print(paste0("WARNING: Step control activated. The simulation will be paused before each time step (hourly scale). Call RunSimulation again to run the next step."))
      #   simInstances[[simId]]$pauseEachStep <- T
      # }
      
      simInstances[[simId]]$OnEndDailyStep  <- OnEndDailyStep
      simInstances[[simId]]$OnEndHourlyStep <- OnEndHourlyStep
      
      GeneralModel(simVars = simInstances[[simId]])
      end_time <- Sys.time()
      simInstances[[simId]]$simulatioTime <- end_time - start_time
    }
    
  }
  
}





#' Run one instance of simulation.
#' 
#' @param simVars: Instance of simulation.
#' 
GeneralModel <- function(simVars = NULL) {
  
  
  if(!simVars$initialized) {
    
    # TODO: Verificar o valor do temp para NaturalVeg
    simVars$temp[] <- 1
    
    point   <- simVars$point
    config  <- simVars$config
    
    simVars$outputHourly  <-  file(paste0("output/outputHourly_",config$id,".dat"), "w")
    simVars$dailyOutput   <- file(paste0("output/dailyOutput_", config$id,".dat"), "w")
    
    simVars$absStep <- 1
    
    simVars$year0 <- config$startYear
    simVars$nrun  <- (config$endYear - config$startYear) + 1
    
    # if(config$npft == 1) {
    #   simVars$year0      <-  config$plant1$startYear 
    #   simVars$nrun       <-  simVars$plantList[[1]]$totalYears
    # } else {
    #   
    #   # No caso de haver mais de uma planta, nrun será a somatória do totalYears de todas as plantas.
    #   simVars$year0      <- config$plant1$startYear
    #   
    #   for(i in 1:simVars$npft) 
    #     simVars$nrun <- simVars$nrun + simVars$plantList[[i]]$totalYears
    #   
    # }
    
    
    simVars$dtime      <- 3600
    
    # General configurations
    simVars$soilcspin  <- config$soilcspin    
    simVars$isimveg    <- config$isimveg
    simVars$isimfire   <- config$isimfire          
    simVars$isimco2    <- simVars$FIXED_CO2                
    simVars$irrigate   <- config$irrigate                  
    simVars$overveg    <- simVars$COMPETE_CLIM_CONSTRAINTS  
    
    simVars$co2init    <- 0.000380 
    simVars$o2init     <- 0.209000 
    
    simVars$lat        <- point$coord$lat
    simVars$lon        <- point$coord$lon
    
    # Função de Configuração do output
    # createOutput(simVars)
    
    simVars$cropsums <- length(simVars$plantList)
    # TODO: Para testar a planta, por causa dos if's, coloca como 0. Remover essa variável e os if's depois.
    simVars$cropsums <- 1 #Jair e Victor atencao na hora de testar o modelo
    
    
    simVars$iyrlast <- simVars$year0 - 1
    
    simVars$lonindex <- point$lonIndex
    simVars$latindex <- point$latIndex
    
    simVars$lonscale <- point$coord$lon #SCV/Jair - alterar para coordenado do template
    simVars$latscale <- point$coord$lat #SCV/Jair - alterar para coordenado do template
    
    #### Dados de entrada lidos do netcdf ####
    simVars$xintopo  <- point$xintopo  #SCV/Jair - inserir no template
    simVars$xinveg   <- point$xinveg   #SCV/Jair - remover
    simVars$deltat   <- point$deltat   #SCV/Jair - remover
   


      # Henrique e Leandro - Remover, colocar o valor default dentro da cultura, caso não seja lido do arquivo.
    simVars$fertmaize  <- matrix(5.76626, nrow = 1, ncol = 51)           #SCV/Jair - alterar dentro dos modelos e depois substituir com a leitura da fertilizacao
    simVars$fertsgc    <- simVars$fertmaize * 2                          #SCV/Jair - alterar dentro dos modelos e depois substituir com a leitura da fertilizacao
    simVars$fertsoy    <- matrix(2.24803, nrow = 1, ncol = 51)           #SCV/Jair - alterar dentro dos modelos e depois substituir com a leitura da fertilizacao  

    simVars$co2conc    <- simVars$co2init
    simVars$o2conc     <- simVars$o2init
    
    simVars$precip <- numeric(1)
    
    simVars$pstart[] <- 999  # Jair, essa, e as demais variávies, não deveria ser criadas dentro de um arquivo separado de inicializacao?
    
    simVars$soilType <- config$soilId # as.character(ptos.sim[ii,]$SOIL_BR) # Soil type information from database
    
    environment(climanl)              <- simVars
    environment(initial)              <- simVars
    environment(initialcrop)          <- simVars
    environment(ReadDailyStationData) <- simVars
    environment(ReadMethourlyData)    <- simVars
    environment(UseDailyStationData)  <- simVars
    
    
    environment(Cropupdate)              <- simVars
    environment(CropPhenoUpdate)         <- simVars
    environment(ResetCropsAfterHarvest)  <- simVars
    
    environment(SoilbgcModel)     <- simVars
    environment(irrigation)       <- simVars
    environment(UseMethourlyData) <- simVars
    environment(diurnalmet)       <- simVars
    environment(diurnal)          <- simVars
    environment(lsxmain)          <- simVars
    environment(sumnow)           <- simVars
    environment(sumday)           <- simVars
    environment(summonth)         <- simVars
    environment(sumyear)          <- simVars
    environment(nitrostress)      <- simVars
    environment(leaching)         <- simVars
    environment(co2)              <- simVars
    environment(dynaveg)          <- simVars
    
    environment(PhenoUpdate)      <- simVars
    
    config$stationIDH=""
    if(file.exists(paste0("inst/input/",config$stationID,"H.csv"))==T) config$stationIDH=paste0("inst/input/",config$stationID,"H.csv")
    
    if(config$stationIDH != "" ) {
      ReadMethourlyData(path = config$stationIDH) 
    } else {
      simVars$imetyear <- -999
      simVars$dmetyear <- -999
      simVars$imetend  <- -999
      simVars$dmetend  <- -999
    }
    
    climanl()
    
    initial(simVars$isimveg)
    
    # PFT_UPDATE: Depois de modularizar para cada tipo de planta, retirar essa variável "cropsums",
    #             pois o tipo será decidido pelo usuário e as configurações setadas antes de iniciar
    #             a simulação.
    # SVC - levei tudo para iniveg
    # initialcrop()
    
    
    # determine the number of timesteps per day
    simVars$niter <-  round(86400 / simVars$dtime)
    
    simVars$iy1     <- simVars$iyrlast + 1
    simVars$iy2     <- simVars$iyrlast + simVars$nrun
    
    # simVars$out_tower <- file(outputDailyFileName, "w")
    
    flx <- array(0,  50)
    
    # inicializou o simulador
    simVars$inicialized <- T
    
  }
  
  for(year in seq(simVars$iy1, simVars$iy2)) {
    
    simVars$year <- year
    
    # TODO: Colocar em outro local, pois se a planta n inicia o ano crescendo e é colhida no meio do ano a próxima não é plantada.
    # Ativa planta caso esteja no ano de plantar (input da planilha de controle)
    # if(simVars$currentPlant <= length(simVars$plantList)) {
    #   if(simVars$plantList[[simVars$currentPlant]]$startYear == year & !simVars$plantList[[simVars$currentPlant]]$active) {
    #     simVars$plantList[[simVars$currentPlant]]$active <- T
    #     simVars$exist[simVars$currentPlant]              <- 1
    #   }
    # } else {
    #   print("There is no plant configured to run in this simulation!")
    # }
    
    # OK
    # reset julian date
    simVars$jday <- 0
    
    simVars$startOfSimulation <- config$startOfSimulation
    simVars$endOfSimulation <- config$endOfSimulation
    
    leapYearOut       <- LeapYear(year)
    simVars$ndaypm[2] <- leapYearOut$ndaypm
    simVars$ndaypy    <- leapYearOut$ndaypy
    
    if (!is.na(simVars$endOfSimulation)){
      simVars$simulationEndMonth <- as.numeric(format(as.Date(simVars$endOfSimulation-1, origin = as.Date(paste0(simVars$iy2,"-01-01"))), "%m"))
      simVars$simulationEndDay   <- as.numeric(format(as.Date(simVars$endOfSimulation-1, origin = as.Date(paste0(simVars$iy2,"-01-01"))), "%d"))
    }
    
    if (simVars$year == simVars$iy1 && !is.na(simVars$startOfSimulation)){
      simVars$simulationStartMonth <- as.numeric(format(as.Date(simVars$startOfSimulation-1, origin = as.Date(paste0(simVars$year,"-01-01"))), "%m"))
      simVars$simulationStartDay   <- as.numeric(format(as.Date(simVars$startOfSimulation-1, origin = as.Date(paste0(simVars$year,"-01-01"))), "%d"))
      simVars$jday <- simVars$startOfSimulation-1
    }else{
      simVars$simulationStartMonth <- 1 
      simVars$simulationStartDay   <- 1
    }
    
    for(month in seq(simVars$simulationStartMonth, 12)) {
      
      simVars$month <- month
      
      if (!is.na(simVars$startOfSimulation) && year == simVars$iy1 && (month == (simVars$simulationStartMonth + 1) || month == 1)){
        simVars$simulationStartDay <- 1
      }
      
      for(day in seq(simVars$simulationStartDay, daypm(month, year))) {
        
        
        for(i in seq(1,simVars$npft)) {
          if(simVars$plantList[[i]]$active) next
          if(simVars$plantList[[i]]$startYear == year) {
            simVars$plantList[[i]]$active <- T
            simVars$exist[i]              <- 1
          }
        }

        
        simVars$day <- day

        simVars$jday <- simVars$jday + 1
        
        print(paste("Simulation ",day, month, year,simVars$lai[1]*simVars$fl,simVars$lai[2]*simVars$fu,sep = " / "))
        
        UseDailyStationData(day, month, year)
        

        for(j in seq(1,simVars$npft)) {
          if(!simVars$plantList[[j]]$active) next
          if(simVars$plantList[[j]]$type == simVars$CROPS) {
            if(simVars$irrigate == 1 && simVars$croplive[j]) { 
              irrigation(day, month) 
            }
          }
        }
        
        # INICIO LOOP HORÁRIO
        
        # cria as Variáveis de média
        # initializeMeansVariables(simVars, simVars$outputDailyList)
        
        for(step in seq(1, simVars$niter)) {
          
          simVars$step <- step

          time <- (step - 1) * simVars$dtime
          
          if ( (year == simVars$imetyear && simVars$jday >= simVars$dmetyear) || 
               (year > simVars$imetyear && year < simVars$imetend) || 
               (year == simVars$imetend && simVars$jday <= simVars$dmetend)) {
            
            UseMethourlyData(year, simVars$jday, time)
            
            diurnalmet(simVars, time, simVars$jday, simVars$irrigate)
          } else {
            
            diurnal(simVars, time, simVars$jday, simVars$irrigate)
            
          }
          
          
          lsxmain(time, day, month, year, simVars$jday)
          
          sumnow() # codigo em R
          
          sumday(step, plens, year, simVars$jday)
          #CSVC summonth(step, day, month)
          #CSVC sumyear(step, day, month)
          
          nitrostress(step, day, month)
          
          leaching(0, step, day, month, year, 0, simVars$year0)
          
          
          # Adiciona 1 ao passo absoluto
          simVars$absStep <- simVars$absStep + 1 
          
          # # Salva o valor incrementando ele mesmo todos os dias 
          # for (variavel in simVars$outputDailyList) {
          #   simVars[[paste(variavel, "Mean", sep = '')]] <- simVars[[paste(variavel, "Mean", sep = '')]] + simVars[[variavel]]
          # } 
          # 
          # if (!is.null(simVars$outputHourlyList))
          #   outputC(simVars, paste(year, month, day, sep = '-'), simVars$HOURLY)
          # 
          # output_hourly <- paste(year, simVars$jday, step, - simVars$tneetot * 1e6 , sep=',')
          # writeLines(output_hourly, simVars$out_tower_hourly)
          # 
          if(!is.null(simVars$OnEndHourlyStep))
            simVars$OnEndHourlyStep(simVars)
          
          
        } # FIM DO LOOP HORÁRIO
        
        
        
#       Chama os modelos de vegetacao
        
        # TODO: Testando, comentar caso queira rodar o modelo corretamente (ou antes de terminar de testar)
        # determine the daily vegetation cover characteristics
        for(i in seq(1, simVars$npft)) {
          if(!simVars$plantList[[i]]$active) next
          if(simVars$plantList[[i]]$type == simVars$NATURAL_VEG && !is.null(simVars$plantList[[i]]$Model)) {
            environment(simVars$plantList[[i]]$Model) <- simVars
            simVars$plantList[[i]]$Model(simVars$jday, i)
          }
        }
        
        for(i in seq(1,simVars$npft)) {
          if(!simVars$plantList[[i]]$active) next
          if(simVars$plantList[[i]]$type == simVars$CROPS && !is.null(simVars$plantList[[i]]$Model)) {
            environment(simVars$plantList[[i]]$Model) <- simVars
            simVars$plantList[[i]]$Model(year, month, day, i)
          }
        } 
        
        
        ResetCropsAfterHarvest()
        
#   Atualiza as propriedades da vegetacao 
        CropPhenoUpdate()
        
        # Check if the cycle is complete
        for(i in seq(1,simVars$npft)) {
          
          if(!simVars$plantList[[i]]$active || simVars$plantList[[i]]$type == simVars$NATURAL_VEG) next
          
          if(simVars$endCycle[i]) {
            
            print(paste0("Harvest ", simVars$plantList[[i]]$name, " - cycle ", simVars$plantList[[i]]$currentCycle))
            simVars$plantList[[i]]$currentCycle <- simVars$plantList[[i]]$currentCycle + 1
            
            # reset end cycle variable
            simVars$endCycle[i] <- F
            
            if(simVars$plantList[[i]]$currentCycle > simVars$plantList[[i]]$totalCycles) {
              
              browser()
              print(paste0("Crop ", simVars$plantList[[i]]$name, " is finished"))
              # turn off current plant
              simVars$plantList[[i]]$active <- F
              simVars$exist[i]              <- 0
              # if exist a plant to simulate next, increase the currentPlant by one.
              # simVars$currentPlant <- simVars$currentPlant + 1
              
            }
          }
        }
        
        
        # call soil biogeochemistry model
        SoilbgcModel(year, month, day)
        
        
        
        # # Divide todos os valor por 24 para calcular a média do dia 
        # for (variavel in simVars$outputDailyList) {
        #   simVars[[paste(variavel, "Mean", sep = '')]] <- simVars[[paste(variavel, "Mean", sep = '')]] / 24
        # }
        
        # if (!is.null(simVars$outputDailyList))
        #   outputC(simVars, paste(year, month, day, sep = '-'), simVars$NO_HOURLY)
        
        
        if(!is.null(simVars$OnEndDailyStep)) 
          simVars$OnEndDailyStep(simVars)
        
        
        if(year == simVars$iy2 && month == simVars$simulationEndMonth && day == simVars$simulationEndDay){
          simVars$simulationEnd <- T 
          break()
        }
        
      } # FIM DO LOOP DIÁRIO
      
      if (simVars$simulationEnd) {
        break()
      }
    } # FIM DO LOOP MENSAL
    
    
    if (simVars$isimco2 == 1) co2(simVars$co2init, simVars$co2conc, year)
    
    if (simVars$isimveg != 0) dynaveg(simVars$isimfire)
    
    simVars$iyrlast <- simVars$iyrlast + 1
    
    # dataFrameGenerator(simVars)
    if (simVars$simulationEnd){
      break()
    } 
  } # FIM DO LOOP ANUAL
  
  
  print(".")
  print(".")
  print(".")
  print("Simulation complete.")
  
  closeAllConnections()
  
}












