
ConfigSimulationFromFile <- function(configFilePath, paramsPath, stationDataPath) {
  
  
  # TODO: Decidir se simConfigs fica uma lista ou um ambiente
  simConfigs <- ReadConfigFromCSV(configFilePath)
  assign("simConfigs", simConfigs, env = globalenv())
  
  simDataVars$gridSolved <- F
  
  len <- length(simConfigs)
  for(i in seq(1,len)) {
    
    id <- simConfigs[[i]]$id
    
    # Cria uma instância da simulação
    simInstances[[id]] <- new.env(parent = globalenv())
    
    
    # copy all content of simDataVars to new simulation instance
    for(n in ls(simDataVars, all.names=TRUE)) assign(n, get(n, simDataVars), env = simInstances[[id]])
    
    # cria uma auto referência para a instancia da simulação
    simInstances[[id]]$env <- simInstances[[id]]
    
    # resize variables of specific instance based on input configuration file
    
    # resize npft
    simInstances[[id]]$npft <- simConfigs[[i]]$npft
    for(n in seq(1,length(varGroups$npft))) {
      name <- varGroups$npft[n]
      simInstances[[id]][[name]] <- numeric(simConfigs[[i]]$npft)
    }
    
    # rezise nband (nband = 2)
    simInstances[[id]][["rhoveg"]] <- matrix(0, simInstances[[id]]$nband, 2)
    simInstances[[id]][["tauveg"]] <- matrix(0, simInstances[[id]]$nband, 2)
    simInstances[[id]][["solad"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["solai"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["asurd"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["asuri"]]  <- numeric(simInstances[[id]]$nband)
    
    
    tab.DSSAT <- read.csv('inst/input/perfil_solo_ecosmos_UPDATE.csv',sep = ",")
    if(!is.na(simConfigs[[i]]$soilId)) {
      simInstances[[id]]$layers    <- subset(tab.DSSAT, SID == simConfigs[[i]]$soilId)
      simInstances[[id]]$nsoilay <- length(simInstances[[id]]$layers$SID)
    }

    simInstances[[id]][["tsoi"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["wsoi"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["wisoi"]]    <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["consoi"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["csoi"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["hydraul"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["suction"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["bex"]]      <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["sfield"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["swilt"]]    <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["rhosoi"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["poros"]]    <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["porosflo"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["sand"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["clay"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["stressl"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["stressu"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["upsoiu"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["upsoil"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["soisand"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["soiclay"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["domtext"]]  <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["fracsand"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["fracsilt"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["fracclay"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["cpwf"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["swater"]]   <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["sice"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["hflo"]]     <- numeric(simInstances[[id]]$nsoilay+1)
    simInstances[[id]][["ibex"]]     <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["pmsoil"]]   <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["pmsoln"]]   <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["pcsoln"]]   <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["smsoil"]]   <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["smsoln"]]   <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["fout"]]     <- numeric(simInstances[[id]]$nsoilay*2+1) 
    simInstances[[id]][["nout"]]     <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["csoln"]]    <- numeric(simInstances[[id]]$nsoilay*2+1)  
    simInstances[[id]][["tnuptake"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["anuptake"]] <- numeric(simInstances[[id]]$nsoilay)
    simInstances[[id]][["wflo"]]     <- numeric(simInstances[[id]]$nsoilay+1)
    simInstances[[id]][["froot"]]    <- matrix(0, simInstances[[id]]$nsoilay, 2)
    
    # Criando na instancia da simulação a lista de plantas que serão simuladas
    for(j in seq(1, simConfigs[[i]]$npft)) {

      # Busca na baseList a planta pelo id passado no arquivo de configuração. Se a planta não existir, interrompe a execução.
      name  <- simConfigs[[i]][[paste0("plant",j)]]$name
      model <- basePlantList[[name]]
      
      if(!is.null(model)) {
        
        simInstances[[id]]$plantList[[name]] <- model
        
        # NOVO: A planta agora é ativada no loop, dependendo do ano que deve iniciar
        simInstances[[id]]$plantList[[name]]$active <- F
        if(simInstances[[id]]$plantList[[name]]$type == simDataVars$NATURAL_VEG) {
          simInstances[[id]]$plantList[[name]]$active <- T
        }
        
        
      } else {
        stop(paste0("Model ", simConfigs[[name]]$plant1$name," does not exist in the simulator. Check if you have an implementation in your project."))
      }
    }
    
    # Passa a configuração para a instancia da simulação
    simInstances[[id]]$config <- simConfigs[[i]]
    
    # Coord info
    simInstances[[id]]$point <- GetPointsFromRect(simConfigs[[i]]$coord$lat, simConfigs[[i]]$coord$lat,
                                                  simConfigs[[i]]$coord$lon, simConfigs[[i]]$coord$lon)$point1
    
    # This variable controls the end of the cycle
    simInstances[[id]]$endCycle <- F
    
    ReadDailyStationData(stationDataPath, simConfigs[[i]]$coord$lat, simConfigs[[i]]$coord$lon,  simInstances[[id]])
    
  }
  
  # TODO: Nessa prieira versão uma planta roda após a outra, tal como especificado no arquivo de configuração.
  #       Depois, fazer um jeito de definir plantas rodando ao mesmo tempo.
  
  ReadPlantParamsFromFile(path = paramsPath)
  
  # read daily station data
  
  
}
