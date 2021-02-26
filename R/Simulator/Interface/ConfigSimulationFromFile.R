
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
  
    
    # rezise nband (nband = 2)
    simInstances[[id]][["rhoveg"]] <- matrix(0, simInstances[[id]]$nband, 2)
    simInstances[[id]][["tauveg"]] <- matrix(0, simInstances[[id]]$nband, 2)
    simInstances[[id]][["solad"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["solai"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["asurd"]]  <- numeric(simInstances[[id]]$nband)
    simInstances[[id]][["asuri"]]  <- numeric(simInstances[[id]]$nband)
    
    tab.SOIL <- read.csv('inst/input/SOIL.csv',sep = ",")
    if(!is.na(simConfigs[[i]]$soilId)) {
      simInstances[[id]]$SOIL.profile <- subset(tab.SOIL, SOILID == simConfigs[[i]]$soilId)
      simInstances[[id]]$nsoilay  <- length(simInstances[[id]]$SOIL.profile$SOILID)
    }else{
      print('SID not defined in the Simulation Template or not found in the SOIL.csv')
      stop()
    }
    
    # Henrique & Leandro: including initial conditions (IC) [2020-11-04]
    simInstances[[id]]$soilic <- simConfigs[[i]]$soilic
    if(simConfigs[[i]]$soilic == 1) {
      tab.IC <- read.csv('inst/input/initial_conditions.csv',sep = ",")
      # when the user insert the initial conditions (nsoilay MUST be equal in both tab's [DSSAT & IC])
      simInstances[[id]]$swic  <- tab.IC$SWic[tab.IC$SimIDic == id]
      
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
      

      name  <- simConfigs[[i]][[paste0("plant",j)]]$name
      
      simInstances[[id]][['biomeName']] <- name
      
      #
      # Para modelos compostos
      #
      if(IsBioma(name)) {
        

        controlConfigs <- simConfigs[[i]][[paste0("plant",j)]]
        
        models <- biomaModels[[name]]
        
        for(n in 1:length(models)) {
          
          modelName <- models[n]
          simInstances[[id]]$plantList[[modelName]]                <- basePlantList[[modelName]]
          simInstances[[id]]$plantList[[modelName]]$active         <- F
          
          controlConfigs$name <- modelName
          simInstances[[id]]$plantList[[modelName]]$controlConfigs <- controlConfigs
            
          # flag para verificar se a planta faz parte de um bioma
          simInstances[[id]]$plantList[[modelName]]$bioma  <- T
          
        }
        
      #
      # Para modelos isolados
      #  
      } else {
        
        model <- basePlantList[[name]]
        
        if(!is.null(model)) {
          
          simInstances[[id]]$plantList[[name]] <- model
          
          # NOVO: A planta agora é ativada no loop, dependendo do ano que deve iniciar
          simInstances[[id]]$plantList[[name]]$active         <- F
          simInstances[[id]]$plantList[[name]]$controlConfigs <- simConfigs[[i]][[paste0("plant",j)]]
          simInstances[[id]]$plantList[[name]]$bioma          <- F
          # TODO: Agora a vegetação natural também é afetada pelos ciclos
          # if(simInstances[[id]]$plantList[[name]]$type == simDataVars$NATURAL_VEG) {
          #   simInstances[[id]]$plantList[[name]]$active <- T
          # }
          
          
        } else {
          stop(paste0("Model ", simConfigs[[name]]$plant1$name," does not exist in the simulator. Check if you have an implementation in your project."))
        }
        
      }
      
    }
    
    # resize npft
    simInstances[[id]]$npft <- length(simInstances[[id]]$plantList)
    for(n in seq(1,length(varGroups$npft))) {
      name <- varGroups$npft[n]
      simInstances[[id]][[name]] <- numeric(simInstances[[id]]$npft)
    }
    

    # create endCycle for each plant
    simInstances[[id]]$endCycle <- numeric(simInstances[[id]]$npft)

    # Passa a configuração para a instancia da simulação
    simInstances[[id]]$config <- simConfigs[[i]]
    
    # Coord info
    simInstances[[id]]$point <- GetPointsFromRect(simConfigs[[i]]$coord$lat, simConfigs[[i]]$coord$lat,
                                                  simConfigs[[i]]$coord$lon, simConfigs[[i]]$coord$lon)$point1
    
    
    # TODO: Nessa prieira versão uma planta roda após a outra, tal como especificado no arquivo de configuração.
    #       Depois, fazer um jeito de definir plantas rodando ao mesmo tempo.
    ReadPlantParamsFromFile(path = paramsPath)
    
    #___________________________________________    
    # READ DAILY STATION DATA
    
    if(file.exists(paste0("inst/input/",simConfigs[[i]]$stationID,".csv"))==T) {
      
      pathw <- paste0("inst/input/",simConfigs[[i]]$stationID,".csv")
      
    }else{
      
      latc <-  simInstances[[id]]$point$coord$lat + 0.125
      lonc <-  simInstances[[id]]$point$coord$lon - 0.125
      lons<-substr(lonc*1000,2,5)
      
      if(latc*1000>0){
        if(latc*1000<1000){lats<-paste0("N0",substr(latc*1000,1,2))}else{lats<-paste0("N",substr(latc*1000,1,3)) }}else{
          if(latc*1000>(-10*1000)){lats<-paste0("0",substr(latc*1000,2,4))}else{lats<-substr(latc*1000,2,5)}
        }
      
      wth<- as.character(sprintf("%4s%4s",lats,lons))
      wth<-gsub(' ','0',wth)
      
      pathw<- paste0(stationDataPath,wth,".csv")
      if(file.exists(pathw)==F){print(paste0('FILE ',pathw ,'DOES NOT EXIST'))
        stop()}
    }
    

    ReadDailyStationData(pathw, simInstances[[id]]$point$coord$lat, simInstances[[id]]$point$coord$lon,  simInstances[[id]])
    
    #___________________________________________    
    # READ HORLY STATION DATA
    
    
    assign("irriON", ifelse(simConfigs[[i]]$irrigate > 0, T, F), envir = simInstances[[id]])
    
    # Henrique & Leandro: irrigation feature [2020-11-06]
    assign("irriON", ifelse(simConfigs[[i]]$irrigate > 0, T, F), envir = simInstances[[id]])
    if(simConfigs[[i]]$irrigate>0){try(ReadDailyIrrigationData(id, instanceEnv = simInstances[[id]]), silent=TRUE)}
  }
  

  
}
