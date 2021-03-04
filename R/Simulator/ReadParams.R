

# TODO: Document funcition

ReadGlobalParamsFromFile <- function(path = "", col = 1) {
  
  data <- read.csv(file = path, header = T, stringsAsFactors = F, sep = ",")
  
  envToSet <- simDataVars
  
  ##
  ## canopy
  ##
  
  assign("tau15", as.numeric(data[1,][2]), envir = envToSet)
  assign("kc15", as.numeric(data[2,][2]), envir = envToSet) 
  assign("ko15", as.numeric(data[3,][2]), envir = envToSet) 
  assign("cimax", as.numeric(data[4,][2]), envir = envToSet)
  assign("woodnorm", as.numeric(data[5,][2]), envir = envToSet) 
  
  # TO DO - Remover (nband e' sempre igual a 2 no nosso modelo)
  assign("nband", as.numeric(data[6,][2]), envir = envToSet) 
  
  # TO DO - Remover - esta' no final do Global_params 
  rhoveg <- matrix(0, simDataVars$nband, 2)
  rhoveg[1,] <- c( as.numeric(data[7,][2]),  as.numeric(data[7,][3]))  # vis leaf reflectance, lower story
  rhoveg[2,] <- c( as.numeric(data[8,][2]),  as.numeric(data[8,][3]))  # vis leaf reflectance, upper story
  assign("rhoveg", rhoveg, envir = envToSet) 
  # TO DO - Remover 
  
  tauveg <- matrix(0, simDataVars$nband, 2)
  tauveg[1,] <- c( as.numeric(data[9,][2]),  as.numeric(data[9,][3]))   # vis leaf transmittance, lower story
  tauveg[2,] <- c( as.numeric(data[10,][2]),  as.numeric(data[10,][3])) # vis leaf transmittance, upper story
  assign("tauveg", tauveg, envir = envToSet) 
  
  dleaf <- c( as.numeric(data[11,][2]),  as.numeric(data[11,][3]))
  assign("dleaf",  dleaf, envir = envToSet) 
  
  dstem <- c( as.numeric(data[12,][2]),  as.numeric(data[12,][3]))
  assign("dstem",  dstem, envir = envToSet)
  
  assign("alaimu",  as.numeric(data[13,][2]), envir = envToSet)  
  assign("alaiml",  as.numeric(data[14,][2]), envir = envToSet)  
  assign("cleaf",  as.numeric(data[15,][2]), envir = envToSet)  
  assign("cstem",  as.numeric(data[16,][2]), envir = envToSet) 
  assign("cgrass",  as.numeric(data[17,][2]), envir = envToSet) 
  assign("chs",  as.numeric(data[18,][2]), envir = envToSet)  
  assign("chu",  as.numeric(data[19,][2]), envir = envToSet)
  assign("chl",  as.numeric(data[20,][2]), envir = envToSet) 
  assign("wliqumax",  as.numeric(data[21,][2]), envir = envToSet) 
  assign("wliqsmax",  as.numeric(data[22,][2]), envir = envToSet) 
  assign("wliqlmax",  as.numeric(data[23,][2]), envir = envToSet)  
  assign("wsnoumax",  as.numeric(data[24,][2]), envir = envToSet)   
  assign("wsnosmax",  as.numeric(data[25,][2]), envir = envToSet)   
  assign("wsnolmax",  as.numeric(data[26,][2]), envir = envToSet)   
  assign("tdripu",  as.numeric(data[27,][2]), envir = envToSet)    
  assign("tdrips",  as.numeric(data[28,][2]), envir = envToSet)  
  assign("tdripl",  as.numeric(data[29,][2]), envir = envToSet)   
  assign("tblowu",  as.numeric(data[30,][2]), envir = envToSet)    
  assign("tblows",  as.numeric(data[31,][2]), envir = envToSet) 
  assign("tblowl",  as.numeric(data[32,][2]), envir = envToSet)  
  
  ##
  ## vegetation
  ##
  
  assign("plaiupper",  as.numeric(data[34,][2]), envir = envToSet)   
  assign("plailower",  as.numeric(data[35,][2]), envir = envToSet)   
  assign("xminlai",  as.numeric(data[36,][2]), envir = envToSet) 
  assign("sapfrac_init",  as.numeric(data[37,][2]), envir = envToSet)  
  # assign("beta1",  as.numeric(data[38,][2]), envir = envToSet)  Virou parametro específico da planta
  assign("beta2",  as.numeric(data[38,][2]), envir = envToSet)  
  
  ##
  ## crops
  ##
  
  assign("alphac",  as.numeric(data[40,][2]), envir = envToSet) 
  assign("gnmin",  as.numeric(data[41,][2]), envir = envToSet) 
  assign("smax",  as.numeric(data[42,][2]), envir = envToSet) 
  assign("availn",  as.numeric(data[43,][2]), envir = envToSet) 
  assign("cnmax",  as.numeric(data[44,][2]), envir = envToSet) 
  
  ##
  ## soil
  ##
  
  assign("nsoilay",  as.numeric(data[46,][2]), envir = envToSet)
  
  # TODO: Verificar necessidade do hsoi ter uma posição a mais.
  simDataVars$hsoi <- numeric(simDataVars$nsoilay+1) 
  for(i in 2:(simDataVars$nsoilay+1)) {
    simDataVars$hsoi[i-1] <- as.numeric(data[47,][i])
  }
  
  
   assign("nslaym", as.numeric(data[48,][2]), envir = envToSet) # TODO: Leandro, retirar dos parametros globias:
  assign("bperm", as.numeric(data[49,][2]), envir = envToSet)
  assign("wpudmax", as.numeric(data[50,][2]), envir = envToSet)
  assign("zwpmax", as.numeric(data[51,][2]), envir = envToSet)
  
  assign("nsoi", as.numeric(data[52,][2]), envir = envToSet)
  assign("ndat", as.numeric(data[53,][2]), envir = envToSet)
  
  simDataVars$texdat     <- matrix(0, 3, simDataVars$ndat) # TODO: Leandro, retirar dos parametros globias:
  simDataVars$porosdat   <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  simDataVars$sfielddat  <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  simDataVars$swiltdat   <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  simDataVars$bexdat     <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  simDataVars$suctiondat <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  simDataVars$hydrauldat <- numeric(simDataVars$ndat)      # TODO: Leandro, retirar dos parametros globias:
  
  for(i in 2:(simDataVars$ndat+1)) {
    simDataVars$texdat[1, i-1]  <- as.numeric(data[54,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$texdat[2, i-1]  <- as.numeric(data[55,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$texdat[3, i-1]  <- as.numeric(data[56,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$porosdat[i-1]   <- as.numeric(data[57,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$sfielddat[i-1]  <- as.numeric(data[58,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$swiltdat[i-1]   <- as.numeric(data[59,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$bexdat[i-1]     <- as.numeric(data[60,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$suctiondat[i-1] <- as.numeric(data[61,][i])# TODO: Leandro, depois que retirar dos parametros globias:
    simDataVars$hydrauldat[i-1] <- as.numeric(data[62,][i])# TODO: Leandro, depois que retirar dos parametros globias:
  }
  
  assign("lig_frac",  as.numeric(data[63,][2]), envir = envToSet)
  assign("fbsom", as.numeric(data[64,][2]), envir = envToSet)
  assign("effac", as.numeric(data[65,][2]), envir = envToSet)
  
  simDataVars$cnr <- numeric(8)
  for(i in 2:9) {
    simDataVars$cnr[i-1] <- as.numeric(data[66,][i])
  }
  
  assign("fmxcpool", as.numeric(data[67,][2]), envir = envToSet)
  assign("rconst", as.numeric(data[68,][2]), envir = envToSet)
  assign("h20", as.numeric(data[69,][2]), envir = envToSet)
  
  assign("klm", as.numeric(data[70,][2]), envir = envToSet)
  assign("kls", as.numeric(data[71,][2]), envir = envToSet)
  assign("kll", as.numeric(data[72,][2]), envir = envToSet)
  assign("krm", as.numeric(data[73,][2]), envir = envToSet)
  assign("krs", as.numeric(data[74,][2]), envir = envToSet)
  assign("krl", as.numeric(data[75,][2]), envir = envToSet)
  assign("kwm", as.numeric(data[76,][2]), envir = envToSet)
  assign("kws", as.numeric(data[77,][2]), envir = envToSet)
  assign("kwl", as.numeric(data[78,][2]), envir = envToSet)
  
  assign("kbn", as.numeric(data[79,][2]), envir = envToSet)
  assign("kbp", as.numeric(data[80,][2]), envir = envToSet)
  assign("knb", as.numeric(data[81,][2]), envir = envToSet)
  assign("kns", as.numeric(data[82,][2]), envir = envToSet)
  assign("kpb", as.numeric(data[83,][2]), envir = envToSet)
  assign("kps", as.numeric(data[84,][2]), envir = envToSet)
  assign("ksb", as.numeric(data[85,][2]), envir = envToSet)
  
  assign("ylm", as.numeric(data[86,][2]), envir = envToSet)
  assign("yrm", as.numeric(data[87,][2]), envir = envToSet)
  assign("ywm", as.numeric(data[88,][2]), envir = envToSet)
  assign("yls", as.numeric(data[89,][2]), envir = envToSet)
  assign("yrs", as.numeric(data[90,][2]), envir = envToSet)
  assign("yws", as.numeric(data[91,][2]), envir = envToSet)
  assign("yll", as.numeric(data[92,][2]), envir = envToSet)
  assign("yrl", as.numeric(data[93,][2]), envir = envToSet)
  assign("ywl", as.numeric(data[94,][2]), envir = envToSet)
  
  assign("ybn", as.numeric(data[95,][2]), envir = envToSet)
  assign("ybp", as.numeric(data[96,][2]), envir = envToSet)
  assign("yps", as.numeric(data[97,][2]), envir = envToSet)
  assign("yns", as.numeric(data[98,][2]), envir = envToSet)
  assign("ysb", as.numeric(data[99,][2]), envir = envToSet)
  assign("ypb", as.numeric(data[100,][2]), envir = envToSet)
  assign("ynb", as.numeric(data[101,][2]), envir = envToSet)
  assign("za",  as.numeric(data[102,][2]), envir = envToSet)
  assign("isoilay",  as.numeric(data[103,][2]), envir = envToSet)
}


ReadPlantParamsFromFile <- function(path = "") {
  
  userParams   <- read.csv(file = path, header = T, stringsAsFactors = F, sep = ",")
  natVegParams <- read.csv(file = "inst/natveg_params.csv", header = T, stringsAsFactors = F, sep = ",")
  
  for(simId in ls(simInstances)) {
    
    npft <- simInstances[[simId]]$npft

    for(i in 1:npft) {
      
      if(simInstances[[simId]]$plantList[[i]]$type == simInstances[[simId]]$NATURAL_VEG) {
        
        data   <- natVegParams
        column <- simInstances[[simId]]$plantList[[i]]$name
        
        config <- simInstances[[simId]]$plantList[[i]]$controlConfigs
        
        startYear     <- config$startYear
        plantJDay     <- config$plantJday
        cycleLength   <- config$cycleLength
        plantingDate  <- as.Date(plantJDay-1, origin = as.Date(paste0(startYear,"-01-01")))
        plantingDay   <- as.numeric(format(plantingDate, "%d"))
        plantingMonth <- as.numeric(format(plantingDate, "%m"))
        
        nextHarvestDate <- as.Date(cycleLength-1, origin = plantingDate)
        
        pmMin <- plantingMonth
        pdMin <- plantingDay
        
        simInstances[[simId]]$plantList[[i]]$totalYears <- (as.numeric(format(nextHarvestDate, "%Y")) - as.numeric(format(plantingDate, "%Y")) + 1) * config$ncycles
        
        simInstances[[simId]]$plantList[[i]]$startYear     <- startYear
        simInstances[[simId]]$plantList[[i]]$currentCycles <- 1
        simInstances[[simId]]$plantList[[i]]$totalCycles   <- config$ncycles
        
      } else {
        
        data   <- userParams
        column <- simInstances[[simId]]$config[[paste0("plant", i)]]$params
        
        config <- simInstances[[simId]]$plantList[[i]]$controlConfigs
        
        # Cuidado, na função as.Date, o dia juliano começa com 0!!! Será preciso subtrair 1 do valor da planilha
        startYear     <- config$startYear
        plantJDay     <- config$plantJday
        cycleLength   <- config$cycleLength
        plantingDate  <- as.Date(plantJDay-1, origin = as.Date(paste0(startYear,"-01-01")))
        plantingDay   <- as.numeric(format(plantingDate, "%d"))
        plantingMonth <- as.numeric(format(plantingDate, "%m"))
        
        # Verificar se a quantidade de ciclos é maior que 1, se sim, atualiza.
        nextHarvestDate <- as.Date(cycleLength-1, origin = plantingDate)
        
        # These two values is being read from simulation config table.
        pmMin <- plantingMonth
        pdMin <- plantingDay
        
        simInstances[[simId]]$plantList[[i]]$totalYears <- (as.numeric(format(nextHarvestDate, "%Y")) - as.numeric(format(plantingDate, "%Y")) + 1) * config$ncycles
        
        # create a variable for tracking the current cycle of the plant.
        simInstances[[simId]]$plantList[[i]]$startYear     <- startYear
        simInstances[[simId]]$plantList[[i]]$currentCycles <- 1
        simInstances[[simId]]$plantList[[i]]$totalCycles   <- config$ncycles
        
      }
    
      indexOfPlant <- i
      type         <- simInstances[[simId]]$plantList[[i]]$type
      
      if(is.null(data[[column]])) {
        stop(paste0("This column does not exist in parameters table."))
      }
      
      envToSet <- simInstances[[simId]]
      if(is.null(envToSet)) {
        stop(paste0("Erro na leitura de parâmetro: Não existe simulação com id: ", simId))
      }
      
      ##
      ## Canopy
      ##
      envToSet$alpha[indexOfPlant] <- as.numeric(data[1,column])
      envToSet$theta[indexOfPlant]<- as.numeric(data[2,column])
      envToSet$beta[indexOfPlant]<-as.numeric(data[3,column])
      envToSet$gamma[indexOfPlant]<-as.numeric(data[4,column])
      envToSet$coefm[indexOfPlant]<-as.numeric(data[5,column])
      envToSet$coefb[indexOfPlant]<-as.numeric(data[6,column])
      envToSet$gsmin[indexOfPlant]<-as.numeric(data[7,column])
      envToSet$vmax_pft[indexOfPlant]<- as.numeric(data[8,column])
      envToSet$specla[indexOfPlant]<-as.numeric(data[9,column])
      envToSet$tauleaf[indexOfPlant]<-as.numeric(data[10,column])
      envToSet$tauroot[indexOfPlant]<-as.numeric(data[11,column])
      envToSet$tauwood0[indexOfPlant]<-as.numeric(data[12,column])
      envToSet$aleaf[indexOfPlant]<-as.numeric(data[13,column])
      envToSet$aroot[indexOfPlant]<-as.numeric(data[14,column])
      envToSet$awood[indexOfPlant]<-as.numeric(data[15,column])
      
      ##
      ## Vegetation
      ##
      
      envToSet$TminL[indexOfPlant] <- as.numeric(data[17,column])
      envToSet$TminU[indexOfPlant] <- as.numeric(data[18,column])
      envToSet$Twarm[indexOfPlant] <- as.numeric(data[19,column])
      envToSet$GDD[indexOfPlant]   <- as.numeric(data[20,column])
      envToSet$beta1[indexOfPlant] <- as.numeric(data[21,column])
      
      ##
      ## Crops
      ##
      
      envToSet$stressBeta0[indexOfPlant] <-as.numeric(data[23,column])
      envToSet$stressBeta1[indexOfPlant] <-as.numeric(data[24,column])

      envToSet$rgrowthc[indexOfPlant] <-as.numeric(data[25,column])
      envToSet$lotemp[indexOfPlant] <-as.numeric(data[26,column])
      envToSet$hitemp[indexOfPlant] <-as.numeric(data[27,column])
      envToSet$f1[indexOfPlant] <-as.numeric(data[28,column])
      envToSet$f2[indexOfPlant] <-as.numeric(data[29,column])
      envToSet$q10[indexOfPlant] <-as.numeric(data[30,column])
      envToSet$drought[indexOfPlant] <-as.numeric(data[31,column])
      envToSet$baset[indexOfPlant] <- as.numeric(data[32,column])
      envToSet$mxtmp[indexOfPlant] <-as.numeric(data[33,column])
      envToSet$tkill[indexOfPlant] <-as.numeric(data[34,column])
      envToSet$laicons[indexOfPlant] <-as.numeric(data[35,column])
      envToSet$allconsl[indexOfPlant] <-as.numeric(data[36,column])
      envToSet$allconss[indexOfPlant] <-as.numeric(data[37,column])
      envToSet$laimx[indexOfPlant] <-as.numeric(data[38,column])
      envToSet$arooti[indexOfPlant] <-as.numeric(data[39,column])
      envToSet$arootf[indexOfPlant] <- as.numeric(data[40,column])
      envToSet$aleaff[indexOfPlant] <-as.numeric(data[41,column])
      envToSet$astemf[indexOfPlant] <-as.numeric(data[42,column])
      envToSet$declfact[indexOfPlant] <-as.numeric(data[43,column])
      envToSet$fleafi[indexOfPlant] <-as.numeric(data[44,column])
      envToSet$ptemp[indexOfPlant] <-as.numeric(data[45,column])
      envToSet$pmintemp[indexOfPlant] <-as.numeric(data[46,column])
      
      # Agora o dia de plantar é definido pelos valores do arquivo de configuração
      envToSet$pmmin[indexOfPlant] <- pmMin #as.numeric(data[46,column])
      envToSet$pdmin[indexOfPlant] <- pdMin #as.numeric(data[47,column])
      
      envToSet$pcm[indexOfPlant] <- as.numeric(data[49,column])
      envToSet$pcd[indexOfPlant] <- as.numeric(data[50,column])
      envToSet$hybgdd[indexOfPlant] <-as.numeric(data[51,column])
      envToSet$gddmin[indexOfPlant] <-as.numeric(data[52,column])
      envToSet$mxgddgf[indexOfPlant] <-as.numeric(data[53,column])
      envToSet$mxdgfi[indexOfPlant] <-as.numeric(data[54,column])
      
      # O mxmat é igual o período máximo do ciclo.
      envToSet$mxmat[indexOfPlant] <- cycleLength  # as.numeric(data[54,column])
      
      envToSet$lfemerg[indexOfPlant] <-as.numeric(data[56,column])
      envToSet$grnfill[indexOfPlant] <-as.numeric(data[57,column])
      envToSet$bfact[indexOfPlant] <-as.numeric(data[58,column])
      envToSet$ztopmxPft[indexOfPlant] <-as.numeric(data[59,column])
      envToSet$cgrain[indexOfPlant] <- as.numeric(data[60,column])
      envToSet$convfact[indexOfPlant] <- as.numeric(data[61,column])
      envToSet$maxhi[indexOfPlant] <-as.numeric(data[62,column])
      envToSet$fyield[indexOfPlant] <-as.numeric(data[63,column])
      envToSet$cfrac[indexOfPlant] <-as.numeric(data[64,column])
      envToSet$fnlfmx[indexOfPlant] <-as.numeric(data[65,column])
      envToSet$fngrmx[indexOfPlant] <-as.numeric(data[66,column])
      envToSet$sratio[indexOfPlant] <-as.numeric(data[67,column])
      envToSet$rratio[indexOfPlant] <-as.numeric(data[68,column])
      envToSet$fnopt[indexOfPlant] <- as.numeric(data[69,column])
      envToSet$fngrain[indexOfPlant] <- as.numeric(data[70,column])
      
      envToSet$chiflz[indexOfPlant] <- as.numeric(data[71,column])
      envToSet$chifuz[indexOfPlant] <- as.numeric(data[72,column])
      
      if(type == simInstances[[simId]]$NATURAL_VEG) {
        envToSet$chiflz[indexOfPlant] <- 0
        envToSet$chifuz[indexOfPlant] <- 0
      }
      
      envToSet$rhovegvgin[indexOfPlant]  <- as.numeric(data[73,column])
      envToSet$rhovegvbin[indexOfPlant]  <- as.numeric(data[74,column])
      envToSet$rhovegirgin[indexOfPlant] <- as.numeric(data[75,column])
      envToSet$rhovegirbin[indexOfPlant] <- as.numeric(data[76,column])
      envToSet$tauvegvgin[indexOfPlant]  <- as.numeric(data[77,column])
      envToSet$tauvegvbin[indexOfPlant]  <- as.numeric(data[78,column])
      envToSet$tauvegirgin[indexOfPlant] <- as.numeric(data[79,column])
      envToSet$tauvegirbin[indexOfPlant] <- as.numeric(data[80,column])
      
      if(type == simInstances[[simId]]$NATURAL_VEG) {
        envToSet$rhovegvgin[indexOfPlant]  <- 0
        envToSet$rhovegvbin[indexOfPlant]  <- 0
        envToSet$rhovegirgin[indexOfPlant] <- 0
        envToSet$rhovegirbin[indexOfPlant] <- 0
        envToSet$tauvegvgin[indexOfPlant]  <- 0
        envToSet$tauvegvbin[indexOfPlant]  <- 0
        envToSet$tauvegirgin[indexOfPlant] <- 0
        envToSet$tauvegirbin[indexOfPlant] <- 0
      }

            
      # TODO: Estava calculando assim nas versões anteriores. Mantém fazendo esse calculo?
      # envToSet$pcm[indexOfPlant] <- as.integer((((envToSet$pmmin[indexOfPlant] + envToSet$mxmat[indexOfPlant]/30)-1) %% 12) + 1)
      # envToSet$pcd[indexOfPlant] <- envToSet$pdmin[indexOfPlant]
      
      # TODO: Estava calculando assim nas versões anteriores. Mantém fazendo esse calculo?
      # envToSet$pcm[indexOfPlant] <- as.integer((((envToSet$pmmin[indexOfPlant] + envToSet$mxmat[indexOfPlant]/30)-1) %% 12) + 1)
      envToSet$pcm[indexOfPlant] <- envToSet$pmmin[indexOfPlant] 
      envToSet$pcd[indexOfPlant] <- envToSet$pdmin[indexOfPlant]
      
      ##
      ## Adicional
      ##
      # TODO: Criar a lista de nomes no modelo. O usuário deve listar os parâmetros adicionais.
      # seta os parâmetros adicionais automaticamente
      # if(!is.na(as.numeric(data[71,column]))) {
      n <- 81
      # funcao mandar em parametro nome da variavel em nome da coluna
      
      # TODO: Leandro, ler os arquivos especificos das culturas dentro dos modelos para cada uma delas
      if(simInstances[[simId]]$plantList[[i]]$type != simInstances[[simId]]$NATURAL_VEG) {
        
        if (simInstances[[simId]]$config[[paste0("plant", i)]]$name == "soybean") {
          
          # environment(readSoybeanParams) <- simInstances
          
          source("./R/CropModels/Soybean/readSoybeanParams.R")
          
          if(!is.na(as.character(data[n,column])) && (!grepl("^[0-9]*$", as.character(data[n,column]), perl = T))){
            readSoybeanParams(pathExcel = as.character(data[n,column]) ,simInstances = simInstances,column = column , simId = simId , i = i)#pathExcel = as.character(data[n,column]), filePath = "SBGRO047", coluna = column, varSolo = "BR0001", simInstances, simId, i)
            n <- n + 1
          }
        } else if(simInstances[[simId]]$config[[paste0("plant", i)]]$name == "forage") {
          
          source("./R/CropModels/PerennialForage/readForageParams.R")
          
          if(!is.na(as.character(data[n,column])) && (!grepl("^[0-9]*$", as.character(data[n,column]), perl = T))){
            readForageParams(pathExcel = as.character(data[n,column]) ,simInstances = simInstances,column = column , simId = simId , i = i)#pathExcel = as.character(data[n,column]), filePath = "SBGRO047", coluna = column, varSolo = "BR0001", simInstances, simId, i)
            n <- n + 1
          }
          ReadForageHarvData(simId, simInstances = simInstances[[simId]])
        }
        while(!is.na(as.character(data[n,1]))) {
          if(!is.na(as.numeric(data[n,column]))) { 
            simInstances[[simId]]$plantList[[i]]$params[[as.character(data[n,1])]] <- as.numeric(data[n,column])
          }
          n <- n + 1
        }
        
      }
      
    }
  }
}