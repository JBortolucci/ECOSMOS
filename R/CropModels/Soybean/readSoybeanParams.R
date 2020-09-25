library(readxl)
library(dplyr)


readSoybeanParams <-function(pathExcel = "a",filePath = "SBGRO047", simInstances = new.env() ,varSolo = "BR0001", simId = 1, i = 1){
  
  data <- read_excel(pathExcel, sheet = paste0(filePath,'.SPE'))
  
  # if(!(coluna %in% colnames(data))) stop(paste0("The column ",coluna, " does not exist in parameters table."))
  
  n <-1
  
  while(!(is.na(as.character(data[n,"Parameter"])) && is.na(as.character(data[n,"value"])))){
    # if(!(startsWith(as.character(data[n,"Parameter"]), '!'))){
      if(is.na(as.character(data[n+1,"Parameter"])) && !is.na(as.character(data[n+1,"value"]))){
        vector <- c()
        vectorName <- as.character(data[n,"Parameter"])
        #atribui ao vetor - primeiro valor
        vector <- c(vector, as.numeric(data[n,"value"]))
        n <- n + 1
        while(is.na(as.character(data[n,"Parameter"])) && !is.na(as.character(data[n,"value"]))){
          #atribui ao vetor - proximos valores
          vector <- c(vector, as.numeric(data[n,"value"]))
          if(is.na(as.character(data[n+1,"Parameter"]))) {
            n <- n + 1
          }else{
            simInstances[[simId]]$plantList[[i]]$params[[vectorName]] <- vector
            break()
          }
        }
      }else{
        #Variavel normal
        if (grepl("^[0-9]*$", as.character(data[n,"value"]), perl = T)){
          simInstances[[simId]]$plantList[[i]]$params[[as.character(data[n,"Parameter"])]] <- as.numeric(data[n,"value"])
        }else{
          simInstances[[simId]]$plantList[[i]]$params[[as.character(data[n,"Parameter"])]] <- as.character(data[n,"value"])
        }
      }
    # }
    n <- n + 1
  }
  
  data <- read_excel(pathExcel, sheet = paste0(filePath,'.CUL'))
  
  simInstances[[simId]]$plantList[[i]]$params[["VARNAME"]] <- as.character(data[which(data$`VAR#` %in% varSolo)[1],2])
  simInstances[[simId]]$plantList[[i]]$params[["ECO"]] 	   <- as.character(data[which(data$`VAR#` %in% varSolo)[1],3])
  simInstances[[simId]]$plantList[[i]]$params[["CSDL"]]    <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],4])
  simInstances[[simId]]$plantList[[i]]$params[["PPSEN"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],5])
  simInstances[[simId]]$plantList[[i]]$params[["EM_FL"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],6])
  simInstances[[simId]]$plantList[[i]]$params[["FL_SH"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],7])
  simInstances[[simId]]$plantList[[i]]$params[["FL_SD"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],8])
  simInstances[[simId]]$plantList[[i]]$params[["SD_PM"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],9])
  simInstances[[simId]]$plantList[[i]]$params[["FL_LF"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],10])
  simInstances[[simId]]$plantList[[i]]$params[["LFMAX"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],11])
  simInstances[[simId]]$plantList[[i]]$params[["SLAVR"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],12])
  simInstances[[simId]]$plantList[[i]]$params[["SIZLF"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],13])
  simInstances[[simId]]$plantList[[i]]$params[["XFRT"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],14])
  simInstances[[simId]]$plantList[[i]]$params[["WTPSD"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],15])
  simInstances[[simId]]$plantList[[i]]$params[["SFDUR"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],16])
  simInstances[[simId]]$plantList[[i]]$params[["SDPDV"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],17])
  simInstances[[simId]]$plantList[[i]]$params[["PODUR"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],18])
  simInstances[[simId]]$plantList[[i]]$params[["THRSH"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],19])
  simInstances[[simId]]$plantList[[i]]$params[["SDPRO"]] 	 <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],20])
  simInstances[[simId]]$plantList[[i]]$params[["SDLIP"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],21])

  # SBGRO047.ECO

  simInstances[[simId]]$plantList[[i]]$params[["ECONAME"]] <- as.character(data[which(data$`VAR#` %in% varSolo)[1],22])
  simInstances[[simId]]$plantList[[i]]$params[["MG"]] 	   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],23])
  simInstances[[simId]]$plantList[[i]]$params[["TM"]] 	   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],24])
  simInstances[[simId]]$plantList[[i]]$params[["THVAR"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],25])
  simInstances[[simId]]$plantList[[i]]$params[["PL_EM"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],26])
  simInstances[[simId]]$plantList[[i]]$params[["EM_V1"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],27])
  simInstances[[simId]]$plantList[[i]]$params[["V1_JU"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],28])
  simInstances[[simId]]$plantList[[i]]$params[["JU_R0"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],29])
  simInstances[[simId]]$plantList[[i]]$params[["PM06"]]    <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],30])
  simInstances[[simId]]$plantList[[i]]$params[["PM09"]]    <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],31])
  simInstances[[simId]]$plantList[[i]]$params[["LNGSH"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],32])
  simInstances[[simId]]$plantList[[i]]$params[["R7_R8"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],33])
  simInstances[[simId]]$plantList[[i]]$params[["FL_VS"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],34])
  simInstances[[simId]]$plantList[[i]]$params[["TRIFL"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],35])
  simInstances[[simId]]$plantList[[i]]$params[["RWDTH"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],36])
  simInstances[[simId]]$plantList[[i]]$params[["RHGHT"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],37])
  simInstances[[simId]]$plantList[[i]]$params[["R1PPO"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],38])
  simInstances[[simId]]$plantList[[i]]$params[["OPTBI"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],39])
  simInstances[[simId]]$plantList[[i]]$params[["SLOBI"]]   <- as.numeric(data[which(data$`VAR#` %in% varSolo)[1],40])
}

