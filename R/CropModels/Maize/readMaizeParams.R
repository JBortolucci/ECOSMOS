library(readxl)
library(dplyr)

# TODO: verificar filePath, colocar um nome padrão
readMaizeParams <-function(pathExcel = "a",filePath = "BRFRM047", simInstances = new.env() ,column = "BR0001", simId = 1, i = 1){
  
  data <- read_excel(pathExcel)

  # if(!(coluna %in% colnames(data))) stop(paste0("The column ",coluna, " does not exist in parameters table."))
  
  simInstances[[simId]]$plantList[[i]]$params[["VARNAME"]]       <- as.character(data[which(data$`VAR#` %in% column)[1],2])
  simInstances[[simId]]$plantList[[i]]$params[["gddgerm"]] 	     <- as.character(data[which(data$`VAR#` %in% column)[1],3])
  simInstances[[simId]]$plantList[[i]]$params[["gddemerg"]]      <- as.numeric(data[which(data$`VAR#` %in% column)[1],4])
  simInstances[[simId]]$plantList[[i]]$params[["emerg_limit"]] 	 <- as.numeric(data[which(data$`VAR#` %in% column)[1],5])
  simInstances[[simId]]$plantList[[i]]$params[["gddf"]] 	       <- as.numeric(data[which(data$`VAR#` %in% column)[1],6])
  simInstances[[simId]]$plantList[[i]]$params[["gddt"]] 	       <- as.numeric(data[which(data$`VAR#` %in% column)[1],7])
  simInstances[[simId]]$plantList[[i]]$params[["laic"]] 	       <- as.numeric(data[which(data$`VAR#` %in% column)[1],8])
  simInstances[[simId]]$plantList[[i]]$params[["laim"]] 	       <- as.numeric(data[which(data$`VAR#` %in% column)[1],9])
  simInstances[[simId]]$plantList[[i]]$params[["sg"]] 	         <- as.numeric(data[which(data$`VAR#` %in% column)[1],10])
  simInstances[[simId]]$plantList[[i]]$params[["phyl"]] 	       <- as.numeric(data[which(data$`VAR#` %in% column)[1],11])
  simInstances[[simId]]$plantList[[i]]$params[["dsstop"]] 	     <- as.numeric(data[which(data$`VAR#` %in% column)[1],12])
  simInstances[[simId]]$plantList[[i]]$params[["ph1"]] 	         <- as.numeric(data[which(data$`VAR#` %in% column)[1],13])
  simInstances[[simId]]$plantList[[i]]$params[["ph2"]] 	         <- as.numeric(data[which(data$`VAR#` %in% column)[1],14])
  simInstances[[simId]]$plantList[[i]]$params[["G2"]] 	         <- as.numeric(data[which(data$`VAR#` %in% column)[1],15])
  simInstances[[simId]]$plantList[[i]]$params[["G5"]] 	         <- as.numeric(data[which(data$`VAR#` %in% column)[1],16])
  simInstances[[simId]]$plantList[[i]]$params[["efftrans"]] 	   <- as.numeric(data[which(data$`VAR#` %in% column)[1],17])
  
}

