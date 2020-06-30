#' VariablesInicialization
#'
#' Função que lê de um arquivo texto (.txt), as variáveis da simulação e às inicializa  
#'
#' @param filePath Caminho do arquivo texto que contém as variáveis da simulação
#'
#' @examples
#' VariablesInicialization("R/Simulator/AllVariables.txt")

VariablesInicialization <- function(filePath = "R/Simulator/AllVariables.txt") {
  
  
  linhas      <- file(filePath, "r")
  ret         <- lineRead(linhas)
  variavel    <- ret$line
  linha       <- ret$fullLine
  
  npft    <- c()
  nband   <- c()
  nsoilay <- c()
  ndat    <- c()
  auxnsoilay  <- 0
  total       <- 0
  
  repeat{
    if(is.null(variavel)) break()
    # Inicialização de Matriz com informação de quantidade de linhas, colunas e valor inicial.
    if(!identical(variavel, character(0))){
      total <- total + 1
      if(length(variavel) == 5){
        if(variavel[2] == "matrix") {
          if (grepl("(npft)|(nband)|(nsoilay)|(ndat)", linha)){
            if(grepl("(npft)", linha)){
              simDataVars[[variavel[1]]] <- matrix(data = as.numeric(variavel[5]), nrow = 1, ncol = 1)
              npft <- c(npft, variavel[1])
            }else if(grepl("(nband)", linha)){
              simDataVars[[variavel[1]]] <- matrix(data = as.numeric(variavel[5]), nrow = 1, ncol = 1)
              nband <- c(nband, variavel[1])
            }else if(grepl("(nsoilay)", linha)){
              simDataVars[[variavel[1]]] <- matrix(data = as.numeric(variavel[5]), nrow = 1, ncol = 1)
              nsoilay <- c(nsoilay, variavel[1])
            }else if(grepl("(ndat)", linha)){
              simDataVars[[variavel[1]]] <- matrix(data = as.numeric(variavel[5]), nrow = 1, ncol = 1)
              ndat <- c(ndat, variavel[1])
            }
          } else {
            simDataVars[[variavel[1]]] <- matrix(data = as.numeric(variavel[5]), nrow = as.numeric(variavel[3]), ncol = as.numeric(variavel[4]))
          }
        }
      }
      # Inicialização de vetor com informação do tamanho e valor inicial.
      else if(length(variavel) == 4){
        if(variavel[2] == "vector") {
          if (grepl("(npft)|(nband)|(nsoilay)|(ndat)", linha)){
            if(grepl("(npft)", linha)){
              simDataVars[[variavel[1]]] <- array(data = as.numeric(variavel[4]), 1)
              npft <- c(npft, variavel[1])
            }else if(grepl("(nband)", linha)){
              simDataVars[[variavel[1]]] <- array(data = as.numeric(variavel[4]), 1)
              nband <- c(nband, variavel[1])
            }else if(grepl("(nsoilay)", linha)){
              simDataVars[[variavel[1]]] <- array(data = as.numeric(variavel[4]), 1)
              nsoilay <- c(nsoilay, variavel[1])
            }else if(grepl("(ndat)", linha)){
              simDataVars[[variavel[1]]] <- array(data = as.numeric(variavel[4]), 1)
              ndat <- c(ndat, variavel[1])
            }
          } else {
            simDataVars[[variavel[1]]] <- array(data = as.numeric(variavel[4]), as.numeric(variavel[3]))
          }
        }
      }
      else if(length(variavel) == 3){
        if(variavel[2] == "logical") {
          simDataVars[[variavel[1]]] <- as.logical(variavel[3])
        } else {
          # ERRO
        }
      }
      # Inicialização de Variavel com informação do valor inicial.
      else if(length(variavel) == 2){
        if( is.na(as.numeric(variavel[2])) ){
          simDataVars[[variavel[1]]]<- readVar(variavel[2])
        } else {
          simDataVars[[variavel[1]]]<- as.numeric(variavel[2])
        }
      }
    }
    ret         <- lineRead(linhas)
    variavel    <- ret$line
    linha       <- ret$fullLine
  }
  
  close(linhas)
  
  varGroups <- list(npft    = npft,
                    nband   = nband,
                    nsoilay = nsoilay,
                    ndat    = ndat)
  
  # TODO: Trocar globalenv por NAMESPACE
  assign("varGroups", varGroups, envir = globalenv())
  
}


# TODO: Decidir oque fazer com essa função (lineRead).
#       Realizar as alterações na função readItem (utilities.R)?
#       Utilizar essa?

lineRead <- function(fl) {
  
  line <- readLines(fl, n = 1)
  fullLine <- line
  
  if (identical(line, character(0)))
    return(NULL)
  
  # Skip empty or commented lines
  while(grepl("^[ \t]*#", line)){
    line <- readLines(fl, n = 1)
    fullLine <- line
  }
  
  # Remove whitespaces at the beginning of the line
  line <- gsub("^[ \t]*", "", line)
  
  # Remove comments at the end of the line
  line <- gsub("[ \t]*#.*|[ \t]*!.*$", "", line)
  
  # Splits line (if there are spaces or tabs)
  line <- unlist(strsplit(line, " "))
  
  # If line has been split in an array and contain empty elements, remove the
  # empty elements
  if(length(which(grepl("^[ \t]*$", line))) > 0)
    line <- line[-which(grepl("^[ \t]*$", line))]
  
  if ("-" %in% line){
    line <- line[-match("-", line)]
  }
  return(list(line=line, fullLine=fullLine))
}

# source("R/ConstantsDefinitions.R")
# source("R/common_blocks/com1d.R")
# source("R/common_blocks/comatm.R")
# source("R/common_blocks/combcs.R")
# source("R/common_blocks/combgc.R")
# source("R/common_blocks/comcrop.R")
# source("R/common_blocks/comhour.R")
# source("R/common_blocks/comhyd.R")
# source("R/common_blocks/comnitr.R")
# source("R/common_blocks/compar.R")
# source("R/common_blocks/compft.R")
# source("R/common_blocks/comsno.R")
# source("R/common_blocks/comsoi.R")
# source("R/common_blocks/comsum.R")
# source("R/common_blocks/comtex.R")
# source("R/common_blocks/comveg.R")
# source("R/common_blocks/comwork.R")
# source("R/NovasVariaveisPFT.R")



getOutputList <- function(arquivo){
  
  linhas <- file(arquivo, "r")
  listaInput <- c()
  nameVars <- c()
  variavel <- lineRead2(linhas)
  
  while( !is.null(variavel) ){
    nome <- variavel[2]
    tipo <- variavel[1]
    if(grepl("\\.h", nome)) {
      if(tipo == "0"){
        repeat {
          variavel <- lineRead2(linhas)
          nome <- variavel[2]
          tipo <- variavel[1]
          listaInput <- c(listaInput, variavel)
          if ((grepl("\\.h", nome)) || is.null(variavel)) break()
        }
      }
      else if(tipo == "1"){
        repeat {
          variavel <- lineRead2(linhas)
          nome <- variavel[2]
          tipo <- variavel[1]
          listaInput <- c(listaInput, variavel)
          if ((grepl("\\.h", nome)) || is.null(variavel)) break()
          nameVars <- c(nameVars, nome) 
        }
      }
      else if(tipo == "2"){
        repeat {
          variavel <- lineRead2(linhas)
          nome <- variavel[2]
          tipo <- variavel[1]
          listaInput <- c(listaInput, variavel)
          if ((grepl("\\.h", nome)) || is.null(variavel)) break()
          if (tipo == "1"){
            nameVars <- c(nameVars, nome) 
          }
        }
      }
      else{
        variavel <- lineRead2(linhas)
        nome <- variavel[2]
        tipo <- variavel[1]
      }
    }
  }
  close(linhas)
  return(nameVars)
}

lineRead2 <- function(fl) {
  
  line <- readLines(fl, n = 1, warn=FALSE)
  
  if (identical(line, character(0)))
    return(NULL)
  
  # Skip empty or commented lines
  while(grepl("^[ \t]*#|^[ \t]*c|^[ \t]*$", line))
    line <- readLines(fl, n = 1, warn=FALSE)
  
  # Remove whitespaces at the beginning of the line
  line <- gsub("^[ \t]*", "", line)
  
  # Remove comments at the end of the line
  line <- gsub("[ \t]*#.*|[ \t]*!.*$", "", line)
  
  # Splits line (if there are spaces or tabs)
  line <- unlist(strsplit(line, " |\t|<-|\\(|\\)"))
  
  # If line has been split in an array and contain empty elements, remove the
  # empty elements
  if(length(which(grepl("^[ \t]*$", line))) > 0)
    line <- line[-which(grepl("^[ \t]*$", line))]
  
  if ("-" %in% line){
    line <- line[-match("-", line)]
  }
  return(line)
}


