
###############################################################################################################
#                         
#  Lê informações necessárias para simulação. Transforma o conjunto de pontos extraidos de um grid
#  em uma lista. Cada ponto é armazenado em uma estrutura que agrega todas as informações do mapa. 
#  (ver info's. na estrutura a seguir)
#
###############################################################################################################

# Source nas funções para leitura dos arquivos netcdf
source("R/Simulator/ReadNcData.R")

# Modelo da estrutura do ponto              
#                                           Geo ref.
#                                           ###################################################
#                                           #                                                 # 
# struct Ponto {                            #                          (LAT)                  #
#                                           #       N                  89.75                  #  
#   geo infos:                              #       \                    \                    #
#     lat (float)                           #  W -- o -- E    -179.75 -- o -- 179.75 (LON)    #
#     lon (float)                           #       \                    \                    #    
#     lonIndex (int)                        #       S                 -89.75                  #    
#     latIndex (int)                        #                                                 #
#                                           ###################################################
#     topography (int)
#     vegetation (int)
#     deltat     (float)
#     sand      (vector)
#     clay      (vector)
#
# }

# Cria lista dos pontos a partir das coordenadas geográficas (north, south, east, west)

# north <- -23.25 
# south <- -23.25 
# west  <- -48.75
# east  <- -48.75 

# teste 1
# north  <- -23.25  
# south  <- -23.75  
# west   <- -48.75  
# east   <- -48.25  

##############################################################
#                                                            #
#   Lê land mask netcdf e calcula variáveis gerais do grid   #
#                                                            #   
##############################################################

GetPointsFromRect <- function(north, south, west, east) {
  
  landMaskData <- ReadLandMaskFile()
  
  #### get nearest point ######################
  
  if(!simDataVars$gridSolved) {
    
    lats <- expand.grid(landMaskData$latscale, landMaskData$lonscale)[["Var1"]]
    lons <- expand.grid(landMaskData$latscale, landMaskData$lonscale)[["Var2"]]

    
    for(i in 1:length(lats)) {
      idx <- c(which(landMaskData$latscale == lats[i]), which(landMaskData$lonscale == lons[i]))
      if(landMaskData$maskValues[idx[2], idx[1]] == 0) {
        lats[i] <- 99999999
        lons[i] <- 99999999
      }
    }
    
    simDataVars$lats <- lats
    simDataVars$lons <- lons
    
    simDataVars$gridSolved <- T
    
  }
  
  dx <- (west - simDataVars$lons)^2
  dy <- (north - simDataVars$lats)^2
  
  dist <- dx + dy
  
  minDistIdx <- which(dist == min(dist))
  
  nearestLat <- simDataVars$lats[minDistIdx[1]]
  nearestLon <- simDataVars$lons[minDistIdx[1]]
  
  north <- nearestLat
  west  <- nearestLon
  
  print(paste(north, west, "netcdf",sep="/"))
  
  #############################################
  
  # JAIR: Sempre será 1
  # total de pontos para latitude e longitude
  totalLat <- 1#(north - south) / landMaskData$yres + 1
  totalLon <- 1#(east - west) / landMaskData$xres   + 1
  
  # primeiro indice do recorte do mapa (top left)
  mapInitialIndex        <- c(which(landMaskData$latscale == north), which(landMaskData$lonscale == west))
  mapFinalIndex          <- c(mapInitialIndex[1]+totalLat-1, mapInitialIndex[2]+totalLon-1)
  names(mapInitialIndex) <- c("lat", "lon")
  names(mapFinalIndex)   <- c("lat", "lon")
  
  # obtém valores da máscara
  mask <- landMaskData$maskValues[mapInitialIndex["lon"]:mapFinalIndex["lon"], mapInitialIndex["lat"]:mapFinalIndex["lat"]]
  
  # valores NA são convertidos para 0
  mask[is.na(mask)] <- 0
  
  # calcula número total de pontos válidos
  totalPoints <- sum(mask)
  
  # Ponto base para inicializar a lista
  basePoint <- list(coord = c(0, 0), lonIndex = 0,  latIndex = 0, xintopo = 0, xinveg = 0, deltat = 0, sand = 0, clay = 0)
  
  # cria uma lista n pontos
  pointList <- rep(list(basePoint), length = totalPoints)
  
  # nomeia os pontos da lista e.g. point1, point2, ... , pointn
  pointListNames <- numeric(totalPoints)
  for(i in 1:totalPoints) pointListNames[i] <- paste0("point", i)
  names(pointList) <- pointListNames
  
  
  ###################################
  #                                 #
  #   Lê demais arquivos netcdf     #
  #                                 #  
  ###################################
  
  # topografia
  topographyData <- ReadTopographyFile(start = c(mapInitialIndex["lon"], mapInitialIndex["lat"], 1, 1), count = c(totalLon, totalLat, 1, 1))
  
  # vegetation
  # TODO: Chamar depois da configuração da simulação (descomentar if)
  # if(isimveg == STATIC_VEG || isimveg == DYNAMIC_VEG) {
  vegtypeData <- ReadVegetationFile(start = c(mapInitialIndex["lon"], mapInitialIndex["lat"], 1, 1), count = c(totalLon, totalLat, 1, 1))
  #}
  
  
  # sand
  sandData <- ReadSandFile(start = c(mapInitialIndex["lon"], mapInitialIndex["lat"], 1, 1), count = c(totalLon, totalLat, 6, 1))
  
  # clay
  clayData <- ReadClayFile(start = c(mapInitialIndex["lon"], mapInitialIndex["lat"], 1, 1), count = c(totalLon, totalLat, 6, 1))
  
  
  # Percorre mapa recortado e armazena informações dos pontos (de todos os netcdf's)
  for(i in 1:totalLat) {
    for(j in 1:totalLon) {
      
      mapIndexLat <- mapInitialIndex[[1]]+i-1 
      mapIndexLon <- mapInitialIndex[[2]]+j-1 
      
      maskValue   <- landMaskData$maskValues[mapIndexLon, mapIndexLat]
      
      # se ponto é diferente de 1 ignora
      if(maskValue != 1) {
        next
      }
      # valor da latitude e longitude do ponto
      lat         <- landMaskData$latscale[mapIndexLat]
      lon         <- landMaskData$lonscale[mapIndexLon]
      
      xintopo     <- topographyData[j, i]
      xinveg      <- vegtypeData[j, i]
      sand        <- sandData[j, i,] 
      clay        <- clayData[j, i,]
      
      coord        <- list(lat, lon)
      names(coord) <- c("lat", "lon")
      pointAux     <- list(coord = coord, lonIndex = mapIndexLon, latIndex = mapIndexLat, xintopo = xintopo, xinveg = xinveg, deltat = deltat, sand = sand, clay = clay)
      
      # Após obter todas as informações do ponto, adiciona ponto na lista
      pointList[[j + totalLat * (i-1)]] <- pointAux
      
    }
  }
  
  
  return(pointList)
}







