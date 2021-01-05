
GetCoords <- function(path = "") {
 
  fileNames  <- list.files(path = path, pattern=NULL, all.files=FALSE, full.names=FALSE)
 
  split1     <- strsplit(fileNames, "_")
  
  finalLat <- numeric(length(split1))
  finalLon <- numeric(length(split1))
  
  for(j in 1:length(split1)) {
    
    coords <- strsplit(split1[[j]][3], "")[[1]]
    
    lonNeg <- F
    latNeg <- F
    
    lon <- ""
    lat <- ""
    
    n <- 5
    
    # LONGITUDE
    
    if(coords[1] == "-") {
      lonNeg <- T
      n      <- 6
    }
    
    
    for(i in 1:n) { 
      lon <- paste(lon, coords[i])
    }
    
    # remove espacos
    lon <- as.numeric(gsub(" ", "",lon, fixed = TRUE))/1000
    
    n <- n + 1
    
    if(coords[n] == "-") {
      latNeg <- T
    }
    
    # LATITUDE
    
    while(coords[n] != ".") {
      lat <- paste(lat, coords[n])
      n <- n + 1
    }
    
    lat <- as.numeric(gsub(" ", "",lat, fixed = TRUE))/1000
    
    finalLat[j] <- lat
    finalLon[j] <- lon
    
  }
  
  coords <- list(finalLat, finalLon)
   
  return(coords)
  
}

GetNearestCoord <- function(finalLat, finalLon, inputLat = 0, inputLon = 0) { 
  
  dx <- (finalLon - inputLon)^2
  dy <- (finalLat - inputLat)^2
  dist <- dx + dy
  
  minDistIdx <- which(dist == min(dist))
  
  nearestLat <- finalLat[minDistIdx[1]]
  nearestLon <- finalLon[minDistIdx[1]]
  
  return(c(nearestLat, nearestLon))
  
}

# GetNearestCoord <- function(path = "", inputLat = 0, inputLon = 0, plotResult = F) {
#   
#   fileNames  <- list.files(path = path, pattern=NULL, all.files=FALSE, full.names=FALSE)
#   
#   # coordsFile <- file("coords.txt",open = "wt")
#   
#   split1     <- strsplit(fileNames, "_")
#   
#   finalLat <- numeric(length(split1))
#   finalLon <- numeric(length(split1))
#   
#   for(j in 1:length(split1)) {
#     
#     coords <- strsplit(split1[[j]][3], "")[[1]]
#     
#     lonNeg <- F
#     latNeg <- F
#     
#     lon <- ""
#     lat <- ""
#     
#     n <- 5
#     
#     # LONGITUDE
#     
#     if(coords[1] == "-") {
#       lonNeg <- T
#       n      <- 6
#     }
#     
#     
#     for(i in 1:n) { 
#       lon <- paste(lon, coords[i])
#     }
#     
#     # remove espa?os
#     lon <- as.numeric(gsub(" ", "",lon, fixed = TRUE))/1000
#     
#     n <- n + 1
#     
#     if(coords[n] == "-") {
#       latNeg <- T
#     }
#     
#     # LATITUDE
#     
#     while(coords[n] != ".") {
#       lat <- paste(lat, coords[n])
#       n <- n + 1
#     }
#     
#     lat <- as.numeric(gsub(" ", "",lat, fixed = TRUE))/1000
#     
#     finalLat[j] <- lat
#     finalLon[j] <- lon
# 
#   }
#   
#   if(plotResult) { 
#     plot(finalLon,finalLat, pch=16, col = "red")
#     points(inputLon, inputLat, pch=16, col = "blue")
#   }
#   
#   dx <- (finalLon - inputLon)^2
#   dy <- (finalLat - inputLat)^2
#   dist <- dx + dy
#   
#   minDistIdx <- which(dist == min(dist))
#   
#   nearestLat <- finalLat[minDistIdx[1]]
#   nearestLon <- finalLon[minDistIdx[1]]
#   
#   
#   if(plotResult)
#     points(nearestLon, nearestLat, pch=16, col = "green")
#   
#   return(c(nearestLat, nearestLon))
#   
# }






