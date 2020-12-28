
source("R/CropModels/Sugarcane/SugarcanePlanting.R")
source("R/CropModels/Sugarcane/SugarcanePheno.R")
source("R/CropModels/Sugarcane/SugarcaneResidue.R")

SugarcaneModel <- function(year, month, day, index) {
  
  environment(SugarcanePlanting)  <- env
  environment(SugarcanePheno)     <- env
  environment(SugarcaneResidue)   <- env
  
  SugarcanePlanting(year0, year, month, day, jday, ffact, index)
  
  SugarcanePheno(year, year0, month, day, jday, index)
  
  SugarcaneResidue(year, year0, jday, index)
  
  ###----------------------------------------------------
  ### Michel: Fraction of root in the first 30 cm from top soil
  sumfroot <- matrix(nrow = 1, ncol = 2)
  
  for(k in 1: nsoilay) {
    
    if(nslaym < depth[1]){
      
      sumfroot[1, 1] <- sum(froot[1:k, 1])
      sumfroot[1, 2] <- sum(froot[1:k, 2])
      
    } else if(depth[k] <= nslaym){
      
      sumfroot[1, 1] <- sum(froot[1:k, 1])
      sumfroot[1, 2] <- sum(froot[1:k, 2])
      
    } else if (depth[k] > nslaym && depth[k-1] <= nslaym) {
      
      sumfroot[1, 1] <- sum(sumfroot[1, 1], 1 - beta1 ** (nslaym - depth[k-1]))
      sumfroot[1, 2] <- sum(sumfroot[1, 2], 1 - beta2 ** (nslaym - depth[k-1]))
      break
      
    }
    
  }
  ###----------------------------------------------------
  
  assign("froot", froot, envir = env)
  assign("sumfroot", sumfroot, envir = env)
  
}