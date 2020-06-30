
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
  
}