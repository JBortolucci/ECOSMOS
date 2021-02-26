
source("R/NaturalVegModels/C3Grass/C3GrassIniVeg.R")
source("R/NaturalVegModels/C3Grass/C3GrassPheno.R")
source("R/CropPhenoUpdate.R")
# source("R/NaturalVegModels/C3Grass/C3GrassResidue.R")

C3GrassModel <- function(jday, index) {
  
  environment(C3GrassIniVeg)   <- env
  environment(C3GrassPheno)    <- env
  environment(CropPhenoUpdate) <- env
  
  Deadleaves <- list()
  Deadfroots <- list()
  
  assign('Deadleaves', Deadleaves, envir = env)
  assign('Deadfroots', Deadfroots, envir = env)
  
  C3GrassIniVeg(jday, index)
  C3GrassPheno(jday, index)
  CropPhenoUpdate()
  
}
