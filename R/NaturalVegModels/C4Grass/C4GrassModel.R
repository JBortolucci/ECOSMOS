
source("R/NaturalVegModels/C4Grass/C4GrassIniVeg.R")
source("R/NaturalVegModels/C4Grass/C4GrassPheno.R")
source("R/CropPhenoUpdate.R")
# source("R/NaturalVegModels/C4Grass/C4GrassResidue.R")

C4GrassModel <- function(jday, index) {
  
  environment(C4GrassIniVeg)   <- env
  environment(C4GrassPheno)    <- env
  environment(CropPhenoUpdate) <- env
  
  Deadleaves <- list()
  Deadfroots <- list()
  
  assign('Deadleaves', Deadleaves, envir = env)
  assign('Deadfroots', Deadfroots, envir = env)
  
  C4GrassIniVeg(jday, index)
  C4GrassPheno(jday, index)
  CropPhenoUpdate()
  
}
