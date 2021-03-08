
source("R/NaturalVegModels/C3Grass/C3GrassIniVeg.R")
source("R/NaturalVegModels/C3Grass/C3GrassPheno.R")
source("R/NaturalVegModels/C3Grass/C3GrassResidue.R")

C3GrassModel <- function(jday, index) {
  
  environment(C3GrassIniVeg)  <- env
  environment(C3GrassPheno)   <- env
  # environment(C3GrassResidue) <- env
  
  C3GrassIniVeg(jday, index)
  
}
