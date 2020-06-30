# Global Vars:
# disturbf  # annual fire disturbance regime (m2/m2/yr)
# firefac   # factor that respresents the annual average fuel dryness of a grid cell, and hence characterizes the readiness to burn
# totlit    # total carbon in all litter pools (kg_C m-2)

fire <- function() {
  
  burn <- firefac * min (1, totlit / 0.200)
  
  disturbf <- 1 - exp( - 0.5 * burn)
  
  disturbf <- max (0, min (1, disturbf))
  
  assign("disturbf", disturbf, envir = env)
  
  return()
}