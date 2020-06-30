# Global Vars:
# epsilon  # small quantity to avoid zero-divides and other
# tmelt    # freezing point of water (K)

impexp2 <- function (wimp, t, told, iter) {
  # sets the implicit vs explicit fraction in turvap calcs for
  # seaice or snow skin temperatures, to account for temperatures
  # of freezing / melting surfaces being constrained at the melt
  # point
  #
  # unlike impexp, don't have to allow for all h2o 
  # vanishing within the timestep
  #
  # wimp <- implicit fraction (0 to 1) (returned)
  #
  # for first iteration, set wimp to fully implicit, and return
  
  if(iter == 1) {
    wimp[] <- 1
    return(wimp)
  }
    
  if ((t - told) > epsilon) wimp <- (tmelt - told) /  (t - told)
    
  wimp <- max (0, min (1, wimp))
  
  return(wimp)
}