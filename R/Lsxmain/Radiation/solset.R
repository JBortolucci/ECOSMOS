solsetR <- function(envi) {
  
  
  # zero albedos returned just as a niceity
  asurd[] <- 0
  asuri[] <- 0
  
  # zeros absorbed solar fluxes sol[u,s,l,g,i]1 since only points
  # with +ve coszen will be set in solarf, and since
  # sol[u,l,s,g,i]1 are summed over wavebands in solarf
  
  # similarly zero par-related arrays set in solarf for turvap
  solu[] <- 0
  sols[] <- 0
  soll[] <- 0
  solg[] <- 0
  soli[] <- 0
  solsoi[] <- 0
  
  topparu[] <- 0
  topparl[] <- 0
  
  # set canopy scaling coefficients for night-time conditions
  scalcoefl[] <- 0
  scalcoefu[] <- 0
  
  # set index of points with positive coszen
  nsol <- 0
  
  
  if(coszen > 0) {
    nsol <- nsol + 1
    indsol[nsol] <- 1
  }
  
  assign("nsol", nsol, envir = env)
  assign("asurd", asurd, envir = env)
  assign("asuri", asuri, envir = env)
  assign("solu", solu, envir = env)
  assign("sols", sols, envir = env)
  assign("soll", soll, envir = env)
  assign("solg", solg, envir = env)
  assign("soli", soli, envir = env)
  assign("solsoi", solsoi, envir = env)
  assign("topparu", topparu, envir = env)
  assign("topparl", topparl, envir = env)
  assign("scalcoefl", scalcoefl, envir = env)
  assign("scalcoefu", scalcoefu, envir = env)
  assign("indsol", indsol, envir = env)
  
}