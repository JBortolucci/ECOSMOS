# if no veg surfaces exist, set prog vars to nominal values
# 
# (sensible fluxes fsen[u,s,l], latent fluxes fvap[u,s,l]*, 
# temperature t[u,s,l], and intercepted liquid, snow amounts 
# wliq[u,s,l], wsno[u,s,l] have been calculated for a unit 
# leaf/stem surface, whether or not one exists.)

novegR <- function(envi){
  
  tav <- (1.-fi)*tg + fi*ti
  
  if (lai[2] == 0. | fu == 0.) {
    tu <- tav
    wliqu <- 0.
    wsnou <- 0.
  }
  
  if (sai[2] == 0. | fu == 0.) {
    ts <- tav
    wliqs <- 0.
    wsnos <- 0.
  }
  
  x <- 2.0 * (lai[1] + sai[1])
  y <- fl*(1.-fi)
  
  if (x == 0. | y == 0.) {
    tl <- tav 
    wliql <- 0.
    wsnol <- 0.
  }
  assign("tu", tu, envir = env)
  assign("wliqu", wliqu, envir = env)
  assign("wsnou", wsnou, envir = env)
  assign("ts", ts, envir = env)
  assign("wliqs", wliqs, envir = env)
  assign("wsnos", wsnos, envir = env)
  assign("tl", tl, envir = env)
  assign("wliql", wliql, envir = env)
  assign("wsnol", wsnol, envir = env)
}