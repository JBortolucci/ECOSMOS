# calculates fwet[u,s,l], the fractional areas wetted by 
# intercepted h2o (liquid and snow combined) -  the maximum value
# fmax (<1) allows some transpiration even in soaked conditions
# use a linear relation between fwet* and wliq*,wsno* (at least
#   for small values), so that the implied "thickness" is constant
# (equal to wliq*max, wsno*max as below) and the typical amount
# evaporated in one timestep in steph2o will not make wliq*,wsno*
#   negative and thus cause a spurious unrecoverable h2o loss
# (the max(w*max,.01) below numericaly allows w*max = 0 without
#    blowup.) in fact evaporation in one timestep *does* sometimes
# exceed wliq*max (currently 1 kg/m2), so there is an additional
# safeguard in turvap that limits the wetted-area aerodynamic
# coefficients suw,ssw,slw -- if that too fails, there is an 
# ad-hoc adjustment in steph2o2 to reset negative wliq*,wsno*
#   amounts to zero by taking some water vapor from the atmosphere.
# also sets rliq[u,s,l], the proportion of fwet[u,s,l] due to
# liquid alone. fwet,rliq are used in turvap, rliq in steph2o. 
# (so rliq*fwet, (1-rliq)*fwet are the fractional areas wetted
#    by liquid and snow individually.) if fwet is 0, choose rliq
# = 1 if t[u,s,l] ge tmelt or 0 otherwize, for use by turvap and
# steph2o in case of initial dew formation on dry surface.

fwetcalR <- function(envi) {
  
  fmax <- 0.08
  
  # upper leaves
  xliq <- wliqu / max(wliqumax, 0.01)
  xtot <- xliq + wsnou / max(wsnoumax, 0.01)
  
  fwetu <- min (fmax, xtot)
  
  rliqu <- xliq / max (xtot, epsilon)
  
  if (fwetu == 0.0) {
    rliqu <- 1.0
    if (tu < tmelt) {
      rliqu <- 0.0
    }
  }
  
  #  upper stems

  xliq <- wliqs / max (wliqsmax, 0.01)
  xtot <- xliq + wsnos / max (wsnosmax, 0.01)
  
  fwets <- min (fmax, xtot)
  rliqs <- xliq / max (xtot, epsilon)
  
  if (fwets == 0.0) {
    rliqs <- 1.0
    if (ts < tmelt) rliqs <- 0.0
  }
  
  # lower veg
  
  xliq <- wliql / max (wliqlmax, 0.01)
  xtot <- xliq + wsnol / max (wsnolmax, 0.01)
  
  fwetl <- min (fmax, xtot)
  rliql <- xliq / max (xtot, epsilon)
  
  if (fwetl == 0.) {
    rliql <- 1.0
    if (tl < tmelt) rliql <- 0.0
  }

  assign("fwetu", fwetu, envir = env)
  assign("rliqu", rliqu, envir = env)
  assign("fwets", fwets, envir = env)
  assign("rliqs", rliqs, envir = env)
  assign("fwetl", fwetl, envir = env)
  assign("rliql", rliql, envir = env)
  
}
