

SoilbgcModel <- function(year, month, day) {
  
  environment(soilbgc) <- env
  
  spinfrac  <- 0.75
  spincons  <- 40.0
  eqyears   <- 15

  nspinsoil <- year0 + 50

  spinmax <- 1
  if(soilcspin == 1) {
    if ((year - year0) <= (spinfrac * (nspinsoil - year0 - eqyears))) {
      spinmax <- spincons
    } else if ((year - year0) < (nspinsoil - year0 -  eqyears)) {
      slope   <- spincons / ((nspinsoil - year0 - eqyears) - (spinfrac * (nspinsoil - year0 - eqyears)))
      spinmax <- floor (spincons - (slope * ((year - year0) - (spinfrac * (nspinsoil - year0 - eqyears)))))
      spinmax <- max(spinmax, 1)
    }
  }

  for(spin in 1:spinmax) {
    soilbgc(year, year0, month, day, jday, nspinsoil, spin, spinmax)
  }
  
}