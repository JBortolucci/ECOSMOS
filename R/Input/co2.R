# Global Vars:
# co2conc  # co2 concentration (mol/mol)

co2 <- function (co2init, co2conc, iyear) {
  # calculate co2 concentration for this year
  
  if(iyear < 1860) {
    co2conc <- co2init
  } else {
    # 1992 IPCC estimates
    #        iyr <- iyear - 1860 + 1
    #        co2conc <- (297.12 - 0.26716 * iyr +
    # > 0.0015368 * iyr ** 2 +
    # > 3.451e-5 * iyr ** 3) * 1e-6
    #
    # 1996 IPCC estimates
    
    iyr <- iyear - 1860 + 1
    co2conc <- (303.514 - 0.57881 * iyr +
                  0.00622 * iyr ** 2 +
                  1.3e-5 * iyr ** 3) * 1e-6
  }
  
  assign("co2conc", co2conc, envir = env)
}