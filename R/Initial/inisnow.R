# ---------------------------------------------------------------------
# subroutine inisnow
# ---------------------------------------------------------------------
# does initialization for snow model
inisnow <- function() {
  
  # rhos is density of snow
  assign("rhos",  0.15 * rhow, envir = env)
  
  # consno is thermal conductivity of snow
  assign("consno",  0.20, envir = env)
  
  # hsnotop is "adaptive-grid" thickness of top snow layer
  assign("hsnotop", 0.1, envir = env)
  
  # hsnomin is minimum total snow thickness. total thickness
  # is constrained to hsnomin for less than 100% cover. (hsnomin
  # should be ge nsnolay*hsnotop for vadapt to work properly.)
  
  assign("hsnomin", max (0.3, nsnolay * hsnotop), envir = env)
  
  # fimin and fimax are minimum and maximum snowcover fractions
  assign("fimin", 0.00002 * (dtime / 1800.) * (0.3 / hsnomin), envir = env)
  assign("fimax", 1.000, envir = env)
  
  # z0sno is roughness lenth of snow cover
  assign("z0sno", 0.0005, envir = env)
  
}
