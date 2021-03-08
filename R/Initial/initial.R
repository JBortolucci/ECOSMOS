

initial <- function(isimveg) {  #  0 = static veg, 1 = dynamic veg, 2 = dynamic veg with cold start


  environment(inisurf)   <- env
  environment(inisnow)   <- env
  environment(inisoil)   <- env
  environment(iniveg)    <- env
  environment(inisum)    <- env
  

  # initialize physical consts, dimensions, unit numbers, lsx model
  inisurf()
  
  # initialize snow model
  inisnow()  
  
  # initialize soil model
  inisoil()
  
  # initialize vegetation parameters
  iniveg (isimveg)
  
  # initialize variables for time averaging
  inisum()
  
  
}