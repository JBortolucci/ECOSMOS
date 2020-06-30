sysName <- Sys.info()['sysname']

if(sysName == "Windows") {
  
  dyn.load("inst/dynamic_libraries/win_x86_64/soil/soil.dll")
  
} else if(sysName == "Linux") {
  dyn.load("inst/dynamic_libraries/linux_x86_64/canopy/canopy.so")
  dyn.load("inst/dynamic_libraries/linux_x86_64/radiation/radiation.so")
  dyn.load("inst/dynamic_libraries/linux_x86_64/snow/snow.so")
  dyn.load("inst/dynamic_libraries/linux_x86_64/soil/soil.so")
  dyn.load("inst/dynamic_libraries/linux_x86_64/vegetation/vegetation.so")
  dyn.load("inst/dynamic_libraries/linux_x86_64/turvap/turvap.so")
  # dyn.load("inst/dynamic_libraries/linux_x86_64/weather/daily.so")
}

source("R/Lsxmain/Soil/soilctl.R")
# source("R/compiledCanopy.R")
# source("R/compiledRadiation.R")
# source("R/compiledSnow.R")
# source("R/compiledVegetation.R")
# source("R/compiledWeather.R")


