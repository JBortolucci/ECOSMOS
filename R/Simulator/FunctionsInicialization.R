
# Initialize all functions used in ECOSMOS.
# TODO: Create a NAMESPACE for it.

source("R/Input/ReadMap.R")

source("R/Simulator/utilities.R")

# TODO: Check how to compile dll/so in case they do not exist in the folder.
# compile_fortran()
source("R/Dynaveg/existence.R")
source("R/Dynaveg/climanl.R")
source("R/Lsxmain/comsat.R")
source("R/Input/ReadDailyStationData.R")
source("R/Weather/UseDailyStationData.R")
source("R/Input/ReadMethourlyData.R")
source("R/Weather/UseMethourlyData.R")
source("R/Input/ReadDailyIrrigationData.R") # Henrique & Leandro: irrigation feature [2020-11-06]
<<<<<<< HEAD
=======
source("R/Input/ReadDailyFertilizationData.R") # Henrique & Leandro: fertilization feature [2020-11-30]
source("R/CropControl/UseDailyFertilizationData.R")
>>>>>>> Perennial_Forage

# UseDailyStationData = cmpfun(UseDailyStationData)
# UseMethourlyData    = cmpfun(UseMethourlyData)

# source("R/rd_param.R")
source("R/Lsxmain/Soil/soilbgc.R")
source("R/Lsxmain/Soil/SoilbgcModel.R")
source("R/Weather/diurnal.R")
source("R/Weather/diurnalmet.R")
# source("R/weather/daily.R")

# soilbgc      = cmpfun(soilbgc)
# SoilbgcModel = cmpfun(SoilbgcModel)
# diurnal      = cmpfun(diurnal)
# diurnalmet   = cmpfun(diurnalmet)

source("R/Initial/coldstart.R")
source("R/Initial/inisnow.R")
source("R/Initial/inisoil.R")
source("R/Initial/inisum.R")
source("R/Initial/inisurf.R")
source("R/Initial/initialcrop.R")
source("R/Initial/iniveg.R")
source("R/Initial/initial.R")

source("R/CropControl/cropupdate.R")
source("R/CropPhenoUpdate.R")

source("R/CropControl/irrigation.R")
source("R/Lsxmain/Canopy/nitrostress.R")
source("R/Dynaveg/vegmap.R")
source("R/Lsxmain/Soil/leaching.R")
source("R/Dynaveg//dynaveg.R")
source("R/Dynaveg/fire.R")

# irrigation  = cmpfun(irrigation)
# nitrostress = cmpfun(nitrostress)
# leaching    = cmpfun(leaching)
# dynaveg     = cmpfun(dynaveg)
# Cropupdate  = cmpfun(Cropupdate)

source("R/CropControl/ResetCropsAfterHarvest.R")

# ResetCropsAfterHarvest = cmpfun(ResetCropsAfterHarvest)

source("R/Stats/sumday.R")
source("R/Stats/summonth.R")
source("R/Stats/sumyear.R")

# sumday   = cmpfun(sumday)
# summonth = cmpfun(summonth)
# sumyear  = cmpfun(sumyear)

source("R/Lsxmain/Canopy/Physiology.R")
source("R/Lsxmain/Canopy/StomataC4Crops.R")
source("R/Lsxmain/Canopy/StomataC3Crops.R")
source("R/NAOUTILIZADOS/physiology/StomataC4Grass.R")
source("R/NAOUTILIZADOS/physiology/StomataC3Grass.R")

# Stomata2       = cmpfun(Stomata2)
# StomataC4Crops = cmpfun(StomataC4Crops)
# StomataC3Crops = cmpfun(StomataC3Crops)
# StomataC4Grass = cmpfun(StomataC4Grass)
# StomataC3Grass = cmpfun(StomataC3Grass)

# ## Funcoes Lsxmain em **R**
source("R/Lsxmain/Canopy/sumnow.R")

# sumnow = cmpfun(sumnow)

source("R/Lsxmain/Canopy/turcof.R")
source("R/Lsxmain/Canopy/fstrat.R")

source("R/Lsxmain/Canopy/turvap.R")
source("R/Lsxmain/Canopy/impexp.R")
source("R/Lsxmain/Canopy/impexp2.R")
source("R/Lsxmain/Canopy/linsolve.R")

source("R/Lsxmain/Radiation/irrad.R")
source("R/Lsxmain/Canopy/drystress.R")
source("R/Lsxmain/Canopy/noveg.R")
source("R/Lsxmain/Radiation/solsur.R")
source("R/Lsxmain/Radiation/solarf.R")
source("R/Lsxmain/Canopy/cascad2.R")
source("R/Lsxmain/Canopy/fwetcal.R")
source("R/Lsxmain/Canopy/cascade.R")
source("R/Lsxmain/Canopy/canini.R")
source("R/Lsxmain/Radiation/solalb.R")
source("R/Lsxmain/Radiation/solset.R")
source("R/Lsxmain/Soil/setsoi.R")

## Natural veg
source("R/PhenoUpdate.R")

# Funcoes Lsxmain em **C**
sourceCpp("R/Lsxmain/Comsat.cpp")
sourceCpp("R/Lsxmain/Canopy/DrystressCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/NovegCpp.cpp")
sourceCpp("R/Lsxmain/Radiation/SolsurCpp.cpp")
sourceCpp("R/Lsxmain/Radiation/SolsetCpp.cpp")
sourceCpp("R/Lsxmain/Radiation/SolarfCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/Cascad2Cpp.cpp")
sourceCpp("R/Lsxmain/Canopy/FwetcalCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/CascadeCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/CaniniCpp.cpp")
sourceCpp("R/Lsxmain/Radiation/IrradCpp.cpp")
sourceCpp("R/Lsxmain/Radiation/SolalbCpp.cpp")
sourceCpp("R/Lsxmain/Soil/SetsoiCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/TurcofCpp.cpp")
sourceCpp("R/Lsxmain/Canopy/TurvapCpp.cpp")
sourceCpp("R/Weather/DiurnalmetCpp.cpp")
sourceCpp("R/Weather/DiurnalCpp.cpp")
# sourceCpp("R/SumnowCpp.cpp")
# sourceCpp("R/SumdayCpp.cpp")

source("R/Simulator/FunctionSelector.R")

# Output
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("R/Simulator/Output/outputC.cpp")
source("R/Simulator/Output/outputUtils.R")

# load extern libraries
source("R/Simulator/loadCompiledLibraries.R")

source("R/Lsxmain/lsxmain.R")

# lsxmain = cmpfun(lsxmain)

#
# # # Simulator engine functions # # #
#

# Interface
source("R/Simulator/Interface/ModelAssembler.R")
# lsxmain = cmpfun(lsxmain)

source("R/Simulator/Interface/ReadConfigFromCSV.R")
# ReadConfigFromCSV = cmpfun(ReadConfigFromCSV)

source("R/Simulator/Interface/ConfigSimulationFromFile.R")
# ConfigSimulationFromFile = cmpfun(ConfigSimulationFromFile)

source("R/Simulator/ReadParams.R")

# ReadPlantParamsFromFile  = cmpfun(ReadPlantParamsFromFile)
# ReadGlobalParamsFromFile = cmpfun(ReadGlobalParamsFromFile)

# source("R/Simulator/Interface/SetValue.R")
# lsxmain = cmpfun(lsxmain)