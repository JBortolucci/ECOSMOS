#####################################################################
#
#  ECOSMOS USE CASE
#
#  This file shows how to prepar a simulation using ECOSMOS package.
#
#  For more information visit the website: wwww.ecosmos.embrapa.br
#
#####################################################################

# Load ecosmos package
# library(Ecosmos)
# TODO: While package isn't ready source RunSimulation file.
library("parallel")
source("R/Simulator/RunSimulation.R")

# ###############################################################################################
# Create or load a model through ecosmos interface (set of tools to create a user defined model).
# Warning: This step is mandatory because simulator no longer has built-in implementation of the plant functional types.
# .
# .
# TODO: Implement interface for model creation and coupling (from file).

source("R/CropModels/OilPalm/OilPalmModel.R")

CreateModel(nameArg           = "oilpalm",
            typeArg           = simDataVars$CROPS,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = OilPalmModel)



ReadGlobalParamsFromFile("inst/global_params.csv")


######################
# Configure simulation
ConfigSimulationFromFile("inst/Template_Simulation_Control_ECOSMOS_oilpalm.csv", "inst/plant_params.csv", "inst/input_xavier/")

if(file.exists("output/outputHourly.M2006.dat")) system(command = 'rm output/outputHourly.M2006.dat')

HourlyStep <- function(simVars) {
  
  rn <- simVars$solad[1]*(1-simVars$asurd[1]) +
    simVars$solad[2]*(1-simVars$asurd[2]) +
    simVars$solai[1]*(1-simVars$asuri[1]) +
    simVars$solai[2]*(1-simVars$asuri[2]) +
    simVars$fira - simVars$firb
  
  hourlyOutputData <- paste(simVars$year, simVars$jday, simVars$step-1,
                            as.numeric(sprintf("%6.2f",rn)),
                            as.numeric(sprintf("%6.2f",-simVars$fsena)),
                            as.numeric(sprintf("%6.2f",-simVars$fvapa*simVars$hvap)),
                            as.numeric(sprintf("%6.2f",simVars$soihfl)),
                            as.numeric(sprintf("%10.6f",simVars$tneetot*1.0E6)),
                            as.numeric(sprintf("%10.6f",-simVars$fvapa)),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[1])),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[2])),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[3])),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[4])),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[5])),
                            as.numeric(sprintf("%10.6f",simVars$tsoi[6])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[1])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[2])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[3])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[4])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[5])),
                            as.numeric(sprintf("%10.6f",simVars$consoi[6])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[1])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[2])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[3])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[4])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[5])),
                            as.numeric(sprintf("%10.6f",simVars$wsoi[6])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[2])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[3])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[4])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[5])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[6])),
                            as.numeric(sprintf("%10.6f",simVars$hflo[7])),
                            as.numeric(sprintf("%10.6f",simVars$plai[simVars$currentPlant])),
                            sep = " ")

  write(x = hourlyOutputData, file = "output/outputHourly.M2006.dat", append = TRUE)
  
}


#################
# Run simulation.
# Warning: The simulation will run for configuration previously defined, if no configuration is provided, the default config will be
# setted automatically. Be aware, this can result in unexpected output, for security read the documetation for more details about
# default configs.

RunSimulation(parallel = F, compiled = T, OnEndHourlyStep = HourlyStep)
