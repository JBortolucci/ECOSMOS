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

source("./R/Simulator/RunSimulation.R")

# ###############################################################################################
# Create or load a model through ecosmos interface (set of tools to create a user defined model).
# Warning: This step is mandatory because simulator no longer has built-in implementation of plant functional types.
# .
# .
# TODO: Implement interface for model creation and coupling (from file).

# User specific filesy
source("./R/NaturalVegModels/C4Grass.R")
source("./R/NaturalVegModels/C3Grass.R")
source("./R/NaturalVegModels/Upper.R")
# source("./R/CropModels/Eucalipto/EucaliptoModel.R")

CreateModel(nameArg           = "eucalipto",
            typeArg           = simDataVars$CROPS,
            canopyArg         = simDataVars$UPPER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = EucaliptoModel)

CreateModel(nameArg           = "C4Grass",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C4,
            ModelArg          = C4GrassPheno)

CreateModel(nameArg           = "C3Grass",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = C3GrassPheno)

CreateModel(nameArg           = "CDShrubs",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = C3GrassPheno)

CreateModel(nameArg           = "EShrubs",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$LOWER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = C3GrassPheno)

CreateModel(nameArg           = "BroadleafE",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$UPPER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = UpperPheno)

CreateModel(nameArg           = "BroadleafDD",
            typeArg           = simDataVars$NATURAL_VEG,
            canopyArg         = simDataVars$UPPER,
            carbonFixationArg = simDataVars$C3,
            ModelArg          = UpperPheno)


ReadGlobalParamsFromFile("inst/global_params.csv")


######################
# Configure simulation
ConfigSimulationFromFile("inst/Template_Simulation_Control_ECOSMOS_old.csv", "inst/plant_params.csv", "D:/input_xavier/")
# ConfigSimulationFromFile("inst/Template_Simulation_Control_ECOSMOS_old.csv", "inst/plant_params_C3Grass.csv", "D:/input_xavier/")
# ConfigSimulationFromFile("inst/Template_Simulation_Control_NatVeg.csv", "inst/plant_params_CDShrubs.csv", "D:/input_xavier/")
# ConfigSimulationFromFile("inst/Template_Simulation_Control_NatVeg.csv", "inst/plant_params_EShrubs.csv", "D:/input_xavier/")
# ConfigSimulationFromFile("inst/Template_Simulation_Control_NatVeg.csv", "inst/plant_params_BroadleafE.csv", "D:/input_xavier/")
# ConfigSimulationFromFile("inst/Template_Simulation_Control_NatVeg.csv", "inst/plant_params_BroadleafDD.csv", "D:/input_xavier/")
# 


#################
# Run simulation. 
# Warning: The simulation will run for configuration previously defined, if no configuration is provided, the default config will be 
# setted automatically. Be aware, this can result in unexpected output, for security read the documetation for more details about 
# default configs.

RunSimulation(parallel = F, compiled = T)

