
CreateModel <- function(nameArg           = "unnamed", 
                        typeArg           = 0,
                        canopyArg         = 0,
                        carbonFixationArg = 0,
                        ModelArg          = NULL) {
  
  plant <- list(name           = nameArg,
                type           = typeArg,
                canopy         = canopyArg,
                carbonFixation = carbonFixationArg,
                Model          = ModelArg,
                active         = F,
                params         = list())
  
  # TODO: Put basePlantList in NAMESPACE
  if(is.null(basePlantList[[nameArg]])) {
     basePlantList[[nameArg]] <- plant
  } else {
    stop("You are trying to create a model that already exist in simulator!")
  }
  
}



# TODO: Criar outro script para o BIOMA
# funções e variáveis de montagem de biomas (modelos)

source("./R/NaturalVegModels/C4Grass/C4GrassModel.R")
source("./R/NaturalVegModels/C3Grass/C3GrassModel.R")
source("./R/NaturalVegModels/Upper.R")

biomaNames <- c("TROPEF", "TROPDF", "TEMPEBF", "TEMPECF", "TEMPDF", "BOREALEF",
                "BOREALDF", "MIXF", "SAVANA", "GRASS", "SHRUD", "SHRUO", "TUNDRA", 
                "DESERT", "DESERTP")

biomaModels <- list(TROPBET  = c("TROPBET"),
                    TROPDF   = c("TROPBDT"),
                    TEMPEBF  = c("TEMPBET"),
                    TEMPECF  = c("TEMPCET"),
                    TEMPDF   = c("TEMPBDT"),
                    BOREALEF = c("BOREALCET"),
                    BOREALDF = c("BOREALBDT", "BOREALBDT"),
                    MIXF     = c("TROPBET", "TROPBDT", "TEMPBET", "TEMPCET", "TEMPBDT", "BOREALCET", "BOREALBDT", "BOREALCDT"),
                    SAVANA   = c("TROPBDT", "EShrubs", "CDShrubs", "C4Grass", "C3Grass"),
                    GRASS    = c("C4Grass", "C3Grass"),
                    SHRUD    = c("EShrubs", "CDShrubs", "C4Grass", "C3Grass"),
                    SHRUO    = c("EShrubs", "CDShrubs", "C4Grass", "C3Grass"),
                    TUNDRA   = c("EShrubs", "CDShrubs", "C4Grass", "C3Grass"),
                    DESERT   = c("EShrubs", "CDShrubs", "C4Grass", "C3Grass"),
                    DESERTP  = c("EShrubs", "CDShrubs", "C4Grass", "C3Grass"))


IsBioma <- function(name) {
  for(i in 1:length(biomaNames)) {
    if(biomaNames[i] == name) return(TRUE)
  }
  return(FALSE)
}


CreateBuiltInNatVegModels <- function() {
  
  #  1: tropical broadleaf evergreen trees
  CreateModel(nameArg           =   "TROPBET",              
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  
  
  #  2: tropical broadleaf drought - deciduous trees
  CreateModel(nameArg           = "TROPBDT",                   
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  

  #  3:  temperate broadleaf evergreen trees
  CreateModel(nameArg           = "TEMPBET",                         
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)

  #  4: temperate conifer evergreen trees
  CreateModel(nameArg           = "TEMPCET",                  
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  

  #  5: temperate broadleaf cold - deciduous trees
  CreateModel(nameArg           = "TEMPBDT",                  
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  

  #  6: boreal conifer evergreen trees
  CreateModel(nameArg           = "BOREALCET",        
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  

  #  7: boreal broadleaf cold - deciduous trees
  CreateModel(nameArg           = "BOREALBDT",                  
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
  
  
  #  8: boreal conifer cold - deciduous trees
  CreateModel(nameArg           = "BOREALCDT",                 
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$UPPER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = UpperPheno)
 
  #  9: evergreen shrubs
  CreateModel(nameArg           = "EShrubs",
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$LOWER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = C3GrassPheno)
  
  # 10: cold - deciduous shrubs
  CreateModel(nameArg           = "CDShrubs",
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$LOWER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = C3GrassPheno)
  
  # 11: warm (c4) grasses
  CreateModel(nameArg           = "C4Grass",
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$LOWER,
              carbonFixationArg = simDataVars$C4,
              ModelArg          = C4GrassModel)
  
  # 12: cool (c3) grasses
  CreateModel(nameArg           = "C3Grass",
              typeArg           = simDataVars$NATURAL_VEG,
              canopyArg         = simDataVars$LOWER,
              carbonFixationArg = simDataVars$C3,
              ModelArg          = C3GrassModel)
  
}

# TROPBET  -> TROPBET
# TROPDF   -> TROPBDT
# TEMPEBF  -> TEMPBET
# TEMPECF  -> TEMPCET
# TEMPDF   -> TEMPBDT
# BOREALEF -> BOREALCET
# BOREALDF -> BOREALBDT, BOREALBDT
# MIXF     -> TROPBET, TROPBDT, TEMPBET, TEMPCET, TEMPBDT, BOREALCET, BOREALBDT, BOREALCDT
# SAVANA   -> TROPBDT, EShrubs, CDShrubs, C4Grass, C3Grass
# GRASS    -> C4Grass, C3Grass
# SHRUD    -> EShrubs, CDShrubs, C4Grass, C3Grass
# SHRUO    -> EShrubs, CDShrubs, C4Grass, C3Grass
# TUNDRA   -> EShrubs, CDShrubs, C4Grass, C3Grass
# DESERT   -> EShrubs, CDShrubs, C4Grass, C3Grass
# DESERTP  -> EShrubs, CDShrubs, C4Grass, C3Grass
