
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