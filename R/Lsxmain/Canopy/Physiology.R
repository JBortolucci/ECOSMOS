
Stomata2 <- function(i) {
  
  environment(StomataC4Crops) <- env
  environment(StomataC3Crops) <- env
  environment(StomataC4Grass) <- env
  environment(StomataC3Grass) <- env
  
  if(plantList[[i]]$type == CROPS) {
    if(plantList[[i]]$carbonFixation == C3) {
      StomataC3Crops(i)
    } else {
      StomataC4Crops(i)
    }
  } else {
      if(plantList[[i]]$carbonFixation == C3) {
        StomataC3Grass(i)
      } else {
        StomataC4Grass(i)
      }
  }
  
  
}