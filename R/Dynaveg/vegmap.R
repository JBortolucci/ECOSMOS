# Global Vars:
# biomass   # total biomass of each plant functional type  (kg_C m-2)
# gdd0      # growing degree days > 0C 
# gdd5      # growing degree days > 5C 
# plai      # total leaf area index of each plant functional type
# totlail   # total leaf area index for the lower canopy
# totlaiu   # total leaf area index for the upper canopy
# vegtype0  # annual vegetation type - ibis classification

vegmap <- function() {
  
  # classify vegetation cover into standard ibis vegetation classes 
  
  # ---------------------------------------------------
  #  1: tropical evergreen forest / woodland
  #  2: tropical deciduous forest / woodland
  #  3: temperate evergreen broadleaf forest / woodland
  #  4: temperate evergreen conifer forest / woodland
  #  5: temperate deciduous forest / woodland
  #  6: boreal evergreen forest / woodland
  #  7: boreal deciduous forest / woodland
  #  8: mixed forest / woodland
  #  9: savanna
  # 10: grassland / steppe 
  # 11: dense shrubland
  # 12: open shrubland
  # 13: tundra
  # 14: desert 
  # 15: polar desert / rock / ice
  # 16: croplands
  # ---------------------------------------------------
  
  
  # determine total lai and tree, shrub, and grass fractions
  
  treelai <- totlaiu 
  shrublai <- plai[9] + plai[10]
  grasslai <- plai[11] + plai[12]
  
  # crop biomass -- as used as an overriding condition for
  # determining a vegetation class
  
  cropbio <- 0
  for(j in scpft:ecpft) { 
    cropbio <- cropbio + biomass[j]
  }
  
  totlai <- max (0.01, totlail + totlaiu)
  
  # determine dominant tree type by lai dominance
  domtree <- 0
  maxlai <- 0
  
  for(j in 1: 8) { 
    if(plai[j] > maxlai) {
      domtree <- j
      maxlai <- plai[j]
    }
  }
  
  # assign initial vegetation type
  vegtype0 <-  - 999.99
  
  # dominant type:  tropical broadleaf evergreen tree
  if(domtree  == 1) {
    if (treelai > 2.5)          vegtype0 <- 1  # tropical evergreen forest / woodland
    if (treelai <= 2.5)         vegtype0 <- 9  # savanna
    if(treelai <= 0.5) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # dominant type:  tropical broadleaf drought - deciduous tree
  if(domtree  == 2) {
    if (treelai > 2.5)          vegtype0 <- 2  # tropical deciduous forest / woodland
    if (treelai <= 2.5)         vegtype0 <- 9  # savanna
    if(treelai <= 0.5) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # dominant type:  warm - temperate broadleaf evergreen tree
  if(domtree  == 3) {
    if (treelai > 2.5)          vegtype0 <- 3  # temperate evergreen broadleaf forest / woodland
    if (treelai <= 2.5)         vegtype0 <- 9  # savanna
    if(treelai <= 0.5) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # dominant type:  temperate conifer evergreen tree
  if(domtree  == 4) {
    if (treelai > 1.5)          vegtype0 <- 4  # temperate evergreen conifer forest / woodland
    if (treelai <= 1.5)         vegtype0 <- 9  # savanna
    if(treelai <= 0.5) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # dominant type:  temperate broadleaf deciduous tree
  if(domtree  == 5) {
    if (treelai > 1.5)         vegtype0 <- 5  # temperate deciduous forest / woodland
    if (treelai <= 1.5)         vegtype0 <- 9  # savanna
    if(treelai <= 0.5) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # dominant type:  boreal conifer evergreen tree
  if (domtree  == 6)             vegtype0 <- 6  # boreal evergreen forest / woodland
  
  #       if (domtree  == 6) then
  #         if (treelai > 1)         vegtype0[i] <- 6  ! boreal evergreen forest / woodland
  #         if (treelai <= 1) then
  #           if (grasslai >= shrublai) vegtype0[i] <- 10  ! grassland
  #           if (shrublai >= grasslai) vegtype0[i] <- 11  ! closed shrubland
  #         }
  #       }
  
  # dominant type:  boreal broadleaf cold - deciduous tree
  
  if (domtree  == 7)             vegtype0 <- 7  # boreal deciduous forest / woodland
  
  #       if (domtree  == 7) then
  #         if (treelai > 1)         vegtype0[i] <- 7  ! boreal deciduous forest / woodland
  #         if (treelai <= 1) then
  #           if (grasslai >= shrublai) vegtype0[i] <- 10  ! grassland
  #           if (shrublai >= grasslai) vegtype0[i] <- 11  ! closed shrubland
  #         }
  #       }
  
  # dominant type:  boreal conifer cold - deciduous tree
  if (domtree  == 8)             vegtype0 <- 7  # boreal deciduous forest / woodland
  
  #       if (domtree  == 8) then
  #         if (treelai > 1)         vegtype0[i] <- 7  ! boreal deciduous forest / woodland
  #         if (treelai <= 1) then
  #           if (grasslai >= shrublai) vegtype0[i] <- 10  ! grassland
  #           if (shrublai >= grasslai) vegtype0[i] <- 11  ! closed shrubland
  #         }
  #       }
  
  # temperate/boreal forest mixtures
  if((domtree  >= 4) && (domtree  <= 8)) {
    ratio <- (plai[5] + plai[7] + plai[8]) /  
      (plai[4] + plai[5] + plai[6] +  
         plai[7] + plai[8])
    if(treelai > 1) {
      if ((ratio > 0.45) && (ratio < 0.55)) vegtype0 <- 8
    }
    if((domtree  <= 5) && (treelai <= 1)) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # no tree is dominant
  if(domtree  == 0) {
    if (treelai > 1)         vegtype0 <- 9  # savanna
    if(treelai <= 1) {
      if (grasslai >= shrublai) vegtype0 <- 10  # grassland
      if (shrublai >= grasslai) vegtype0 <- 11  # closed shrubland
    }
  }
  
  # overriding vegtation classifications
  if (totlai < 1)            vegtype0 <- 12  # open shrubland
  if (totlai <= 0.4)            vegtype0 <- 14  # desert
  
  # overriding climatic rules
  if(gdd5 < 350) {
    if (totlai >= 0.4)          vegtype0 <- 13  # tundra
    if (totlai < 0.4)          vegtype0 <- 15  # polar desert
  }
  
  if (gdd0 < 100)         vegtype0 <- 15  # polar desert
  
  if (cropbio > 0)         vegtype0 <- 16  # croplands
  
  
  assign("vegtype0", vegtype0, envir = env)
  
  # return to the main program
  return()
  
}
