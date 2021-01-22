

simDataVars$M2_AS_HA     <- 1/10000 #m2 as ha
simDataVars$KG_AS_TONNES <- 1/1000 #kg as tonnes
simDataVars$kg_C_M2_to_T_ha <- 10000/1000 #Tonnes per ha as kg per m2
simDataVars$G_AS_KG         <- 1/1000 # grams as kilograms
simDataVars$Sapwood     <- 0.0001
simDataVars$Heartwood   <- 0.0001
simDataVars$Deadcoroots <- 0.0001
simDataVars$tauleaf_branch <- 365



source("R/CropModels/Maize/MaizeGrowthPheno.R")
source("R/CropModels/Maize/MaizePlanting.R")
source("R/CropModels/Maize/MaizeCropresidue.R")


# TODO: remover index depois, passar as variáveis automaticamente.
MaizeModel <- function(year, month, day, index) {
  
  #TODO Victor, 'desliguei' o print e por hora aqui pras minhas simulações [2020-01-20]
  #cat('\r',paste(year, sprintf('%02d', month), sprintf('%02d', day), sep = '-'))
  
  # TODO: Verificar se há uma maneira de fazer isso automaticamente para funções do usuário.
  # Se for possível setar o environment fora da função, é possível fazer isso por meio de uma função
  # na qual o usuário passa a hierarquia do modelo por nome e o modelo é construido pelo simulador, 
  # isto é os assigns são passados automaticamente.
  #' Por exemplo:
  #' 
  #' Indica que o modelo MaizePlanting está dentro de MaizeModel.
  #' AddSubModel(MaizePlanting, MaizeModel)
  #'
  
  b1 <- plantList$maize$params$b1 # não usando
  b2 <- plantList$maize$params$b2 # não usando 
  
  
  environment(MaizePlanting)    <- env
  environment(MaizeGrowthPheno)   <- env
  environment(MaizeCropresidue) <- env
  
  depth <- numeric(nsoilay)
  
  # new michel
  # beta1[index] <- min(0.982 + idpp[index] * 0.00002, 0.995)
  beta1[index] <- min(0.995*(1-exp(-0.1*idpp[index])), 0.995) # funcao que altera vmax_pft do Maize em funcao do tempo
  assign("beta1", beta1[index], envir = env)

  totdepth <- 0
  for(k in 1: nsoilay) {
    totdepth <- totdepth + hsoi[k] * 100
  }
  # normalization factors
  frootnorm1 <- 1 - beta1[index] ^ totdepth
 
  # calculate rooting profiles
  for(k in 1:nsoilay) {
    if(k == 1) {
      depth[k] <- hsoi[k] * 100
      froot[k,1] <- 1 - beta1[index] ^ depth[k]
    } else {
      depth[k] <- depth[k - 1] + hsoi[k] * 100
      froot[k,1] <- (1 - beta1[index] ^ depth[k]) - (1 - beta1[index] ^ depth[k - 1])
    }
    froot[k,1] <- froot[k,1] / frootnorm1
  }
  assign("froot", froot, envir = env)

# ### using the database rooting proflies: BY MICHEL
#   # calculate rooting profiles
#   for(k in 1: nsoilay) {
#     froot[k,1] <- SRGF[K]/sum(SRGF)
#   }
#   assign("froot", froot, envir = env)
  
  MaizePlanting(year0, year, month, day, jday, ffact, index)
  
  MaizeGrowthPheno(year, year0, month, day, jday, index)
  
  MaizeCropresidue(year, year0, jday, index)
  
 
}


