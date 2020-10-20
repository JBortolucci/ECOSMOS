


### End of companies params


source("R/CropModels/Soybean/SoybeanPlanting.R")
source("R/CropModels/Soybean/SoybeanCROPGRO.R")
source("R/CropModels/Soybean/SoybeanCropresidue.R")


# TODO: remover index depois, passar as variáveis automaticamente.
SoybeanModel <- function(year, month, day, index) {
  
  
  
  # TODO: Verificar se há uma maneira de fazer isso automaticamente para funções do usuário.
  # Se for possível setar o environment fora da função, é possível fazer isso por meio de uma função
  # na qual o usuário passa a hierarquia do modelo por nome e o modelo é construido pelo simulador, 
  # isto é os assigns são passados automaticamente.
  #' Por exemplo:
  #' 
  #' Indica que o modelo soybeanPlanting está dentro de soybeanModel.
  #' AddSubModel(soybeanPlanting, soybeanModel)
  #'
  
  b1 <- plantList$soybean$params$b1 # não usando
  b2 <- plantList$soybean$params$b2 # não usando 
  
  
  environment(SoybeanPlanting)    <- env
  environment(SoybeanCROPGRO)     <- env
  environment(SoybeanCropresidue) <- env
  
  depth <- numeric(nsoilay)
  
  # new michel
  # beta1[index] <- min(0.982 + idpp[index] * 0.00002, 0.995)
  # beta1[index] <- min(0.995*(1-exp(-0.1*idpp[index])), 0.995) #
  
  # To do: Henrique, conversar com o Michel e parametrizar essa funacao do crescimento radicular
  # To do: ou trazer o valores resolvido pelo CROPGRO para o froot
  beta1[index] <- 0.992 
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
  
  SoybeanPlanting(year0, year, month, day, jday, ffact, index)
  
  SoybeanCROPGRO(year, year0, month, day, jday, index)
  
  SoybeanCropresidue(year, year0, jday, index)
  
 
}


