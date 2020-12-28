
# simDataVars$BdecayStart <- 0.2146875  #Start of branch dying (years)
# simDataVars$Wdecay      <- 0.005  #stem turnover (<-bark fall)
# simDataVars$Cfracts     <- 0.45 #Fraction of Carbon
# simDataVars$Density     <- 480 # Wood density (Kg/m3)
# simDataVars$Sapheight   <- 0.396068225 #current sapwood area is function of height : Sapwoodarea <- Sapheight*Height 
# simDataVars$Bdecay      <- 0.24839385 #Branch turnover rate
# simDataVars$Cdecay      <- 0 #Coarse root turnover
# simDataVars$deltay      <- (1/365.25) #Days as year
# simDataVars$Rdecay2     <- 1.97265625 #Parameter regulating the fine root mortality
# simDataVars$Rdecay1     <- 0.161328125 #Parameter regulating the fine root mortality Rdecay<-Rdecay1 + Rdecay2 * (Fineroot / Fineroottarget );
# simDataVars$Fineroot1   <- 0.1868973 #Relationship between fine root (target) and LAI : Finerootexp <- Fineroot1 * Lai
# simDataVars$Fdecay1     <- 0.0078125 # Parameter regulating the leaves mortality : Fdecay<-Fdecay1+Fdecay2*(Leaftosaparea/leaftosapexp)*(1+Cpleaf/Fdecay3);
# simDataVars$Fdecay2     <- 0.52421875 # Parameter regulating the leaves mortality
# simDataVars$Fdecay3     <- 11.3638138015625 #Parameter regulating the leaves mortality
# simDataVars$Fdecay4     <- 19.9999122619628 #Parameter regulating the leaves mortality
# simDataVars$Leafsap1    <- 500.000030517578 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*Height)
# simDataVars$Leafsap2    <- 29999.9696540832 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*Height)
# simDataVars$Leafsap3    <- 0.12643 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*Height)
# simDataVars$LimLai      <- 5.2 #not used (modellai <- 1 option only)
# simDataVars$Alleafinit  <- 0.3546875 #At the very beginning force alloc to leaves
# simDataVars$Fineroot1   <- 0.1868973 #Relationship between fine root (target) and LAI : Finerootexp <- Fineroot1 * Lai
# simDataVars$Allocsensf  <- 0.1 #Sensitivity of forcing to target for leaves (not used in Ahmed version)
# simDataVars$nrx         <- 0.5 #Parameter regulating the fine root allocation factor 
# simDataVars$nrn         <- 0.2 #Parameter regulating the fine root allocation factor 
# simDataVars$Fwpmax      <- 0.478759765625 #Used to compute wtfac <- (water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin ); with "water" the actual content and capac the actual max content 
# simDataVars$Fwpmin      <- 0.076875 #used to compute wtfac <- (water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin ); with "water" the actual content and capac the actual max content 
# simDataVars$Branch1     <- 0.271347 #Relationship between branch biomass (target) and LAI : Branchexp <- (Branch1 * pow( Lai , Branch2 ) ) 
# simDataVars$Branch2     <- 1.6899703 #Relationship between branch biomass (target) and LAI : Branchexp <- (Branch1 * pow( Lai , Branch2 ) ) 
# simDataVars$Allocsensb  <- 0.1 #Sensitivity of forcing to target for branches
# simDataVars$Coroot1     <- 0.3 #Relationship between coarse root biomass (target) and stem biomass : Corootexp <- (Coroot1 * pow( Stem , Coroot2 ) )
# simDataVars$Coroot2     <- 0.8 #Relationship between coarse root biomass (target) and stem biomass : Corootexp <- (Coroot1 * pow( Stem , Coroot2 ) )
# simDataVars$Allocsenscr <- 0.1 #sensitivity of forcing to target for coarse roots
# simDataVars$Alleafmin   <- 0.26875 #
# simDataVars$Alleaf1     <- 0.6026091 #
# simDataVars$Alleaf2     <- 0.1328125 #
# simDataVars$Alleafremain <- 0.097646484375 #To leave a minimum allocation to branches coarse roots trunk //not used in new version
# simDataVars$Callocf      <- 0.207737 # Maximum C allocation coefficients to leaves
# simDataVars$Callocb      <- 0.3200564 #Maximum C allocation coefficients to branches
# simDataVars$Calloccr     <- 0.2781304 #Maximum C allocation coefficients to coarse roots
# simDataVars$Siginit      <- 18 #Initial SLA value
# simDataVars$Sigmin       <- 11 #Minimum SLA value of new leaves
simDataVars$M2_AS_HA     <- 1/10000 #m2 as ha
simDataVars$KG_AS_TONNES <- 1/1000 #kg as tonnes
# simDataVars$BfallStart   <- 2 #Start of dead branches fall from the tree
# simDataVars$Bfall        <- 0.786798095703124 #Dead branches fall from the tree (turnover)
simDataVars$kg_C_M2_to_T_ha <- 10000/1000 #Tonnes per ha as kg per m2
simDataVars$G_AS_KG         <- 1/1000 # grams as kilograms
# simDataVars$b1              <- 0.7929 #parameter from the relaionship of Stem and stem + Corase Root: nls(Stem_Input~b1*AgroIBIS_StemCoroot_Input^b2start <- list(b1=0.1, b2=1))
# simDataVars$b2              <- 1.024  #parameter from the relaionship of Stem and stem + Corase Root: nls(Stem_Input~b1*AgroIBIS_StemCoroot_Input^b2start = list(b1=0.1, b2=1))
# 
# simDataVars$mh_aw <- 10.0  #maximum root effective depht

#initial values:
simDataVars$Sapwood     <- 0.0001
simDataVars$Heartwood   <- 0.0001
simDataVars$Deadcoroots <- 0.0001
simDataVars$tauleaf_branch <- 365
simDataVars$betag          <- 0.1

  
### AVERAGE companies params: FROM AHTIA ET AL. (2019)
#simDataVars$Leafsap3      <- 0.13 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- Leafsap1 + Leafsap2*exp(-Leafsap3*Height)
#simDataVars$Calloccr      <- 0.28 #Carbon allocation coarse roots
#simDataVars$Callocf       <- 0.21 #Maximum C allocation coefficients to leaves
#simDataVars$Callocb       <- 0.32 #Carbon allocattion branches
#simDataVars$Branch1       <- 0.27 #Relationship between branch biomass (target) and LAI : Branchexp <- (Branch1 * pow( Lai , Branch2 ) ) 
#simDataVars$Branch2       <- 1.57 #Relationship between branch biomass (target) and LAI : Branchexp <- (Branch1 * pow( Lai , Branch2 ) ) 
#simDataVars$Alleafinit    <- 0.43 #At the very beginning force alloc to leaves
#simDataVars$Alleafremain  <- 0.098 #To leave a minimum allocation to branches coarse roots trunk //not used in new version
#simDataVars$Fdecay1       <- 0.0078 # Parameter regulating the leaves mortality : Fdecay<-Fdecay1+Fdecay2*(Leaftosaparea/leaftosapexp)*(1+Cpleaf/Fdecay3);
#simDataVars$Fdecay2       <- 0.53 # Parameter regulating the leaves mortality
#simDataVars$Fdecay3       <- 10.31 #Parameter regulating the leaves mortality
#simDataVars$Fdecay4       <- 21.53 #Parameter regulating the leaves mortality
#simDataVars$Bdecay        <- 0.24 #Branch turnover rate
#simDataVars$Bfall         <- 0.70 #Dead branches fall from the tree (turnover)
### End of companies params


source("R/CropModels/Eucalipto/EucaliptoPhenocrop.R")
source("R/CropModels/Eucalipto/EucaliptoPlanting.R")
source("R/CropModels/Eucalipto/EucaliptoCropresidue.R")


# TODO: remover index depois, passar as variáveis automaticamente.
EucaliptoModel <- function(year, month, day, index) {
  
  
  
  # TODO: Verificar se há uma maneira de fazer isso automaticamente para funções do usuário.
  # Se for possível setar o environment fora da função, é possível fazer isso por meio de uma função
  # na qual o usuário passa a hierarquia do modelo por nome e o modelo é construido pelo simulador, 
  # isto é os assigns são passados automaticamente.
  #' Por exemplo:
  #' 
  #' Indica que o modelo eucaliptoPlanting está dentro de eucaliptoModel.
  #' AddSubModel(EucaliptoPlanting, EucaliptoModel)
  #'
  
  b1 <- plantList$eucalipto$params$b1 # não usando
  b2 <- plantList$eucalipto$params$b2 # não usando 
  
  beta1A       <- plantList$eucalipto$params$beta1A
  betamax      <- plantList$eucalipto$params$betamax
  
  environment(EucaliptoPlanting)    <- env
  environment(EucaliptoPhenocrop)   <- env
  environment(EucaliptoCropresidue) <- env
  
  depth <- numeric(nsoilay)
  
  ###-------------------
  # new michel
  # beta1[index] <- min(0.982 + idpp[index] * 0.00002, 0.995)
  # beta1[index] <- min(0.995*(1-exp(-0.1*idpp[index])), 0.995) # funcao que altera vmax_pft do eucalipto em funcao do tempo
  # assign("beta1", beta1[index], envir = env)

  
  ###-------------------
  #### By Michel: Crescimento da raiz em funcao do tempo. perfis de raizes baseado no artigo de Christina et al. (2011). OBS: O Mathias passou os dados de densidade de raiz
  awc     <- 0
  sumroot <- 0
  for (k in 1:nsoilay) {
    # crescimento da raiz em função da disponibilidade de agua no solo
    awc     <- awc + froot[k]*min (1.0, max (0.0, (wsoi[k]*(1 - wisoi[k]) - swilt[k]) / (sfield[k] - swilt[k])))
    sumroot <- sumroot + froot[k]
  }
  awc <- awc/sumroot
  
  if(idpp[index]>1) { betag <- betag + 0.1*max(0.5,awc) }
  beta1[index] <- min(betamax*(1-beta1A*exp(-betag/12)), betamax) # funcao que altera densidade de raiz em função do tempo e quantidade de agua: Michel
  # beta1[index] <- min(betamax*(1-0.05*exp(-betag/12)), betamax) # funcao que altera densidade de raiz em função do tempo e quantidade de agua: Michel
  #beta1[index] <- min((0.95+(0.045/180)*betag), 0.995) # funcao que altera densidade de raiz em função do tempo e quantidade de agua: Santiago
  #print(paste(idpp[index],0.995*(1-0.05*exp(-idpp[index]/120)),0.995*(1-0.05*exp(-betag/12)),0.95+(0.045/180)*betag,sep=" / "))
  ###-------------------
  
  ###  beta1[index] <- min(0.995*(1-0.045*exp(-idpp[index]/120)), 0.995) # funcao que altera densidade de raiz em função do tempo. Perfis de raizes baseado no artigo de Christina et al. (2011). OBS: O Mathias passou os dados de densidade de raiz
  ###-------------------

  
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
  
  
  ###----------------------------------------------------
  ### Michel: Fraction of root in the first 30 cm from top soil
  sumfroot <- matrix(nrow = 1, ncol = 2)
  
  for(k in 1: nsoilay) {
    
    if(nslaym < depth[1]){
      
      sumfroot[1, 1] <- sum(froot[1:1, 1])
      sumfroot[1, 2] <- sum(froot[1:1, 2])
      
    } else if(depth[k] <= nslaym){
      
      sumfroot[1, 1] <- sum(froot[1:k, 1])
      sumfroot[1, 2] <- sum(froot[1:k, 2])
      
    } else if (depth[k] > nslaym && depth[k-1] <= nslaym) {
      
      sumfroot[1, 1] <- sum(sumfroot[1, 1], 1 - beta1 ** (nslaym - depth[k-1]))
      sumfroot[1, 2] <- sum(sumfroot[1, 2], 1 - beta2 ** (nslaym - depth[k-1]))
      break
      
    }
    
  }
  ###----------------------------------------------------
  
  assign("froot", froot, envir = env)
  assign("sumfroot", sumfroot, envir = env)
  
  assign("betag", betag, envir = env)
  assign("beta1",beta1[index],envir = env)
  
# ### using the database rooting proflies: BY MICHEL
#   # calculate rooting profiles
#   for(k in 1: nsoilay) {
#     froot[k,1] <- SRGF[K]/sum(SRGF)
#   }
#   assign("froot", froot, envir = env)
  
  EucaliptoPlanting(year0, year, month, day, jday, ffact, index)
  
  EucaliptoPhenocrop(year, year0, month, day, jday, index)
  
  EucaliptoCropresidue(year, year0, jday, index)
  
 
}


