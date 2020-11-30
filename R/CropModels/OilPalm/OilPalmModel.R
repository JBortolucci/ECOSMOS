#
# simDataVars$BdecayStart <- 0.2146875  #Start of branch dying (years)
# simDataVars$Wdecay      <- 0.005  #stem turnover (<-bark fall)
# simDataVars$Cfracts     <- 0.45 #Fraction of Carbon
# simDataVars$Density     <- 480 # Wood density (Kg/m3)
# simDataVars$Sapheight   <- 0.396068225 #current sapwood area is function of height : Sapwoodarea <- Sapheight*Height
# simDataVars$Bdecay      <- 0.24839385 #Branch turnover rate
# simDataVars$Cdecay      <- 0 #Coarse root turnover
simDataVars$deltay      <- (1/365.25) #Days as year
# simDataVars$Rdecay2     <- 1.97265625 #Parameter regulating the fine root mortality
# simDataVars$Rdecay1     <- 0.161328125 #Parameter regulating the fine root mortality Rdecay<-Rdecay1 + Rdecay2 * (Fineroot / Fineroottarget );
# simDataVars$Fineroot1   <- 0.1868973 #Relationship between fine root (target) and LAI : Finerootexp <- Fineroot1 * Lai
# simDataVars$Fdecay1     <- 0.0078125 # Parameter regulating the leaves mortality : Fdecay<-Fdecay1+Fdecay2*(Leaftosaparea/leaftosapexp)*(1+Cpleaf/Fdecay3);
# simDataVars$Fdecay2     <- 0.52421875 # Parameter regulating the leaves mortality
# simDataVars$Fdecay3     <- 11.3638138015625 #Parameter regulating the leaves mortality
# simDataVars$Fdecay4     <- 19.9999122619628 #Parameter regulating the leaves mortality
# simDataVars$leavesap1    <- 500.000030517578 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- leavesap1 + leavesap2*exp(-leavesap3*Height)
# simDataVars$leavesap2    <- 29999.9696540832 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- leavesap1 + leavesap2*exp(-leavesap3*Height)
# simDataVars$leavesap3    <- 0.12643 #Computation of target leaf to sapwood area in function of height : leaftosapexp <- leavesap1 + leavesap2*exp(-leavesap3*Height)
# simDataVars$LimLai      <- 5.2 #not used (modellai <- 1 option only)
# simDataVars$Alleafinit  <- 0.4546875 #At the very beginning force alloc to leaves
# simDataVars$Fineroot1   <- 0.1868973 #Relationship between fine root (target) and LAI : Finerootexp <- Fineroot1 * Lai
# simDataVars$Allocsensf  <- 0.1 #Sensitivity of forcing to target for leaves (not used in Ahmed version)
# simDataVars$nrx         <- 0.5 #Parameter regulating the fine root allocation factor
# simDataVars$nrn         <- 0.2 #Parameter regulating the fine root allocation factor
 simDataVars$Fwpmax      <- 0.478759765625 #Used to compute wtfac <- (water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin ); with "water" the actual content and capac the actual max content
 simDataVars$Fwpmin      <- 0.076875 #used to compute wtfac <- (water / capac) - Fwpmin ) / ( Fwpmax - Fwpmin ); with "water" the actual content and capac the actual max content
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
# simDataVars$Callocf      <- 0.207737 #
# simDataVars$Callocb      <- 0.3200564 #Carbon allocattion branches
# simDataVars$Calloccr     <- 0.2781304 #Carbon allocation coarse roots
# simDataVars$Siginit      <- 18 #Initial SLA value
# simDataVars$Sigmin       <- 11 #Minimum SLA value of new leaves
# simDataVars$M2_AS_HA     <- 1/10000 #m2 as ha
# simDataVars$KG_AS_TONNES <- 1/1000 #kg as tonnes
# simDataVars$BfallStart   <- 2 #Start of dead branches fall from the tree
# simDataVars$Bfall        <- 0.786798095703124 #Dead branches fall from the tree (turnover)
 simDataVars$kg_C_M2_to_T_ha <- 10000/1000 #Tonnes per ha as kg per m2
 simDataVars$betag           <- 0.1 # grams as kilograms
# simDataVars$b1              <- 0.7929 #parameter from the relaionship of Stem and stem + Corase Root: nls(Stem_Input~b1*AgroIBIS_StemCoroot_Input^b2start <- list(b1=0.1, b2=1))
# simDataVars$b2              <- 1.024  #parameter from the relaionship of Stem and stem + Corase Root: nls(Stem_Input~b1*AgroIBIS_StemCoroot_Input^b2start = list(b1=0.1, b2=1))
#
# simDataVars$mh_aw <- 10.0  #maximum root effective depht

#initial values:
simDataVars$Sapwood     <- 0.0001
simDataVars$Heartwood   <- 0.0001
simDataVars$Deadcoroots <- 0.0001
#
# simDataVars$tauleaf_branch <- 365

simDataVars$gddspate  <- array(0, 1) # accumulated GDD to budding a spate
simDataVars$spates    <- data.frame("gdd" = 0, "islive" = 1, "dm" = 0, "dpp" = 0, "dmmax" = 0) # Phenological development of each spate (GDD, 'is live' flag, accumulated dry matter and daps past budding)
# simDataVars$leaves     <- data.frame("gdd" = 0, "islive" = 1, "dm" = 0, "dpp" = 0, "index" = 0, "dry" = 0, "n" = 0) # Phenological development of each leaf (GDD, 'is live' flag, accumulated dry matter and daps past opening)
# simDataVars$fruits    <- data.frame("ssi" = 0, "hrvday" = 0, "dm" = 0, "dpp" = 0, "dmmax" = 0, "dif" = 0) # Phenological development of bounches (sink size index, day of harvest of the bunch and accumulated dry matter)
simDataVars$paleafmat <- array(0, 1)
simDataVars$dppmat    <- array(0, 1)
simDataVars$cbiod     <- array(0, 1)
simDataVars$cbios     <- array(0, 1)
simDataVars$arepr     <- array(0, 1)
simDataVars$yld       <- 0
simDataVars$gddl      <- array(0, 1)
simDataVars$outl      <- array(0, 1)

source("R/CropModels/OilPalm/OilPalmPhenocrop.R")
source("R/CropModels/OilPalm/OilPalmPlanting.R")
source("R/CropModels/OilPalm/OilPalmCropresidue.R")


# TODO: remover index depois, passar as variáveis automaticamente.
OilPalmModel <- function(year, month, day, index) {


  # TODO: Verificar se há uma maneira de fazer isso automaticamente para funções do usuário.
  # Se for possível setar o environment fora da função, é possível fazer isso por meio de uma função
  # na qual o usuário passa a hierarquia do modelo por nome e o modelo é construido pelo simulador,
  # isto é os assigns são passados automaticamente.
  #' Por exemplo:
  #'
  #' Indica que o modelo eucaliptoPlanting está dentro de eucaliptoModel.
  #' AddSubModel(EucaliptoPlanting, EucaliptoModel)
  #'

  environment(OilPalmPlanting)    <- env
  environment(OilPalmPhenocrop)   <- env
  environment(OilPalmCropresidue) <- env

  depth <- numeric(nsoilay)

  ###-------------------
  #### By Michel: Crescimento da raiz em funcao do tempo. perfis de raizes baseado no artigo de Christina et al. (2011). OBS: O Mathias passou os dados de densidade de raiz
  # awc     <- 0
  # sumroot <- 0
  # for (k in 1:nsoilay) {
  #   # crescimento da raiz em função da disponibilidade de agua no solo
  #   awc     <- awc + froot[k]*min (1.0, max (0.0, (wsoi[k]*(1 - wisoi[k]) - swilt[k]) / (sfield[k] - swilt[k])))
  #   sumroot <- sumroot+froot[k]
  # }
  #
  # awc <- awc/sumroot
  #
  # if(idpp[index]>1) { betag <- betag + 0.1*max(0.5,awc) }
  # beta1[index] <- min((0.95+(0.045/180)*betag), 0.995)
  #
  # assign("froot", froot, envir = env)
  # assign("betag", betag, envir = env)
  # assign("beta1", beta1[index], envir = env)

  OilPalmPlanting(year0, year, month, day, jday, ffact, index)
  OilPalmPhenocrop(year, year0, month, day, jday, index)
  OilPalmCropresidue(year, year0, jday, index)


}
