#----------------------------------------------------------------------------------
# called from main.f or physiology.f
# 
# subroutine to initialize crop related variables that are dependent  
# on a crop growing season basis.  Variables are initialized at the end 
# of the growing season after harvest date and after data is written out (io.f)   
# 
# additionally, this routine needs to be called at (1) first time 
# crops are grown in a model run (a restart), or (2) when 
# crops are replacing natural vegetation (which could also be a restart) 
#----------------------------------------------------------------------------------
# Globals used
#----------------------------------------------------------------------------------

initialcrop <- function() {
  
  # reset variables after harvest date and data is written   
  # to io.f and crop diagnostic output files                
  # have to make sure there is no memory in the system      
  # of upper canopy or lower canopy vegetation when crops    
  # replace natural vegetation!
  for(i in seq(1, npft)) {
    if(plantList[[i]]$type == CROPS) {
      
      env$thrlai[i] <- 0
      env$peaklai[i] <- 0
      env$cbioc[i] <- 0
      env$hui[i] <- 0
      env$acob[i]  <- 0

      env$plai[i] <- 0.01
      env$pgreenfrac[i]  <- 1
      env$cbiol[i]   <- 0
      env$cbios[i]   <- 0
      env$cbiog[i]   <- 0
      env$cbiow[i]   <- 0
      env$cbiob[i]   <- 0 
      env$cbior[i]   <- 0 
      env$cbiocr[i]  <- 0
      
      env$cbiold[i]   <- 0
      env$cbiosd[i]   <- 0
      env$cbiogd[i]   <- 0
      env$cbiowd[i]   <- 0
      env$cbiobd[i]   <- 0 
      env$cbiord[i]   <- 0 
      env$cbiocrd[i]  <- 0


      env$aleaf[i]   <- 0
      env$astem[i]   <- 0
      env$arepr[i]   <- 0
      env$awood[i]   <- 0 
      env$abranch[i] <- 0
      env$aroot[i]   <- 0
      env$acroot[i]  <- 0


      env$aybprod[i] <- 0
      env$ayrprod[i] <- 0
      env$ayabprod[i]<- 0
      env$aylprod[i] <- 0

      env$biomass[i] <- 0
      env$totnuptake[i] <- 0
      env$tnplant[i] <- 0
      env$totnfix[i] <- 0
      env$idpp[i] <- 0
      env$fixn[i] <- 0
      env$gddplant[i] <- 0
      env$fertinput[i] <- 0
      env$pstart[i] <- 999
      
    }
    
  }
  
  assign("cropy", array(0, npft), envir = env)
  assign("sai", matrix(0, 1, 2), envir = env)
  assign("fu", array(0, 1), envir = env)
  assign("lai", matrix(0, 1, 2), envir = env)
  assign("greenfrac", matrix(0, 1, 2), envir = env)
  assign("zbot", matrix(0, 1, 2), envir = env)
  assign("ztop", matrix(0, 1, 2), envir = env)


}