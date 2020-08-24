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
      plai[i] <- 0.01
      
      env$thrlai[i] <- 0
      env$peaklai[i] <- 0
      env$cbiol[i] <- 0
      env$cbios[i] <- 0
      env$cbior[i] <- 0
      env$cbiow[i] <- 0
      env$cbiog[i] <- 0
      env$cbiop[i] <- 0
      env$hui[i] <- 0
      
      env$aleaf[i] <- 0
      env$aroot[i] <- 0
      env$astem[i] <- 0
      env$arepr[i] <- 0
      env$awood[i] <- 0
      
      env$aybprod[i] <- 0
      env$ayrprod[i] <- 0
      env$ayabprod[i] <- 0
      env$aylprod[i] <- 0
      env$harvidx[i] <- 0
      env$leafout[i] <- 0
      
      env$cumlvs[i] <- 0
      env$plaimx[i] <- 0
      env$dpgf[i] <- 0
      env$biomass[i] <- 0
      env$totnuptake[i] <- 0
      env$tnplant[i] <- 0
      env$totnfix[i] <- 0
      env$idpp[i] <- 0
      env$idpe[i] <- 0
      env$fixn[i] <- 0
      env$gddplant[i] <- 0
      env$crmclim[i] <- 0
      env$crmact[i] <- 0
      env$crmplant[i] <- 0
      env$grainday[i] <- 9999
      env$gddtsoi[i] <- 0
      env$fertinput[i] <- 0
      env$pstart[i] <- 999
      env$ik[i] <- 1
    }
    
  }
  
  assign("htmx", matrix(0, 1, 2), envir = env)
  assign("cdays", array(0, 1), envir = env)
  assign("cdays", array(0, 1), envir = env)
  assign("cropy", array(0, 1), envir = env)
  assign("sai", matrix(0, 1, 2), envir = env)
  assign("fu", array(0, 1), envir = env)
  assign("lai", matrix(0, 1, 2), envir = env)
  assign("zbot", matrix(0, 1, 2), envir = env)
  assign("ztop", matrix(0, 1, 2), envir = env)
  assign("totbiou", array(0, 1), envir = env)
  assign("totbiol", array(0, 1), envir = env)
  assign("totlaiu", array(0, 1), envir = env)
  assign("totlail", array(0, 1), envir = env)
  assign("vf",  array(0, 1), envir = env)
  assign("ncyears",  array(0, 1), envir = env)
  
}