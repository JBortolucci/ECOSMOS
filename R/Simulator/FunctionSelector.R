setDefaultFunctions <- function(){
  setsoi    <- setsoiR
  fwetcal   <- fwetcalR
  solset    <- solsetR
  solsur    <- solsurR
  solalb    <- solalbR
  solarf    <- solarfR
  irrad     <- irradR
  cascade   <- cascadeR
  canini    <- caniniR
  drystress <- drystressR
  turcof    <- turcofR
  turvap    <- turvapR
  cascad2   <- cascad2R
  noveg     <- novegR
  diurnalmet<- diurnalmetR
  diurnal   <- diurnalR
  
  assign("setsoi", setsoi, envir = globalenv())
  assign("fwetcal", fwetcal, envir = globalenv())
  assign("solset", solset, envir = globalenv())
  assign("solsur", solsur, envir = globalenv())
  assign("solalb", solalb, envir = globalenv())
  assign("solarf", solarf, envir = globalenv())
  assign("irrad", irrad, envir = globalenv())
  assign("cascade", cascade, envir = globalenv())
  assign("canini", canini, envir = globalenv())
  assign("drystress", drystress, envir = globalenv())
  assign("turcof", turcof, envir = globalenv())
  assign("turvap", turvap, envir = globalenv())
  assign("cascad2", cascad2, envir = globalenv())
  assign("noveg", noveg, envir = globalenv())
  assign("diurnalmet", diurnalmet, envir = globalenv())
  assign("diurnal", diurnal, envir = globalenv())
}

SetCompiledFunctions <- function(){
  setsoi    <- setsoiCpp
  fwetcal   <- fwetcalCpp
  solset    <- solsetCpp
  solsur    <- solsurR
  solalb    <- solalbCpp
  solarf    <- solarfCpp
  irrad     <- irradCpp
  cascade   <- cascadeCpp
  canini    <- caniniCpp
  drystress <- drystressCpp
  turcof    <- turcofCpp
  turvap    <- turvapCpp
  cascad2   <- cascad2Cpp
  noveg     <- novegCpp
  diurnalmet<- diurnalmetCpp
  diurnal   <- diurnalCpp
  
  assign("setsoi", setsoi, envir = globalenv())
  assign("fwetcal", fwetcal, envir = globalenv())
  assign("solset", solset, envir = globalenv())
  assign("solsur", solsur, envir = globalenv())
  assign("solalb", solalb, envir = globalenv())
  assign("solarf", solarf, envir = globalenv())
  assign("irrad", irrad, envir = globalenv())
  assign("cascade", cascade, envir = globalenv())
  assign("canini", canini, envir = globalenv())
  assign("drystress", drystress, envir = globalenv())
  assign("turcof", turcof, envir = globalenv())
  assign("turvap", turvap, envir = globalenv())
  assign("cascad2", cascad2, envir = globalenv())
  assign("noveg", noveg, envir = globalenv())
  assign("diurnalmet", diurnalmet, envir = globalenv())
  assign("diurnal", diurnal, envir = globalenv())
}