# ---------------------------------------------------------------------
#  subroutine inisurf  
# ---------------------------------------------------------------------
# Does initialization for model
# ---------------------------------------------------------------------
# Globals:
#
# asurd
# asuri
# cair
# cappa
# cdisturb
# ch2o
# cic3
# cic4
# cice
# cil3
# cil4
# cils
# ciub
# ciuc
# clitll
# clitlm
# clitls
# clitrl
# clitrm
# clitrs
# clitwl
# clitwm
# clitws
# csc3
# csc4
# csl3
# csl4
# csls
# csoipas
# csoislon
# csoislop
# csub
# csuc
# cvap
# epsilon
# gdrain
# ginvap
# grav
# greenfracl3
# greenfracl4
# grunof
# gsc3
# gsc4
# gsl3
# gsl4
# gsls
# gsub
# gsuc
# gsuvap
# gtrans
# hfus
# hsub
# hvap
# ireccru
# iwet
# iwetday
# nband
# npoi
# offflagl3
# offflagl4
# offflagu
# onflagl3
# onflagl4
# onflagu
# precipday
# q12
# q34
# rair
# rhow
# rvap
# sl
# ss
# stef
# stincldd
# stinprecd
# stinqd
# stinrad
# stintd
# stintmax
# stintmin
# stinwindd
# su
# t12
# t34
# tco2mic
# templs
# tempu
# tl
# tlsub
# tmelt
# tneetot
# tnmin
# tnpptot
# totalit
# totanlit
# totcmic
# totcondc3
# totcondc4
# totcondl3
# totcondl4
# totcondls
# totcondub
# totconduc
# totcsoi
# totfall
# totlit
# totnlit
# totnmic
# totrlit
# totrnlit
# ts
# tu
# vonk
# vzero
# wliql
# wliqs
# wliqu
# wsnol
# wsnos
# wsnou
# xstore
# ---------------------------------------------------------------------

inisurf <- function() {
  
  assign("stef", 5.67e-8 , envir = env)
  assign("vonk", 0.4, envir = env)
  assign("grav", 9.80616, envir = env)
  assign("tmelt", 273.16, envir = env)
  assign("hvap", 2.5104e+6, envir = env)
  assign("hfus", 0.3336e+6, envir = env)
  assign("hsub", hvap + hfus, envir = env)
  assign("ch2o", 4.218e+3, envir = env)
  assign("cice", 2.106e+3, envir = env)
  assign("cair", 1.004640000e+3, envir = env)
  assign("cvap", 1.810000000e+3, envir = env)
  assign("rair", 287.04, envir = env)
  assign("rvap", 461.0, envir = env)
  assign("cappa", rair / cair, envir = env)
  assign("rhow", 1.0e+3, envir = env)
  assign("ireccru", 0, envir = env)
  
  assign("vzero", array(0, 1), envir = env)
  
  # specify the epsilon value for the model
  assign("epsilon", 1.0e-6, envir = env)
  
  # initialize integer variables (can't use const for this)
  # wet day / dry day flag initialized to dry day (0)
  assign("iwet", array(0, 1), envir = env)
  
  
  assign("iwetday", matrix(0, nrow = 1, ncol = 31), envir = env)
  assign("precipday", matrix(0, nrow = 1, ncol = 31), envir = env)
  
  # zero flux arrays, and global diagnostic arrays
  assign("asurd",  matrix(0, 1, nband), envir = env)
  assign("asuri", matrix(0, 1, nband), envir = env)
  assign("totcondub", array(0, 1), envir = env)
  assign("totconduc", array(0, 1), envir = env)
  assign("totcondls", array(0, 1), envir = env)
  assign("totcondl3", array(0, 1), envir = env)
  assign("totcondc4", array(0, 1), envir = env)
  assign("ginvap", array(0, 1), envir = env)
  assign("gsuvap", array(0, 1), envir = env)
  assign("gtrans", array(0, 1), envir = env)
  assign("grunof", array(0, 1), envir = env)
  assign("gdrain", array(0, 1), envir = env)

  
  # initialize station data vars
  assign("stinrad", array(-999, 1), envir = env)
  
  
  # initialize vegetation prognostic variables
  # initialize all temperature fields to 10 degrees C
  assign("tu", array(283.16, 1) , envir = env)
  assign("ts", array(283.16, 1) , envir = env)
  assign("tl", array(283.16, 1) , envir = env)
  
  # initialize weather generator 'memory'
  assign("xstore", matrix(0, 1, 3), envir = env)
  
  # initialize temperature of lower canopy buried by
  # snow to 0 degrees C
  assign("tlsub", array(273.16, 1), envir = env)
  
  # initialize canopy air conditions (used in turvap)
  assign("t12", array(283.16, 1), envir = env)
  assign("t34", array(283.16, 1), envir = env)
  assign("q12", array(0, 1), envir = env)
  assign("q34", array(0, 1), envir = env)
  
  # initialize all co2 concentrations (mol/mol)
  assign("ciub", array(350.0e-06, 1), envir = env)
  assign("ciuc", array(350.0e-06, 1), envir = env)
  assign("cils", array(350.0e-06, 1), envir = env)
  assign("cil3", array(350.0e-06, 1), envir = env)
  assign("cil4", array(350.0e-06, 1), envir = env)
  assign("cic3", array(350.0e-06, 1), envir = env)
  assign("cic4", array(350.0e-06, 1), envir = env)
  
  # PFT_UPDATE: Inicialização do novo vetor
  assign("ci", array(350.0e-06, npft), envir = env)
  
  assign("csub", array(350.0e-06, 1), envir = env)
  assign("csuc", array(350.0e-06, 1), envir = env)
  assign("csls", array(350.0e-06, 1), envir = env)
  assign("csl3", array(350.0e-06, 1), envir = env)
  assign("csl4", array(350.0e-06, 1), envir = env)
  assign("csc3", array(350.0e-06, 1), envir = env)
  assign("csc4", array(350.0e-06, 1), envir = env)
  
  # PFT_UPDATE: Inicialização do novo vetor
  assign("cs", array(350.0e-06, npft), envir = env)
  
  # initialize stomatal conductance (mol-h2o/m**2/sec)
  assign("gsub", array(0.5, 1), envir = env)
  assign("gsuc", array(0.5, 1), envir = env)
  assign("gsls", array(0.5, 1), envir = env)
  assign("gsl3", array(0.5, 1), envir = env)
  assign("gsl4", array(0.5, 1), envir = env)
  assign("gsc3", array(0.5, 1), envir = env)
  assign("gsc4", array(0.5, 1), envir = env)
  
  # PFT_UPDATE: Inicialização do novo vetor
  assign("gs", array(0.5, npft), envir = env)
  
  # initialize soil biogeochemistry variables
  #if (irestart == 0) {
    assign("clitlm", array(0, 1), envir = env)
    assign("clitls", array(0, 1), envir = env)
    assign("clitll", array(0, 1), envir = env)
    assign("clitrm", array(0, 1), envir = env)
    assign("clitrs", array(0, 1), envir = env)
    assign("clitrl", array(0, 1), envir = env)
    assign("clitwm", array(0, 1), envir = env)
    assign("clitws", array(0, 1), envir = env)
    assign("clitwl", array(0, 1), envir = env)
    assign("totcmic", array(0, 1), envir = env)
    assign("csoislop", array(0, 1), envir = env)
    assign("csoislon", array(0, 1), envir = env)
    assign("csoipas", array(0, 1), envir = env)
  #}
  
  assign("totlit", array(0, 1), envir = env)
  assign("totnlit", array(0, 1), envir = env)
  assign("totfall", array(0, 1), envir = env)
  assign("totalit", array(0, 1), envir = env)
  assign("totrlit", array(0, 1), envir = env)
  assign("totanlit", array(0, 1), envir = env)
  assign("totrnlit", array(0, 1), envir = env)
  assign("totcsoi", array(0, 1), envir = env)
  assign("totnmic", array(0, 1), envir = env)
  assign("tco2mic", array(0, 1), envir = env)
  assign("tnpptot", array(0, 1), envir = env)
  assign("tneetot", array(0, 1), envir = env)
  assign("tnmin", array(0, 1), envir = env)
  
  # initialize carbon lost to atmosphere due
  # to biomass burning
  assign("cdisturb", array(0, 1), envir = env)
  
  # initialize phenology flags
  #if (irestart == 0) {
    assign("tempu", array(1, 1), envir = env)
    assign("templs", array(1, 1), envir = env)
    assign("greenfracl3", array(1, 1), envir = env)
    assign("greenfracl4", array(1, 1), envir = env)
    # TODO: Verificar valor do greenfrac para NaturalVeg
    greenfrac[] <- 1
    assign("greenfrac", greenfrac, envir = env)
    assign("onflagu", as.logical(array(F, 1)), envir = env)
    assign("onflagl3", as.logical(array(F, 1)), envir = env)
    assign("onflagl4", as.logical(array(F, 1)), envir = env)
    assign("offflagu", as.logical(array(T, 1)), envir = env)
    assign("offflagl3", as.logical(array(T, 1)), envir = env)
    assign("offflagl4", as.logical(array(T, 1)), envir = env)
  #}
  
  # initialize water and snow interception fractions
  assign("wliqu", array(0, 1) , envir = env)
  assign("wliqs", array(0, 1) , envir = env)
  assign("wliql", array(0, 1) , envir = env)
  
  assign("wsnou", array(0, 1) , envir = env)
  assign("wsnos", array(0, 1) , envir = env)
  assign("wsnol", array(0, 1) , envir = env)
  
  assign("su", array(0, 1) , envir = env)
  assign("ss", array(0, 1) , envir = env)
  assign("sl", array(0, 1) , envir = env)
  
}



