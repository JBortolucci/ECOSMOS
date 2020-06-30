#-------------------------------------------------------------
# Initialize some model variables for cold start conditions
#-------------------------------------------------------------
# Globals used
#-------------------------------------------------------------
# npoi
# nsnolay
# nsoilay
# fi
# hsno
# tsno
# tsoi
# wsoi
# wisoi
#-------------------------------------------------------------

coldstart <- function() {
  # TODO: As variáveis wsoi, wisoi e tsoi são inicializadas 2 vezes, aqui, e dentro da função
  # inisoil, pois removeu irestart. remover inicializacao de inisoil?
  nsnolay <- 3
  assign("nsnolay",  nsnolay, envir = env)
  
  assign("fi",  array(0, 1), envir = env)
  assign("hsno",  matrix(0, 1, nsnolay), envir = env)
  assign("tsno",  matrix(273.16, 1, nsnolay), envir = env)
  assign("tsoi",  matrix(278.16, 1, nsnolay), envir = env)
  assign("wsoi",  matrix(0.5   , 1, nsoilay), envir = env)
  assign("wisoi", matrix(0     , 1, nsoilay), envir = env)
  
}
