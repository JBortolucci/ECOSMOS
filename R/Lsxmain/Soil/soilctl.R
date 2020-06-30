##########################################################
#                     rotina 'soilctl'                   #
##########################################################

soilctl <- function() {
  
  dim_params <- c(1, nsoilay)
  
  double_params <- c(dtime, epsilon, ch2o, cice, hfus, rhow, tmelt, wpudmax, bperm)
  
  npoi_matrix <- matrix(c(fvapg, fwpud, fwtop, gadjust, gdrain, grunof, heatg, raing,
                          soihfl, tl, traing, tu, wipud, wpud), 
                        nrow = 14, ncol = 1, byrow = T)
  
  # Caso alguma variváel tenha valor infinito ou NaN
  npoi_matrix[is.nan(npoi_matrix)]     <- 0
  npoi_matrix[!is.finite(npoi_matrix)] <- 0
  
 
  out <- .Fortran("soilctlWrapper", as.vector(dim_params, mode = "integer"),
                  as.vector(double_params, mode = "double"),
                  as.matrix(npoi_matrix),
                  as.matrix(matrix(as.integer(ibex), 1, nsoilay), mode = "integer"),
                  as.matrix(csoi),
                  as.vector(hsoi),
                  as.matrix(hydraul),
                  as.matrix(poros),
                  as.matrix(porosflo),
                  as.matrix(qglif),
                  as.matrix(rhosoi),
                  as.matrix(tsoi),
                  as.matrix(upsoil),
                  as.matrix(upsoiu),
                  as.matrix(wflo),
                  as.matrix(wisoi),
                  as.matrix(wsoi),
                  as.matrix(bex),
                  as.matrix(suction),
                  as.matrix(consoi),
                  as.matrix(hflo),
                  as.matrix(sice),
                  as.matrix(swater))
  
  names <- c("dtime", "epsilon", "ch2o", "cice", "hfus", "rhow", "tmelt", "wpudmax", "bperm")
  
  for(i in 1:length(names)) 
    assign(names[i], out[[2]][i], envir = env)
  
  names <- c("fvapg", "fwpud", "fwtop", "gadjust", "gdrain", "grunof", "heatg", "raing", "soihfl", "tl", "traing", "tu", "wipud", "wpud")
  
  for(i in 1:length(names)) 
    assign(names[i], out[[3]][i,], envir = env)
  
  names <- c("ibex", "csoi", "hsoi", "hydraul", "poros", "porosflo", "qglif", "rhosoi", "tsoi",
             "upsoil", "upsoiu", "wflo", "wisoi", "wsoi", "bex", "suction", "consoi", "hflo", "sice",
             "swater")
  
  for(i in 4:length(out))
    assign(names[i-3], out[[i]], envir = env)
  
}