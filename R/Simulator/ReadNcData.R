
###################################################################
#                         
#   Funções especializadas para leitura dos dados dos netcdf's
#
###################################################################

# Função genérica que lê variável do netcdf
ReadVarFromNetcdfFile <- function(path = "", varName = "", start = c(1, 1, 1, 1), count = c(720, 360, 1, 1), ndim = 2) {
  fid <- nc_open(path)
  dim <- count[1:ndim]
  values <- array(ncvar_get(fid, varName, start = start, count = count, verbose = F), dim = dim)
  nc_close(fid)
  return(values)
}

# Funções específicas para leitura de cada arquivo netcdf.
# Caminho dos arquivos e nome das variáveis pré-definidas.

ReadTopographyFile <- function(start = c(1, 1, 1, 1), count = c(720, 360, 1, 1)) {
  return(ReadVarFromNetcdfFile(path = "inst/input/topo.nc", varName = "topo", start = start, count = count, ndim = 2))
}

ReadVegetationFile <- function(start = c(1, 1, 1, 1), count = c(720, 360, 1, 1)) {
  return(ReadVarFromNetcdfFile(path = "inst/input/vegtype.nc", varName = "vegtype", start = start, count = count, ndim = 2))
}


ReadSandFile <- function(start = c(1, 1, 1, 1), count = c(720, 360, 6, 1)) {
  return(ReadVarFromNetcdfFile(path = "inst/input/soita.sand.nc", varName = "sandpct", start = start, count = count, ndim = 3))
}

ReadClayFile <- function(start = c(1, 1, 1, 1), count = c(720, 360, 6, 1)) {
  return(ReadVarFromNetcdfFile(path = "inst/input/soita.clay.nc", varName = "claypct", start = start, count = count, ndim = 3))
}


# Leitura do arquivo surta especializada, pois obtém outros dados e variáveis
# Deve ser lido primeiro pois é utilizado como base para leitura dos demais netcdf's
ReadLandMaskFile <- function(path = "inst/input/surta.nc") {
  
  fid <- nc_open(path)
  
  nlon <- fid$dim[[1]]$len # Number of longiude points in the entire file 
  nlat <- fid$dim[[2]]$len # Number of latitude points in the entire file 
  
  values <- array(ncvar_get(fid, "surta", start = c(1, 1, 1, 1), count = c(nlon, nlat, 1, 1), verbose = F), dim = c(nlon, nlat))
  
  # consultar valor invalido netcdf
  values[values=="NaN"] <- NA
  
  lonscale <- ncvar_get(fid, fid$dim[[1]]$name) # todas as coordenadas horizontais
  latscale <- ncvar_get(fid, fid$dim[[2]]$name) # todas as coordenadas verticais
  
  yres <- latscale[1]-latscale[2]  # latitude resolution (in degrees) 
  xres <- lonscale[2]-lonscale[1]  # longitude resolution (in degrees) 
  
  res <- list(
    nlon       = nlon,
    nlat       = nlat,
    yres       = yres,
    xres       = xres,
    maskValues = values,
    lonscale   = lonscale,
    latscale   = latscale
  )
  
  nc_close(fid)
  
  return(res)
}
