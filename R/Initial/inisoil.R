

inisoil <- function() {
  

  # set sand/silt/clay vectors (xdat,ydat,zdat) for 11 data points
  assign("xdat", texdat[1, 1:ndat], envir = env)
  assign("ydat", texdat[2, 1:ndat], envir = env)
  assign("zdat", texdat[3, 1:ndat], envir = env)
  

  # set physical parameters of soil
  assign("z0soi", array(0.005, 1), envir = env)
  
  # initialize soil water and soil temperature fields
  # also initialize daily average fields for crops
  
  assign("adwsoilay", matrix(0.5, 1, nsoilay), envir = env)
  assign("adwisoilay", matrix(0, 1, nsoilay) , envir = env)
  assign("adtsoilay", matrix(278.13, 1, nsoilay) , envir = env)
  
 
  assign("wsoi",  matrix(0.75   , 1, nsoilay), envir = env)
  assign("tsoi",  matrix(Tavgann+273.16, 1, nsoilay), envir = env)
  assign("wisoi", matrix(0     , 1, nsoilay), envir = env)
  assign("fi",  array(0, 1), envir = env)
  assign("wipud", array(0, 1), envir = env)
  
  
  # added for Green-Ampt
  assign("fwpudtot", array(0, 1), envir = env)
  assign("wpud", array(0, 1), envir = env)
  
  # initialize total irrigation
  assign("totirrig", array(0, 1), envir = env)
  
  # initialize plant available nitrogen pool (kg/m2)
  assign("aplantn", array(0.008, 1), envir = env)
  
  # initialize soil nitrogen variables
  assign("totnuptake", matrix(0, 1, npft), envir = env)
  assign("stressn", matrix(1, 1, npft), envir = env)
  assign("tg", array(278.13, 1), envir = env)
  assign("ti", array(273.13, 1), envir = env)

  #TODO Henrique: kept when merging forage into soybean on 2020-02-09
  sand<- array(0, nsoilay)
  clay<- array(0, nsoilay)

  # Henrique: Commented on 2020-11-04
  #fclay    <- 0
  #fsilt    <- 0
  #fsand    <- 0

  rhosoi   <- array(0, nsoilay)
  csoi     <- array(0, nsoilay)
  fracsand <- array(0, nsoilay)
  fracsilt <- array(0, nsoilay)
  fracclay <- array(0, nsoilay)
  swilt    <- array(0, nsoilay)  
  sfield   <- array(0, nsoilay)  
  poros    <- array(0, nsoilay)
  hydraul  <- array(0, nsoilay)
  bex      <- array(0, nsoilay)
  ibex     <- array(0, nsoilay)
  suction  <- array(0, nsoilay)
  cpwf     <- array(0, nsoilay)
  swater   <- array(0, nsoilay)
  sice     <- array(0, nsoilay)
  soilbase <- array(0, nsoilay)
  depth    <- array(0, nsoilay)
  
  cpwfdat <- c(
    0.0495,     # sand
    0.0613,     # loamy sand
    0.1101,     # sandy loam
    0.0889,     # loam
    0.1668,     # silt loam
    0.2185,     # sandy clay loam
    0.2088,     # clay loam
    0.2730,     # silty clay loam
    0.2390,     # sandy clay
    0.2922,     # silty clay
    0.3163)
  
  
  for(k in 1:nsoilay) {
    
    # Minimum information
    fracclay[k] <- as.integer(SOIL.profile$SLCL[k])/100               # clay content
    fracsilt[k] <- as.integer(SOIL.profile$SLSI[k])/100               # silt content
    fracsand[k] <- 1- fracclay[k] - fracsilt[k]           # sand content 
    
      
    if(k==1) {  
      
      hsoi[k]<-SOIL.profile$SLB[k]/100 
      
    } else {
      
      hsoi[k]<-(SOIL.profile$SLB[k]/100) - (SOIL.profile$SLB[k-1]/100)
      
    }
    
    
    if( (fracclay[k] + fracsilt[k] + fracsand[k])!= 1 | hsoi[k] < 0.01 ){
      print('WARNING: Texture does not sum 1 or hsoil <=0.01') 
      print('Check soil layers depth in the inst/input/SOIL.csv')
      print(paste(fracclay[k],fracsilt[k],fracsand[k],hsoi[k],sep=" / "))
        }
    
    # Apply Pedotransfer function    
    pedofunct <- 1 # the PTF option

    if(pedofunct == 1) {
      
      poros[k]   <- (50.5 - 3.7 * fracclay[k] - 14.2 * fracsand[k]) / 100 # porosity (fraction):
      
      hydraul[k] <- 1.0 * 10 ** (-0.6 - 0.64 * fracclay[k] + 1.26 * fracsand[k]) * 0.0254 / (3600)
      bex[k]     <- 3.10 + 15.7 * fracclay[k] - 0.3 * fracsand[k]
      sfield[k]  <- (1 / poros[k]) * (50.5-3.7*fracclay[k]-14.2*fracsand[k])/100 * (1.157e-9 / hydraul[k])**(1/(2*bex[k]+3))
      swilt[k]   <- (1 / poros[k]) * (50.5-3.7*fracclay[k]-14.2*fracsand[k])/100 *((10**(2.17-0.63*fracclay[k]-1.58*fracsand[k])*0.01)/(1500/9.80665))**(1/bex[k])
#      suction[k] <- 10 ** (2.17 - 0.63 * fracclay[k] - 1.58 * fracsand[k]) * 0.01
      suction[k] <- 1.261* (swilt[k]*poros[k]) + 0.027
      bperm      <- 0.01
      
      print(paste('Calculated Soil Properties  ',k,swilt[k]*poros[k],sfield[k]*poros[k],poros[k],
                  hydraul[k],bex[k],bperm,suction[k],sep=" / "))
      
     
    } 
    
    if(!is.na(SOIL.profile$SSAT[k])) {poros[k]    <- SOIL.profile$SSAT[k]                  } # porosity (volume fraction)
    
    if(!is.na(SOIL.profile$SDUL[k])) {sfield[k]   <- (1 / poros[k]) * SOIL.profile$SDUL[k] } # field capacity as fraction of porosity (volume fraction)
    if(!is.na(SOIL.profile$SLLL[k])) {swilt[k]    <- (1 / poros[k]) * SOIL.profile$SLLL[k] } # wilting point as fraction of porosity  (volume fraction)
    if(!is.na(SOIL.profile$SSKS[k])) {hydraul[k]  <- SOIL.profile$SSKS[k] / (1000 * 3600)  } # SHC : saturated hydraulic conductivity (m s-1)
    if(!is.na(SOIL.profile$SLLL[k])) {suction[k]  <- 1.261*(SOIL.profile$SLLL[k]) + 0.027  } # AEP : air entry potential (m-H20)
    if(!is.na(SOIL.profile$AEP[k]))  {suction[k]  <- SOIL.profile$AEP[k]                   } # AEP : air entry potential (m-H20)
    if(!is.na(SOIL.profile$BEXP[k])) {bex[k]      <- SOIL.profile$BEXP[k]                  } # Campbell's 'b' exponent
    if(!is.na(SOIL.profile$SRGF[k])) {SRGF[k]     <- SOIL.profile$SRGF[k]                  } # Root hospitality factor 
    if(!is.na(SOIL.profile$SBDM[k])) {bulkd[k]    <- SOIL.profile$SBDM[k]                  } # Bulk density
    if(!is.na(SOIL.profile$Bperm[k])){bperm       <- SOIL.profile$Bperm[1]                 } # Soil hydraulic diffusivity lower b.c. (units???)
    
    ibex[k]     <- round(bex[k])
    soilbase[k] <- SOIL.profile$SLB[k]
    depth[k]    <- hsoi[k] * 100
    
    # Convert input sand and clay percents to fractions
    # Changed by TET
    # for now, we assume that all soils have a 1% organic content --
    # this is just a place holder until we couple the soil carbon
    # dynamics to the soil physical properties
    
    forganic  <- 0.010
    
    # density of soil material (without pores, not bulk) (kg m-3)
    # from Campbell and Norman, 1998
    rhosoi[k] <- 2650.0 * (1.0 - forganic) + 1300.0 * forganic
    
    # specific heat of soil material (j kg-1 k-1):
    # from Campbell and Norman, 1998
    csoi[k]   <- 870.0 * (1.0 - forganic) + 1920.0 * forganic
    
    # added for Green-Ampt
    cpwf[k]   <- cpwfdat[3]
    swater[k] <- 0.000001
    sice[k]   <- 0
    
    if(soilic==1){
      if((swic[k]/ poros[k])>= (swilt[k]-0.01) && (swic[k]/ poros[k]) <= 1.0) wsoi[k]<- swic[k]/poros[k]
    }

         wsoi[k] <- min(max(wsoi[k],swilt[k]),sfield[k])
   
    print(paste('Soil Properties from Profile',k,swilt[k]*poros[k],sfield[k]*poros[k],poros[k],
                hydraul[k],bex[k],bperm,suction[k],SRGF[k],bulkd[k], wsoi[k],sep=" / "))

      }


  #SVC Move carfrac and texfact from soilbgc to here, need to compute only once   
  # Top 1 m of soil 
  rdepth  <- 0
  carfrac <- 0
  texfact <- 0 
  
  # top 1 m of soil 
  for(k in 1:nsoilay) { 
    
    if(rdepth < 1){
      rdepth  <- rdepth + hsoi[k]
      carfrac <- carfrac + fracclay[k] * hsoi[k]
      texfact <- texfact + fracsand[k] * hsoi[k]
      print(paste(rdepth, carfrac, texfact))
    }
  }
  
  carfrac <- carfrac / rdepth
  texfact <- texfact / rdepth
  
  assign("wsoi",  wsoi, envir = env)
  assign("hsoi", hsoi, envir = env)
  assign("tsoi",  tsoi, envir = env)
  assign("rhosoi",  rhosoi, envir = env)
  assign("csoi",  csoi, envir = env)
  assign("fracsand",  fracsand, envir = env)
  assign("fracsilt",  fracsilt, envir = env)
  assign("fracclay",  fracclay, envir = env)
  assign("poros",  poros, envir = env)
  assign("sfield",  sfield, envir = env)
  assign("swilt",  swilt, envir = env)
  assign("bex",  bex, envir = env)
  assign("ibex",  ibex, envir = env)
  assign("suction",  suction, envir = env)
  assign("hydraul",  hydraul, envir = env)
  assign("cpwf",  cpwf, envir = env)
  assign("swater",  swater, envir = env)
  assign("sice",  sice, envir = env)

  assign("sand",  sand, envir = env)
  assign("clay",  clay, envir = env)

  assign("SRGF", SRGF, envir = env)
  assign("bulkd", bulkd, envir = env)
  assign("bperm", bperm, envir = env)
  assign("carfrac", carfrac, envir = env)
  assign("texfact", texfact, envir = env)
  assign("soilbase",  soilbase, envir = env)
  assign("depth",  depth, envir = env)
  
  # surface parameters
  # albsav -> saturated soil surface albedo (visible waveband)
  # albsan -> saturated soil surface albedo (near-ir waveband)
  assign("albsav",  fracsand[1] * 0.120 + fracsilt[1] * 0.085 + fracclay[1] * 0.050, envir = env)
  assign("albsan",  2.0 * albsav, envir = env)

  
  assign("isoilay",  min(isoilay,nsoilay), envir = env) # soil layer for which drainage and leaching data is output

  
}




# TODO: Substituir por função mais eficiente (reimplementar ou usar função built-in do R)

#------------------------------------------------------------------------------
#
#                             INPOLY
#   Function to tell if a point is inside a polygon or not.
#-------------------------------------------------------------------------------
#   Copyright (c) 1995-1996 Galacticomm, Inc.  Freeware source code.
#
#   Please feel free to use this source code for any purpose, commercial
#   or otherwise, as long as you don't restrict anyone else's use of
#   this source code.  Please give credit where credit is due.
# 
#   Point-in-polygon algorithm, created especially for World-Wide Web
#   servers to process image maps with mouse-clickable regions.
#
#   Home for this file:  http://www.gcomm.com/develop/inpoly.c
#
#                                       6/19/95 - Bob Stein & Craig Yap
#                                       stein@gcomm.com
#                                       craig@cse.fau.edu
#--------------------------------------------------------------------------------
#   Modified by:
#   Aris Gerakis, apr. 1998: 1.  translated to Fortran
#                            2.  made it work with real coordinates
#                            3.  now resolves the case where point falls
#                                on polygon border.
#   Aris Gerakis, nov. 1998: Fixed error caused by hardware arithmetic
#   Aris Gerakis, july 1999: Now all borderline cases are valid
#--------------------------------------------------------------------------------
#   Glossary:
#   function inpoly: true=inside, false=outside (is target point inside a 2D polygon?)
#   poly(*,2):  polygon points, [0]=x, [1]=y
#   npoints: number of points in polygon
#   xt: x (horizontal) of target point
#   yt: y (vertical) of target point
#--------------------------------------------------------------------------------

inpoly <- function(poly, npoints, xt, yt) {
  
  inside    <- FALSE
  on_border <- FALSE
  
  if(npoints < 3) {
    inpoly_output <- FALSE
    return(inpoly_output)
  }
  
  xold <- poly[npoints, 1]
  yold <- poly[npoints, 2]
  
  for(i in 1:npoints) {
    
    xnew <- poly[i, 1]
    ynew <- poly[i, 2]
    
    if (xnew > xold) {
      x1 <- xold
      x2 <- xnew
      y1 <- yold
      y2 <- ynew
    } else {
      x1 <- xnew
      x2 <- xold
      y1 <- ynew
      y2 <- yold
    }
    
    if ((xnew < xt && xt <= xold) || (!(xnew < xt) && !(xt <= xold))) {
      if ((yt-y1)*(x2-x1) == (y2-y1)*(xt-x1)) {
        on_border <- TRUE
      } else if(((yt-y1)*(x2-x1) < (y2-y1)*(xt-x1) + 0.001)) {
        inside <- !inside 
      }
      
    } else if((xnew == xt || xold == xt) && 
              (yt-y1)*(x2-x1) == (y2-y1)*(xt-x1) &&
              ((ynew <= yt && yt <= yold) || 
               (!(ynew < yt) && !(yt < yold)))) {
      
      on_border <- TRUE 
    }
    
    xold <- xnew
    yold <- ynew
  }
  
  # If test point is not on a border, the function result is the last state 
  # of INSIDE variable.  Otherwise, INSIDE doesn't matter.  The point is
  # inside the polygon if it falls on any of its borders:
  
  if (!on_border)
    inpoly_output <- inside
  else
    inpoly_output <- TRUE
  
  
  return(inpoly_output)
  
}




# -------------------------------------------------------------------------
# 
#                           T R I A N G L E
#
#  Main program that calls WHAT_TEXTURE, a function that classifies soil
#  in the USDA textural triangle using sand and clay %
#--------------------------------------------------------------------------
#  Created by: aris gerakis, apr. 98 with help from brian baer
#  Modified by: aris gerakis, july 99: now all borderline cases are valid
#  Modified by: aris gerakis, 30 nov 99: moved polygon initialization to
#               main program
#--------------------------------------------------------------------------
#  COMMENTS
#  o Supply a data file with two columns, in free format:  1st column sand,
# 2nd column clay %, no header.  The output is a file with the classes.
#--------------------------------------------------------------------------
#  You may use, distribute and modify this code provided you maintain
#  this header and give appropriate credit.
#--------------------------------------------------------------------------
textcls <- function(msand, mclay) {
  
  
  sandy      <- matrix(c(85, 90, 100, 0, 0, 0, 0,
                         0, 10,   0, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  loamy_sand <- matrix(c( 70, 85, 90, 85, 0, 0, 0,
                          0, 15, 10, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_loam <- matrix(c(50, 43, 52, 52, 80, 85, 70,
                         0,  7,  7, 20, 20, 15,  0), nrow = 7, ncol = 2)
  
  loam      <- matrix(c(43, 23, 45, 52, 52, 0, 0,
                        7, 27, 27, 20,  7, 0, 0),nrow = 7, ncol = 2)
  
  silty_loam <- matrix(c(0, 0, 23, 50, 0, 0, 0,
                         0, 27, 27, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_clay_loam <- matrix(c(52, 45, 45, 65, 80, 0, 0,
                              20, 27, 35, 35, 20, 0, 0), nrow = 7, ncol = 2)
  
  clay_loam <- matrix(c(20, 20, 45, 45, 0, 0, 0,
                        27, 40, 40, 27, 0, 0, 0), nrow = 7, ncol = 2)
  
  silty_clay_loam <- matrix(c(0,  0, 20, 20, 0, 0, 0, 
                              27, 40, 40, 27, 0, 0, 0), nrow = 7, ncol = 2)
  
  sandy_clay <- matrix(c(45, 45, 65, 0, 0, 0, 0,
                         35, 55, 35, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  silty_clay <- matrix(c(0,  0, 20, 0, 0, 0, 0,
                         40, 60, 40, 0, 0, 0, 0), nrow = 7, ncol = 2)
  
  clayey <- matrix(c(20,  0,   0, 45, 45, 0, 0,
                     40, 60, 100, 55, 40, 0, 0), nrow = 7, ncol = 2)
  
  
  
  # polygon coordinates  
  # 
  #      sand
  # 
  #      >  85, 90, 100, 0, 0, 0, 0,       ! sand
  #      >  70, 85, 90, 85, 0, 0, 0,       ! loamy sand
  #      >  50, 43, 52, 52, 80, 85, 70,    ! sandy loam
  #      >  43, 23, 45, 52, 52, 0, 0,      ! loam
  #      >   0, 0, 23, 50, 0, 0, 0,        ! silt loam (combined with silt)
  #      >  52, 45, 45, 65, 80, 0, 0,      ! sandy clay loam
  #      >  20, 20, 45, 45, 0, 0, 0,       ! clay loam
  #      >   0, 0, 20, 20, 0, 0, 0,        ! silty clay loam
  #      >  45, 45, 65, 0, 0, 0, 0,        ! sandy clay
  #      >   0, 0, 20, 0, 0, 0, 0,         ! silty clay 
  #      >  20, 0, 0, 45, 45, 0, 0         ! clay
  # 
  #       clay
  # 
  #      > 0, 10, 0, 0, 0, 0, 0,           ! sand
  #      > 0, 15, 10, 0, 0, 0, 0,          ! loamy sand
  #      > 0, 7, 7, 20, 20, 15, 0,         ! sandy loam 
  #      > 7, 27, 27, 20, 7, 0, 0,         ! loam
  #      > 0, 27, 27, 0, 0, 0, 0,          ! silt loam (combined with silt)
  #      > 20, 27, 35, 35, 20, 0, 0,       ! sandy clay loam
  #      > 27, 40, 40, 27, 0, 0, 0,        ! clay loam
  #      > 27, 40, 40, 27, 0, 0, 0,        ! silty clay loam
  #      > 35, 55, 35, 0, 0, 0, 0,         ! sandy clay
  #      > 40, 60, 40, 0, 0, 0, 0,         ! silty clay
  #      > 40, 60, 100, 55, 40, 0, 0       ! clay
  # 
  #-----------------------------------------------------------------------
  # figure out what texture grid cell and layer are part of  
  # classify a soil in the triangle based on sand and clay %
  #-----------------------------------------------------------------------
  # Created by: aris gerakis, apr. 98
  # Modified by: aris gerakis, june 99.  Now check all polygons instead of
  # stopping when a right solution is found.  This to cover all borderline 
  # cases.
  #-----------------------------------------------------------------------
  
  # C.Molling 8/18/2004
  # The following lines of code are necessary, as the algorithm always
  # gives siltloam if sand or clay is 0%.  We use a minimum of 1% for
  # sand and clay, and then adjust the greater of the two if the sum
  # is more than 100%.
  
  msand <- max(msand, 1)
  mclay <- max(mclay, 1)
  
  if (msand + mclay > 100) {
    if (msand > mclay) {
      msand <- msand - 1
    } else
      mclay <- mclay - 1
  }
  # end C. Molling 8/18/2004
  
  # find polygon(s) where the point is.  
  textcls_output <- 0
  
  
  if (msand > 0 && mclay > 0) {
    
    if(inpoly(sandy, 3, msand, mclay)) 
      textcls_output <- 1      # sand
    
    if(inpoly(loamy_sand, 4, msand, mclay))  
      textcls_output <- 2      # loamy sand
    
    if(inpoly(sandy_loam, 7, msand, mclay))  
      textcls_output <- 3      # sandy loam
    
    if(inpoly(loam, 5, msand, mclay))  
      textcls_output <- 4      # loam
    
    if(inpoly(silty_loam, 4, msand, mclay))  
      textcls_output <- 5      # silt loam
    
    if(inpoly(sandy_clay_loam, 5, msand, mclay))  
      textcls_output <- 6      # sandy clay loam
    
    if(inpoly(clay_loam, 4, msand, mclay))  
      textcls_output <- 7      # clay loam
    
    if(inpoly(silty_clay_loam, 4, msand, mclay))  
      textcls_output <- 8      # silty clay loam
    
    if(inpoly(sandy_clay, 3, msand, mclay))  
      textcls_output <- 9      # sandy clay
    
    if(inpoly(silty_clay, 3, msand, mclay))  
      textcls_output <- 10     # silty clay
    
    if(inpoly(clayey, 5, msand, mclay))  
      textcls_output <- 11     # clay
    
  }
  
  if (textcls_output == 0)
    textcls_output <- 5 #silt loam
  
  return(textcls_output)
  
}


