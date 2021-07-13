# Global Vars:
# consoi   # thermal conductivity of each soil layer (W m-1 K-1)
# epsilon  # small quantity to avoid zero-divides and other
# fracclay
# fracsand
# fracsilt
# hvasug   # latent heat of vap/subl, for soil surface (J kg-1)
# hvasui   # latent heat of vap/subl, for snow surface (J kg-1)
# poros    # porosity (mass of h2o per unit vol at sat / rhow)
# qglif    # 1: fraction of soil evap (fvapg) from soil liquid
# ta       # air temperature (K)
# tmelt    # freezing point of water (K)
# tsno     # temperature of snow layers (K)
# tsoi     # soil temperature for each layer (K)
# wipud    # ice content of puddles per soil area (kg m-2)
# wisoi    # fraction of soil pore space containing ice
# wpud     # liquid content of puddles per soil area (kg m-2)
# wpudmax  # normalization constant for puddles (kg m-2)
# wsoi     # fraction of soil pore space containing liquid water
# zwpmax   # assumed maximum fraction of soil surface

setsoiR <- function(envi) {
  
  environment(hvapf) <- env
  environment(hsubf) <- env
  
  # sets diagnostic soil quantities
  
  # set soil layer quantities
  for(k in 1:nsoilay) { 
    
    # Convert input sand and clay percents to fractions
    
    # Change by TET
    #          if (k <= 4) then
    #            msand <- nint(sand[i,k])
    #            mclay <- nint(clay[i,k])
    
    #          } else {
    #            msand <- nint(sand[i,4])
    #            mclay <- nint(clay[i,4])
    #          }
    # 
    #          fsand <- 0.01 * msand
    #          fclay <- 0.01 * mclay
    #          fsilt <- 0.01 * (100 - msand - mclay)
    
    # update thermal conductivity (w m - 1 k - 1)
    
    # based on c <- c1 ** v1 * c2 ** v2 * c3 ** v3 * c4 ** v4 where c1,c2.
    # are conductivities of soil grains, air, liquid and ice
    # respectively, and v1,v2.. are their volume fractions 
    # (so v1 <- 1 - p where p is the porosity, and v1 + v2 + v3 + v4 <- 1).
    # then condry <- c1 ** (1 - p) * c2 ** p  is the dry - soil
    # conductivity, and c <- condry * (c3 / c2) ** v3 * (c4 / c2) ** v4, 
    # where c2 <- conductivity of air <- .025 w m - 1 k - 1
    # however this formula agrees better with williams + smith
    # table 4 for wet (unfrozen) sand and clay if c2 is decreased
    # to ~.005 (for peat in next section, ok if c2 <- .025).
    # also see lachenbruch etal,1982,jgr,87,9301 and refs therein.
    
    powliq <- poros[k] * wsoi[k] * (1 - wisoi[k])
    powice <- poros[k] * wisoi[k]
    
    #          zcondry <- fsand * 0.300 +
    # > fsilt * 0.265 +
    # > fclay * 0.250
    
    zcondry <- fracsand[k] * 0.300 + fracsilt[k] * 0.265 + fracclay[k] * 0.250 # +  
    # > forganic * 0.026      ! for future use CJK
    
    consoi[k] <- zcondry * ((0.56 * 100) ** powliq) *  ((2.24 * 100) ** powice)
    
  }
  
  # set qglif - the fraction of soil sfc evaporation from soil liquid,
  # soil ice, puddle liquid, and puddle ice (relative to total sfc evap)
  
  # zwpud:   fraction of surface area covered by puddle (range: 0 - zwpmax)
  # zwpmax:  maximum value of zwpud[currently assumed to be 0.5]
  # 1 - zwpud: fraction of surface area covered by soil (range: (1 - zwpmax) - 1)
  # zwsoi:   volumetric water content of top soil layer (range: 0 - 1)
  
  # qglif[i,1]: fraction of soil evap (fvapg) from soil liquid
  # qglif[i,2]: fraction of soil evap (fvapg) from soil ice
  # qglif[i,3]: fraction of soil evap (fvapg) from puddle liquid
  # qglif[i,4]: fraction of soil evap (fvapg) from puddle ice
  
  
  #       zwpmax <- 0.5
  zwpud <- max (0, min (zwpmax, zwpmax * (wpud + wipud) / wpudmax) )
  zwsoi <- (1 - wisoi[1]) * wsoi[1] + wisoi[1]
  
  if(zwsoi >= epsilon) {
    
    rwork1 <- 1 / zwsoi
    
    if(zwpud >= epsilon) {
      rwork2 <- 1 / (wpud + wipud)
      qglif[1] <- (1 - zwpud) * (1 - wisoi[1]) * wsoi[1] * rwork1
      qglif[2] <- (1 - zwpud) * wisoi[1] * rwork1
      qglif[3] <- zwpud * wpud * rwork2
      qglif[4] <- zwpud * wipud * rwork2
    } else {
      qglif[1] <- (1 - wisoi[1]) * wsoi[1] * rwork1
      qglif[2] <- wisoi[1] * rwork1
      qglif[3] <- 0
      qglif[4] <- 0
    }
    
  } else {
    
    # for a 100% dry soil surface, assign all soil evap to the puddles.
    # Note that for small puddle sizes, this could lead to negative
    # puddle depths. However, for a 100% dry soil with small puddles,
    # evaporation is likely to be very small or less than zero
    # (condensation), so negative puddle depths are not likely to occur.
    
    if(zwpud >= epsilon) {
      rwork2 <- 1 / (wpud + wipud)
      qglif[1] <- 0
      qglif[2] <- 0
      qglif[3] <- zwpud * wpud * rwork2
      qglif[4] <- zwpud * wipud * rwork2
    } else {
      if(tsoi[1] >= tmelt) {
        
        # above freezing
        
        qglif[1] <- 0
        qglif[2] <- 0
        qglif[3] <- 1
        qglif[4] <- 0
        
      } else {
        
        # below freezing
        
        qglif[1] <- 0
        qglif[2] <- 0
        qglif[3] <- 0
        qglif[4] <- 1
      }
    }
    
  }
  
  # set latent heat values
  
  zvap <- hvapf (tsoi[1], ta)
  zsub <- hsubf (tsoi[1], ta)
  
  hvasug <- (qglif[1] + qglif[3]) * zvap + (qglif[2] + qglif[4]) * zsub 
  
  hvasui <- hsubf(tsno[1],ta)

    
  assign("consoi", consoi, envir = env)
  assign("qglif", qglif, envir = env)
  assign("hvasug", hvasug, envir = env)
  assign("hvasui", hvasui, envir = env)
  
  return()
}

hvapf <- function(t,tair) {
  hvap + cvap*(tair-273.16) - ch2o*(t-273.16)
}


hsubf <- function(t,tair) {
  hsub + cvap*(tair-273.16) - cice*(t-273.16)
}
