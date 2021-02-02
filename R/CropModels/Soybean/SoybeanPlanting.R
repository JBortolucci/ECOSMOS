
SoybeanPlanting <- function(iyear0, iyear, imonth, iday, jday, ffact, index) {
  
  i <- index
  
  # crop plant functional types only
  # in order to only allow a crop to be planted once each year
  # initialize cropplant = 0, but hold it = 1 through the end of the year
  
  if (day == pdmin[j] && month == pmmin[j] && croplive[j] != 1 &&  exist[j] == 1){ pstart[j] <- 0             }
  if (                                        croplive[j] != 1 &&  exist[j] == 1){ pstart[j] <- pstart[j] + 1 }
  
  
  if(pstart[i] >= 1  & exist[i] == 1 & croplive[i] != 1) {
    print(paste("Start planting  at ",iday,imonth,iyear," min date is ", pdmin[i], pmmin[i],sep=" / "))
  }
  
  
  #_______________________________________________________
  #   reset variables at the beginning  of crop year   
  
  if (iday == pcd[i] & imonth == pcm[i]) {
    
    pdate[i]     <- 0
    
    
    if (croplive[i] == 0) {
      cropplant[i] <- 0
    } 
    
    
    harvdate[i]   <- 999
    dmyield[i]    <- 0
    dmleaf[i]     <- 0
    dmstem[i]     <- 0
    dmroot[i]     <- 0
    dmresidue[i]  <- 0
    dmcrop[i]     <- 0
    residuen[i]   <- 0
    nconcl[i]     <- 0
    nconcs[i]     <- 0
    nconcr[i]     <- 0
    nconcg[i]     <- 0
    cropn[i]      <- 0
    cropfixn[i]   <- 0
    cntops[i]     <- 40
    cnroot[i]     <- 60
    fertinput[i]  <- 0
  } 
  
  
  #_______________________________________________________________    
  #__________________ Start Planting Block _______________________
  
  
  if (exist[i] == 1 & croplive[i] != 1 & cropplant[i] == 0) {
    
    
    # Plating block for Soybean, corn, and wheat
    
    
    if(cropy == 0 &  pstart[i] >=1 & pstart[i] <= 180) {
      
      croplive[i]     <- 1        # initialize freeze kill function to 1 - crops living 
      cropplant[i]    <- 1        # initialize freeze kill function to 1 - crops living 
      cropy            <- 1
      gddmaturity[i]  <- hybgdd[i]
      
      print(paste('1st Plant Soybean ', jday, imonth, iyear,hybgdd[i],gddmaturity[i],sep=' / '))
      
    }
    
    
    # add fertilizer nitrogen input for each crop planted (kg m-2)
    # on the planting date
    # either input here, or read from a gridded dataset
    # is treated a single, broadcast pulse at planting to the top soil layer
    # this fertilizer is assumed to be ammonium nitrate, in a 50/50 ratio
    # of NH4/NO3
    #
    # define amount of fertilizer added when crop is planted
    # use historical changes for years between 1945-1996 (Alexander et al.,) 
    # only add fertilizer on day of planting - use croplive funtion to
    # make sure that in successive years, idop from previous year doesn't
    # get applied here.
    #
    # also, don't cycle through all crop types - only the one that
    # is planted this year...otherwise it will zero out the fertilizer
    # values for pfts 13, 14 if going through all pfts through 15 
    
    if (jday == pdate[i] & croplive[i] == 1 & exist[i] == 1) {
      
      if (iyear < 1950) {
        fertnitro[i] <- 0.0009       # sugarcane - kg_n m-2 y-1
      } else if(iyear > 2000) {
        fertnitro[i] <- fertsgc[51]   * ffact * 1e-04 # sugarcane - kg_n m-2 y-1
      } else {
        fertnitro[i] <- fertsgc[iyear+1-1950]   * 1e-04 # sugarcane - kg_n m-2 y-1
      }
      
      # assign annual fertilizer input in kg/ha
      fertinput[i]  <- fertnitro[i] * 1.e+04
      
    } else if(exist[i] == 1) {
      fertnitro[i] <- 0     
    }
    
    
    if(idpp[i] == 1) {
      fertnitro[i] <- 0.025
    } else if(idpp[i] != 1) {
      fertnitro[i] <- 0
    }
    
  }
  
 
  assign("pstart", pstart, envir = env)
  assign("pdate", pdate, envir = env)
  assign("cropplant", cropplant, envir = env)
  assign("cropy", cropy, envir = env)
  assign("harvdate", harvdate, envir = env)
  assign("dmyield", dmyield, envir = env)
  assign("dmleaf", dmleaf, envir = env)
  assign("dmstem", dmstem, envir = env)
  assign("dmroot", dmroot, envir = env)
  assign("dmresidue", dmresidue, envir = env)
  assign("dmcrop", dmcrop, envir = env)
  assign("residuen", residuen, envir = env)
  assign("nconcl", nconcl, envir = env)
  assign("nconcs", nconcs, envir = env)
  assign("nconcr", nconcr, envir = env)
  assign("nconcg", nconcg, envir = env)
  assign("cropn", cropn, envir = env)
  assign("cropfixn", cropfixn, envir = env)
  assign("cntops", cntops, envir = env)
  assign("cnroot", cnroot, envir = env)
  assign("fertinput", fertinput, envir = env)
  assign("croplive", croplive, envir = env)
  assign("soydop", soydop, envir = env)
  assign("corndop", corndop, envir = env)
  assign("whtdop", whtdop, envir = env)
  assign("gddmaturity", gddmaturity, envir = env)
  assign("avehybrid", avehybrid, envir = env)
  assign("fertnitro", fertnitro, envir = env)
  
}
  
  
  
