

SugarcanePlanting <- function(year0, year, month, day, jday, ffact, index) {
  
  j <- index

    
    # in order to only allow a crop to be planted once each year
    # initialize cropplant = 0, but hold it = 1 through the end of the year
    
    if (day == pdmin[j] && month == pmmin[j] && croplive[j] != 1 &&  exist[j] == 1){
      pstart[j] <- cdays
    }
    
    
    if(pstart[j] == cdays && exist[j] == 1 && croplive[j] != 1) {
      print(paste("Start planting  at ",day,month,year," min date is ", pdmin[j], pmmin[j]," cdays ",cdays,sep=" / "))
    }
    
    
    #_______________________________________________________
    #   reset variables at the beginning  of crop year   
    
    if (day == pcd[j] && month == pcm[j]) {
      
      pdate[j]     <- 0
      
      
      if (croplive[j] == 0) {
        cropplant[j] <- 0
        if(j == j && exist[j] == 1 ) { cropy <- 0  }
      } 
  
      
      harvdate[j]   <- 999
      hdate[j]      <- 0
      harvidx[j]    <- 0
      croplaimx[j]  <- 0
      grainn[j]     <- 0
      cropyld[j]    <- 0
      dmyield[j]    <- 0
      dmleaf[j]     <- 0
      dmstem[j]     <- 0
      dmroot[j]     <- 0
      dmresidue[j]  <- 0
      dmcrop[j]     <- 0
      residuen[j]   <- 0
      nconcl[j]     <- 0
      nconcs[j]     <- 0
      nconcr[j]     <- 0
      nconcg[j]     <- 0
      cropn[j]      <- 0
      cropfixn[j]   <- 0
      cntops[j]     <- 40
      cnroot[j]     <- 60
      fertinput[j]  <- 0
      idppout[j]    <- 0
    } 
    
    
    #_______________________________________________________________    
    #__________________ Start Planting Block _______________________
    
    
    if (exist[j] == 1 && croplive[j] != 1 && cropplant[j] == 0) {
      

      
      #####################################################
      ############## Start of Sugarcane Crop ##############
      #####################################################
      
      if(    (cropy == 0 && j == j) &&
             a10td > ptemp[j] &&       # 10-day average soil temperature
             a10tmin > pmintemp[j] &&
             cdays >= pstart[j] && cdays < (pstart[j]+180) ) { # impose earliest planting date 
        
        # sant- to avoid that a crop that was killed by a frost be planted again in the same year. 
        croplive[j]     <- 1        # initialize freeze kill function to 1 - crops living 
        cropplant[j]    <- 1        # initialize freeze kill function to 1 - crops living 
        idop[j]         <- jday    
        cropy          <- 1
        
        gddmaturity[j]  <- min (gddsgcp, hybgdd[j])
        print(paste('Plant SugarCane in ',year, month, jday," GDD " ,gddsgcp, hybgdd[j], gddmaturity[j],sep=" / "))
        
      }
      #####################################################################
      ###################### end of Sugarcane Crop ####################### 
      #####################################################################
      
      
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
      
      if (jday == idop[j] && croplive[j] == 1 && exist[j] == 1) {
        if (year < 1950) {
         
          fertnitro[j] <- 0.0009       # sugarcane - kg_n m-2 y-1
              
          
        } else if(year > 2000) {
        
          fertnitro[j] <- fertsgc[51]   * ffact * 1e-04 # sugarcane - kg_n m-2 y-1

        } else {
          
          fertnitro[j] <- fertsgc[year+1-1950]   * 1e-04 # sugarcane - kg_n m-2 y-1
         
        }
        
        # assign annual fertilizer input in kg/ha
        fertinput[j]  <- fertnitro[j] * 1.e+04
      } else if(exist[j] == 1) {
        fertnitro[j] <- 0     
      }
      
      if(idpp[j] == 1 && j == j) {
        fertnitro[j] <- 0.025
      } else if(idpp[j] != 1 && j == j) {
        fertnitro[j] <- 0
      }
      
      
    }
    
    assign("pstart", pstart, envir = env)
    assign("pdate", pdate, envir = env)
    assign("cropplant", cropplant, envir = env)
    assign("cropy", cropy, envir = env)
    assign("harvdate", harvdate, envir = env)
    assign("hdate", hdate, envir = env)
    assign("harvidx", harvidx, envir = env)
    assign("croplaimx", croplaimx, envir = env)
    assign("grainn", grainn, envir = env)
    assign("cropyld", cropyld, envir = env)
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
    assign("idppout", idppout, envir = env)
    assign("croplive", croplive, envir = env)
    assign("idop", idop, envir = env)
    assign("soydop", soydop, envir = env)
    assign("corndop", corndop, envir = env)
    assign("whtdop", whtdop, envir = env)
    assign("gddmaturity", gddmaturity, envir = env)
    assign("avehybrid", avehybrid, envir = env)
    assign("fertnitro", fertnitro, envir = env)

  
}