# Global Vars:
# adcsoln    # daily average nitrate concentration in solution (mg/liter)
# aleaf      # fraction allocation to leaves
# anuptake   # actual nitrogen uptake for current timestep (kg_n/timestep)
# aroot      # fraction allocation to fine roots
# assimn     # annual total nitrogen assimilated by natural vegetation (kg n m-2 y-1)
# awood      # fraction allocation to wood
# concn      # nitrate concentration at specified depth in solute.f (mg/liter or ppm)
# croplive   # 0 crops have been planted and living : 1 crops not living  
# cropn      # nitrogen removed by crop in kg/ha/yr
# csoln      # daily average nitrate concentration in solution (mg/liter)
# csoln      # previous timestep solute concentration in solution (mg/liter)
# ctot       # total inorganic nitrogen in soil profile (kg n  m-2)
# ctoti      # initial total inorganic nitrogen in profile at beginning of day (kg n m-2)
# daydrn     # daily drainage at specified depth in solute.f (mm/day)
# daynconc   # daily average nitrate concentration at specified depth in solute.f (mg/liter)  
# ddrn       # daily total drainage through specified layer in soil profile (mm/day)
# deposn     # annual total nitrogen deposition (wet and dry) (kg n m-2 y-1)
# deposn     # daily nitrogen deposition from atmosphere (kg n m-2 day-1)
# dnileach   # daily rate of nitrate-nitrogen leached from profile (kg no3 m-2 y-1)
# drntot     # annual total drainage through the soil profile (mm y-1)
# dtime      # model timestep (seconds)
# dtnleach   # daily total inorganic nitrogen leached from entire profile (kg (nh4 + no3) m-2 d-1)
# exist      # probability of existence of each plant functional type in a gridcell
# fertnitro  # nitrogen added as fertilizer to each crop
# fixn       # nitrogen fixation by crop in kg/ha/yr 
# fixn       # timstep total nitrogen fixation 
# fixsoin    # annual total nitrogen fixation by natural vegetation (kg n m-2 y-1) 
# fout       # current timestep total nitrogen leaching (flux)  (kg_solute/m2)
# fout       # gdd accumuation index for leaf emergence in crops
# froot      # fraction of root in soil layer 
# ftot       # annual total inorganic nitrogen leached from soil profile (kg n  m-2 y-1)
# fwpud      # h20 flux into top layer from puddle (kg m-2/s)
# fwtop      # h20 flux into top layer from evap/condens 
# hsoi       # soil layer thickness (m)
# idoy       # day of year counter  
# isoilay    # daily average soil ice content for each soil layer            #Henrique: duas definições para mesma sigla? (25/09/2020)
# isoilay    # soil layer for which drainage and leaching data is output
# nout       # current timestep nitrate leaching flux  (kg_solute/m2)
# smsoil     # current timestep solute in soil (kg_solute/m2)
# smsoln     # current timestep solute in solution (kg_solute/m2)
# snbalance  # annual soil nitrogen balance calculation (kg n ha-1)
# taninp     # total annual inorganic nitrogen inputs to soil (kg n m-2 y-1)
# tnmin      # instantaneous nitrogen mineralization (kg_N m-2/timestep)
# tnpp       # instantaneous NPP for each pft (mol-CO2 / m-2 / second)
# totnvegn   # annual total inorganic nitrogen uptake by natural vegetation (kg n m-2 y-1) 
# tpnuptake  # daily total plant nitrogen uptake (kg n m-2 d-1) 
# tsinp      # daily total inorganic nitrogen inputs (kg n m-2 d-1)
# tslay      # daily total inorganic nitrogen inputs to a soil depth determined in solute.f
# upsoil     # daily total soil water uptake from lower canopy (kg m-2/day)
# upsoil     # soil water uptake from transpiration (kg_h2o m-2 s-1)
# upsoiu     # soil water uptake from transpiration (kg_h2o m-2 s-1)
# wflo       # downward h20 flow across boundaries 
# wisoi      # daily average soil ice (fraction)
# wisoi      # fraction of soil pore space containing ice
# wsoi       # daily average soil moisture (fraction)
# wsoi       # fraction of soil pore space containing liquid water
# yfixsoin   # annual total nitrogen fixation by natural vegetation (kg n m-2 y-1) 
# yno3leach  # annual total nitrate-n leached from profile (kg n m-2 y-1)

leaching <- function (irstyear, istep,iday,
                      imonth,iyear,iholdsoiln,iyear0) { 
  
  
  depth <- array(0, nsoilay)
  snode <- array(0, nsoilay)
  fin <- matrix(0, 1, nsoilay)
  drn <- matrix(0, 1, nsoilay+1)
  
  # algorithms were derived from the ALEXI model (Anderson et al., 1999)
  # it keeps track of inorganic nitrogen pools and its location
  # and movement through the soil column profile.  Each layer has a total
  # quantity of solute  - where all is available to the plant for uptake -
  # however, a buffering constant keeps a fixed % N in soil solution at any time while the rest
  # is assumed bound to soil aggregates.  This allows for a buffering action to
  # be built in where as solution N is decreased, N is allowed to move from
  # the total soil pool to solution. 
  #
  # the inputs of nitrate to the model are : fertilizer, nitrogen deposition
  # nitrogen fixation in soybean, nitrogen mineralization through biogeochemistry
  #
  # the outputs are the leaching through the bottom of the soil profile, and
  # plant n uptake through carbon assimilation
  
  # set constants
  rwork <- dtime  * 12e-03 
  
  # these defined variables are also in biogeochem.f for decomposition
  # to maintain proper relationship between nitrogen mineralization and
  # nitrogen uptake here due to vegetation growth requirements, make sure
  # these values are consistent for natural vegetation
  cnwood <- 200
  cnfroot <- 80
  cnleaf <- 60
  
  co <- 0.0050      # initial inorganic nitrogen in soil storage pools 
  cf <- 0.07        # buffering constant for inorganic N in soil solution
  fnitrate <- 0.90  # fraction of leached inorganic nitrogen that is nitrate  - other portion is ammonium values from kr brye
  nloss <- 0.50     # fraction of immobile inorganic nitrogen that is lost annually to volatilization / denitrification 
  cndepth <- 0
  
  # set day of year counter
  if (iday == 1  && imonth == 1  && istep == 1) idoy <- 0 
  if (istep == 1) idoy <- idoy + 1
  
  #------------------------------------------------------------------------------
  # calculate cumulative depth to bottom of each layer
  # this only needs to be called once per run
  # depth could be local variable, where snode will be global
  #------------------------------------------------------------------------------
  
  for(k in 1:nsoilay ) { 
    if(k == 1) {
      depth[k] <- hsoi[k]
    } else {
      depth[k] <- depth[k - 1] + hsoi[k]
    }
  }
  
  for(k in 1:nsoilay) { 
    if(k == 1) { 
      snode[k] <- (depth[k + 1] - 0) / 2 
    } else if(k == nsoilay) {
      snode[k] <- (depth[k] - depth[k - 1]) / 2 
    } else {
      snode[k] <- (depth[k + 1] - depth[k - 1]) / 2             
    }
    cndepth <- cndepth + snode[k]
  }
  
  
  # set initial concentrations for first timestep of model run
  if(iyear == iyear0 && istep == 1  &&  iday == 1  && imonth == 1) { 
    
    ctoti <- 0  # total initial inorganic nitrogen in profile
    ctot <- 0  # total current inorganic nitrogen in soil profile (solution and soil) (kgm - 2)
    drntot <- 0  # total drainage 
    ftot <- 0  # total leaching of inorganic  nitrogen
    yno3leach <- 0  # total annual nitrate leaching
    assimn <- 0   
    snbalance <- 0  # soil nitrogen balance
    #browser()
    for(k in 1:nsoilay) { 
      smsoil[k] <- co * (snode[k] / cndepth)
      smsoln[k] <- cf * smsoil[k]
      
      # adjust mass balance
      smsoil[k] <- smsoil[k] - smsoln[k]
      
      # check conversion factor from kg m - 2 (ibis) to kg ha - 1 (kris' equations) to mg liter - 1
      # concentration in solution
      # have to account for ice fraction because wsoi can be 0 for much of the year 
      csoln[k] <- smsoln[k] * (1e+09) / (1e+04 * (wsoi[k] + wisoi[k]) * snode[k] * 10)   
      ctoti <- ctoti + smsoil[k] + smsoln[k]
    }
  } else if(iday == 1  && imonth == 1  && istep == 1) {
    # initialize annual drainage and solute leached to zero at beginning of year
    drntot <- 0     # total drainage through profile (mm)
    ftot <- 0     # cumulative inorganic nitrogen leached (kg m - 2)
    yno3leach <- 0     # annual total nitrate leaching (kg no3 m - 2)
    assimn <- 0
    totnvegn <- 0     # cumulative inorganic nitrogen uptake by natural vegetation (kg m - 2 y - 1)  
    taninp <- 0     # total annual inputs of inorganic nitrogen 
    snbalance <- 0     # annual soil nitrogen balance
    
    ctoti <- ctot # initial total inorganic N in profile at beginning of timestep 
    tsinp <- 0     # total daily inorganic N input
    tslay <- 0     # total daily inorganic N input to depth determined by soilay
    dtnleach <- 0     # daily total inorganic nitrogen solute (ammonium and nitrate) leached
    dnileach <- 0     # daily total nitrate-nitrogen leached
    tpnuptake <- 0     # total daily plant nitrogen uptake
    ctot <- 0     
    ddrn <- 0     # daily total drainage at specified depth in profile 
    
  } else if(istep == 1) {
    # initialize daily sums used in mass balance calculation
    ctoti <- ctot # initial total inorganic N in profile at beginning of timestep 
    tsinp <- 0     # total daily inorganic N input
    tslay <- 0     # total daily inorganic N input to depth determined by soilay
    dtnleach <- 0     # daily total inorganic nitrogen solute (ammonium and nitrate) leached
    dnileach <- 0     # daily total nitrate-nitrogen leached
    tpnuptake <- 0     # total daily plant nitrogen uptake
    ctot <- 0     
    ddrn <- 0     # daily total drainage at specified depth in profile 
  } else {  # all other timesteps
    fin[1] <- 0     # initialize input to top layer each timestep
    ctot <- 0
  }
  
  #------------------------------------------------------------------------------
  # calculating drainage (mm) through each node per timestep
  # note that the drainage is from the above layer through the top of (k) - thus
  # to get drainage out of the profile, have to assume that an additional hypothetical
  # layer exists below, where bperm influences the gdrain calculated in soil.f  
  #------------------------------------------------------------------------------
  drn[1] <- max(0, (fwtop + fwpud) * dtime)  # water infiltration (mm) into top layer
  
  # drainage out the bottom is an additional soil layer
  for(k in 2:(nsoilay + 1)) { 
    # rate of drainage through each node 
    # - calculated in soil.f (kg m - 2 s - 1) 
    # drn[i,nsoilay + 1] is equal to gdrain[i]  
    drn[k] <- max(0, wflo[k] * dtime)  
  }
  
  # designate which layer in the profile you are tracking drainage 
  # i.e. KRB measures drainage at ARL at 1.4 m with lysimeter
  drntot <- drntot + drn[isoilay] 
  ddrn <- ddrn + drn[isoilay]
  
  # determine nitrogen uptake by natural vegetation
  tprod <- 0
  
  # loop through natural vegetation types to check for carbon 
  # assimilation 
  for(j in 1:npft) { 
    if(!plantList[[j]]$active) next
    # total carbon assimilation for the timestep (tnpp has units of mol - co2 / m-2 / s)
    if(plantList[[j]]$type == NATURAL_VEG)
              tprod <- tprod + tnpp[j] * dtime  
  }
  
  # if natural vegetation has positive carbon assimilation for this timestep then calculate 
  # how much nitrogen was required for growth based on carbon / nitrogen ratios dictated in biogeochem.f
  cassn <- 0
  if(tprod > 0) { 
    for(j in 1:npft) {  # loop through natural vegetation types
      if(!plantList[[j]]$active) next
      # calculate assimilated carbon for this timestep and nitrogen required
      # to satisfy growth
      if(plantList[[j]]$type == NATURAL_VEG) {
          cassn <- cassn + (tnpp[j] * rwork * awood[j] / cnwood) +  (tnpp[j] * rwork * aleaf[j] / cnleaf) + (tnpp[j] * rwork * aroot[j] / cnfroot) 
      }
    }
    
    assimn <- assimn + cassn
    tupsoil <- 0 
    
    # calculate total transpiration stream for both [u]pper and [l]ower
    # canopies - calculated in canopy.f
    for(l in 1:nsoilay) { 
      tupsoil <- tupsoil + (upsoiu[l] + upsoil[l]) * dtime 
    }
    
    # determine that the fraction of the total nitrogen required for uptake
    # is proportional to the fraction of the total transpiration stream
    # contribution of that layer 
    #
    # anuptake is same variable used in crops.f for actual nitrogen uptake for that
    # timestep
    for(l in 1:nsoilay) { 
      if(tupsoil > 0) {
        fnuptake <- ((upsoiu[l] + upsoil[l]) * dtime) / tupsoil 
      } else {
        fnuptake <- 0
      }
      
      # add total nitrogen uptake for whole grid cell - could eventually
      # be a combinatation of natural vegetation and crops
      # anuptake is also being calculated in crops.f 
      anuptake[l] <- min((smsoil[l] + smsoln[l]), fnuptake  * cassn)
      
      # update annual nitrogen uptake by natural vegetation (forests / grasses / shrubs) 
      totnvegn <- totnvegn + anuptake[l] 
      
    }
    
  }  # natural vegetation nitrogen uptake 
  
  #------------------------------------------------------------------------------
  for(k in 1:nsoilay) { 
    # calculate the average root profile in the grid cell for upper and lower canopies
    # combined
    frootavg <- (froot[k,1] + froot[k,2]) / 2
    
    if(k == 1  && istep == 1) { 
      # first timestep / top layer - each day checked
      for(j in 1:npft) { 
        if(!plantList[[j]]$active) next
        # managed crop ecosystems: 
        # human dependency - get at time of planting
        # this could be changed in crops.f - to change timing / method of nitrogen fertilizer management
        # value of fertnitro[j] to be 0 at other times of year
        if(exist[j] != 0) {
          if(fertnitro[j] > 0 && croplive[j] == 1) {
            fin[1] <- fertnitro[j] 
          } else { 
            fin[1] <- 0
          } 
        }
      }
      
      # add nitrogen deposition to the amount coming into the top
      # layer for that particular day - based on daily precipitation
      # and calculated in biogeochem.f 
      # only added on beginning timestep of each day
      fin[1] <- fin[1] + deposn 
    } else {
      fout[1] <- 0
      nout[1] <- 0
      fin[k] <- fout[k] 
    }
    
    # nitrogen movement - potential inputs are fertilizer, n - deposition, n - fixation,n - mineralization.  
    # nitrogen mineralization is calculated in biogeochem.f as a daily rate and
    # was converted to mole-N s - 1
    #
    # have to assume where nitrogen mineralization is taking place in the profile -
    # and how amount of atmopheric n - fixation (daily quantity) is being added by roots
    # during this timestep (dtime)
    # use a weighted average of fine root distribution profiles for lower and upper plant canopies
    # fine root distribution is froot - initialized in initial.f  
    #
    # value for anuptake is calculated in nitrostress - which is applied to
    # the vmax rate for each timestep 
    #
    # if crops are planted, no n - fixation used from biogeochem.f 
    # that n - fixation is assumed from natural vegetation types that fix atmospheric N2
    #
    # n - fixation from soybeans is incorporated into soil inorganic N pools in crops.f
    # 
    #             if (icropsum[i] > 0) then
    #                 fixsoin[i] <- 0
    #                 yfixsoin[i] <- 0
    #              }
    #
    # assume natural nitrogen fixation <- 0 
    # cjk 11.18.01
    
    fixsoin <- 0
    yfixsoin <- 0
    
    smsoil[k] <- smsoil[k] + fin[k] +
      fixsoin * frootavg * dtime  / 86400 +  
      (tnmin * frootavg * dtime  * 0.014) -  
      anuptake[k]  
    
    # if total plant nitrogen uptake cannot be derived entirely from the immobile pool
    # then remove it from the solution pool
    if(smsoil[k] < 0) {
      deficit <- smsoil[k] 
      smsoln[k] <- max(0, smsoln[k] + deficit) 
      smsoil[k] <- 0
    }
    
    # keep sum of daily plant nitrogen uptake for mass balance calculation
    tpnuptake <- tpnuptake + anuptake[k] 
    smsoil[k] <- smsoil[k] - (cf * smsoil[k] - smsoln[k])
    smsoln[k] <- smsoln[k] + (cf * smsoil[k] - smsoln[k])
    
    # account for ice fraction when calculating concentration in solution
    csoln[k] <- smsoln[k] * (1e+09) / (1e+04 * (wisoi[k] + wsoi[k]) * snode[k] * 10)   
    
    # drainage is designated by subscript of layer it is going INTO 
    # fout is for current layer 
    #
    # KRBs units are kg ha - 1  - for fout
    # since we are kg m - 2  - need to reduce fout by larger constant
    #
    # we are interested in tracking nitrate only for leaching - add factor in these
    # equations to account for the fact that csoln can contain both ammonium and nitrate 
    # the fertnitro addition assumes that both nitrate and ammonium are added together
    fout[k+1] <- min(smsoln[k], csoln[k] * drn[k + 1] / 1e+06)
    nout[k+1] <- min(smsoln[k], csoln[k] * fnitrate * drn[k + 1] / 1e+06)
    fout[k+1] <- max(0, fout[k+1])
    nout[k+1] <- max(0, nout[k+1])
    
    # remove leached inorganic - N from layer reassign total nitrogen to each layer available to leach / total
    # redistribute inorganic - N in layer between solution and total in soil
    smsoln[k] <- smsoln[k] - fout[k+1]
    smsoil[k] <- smsoil[k] - (cf * smsoil[k] - smsoln[k])
    smsoln[k] <- smsoln[k] + (cf * smsoil[k] - smsoln[k])
    
    if(smsoil[k] < 0) {
      smsoil[k] <- 0
      smsoln[k] <- 0
    }
    
    ctot <- ctot + smsoln[k] + smsoil[k]
    
    # account for ice fraction in soil when calculating solute concentration
    csoln[k] <- smsoln[k] * (1e+09) / (1e+04 * (wisoi[k] + wsoi[k]) * snode[k] * 10)   
    
    # assign daily nitrate concentration for 1.4 m layer and
    # total amount of daily drainage through that layer for arlington comparisons 
    if(istep == 86400 / dtime) {
      daynconc[idoy] <- fnitrate  * adcsoln[isoilay]
      daydrn[idoy] <- ddrn
    }
  }  # loop through all soil layers
  
  # total inputs into the soil for this daily timestep
  tsinp <- tsinp + fin[1] + fixsoin * dtime  / 86400 + tnmin * dtime  * 0.014        
  
  # calculate the total daily inputs to top number of layers - determined by the
  # soilay constant - to help in mass balance calculation to that depth
  for(k in 1:isoilay) { 
    frootavg <- (froot[k,1] + froot[k,2]) / 2
    tslay <- tslay + tnmin * frootavg * dtime * 0.014 + fixsoin * frootavg * dtime  / 86400               
  }
  
  tslay <- tslay + fin[1]
  # calculate total amount of total nitrogen leached out of entire profile
  # for daily timestep
  
  dtnleach <- dtnleach + fout[nsoilay+1] 
  # convert to a rate (y - 1) at end of day for daily output for Simon
  # base on rate kg nitrate per hectare  - for layer we are interested in designated
  # as input to baseflow
  
  dnileach <- dnileach + nout[isoilay+1] 
  # update annual total nitrate-nitrogen leaching - covert to kg / ha
  # trying to compare to KRBs measurements at 1.4 m in profile
  # or input to baseflow at soilay <- 1.5 m for regional modeling 
  
  ftot <- ftot + fout[isoilay+1] * 1e+04
  yno3leach <- yno3leach + nout[isoilay+1] * 1e+04
  
  # end of year calculation for flow - weighted mean nitrate concentration 
  if(istep == 86400 / dtime  &&  
     imonth == 12 && iday == 31) { 
    
    # put in check for division by zero for annual drainage
    if(drntot <= 0) {
      concn <- 0
    } else {
      sum <- 0
      for(l in 1:idoy) { 
        sum <- sum + (daydrn[l] / drntot) * daynconc[l]
      }
      concn <- sum
    }
  }
  
  #------------------------------------------------------------------------------
  # calculate mass balance approach at each day to make sure solute is being
  # conserved 
  #------------------------------------------------------------------------------
  #
  # calculate each timestep addition of nitrogen fixation from nitrostress routine
  # in crops.f 
  
  # PFT_UPDATE: Genérico
  for(j in 1:npft) {
    if(!plantList[[j]]$active) next
    # only add fixed nitrogen to taninp for the top soil layers according to soilay
    for(k in 1: isoilay) { 
      taninp <- taninp + fixn[j] * froot[k,1]
    }
    
    # add fixation - which is a total for each timestep - to the total soil inputs
    tsinp <- tsinp + fixn[j]
  }
  
  if(istep == 86400 / dtime) {
    taninp <- taninp + tslay 
    
    bal1 <- ctoti + tsinp - dtnleach - ctot - tpnuptake
    
    # excess inputs vs. outputs 
    if(bal1 != 0) {      
      dif <- bal1
      ctot <- 0
      for(k in 1: nsoilay) { 
        smsoil[k] <- smsoil[k] + (dif * snode[k] / cndepth) 
        smsoil[k] <- smsoil[k] - (cf * smsoil[k] - smsoln[k])
        smsoln[k] <- smsoln[k] + (cf * smsoil[k] - smsoln[k])
        ctot <- ctot + smsoil[k] + smsoln[k]
      }
    }
    
    # convert to a rate (y - 1) at end of day for daily output for Simon
    # base on rate kg per hectare
    dnileach <- dnileach * 1e+04 * 365 
    
    # cropn is in units of kg / ha 
    ypnuptake <- 0
    for(j in 1:npft) {
      if(!plantList[[j]]$active) next
      ypnuptake <- ypnuptake  + cropn[j]
    }
    ypnuptake <- ypnuptake  + totnvegn * 1e+04
    
    # calculate nitrogen balance for soil - crops - natural vegetation - inputs
    # in kg ha - 1
    snbalance <- taninp * 1e+04 - ypnuptake - ftot
  }
  
  assign("idoy", idoy, envir = env)
  assign("ctoti", ctoti, envir = env)
  assign("ctot", ctot, envir = env)
  assign("drntot", drntot, envir = env)
  assign("ftot", ftot, envir = env)
  assign("yno3leach", yno3leach, envir = env)
  assign("assimn", assimn, envir = env)
  assign("snbalance", snbalance, envir = env)
  assign("smsoil", smsoil, envir = env)
  assign("smsoln", smsoln, envir = env)
  assign("csoln", csoln, envir = env)
  assign("totnvegn", totnvegn, envir = env)
  assign("taninp", taninp, envir = env)
  assign("tsinp", tsinp, envir = env)
  assign("tslay", tslay, envir = env)
  assign("dtnleach", dtnleach, envir = env)
  assign("dnileach", dnileach, envir = env)
  assign("tpnuptake", tpnuptake, envir = env)
  assign("ddrn", ddrn, envir = env)
  assign("anuptake", anuptake, envir = env)
  assign("fout", fout, envir = env)
  assign("nout", nout, envir = env)
  assign("fixsoin", fixsoin, envir = env)
  assign("yfixsoin", yfixsoin, envir = env)
  assign("daynconc", daynconc, envir = env)
  assign("daydrn", daydrn, envir = env)
  assign("concn", concn, envir = env)
  
  return() 
}

