############################################
# subroutine nitrostress(istep,iday,imonth) 
############################################
# calculates <- function  the effects of the amount of available inorganic {
# nitrogen on carbon assimilation in crops 
#
# strictly speaking, stressn * is multiplied to the vmax parameters 
# used in the photosynthesis calculations

# Global Vars:
# alphac                        # small amount of N allowed to diffuse to roots under low uptake conditions
# anuptake(npoi,nsoilay)        # actual nitrogen uptake for current timestep (kg_n/timestep)
# availn                        # amount (fraction) of inorganic N pool available to plants
# aybprod(npoi,npft)            # annual total biomass production for crops
# aylprod(npoi,npft)            # annual total leaf carbon accumulation for crops (kg-c/m**2/yr)
# ayrprod(npoi,npft)            # annual total root carbon accumulation for crops (kg-c/m**2/yr)
# cbiog(npoi,npft)              # carbon in grain biomass pool (kg_C m-2)
# cbios(npoi,npft)              # carbon in stem biomass pool (kg_C m-2)
# cfrac(npft)                   # fraction of dry matter production that is carbon
# cnmax                         # maximum c:n allowed of aboveground crop residue
# croplive(npoi,npft)           # 0 crops have been planted and living : 1 crops not living
# dtime                         # model timestep (seconds)
# ecpft                         # ending index for crop pfts
# exist(npoi,npft)              # probability of existence of each plant functional type in a gridcell
# fixn(npoi,npft)               # timstep total nitrogen fixation
# fngrain                       # current fraction of nitrogen in grain dry matter
# fngrmx(npft)                  # maximum amount of N allowed in grain at end of growing season
# fngrmxw(2)                    # grain nitrogen maximum allowed for wheat
# fnleaf                        # current fraction of nitrogen in leaf dry matter
# fnlfmx(npft)                  # maximum amount of N allowed in leaf at end of growing season
# fnlfmxw(2)                    # leaf nitrogen maximum allowed for wheat
# fnopt(npft)                   # minimum leaf nitrogen content that doesn't experience N stress
# fnoptw(2)                     # optimum leaf nitrogen fraction for wheat
# fnplant                       # current fraction of nitrogen in entire plant
# froot(nsoilay,2)              # current fraction of nitrogen in root dry matter
# fnstem                        # current fraction of nitrogen in stem dry matter
# froot                         # fraction of root in soil layer
# gddmaturity(npoi,npft)        # accumulated growing degrees needed for plant to reach both vegetative and physiological maturity
# hsoi(nsoilay+1)               # soil layer thickness (m)
# gddplant(npoi,npft)                # heat unit index
# iwheat                        # 0: wheat not planted 1: spring wheat planted 2: winter wheat (ibis.infile)
# npoi                          # total number of land points
# nsoilay                       # number of soil layers
# poros(npoi,nsoilay)           # porosity (mass of h2o per unit vol at sat / rhow)
# rratio(npft)                  # leaf:root N allocation ratio
# scpft                         # starting index for crop pfts - C. Kucharik
# sfield(npoi,nsoilay)          # field capacity soil moisture value (fraction of pore space)
# smsoil(npoi,-nsoilay:nsoilay) # current timestep solute in soil (kg_solute/m2)
# smsoln(npoi,-nsoilay:nsoilay) # current timestep solute in solution (kg_solute/m2)
# sratio(npft)                  # leaf:stem N allocation ratio
# stressl(npoi,nsoilay)         # soil moisture stress factor for the lower canopy (dimensionless)
# stressn(npoi,npft)            # stess factor applied to vmax based on leaf nitrogen content in crops (dimensionless)
# swilt(npoi,nsoilay)           # wilting soil moisture value (fraction of pore space)
# tnplant                       # total nitrogen in plant dry matter
# tnpptot(npoi)                 # instantaneous npp (mol-CO2 / m-2 / second)
# tnuptake(npoi,nsoilay)        # total potential nitrogen uptake for current timestep (kg_n/timestep)
# totnfix(npoi,npft)            # annual total nitrogen fixation through symbiosis (kg_n m-2 y-1)
# totnuptake(npoi,npft)         # annual total nitrogen uptake (kg_n m-2 y-1)
# upsoil(npoi,nsoilay)          # soil water uptake from transpiration (kg_h2o m-2 s-1)
# wisoi(npoi,nsoilay)           # fraction of soil pore space containing ice
# wsoi(npoi,nsoilay)            # fraction of soil pore space containing liquid water

nitrostress <- function (istep, iday, imonth)  {
  
  plantn <- 0
  
  # TODO: Colocar dentro do modelo do trigo
  # if(iwheat > 0) {
  #   fnlfmx[15] <- fnlfmxw[iwheat] 
  #   fngrmx[15] <- fngrmxw[iwheat]
  #   fnopt[15] <- fnoptw[iwheat]
  # }
  
  # PFT_UPDATE:
  # to do: Jair, colocar esses parametros como a ultima coluna da ultima tabela do params.crp
  
  # nitrogen from pools - above 1 means plant can take
  # some up in excess of what is in the transpiration stream 
  # fngrain[13] <- 0.035  
  # fngrain[14] <- 0.013
  # fngrain[15] <- 0.020  
  # fngrain[16] <- 0.013
  # fngrain[17] <- 0.013
  # fngrain[18] <- 0.013
  
  tnsupply <- 0
  awc      <- 0
  sumnval  <- 0
  
  # initialize layer nitrogen uptake
  for(k in seq(1,nsoilay)) { 
    tnuptake[k] <- 0
    anuptake[k] <- 0
  }
  
  # PFT_UPDATE: Percorre todas as culturas agrícolas, mas é generico.
  for(j in seq(1,npft)) {
    if(!plantList[[j]]$active) next
    if(plantList[[j]]$type == CROPS) {
      # PFT_UPDATE: Vetor exist pode desaparecer para culturas agricolas.
      if(exist[j]  ==  1) {
        if(croplive[j]  ==  1) { 
          fnmin <- cfrac[j] / cnmax
          stressn[j] <- 1
          gfn <- 1
          
          # calculate the total nitrogen supply rate (kg / m2 / day) for each soil layer based on
          # 1) the total daily water uptake for crops (lower canopy) (mm / day - stats.f)
          # 2) total available nitrogen pool to roots (soil solution) (kg - no3 / m2)
          # 3) and available water content (mm)
          #
          # NOTE : at this time, logic in IBIS cannot be used to determine
          # the uptake of nitrogen for each specific pft (mixed in each grid
          # cell because upsoil is for the entire lower canopy...will have
          # to weight it for now on the lai of that pft [frac[i,j]] 
          #
          # since it is being called each timestep, use instantaneous values
          # of soil ice and moisture
          for(k in 1:nsoilay ) { 
            # calculate water content in each layer - based on EPIC parameterizations
            # that look at actual water content in mm and not available water 
            wc <- max(0, (wisoi[k] + (1 - wisoi[k]) * wsoi[k])) * hsoi[k] * poros[k] * 1000
            
            
            # alphac is minimum uptake rate to account for nitrogen usage
            # even when transpiration is small (plant still able to take up
            # nitrogen)
            #
            # allow plant to take up nitrogen in excess of the
            # transpiration stream at low rates early in the
            # season 
            #
            # supply of nitrogen to crops is from roots corresponding to [l]ower
            # canopy in model - since this routine is being called each timestep
            # use the value of upsoil[i,k] from canopy.f rather than the value from
            # stats.f which is the daily average (adupsoil)   
            # upsoil is in units of mm / m2 / s of transpiration
            wsupply <- upsoil[k] * dtime 
            
            # make sure that water content is not zero -  
            # set to small limit
            wc <- max(1, wc)
            
            # the total nitrogen uptake from the layer comes from the total n
            # in the layer both in solution and in soil - leachable n is only
            # that portion that is in the solution
            #
            # value of tnuptake for this layer is used in in <- function  solute {
            # leaching algorithm as a net sink of nitrogen to the layer 
            #
            # only allow uptake in layers that have roots
            # make sure nitrogen uptake only occurs while crop is active
            # the minimum rate will only be applied when the plant is not
            # experiencing moisture stress
            if(froot[k,1]  >  0.005  &&  tnpptot  >  0) {
              tnuptake[k] <- max(alphac * stressl[k], wsupply) * availn * (smsoil[k] + smsoln[k]) 
            } else {
              tnuptake[k] <- 0
            }
          }
          
          if(aybprod[j] > 0 && aylprod[j]  >  0) {
            # for the purpose of dealing with total nitrogen uptake,
            # we have to use year to date total carbon production
            # in these equations because some root and leaf biomass
            # has been adjusted due to phenology in the model
            dmplant  <- aybprod[j] / cfrac[j]
            fdmleaf  <- (aylprod[j] / cfrac[j]) / dmplant
            fdmstem  <- (cbios[j] / cfrac[j]) / dmplant
            fdmroot  <- (ayrprod[j] / cfrac[j]) / dmplant
            fdmgrain <- (cbiog[j] / cfrac[j]) / dmplant
            
            dmres <- (aylprod[j] + cbios[j]) / cfrac[j] 
            flres <- (aylprod[j] / cfrac[j]) / dmres
            fsres <- (cbios[j] / cfrac[j]) / dmres  
            
            fnplant[j] <- max(0, totnuptake[j] / dmplant) 
            
            # maintain minimum nitrogen concentration in leaf and stem (potential residue)
            iter <- 0
            iter2 <- 0
            
            repeat {
              fnleaf[j] <- (fnplant[j] - fngrain[j] * fdmgrain) /
                (fdmleaf + sratio[j] * fdmstem +
                   rratio[j] * fdmroot)
              
              fnleaf[j] <- max(0, fnleaf[j])
              fnstem[j] <- fnleaf[j] * sratio[j]
              fnroot[j] <- fnleaf[j] * rratio[j]
              
              fnresidue <- fnleaf[j] * flres + fnstem[j] * fsres 
              if (fnresidue  >  fnmin) iter2 <- 1
              if (fnresidue  <=  fnmin) iter <- 1 
              if(iter2  ==  1  &&  fngrain[j] < fngrmx[j] && fdmgrain > 0 && 
                 iter == 0) { 
                fngrain[j] <- min(fngrmx[j], fngrain[j] * 1.01)
              } else {
                break
              }
            }
            
            ###################################################
            # calculate nitrogen content in various pools
            
            tngrain <- fngrain[j] * fdmgrain * dmplant
            tnleaf <- fnleaf[j] * fdmleaf * dmplant      
            tnstem <- fnstem[j] * fdmstem * dmplant      
            tnroot <- fnroot[j] * fdmroot * dmplant      
            
            tnplant[j] <- tngrain + tnleaf + tnstem + tnroot 
            
            fnsmax <- sratio[j] * fnlfmx[j]
            fnrmax <- rratio[j] * fnlfmx[j] 
            
            fnpmax <- fnlfmx[j] * fdmleaf + fnsmax * fdmstem + fnrmax * fdmroot + fngrmx[j] * fdmgrain               
            
            # calculate function controlling rate of nitrogen uptake
            # based on plant nitrogen concentration and maximum value
            # there is a chance early in growing season that fnplant
            # could be higher than fnpmax - thus maximize the below
            # fraction to be  <=  1 
            
            gfn <- 1 - min(1, fnplant[j] / fnpmax) ** 1 
            
            # calculate the annual running total of the actual nitrogen
            # uptake for all pfts in lower canopy (crops)
            #
            # adjust nitrogen uptake by plant by gfn factor - equal in each layer
            for(k in 1 : nsoilay) { 
              anuptake[k] <- tnuptake[k] * gfn
              tnsupply <- tnsupply + anuptake[k]
            } 
            
            totnuptake[j] <- totnuptake[j] + tnsupply              
            
            totnuptake[j] <- max(0, min((1 - cfrac[j]) * dmplant, totnuptake[j])) 
            
            # calculate stress parameter using rectangular hyperbola which
            # relates leaf nitrogen concentration to vmax        
            #
            # rectangular hyperbola 
            # f1 and f2  control the shape of the stress response function 
            # which spans from 0 to 1 for leaf n concentrations from 0 to 4 percent 
            #
            # s - shaped curve nitrogen limitation effect from epic model
            
            f1 <- 8.5 
            f2 <- 11 
            
            # ratio of leaf nitrogen concentration to the optimal maximum
            # for corn / maize
            # sant - I think that is best not use nitro stress for now, but after the model is ready include again.
            f3 <- 2 * (fnleaf[j] / fnopt[j])
            
            #----------------------------------------------------------------------
            # biological fixation of nitrogen through symbiosis in soybeans
            #----------------------------------------------------------------------
            #
            # Key reference:
            # M. Cabelguenne et al., Agricultural systems 60 : 175 - 196, 1999
            # this module is taken from the new epicphase model
            #
            # the amount of daily n - fixation by the plant is based on a fraction
            # of the total daily n - uptake.  It is controlled by three main factors:
            #
            # * growth stage of the crop (0 - 1) *
            # * soil moisture            (0 - 1) *  
            # * nitrogen in rooting zone (0 - 1) *  
            #
            # the growth stage factor (fxp) inhibits fixation in young plants
            # and old plants, and peaks between 30 - 55% of the crop cycle
            #
            # the soil water content factor (fxw) reduces n - fixation when the 
            # water content in the top 0.3 m is less than 85% of field capacity
            #
            # the soil nitrogen (plant available) factor (fxn) reduces n - fixation
            # when the nitrogen amount in the root zone is greater than 100 kg / ha  
            if(j  ==  13) {
              # calculate growth stage and factor (fraction of total average gdd)
              gs <- gddplant[j] / gddmaturity[j]
              
              if(gs  <=  0.15  ||   gs  >=  0.75) {
                fxg <- 0
              } else if(gs  >  0.15  &&  gs  <=  0.30) {
                fxg <- 6.67 * gs - 1
              } else if(gs  >  0.30  &&  gs  <=  0.55) {
                fxg <- 1
              } else {
                fxg <- 3.75 - 5 * gs   
              }
              
              # calculate effect of soil moisture in top 25 - 30 cm
              rdepth <- 1 / (hsoi[1] + hsoi[2])  
              
              fc <- 0
              wp <- 0
              sm <- 0
              plantn <- 0
              
              for(k in 1 : 2) { 
                fc <- fc + sfield[k] * hsoi[k]
                wp <- wp + swilt[k] * hsoi[k]
                sm <- sm + wsoi[k] * hsoi[k]
                
                # calculate available plant nitrogen total in these layers
                plantn <- plantn + smsoil[k] + smsoln[k]
              } 
              
              fc <- fc * rdepth
              wp <- wp * rdepth
              sm <- sm * rdepth
              sm <- min(sm, 0.85 * (fc - wp) + wp)
              
              fxw <- (sm - wp) / (0.85 * (fc - wp)) 
              
              # calculate effect of plant available nitrogen pool
              rd <- 1 # rooting depth in meters
              
              # equation for fxn has to be in kg / ha nitrogen and meters for rd 
              if(plantn  >  0.0300) {
                fxn <- 0
              } else if(plantn  <=  0.0100) {
                fxn <- 1
              } else {
                fxn <- 1.5 - 0.005 * (plantn * 10000) / rd
              }
              
              fxr <- min(1, fxw, fxn) * fxg 
              
              # fixn is thus calculated each timestep
              fixn[j] <- fxr * tnsupply 
              
              # update plant available nitrogen pool for fixation from soybean
            } else {  # non - soybean crop - no fixation
              fixn[j] <- 0
            } # soybeans only fix nitrogen
            
            totnfix[j] <- totnfix[j] + fixn[j]
            
            for(k in seq(1,nsoilay)) { 
              # critical : what quantity do we add the nitrogen fixation
              # to?  previous? depends on what order s <- function  are called {
              smsoil[k] <- smsoil[k] + fixn[j] * froot[k,1]
            }
          } # production gt 0
        } # crop plant  
      } # crop existence 
    } # crop pft 
    
    assign("fnlfmx", fnlfmx, envir = env)
    assign("fngrmx", fngrmx, envir = env)
    assign("fnopt", fnopt, envir = env)
    assign("fngrain", fngrain, envir = env)
    assign("tnuptake", tnuptake, envir = env)
    assign("anuptake", anuptake, envir = env)
    assign("stressn", stressn, envir = env)
    assign("fnplant", fnplant, envir = env)
    assign("fnleaf", fnleaf, envir = env)
    assign("fnstem", fnstem, envir = env)
    assign("fnroot", fnroot, envir = env)
    assign("tnplant", tnplant, envir = env)
    assign("totnuptake", totnuptake, envir = env)
    #assign("f1", f1, envir = env)
    #assign("f2", f2, envir = env)
    assign("fixn", fixn, envir = env)
    assign("totnfix", totnfix, envir = env)
    assign("smsoil", smsoil, envir = env)
  }
}