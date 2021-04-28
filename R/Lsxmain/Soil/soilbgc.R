# Global Vars:
# clay      # percent clay of soil
# clitll    # carbon in leaf litter pool - lignin          (kg_C m-2)
# clitlm    # carbon in leaf litter pool - metabolic       (kg_C m-2)
# clitls    # carbon in leaf litter pool - structural      (kg_C m-2)
# clitrl    # carbon in fine root litter pool - lignin     (kg_C m-2)
# clitrm    # carbon in fine root litter pool - metabolic  (kg_C m-2)
# clitrs    # carbon in fine root litter pool - structural (kg_C m-2)
# clitwl    # carbon in woody litter pool - lignin         (kg_C m-2)
# clitwm    # carbon in woody litter pool - metabolic      (kg_C m-2)
# clitws    # carbon in woody litter pool - structural     (kg_C m-2)
# cnr       # C:N ratios of substrate pools and biomass for leaves and roots.
# cnroot    # cn ratio of plant roots 
# cntops    # cn ratio of plant residue
# croplive  # 0 crops have been planted and living : 1 crops not living  
# csoipas   # carbon in soil - passive humus               (kg_C m-2)
# csoislon  # carbon in soil - slow nonprotected humus     (kg_C m-2)
# csoislop  # carbon in soil - slow protected humus        (kg_C m-2)
# decompl   # litter decomposition factor                  (dimensionless)
# decomps   # soil organic matter decomposition factor     (dimensionless)
# deposn    # annual total nitrogen deposition (wet and dry) (kg n m-2 y-1)
# deposn    # daily nitrogen deposition from atmosphere (kg n m-2 day-1)
# effac     # efficiency of microbial biomass reincorporated into biomass pool. 
# exist     # probability of existence of each plant functional type in a gridcell
# falll     # annual leaf litter fall                      (kg_C m-2/year)
# fallr     # annual root litter input                     (kg_C m-2/year)
# fallw     # annual wood litter fall                      (kg_C m-2/year)
# fbsom     # protected biomass as a fraction of total soil organic C from Verberne et al., 1990
# fixsoin   # annual total nitrogen fixation by natural vegetation (kg n m-2 y-1) 
# fmxcpool  # maximum fraction allowed in resistant fraction (Verbene 1997)
# frac      # fraction of dry matter production that is carbon
# frac      # fraction of woody biomass that is in sapwood
# frac      # split of lignified litter material between protected/non-protected slow OM pools
# h20       # C leaching fudge factor
# harvdate  # day of year that crop pft was harvested
# hsoi      # soil layer thickness (m)
# kbn       # microbial biomass --> nonprotected om 
# kbp       # microbial biomass --> protected om
# kll       # leaf lignin
# klm       # leaf metabolic litter 
# kls       # leaf structural litter
# knb       # nonprotected om   --> biomass
# kns       # nonprotected om   --> passive c 
# kpb       # protected om      --> biomass
# kps       # protected om      --> passive c
# krl       # root lignin
# krm       # root metabolic litter
# krs       # root structural litter
# ksb       # passive c         --> biomass
# kwl       # wood  lignin
# kwm       # woody metabolic litter
# kws       # woody structural litter
# lig_frac  # split of lignified litter material between protected/non-protected slow OM pools
# ndaypy    # number of days per year
# ndepfact  # historical annual average N-deposition factor applied to equations in biogeochem.f          (1950-00)  
# nslaym    # number of soil layers to 1 m depth
# precip    # daily precitation (mm/day)
# rconst    # constant defined as 1200 (from Verbene 1997 equations)
# sand      # percent sand of soil
# tco2mic   # instantaneous microbial co2 flux from soil (mol-CO2 / m-2 / second)
# texdat    # sand/silt/clay fractions
# tnmin     # instantaneous nitrogen mineralization (kg_N m-2/timestep)
# totalit   # total standing aboveground litter (kg_C m-2)
# totanlit  # total standing aboveground nitrogen in litter (kg_N m-2)
# totcmic   # total carbon residing in microbial pools (kg_C m-2)
# totcsoi   # total carbon in all soil pools (kg_C m-2)
# totfall   # total litterfall and root turnover (kg_C m-2/year)
# totimm    # total immobilized nitrogen in timestep (kg_n m-2 timestep-1) 
# totlit    # total carbon in all litter pools (kg_C m-2)
# totmin    # total mineralized nitrogen in timestep (kg_n m-2 timestep-1) 
# totnlit   # total nitrogen in all litter pools (kg_N m-2)
# totnmic   # total nitrogen residing in microbial pool (kg_N m-2)
# totnrel   # total mineralized/immobilized nitrogen in timestep (non-microbial) (kg_n m-2 timestep-1) 
# totnsoi   # total nitrogen in soil (kg_N m-2)
# totrlit   # total root litter carbon belowground (kg_C m-2)
# totrnlit  # total root litter nitrogen belowground (kg_N m-2)
# ybn       # microbial biomass to nonprotected om
# ybp       # microbial biomass to protected om
# ydeposn   # annual total nitrogen deposition (wet and dry) (kg n m-2 y-1)
# yfixsoin  # annual total nitrogen fixation by natural vegetation (kg n m-2 y-1) 
# yll       # leaf lignin
# ylm       # leaf metabolic litter decomposition 
# yls       # leaf structural litter decomposition
# ynb       # nonprotected om to biomass
# yns       # nonprotected om to passive  c
# ypb       # protected om to biomass
# yps       # protected om to passive c
# yrl       # root lignin
# yrleach   # annual total amount C leached from soil profile (kg_C m-2/yr)
# yrm       # root metabolic litter decomposition
# yrs       # root structural litter decomposition
# ysb       # passive c to biomass
# ywl       # wood lignin
# ywm       # woody metabolic litter decomposition
# yws       # woody structural litter decomposition

soilbgc <- function  (iyear, iyear0, imonth, iday, jday, nspinsoil, spin, spinmax) {
  # Local arrays declaration
  outclm   <- 0
  outcls   <- 0
  outcll   <- 0
  outcrm   <- 0
  outcrs   <- 0
  outcrl   <- 0
  outcwm   <- 0
  outcws   <- 0
  outcwl   <- 0
  outcsb   <- 0
  outcps   <- 0
  outcns   <- 0
  outcnb   <- 0
  outcpb   <- 0
  outcbp   <- 0
  outcbn   <- 0
  totc     <- 0
  
  dbdt     <- 0
  dcndt    <- 0
  dcpdt    <- 0
  dcsdt    <- 0
  netmin   <- 0
  nbiors   <- 0
  nbiols   <- 0
  nbiows   <- 0
  nbiowm   <- 0
  nbiolm   <- 0
  nbiorm   <- 0
  nbioslon <- 0
  nbioslop <- 0
  nbiopas  <- 0
  nminrs   <- 0
  nminls   <- 0
  nminws   <- 0
  nminwm   <- 0
  nminlm   <- 0
  nminrm   <- 0
  nminslon <- 0
  nminslop <- 0
  nminpas  <- 0
  nrelps   <- 0
  nrelns   <- 0
  nrelbn   <- 0
  nrelbp   <- 0
  nrelll   <- 0
  nrelrl   <- 0
  nrelwl   <- 0
  ymintot  <- 0
  yminmic  <- 0
  
  # nitrogen in litter and soil pools
  nlitlm   <- 0
  nlitls   <- 0
  nlitll   <- 0
  nlitrm   <- 0
  nlitrs   <- 0
  nlitrl   <- 0
  nlitwm   <- 0
  nlitws   <- 0
  nlitwl   <- 0
  nsoislop <- 0
  nsoipas  <- 0
  nsoislon <- 0
  
  # variables controlling constraints on microbial biomass
  cmicn    <- 0
  cmicp    <- 0
  cmicmx   <- 0
  
  # variables controlling leaching, calculating co2 respiration and n deposition
  cleach    <- 0
  totcbegin <- 0
  totcend   <- 0
  totcin    <- 0
  

  # --------------------------------------------------------------------------
  #gridpt <- nlpoints		   # total number of gridpoints used
  
  #
  # ---------------------------------------------------------------------
  # calculate the fraction of wood, roots and leaves that are structural,
  # decomposable, and resistant based on equations presented in Verberne
  # model discussion (Geoderma, December 1997 special issue).  fmxcpool is the
  # maximum fraction allowed in resistant fraction, rconst is a constant
  # defined as 1200  The cnratio of each plant part has to be less than
  # the value of structural defined above (i.e. 150) otherwise the equations
  # are unstable...thus the wood litter pool value for cnr[6] is substituted
  # with a value higher than that for cnwood[i.e. 250].  this is 
  # insignificant for wood since 97% is structural anyways.
  #
  # ** NOTE ******** 
  # Would like to incorporate different C:N ratios of residue/roots for
  # different biome types based on literature search
  # average c:n ratio would be based on litter inputs from each pft
  # ****************
  # ---------------------------------------------------------------------
  #
  # equations were changed on 1 - 26 - 99 for erratum in literature (Whitmore
  # et al. 1997) which had an error in equations to split litterfall into
  # the correct three fractions
  #
  cnleaf  <- 60      # average c:n ratio for leaf litterfall
  cnfroot <- 80      # average c:n ratio for fine root turnover
  cnwood  <- 200     # average c:n ratio for woody debris
  # 
  # #
  # # total timesteps (daily) used to divide litterfall into daily fractions
  # # for natural vegetation.  Crops are added as a pulse input with totts <- 1
  # #
#  totts <- 1/180
   tottsl <- 1/30
   tottsr <- 1
   tottsw <- 1/220
   
  # #
  # # check to see if crops are planted - if so, modify c:n ratios
  # # crop residue is input on the harvest date
  # 
  # 
  # 
  # for(j in 1:npft) {
  # 
  #   if(plantList[[j]]$type == CROPS) {
  #     if(exist[j] == 1  && cntops[j] != 0  &&  harvdate[j] == jday) {
  #       cnleaf <- max(40, cntops[j])   # calculated in crop residue at harvest
  #       cnfroot <- max(60, cnroot[j])   # calculated in crop residue at harvest
  #       totts <- 1            # pulse input for crops residue
  #     } else if(exist[j] == 1  && croplive[j] == 1) { # PFT_UPDATE: Antes estava com 16, era só aplicado para cana?
  #       cnleaf <- max(40, cntops[j])   # calculated in crop residue at harvest
  #       cnfroot <- max(60, cnroot[j])   # calculated in crop residue at harvest
  #       totts <- 1 #put fall daily.
  #     }
  #   }
  # }
  
   # print(paste("falll/fallw/fallr",falll,fallw,fallr,sep="/"))
   
  
  
  # leaf litter 
  fracll <- fmxcpool * (cnleaf ** 2) / (rconst + cnleaf ** 2)
  fracls <- (1 / cnleaf - fracll / cnr[5] - (1 - fracll) / cnr[7])/
    (1 / cnr[6] - 1 / cnr[7])
  fraclm <- 1 - fracll - fracls
  
  # root litter
  fracrl <- fmxcpool * (cnfroot ** 2) / (rconst + cnfroot ** 2)
  fracrs <- (1 / cnfroot - fracrl / cnr[5] - (1 - fracrl) / cnr[7])/
    (1 / cnr[6] - 1 / cnr[7])
  fracrm <- 1 - fracrl - fracrs
  
  # wood litter
  fracwl <- fmxcpool * (cnwood ** 2) / (rconst + cnwood ** 2)
  fracws <- (1 / cnwood - fracwl / cnr[5] - (1 - fracwl) / cnr[7])/
    (1 / cnr[8] - 1 / cnr[7])
  fracwm <- 1 - fracwl - fracws
  
  # ---------------------------------------------------------------------
  # fraction of decomposing microbial biomass into protected organic
  # matter; taken from the model of Verberne et al., 1990
  # this is the proportion of decomposing dead microbial biomass that
  # is transferred to a protected pool vs. a non - protected pool
  # related to the clay content of the soil. in sandy soils, fbpom <- 0.3,
  # whereas in clay soils fbpom <- 0.7  created a linear function based
  # on clay fraction of soil to adjust according to amount of clay in
  # the top 1 m of profile (weighted average according to depth of each
  # layer)
  #
  # also take care of calculation of texfact, which is a leaching
  # parameter based on the average sand fraction of the top 1 m of
  # soil
  # ---------------------------------------------------------------------
  #
  ### Michel: Updated - its is calculated only once in the "inisoil" routine using the soil database information

  # rdepth <- 0
  # for(kk in 1: nslaym) {
  #   rdepth <- rdepth + hsoi[kk]
  # }
  # rdepth <- 1 / rdepth
  # carfrac <- 0
  # texfact <- 0
  # 
  # for(k in 1: nslaym) {
  #   if(k <= 6) {
  #     msand <- round(sand[k])
  #     mclay <- round(clay[k])
  #   } else {
  #     msand <- round(sand[6])
  #     mclay <- round(clay[6])
  #   }
  # }
  # 
  # # top 1 m of soil -- 8 layers
  # for(kk in 1: nslaym) {
  #   lmin <- textcls(msand,mclay)
  # 
  #   fsand <- texdat[1,lmin]
  #   fclay <- texdat[3,lmin]
  #   carfrac <- carfrac + fclay * hsoi[kk]
  #   texfact <- texfact + fsand * hsoi[kk]
  # }
  # 
  # carfrac <- carfrac * rdepth
  # texfact <- texfact * rdepth

  fbpom <- 0.50

  # ------------------------------------------------------------------------
  # ------------------------------------------------------------------------
  
 
  # ------------------------------------------------------------------------
  # total soil carbon initialized to 0 at beginning of model run
  # used in calculation of soil co2 respiration from microbial decomposition 
  # ------------------------------------------------------------------------
  if(iday == 1  && imonth == 1  && iyear == iyear0) {
    totcbegin <- 0
  }
  
  # ------------------------------------------------------------------------
  # initialize yearly summation of net mineralization and co2 respiration
  # to 0 at beginning of each year; because these quantities are usually 
  # reported on a yearly basis, we wish to do the same in the model so we
  # can compare easily with the data.
  # ------------------------------------------------------------------------
  if(iday == 1  && imonth == 1) {
    yrleach  <- 0
    cleach   <- 0
    ymintot  <- 0
    yminmic  <- 0
    ydeposn  <- 0
    yfixsoin <- 0
  }
  
  # determine amount of substrate available to microbial growth
  #
  # calculate the total amount of litterfall entering soil(C)
  
 
  
  totcin <- falll * tottsl + fallr * tottsr +  fallw * tottsw
  
  # print(totcin)
  
  # calculate the current total amount of carbon at each grid cell
  totc <- clitlm + clitls + clitrm + clitrs +
    clitwm + clitws + csoislop + csoislon +
    csoipas + totcmic + clitll + clitrl + clitwl
  
  # beginning amount of soil C at each timestep (used for respiration
  # calculation)
  totcbegin <- totc
  
  # ------------------------------------------------------------------------
  # split current amount of total soil microbes
  # maximum amount of biomass is a function of the total soil C
  # from Verberne et al., 1990
  # ------------------------------------------------------------------------
  cmicmx <- fbsom * totc 
  
  # calculate the amount of protected and unprotected biomass
  if(totcmic >= cmicmx) {
    cmicp <- cmicmx
    cmicn <- totcmic - cmicmx
  } else {
    cmicn <- 0
    cmicp <- totcmic
  }
  
  # ---------------------------------------------------------------
  # litter pools 
  #
  # add in the amount of litterfall, and root turnover
  # ---------------------------------------------------------------
  

  clitlm <- clitlm + (fraclm * falll * tottsl)  
  clitls <- clitls + (fracls * falll * tottsl)  
  clitll <- clitll + (fracll * falll * tottsl)  
  clitrm <- clitrm + (fracrm * fallr * tottsr)  
  clitrs <- clitrs + (fracrs * fallr * tottsr)  
  clitrl <- clitrl + (fracrl * fallr * tottsr)  
  clitwm <- clitwm + (fracwm * fallw * tottsw)  
  clitws <- clitws + (fracws * fallw * tottsw)  
  clitwl <- clitwl + (fracwl * fallw * tottsw)  

  # ---------------------------------------------------------------
  # calculate microbial growth rates based on available C sources
  # to microbes (substrate : litter, C in slow, passive pools)
  # the amount of biomass added cannot be larger than the amount of
  # available carbon from substrates and other pools at this point.
  # ---------------------------------------------------------------
  outcrs <- min(as.vector(decomps) * as.vector(krs) * as.vector(clitrs), clitrs)
  outcws <- min(as.vector(decompl) * as.vector(kws) * as.vector(clitws), clitws)
  outcls <- min(as.vector(decompl) * as.vector(kls) * as.vector(clitls), clitls)
  outclm <- min(as.vector(decompl) * as.vector(klm) * as.vector(clitlm), clitlm)
  outcrm <- min(as.vector(decomps) * as.vector(krm) * as.vector(clitrm), clitrm)
  outcwm <- min(as.vector(decompl) * as.vector(kwm) * as.vector(clitwm), clitwm)
  outcnb <- min(as.vector(decomps) * as.vector(knb) * as.vector(csoislon), csoislon)
  outcpb <- min(as.vector(decomps) * as.vector(kpb) * as.vector(csoislop), csoislop)
  outcsb <- min(as.vector(decomps) * as.vector(ksb) * as.vector(csoipas), csoipas)
  
  # ---------------------------------------------------------------
  # calculate turnover of microbial biomass
  # two disctinct pools: one with rapid turnover, and one with slow
  # turnover rate
  # ---------------------------------------------------------------
  outcbp <- min(kbp * cmicp,cmicp)
  outcbn <- min(kbn * cmicn,cmicn)
  
  # ---------------------------------------------------------------------
  # recycle microbes back to respective microbial pools based on effac as
  # discussed in NCSOIL model from Molina et al., 1983
  # ---------------------------------------------------------------------
  outcbp <- outcbp * effac
  outcbn <- outcbn * effac
  
  # -------------------------------------------------------------------------
  # have to adjust inputs into microbial pool for the slow
  # and passive carbon amounts that are leaving their respective
  # pools at an increased rate during the spinup procedure.
  # these values should be decreased by the respective spinup factors
  # because the microbial pools will otherwise become larger without
  # scientific reason due to the spinup relationships used.
  # 3 main pools: outcpb, outcnb, outcsb
  # -------------------------------------------------------------------------
  dbdt <- outcrs * yrs + outcws * yws +
    outcls * yls + outclm * ylm +
    outcrm * yrm + outcwm * ywm +
    outcnb * ynb +  
    outcpb * ypb +
    outcsb * ysb - outcbp -
    outcbn
  
  # -------------------------------------------------------------------------
  # change in non - protected organic matter from growth in microbial
  # biomass, lignin input, and stablized organic matter pool
  # the flow out of the pool from its decomposition is always less
  # the yield--which is factored into the pool it is flowing into
  # -------------------------------------------------------------------------
  outcll <- min(as.vector(decompl) * as.vector(kll) * as.vector(clitll),clitll)
  outcrl <- min(as.vector(decomps) * as.vector(krl) * as.vector(clitrl),clitrl)
  outcwl <- min(as.vector(decompl) * as.vector(kwl) * as.vector(clitwl),clitwl)
  outcns <- min(as.vector(decomps) * as.vector(kns) * as.vector(csoislon),csoislon)
  
  # ------------------------------------------------------------ 
  # the lig_frac  factor only applies to lignin content...half goes to
  # protected slow OM, and half goes to non protected slow OM
  # ------------------------------------------------------------
  dcndt <- (lig_frac * (outcll * yll + outcrl * yrl + outcwl * ywl) + (1 - fbpom) * (ybn * outcbn + ybp * outcbp)) - outcnb - outcns
  
  # ------------------------------------------------------------
  # change in protected organic matter from growth in microbial 
  # biomass, lignin input, and stablized organic matter pool
  # ------------------------------------------------------------
  outcps <- min(as.vector(decomps) * as.vector(kps) * as.vector(csoislop), csoislop)
  
  # ------------------------------------------------------------
  # the lig_frac factor only applies to lignin content...half goes to
  # protected slow OM, and half goes to non protected slow OM
  # ------------------------------------------------------------
  dcpdt <- (lig_frac * (outcll * yll + outcrl * yrl +
                          outcwl * ywl) +
              fbpom * (ybn * outcbn +
                         ybp * outcbp)) - outcpb - outcps
  
  # ----------------------------------------------------------------------
  # change in stablized organic matter (passive pool) from growth
  # in microbial biomass, and changes in protected and unprotected
  # SOM
  #
  # add a loss of C due to leaching out of the profile, based
  # on approximation of CENTURY model below 1 m in depth
  # based on water in the profile, and texture of soil
  # tuned to known outputs or leaching that has been measured in the field
  # at Arlington - WI (Courtesy K. Brye, MS) and applied to the global scale
  # on average, this calibration yields about 10 - 50 Kg C ha - 1 yr - 1 leaching
  # depending on C in soil...will need to be tied to an amount of water
  # flowing through the profile based upon precipitation eventually
  # ----------------------------------------------------------------------
  #
  #         h20 <- 0.30e-03
  #
  # h20 is a constant relating to the flow of water through the top 1 m of the
  # profile 
  # use texfact -- the % sand -- or texture factor effect on leaching (see Parton
  # et al. (1991) calculated from the average sand content of top 1 m of soil
  # in the model
  fleach <- h20 / 18 * (0.01 + 0.04 * texfact)
  
  # --------------------------------------------------------------------
  # change in passive organic carbon pool
  # ---------------------------------------------------------------------
  dcsdt <- ((yns * outcns) + (yps * outcps)) - outcsb - (fleach * csoipas)
  
  cleach <- fleach * csoipas + fleach * csoislop + fleach * csoislon
  
  # update slow pools of carbon for leaching losses
  dcndt <- dcndt - fleach * csoislon		
  dcpdt <- dcpdt - fleach * csoislop	
  
  if(spin == spinmax) {
    yrleach <- cleach + yrleach
  }
  
  # ---------------------------------------------------------------------
  # calculate the amount of net N mineralization or immobilization
  # ---------------------------------------------------------------------
  #
  # uptake of n by growth of microbial biomass
  #
  # immobilized n used for requirements of microbial growth
  # is based on flow of carbon and the difference of C / N ratio of
  # the microbes and their efficiency versus the C / N ratio of the
  # material that is being decomposed 
  #
  # ------------------------------
  # structural root decomposition 
  # ------------------------------
  if(yrs / cnr[1] > 1 / cnr[6]) {
    nbiors <- (1 / cnr[6] - yrs / cnr[1]) * outcrs
    nminrs <- 0
  } else {
    nminrs <- (1 / cnr[6] - yrs / cnr[1]) *  outcrs
    nbiors <- 0
  }
  
  # ------------------------------
  # structural leaf decomposition
  # ------------------------------
  if(yls / cnr[1] > 1 / cnr[6]) {
    nbiols <- (1 / cnr[6] - yls / cnr[1]) *  
      outcls
    nminls <- 0
  } else {
    nminls <- (1 / cnr[6] - yls / cnr[1]) *  
      outcls
    nbiols <- 0
  }
  
  # ------------------------------
  # structural wood decomposition
  # ------------------------------
  if(yws / cnr[1] > 1 / cnr[8]) {
    nbiows <- (1 / cnr[8] - yws / cnr[1]) *  
      outcws
    nminws <- 0
  } else {
    nminws <- (1 / cnr[8] - yws / cnr[1]) *  
      outcws
    nbiows <- 0
  }
  
  # ------------------------------
  # metabolic wood decomposition
  # ------------------------------
  if(ywm / cnr[1] > 1 / cnr[8]) {
    nbiowm <- (1 / cnr[8] - ywm / cnr[1]) *  outcwm
    nminwm <- 0
  } else {
    nminwm <- (1 / cnr[8] - ywm / cnr[1]) *  
      outcwm
    nbiowm <- 0
  }
  
  # ------------------------------
  # metabolic leaf decomposition
  # ------------------------------
  if(ylm / cnr[1] > 1 / cnr[7]) {
    nbiolm <- (1 / cnr[7] - ylm / cnr[1]) *  
      outclm
    nminlm <- 0
  } else {
    nminlm <- (1 / cnr[7] - ylm / cnr[1]) *  
      outclm
    nbiolm <- 0
  }
  # print(outclm)
  # ------------------------------
  # metabolic root decomposition
  # ------------------------------
  if(yrm / cnr[1] > 1 / cnr[7]) {
    nbiorm <- (1 / cnr[7] - yrm / cnr[1]) *  
      outcrm
    nminrm <- 0
  } else {
    nminrm <- (1 / cnr[7] - yrm / cnr[1]) * outcrm
    nbiorm <- 0
  }
  
  # ----------------------------------------------
  # non - protected organic matter decomposition
  # ----------------------------------------------
  if(ynb / cnr[1] > 1 / cnr[4]) {
    nbioslon <- (1 / cnr[4] - ynb / cnr[1]) * outcnb
    nminslon <- 0
  } else {
    nminslon <- (1 / cnr[4] - ynb / cnr[1]) * outcnb
    nbioslon <- 0
  }
  
  # ----------------------------------------------
  # protected organic matter decomposition
  # ----------------------------------------------
  if(ypb / cnr[1] > 1 / cnr[3]) {
    nbioslop <- (1 / cnr[3] - ypb / cnr[1]) * outcpb
    nminslop <- 0
  } else {
    nminslop <- (1 / cnr[3] - ypb / cnr[1]) * outcpb
    nbioslop <- 0
  }
  
  # ----------------------------------------------
  # stablized organic matter decomposition
  # ----------------------------------------------
  if(ysb / cnr[1] > 1 / cnr[2]) {
    nbiopas <- (1 / cnr[2] - ysb / cnr[1]) *outcsb
    nminpas <- 0
  } else {
    nminpas <- (1 / cnr[2] - ysb / cnr[1]) * outcsb
    nbiopas <- 0
  }
  
  # ----------------------------------------------
  # total immobilized N used for biomass growth
  # ----------------------------------------------
  totimm <- nbiors + nbiols + nbiows + nbiowm +  
    nbiolm + nbiorm + nbioslon + nbioslop +  
    nbiopas
  
  # -----------------------------------------------------------------------------
  # gross amount of N mineralized by decomposition of C by microbial biomass
  # assume that N is attached to the flow of C by the C / N ratio of the substrate
  # also assume that the amount of N attached to CO2 that is respired is also
  # mineralized (i.e. the amount of N mineralized is related to the total outflow
  # of carbon, and not the efficiency or yield)..see Parton et al., 1987
  # -----------------------------------------------------------------------------
  totmin <- nminrs + nminls + nminws + nminwm + nminlm + nminrm + nminslon + nminslop + nminpas
  
  # -----------------------------------------------------------------------------
  # when carbon is transferred from one pool to another, each pool has a distinct
  # C:N ratio.  In the case of pools where carbon is moving from the pool to 
  # the microbial biomass[used for growth / assimilation], net mineralization
  # takes place (N is released) after the requirements of building the biomass
  # are met.  In the cases of other transformations of C, N is not conserved
  # if it follows from one pool to another which has a different C:N ratio;
  # either N is released or is needed to make the transformation and keep N
  # conserved in the model. 
  #
  # other calculations of either N release or immobilization to keep track of
  # the budget
  #
  nrelps <- outcps * (1 / cnr[3] - 1 / cnr[2])
  nrelns <- outcns * (1 / cnr[4] - 1 / cnr[2])
  nrelbn <- (1 - fbpom) * outcbn * (1 / cnr[1] - 1 / cnr[4]) + (1 - fbpom) * outcbp * (1 / cnr[1] - 1 / cnr[4])
  nrelbp <- fbpom * outcbp * (1 / cnr[1] - 1 / cnr[3]) +
    fbpom * outcbn * (1 / cnr[1] - 1 / cnr[3])
  nrelll <- lig_frac * outcll * (1 / cnr[5] - 1 / cnr[3]) +
    lig_frac * outcll * (1 / cnr[5] - 1 / cnr[4])
  nrelrl <- lig_frac * outcrl * (1 / cnr[5] - 1 / cnr[3]) +
    lig_frac * outcrl * (1 / cnr[5] - 1 / cnr[4])
  nrelwl <- lig_frac * outcwl * (1 / cnr[5] - 1 / cnr[3]) +
    lig_frac * outcwl * (1 / cnr[5] - 1 / cnr[4])
  
  totnrel <- nrelps + nrelns + nrelbn + nrelbp + nrelll + nrelrl + nrelwl
  
  # -----------------------------------------------------------------------------
  # calculate whether net mineralization or immobilization occurs
  # on a grid cell basis -- tnmin is an instantaneous value for each time step
  # it is passed along to stats to calculate, daily, monthly and annual totals
  # of nitrogen mineralization
  #
  # this is for mineralization / immobilization that is directly related to 
  # microbial processes (oxidation of carbon)
  #
  # the value of totnrel[i] would need to be added to complete the budget
  # of N in the model. Because it can add / subtract a certain amount of N
  # from the amount of net mineralization.  However, these transformations
  # are not directly related to microbial decomposition, so do we add them
  # into the value or not?
  # -----------------------------------------------------------------------------
  
   netmin <- totmin + totimm + totnrel
   if(netmin > 0) {
     tnmin <- netmin
   } else {
     tnmin <- 0
   }
   
   # convert value of tnmin of Kg - N/m2 / dtime to mole-N / s
   # based on N <- .014 Kg / mole -- divide by the number of seconds in daily timestep
   tnmin <- tnmin / (86400 * 0.014)
   totmin <- totmin / (86400 * 0.014)
   totimm <- totimm / (86400 * 0.014)
   totnrel <- totnrel / (86400 * 0.014)

  # ---------------------------------------------------
  # update soil c pools for transformations of c and n
  # ---------------------------------------------------
  totcmic <- max(totcmic + dbdt, 0)
  csoislon <- max(csoislon + dcndt,0)
  csoislop <- max(csoislop + dcpdt,0)
  csoipas <- max(csoipas + dcsdt,0)
  clitlm <- max(clitlm - outclm,0)
  clitls <- max(clitls - outcls,0)
  clitll <- max(clitll - outcll,0)
  clitrm <- max(clitrm - outcrm,0)
  clitrs <- max(clitrs - outcrs,0)
  clitrl <- max(clitrl - outcrl,0)
  clitwm <- max(clitwm - outcwm,0)
  clitws <- max(clitws - outcws,0)
  clitwl <- max(clitwl - outcwl,0)
  
  # -----------------------------------------------------------
  # update soil n pools based on c:n ratios of each pool
  # this approach is assuming that the c:n ratios are remaining
  # constant through the simulation. flow of nitrogen is attached
  # to carbon 
  # -----------------------------------------------------------
  totnmic <- totcmic / cnr[1]
  nsoislon <- csoislon / cnr[4]
  nsoislop <- csoislop / cnr[3]
  nsoipas <- csoipas / cnr[2]
  nlitlm <- clitlm / cnr[7]
  nlitls <- clitls / cnr[6]
  nlitll <- clitll / cnr[5]
  nlitrm <- clitrm / cnr[7]
  nlitrs <- clitrs / cnr[6]
  nlitrl <- clitrl / cnr[5]
  nlitwm <- clitwm / cnr[8]
  nlitws <- clitws / cnr[8]
  nlitwl <- clitwl / cnr[8]
  
  # total above and belowground litter
  totlit <- clitlm + clitls + clitll + clitrm + clitrs + clitrl + clitwm + clitws + clitwl
  
  # sum total aboveground litter (leaves and wood)
  totalit <- clitlm + clitls + clitwm + clitll + clitws + clitwl
  
  # sum total belowground litter (roots) 
  totrlit <- clitrm + clitrs + clitrl
  
  # determine total soil carbon amounts (densities are to 1 m depth; Kg / m-2)
  totcsoi <- csoipas + csoislop + totcmic + csoislon
  
  # calculate total amount of litterfall occurring (total for year)
  totfall <- falll * tottsl + fallr  * tottsr + fallw  * tottsw
  
  # nitrogen 
  # total nitrogen in litter pools (above and belowground)
  totnlit <- nlitlm + nlitls + nlitrm + nlitrs + nlitwm + nlitws + nlitll + nlitrl + nlitwl
  
  # sum total aboveground litter   (leaves and wood)
  totanlit <- nlitlm + nlitls + nlitwm + nlitll + nlitws + nlitwl
  
  # sum total belowground litter  (roots)
  totrnlit <- nlitrm + nlitrs + nlitrl
  
  # total soil nitrogen to 1 m depth (kg - N/m ** 2)
  totnsoi <- nsoislop + nsoislon + nsoipas + totnmic + totnlit
  
  # --------------------------------------------------------------------------
  # calculate running sum of yearly net mineralization, and nitrogen in pool
  # available to plants for uptake--during spin up period, can only count one
  # of the cycles for each timestep--otherwise false additions will result
  # values of yearly mineralization are in Kg / m-2
  # --------------------------------------------------------------------------
  #
  # calculate total amount of carbon in soil at end of cycle
  # this is used to help calculate the amount of carbon that is respired
  # by decomposing microbial biomass
  totcend <- totlit + totcsoi
  
  # --------------------------------------------------------------------------
  # the amount of co2resp[i] is yearly value and is dependent on the amount
  # of c input each year, the amount in each pool at beginning of the year,
  # and the amount left in the pool at the end of the year
  # along with the amount of root respiration contributing to the flux from
  # calculations performed in stats.f
  # --------------------------------------------------------------------------
  if(spin == spinmax) { 
    # only count the last cycle in the spin - up for co2soi
    # when the iyear is less than the nspinsoil value...otherwise
    # an amount of CO2 respired will be about 10 times the actual
    # value because this routine is called articially 10 extra times
    # each time step to spin up the soil carbon
    #
    # add n - deposition due to rainfall once each day, and
    # the amount of N fixed through N - fixers.  These equations
    # are based on the annual precip input (cm) and are from
    # the CENTURY model...Parton et al., 1987
    # The base equations are in units of (g) N m - 2 so have to
    # divide by 1000 to put in units of Kg.
    #
    # the values in the equation of 0.21 and - 0.18 were adjusted to reflect
    # average daily inputs when no precipitation was falling - the original
    # constants are for the entire year 
    # --------------------------------------------------------------------------
    #
    #           deposn[i] <- (0.21 + 0.0028 * (precip[i] * 0.1)) * 1e-3
    #           fixsoin[i] <- ( - 0.18 + 0.14 * (precip[i] * 0.1)) * 1e-3
    #
    deposn <- (0.0005753 + 0.0028 * (precip * 0.1)) * 1e-3
    #
    # 7 - 06 - 05 CJK : modify rates for the US based on Simon's dataset that adjusts
    # the rates each year (from actual data - 1940 - 1999)
    if (iyear >= 1940 && iyear <= 1999) deposn <- deposn * ndepfact[iyear + 1-1940] 
    
    if (iyear > 1999) deposn <- deposn * ndepfact[60] 
    
    fixsoin <- ( - 0.0004932 + 0.14 * (precip * 0.1)) * 1e-3
    
    # From Landsberg and Gower, 1997
    # changes cjk 5 - 21 - 01 : research on rates of asymbiotic nitrogen fixation
    # which would be characterized more by IBIS than symbiotic, are about 1 - 2% of
    # the symbiotic potential.  So, adjusting fixsoin by that ratio.
    # also maximum allowable annual fixsoin is now 30 kg N ha - 1 based on
    # Landsberg and Gower, 1997
    #
    # typical asymbiotic n - fixation by natural vegetation ranges from 0.1 - 6
    # kg N ha - 1 in decidous and coniferous (temperate and boreal forests) to
    # 20 in tropical forests.
    if(yfixsoin >= 0.0030) {
      fixsoin <- 0 
    } else { 
      fixsoin <- fixsoin * 0.02
    }
    
    ydeposn <- ydeposn + deposn
    yfixsoin <- yfixsoin + fixsoin
    
    # --------------------------------------------------------------------------
    # add to the daily total of co2 flux leaving the soil from microbial
    # respiration -- instantaneous value for each timestep
    # since this subroutine gets called daily...instantaneous fluxes
    # the fluxes need to be put on a per second basis, which will be dependent
    # on the timestep.  Furthermore, because the biogeochem subroutine does
    # not get called each timestep...an approximation for a timestep average
    # microbial flux and nmineralization rate will be applied
    # --------------------------------------------------------------------------
    # calculate daily co2 flux due to microbial decomposition
    tco2mic <- totcbegin + totcin - totcend - cleach
    
    # convert co2 flux from kg C / day  (seconds in a daily timestep) to mol - C/s
    # based on .012 Kg C / mol
    tco2mic <- tco2mic / (86400 * 0.012)
  }
  
  

  falll <- max(falll - falll * tottsl, 0)
  fallr <- max(fallr - fallr * tottsr, 0)
  fallw <- max(fallw - fallw * tottsw, 0)

  
  assign("falll", falll, envir = env)
  assign("fallw", fallw, envir = env)
  assign("fallr", fallr, envir = env)
  
  assign("yrleach", yrleach, envir = env)
  assign("ydeposn", ydeposn, envir = env)
  assign("yfixsoin", yfixsoin, envir = env)
  assign("clitlm", clitlm, envir = env)
  assign("clitls", clitls, envir = env)
  assign("clitll", clitll, envir = env)
  assign("clitrm", clitrm, envir = env)
  assign("clitrs", clitrs, envir = env)
  assign("clitrl", clitrl, envir = env)
  assign("clitwm", clitwm, envir = env)
  assign("clitws", clitws, envir = env)
  assign("clitwl", clitwl, envir = env)
  assign("totimm", totimm, envir = env)
  assign("totmin", totmin, envir = env)
  assign("totnrel", totnrel, envir = env)
  assign("tnmin", tnmin, envir = env)
  assign("totcmic", totcmic, envir = env)
  assign("csoislon", csoislon, envir = env)
  assign("csoislop", csoislop, envir = env)
  assign("csoipas", csoipas, envir = env)
  assign("totnmic", totnmic, envir = env)
  assign("totlit", totlit, envir = env)
  assign("totalit", totalit, envir = env)
  assign("totrlit", totrlit, envir = env)
  assign("totcsoi", totcsoi, envir = env)
  assign("totfall", totfall, envir = env)
  assign("totnlit", totnlit, envir = env)
  assign("totanlit", totanlit, envir = env)
  assign("totrnlit", totrnlit, envir = env)
  assign("totnsoi", totnsoi, envir = env)
  assign("deposn", deposn, envir = env)
  assign("fixsoin", fixsoin, envir = env)
  assign("tco2mic", tco2mic, envir = env)
  assign("fmxcpool", fmxcpool, envir = env)
  assign("fraclm", fraclm, envir = env)
  assign("fracrm", fracrm, envir = env)
  assign("fracwm", fracwm, envir = env)
 
  
  return()
}

