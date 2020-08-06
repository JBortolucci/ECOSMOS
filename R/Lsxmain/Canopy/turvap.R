# Global Vars:
# bex        # exponent "b" in soil water potential
# ch2o       # specific heat of liquid water (J deg-1 kg-1)
# chl        # heat capacity of lower canopy leaves & stems per unit leaf/stem area (J kg-1 m-2)
# chs        # heat capacity of upper canopy stems per unit stem area (J kg-1 m-2)
# chu        # heat capacity of upper canopy leaves per unit leaf area (J kg-1 m-2)
# cice       # specific heat of ice (J deg-1 kg-1)
# cl         # air transfer coefficient (*rhoa) (m s-1 kg m-3) between the 2 canopies (z34 --> z12) (A36 Pollard & Thompson 1995)
# consno     # thermal conductivity of snow (W m-1 K-1)
# consoi     # thermal conductivity of each soil layer (W m-1 K-1)
# cp         # specific heat of air at za (allowing for h2o vapor) (J kg-1 K-1)
# csoi       # specific heat of soil, no pore spaces (J kg-1 deg-1)
# cu         # air transfer coefficient (*rhoa) (m s-1 kg m-3) for upper air region (z12 --> za) (A35 Pollard & Thompson 1995)
# dtime      # model timestep (seconds)
# epsilon    # small quantity to avoid zero-divides and other
# fi         # fractional snow cover
# firb       # net upward ir radiation at reference atmospheric level za (W m-2)
# firg       # ir radiation absorbed by soil/ice (W m-2)
# firi       # ir radiation absorbed by snow (W m-2)
# firl       # ir radiation absorbed by lower canopy leaves and stems (W m-2)
# firs       # ir radiation absorbed by upper canopy stems (W m-2)
# firu       # ir raditaion absorbed by upper canopy leaves (W m-2)
# fl         # fraction of snow-free area covered by lower  canopy
# frac       # fraction of canopy occupied by each plant functional type
# fsena      # downward sensible heat flux between za & z12 at za (W m-2)
# fseng      # upward sensible heat flux between soil surface & air at z34 (W m-2)
# fseni      # upward sensible heat flux between snow surface & air at z34 (W m-2)
# fsenl      # sensible heat flux from lower canopy to air (W m-2)
# fsens      # sensible heat flux from upper canopy stems to air (W m-2)
# fsenu      # sensible heat flux from upper canopy leaves to air (W m-2)
# fu         # fraction of overall area covered by upper canopy
# fvapa      # downward h2o vapor flux between za & z12 at za (kg m-2 s-1)
# fvapg      # h2o vapor flux (evaporation) between soil & air at z34 (kg m-2 s-1/bare ground fraction)
# fvapi      # h2o vapor flux (evaporation) between snow & air at z34 (kg m-2 s-1 / fi )
# fvaplt     # h2o vapor flux (transpiration) between lower canopy & air at z34 (kg m-2 s-1 / LAI lower canopy / fl)
# fvaplw     # h2o vapor flux (evaporation from wet surface) between lower canopy leaves & stems and air at z34 (kg m-2 s-1/ LAI lower canopy/ fl)
# fvaps      # h2o vapor flux (evaporation from wet surface) between upper canopy stems and air at z12 (kg m-2 s-1 / SAI lower canopy / fu)
# fvaput     # h2o vapor flux (transpiration from dry parts) between upper canopy leaves and air at z12 (kg m-2 s-1/ LAI upper canopy/ fu)
# fvapuw     # h2o vapor flux (evaporation from wet parts) between upper canopy leaves and air at z12 (kg m-2 s-1/ LAI upper canopy/ fu)
# fwetl      # fraction of lower canopy stem & leaf area wetted by intercepted liquid and/or snow
# fwetlx     # fraction of lower canopy leaf and stem area wetted if dew forms
# fwets      # fraction of upper canopy stem area wetted by intercepted liquid and/or snow
# fwetsx     # fraction of upper canopy stem area wetted if dew forms
# fwetu      # fraction of upper canopy leaf area wetted by intercepted liquid and/or snow
# fwetux     # fraction of upper canopy leaf area wetted if dew forms
# ginvap     # total evaporation rate from all intercepted h2o (kg_h2o m-2 s-1)
# grav       # gravitational acceleration (m s-2)
# greenfracl
# gsuvap     # total evaporation rate from surface (snow/soil) (kg_h2o m-2 s-1)
# gtrans     # total transpiration rate from all vegetation canopies (kg_h2o m-2 s-1)
# gtransl    # transpiration from lower canopy (kg_h2o m-2 s-1)
# gtransu    # transpiration from upper canopy (kg_h2o m-2 s-1)
# hsno       # thickness of snow layers (m)
# hsnotop    # thickness of top snow layer (m)
# hsoi       # soil layer thickness (m)
# hvap       # latent heat of vaporization of water (J kg-1)
# hvasug     # latent heat of vap/subl, for soil surface (J kg-1)
# hvasui     # latent heat of vap/subl, for snow surface (J kg-1)
# lai        # canopy single-sided leaf area index (area leaf/area veg)
# pfluxl     # heat flux on lower canopy leaves & stems due to intercepted h2o (W m-2)
# pfluxs     # heat flux on upper canopy stems due to intercepted h2o (W m-2)
# pfluxu     # heat flux on upper canopy leaves due to intercepted h2o (W m-2)
# poros      # porosity (mass of h2o per unit vol at sat / rhow)
# psurf      # surface pressure (Pa)
# q12        # specific humidity of air at z12
# q34        # specific humidity of air at z34
# qa         # specific humidity (kg_h2o/kg_air)
# rhosoi     # soil density (without pores, not bulk) (kg m-3)
# rhow       # density of liquid water (all types) (kg m-3)
# rliql      # proportion of fwetl due to liquid
# rliqs      # proportion of fwets due to liquid
# rliqu      # proportion of fwetu due to liquid
# rvap       # gas constant for water vapor (J deg-1 kg-1)
# sai        # current single-sided stem area index
# sg         # air-soil transfer coefficient
# si         # air-snow transfer coefficient
# sl         # air-vegetation transfer coefficients (*rhoa) for lower canopy leaves & stems (m s-1*kg m-3) (A39a Pollard & Thompson 1995)
# solg       # solar flux (direct + diffuse) absorbed by unit snow-free soil (W m-2)
# soli       # solar flux (direct + diffuse) absorbed by unit snow surface (W m-2)
# soll       # solar flux (direct + diffuse) absorbed by lower canopy leaves and stems per unit canopy area (W m-2)
# sols       # solar flux (direct + diffuse) absorbed by upper canopy stems per unit canopy area (W m-2)
# solu       # solar flux (direct + diffuse) absorbed by upper canopy leaves per unit canopy area (W m-2)
# ss         # air-vegetation transfer coefficients (*rhoa) for upper canopy stems (m s-1 * kg m-3) (A39a Pollard & Thompson 1995)
# stef       # stefan-boltzmann constant (W m-2 K-4)
# stressl    # soil moisture stress factor for the lower canopy (dimensionless)
# stresstl   # sum of stressl over all 6 soil layers (dimensionless)
# stresstu   # sum of stressu over all 6 soil layers (dimensionless)
# stressu    # soil moisture stress factor for the upper canopy (dimensionless)
# su         # air-vegetation transfer coefficients (*rhoa) for upper canopy leaves (m s-1 * kg m-3) (A39a Pollard & Thompson 1995)
# suction    # saturated matric potential (m-h2o)
# swilt      # wilting soil moisture value (fraction of pore space)
# t12        # air temperature at z12 (K)
# t34        # air temperature at z34 (K)
# ta         # air temperature (K)
# tfac       # (ps/p) ** (rair/cair) for atmospheric level  (const)
# tg         # soil skin temperature (K)
# ti         # snow skin temperature (K)
# tl         # temperature of lower canopy leaves & stems(K)
# tmelt      # freezing point of water (K)
# totcondc3  # 
# totcondc4
# totcondl3  # 
# totcondl4  # 
# totcondls  # 
# totcondub  # 
# totconduc  # 
# ts         # temperature of upper canopy stems (K)
# tsno       # temperature of snow layers (K)
# tsoi       # soil temperature for each layer (K)
# tu         # temperature of upper canopy leaves (K)
# upsoil     # soil water uptake from transpiration (kg_h2o m-2 s-1)
# upsoiu     # soil water uptake from transpiration (kg_h2o m-2 s-1)
# wipud      # ice content of puddles per soil area (kg m-2)
# wisoi      # fraction of soil pore space containing ice
# wliql      # intercepted liquid h2o on lower canopy leaf and stem area (kg m-2)
# wliqs      # intercepted liquid h2o on upper canopy stem area (kg m-2)
# wliqu      # intercepted liquid h2o on upper canopy leaf area (kg m-2)
# wpud       # liquid content of puddles per soil area (kg m-2)
# wpudmax    # normalization constant for puddles (kg m-2)
# wsnol      # intercepted frozen h2o (snow) on lower canopy leaf & stem area (kg m-2)
# wsnos      # intercepted frozen h2o (snow) on upper canopy stem area (kg m-2)
# wsnou      # intercepted frozen h2o (snow) on upper canopy leaf area (kg m-2)
# wsoi       # fraction of soil pore space containing liquid water

turvapR <- function (envi, iter, niter) {
  
  environment(hvapf) <- env
  environment(hsubf) <- env
  
  environment(impexp) <- env
  environment(impexp2) <- env
  environment(linsolve) <- env
  
  # ---------------------------------------------------------------------
  
  # solves canopy system with linearized implicit sensible heat and
  # moisture fluxes
  
  # first, assembles matrix arr of coeffs in linearized equations
  # for tu,ts,tl,t12,t34,q12,q34,tg,ti and assembles the right hand
  # sides in the rhs vector
  
  # then calls linsolve to solve this system, passing template mplate of
  # zeros of arr 
  # 
  # finally calculates the implied fluxes and stores them 
  # for the agcm, soil, snow models and budget calcs
  
  fradu <- 0
  frads <- 0
  fradl <- 0
  qu <- 0
  qs <- 0
  ql <- 0
  qg <- 0
  qi <- 0
  dqu <- 0
  dqs <- 0
  dql <- 0
  dqg <- 0
  dqi <- 0
  tupre <- 0
  tspre <- 0
  tlpre <- 0
  tgpre <- 0
  tipre <- 0
  suw <- 0
  ssw <- 0
  slw <- 0
  sut <- 0
  slt <- 0
  slt0 <- 0
  suh <- 0
  ssh <- 0
  slh <- 0
  qgfac <- 0
  qgfac0 <- 0
  
  nqn <- 9
  
  arr <- matrix(0, nqn, nqn)
  rhs <- matrix(0, 1, nqn) # right hand side
  vec <- matrix(0, 1, nqn) 
  
  mplate <- matrix(
    c(1,  0,  0,  1,  0,  1,  0,  0,  0, #tu
      0,  1,  0,  1,  0,  1,  0,  0,  0, #ts
      0,  0,  1,  0,  1,  0,  1,  0,  0, #tl
      1,  1,  0,  1,  1,  0,  0,  0,  0, #t12
      0,  0,  1,  1,  1,  0,  0,  1,  1, #t34
      1,  1,  0,  0,  0,  1,  1,  0,  0, #q12
      0,  0,  1,  0,  0,  1,  1,  1,  1, #q34
      0,  0,  0,  0,  1,  0,  1,  1,  0, #tg
      0,  0,  0,  0,  1,  0,  1,  0,  1) #ti)
    , nqn, nqn)
  
  
  # if first iteration, save original canopy temps in t * old
  # (can use tsoi,tsno for original soil / snow skin temps), for
  # rhs heat capacity terms in matrix soln, and for adjustment
  # of canopy temps after each iteration
  
  # also initialize soil / snow skin temps tg, ti to top - layer temps
  
  # the variables t12, t34, q12, q34, for the first iteration
  # are saved via global arrays from the previous gcm timestep,
  # this is worth doing only if the agcm forcing is
  # smoothly varying from timestep to timestep
  
  
  if(iter == 1) {
    
    # weights for canopy coverages
    xu <- 2 * lai[2] * fu
    xs <- 2 * sai[2] * fu
    xl <- 2 * (lai[1] + sai[1]) * fl * (1 - fi)
    
    
    # specific heats per leaf / stem area
    chux <- chu + ch2o * wliqu + cice  * wsnou
    chsx <- chs + ch2o * wliqs + cice  * wsnos
    chlx <- chl + ch2o * wliql + cice  * wsnol
    
    
    rwork <- poros[1] * rhow
    
    chgx <- ch2o * wpud + cice  * wipud + ((1 - poros[1]) * csoi[1] * rhosoi[1] + rwork * (1 - wisoi[1]) * wsoi[1] * ch2o + rwork * wisoi[1] * cice) * hsoi[1]
    
    wlgx <- wpud + rwork * (1 - wisoi[1]) * wsoi[1] * hsoi[1]
    
    wigx <- wipud + rwork * wisoi[1] * hsoi[1]

    # conductivity coeffs between ground skin and first layer
    cog <- consoi[1] / (0.5 * hsoi[1])
    coi <- consno / (0.5 * max (hsno[1], hsnotop))
    
    # d[ir emitted] / dt for soil
    rwork <- 4 * 0.95 * stef
    
    zirg <- rwork * (tg ** 3)
    ziri <- rwork * (ti ** 3)
    
    # updated temperature memory
    tuold <- tu
    tsold <- ts
    tlold <- tl
    tgold <- tg
    tiold <- ti
    
  }
  
  # set implicit / explicit factors w * (0 to 1) for this iteration
  # w * is 1 for fully implicit, 0 for fully explicit
  # for first iteration, impexp and impexp2 set w * to 1
  wu <- impexp (wu, tu, chux, wliqu, wsnou, iter)
  ws <- impexp (ws, ts, chsx, wliqs, wsnos, iter)
  wl <- impexp (wl, tl, chlx, wliql, wsnol, iter)
  wg <- impexp (wg, tg, chgx, wlgx,  wigx,  iter)
  
  # call impexp2 for snow model
  wi <- impexp2 (wi, ti, tiold, iter)
  
  # adjust t * for this iteration 
  
  # in this routine we are free to choose them, 
  # since they are just the central values about which the 
  # equations are linearized - heat is conserved in the matrix
  # solution because t * old are used for the rhs heat capacities
  
  # here, let t * represent the previous soln if it was fully
  # implicit, but weight towards t * old depending on the amount
  # (1 - w*) the previous soln was explicit
  
  # this weighting is necessary for melting / freezing surfaces, for which t*
  # is kept at t * old, presumably at or near tmelt
  
  tu <- wu * tu + (1 - wu) * tuold
  ts <- ws * ts + (1 - ws) * tsold
  tl <- wl * tl + (1 - wl) * tlold
  tg <- wg * tg + (1 - wg) * tgold
  ti <- wi * ti + (1 - wi) * tiold
  
  # save current "central" values for final flux calculations
  tupre <- tu
  tspre <- ts
  tlpre <- tl
  tgpre <- tg
  tipre <- ti
  
  
  # calculate various terms occurring in the linearized eqns,
  # using values of t12, t34, q12, q34 from
  # the previous iteration
  
  # specific humidities for canopy and ground, and derivs wrt t
  # for canopy
  
  # limit derivs to avoid - ve implicit q's below,
  # as long as d[temp]s in one iteration are le 10 deg k
    
  e <- esat(tu)
  qu <- qsat (e, psurf)
  dqu <- dqsat (tu, qu)
  dqu <- min (dqu, qu * 0.1)
  
  e <- esat(ts)
  qs <- qsat (e, psurf)
  dqs <- dqsat (ts, qs)
  dqs <- min (dqs, qs * 0.1)
  
  e <- esat(tl)
  ql <- qsat (e, psurf)
  dql <- dqsat (tl, ql)
  dql <- min (dql, ql * 0.1)
  
  e <- esat(tg)
  qg <- qsat (e, psurf)
  dqg <- dqsat (tg, qg)
  dqg <- min (dqg, qg * 0.1)
  
  e <- esat(ti)
  qi <- qsat (e, psurf)
  dqi <- dqsat (ti, qi)
  dqi <- min (dqi, qi * 0.1)
    
  
  # set qgfac0, factor by which soil surface specific humidity
  # is less than saturation
  #
  # it is important to note that the qgfac expression should
  # satisfy timestep cfl criterion for upper - layer soil moisture
  # for small wsoi[i,1]
  #
  # for each iteration, qgfac is set to qgfac0, or to 1 if
  # condensation onto soil is anticipated (loop 110 in canopy.f)
  #
  # Evaporation from bare soil is calculated using the "beta method"
  # (e.g., eqns 5 & 7 of Mahfouf and Noilhan 1991, JAM 30 1354 - 1365),
  # but converted to the "alpha method" (eqns 2 & 3 of M&N), to match
  # the structure in IBIS. The conversion from the beta to alpha
  # method is through the relationship:
  #   alpha * qgs - q34 <- beta * (hfac * qgs - q34),
  # from which one solves for alpha (which is equal to qgfac0):
  #   qgfac0 <- alpha <- (beta * hfac) + (1 - beta) * (q34 / qgs)
    
  # first calculate the total saturated fraction at the soil surface
  # (including puddles ... see soil.f)
  
  zwpud <- max (0, min (0.5, 0.5 * (wpud + wipud) / wpudmax) )
  zwsoi <- (1 - wisoi[1]) * wsoi[1] + wisoi[1]
  zwtot <- zwpud + (1 - zwpud) * zwsoi
  
  # next calculate the matric potential (from eqn 9.3 of Campbell and
  # Norman), multiply by gravitational acceleration to get in units
  # of J / kg, and calculate the relative humidity at the soil water
  # surface (i.e., within the soil matrix), based on thermodynamic
  # theory (eqn 4.13 of C&N)
  
  psig <-  - grav * suction[1] * (zwtot ** (-bex[1]))
  hfac <- exp(psig / (rvap * tg))
  
  # then calculate the relative humidity of the air (relative to
  # saturation at the soil temperature). Note that if hfac2 > 1
  # (which would imply condensation), then qgfac is set to 1
  # later in the code (to allow condensation to proceed at the
  # "potential rate")
  
  hfac2 <- q34 / qg
  
  # set the "beta" factor and then calculate "alpha" (i.e., qgfac0)
  # as the beta - weighted average of the soil water RH and the "air RH"
  # First calculate beta_w:
  
  zwopt <- 1
  zwdry <- swilt[1]
  betaw <- max(0, min(1, (zwtot - zwdry) / (zwopt - zwdry)) )
  
  # limit evap if soil is frozen or snow - covered
  if(tg <= 273.16 || fi > 0) {
    betaw <- 0.01
  }
  
  # Next convert beta_w to beta_s (see Milly 1992, JClim 5 209 - 226):
  
  emisoil <- 0.95
  e <- esat(t34)
  qs1 <- qsat (e, psurf)
  dqs1 <- dqsat (t34, qs1)
  xnumer <- hvap * dqs1
  xdenom <- cp + (4 * emisoil * stef * (t34) ** 3) / sg
  betafac <- xnumer / xdenom
  betas <- betaw / (1 + betafac * (1 - betaw))
  
  # Combine hfac and hfac2 into qgfac0["alpha"] using beta_s
  
  qgfac0 <- betas * hfac + (1 - betas) * hfac2
  
  
  # set fractions covered by intercepted h2o to 1 if dew forms
  
  # these fwet * x are used only in turvap, and are distinct from
  # the real fractions fwet * that are set in fwetcal
  
  # they must be exactly 1 if q12 > qu or q34 > ql, to zero transpiration
  # by the factor 1 - fwet[u,l]x below, so preventing " - ve" transp
  
  # similarly, set qgfac, allowing for anticipated dew formation
  # to avoid excessive dew formation (which then infiltrates) onto
  # dry soils
  fwetux <- fwetu
  if (q12 > qu) fwetux <- 1
  
  fwetsx <- fwets
  if (q12 > qs) fwetsx <- 1
  
  fwetlx <- fwetl
  if (q34 > ql) fwetlx <- 1
  
  qgfac <- qgfac0
  if (q34 > qg) qgfac <- 1
  
  # set net absorbed radiative fluxes for canopy components
  fradu <- 0
  
  if (lai[2] > epsilon)
    fradu <- (solu + firu) / (2 * lai[2])
  
  frads <- 0
  
  if (sai[2] > epsilon)
    frads <- (sols + firs) / (2 * sai[2])
  
  fradl <- 0
  
  if ((lai[1] + sai[1]) > epsilon)
    fradl <- (soll + firl) /
    (2 * (lai[1] + sai[1]))
    
  
  
  # calculate canopy - air moisture transfer coeffs for wetted
  # leaf / stem areas, and for dry (transpiring) leaf areas
  
  # the wetted - area coeffs suw,ssw,slw are constrained to be less
  # than what would evaporate 0.8 * the intercepted h2o mass in 
  # this timestep (using previous iteration's q * values)
  
  # this should virtually eliminate evaporation - overshoots and the need
  # for the "negative intercepted h2o"  correction in steph2o2
  #        
  
  # coefficient for evaporation from wet surfaces in the upper canopy:
  
  suw <- min ( fwetux * su, 0.8 * (wliqu + wsnou) / max (dtime  * (qu - q12), epsilon))
  
  # coefficient for transpiration from average upper canopy leaves:
  
  # PFT_UPDATE
  # to do: Jair buscar a solucao para identificar o upper e lower dependendo da especificacao da simulacao
  totCondSum <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == UPPER)
      totCondSum <- totCondSum + totcond[i] * frac[i]
  }
  sut <- (1 - fwetux) * 0.5 * totCondSum
  # sut <- (1 - fwetux) * 0.5 *
  #     (totcond[1] * frac[1] +
  #      totcond[2] * frac[2] +
  #      totcond[3] * frac[3] +
  #      totcond[4] * frac[4] +
  #      totcond[5] * frac[5] +
  #      totcond[6] * frac[6] +
  #      totcond[7] * frac[7] +
  #      totcond[8] * frac[8] )
  # sut <- (1 - fwetux) * 0.5 *
  #     (totcondub * frac[1] +
  #      totcondub * frac[2] +
  #      totcondub * frac[3] +
  #      totconduc * frac[4] +
  #      totcondub * frac[5] +
  #      totconduc * frac[6] +
  #      totcondub * frac[7] +
  #      totcondub * frac[8] )
  
  sut <- max (0, sut)
  
  # coefficient for sensible heat flux from upper canopy:
  
  suh <- suw * (rliqu * hvapf(tu,ta) + (1 - rliqu) * hsubf(tu,ta)) + sut * hvapf(tu,ta)
  
  # coefficient for evaporation from wet surfaces on the stems:
  
  ssw <- min (fwetsx * ss,  0.8 * (wliqs + wsnos) / max (dtime  * (qs - q12), epsilon))
  
  # coefficient for sensible heat flux from stems:
  
  ssh <- ssw * (rliqs * hvapf(ts,ta) + (1 - rliqs) * hsubf(ts,ta))
  
  # coefficient for evaporation from wet surfaces in the lower canopy:
  
  # for the regional U.S. version, this is multiplied by the green
  # fraction to reduce interception evaporation during the winter
  # and spring, which otherwise seems too high. There is some
  # physical basis for this, especially in snow covered regions
  # where snow cover can cause masking and / or compaction of grass,
  # thereby reducing interception. (This should be accounted for
  # by fi[i] in ginvap[see below], but areal snow cover fraction
  # is usually significantly underestimated in IBIS.)
  
  #        slw[i] <- min (fwetlx[i] * sl[i],
  slw <- min (fwetlx * sl * min(0.1, greenfracl), 0.8 * (wliql + wsnol) / max (dtime  * (ql - q34), epsilon))
  
  
  # PFT_UPDATE: Quando implementação estiver pronta, substituir essa linha por laço
  #             que irá percorrer cada tipo de planta, verificando se é lower e somando
  #             em slt0
  #
  #   for(1 to n_plant)
  #         if(plant.canopy == LOWER_CANOPY)
  #             slt0 <- slt0 + plant.frac + plant.totcond
  #       
  #   slt0 <- slt0 * (1 - fwetlx) * 0.5
  
  # coefficient for transpiration from average lower canopy leaves:
  totCondSum <- 0
  for(i in 1:npft) {
    if(!plantList[[i]]$active) next
    if(plantList[[i]]$canopy == LOWER)
      totCondSum <- totCondSum + totcond[i] * frac[i]
  }
  slt0 <- (1 - fwetlx) * 0.5 * totCondSum
  # slt0 <- (1 - fwetlx) * 0.5 *
  #     ( totcond[9] * frac[9]  +
  #       totcond[10] * frac[10] +
  #       totcond[11] * frac[11] +
  #       totcond[12] * frac[12] +
  #       totcond[17] * frac[13] +
  #       totcond[14] * frac[14] +
  #       totcond[17] * frac[15] +
  #       totcond[16] * frac[16] +
  #       totcond[17] * frac[17] +
  #       totcond[17] * frac[18] )
  # slt0 <- (1 - fwetlx) * 0.5 *
  #     ( totcondls * frac[9]  +
  #       totcondls * frac[10] +
  #       totcondl4 * frac[11] +
  #       totcondl3 * frac[12] +
  #       totcondc3 * frac[13] +
  #       totcondc4 * frac[14] +
  #       totcondc3 * frac[15] +
  #       totcondc4 * frac[16] +
  #       totcondc3 * frac[17] +
  #       totcondc3 * frac[18] )
  
  slt0 <- max (0, slt0)
  
  
  # averaged over stems and lower canopy leaves:
  # 
  slt <- slt0 * lai[1] / max (lai[1] + sai[1], epsilon)
  
  # coefficient for sensible heat flux from lower canopy:
  
  slh <- slw * (rliql * hvapf(tl,ta) + (1 - rliql) * hsubf(tl,ta)) + slt * hvapf(tl,ta)
  
  
  # set the matrix of coefficients and the right - hand sides
  # of the linearized equations
  arr[,] <- 0
  rhs[,] <- 0
  
  rwork <- 1 / dtime
  
  # upper leaf temperature tu
  rwork2 <- su * cp
  arr[1,1] <- chux * rwork + wu * rwork2 + wu * suh * dqu
  arr[1,4] <- -rwork2
  arr[1,6] <- -suh
  rhs[1] <- tuold * chux * rwork - (1 - wu) * rwork2 * tu - suh * (qu - wu * dqu * tu) + fradu - pfluxu
  
  # upper stem temperature ts
  rwork2 <- ss * cp
  arr[2,2] <- chsx * rwork + ws * rwork2 + ws * ssh * dqs
  arr[2,4] <- -rwork2
  arr[2,6] <- -ssh
  rhs[2] <- tsold * chsx * rwork - (1 - ws) * rwork2 * ts - ssh * (qs - ws * dqs * ts) + frads - pfluxs
  
  # lower veg temperature tl
  rwork2 <- sl * cp
  arr[3,3] <- chlx * rwork + wl * rwork2 + wl * slh * dql
  arr[3,5] <- -rwork2
  arr[3,7] <- -slh
  rhs[3] <- tlold * chlx * rwork - (1 - wl) * rwork2 * tl - slh * (ql - wl * dql * tl) + fradl - pfluxl
    
  # upper air temperature t12
  rwork <- xu * su
  rwork2 <- xs * ss
  arr[4,1] <-  - wu * rwork
  arr[4,2] <-  - ws * rwork2
  arr[4,4] <- cu + cl + rwork + rwork2
  arr[4,5] <-  - cl
  rhs[4] <- cu * ta * tfac + (1 - wu) * rwork * tu + (1 - ws) * rwork2 * ts
    
  # lower air temperature t34
  rwork <- xl * sl
  rwork2 <- fi * si
  arr[5,3] <-  - wl * rwork
  arr[5,4] <-  - cl
  arr[5,5] <- cl + rwork +(1 - fi) * sg + rwork2
  arr[5,8] <-  - wg * (1 - fi) * sg
  arr[5,9] <-  - wi * rwork2
  rhs[5] <- (1 - wl) * rwork * tl + (1 - wg) * (1 - fi) * sg * tg + (1 - wi) * rwork2 * ti
  
  # upper air specific humidity q12
  rwork <- xu * (suw + sut)
  rwork2 <- xs * ssw
  arr[6,1] <- -wu * rwork * dqu
  arr[6,2] <- -ws * rwork2 * dqs
  arr[6,6] <- cu + cl + rwork + rwork2
  arr[6,7] <-  - cl
  rhs[6] <- cu * qa + rwork * (qu - wu * dqu * tu) +rwork2 * (qs - ws * dqs * ts)
    
  # lower air specific humidity q34
  rwork <- xl * (slw + slt)
  rwork2 <- (1 - fi) * sg
  arr[7,3] <- -wl * rwork * dql
  arr[7,6] <- -cl
  arr[7,7] <- cl + rwork + rwork2 + fi * si
  arr[7,8] <- -wg * rwork2 * qgfac * dqg
  arr[7,9] <- -wi * fi * si * dqi
  rhs[7] <- rwork * (ql -wl * dql * tl) + rwork2 * qgfac * (qg - wg * dqg * tg) + fi * si * (qi -wi * dqi * ti)
  
  
  # soil skin temperature
  
  # (there is no wg in this eqn since it solves for a fully
  # implicit tg. wg can be thought of as the fractional soil
  # area using a fully implicit soln, and 1 - wg as that using a
  # fully explicit soln. the combined soil temperature is felt
  # by the lower air, so wg occurs in the t34,q34 eqns above.)
  
  rwork <- sg * cp
  rwork2 <- sg * hvasug
  arr[8,5] <- -rwork
  arr[8,7] <- -rwork2
  arr[8,8] <- rwork + rwork2 * qgfac * dqg + cog + zirg
  rhs[8] <- -rwork2 * qgfac * (qg - dqg * tg) +  cog * tsoi[1] + solg + firg + zirg * tgold
  
  # snow skin temperature
  
  # (there is no wi here, for the same reason as for wg above.)
  rwork <- si * cp
  rwork2 <- si * hvasui
  arr[9,5] <-  - rwork
  arr[9,7] <-  - rwork2
  arr[9,9] <- rwork + rwork2 * dqi + coi + ziri
  rhs[9] <-  - rwork2 * (qi - dqi * ti) + coi * tsno[1] +  soli + firi + ziri * tiold
  
  # solve the systems of equations
  res <- linsolve(arr, rhs, vec, mplate, nqn)
  vec <- res$vec
  rhs <- res$rhs

  # copy this iteration's solution to t * , q12, q34
  tu <- vec[1]
  ts <- vec[2]
  tl <- vec[3]
  t12 <- vec[4]
  t34 <- vec[5]
  tg <- vec[8]
  ti <- vec[9]
  q12 <- vec[6]
  q34 <- vec[7]
  
  
  # all done except for final flux calculations,
  # so loop back for the next iteration (except the last)
  
  if (iter < niter) {
    assign("xu", xu, envir = env)
    assign("xs", xs, envir = env)
    assign("xl", xl, envir = env)
    assign("chux", chux, envir = env)
    assign("chsx", chsx, envir = env)
    assign("chlx", chlx, envir = env)
    assign("chgx", chgx, envir = env)
    assign("wlgx", wlgx, envir = env)
    assign("wigx", wigx, envir = env)
    assign("cog", cog, envir = env)
    assign("coi", coi, envir = env)
    assign("zirg", zirg, envir = env)
    assign("ziri", ziri, envir = env)
    assign("wu", wu, envir = env)
    assign("ws", ws, envir = env)
    assign("wl", wl, envir = env)
    assign("wg", wg, envir = env)
    assign("wi", wi, envir = env)
    assign("tuold", tuold, envir = env)
    assign("tsold", tsold, envir = env)
    assign("tlold", tlold, envir = env)
    assign("tgold", tgold, envir = env)
    assign("tiold", tiold, envir = env)
    
    assign("tu", tu, envir = env)
    assign("ts", ts, envir = env)
    assign("tl", tl, envir = env)
    assign("tg", tg, envir = env)
    assign("ti", ti, envir = env)
    assign("fwetux", fwetux, envir = env)
    assign("fwetsx", fwetsx, envir = env)
    assign("fwetlx", fwetlx, envir = env)
    assign("t12", t12, envir = env)
    assign("t34", t34, envir = env)
    assign("q12", q12, envir = env)
    assign("q34", q34, envir = env)
    assign("fsena", fsena, envir = env)
    assign("fseng", fseng, envir = env)
    assign("fseni", fseni, envir = env)
    assign("fsenu", fsenu, envir = env)
    assign("fsens", fsens, envir = env)
    assign("fsenl", fsenl, envir = env)
    assign("fvapa", fvapa, envir = env)
    assign("fvapuw", fvapuw, envir = env)
    assign("fvaput", fvaput, envir = env)
    assign("fvaps", fvaps, envir = env)
    assign("fvaplw", fvaplw, envir = env)
    assign("fvaplt", fvaplt, envir = env)
    assign("fvapg", fvapg, envir = env)
    assign("fvapi", fvapi, envir = env)
    assign("firg", firg, envir = env)
    assign("firi", firi, envir = env)
    assign("firb", firb, envir = env)
    assign("upsoiu", upsoiu, envir = env)
    assign("upsoil", upsoil, envir = env)
    assign("ginvap", ginvap, envir = env)
    assign("gsuvap", gsuvap, envir = env)
    assign("gtrans", gtrans, envir = env)
    assign("gtransu", gtransu, envir = env)
    assign("gtransl", gtransl, envir = env)
    
    return()
  }
  
  
  # evaluate sensible heat and moisture fluxes (per unit
  # leaf / stem / snow - free/snow - covered area as appropriate)
  
  # *******************************
  # diagnostic sensible heat fluxes
  # *******************************

  fsena <- cp * cu * (ta * tfac - t12)
  
  tgav <- wg * tg + (1 - wg) * tgpre
  fseng <- cp * sg * (tgav - t34)
  
  tiav <- wi * ti + (1 - wi) * tipre
  fseni <- cp * si * (tiav - t34)
  
  tuav <- wu * tu + (1 - wu) * tupre
  fsenu <- cp * su * (tuav - t12)
  
  tsav <- ws * ts + (1 - ws) * tspre
  fsens <- cp * ss * (tsav - t12)
  
  tlav <- wl * tl + (1 - wl) * tlpre
  fsenl <- cp * sl * (tlav - t12)
    

  
  
  # *************************
  # calculate moisture fluxes
  # *************************
  # total evapotranspiration from the entire column
  
  fvapa <- cu * (qa - q12)
  
  # evaporation from wet surfaces in the upper canopy
  # and transpiration per unit leaf area - upper canopy
  
  quav <- qu + wu * dqu * (tu - tupre)
  fvapuw <- suw * (quav - q12)
  fvaput <- max (0, sut * (quav - q12))
  
  # evaporation from wet surfaces on stems
  
  qsav <- qs + ws * dqs * (ts - tspre)
  fvaps <- ssw * (qsav - q12)
  
  # evaporation from wet surfaces in the lower canopy
  # and transpiration per unit leaf area - lower canopy
  
  qlav <- ql + wl * dql * (tl - tlpre)
  fvaplw <- slw * (qlav - q34)
  fvaplt <- max (0, slt0 * (qlav - q34))
  
  #if(time==3600*11)print(paste(iyear,jday,plai[17],slt0,qlav,q34,qa,-(qlav - q34),-fvaplt*hvap * (dtime/10 ** 6),sep=';'))
  
  
  # evaporation from the ground
  
  qgav <- qg + wg * dqg * (tg - tgpre)
  fvapg <- sg * (qgfac * qgav - q34)
  
  # evaporation from the snow
  
  qiav <- qi + wi * dqi * (ti - tipre)
  fvapi <- si * (qiav - q34)
    
  # 
  # adjust ir fluxes
  firg <- firg - wg * zirg * (tg - tgold)
  firi <- firi - wi * ziri * (ti - tiold)
  firb <- firb + (1 - fi) * wg * zirg * (tg - tgold) + fi * wi * ziri * (ti - tiold)

    # impose constraint on skin temperature
  ti <- min (ti, tmelt)
    
  
  # set upsoi[u,l], the actual soil water uptake rates from each
  # soil layer due to transpiration in the upper and lower stories,
  # for the soil model 
  
  for(k in 1: nsoilay) { 

    upsoiu[k] <- fvaput * 2 * lai[2] * fu * stressu[k] / max (stresstu, epsilon)
    upsoil[k] <- fvaplt * 2 * lai[1] * fl * (1 - fi) * stressl[k] / max (stresstl, epsilon)
    
  }
  
  # set net evaporation from intercepted water, net evaporation
  # from the surface, and net transpiration rates
  
  
  
  # evaporation from intercepted water
  
  ginvap <- fvapuw * 2 * lai[2] * fu + fvaps * 2 * sai[2] * fu + fvaplw * 2 * (lai[1] + sai[1]) * fl * (1 - fi)
  
  # evaporation from soil and snow surfaces
  gsuvap <- fvapg * (1 - fi) + fvapi * fi
  
  # transpiration
  gtrans <- fvaput * 2 * lai[2] * fu + fvaplt * 2 * lai[1] * fl * (1 - fi)
  gtransu <- fvaput * 2 * lai[2] * fu
  gtransl <- fvaplt * 2 * lai[1] * fl * (1 - fi)
    
  
  assign("xu", xu, envir = env)
  assign("xs", xs, envir = env)
  assign("xl", xl, envir = env)
  assign("chux", chux, envir = env)
  assign("chsx", chsx, envir = env)
  assign("chlx", chlx, envir = env)
  assign("chgx", chgx, envir = env)
  assign("wlgx", wlgx, envir = env)
  assign("wigx", wigx, envir = env)
  assign("cog", cog, envir = env)
  assign("coi", coi, envir = env)
  assign("zirg", zirg, envir = env)
  assign("ziri", ziri, envir = env)
  assign("wu", wu, envir = env)
  assign("ws", ws, envir = env)
  assign("wl", wl, envir = env)
  assign("wg", wg, envir = env)
  assign("wi", wi, envir = env)
  assign("tuold", tuold, envir = env)
  assign("tsold", tsold, envir = env)
  assign("tlold", tlold, envir = env)
  assign("tgold", tgold, envir = env)
  assign("tiold", tiold, envir = env)
  
  assign("tu", tu, envir = env)
  assign("ts", ts, envir = env)
  assign("tl", tl, envir = env)
  assign("tg", tg, envir = env)
  assign("ti", ti, envir = env)
  assign("fwetux", fwetux, envir = env)
  assign("fwetsx", fwetsx, envir = env)
  assign("fwetlx", fwetlx, envir = env)
  assign("t12", t12, envir = env)
  assign("t34", t34, envir = env)
  assign("q12", q12, envir = env)
  assign("q34", q34, envir = env)
  assign("fsena", fsena, envir = env)
  assign("fseng", fseng, envir = env)
  assign("fseni", fseni, envir = env)
  assign("fsenu", fsenu, envir = env)
  assign("fsens", fsens, envir = env)
  assign("fsenl", fsenl, envir = env)
  assign("fvapa", fvapa, envir = env)
  assign("fvapuw", fvapuw, envir = env)
  assign("fvaput", fvaput, envir = env)
  assign("fvaps", fvaps, envir = env)
  assign("fvaplw", fvaplw, envir = env)
  assign("fvaplt", fvaplt, envir = env)
  assign("fvapg", fvapg, envir = env)
  assign("fvapi", fvapi, envir = env)
  assign("firg", firg, envir = env)
  assign("firi", firi, envir = env)
  assign("firb", firb, envir = env)
  assign("upsoiu", upsoiu, envir = env)
  assign("upsoil", upsoil, envir = env)
  assign("ginvap", ginvap, envir = env)
  assign("gsuvap", gsuvap, envir = env)
  assign("gtrans", gtrans, envir = env)
  assign("gtransu", gtransu, envir = env)
  assign("gtransl", gtransl, envir = env)
  
  return()
}
