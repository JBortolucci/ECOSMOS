# steps soil/seaice model through one timestep

soilctlR <- function() {
  
  environment(soilh2o)  <- env
  environment(soilheat) <- env
  environment(wadjust)  <- env
  environment(tridia)   <- env
  
  c0pud <- matrix(0, 1, nsoilay)
  c1pud <- matrix(0, 1, nsoilay)
  owsoi <- numeric(0)
  otsoi <- numeric(0)
  
  c0pud [,] <- 0
  c1pud [,] <- 0
  
  # for soil, set soil infiltration rate fwtop (for 
  # soilh2o) and upper heat flux fhtop (for soilheat)
  # 
  # also step puddle model wpud, wipud
  # 
  # procedure is:
  # 
  # (1) apportion raing btwn puddle liquid(wpud) or runoff(grunof)
  # 
  # (2) apportion evap/condens (fvapg) btwn infil rate(fwtop), soil
  # ice (wisoi(i,1)), puddle liq (wpud), or puddle ice (wipud)
  # 
  # (3) transfer some puddle liquid to fwtop
  # 
  # (4) compute upper heat flx fhtop: includes fwtop*ch2o*tsoi(i,1)
  # to be consistent with whflo in soilheat, and accounts for
  # changing rain temp from traing to tsoi(i,1) and runoff temp
  # from tsoi to max(tsoi(i,1),tmelt)
  # 
  # (5) transfer any excess puddle liq to runoff
  
  # (1) apportion sfc-level rain between puddle liquid and runoff
  # 
  # linear dependence of runoff fraction on wpud+wipud assumes
  # uniform statistical distribution of sub-grid puddle
  # capacities between 0 and wpudmax. runoff fraction is
  # reduced linearly for tsoi < tmelt (by zfrez) in which case
  # any rain will increase wpud which will be frozen to wipud
  # below
  
  zfrez <- max (0., min (1., (tsoi[1] - tmelt + .5) * 2.))
  
  # always have some minimal amount of runoff (3%) even if
  # puddles are dry or soil is cold, but don't allow more than
  # a specified maximum (30%), since the rain must also be allowed
  # to infiltrate (and more surface runoff is generated later in
  # step (5) anyway). zmin was "tuned" based on drainage-to-total
  # runoff ratios for the Trout Lake region (for wpudmax=200mm).
  # zmax is based on the assumption that 70% of precip goes to ET
  # (on average over the Upper Midwest); under saturated conditions,
  # the remainder (30%) is assumed to go directly to surface runoff.
  # Both zmin and zmax can be considered tunable, but it would
  # probably be better to just adjust wpudmax.
  
  zmin <- 0.03
  # CJK 1-16-03      zmax = 1.00
  zmax <- 0.30   # zmax change according to TET on 1-16-03 
  zrunf <- zmin + (zmax - zmin) * zfrez * max (0.0, min (1., (wpud + wipud) / wpudmax))
  
  wpud <- wpud + (1. - zrunf) * raing * dtime
  
  grunof <- zrunf * raing
  
  # (2) apportion evaporation or condensation between 4 h2o stores:
  
  rwork <- fvapg * dtime
  
  if (fvapg >= 0.) {
    
    # evaporation: split according to qglif
    
    fwtop   <-       - qglif[1] * fvapg
    wpud    <- wpud  - qglif[3] * rwork
    wipud   <- wipud - qglif[4] * rwork
    
    wipre <- wisoi[1]
    wisoi[1] <- max (0, wipre - qglif[2]*rwork / (rhow * poros[1] * hsoi[1]))
    
    if (1. - wisoi[1] > epsilon) { 
      wsoi[1] <- wsoi[1]*(1.-wipre)/(1.-wisoi[1])
    }
    
  } else {
    
    # condensation: give all to puddles (to avoid wsoi, wisoi > 1)
    
    fwtop <- 0.
    wpud <- wpud  - (qglif[1] + qglif[3]) * rwork
    wipud<- wipud - (qglif[2] + qglif[4]) * rwork
    
  }
  
  # (3) transfer some puddle liquid to infiltration; can lead
  # to small amounts of negative wpud (in soilh2o) due to
  # round-off error
  # ------------------------------------------------------------------
  # the following is commented out by Kaiyuan Li for Green-ampt
  # this is the origianl code for infiltration part
  
  zdpud <- rhow * dtime * max (0., 1.-wisoi[1])^2 * hydraul[1]
  
  # ------------------------------------------------------------------
  # 
  # ------------------------------------------------------------------
  # Added by Kaiyuan Li for incorporation of Green-ampt infiltration 
  # calculate potential iinfiltration (actual infiltration is fwpud)
  # The Green-Ampt equation adopted bolow is from Julien et al. 1995
  # Water resources buttetin vol. 31, No. 3: 523 - 536, 1995
  # 
  # call delta_sw(fwpudtot(i), dtsw, layerno, i)   ! calculate delta soil water content
  # 
  # ---- calculate potential infiltration zdpud
  # zdpud = 0.5 * ((hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime - 2.0 * fwpudtot(i))
  # >   +  sqrt((hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime - 2.0 * fwpudtot(i)) ** 2
  # >   +  8.0 * hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime *
  # >      (cpwf(i, layerno) * 1000 * dtsw + fwpudtot(i))))           
  # -----------------
  # end Green Ampt
  #        
  # ---- calculate potential infiltration zdpud (Christine Molling's version, wrong!)        
  # dtsw = poros(i, 1)*(1.0 - wisoi(i, 1))*(1.0 - wsoi(i, 1))
  # zdpud = 0.5 * ((hydraul(i, 1) * 1000.0 * dtime - 2.0 * fwpudtot(i))
  # >   +  sqrt((hydraul(i, 1) * 1000.0 * dtime - 2.0 * fwpudtot(i)) ** 2
  # >   +  8.0 * hydraul(i, 1) * 1000.0 * dtime * (cpwf(i, 1) * 1000 * dtsw + fwpudtot(i))))
  # ------------------------------------------------------------------
  
  fwpud <- max (0., min(wpud, zdpud)) / dtime
  c0pud[1] <- ch2o * wpud + cice * wipud
  
  # (4) compute upper soil heat flux
  
  fhtop <- heatg + raing*ch2o*(traing-tsoi[1]) - grunof*ch2o*max(tmelt-tsoi[1], 0.)
  
  soihfl <- fhtop
  
  # update diagnostic variables
  
  gadjust <- 0.0
  
  # reduce soil moisture due to transpiration (upsoi[u,l], from
  # turvap).need to do that before other time stepping below since 
  # specific heat of this transport is neglected
  # 
  # first set porosflo, reduced porosity due to ice content, used
  # as the effective porosity for uptake here and liquid hydraulics
  # later in soilh2o. to avoid divide-by-zeros, use small epsilon
  # limit; this will always cancel with epsilon or 0 in numerators
  # 
  # also increment soil temperature to balance transpired water
  # differential between temps of soil and leaf. physically
  # should apply this to the tree, but would be awkward in turvap.
  #  
  # also, save old soil moisture owsoi and temperatures otsoi so
  # implicit soilh2o and soilheat can aposteriori deduce fluxes.
  
  for (k in 1:nsoilay) {
    
    porosflo[k] <- poros[k] * max (epsilon, (1.-wisoi[k]))
    
    # next line just for ice whose poros(i,k) is 0.0
    
    porosflo[k] <- max (porosflo[k], epsilon)
    
    wsoi[k] <- wsoi[k] - dtime * (upsoiu[k] + upsoil[k]) / (rhow * porosflo[k] * hsoi[k])
    
    cx <- c0pud[k] + ((1.-poros[k])*csoi[k]*rhosoi[k] + poros[k]*(1.-wisoi[k])*wsoi[k]*ch2o*rhow + poros[k]*wisoi[k]*cice*rhow) * hsoi[k]
    
    tsoi[k] <- tsoi[k] - dtime * ch2o * (upsoiu[k]*(tu-tsoi[k]) + upsoil[k]*(tl-tsoi[k]) ) / cx
    
    owsoi[k]  <- wsoi[k]
    otsoi[k]  <- tsoi[k]
  }
  
  # step soil moisture calculations

  assign("fwpud", fwpud, envir = env)
  assign("fwtop", fwtop, envir = env)
  assign("porosflo", porosflo, envir = env)
  assign("wisoi", wisoi, envir = env)
  assign("wpud", wpud, envir = env)
  assign("wsoi", wsoi, envir = env)
  
  # assign("bex", bex, envir = env)
  # assign("bperm", bperm, envir = env)
  # assign("hsoi", hsoi, envir = env)
  # assign("hydraul", hydraul, envir = env)
  # assign("ibex", ibex, envir = env)
  # assign("poros", poros, envir = env)
  # assign("rhow", rhow, envir = env)
  # assign("suction", suction, envir = env)
  # assign("wflo", wflo, envir = env)

  soilh2o(owsoi, fsqueez)

  # update drainage and puddle
  gdrain  <- wflo[nsoilay+1]
  c1pud[1] <- ch2o*wpud + cice*wipud
  
  # --------------------------------------------------------------------------------
  # added for Green-Ampt infiltration model
  # if (raing(i) .lt. 0.000001/3600.0)  then
  # fwpudtot(i) = 0
  # else
  # fwpudtot(i) = fwpudtot(i) + (fwpud(i) - fsqueez(i)) * dtime
  # end if
  # ---------------------------------------------------------------------------------

  # step temperatures due to conductive heat transport
  assign("wpud", wpud, envir = env)
  assign("wisoi", wisoi, envir = env)
  assign("wsoi", wsoi, envir = env)
  assign("hflo", hflo, envir = env)
  assign("tsoi", tsoi, envir = env)
  
  # assign("ch2o", ch2o, envir = env)
  # assign("cice", cice, envir = env)
  # assign("consoi", consoi, envir = env)
  # assign("csoi", csoi, envir = env)
  # assign("hsoi", hsoi, envir = env)
  # assign("poros", poros, envir = env)
  # assign("rhosoi", rhosoi, envir = env)
  # assign("rhow", rhow, envir = env)
  # assign("wflo", wflo, envir = env)

  soilheat(otsoi, owsoi, c0pud, fhtop, c1pud)
  
  # set wsoi, wisoi to exactly 0 or 1 if differ by negligible 
  # amount (needed to avoid epsilon errors in loop 400 below)

  # gadjust
  # hsoi
  # poros
  # rhow
  # wisoi
  # wsoi
  
  assign("gadjust", gadjust, envir = env)
  assign("wisoi", wisoi, envir = env)
  assign("wsoi", wsoi, envir = env)
  
  # assign("hsoi", hsoi, envir = env)
  # assign("poros", poros, envir = env)
  # assign("rhow", rhow, envir = env)
  
  wadjust()

  
  # heat-conserving adjustment for liquid/ice below/above melt
  # point. uses exactly the same logic as for intercepted veg h2o
  # in steph2o2. we employ the fiction here that soil liquid and
  # soil ice both have density rhow, to avoid "pot-hole"
  # difficulties of expansion on freezing. this is done by 
  # dividing all eqns through by rhow(*hsoi).
  # 
  # the factor (1-wsoi(old))/(1-wisoi(new)) in the wsoi increments
  # results simply from conservation of h2o mass; recall wsoi is
  # liquid content relative to ice-reduced pore space.
  
  for (k in 1:nsoilay) {
    
    # next line is just to avoid divide-by-zero for ice with
    # poros = 0
    
    zporos <- max (poros[k], epsilon)
    rwork <- c1pud[k]/rhow/hsoi[k] + (1.-zporos)*csoi[k]*rhosoi[k]/rhow
    
    chav <- rwork + zporos*(1.-wisoi[k])*wsoi[k]*ch2o + zporos*wisoi[k]*cice
    
    # if liquid exists below melt point, freeze some to ice
    # 
    # (note that if tsoi>tmelt or wsoi=0, nothing changes.)
    # (also note if resulting wisoi=1, either dw=0 and prev
    # wisoi=1, or prev wsoi=1, so use of epsilon is ok.)
    
    zwsoi <- min (1., wsoi[k])
    
    dh <- chav * (tmelt-tsoi[k])
    dw <- min (zporos*(1.-wisoi[k])*zwsoi,  max (0.,dh/hfus) )
    
    wisoi[k] <- wisoi[k] +  dw/zporos
    wsoi[k]  <- wsoi[k]  - (dw/zporos)*(1.-zwsoi) / max (epsilon,1.-wisoi[k])
    
    chav <- rwork + zporos*(1.-wisoi[k])*wsoi[k]*ch2o + zporos*wisoi[k]*cice
    
    tsoi[k] <- tmelt - (dh-hfus*dw) / chav
    
    # if ice exists above melt point, melt some to liquid
    # 
    # note that if tsoi<tmelt or wisoi=0, nothing changes
    # 
    # also note if resulting wisoi=1, dw=0 and prev wisoi=1,
    # so use of epsilon is ok
    
    dh <- chav * (tsoi[k] - tmelt)
    dw <- min ( zporos*wisoi[k], max (0., dh/hfus) )
    
    wisoi[k] <- wisoi[k] -  dw/zporos
    wsoi[k]  <- wsoi[k]  + (dw/zporos) * (1.-wsoi[k]) / max(epsilon,1.-wisoi[k])
    
    chav <- rwork + zporos*(1.-wisoi[k])*wsoi[k]*ch2o + zporos*wisoi[k]*cice
    
    tsoi[k] <- tmelt + (dh-hfus*dw) / chav
    
    # reset porosflo (although not used after this)
    
    porosflo[k] <- zporos * max (epsilon, 1.-wisoi[k])
    
  }
  # set wsoi, wisoi to exactly 0 or 1 if differ by negligible 
  # amount (roundoff error in loop 400 above can produce very
  # small negative amounts)
  
  assign("gadjust", gadjust, envir = env)
  assign("wisoi", wisoi, envir = env)
  assign("wsoi", wsoi, envir = env)
  
  # assign("hsoi", hsoi, envir = env)
  # assign("poros", poros, envir = env)
  # assign("rhow", rhow, envir = env)
  
  wadjust()

  
  # repeat ice/liquid adjustment for upper-layer puddles (don't 
  # divide through by rhow*hsoi). upper-layer moistures wsoi,wisoi
  # are already consistent with tsoi(i,1) > or < tmelt, and will 
  # remain consistent here since tsoi(i,1) will not cross tmelt
  
  k <- 1
  
  # if any puddle liquid below tmelt, freeze some to puddle ice
  
  rwork <- ((1.-poros[k])*csoi[k]*rhosoi[k]+ poros[k]*(1.-wisoi[k])*wsoi[k]*ch2o*rhow+ poros[k]*wisoi[k]*cice*rhow) * hsoi[k]
  
  chav <- ch2o*wpud + cice*wipud + rwork
  
  dh <- chav * (tmelt-tsoi[k])
  dw <- min (wpud, max (0., dh/hfus))
  wipud <- wipud + dw
  wpud  <- wpud  - dw
  chav <- ch2o*wpud + cice*wipud + rwork
  tsoi[k] <- tmelt - (dh-hfus*dw) / chav
  
  # (5) transfer any excess puddle liq to runoff
  # 
  # the following runoff formulation could give rise to very
  # small amounts of negative runoff
  
  grun1 <- (min (wpud, max (0., wpud + wipud - wpudmax))) / dtime
  
  grunof <- grunof + grun1
  
  wpud <- wpud - grun1 * dtime
  
  # if any puddle ice above tmelt, melt it and send to puddle liquid
  # (not apportioned between puddle and surface runoff, to avoid
  # potential double shunting to runoff, i.e. duplicating step 1).
  
  dh <- chav * (tsoi[k]-tmelt)
  dw <- min (wipud, max (0., dh/hfus))
  wipud <- wipud - dw
  wpud  <- wpud + dw
  chav <- ch2o*wpud + cice*wipud + rwork
  tsoi[k] <- tmelt + (dh-hfus*dw) / chav
  
  wpud <- 0
  
  assign("fwpud", fwpud, envir = env)       # altera
  assign("fwtop", fwtop, envir = env)       # altera
  assign("gadjust", gadjust, envir = env)   # altera
  assign("gdrain", gdrain, envir = env)     # altera
  assign("grunof", grunof, envir = env)     # altera
  assign("soihfl", soihfl, envir = env)     # altera
  assign("wipud", wipud, envir = env)       # altera
  assign("wpud", wpud, envir = env)         # altera
  assign("porosflo", porosflo, envir = env) # altera
  assign("tsoi", tsoi, envir = env)         # altera
  assign("wflo", wflo, envir = env)         # altera
  assign("wisoi", wisoi, envir = env)       # altera
  assign("wsoi", wsoi, envir = env)         # altera
  assign("hflo", hflo, envir = env)         # altera
  
  # assign("ch2o", ch2o, envir = env)
  # assign("cice", cice, envir = env)
  # assign("hfus", hfus, envir = env)
  # assign("rhow", rhow, envir = env)
  # assign("tmelt", tmelt, envir = env)
  # assign("wpudmax", wpudmax, envir = env)
  # assign("bperm", bperm, envir = env)
  # assign("fvapg", fvapg, envir = env)
  # assign("heatg", heatg, envir = env)
  # assign("raing", raing, envir = env)
  # assign("tl", tl, envir = env)
  # assign("traing", traing, envir = env)
  # assign("tu", tu, envir = env)
  # assign("ibex", ibex, envir = env)
  # assign("csoi", csoi, envir = env)
  # assign("hsoi", hsoi, envir = env)
  # assign("hydraul", hydraul, envir = env)
  # assign("poros", poros, envir = env)
  # assign("qglif", qglif, envir = env)
  # assign("rhosoi", rhosoi, envir = env)
  # assign("upsoil", upsoil, envir = env)
  # assign("upsoiu", upsoiu, envir = env)
  # assign("bex", bex, envir = env)
  # assign("suction", suction, envir = env)
  # assign("consoi", consoi, envir = env)
  
}

# sets up call to tridia to solve implicit soil moisture eqn,
# using soil temperatures in wsoi (in comsoi)
# 
# lower bc can be no h2o flow or free drainage, set by bperm below

soilh2o <- function(owsoi, fsqueez) {
  
  # bex
  # bperm
  # fwpud
  # fwtop
  # hsoi
  # hydraul
  # ibex
  # poros
  # porosflo
  # rhow
  # suction
  # wflo
  # wisoi
  # wpud
  # wsoi
    
  environment(tridia) <- env
  
  hsoim <- numeric(0)
  
  weim <- numeric(0)
  weip <- numeric(0)
  
  wsoim <- numeric(0)
  wsoia <- numeric(0)
  wsoib <- numeric(0)
  
  e <- matrix(0, 1, nsoilay+1)
  f <- matrix(0, 1, nsoilay+1)
  g <- matrix(0, 1, nsoilay+1)
  
  d1 <- matrix(0, 1, nsoilay)
  d2 <- matrix(0, 1, nsoilay)
  d3 <- matrix(0, 1, nsoilay)
  rhs <- matrix(0, 1, nsoilay)
  w1 <- matrix(0, 1, nsoilay)
  w2 <- matrix(0, 1, nsoilay)
  
  
  dmin <- 1.e-9
  rimp <- 1.0
  
  # set lower boundary condition for the soil
  # (permeability of the base)
  # 
  #    bperm = 0.00  ! e.g. fully impermeable base
  #    bperm = 1.00  ! e.g. fully permeable base
  # 
  #    bperm = 0.10
  # 
  # set level vertical distances, interpolated moistures, and
  # interpolation weights
  # 
  # top layer
  
  k <- 1
  
  hsoim[k] <- 0.5 * hsoi[k]
  
  weim[k] <- 0.0
  weip[k] <- 1.0
  
  wsoim[k] <- wsoi[k]
  wsoia[k] <- min (wsoim[k], 1.0)
  wsoib[k] <- min (wsoim[k], 1.0)
  
  # middle layers
  
  for (k in 2:nsoilay) {
    
    hsoim[k] <- 0.5 * (hsoi[k-1] + hsoi[k])
    
    weim[k] <- 0.5 * hsoi[k] / hsoim[k]
    weip[k] <- 1.0 - weim[k]
    
    wsoim[k] <- weim[k] * wsoi[k-1] + weip[k] * wsoi[k]
    wsoia[k] <- min (wsoim[k], 1.0)
    wsoib[k] <- min (wsoim[k], 1.0)
    
  }
  # bottom layer
  
  k <- nsoilay + 1
  
  hsoim[k] <- 0.5 * hsoi[k-1]
  
  weim[k] <- 1.0
  weip[k] <- 0.0
  
  wsoim[k] <- wsoi[k-1]
  wsoia[k] <- min (wsoim[k], 1.0)
  wsoib[k] <- min (wsoim[k], 1.0)
  
  # set intermediate quantities e,f,g. these are terms in the
  # expressions for the fluxes at boundaries between layers,
  # so are zero for k=1. use bwn1 to account for minimum 
  # diffusivity dmin. bperm is used for k=nsoilay+1 to set the
  # type of the lower bc.
  # 
  # top layer
  # 
  k <- 1
  
  e[1,k] <- 0
  f[1,k] <- 0
  g[1,k] <- 0
  
  # middle layers
  
  for (k in 2:nsoilay) {
    
    # now that hydraul, suction and ibex can vary with depth,
    # use averages of surrounding mid-layer values
    # 
    # (see notes 8/27/93)
    
    a <- weim[k] * hydraul[k-1] + weip[k] * hydraul[k]
    
    b <- weim[k] * hydraul[k-1] * suction[k-1] * bex[k-1] + weip[k] * hydraul[k] * suction[k] * bex[k]
    
    zbex <- weim[k] * bex[k-1] + weip[k] * bex[k] 
    
    m <- 2 * round(zbex) + 3
    n <-     round(zbex) + 2
    
    bwn1 <- b * (wsoib[k]^(n-1))
    bwn  <- bwn1 * wsoib[k]
    
    if (bwn < dmin) bwn1 <- 0.0
    
    bwn <- max (bwn, dmin)
    
    e[k] <-  (-1.+rimp*m)*a*(wsoia[k]^m) + ((1.-rimp)*bwn - rimp*n*bwn1*wsoib[k]) * (wsoi[k]-wsoi[k-1]) / hsoim[k]
    
    f[k] <- - rimp*m*a*(wsoia[k]^(m-1)) + rimp*n*bwn1 * (wsoi[k]-wsoi[k-1]) / hsoim[k]
    
    g[k] <- rimp*bwn
    
  }
  
  # bottom layer
  
  k <- nsoilay + 1
  
  a <- hydraul[nsoilay] 
  b <- hydraul[nsoilay]*suction[nsoilay]*ibex[nsoilay]
  
  m <- 2*ibex[nsoilay] + 3
  n <- ibex[nsoilay]   + 2
  
  e[k] <- -a*(wsoia[k]^m)*bperm
  f[k] <- 0
  g[k] <- 0
  
  # deduce all e,f,g in proportion to the minimum of the two 
  # adjacent layers' (1-wisoi), to account for restriction of flow
  # by soil ice. this will cancel in loop 300  with the factor 
  # 1-wisoi in (one of) the layer's porosflo, even if wisoi=1 by 
  # the use of epsilon limit. so a layer with wisoi=1 will form a 
  # barrier to flow of liquid, but still have a predicted wsoi
  for (k in 1:nsoilay+1) {
    
    kka <- max (k-1,1)
    kkb <- min (k,nsoilay)
    
    # multiply by an additional factor of 1-wisoi for stability
    
    z <- max(0.,1.-max(wisoi[kka],wisoi[kkb]))**2
    
    e[k] <- z * e[k]
    f[k] <- z * f[k]
    g[k] <- z * g[k]
    
  }
  # set matrix diagonals and right-hand sides
  
  
  for (k in 1:nsoilay) {
    
    dt <- dtime / (porosflo[k]*hsoi[k])
    d1[k] <- dt*(f[k]*0.5*hsoi[k]/hsoim[k] - g[k]/hsoim[k])
    rhs[k] <- wsoi[k] + dt*(e[k+1] - e[k])
    
    if (k == 1) {
      
      dt <- dtime / (porosflo[k]*hsoi[k])
      rhs[k] <- rhs[k] + dt*(fwtop+fwpud)/rhow
    }
    if (k < nsoilay) {
      
      km1 <- max (k-1,1)
      
      dt <- dtime / (porosflo[k]*hsoi[k])
      d2[k] <- 1. + dt*( - f[k+1]*0.5*hsoi[k+1]/hsoim[k+1] + f[k]  *0.5*hsoi[km1]/hsoim[k] + g[k+1]/hsoim[k+1] + g[k]  /hsoim[k] )
      d3[k] <- dt*( - f[k+1]*0.5*hsoi[k]/hsoim[k+1] - g[k+1] /hsoim[k+1])
      
    } else {
      if (k == nsoilay) {
        
        dt <- dtime / (porosflo[k]*hsoi[k])
        d2[k] <- 1. + dt*( - f[k+1] + f[k]  *0.5*hsoi[k-1]/hsoim[k] + g[k]  /hsoim[k] )
        d3[k] <- 0.0
        
      }
    }
  }
  
  # solve the systems of equations
  output <- tridia(nsoilay, d1, d2, d3, rhs, wsoi, w1, w2)
  wsoi <- output
  
  fsqueez <- 0.0
  wflo[nsoilay+1] <- - rhow * e[nsoilay+1]
  
  for (k in nsoilay:1) {
    
    zz <- rhow * poros[k] * max(epsilon, (1.-wisoi[k])) * hsoi[k]
    
    wsoi[k] <- wsoi[k] + dtime * fsqueez / zz 
    fsqueez <- max (wsoi[k]-1.,0.) * zz / dtime
    wsoi[k] <- min (wsoi[k],1.)
    
    wflo[k] <- wflo[k+1] + (wsoi[k]-owsoi[k]) * zz / dtime
    
  }
  # step puddle liquid due to fsqueez and fwpud
  
  # also subtract net puddle-to-top-layer flux from wflo(i,1),
  # since puddle and top soil layer are lumped together in soilheat
  # so upper wflo should be external flux only (evap/condens)
  
  wpud   <- wpud   + (fsqueez - fwpud) * dtime
  wflo[1] <- wflo[1] + (fsqueez - fwpud)
  
  # assign("bex", bex, envir = env)
  # assign("bperm", bperm, envir = env)
  # assign("fwpud", fwpud, envir = env)
  # assign("fwtop", fwtop, envir = env)
  # assign("hsoi", hsoi, envir = env)
  # assign("hydraul", hydraul, envir = env)
  # assign("ibex", ibex, envir = env)
  # assign("poros", poros, envir = env)
  # assign("porosflo", porosflo, envir = env)
  # assign("rhow", rhow, envir = env)
  # assign("suction", suction, envir = env)
  assign("wflo", wflo, envir = env)
  # assign("wisoi", wisoi, envir = env)
  assign("wpud", wpud, envir = env)
  assign("wsoi", wsoi, envir = env)
  
  # return(fsqueez) nao é utilizado depois da função
  
}

# sets up call to tridia to solve implicit soil/ice heat 
# conduction, using layer temperatures in tsoi (in comsoi).
# the heat flux due to liquid flow previously calculated
# in soilh2o is accounted for. lower bc is conductive flux = 0
# for soil (although the flux due to liquid drainage flow can
# be > 0)

soilheat <- function(otsoi, owsoi, c0pud, fhtop, c1pud) {
  
  environment(tridia) <- env
  
  whflo <- matrix(0, 1, nsoilay)
  con <- matrix(0, 1, nsoilay)
  c0 <- matrix(0, 1, nsoilay)
  c1 <- matrix(0, 1, nsoilay)
  d1 <- matrix(0, 1, nsoilay)
  d2 <- matrix(0, 1, nsoilay)
  d3 <- matrix(0, 1, nsoilay)
  rhs <- matrix(0, 1, nsoilay)
  w1 <- matrix(0, 1, nsoilay)
  w2 <- matrix(0, 1, nsoilay)
  
  rimp <- 1.0
  
  # set conduction coefficient between layers, and heat fluxes
  # due to liquid transport
  # 
  # top layer
  
  k <- 1
  
  con[k] <- 0.0
  whflo[k] <- wflo[k] * ch2o * tsoi[k]
  
  # middle layers
  
  for (k in 2:nsoilay) {
    
    con[k] <-  1. / (0.5 * (hsoi[k-1] / consoi[k-1] + hsoi[k]  / consoi[k]))
    
    t <- (hsoi[k] * tsoi[k-1] + hsoi[k-1] * tsoi[k]) / (hsoi[k-1] + hsoi[k])
    
    whflo[k] <- wflo[k] * ch2o * t
    
  }
  
  # bottom layer
  
  k <- nsoilay + 1
  
  con[k] <- 0.0
  whflo[k] <- wflo[k] * ch2o * tsoi[k-1]
  
  # set diagonals of matrix and right-hand side. use old and
  # new heat capacities c0, c1 consistently with moisture fluxes
  # whflo computed above, to conserve heat associated with 
  # changing h2o amounts in each layer
  
  for (k in 1:nsoilay) {
    
    km1 <- max (k-1,1)
    kp1 <- min (k+1,nsoilay)
    
    rwork1 <- (1.-poros[k])*csoi[k]*rhosoi[k]
    rwork2 <- poros[k]*(1.-wisoi[k])*ch2o*rhow
    rwork3 <- poros[k]*wisoi[k]*cice*rhow
    
    c0[k] <- c0pud[k] + (rwork1 + rwork2 * owsoi[k] + rwork3) * hsoi[k]
    
    c1[k] <- c1pud[k] + (rwork1 + rwork2 * wsoi[k] + rwork3) * hsoi[k]
    
    rwork <- dtime/c1[k]
    
    d1[k] <-    - rwork * rimp * con[k]
    d2[k] <- 1. + rwork * rimp * (con[k]+con[k+1])
    d3[k] <-    - rwork * rimp * con[k+1]
    
    rhs[k] <- (c0[k]/c1[k])*tsoi[k] + rwork * ( (1.-rimp)*con[k]*(tsoi[km1]-tsoi[k])+ (1.-rimp)*con[k+1]*(tsoi[kp1]-tsoi[k]) + whflo[k] - whflo[k+1] )
    
    
    if (k == 1) {
      rhs[k] <- rhs[k] + (dtime/c1[k])*fhtop
    }
    
  }
  
  # solve systems of equations
  output <- tridia(nsoilay, d1, d2, d3, rhs, tsoi, w1, w2)
  tsoi   <- output

  
  # deduce downward heat fluxes between layers
  hflo[1] <- fhtop
  
  for (k in 1:nsoilay) {
    hflo[k+1] <- hflo[k] - (c1[k]*tsoi[k] - c0[k]*otsoi[k]) / dtime
  }
  
  # assign("ch2o", ch2o, envir = env)
  # assign("cice", cice, envir = env)
  # assign("consoi", consoi, envir = env)
  # assign("csoi", csoi, envir = env)
  assign("hflo", hflo, envir = env)
  # assign("hsoi", hsoi, envir = env)
  # assign("poros", poros, envir = env)
  # assign("rhosoi", rhosoi, envir = env)
  # assign("rhow", rhow, envir = env)
  # assign("tsoi", tsoi, envir = env)       # Não altera o valor
  # assign("wflo", wflo, envir = env)
  # assign("wisoi", wisoi, envir = env)
  # assign("wsoi", wsoi, envir = env)
  
}


# set wsoi, wisoi to exactly 0 if differ by negligible amount, 
# to protect epsilon logic in soilctl and soilh2o
# 
# ice-liquid transformations in soilctl loop 400 can produce very
# small -ve amounts due to roundoff error, and very small -ve or +ve
# amounts can cause (harmless) "underflow" fpes in soilh2o
wadjust <- function() {
  
  for (k in 1:nsoilay) {
    
    # initial total soil water
    
    ztot0 <- hsoi[k] * poros[k] * rhow * ((1. - wisoi[k]) * wsoi[k] + wisoi[k])
    
    # set bounds on wsoi and wisoi
    
    if (wsoi[k] < epsilon)  wsoi[k]  <- 0.0
    if (wisoi[k] < epsilon) wisoi[k] <- 0.0
    
    wsoi[k]  <- min (1., wsoi[k])
    wisoi[k] <- min (1., wisoi[k])
    
    if (wisoi[k] >= 1-epsilon) wsoi[k] <- 0.0
    
    # for diagnosis of total adjustment
    
    ztot1 <- hsoi[k] * poros[k] * rhow * ((1. - wisoi[k]) * wsoi[k] + wisoi[k])
    
    gadjust <- gadjust + (ztot1 - ztot0) / dtime
    
  }
  
  assign("gadjust", gadjust, envir = env)
  # assign("hsoi", hsoi, envir = env)   nao alterado
  # assign("poros", poros, envir = env) nao alterado
  # assign("rhow", rhow, envir = env)   nao alterado
  assign("wisoi", wisoi, envir = env)
  assign("wsoi", wsoi, envir = env)
  
}

tridia <- function(ne, a, b, c, y, x, alpha, gamma) {
  
  # purpose:
  #      to compute the solution of many tridiagonal linear systems.
  # 
  #     arguments:
  # 
  #     ns ..... the number of systems to be solved.
  # 
  #         nd ..... first dimension of arrays (ge ns).
  # 
  #     ne ..... the number of unknowns in each system.
  #              this must be > 2. second dimension of arrays.
  # 
  #     a ...... the subdiagonals of the matrices are stored
  #              in locations a(j,2) through a(j,ne).
  # 
  #     b ...... the main diagonals of the matrices are stored
  #              in locations b(j,1) through b(j,ne).
  # 
  #     c ...... the super-diagonals of the matrices are stored in
  #              locations c(j,1) through c(j,ne-1).
  # 
  #     y ...... the right hand side of the equations is stored in
  #              y(j,1) through y(j,ne).
  # 
  #     x ...... the solutions of the systems are returned in
  #              locations x(j,1) through x(j,ne).
  # 
  #     alpha .. work array dimensioned alpha(nd,ne)
  # 
  #     gamma .. work array dimensioned gamma(nd,ne)
  #  
  #     history:  based on a streamlined version of the old ncar
  #                ulib subr trdi used in the phoenix climate
  #                model of schneider and thompson (j.g.r., 1981).
  #                revised by starley thompson to solve multiple
  #                systems and vectorize well on the cray-1.
  #                later revised to include a parameter statement
  #                to define loop limits and thus enable cray short
  #                vector loops.
  # 
  #      algorithm:  lu decomposition followed by solution.
  #                  note: this subr executes satisfactorily
  #                  if the input matrix is diagonally dominant
  #                  and non-singular.  the diagonal elements are
  #                  used to pivot, and no tests are made to determine
  #                  singularity. if a singular or numerically singular
  #                  matrix is used as input a divide by zero or
  #                  floating point overflow will result.
  # 
  #        last revision date:      4 february 1988
  
  nm1 <- ne-1
  
  # obtain the lu decompositions
  
  alpha[1] <- 1./b[1]
  gamma[1] <- c[1]*alpha[1]
  
  for (i in 2:nm1) { #nsoilay+1
    alpha[i] <- 1./(b[i]-a[i]*gamma[i-1])
    gamma[i] <- c[i]*alpha[i]
  }
  
  # solve
  x[1] <- y[1]*alpha[1]
  
  for (i in 2:nm1) { #nsoilay+1
    x[i] <- (y[i]-a[i]*x[i-1])*alpha[i]
  }
  
  x[ne] <- (y[ne]-a[ne]*x[nm1])/ (b[ne]-a[ne]*gamma[nm1])
  
  for (i in 1:nm1) { #nsoilay+1
    ib <- ne-i
    x[ib] <- x[ib]-gamma[ib]*x[ib+1]
  }
  
  return(x)
  
}
