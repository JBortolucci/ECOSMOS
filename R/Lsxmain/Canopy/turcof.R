# Global Vars:
# alog1   # log (z1 - dispu) 
# alog2   # log (z2 - displ)
# alog3   # log (z3 - displ)
# alog4   # log (max(z4, 1.1*z0sno, 1.1*z0soi)) 
# aloga   # log (za - dispu) 
# alogav  # average of alogi and alogg 
# alogl   # log (roughness length of lower canopy)
# alogu   # log (roughness length of upper canopy)
# bdl     # aerodynamic coefficient ([(tau/rho)/u**2] for laower canopy (A31/A30 Pollard & Thompson 1995)
# bdu     # aerodynamic coefficient ([(tau/rho)/u**2] for upper canopy (A31/A30 Pollard & Thompson 1995)
# cgrass  # empirical constant in lower canopy-air aerodynamic transfer coefficient (m s-0.5) (A39a Pollard & Thompson 95)
# cl      # air transfer coefficient (*rhoa) (m s-1 kg m-3) between the 2 canopies (z34 --> z12) (A36 Pollard & Thompson 1995)
# cleaf   # empirical constant in upper canopy leaf-air aerodynamic transfer coefficient (m s-0.5) (A39a Pollard & Thompson 95)
# cstem   # empirical constant in upper canopy stem-air aerodynamic transfer coefficient (m s-0.5) (A39a Pollard & Thompson 95)
# cu      # air transfer coefficient (*rhoa) (m s-1 kg m-3) for upper air region (z12 --> za) (A35 Pollard & Thompson 1995)
# dil     # inverse of momentum diffusion coefficient within lower canopy (m)
# diu     # inverse of momentum diffusion coefficient within upper canopy (m)
# dleaf   # typical linear leaf dimension in aerodynamic transfer coefficient (m)
# dstem   # typical linear stem dimension in aerodynamic transfer coefficient (m)
# dtime   # model timestep (seconds)
# exphl   # exp(lamda/2*(z3-z4)) for lower canopy (A30 Pollard & Thompson)
# exphu   # exp(lamda/2*(z3-z4)) for upper canopy (A30 Pollard & Thompson)
# expl    # exphl**2
# expu    # exphu**2
# fl      # fraction of snow-free area covered by lower  canopy
# lai     # canopy single-sided leaf area index (area leaf/area veg)
# q12     # specific humidity of air at z12
# q34     # specific humidity of air at z34
# qa      # specific humidity (kg_h2o/kg_air)
# rhoa    # air density at za (allowing for h2o vapor) (kg m-3)
# richl   # richardson number for air above upper canopy (z3 to z2)
# richu   # richardson number for air between upper & lower canopy (z1 to za)
# sg      # air-soil transfer coefficient
# si      # air-snow transfer coefficient
# sl      # air-vegetation transfer coefficients (*rhoa) for lower canopy leaves & stems (m s-1*kg m-3) (A39a Pollard & Thompson 1995)
# ss      # air-vegetation transfer coefficients (*rhoa) for upper canopy stems (m s-1 * kg m-3) (A39a Pollard & Thompson 1995)
# strahl  # heat/vap correction factor for stratif between upper & lower canopy (z3 to z2) (louis et al.)
# strahu  # heat/vap correction factor for stratif above upper canopy (z1 to za) (louis et al.)
# straml  # momentum correction factor for stratif between upper & lower canopy (z3 to z2) (louis et al.)
# stramu  # momentum correction factor for stratif above upper canopy (z1 to za) (louis et al.)
# su      # air-vegetation transfer coefficients (*rhoa) for upper canopy leaves (m s-1 * kg m-3) (A39a Pollard & Thompson 1995)
# t12     # air temperature at z12 (K)
# t34     # air temperature at z34 (K)
# ta      # air temperature (K)
# tfac    # (ps/p) ** (rair/cair) for atmospheric level  (const)
# u1      # wind speed at level z1 (m s-1)
# u12     # wind speed at level z12 (m s-1)
# u2      # wind speed at level z2 (m s-1)
# u3      # wind speed at level z3 (m s-1)
# u34     # wind speed at level z34 (m s-1)
# u4      # wind speed at level z4 (m s-1)
# ua      # wind speed (m s-1)
# use
# ustar   # friction velocity (m s-1) 
# vonk    # von karman constant (dimensionless)
# z1      # effective top of upper canopy (for momentum) (m)
# z12     # effective middle of the upper canopy (for momentum) (m)
# z2      # effective bottom of the upper canopy (for momentum) (m)
# z3      # effective top of the lower canopy (for momentum) (m)
# z34     # effective middle of the lower canopy (for momentum) (m)
# z4      # effective bottom of the lower canopy (for momentum) (m)
# za      # height above the surface of atmospheric forcing (m)

turcofR <- function (envi, iter, time, jday) {
  # solves for wind speeds at various levels
  #
  # also computes upper and lower - region air - air transfer coefficients
  # and saves them in com1d arrays cu and cl for use by turvap,
  # and similarly for the solid - air transfer coefficients
  # su, ss, sl, sg and si
  
  
  
  # Arguments (input)
  # 
  # integer iter,jday          #current iteration number
  # 
  # 
  # # Local variables
  # 
  # integer i             # loop indice
  # 
  # real xfac,  time,     
  # x,               
  # rwork,           # working variable
  # cdmax,           # max value for cd
  # tauu,            
  # a,b,c,d,         
  # taul,            
  # ca,              # to compute inverse air - air transfer coeffs
  # cai, cbi, cci,   
  # cdi, cei, cfi,   
  # sg0,             # to compute air - solid transfer coeff for soil
  # si0              # to compute air - solid transfer coeff for ice
  
  environment(fstrat) <- env
  
  # real yu[1], yl[1]
  yu <- 0
  yl <- 0
  
  # set stratification factors for lower and upper regions
  # using values from the previous iteration
  
  xfac <- 1
  
  res <- fstrat (t34, t12, xfac, q34, q12, z3, z2, 
               alogl, alogl, alog2, u2, richl, straml, strahl, iter)
  
  richl  <- res$rich
  straml <- res$stram
  strahl <- res$strah
  
  res <- fstrat (t12, ta,  tfac, q12, qa,  z1, za, 
               alogu, alogu, aloga, ua, richu, stramu, strahu, iter)
  
  richu  <- res$rich
  stramu <- res$stram
  strahu <- res$strah
  
  # eliminate c / d from eq (28), tau_l / rho from (26),(27), to get
  # lower - story roughness alogl. yl / bdl is (tau_l / rho) / (c + d)
  
  # equation numbers correspond to lsx description section 4e
  

    x <- ((alog4 - alogav) / vonk) ** 2  * bdl
    
   
    
    rwork <- 1 / expl
    yl <- ((x + 1) * expl + (x - 1) * rwork) /  
      ((x + 1) * expl - (x - 1) * rwork)
    
    alogl <- alog3 - vonk * sqrt(yl / bdl)
    
 
  
  # eliminate tau_l / rho from (24),(25), tau_u / rho and a / b from
  # (22),(23), to get upper - story roughness alogu
  # 
  # yu / bdu is (tau_u / rho) / (a + b)
  
    #          
    x <- ((alog2 - alogl) / vonk) ** 2  * bdu / straml
    
 
    
    rwork <- 1 / expu
    yu <- ((x + 1) * expu + (x - 1) * rwork) /  
      ((x + 1) * expu - (x - 1) * rwork)
    
    
    alogu <- alog1 - vonk * sqrt(yu / bdu)
    
 
  
  # define the maximum value of cd
  
  cdmax <- 300 / (2 * dtime)
  
  # get tauu[ <- tau_u / rho] from (21), a and b from (22),(23),
  # taul[ <- tau_u / rho] from (25), c and d from (26),(27)
  
  # changed the following to eliminate small errors associated with
  # moving this code to single precision - affected c and d,
  # which made u_ become undefined, as well as affecting some
  # other variables
  

    tauu <- (ua * vonk / (aloga - alogu)) ** 2  * stramu
      
    ustar <- tauu ** 0.5        
    
    
    #sant	if(i == 1  && iter == 3)write(222, * )jday,time/3600,ustar
    
    a <- 0.5 * tauu * (yu + 1) / bdu
    b <- 0.5 * tauu * (yu - 1) / bdu
    
    taul <- bdu * (a / expu - b*expu)
    
    c <- 0.5 * taul * (yl + 1) / bdl
    d <- 0.5 * taul * (yl - 1) / bdl
    
    # evaluate wind speeds at various levels, keeping a minimum 
    # wind speed of 0.01 m / s at all levels
    #   
    u1 <- max (0.01, sqrt (max (0, (a + b))))
    #sant	if(i == 1)  print(paste0(u1))
    u12 <- max (0.01, sqrt (max (0, (a / exphu + b*exphu))))
    u2 <- max (0.01, sqrt (max (0, (a / expu + b*expu))))
    u3 <- max (0.01, sqrt (max (0, (c + d))))
    u34 <- max (0.01, sqrt (max (0, (c / exphl + d*exphl))))
    u4 <- max (0.01, sqrt (max (0, (c / expl + d*expl))))
    
  
 
  # compute inverse air - air transfer coeffs
  
  # use of inverse individual coeffs cai, cbi, cci, cdi, cei, cfi avoids
  # divide-by - zero as vegetation vanishes - combine into
  # upper - region coeff cu from za to z12, and lower - region coeff
  # cl from z34 to z12, and also coeffs
  

    ca <- ua * strahu * vonk ** 2  / ((aloga - alogu) * (aloga - alog1))
    
    ca <- min (cdmax, ca / (1 + ca * 1e-20))
    
    cai <- 1 / (rhoa * ca)
    
    cbi <- diu * (z1 - z12) / (rhoa * 0.5 * (u1 + u12))
    cci <- diu * (z12 - z2) / (rhoa * 0.5 * (u12 + u2))
    
    cdi <- (alog2 - alogl) * (alog2 - alog3) / (rhoa * u2 * strahl * vonk ** 2)
    
    cei <- dil * (z3 - z34) / (rhoa * 0.5 * (u3 + u34))
    cfi <- dil * (z34 - z4) / (rhoa * 0.5 * (u34 + u4))
    
    cu <- 1 / (cai + cbi)
    cl <- 1 / (cci + cdi + cei)
    
    # compute air - solid transfer coeffs for upper leaves, upper
    # stems, lower story (su,ss,sl)
    
    su <- rhoa * cleaf * sqrt (u12 / dleaf[2])
    ss <- rhoa * cstem * sqrt (u12 / dstem[2])
    sl <- rhoa * cgrass * sqrt (u34 / dleaf[1])
    

    # compute air - solid transfer coeffs for soil and snow[sg,si]
    
    # old technique
    
    #       sg0 <- rhoa * u4 * (vonk / (alog4 - alogg)) ** 2
    #       si0 <- rhoa * u4 * (vonk / (alog4 - alogi)) ** 2
    
    # replace above formulations which depend on the log - wind profile
    # (which may not work well below a canopy), with empirical formulation
    # of Norman's. In the original LSX, turcof.f solves for the winds at
    # the various levels from the momentum equations. This gives the transfer
    # coefficients for heat and moisture. Heat and moisture eqns are then solved 
    # in subroutine turvap. Using the empirical formulation of John Norman is 
    # not consistent with the earlier solution for u4 (based on a logarithmic 
    # profile just above the ground. However, this is used here because it 
    # improved a lot simulations of the sensible heat flux over the 
    # HAPEX - MOBILHY and FIFE sites
    
    
    sg0 <- rhoa * (0.004 + 0.012 * u4)
    si0 <- rhoa * (0.003 + 0.010 * u4)
    
    # modify the cofficient to deal with cfi[see above]
    sg <- 1 / (cfi + 1 / sg0)
    si <- 1 / (cfi + 1 / si0)
    
  
  assign("richl", richl, envir = env)
  assign("straml", straml, envir = env)
  assign("strahl", strahl, envir = env)
  assign("richu", richu, envir = env)
  assign("stramu", stramu, envir = env)
  assign("strahu", strahu, envir = env)
  assign("alogl", alogl, envir = env)
  assign("alogu", alogu, envir = env)
  assign("ustar", ustar, envir = env)
  assign("u1", u1, envir = env)
  assign("u12", u12, envir = env)
  assign("u2", u2, envir = env)
  assign("u3", u3, envir = env)
  assign("u34", u34, envir = env)
  assign("u4", u4, envir = env)
  assign("cu", cu, envir = env)
  assign("cl", cl, envir = env)
  assign("su", su, envir = env)
  assign("ss", ss, envir = env)
  assign("sl", sl, envir = env)
  assign("use", use, envir = env)
  assign("sg", sg, envir = env)
  assign("si", si, envir = env)
  
  return()
}