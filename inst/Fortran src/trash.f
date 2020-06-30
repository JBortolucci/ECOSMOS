c ---------------------------------------------------------------------
      subroutine diurnalmet (time, jday, plens, startp, endp, seed,
     >                       irrigate, ilens, starti, endi)
c ---------------------------------------------------------------------
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comhour.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer jday,    ! current day
     >        seed,    !
     >        irrigate
c
      real time,
     >     plens,      ! length of the precip event (s)
     >     startp,     ! time to start precip event (s)
     >     endp,       ! time to end precip event (s)
     >     ilens,
     >     starti,
     >     endi,
     >     truecloud 
c
      integer i,         ! loop indice
     >        jj, 
     >        ib         ! waveband number 1= visible, 2= near-IR
c
      real rtime,        ! time in hours
     >     orbit,        ! earth's orbital angle (around the sun) in radians
     >     angle,        ! solar hour angle in radians
     >     xdecl,        ! solar declination angle
     >     sw,           ! effective solar constant
     >     xlat,         ! latitude in radians
     >     trans,        ! solar transmission through the atmosphere
     >     fdiffuse,     ! fraction of indirect (diffuse) solar radiation
     >     fracw,        ! fraction of energy in each waveband
     >     wfrac,
     >     gamma,        !
     >     qmin,         !
     >     qmax,
     >     qsa,
     >     ran2,
     >     emb,
     >     ea,
     >     ec,
     >     dtair,
     >     dtcloud
c
      integer checkP,
     >        niter,
     >        plen,
     >        plenmin,
     >        plenmax
c
      include 'comsat.h'
c
c ---------------------------------------------------------------------- 
c * * * calendar and orbital calculations * * *
c ---------------------------------------------------------------------- 
c
c calculate time in hours
c
      rtime = time / 3600.0
c
c calculate the earth's orbital angle (around the sun) in radians
c
      orbit = 2.0 * pi * float(jday) / 365.2425
c
c calculate the solar hour angle in radians
c
      angle  = 2.0 * pi * (rtime - 12.0) / 24.0
c
c calculate the current solar declination angle
c ref: global physical climatology, hartmann, appendix a
c
      xdecl =  0.006918                    -
     >         0.399912 * cos(orbit)       + 
     >         0.070257 * sin(orbit)       -
     >         0.006758 * cos(2.0 * orbit) +
     >         0.000907 * sin(2.0 * orbit) -
     >         0.002697 * cos(3.0 * orbit) +
     >         0.001480 * sin(3.0 * orbit)
c
c calculate the effective solar constant, including effects of eccentricity
c ref: global physical climatology, hartmann, appendix a
c
      sw = 1370. * (1.000110 +
     >              0.034221 * cos(orbit)        + 
     >              0.001280 * sin(orbit)        + 
     >              0.000719 * cos(2.0 * orbit)  +
     >              0.000077 * sin(2.0 * orbit)) 
c
c 9001 continue
c
c do for all gridcells
c
      do 100 i = 1, npoi
c
c ---------------------------------------------------------------------- 
c * * * solar calculations * * *
c ---------------------------------------------------------------------- 
c
c calculate the latitude in radians
c
        jj = latindex(i)
c
        xlat = latscale(jj) * pi / 180.0
c
c calculate the cosine of the solar zenith angle
c
        coszen(i) = max (0.0, (sin(xlat) * sin(xdecl) +
     >                         cos(xlat) * cos(xdecl) * cos(angle)))
c
c find daylength to be used in pheno subroutine
c
        daylength(i) = (180./pi)*((2.*60.)/15.)*(acos((coszen(i)
     >      - (sin(xlat)*sin(xdecl))) / (cos(xlat)*cos(xdecl))))
c
c calculate the solar transmission through the atmosphere
c using simple linear function of tranmission and cloud cover
c
c note that the 'cloud cover' data is typically obtained from
c sunshine hours -- not direct cloud observations
c
c where, cloud cover = 1 - sunshine fraction 
c
c different authors present different values for the slope and 
c intercept terms of this equation
c
c Friend, A: Parameterization of a global daily weather generator for
c terrestrial ecosystem and biogeochemical modelling, Ecological 
c Modelling
c
c Spitters et al., 1986: Separating the diffuse and direct component
c of global radiation and its implications for modeling canopy
c photosynthesis, Part I: Components of incoming radiation,
c Agricultural and Forest Meteorology, 38, 217-229.
c
c A. Friend       : trans = 0.251 + 0.509 * (1.0 - cloud(i))
c Spitters et al. : trans = 0.200 + 0.560 * (1.0 - cloud(i))
c
c we are using the values from A. Friend
c
cjk     trans = 0.251 + 0.509 * (1.0 - cloud(i)) 
        trans = cloud(i) / sw     ! cloud(i) is surface insolation
c
cjk
c        if (jday .eq. 1) open (26, file='met.trans',status='unknown')
c        write(26,*) jday, istep, trans
c
c calculate the fraction of indirect (diffuse) solar radiation
c based upon the cloud cover
c
c note that these relationships typically are measured for either
c monthly or daily timescales, and may not be exactly appropriate
c for hourly calculations -- however, in ibis, cloud cover is fixed
c through the entire day so it may not make much difference
c
c method i --
c
c we use a simple empirical relationships from Nikolov and Zeller (1992)
c
c Nikolov, N. and K.F. Zeller, 1992:  A solar radiation algorithm for ecosystem
c dynamics models, Ecological Modelling, 61, 149-168.
c
        fdiffuse = 1.0045 + 0.0435 * trans 
     >                    - 3.5227 * trans**2
     >                    + 2.6313 * trans**3
c
        if (trans.gt.0.75) fdiffuse = 0.166
c
c method ii --
c
c another method was suggested by Spitters et al. (1986) based on
c long-term data from the Netherlands
c
c Spitters et al., 1986: Separating the diffuse and direct component
c of global radiation and its implications for modeling canopy
c photosynthesis, Part I: Components of incoming radiation,
c Agricultural and Forest Meteorology, 38, 217-229.
c
c       if ((trans.eq.0.00).and.(trans.lt.0.07)) then
c         fdiffuse = 1.0
c       else if ((trans.ge.0.07).and.(trans.lt.0.35)) then
c         fdiffuse = 1.0 - 2.3 * (trans - 0.07)**2
c       else if ((trans.ge.0.35).and.(trans.lt.0.75)) then
c         fdiffuse = 1.33 - 1.46 * trans
c       else
c         fdiffuse = 0.23
c       endif
c
c do for each waveband
c
        do 120 ib = 1, nband
c
c calculate the fraction in each waveband
c
          wfrac = 0.46 + 0.08 * float(ib - 1)
c
c calculate the direct and indirect solar radiation
c
cjk       solad(i,ib) = sw * coszen(i) * wfrac * trans *
cjk  >                  (1. - fdiffuse)  
cjk
          solad(i,ib) = wfrac * cloud(i) * (1. - fdiffuse) 
c
          solai(i,ib) = wfrac * cloud(i) * fdiffuse
c
  120   continue
c
c ---------------------------------------------------------------------- 
c * * * temperature calculations * * *
c ---------------------------------------------------------------------- 
c
c assign hourly temperatures using tmax and tmin 
c following Environmental Biophysics, by Campbell and Norman, p.23
c
c this function fits a fourier series to the diurnal temperature cycle
c note that the maximum temperature occurs at 2:00 pm local solar time
c
c note that the daily mean value of gamma is 0.44, 
c so td = 0.44 * tmax + 0.56 * tmin,  instead of
c    td = 0.50 * tmax + 0.50 * tmin
c
        gamma = 0.44 - 0.46 * sin (      pi / 12.0 * rtime + 0.9) 
     >               + 0.11 * sin (2.0 * pi / 12.0 * rtime + 0.9)
c
cjk    ta(i) = tmax(i) * gamma + tmin(i) * (1.0 - gamma)
c
c ---------------------------------------------------------------------- 
c * * * humidity calculations * * *
c ---------------------------------------------------------------------- 
c
c adjust specific humidity against daily minimum temperatures
c
c To do this, qa is written as an approximate sine function (same as ta)
c to preserve the daily mean specific humidity, while also preventing rh
c from exceeding 99% at night
c
c Note that the daily mean RH is *not* preserved, and therefore the
c output RH will be slightly different from what was read in.
c
c first adjust so that maximum RH cannot exceed 99% at night
c
cjk     qmin = min (qd(i), 0.99 * qsat(esat(tmin(i)), psurf(i)))
cjk     qmax = (qd(i) - 0.56 * qmin) / 0.44
c
c if needed, adjust again to 99% at other times of the day (in which
c case the daily mean *specific* humidity is also not preserved)
c
        qsa  = 0.99 * qsat(esat(ta(i)), psurf(i))
c
c calculate the hourly specific humidity, using the above adjustments
c
cjk     qa(i) = min (qsa, qmax * gamma + qmin * (1.0 - gamma))
        qa(i) = min (qsa, qd(i))
c
c calculate the hourly relative humidity 
c
        rh(i) = 100.0 * qa(i) / qsat(esat(ta(i)), psurf(i))
c
c ---------------------------------------------------------------------- 
c * * * wind speed calculations * * *
c ---------------------------------------------------------------------- 
c
c following logic of the EPIC weather generator
c select random wind speed following this equation
c
cjk     ua(i) = 1.13989 * ud(i) * (-log(ran2(seed)))**0.30 
c
c fix wind speeds to always be above 2.5 m/sec and below 10.0 m/sec
c
        ua(i) = max (2.5, min (10.0, ua(i)))
c
c ---------------------------------------------------------------------- 
c * * * ir flux calculations * * *
c ---------------------------------------------------------------------- 
c
c clear-sky emissivity as a function of water vapor pressure
c and atmospheric temperature
c
c calculate the ir emissivity of the clear sky
c using equation from idso (1981) water resources res., 17, 295-304
c
        emb = 0.01 * (psurf(i) * qa(i) / (0.622 + qa(i)))
        ea  = 0.70 + 5.95e-5 * emb * exp (1500.0 / ta(i))
c
c assign the ir emissivity of clouds (assume they are ~black in the ir)
c
        ec = 0.950
c
c assign the temperature difference of emissions (air + cloud) from
c the surface air temperature
c
        dtair   = 2.0
        dtcloud = 2.0
c
c total downward ir is equal to the sum of:
c
c (1) clear sky contribution to downward ir radiation flux
c (2) cloud contribution to downward ir radiation flux
c
c cjk 
c
        truecloud = 1. - ((trans - 0.251) / 0.509) 
        fira(i) = (1. -  truecloud) * ea * stef * (ta(i) - dtair  )**4 +
     >                   truecloud  * ec * stef * (ta(i) - dtcloud)**4
c
cc      fira(i) = (1. -  cloud(i)) * ea * stef * (ta(i) - dtair  )**4 +
cc   >                   cloud(i)  * ec * stef * (ta(i) - dtcloud)**4
c
c ---------------------------------------------------------------------- 
c * * * snow and rain calculations * * *
c ---------------------------------------------------------------------- 
c
c reset snow and rain to zero
c
        snowa(i) = 0.0
        raina(i) = 0.0
c
c determine the number of timesteps per day
c
        niter = int (86400.0 / dtime)
c
c change the rain length when the amount of rainfall/timestep is
C too high (at the first time step)
c
c        if (time.lt.dtime) then
c
c           plen = plens / dtime
c           plenmin = 1 +  int ((4.0 * 3600. - 1.) / dtime)
c           plenmax = max (int (24.0 * 3600. / dtime), plenmin)
c           checkP = 0
c
c           do  while (((precip(i)/plen) .gt. 15).and.(plen.lt.plenmax))
c              plen = plen + 1
c              checkP = 1
c           end do
c
c           if (checkP.eq.1) then
c
c              print *, 'WARNING: plen changed', i,
c     $             int(precip(i)), int(plens/dtime), plen
c              plens = dtime * plen
c              startp = dtime * min (niter-plen,
c     >             int(ran2(seed)*(niter-plen+1)))
c              endp = startp + plen *dtime
c              goto 9001
c           end if
c
c        end if
c
c if precipitation event then calculate
c
c cjk        if (time.ge.startp .and. time.lt.endp) then  
c
c for rain / snow partitioning, make it all rain if 
c ta > 2.5 C and all snow if ta <= 2.5 C
c
c reference:
c
c Auer, A. H., 1974: The rain versus snow threshold temperatures,
c Weatherwise, 27, 67.
c
c
          if (ta(i)-273.15 .gt. 2.5) then
            raina(i) = precip(i) / plens 
          else
            snowa(i) = precip(i) / plens
          endif
c
c cjk        endif
c
c ---------------------------------------------------------------------- 
c * * * irrigation calculations * * *
c ---------------------------------------------------------------------- 
c
c reset rate of irrigation application per timestep 
c
        xirriga(i) = 0.0
c
c if precipitation event - then no irrigation that day 
c
c
        if (time.ge.starti .and. time.lt.endi
     >      .and. irrigate .eq. 1
     >      .and. precip(i) .eq. 0.00) then  
c
          xirriga(i) = xirrig(i) / ilens
c
c update annual total - totirrig
c rate of irrigation multiplied by length of timestep (mm/s * s) = total applied
c for this timestep 
c
          totirrig(i) = totirrig(i) + (xirriga(i) * dtime)
c
        endif 
c
  100 continue
c
      return
      end
c
