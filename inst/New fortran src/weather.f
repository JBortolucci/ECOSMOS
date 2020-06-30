c
c #    #  ######    ##     #####  #    #  ######  #####
c #    #  #        #  #      #    #    #  #       #    #
c #    #  #####   #    #     #    ######  #####   #    #
c # ## #  #       ######     #    #    #  #       #####
c ##  ##  #       #    #     #    #    #  #       #   #
c #    #  ######  #    #     #    #    #  ######  #    #
c
c ---------------------------------------------------------------------
      subroutine daily (iyear, imonth, iday, jday, seed, jdaily, iyranom, iyrlast, nrun)
c ---------------------------------------------------------------------
c
c overview
c
c this routine generates daily weather conditions from monthly-mean
c climatic parameters
c
c specifically, this routine generates daily values of
c
c  - daily total precipitation
c  - daily maximum temperature
c  - daily minimum temperature
c  - daily average cloud cover
c  - daily average relative humidity
c  - daily average wind speed
c
c in order to generate daily weather conditions, the model uses a series
c of 'weather generator' approaches, which generate random combinations of
c weather conditions based upon the climatological conditions
c
c in general, this weather generator is based upon the so-called Richardson
c weather generator
c
c appropriate references include:
c
c Geng, S., F.W.T. Penning de Vries, and L. Supit, 1985:  A simple
c method for generating rainfall data, Agricultural and Forest
c Meteorology, 36, 363-376.
c
c Richardson, C. W. and Wright, D. A., 1984: WGEN: A model for 
c generating daily weather variables: U. S. Department of
c Agriculture, Agricultural Research Service.
c
c Richardson, C., 1981: Stochastic simulation of daily
c precipitation, temperature, and solar radiation. Water Resources 
c Research 17, 182-190.
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comatm.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsoi.h'
      include 'comcrop.h'
      include 'com1d.h'
c
c Arguments
c
      integer seed,
     >        iyear,
     >        imonth,
     >        iday,
     >        jday,
     >        jdaily   ! 1 if reading in daily weather data
     >                 ! 0 if using random/statistical weather generator      
c
c local variables
c
      integer it1w,      ! indice of previous month (interpolation)
     >        it2w,      ! indice of following month (interpolation)
     >        i,j, k     ! loop indice
c
      real rwork,          ! 
     >     omcloud,        ! cloud cover
     >     omqd,           ! humidity
     >     omtmax,         ! maximum temperature
     >     ran2,           ! function random number generator
     >     dt,             ! used for interpolation
     >     pwet,           ! monthly-average probability of rainy day
     >     pwd,            ! probability of a wet day after a dry day
     >     pww,            ! probability of a wet day after a wet day
     >     rndnum,         ! random number to decide if wet or dry day
     >     rainpwd,        ! average rainfall per wet day
     >     alpha,          ! parameter for gamma function
     >     beta,           ! parameter for gamma function
     >     aa,
     >     ab,
     >     tr1,
     >     tr2,
     >     rn1,rn2, rn3,rn,  !random numbers
     >     s1,
     >     s2,
     >     s12,
     >     z,
     >     tdm,            ! mean daily mean temperature
     >     trngm,          ! mean daily mean temperature
     >     tmaxm,          ! mean maximum temperature
     >     tminm,          ! mean minimum temperature
     >     tmaxd,          ! maximum temperatures for dry days
     >     tmaxw,          ! maximum temperatures for wet days
     >     tmaxe,          !'expected' maximum temperature for today
     >     tmine,          !'expected' minimum temperature for today
     >     tmaxs,          ! standard deviation in maximum temperature (K)
     >     tmins,          ! standard deviation in minimum temperature (K)
     >     cloudm,         ! mean cloud cover for today (fraction)
     >     cloudd,         ! dry day cloud cover
     >     cloudw,         ! wet day cloud cover
     >     cloude,         ! expected cloud cover today
     >     clouds,         ! standard deviation of cloud fraction
     >     v,
     >     tdum,           ! storage variable
     >     qdm,            ! mean relative humidity
     >     qdd,            ! dry day relative humidity
     >     qdw,            ! wet day relative humidity 
     >     qde,            ! expected relative humidity (based on wet/dry decision)
     >     qdup,           ! upper bound of humidity distribution function
     >     qdlow,          ! lower bound of humidity distribution function
     >     y,
     >     b3,
     >     b2,
     >     b1,
     >     x1,
     >     amn,
     >     eud             ! expected daily average wind speed from monthly mean
 
c
      real a(3,3),
     >     b(3,3)
c
      real
     >     ee(3), 
     >     r(3),
     >     rr(3),
     >     x(3)
c
      integer
     >     iyranom,
     >     iyrlast,
     >     nrun


c
      real
     >     precipfac,
     >     dif,
     >     humidfrac,
     >     sphumid,
     >     qcov(12)
c
c define autocorrelation matrices for Richardson generator
c
c note that this matrix should be based upon a statistical
c analysis of regional weather patterns
c
c for global simulations, we use 'nominal' values
c
      data a /  0.600,  0.500,  0.005,
     >          0.010,  0.250,  0.005, 
     >          0.020,  0.125,  0.250 /
c
      data b /  0.500,  0.250, -0.250,
     >          0.000,  0.500,  0.250, 
     >          0.000,  0.000,  0.500 /
c
      include 'comsat.h'
c
c ---------------------------------------------------------------------- 
c * * * initial setup for daily climate calculations * * *
c ---------------------------------------------------------------------- 
c
c define working variables
c
      rwork = (grav / rair / 0.0065)
c
c 'omega' parameters used to calculate differences in expected
c climatic parameters on wet and dry days
c
c following logic of weather generator used in the EPIC crop model
c
c omcloud -- cloud cover
c omqd    -- humidity
c omtmax  -- maximum temperature
c
      omcloud = 0.90    ! originally 0.90
      omqd    = 0.50    ! originally 0.50
      omtmax  = 0.75    ! originally 0.75
c
c calculate weighting factors used in interpolating climatological
c monthly-mean input values to daily-mean values
c
c this is a simple linear interpolation technique that takes into
c account the length of each month
c
      if (jdaily .eq. 0) then
        if (float(iday).lt.float(ndaypm(imonth)+1)/2.0) then
          it1w = imonth - 1
          it2w = imonth 
          dt   = (float(iday) - 0.5) / ndaypm(imonth) + 0.5
        else
          it1w = imonth 
          it2w = imonth + 1
          dt   = (float(iday) - 0.5) / ndaypm(imonth) - 0.5
        end if
c
        if (it1w.lt. 1) it1w = 12
        if (it2w.gt.12) it2w = 1
      end if
c
c initialize this year's values of gdd0, gdd5, tc, tw
c
      if (iday.eq.1 .and. imonth.eq.1) then

c
        call const (tcthis, npoi,  100.0)
        call const (twthis, npoi, -100.0)
c
        call const (gdd0this, npoi, 0.0)
        call const (gdd5this, npoi, 0.0)
        call const (gdd0cthis, npoi, 0.0)
        call const (gdd8this, npoi, 0.0)
        call const (gdd10this, npoi, 0.0)
        call const (gdd12this, npoi, 0.0)
c
c initialize variables to zero at beginning of year
c for crop types that do not grow over two calendar years
c 
c constant for gdd based on 10 cm soil temperature (planting bed-
c used to calculate leaf emergence after planting
c
          do  i = 1, npoi
             do  j = 1, npft
c
              if (j .le. scpft-1) then  ! natural vegetation 
              ayanpp(i,j)     = 0.0
csant--             else if (croplive(i,j) .eq. 0 .and. j .ge. scpft) then !it is yearly, therefore doesnt matter if dead, right?
              else if (                           j .ge. scpft) then
                ayanpp(i,j)     = 0.0
             endif
	     enddo
         enddo
c           
      endif
c
c initialize this crop year's values 


          do  i = 1, npoi

             do  j = scpft, ecpft

      if (iday.eq.pcd(i,j).and.imonth.eq.pcm(i,j)) then
c
	if(j.eq.13) then
	if(exist(i,13).eq.1) then
        gddsoy(i,iyear)  = gddfzsoy(i) 
    
        consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddsoy(i,iyear)=0.0
	endif
	endif

	if(j.eq.14) then
	if(exist(i,14).eq.1)then
        gddcorn(i,iyear) = gddfzcorn(i) 
        consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddcorn(i,iyear)=0.0
	endif
	endif

	if(j.eq.16) then
	if(exist(i,16).eq.1) then
        gddsgc(i,iyear)  = gddfzsgc(i)
	
	print*,'INTO WEATHER.F ',i,iyear,jday,pcd(i,j),pcm(i,j),gddsgc(i,iyear)
    
        consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddsgc(i,iyear)=0.0
	endif
	endif

csant- we dont need this if, the gddplant=0 after harvest and will be count only if croplive, right?
           if (croplive(i,j) .eq. 0 ) then
              gddplant(i,j)   = 0.0
              gddtsoi(i,j)    = 0.0
           endif
          
      endif
	
	     enddo
	enddo
c
c ---------------------------------------------------------------------- 
c * * * set daily climatic variables for entire domain * * *
c ---------------------------------------------------------------------- 
c
      do 200 i = 1, npoi
c
c ---------------------------------------------------------------------- 
c * * * use weather generator to create daily statistics * * *
c ---------------------------------------------------------------------- 
c
        if (jdaily .eq. 0) then

	print*,'Stop  - no daily weather data - not using WGEN for R version' 
	print*,iyear, imonth, iday, jday
	stop
	
c
c ---------------------------------------------------------------------- 
c (1) determine if today will rain or not (following Geng et al.)
c ---------------------------------------------------------------------- 
c
c implement simple first-order Markov-chain precipitation generator logic
c based on Geng et al. (1986), Richardson and Wright (1984),
c and Richardson (1981) 
c
c basically, this allows for the probability of today being a wet day 
c (a day with measureable precipitation) to be a function of what
c yesterday was (wet or dry)
c
c the logic here is that it is more likely that a wet day will follow
c another wet day -- allowing for 'storm events' to persist
c
c calculate monthly-average probability of rainy day 
c
          pwet = max (1., xinwet(i,imonth)) / ndaypm(imonth)
c
c estimate the probability of a wet day after a dry day
c
          pwd = 0.75 * pwet
c
c estimate the probability of a wet day after a wet day
c
          pww = 0.25 + pwd

c
c Beginning of block of code that figures out daily precip for
c entire month on the first day of each month
c
          if (iday .eq. 1) then
c
c Verify the dataset consistency especially when using interannual anomalies
c of precipitations (negative values, or too few wet days in a rainy month)
c
            xinprec(i, imonth) = max(0.01, xinprec(i, imonth))
            xinwet(i, imonth) = max(1., xinwet(i, imonth))
c
 9000       continue
c
c Initialize monthly parameters back to zero
c
            iwetdaysum(i) = 0
            precipdaysum(i) = 0
c
            do 210 j = 1, 31
              iwetday(i,j) = 0
              precipday(i,j) = 0
 210        continue
c
c Loop through number of days in this month and determine precip
c
            do 220 j = 1, ndaypm(imonth)
c
c decide if today is a wet day or a dry day using a random number
c
              rndnum = ran2(seed)
c
c
c If it is the first day of the month do not look at previous day
c
          if (j .eq. 1) then
                if (rndnum .le. pwd) then
                  iwetday(i,j) = 1
                  iwetdaysum(i) = iwetdaysum(i) + 1
                else
                  iwetday(i,j) = 0
                endif
c
         else
c
c If it is not the first day, look at yesterday's wet/dry index to help
c determine if today is wet or dry
c
c	if(i.eq.1)print*,'change all code, have to confirm it...'
             if (iwetday(i,j-1) .eq. 0) then
                  if (rndnum.le.pwd) then
                  iwetday(i,j) = 1
                  iwetdaysum(i) = iwetdaysum(i) + 1
		else
		iwetday(i,j) = 0
                  endif
             else
                  if (rndnum.le.pww) then
		iwetday(i,j) = 1
                  iwetdaysum(i) = iwetdaysum(i) + 1
		else
		iwetday(i,j) = 0
                  endif
c
             endif

          endif
c
c ---------------------------------------------------------------------- 
c (2) determine today's precipitation amount (following Geng et al.)
c ---------------------------------------------------------------------- 
c
c if it is going to rain today
c
              if (iwetday(i,j) .eq. 1) then
c
c calculate average rainfall per wet day
c
                rainpwd = xinprec(i,imonth) * ndaypm(imonth) /
     >                max (0.1, xinwet(i,imonth))
c
c randomly select a daily rainfall amount from a probability density
c function of rainfall
c
c method i --
c
c use the following technique from Geng et al. and Richardson
c to distribute rainfall probabilities
c 
c pick a random rainfall amount from a two-parameter gamma function
c distribution function
c
c estimate two parameters for gamma function (following Geng et al.)
c 
                beta  = max (1.0, -2.16 + 1.83 * rainpwd)
                alpha = rainpwd / beta
c
c determine daily precipitation amount from gamma distribution function
c (following WGEN code of Richardson and Wright (1984))
c
                aa = 1.0 / alpha 
                ab = 1.0 / (1.0 - alpha)
c
                tr1 = exp(-18.42 / aa)
                tr2 = exp(-18.42 / ab)
c
 12             rn1 = ran2(seed)
                rn2 = ran2(seed)
c
c CD: rewrote parts of prehistoric code in fortran 77 
c
                if ((rn1 - tr1) .le. 0) then
                  s1 = 0.0
                else 
                  s1 = rn1**aa
                end if
c
                if ((rn2 - tr2) .le. 0) then 
                  s2 = 0.0
                else 
                  s2 = rn2**ab
                end if
           
c               if (rn1 - tr1) 61, 61, 62
c 61            s1 = 0.0
c               go to 63
c 62            s1 = rn1**aa
c
c 63            if (rn2 - tr2) 64, 64, 65
c 64            s2 = 0.0
c               go to 66
c 65            s2 = rn2**ab
c
c
 66             s12 = s1 + s2
c
                if (s12 - 1.0)  13, 13, 12
 13             z = s1 / s12
c
                rn3 = ran2(seed)
c
                precipday(i,j) = -z * log(rn3) * beta
c

c bound daily precipitation to "realistic" range
c 
c lower end is determined by definition of a 'wet day' (at least
c 0.25 mm of total precipitation)
c
c upper end is to prevent ibis from blowing up
c
                precipday(i,j) = max (precipday(i,j),0.25) ! min =   0.25 mm/day
                precipday(i,j) = min (precipday(i,j),150.00) ! max = 150.00 mm/day
c
c Back to beginning of month loop, this is the end of it
c
              endif
c
c Add today's precip to the monthly summation
c
c
              precipdaysum(i) = precipdaysum(i) + precipday(i,j)
c
 220        continue
c
c Adjust daily precip amounts (using precipfac) so that the monthly
c summation equals the input precip amount, when using interannual
c anomalies
c
c
              if ((precipdaysum(i) .eq. 0) .AND.
     >                (xinprec(i,imonth) .gt. 0.)) then
                    rndnum = 1.0 + (float(ndaypm(imonth)) - 1.0)
     >                   * ran2(seed)
                    iwetday(i,nint(rndnum)) = 1
                    precipday(i,nint(rndnum)) = xinprec(i,imonth)
     >                   * float(ndaypm(imonth))
                    precipdaysum(i) = precipday(i,nint(rndnum))
                    iwetdaysum(i) = 1
                 end if
c
                 precipfac = (xinprec(i,imonth)*float(ndaypm(imonth)))
     >                / max(0.01,precipdaysum(i))
c
                 do 230 j=1,ndaypm(imonth)

                    precipday(i,j) = precipday(i,j) * precipfac
c
                    if (precipday(i,j).gt.140) then 

c                       if (xinwet(i,imonth) .lt. ndaypm(imonth)) then
c                          xinwet(i,imonth) = xinwet(i,imonth) + 1
c                          pwet = xinwet(i,imonth) / ndaypm(imonth)
c                          pwd = 0.75 * pwet
c                          pww = 0.25 + pwd
c                          print *,'WARNING: goto 9000a - SANTIAGO -', i,j,imonth,iyear,'<-data', int(xinwet(i
c     >                         ,imonth)), iwetdaysum(i), int(precipday(i,j)),precipfac
c                          goto 9000
csant                     ------ have to validate it ------
                       if (iwetdaysum(i).le.xinwet(i,imonth)) then
                          pwet = min(0.95,pwet+ ( (xinwet(i,imonth)-iwetdaysum(i))/ndaypm(imonth) )  )

		        if(xinwet(i,imonth).ge.25)then
                          pwd = 0.95 * pwet
                          pww = 0.05 + pwd
		        elseif(xinwet(i,imonth).ge.15.and.xinwet(i,imonth).lt.25)then
                          pwd = 0.85 * pwet
                          pww = 0.15 + pwd
			else
                          pwd = 0.75 * pwet
                          pww = 0.25 + pwd
			endif

                          print *,'goto 9000a-',i,j,imonth,iyear,'<-data', int(xinwet(i
     >                         ,imonth)), iwetdaysum(i), int(precipday(i,j)),precipfac,pwd,pww
c			pause
                          goto 9000
                       else
                          print *, 'WARNING: goto 9000b', i, int(xinwet(i
     >                         ,imonth)), iwetdaysum(i), int(precipday(i,j))
                          goto 9000
                       end if
                    end if
c    
  230            continue
c    
c Verification of the weather generator algorithm
c
                 iwetdaysum(i) = 0
                 precipdaysum(i) = 0.
c
                 do 240 j=1,ndaypm(imonth)
                    precipdaysum(i) = precipdaysum(i) + precipday(i,j)
                    iwetdaysum(i) = iwetdaysum(i) + iwetday(i,j)
  240            continue
c
                 dif = precipdaysum(i) - xinprec(i,imonth)
     >                   * float(ndaypm(imonth))
c    
                 if ((dif.lt.-0.1).or.(dif.gt.0.1)) print *,
     >                'ERROR in DAILY:', i, precipdaysum(i),
     >                xinprec(i,imonth)* float(ndaypm(imonth)),
     >                iwetdaysum(i), xinwet(i,imonth)
c    
c end of the verification
c
           end if               !end of the iday loop
c    
c Relate today's iwetday and precipday to iwet and precip that will be
c used below

           iwet(i) = iwetday(i,iday)
           precip(i) = precipday(i,iday)

c	if(i.eq.1)write(222,*)precip(i)


		if (iwetdaysum(i).gt.ndaypm(imonth)) then
		print*,'ERROR - iwetdaysum(i).ge.ndaypm(imonth)'
		stop
		endif



	if(precip(i).gt.140.)print*,'weather. - need to fix it!',precip(i) ,iwetdaysum(i)

           precip(i) = min (precip(i),140.00) ! max = 150.00 mm/day

csant ---------------------------------------------------------------------------------------- 
c * * * use CDC data to over-write the (CRU + weather generator) 			 * * *
csant ---------------------------------------------------------------------------------------- 
      if (iyear.ge.cdcyear) then

            if(cdcinprecd(i).ge.0.) then 
                precip(i) = cdcinprecd(i)

csant, certo !!                  if(cdcinprecd(i).eq.0.) then
                  if(cdcinprecd(i).lt.0.35) then  ! teste
      		    iwet(i) = 0 
		  elseif(cdcinprecd(i).lt.0.) then
	print*,'CDC prec in less than 0.0'
	stop
		  else
		    iwet(i) = 1
		  endif 		


            endif 

      endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Prec CDC Assimilation * * *
c ---------------------------------------------------------------------- 
csant ---------------------------------------------------------------------------------------- 
c * * * use station data to over-write the (CRU + weather generator) 			* * *
csant ---------------------------------------------------------------------------------------- 

      if (iyear.ge.istyear .and .iyear.le.istend ) then

            if(stinprecd(i).ge.0.) then 
                precip(i) = stinprecd(i)
               
                  if(stinprecd(i).eq.0.) then
      		    iwet(i) = 0 
		  else
		    iwet(i) = 1
		  endif 		


            endif 

      endif ! for Station data

c	if(i.eq.1)print*,iyear,jday,precip(i)
c ---------------------------------------------------------------------- 
c (3) estimate expected minimum and maximum temperatures
c ---------------------------------------------------------------------- 
c
c first determine the expected maximum and minimum temperatures
c (from climatological means) for this day of the year
c
c mean daily mean temperature (K)
c
          tdm = xint(i,it1w) +
     >          dt * (xint(i,it2w) - xint(i,it1w)) + 273.16
c
c mean daily temperature range (K)
c
          trngm = xintrng(i,it1w) +
     >            dt * (xintrng(i,it2w) - xintrng(i,it1w))
c
c mean minimum and maximum temperatures
c
          tmaxm = tdm + 0.56 * trngm
          tminm = tdm - 0.44 * trngm

c
c modify maximum temperatures for wet and dry days
c
       if (pwet .ne. 0.0) then

          if(iyear.ge.cdcyear.and.cdcinprecd(i).ge.0.)then !csant so assim os valores sao sim., ou Tmax media fica 1 graus mais baixa	
            tmaxd = tmaxm + pwet * omtmax * trngm !+ 1.2
	     tmaxw = tmaxd -        omtmax * trngm
        if(i.eq.1.and.jday.eq.1)print*,'WARNING/CDC,    CHANGE Tmax to match with CRU - at least for SP'


           else !original
        tmaxd = tmaxm + pwet * omtmax * trngm
	tmaxw = tmaxd -        omtmax * trngm
          if(i.eq.1.and.jday.eq.1)print*,'Original....'
	    endif		 	
       else
       tmaxd = tmaxm !-2. !+ 3.2
       tmaxw = tmaxm
       endif
c


c set the 'expected' maximum and minimum temperatures for today
c
c note that the expected minimum temperatures are the same for
c both wet and dry days
c
          if (iwet(i).eq.0) tmaxe = tmaxd
          if (iwet(i).eq.1) tmaxe = tmaxw
c
          tmine = tminm
c
c estimate variability in minimum and maximum temperatures
c
c tmaxs : standard deviation in maximum temperature (K)
c tmins : standard deviation in minimum temperature (K)
c
c Regression is based on analysis of 2-m air temperature data from the
c NCEP/NCAR reanalysis (1958-1997) for 294 land points over central
c North America (24N-52N, 130W-60W, 0.5-degree resolution): Daily max
c and min temperatures were calculated for each land point from daily
c mean temperature and temperature range. Anomalies were calculated
c by subtracting similar max and min temperatures calculated from
c monthly mean temperature and range (interpolated to daily). The 40
c years of anomalies were then binned by month and the standard
c deviation calculated for each month. The 294 x 12 standard
c deviations were then regressed against the 3528 long-term monthly
c mean temperatures.
c
c note: the values are bound to be greater than 1.0 K 
c       (at the very least they must be bound so they don't go below zero)
c
          tmaxs = max (1.0, -0.0713 * (tdm - 273.16) + 4.89)
          tmins = max (1.0, -0.1280 * (tdm - 273.16) + 5.73)
c
c ---------------------------------------------------------------------- 
c (4) estimate expected cloud cover
c ---------------------------------------------------------------------- 
c
c the formulation of dry and wet cloud cover has been
c derived from the weather generator used in the epic crop model
c
c cloudm : mean cloud cover for today
c cloudd : dry day cloud cover
c cloudw : wet day cloud cover
c cloude : expected cloud cover today
c
c Verify the data set consistency when using interannual anomalies of
c cloudiness (values under 0 % or over 100 %)
c
          if (iday.eq.1) then
             xincld(i,it1w) = max (0.0, xincld(i,it1w))
             xincld(i,it1w) = min (100., xincld(i,it1w))
             xincld(i,it2w) = max (0.0, xincld(i,it2w))
             xincld(i,it2w) = min (100., xincld(i,it2w))
          end if

c monthly mean cloud cover (%)
c
          cloudm = xincld(i,it1w) +
     >             dt * (xincld(i,it2w) - xincld(i,it1w))
c 
c convert from percent to fraction
c
          cloudm = cloudm / 100.0
c
c adjust cloud cover depending on dry day / rainy day
c following logic of the EPIC weather generator code
c
          if (pwet .ne. 0.0) then
            cloudd = (cloudm - pwet * omcloud) / (1.0 - pwet * omcloud)
            cloudd = min (1.0, max (0.0, cloudd))
            cloudw = (cloudm - (1.0 - pwet) * cloudd) / pwet
          else
            cloudd = cloudm
            cloudw = cloudm
          endif
c
          if (iwet(i).eq.0) cloude = cloudd
          if (iwet(i).eq.1) cloude = cloudw
c
c estimate variability in cloud cover for wet and dry days
c following numbers proposed by Richardson
c
c clouds : standard deviation of cloud fraction
c 
          if (iwet(i).eq.0) clouds = 0.24 * cloude
          if (iwet(i).eq.1) clouds = 0.48 * cloude
c
c ---------------------------------------------------------------------- 
c (5) determine today's temperatures and cloud cover using
c     first-order serial autoregressive technique
c ---------------------------------------------------------------------- 
c
c use the Richardson (1981) weather generator approach to simulate the
c daily values of minimum / maximum temperature and cloud cover
c
c following the implementation of the Richardson WGEN weather generator
c used in the EPIC crop model
c
c this approach uses a multivariate generator, which assumes that the
c perturbation of minimum / maximum temperature and cloud cover are
c normally distributed and that the serial correlation of each
c variable may be described by a first-order autoregressive model
c
c generate standard deviates for weather generator
c

c	print*,seed,ran2(seed)

          do j = 1, 3
 31         rn1 = ran2(seed)
            rn2 = ran2(seed)
            v = sqrt (-2.0 * log(rn1)) * cos(6.283185 * rn2)
            if (abs(v) .gt. 2.5) go to 31
            ee(j) = v
          enddo
c
c zero out vectors
c
          do j = 1, 3
            r(j)  = 0.0
            rr(j) = 0.0
          enddo
c
c update working vectors
c
          do j = 1, 3
            do k = 1, 3
              r(j)  = r(j)  + b(j,k) * ee(j)
              rr(j) = rr(j) + a(j,k) * xstore(i,k)
            enddo
          enddo
c
c solve for x() perturbation vector and save current vector
c into the xim1() storage vector (saved for each point)
c 
          do j = 1, 3
            x(j) = r(j) + rr(j)
            xstore(i,j) = x(j)
          enddo
c
c determine today's minimum and maximum temperature
c
          tmax(i)  = tmaxe + tmaxs * x(1)
          tmin(i)  = tmine + tmins * x(2)

c
c if tmin > tmax, then switch the two around
c
          if (tmin(i).gt.tmax(i)) then
            tdum    = tmax(i)
            tmax(i) = tmin(i)
            tmin(i) = tdum
          endif
c
c daily average temperature
c
          td(i) = 0.44 * tmax(i) + 0.56 * tmin(i)

	  if(ireccru.ge.1) then

csant	print*,'teste para ver se bate!!'
c	tmax(i)= xintmxc(i,imonth)+273.16
c	tmin(i)= xintmmc(i,imonth)+273.16
c          td(i) = 0.44 * tmax(i) + 0.56 * tmin(i)
csan - confere
	  endif

c
c determine today's cloud cover
c
          cloud(i) = cloude + clouds * x(3)
c
c constrain cloud cover to be between 0 and 100%
c
          cloud(i) = max (0.0, min (1.0, cloud(i)))

csant ---------------------------------------------------------------------------------------- 
c * * * use station data to over-write the (CRU + weather generator) or (CRU + NCEP) * * *
csant ---------------------------------------------------------------------------------------- 
c
      if (iyear.ge.istyear .and .iyear.le.istend ) then


         if(stintmax(i).ge.-40) tmax(i) = stintmax(i)

         if(stintmin(i).ge.-40) tmin(i) = stintmin(i)

         if(stintd(i).ge.-40)     td(i) = stintd(i)

        if(stincldd(i).ge.0 .and.stincldd(i).le.1) 
     >                         cloud(i) = stincldd(i)

c constrain cloud cover to be between 0 and 100%
c
          cloud(i) = max (0.0, min (1.0, cloud(i)))

	endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Temperature and Cloud cover Station Assimilation * * *
c ---------------------------------------------------------------------- 

c
c ---------------------------------------------------------------------- 
c (6) estimate today's surface atmospheric pressure
c ---------------------------------------------------------------------- 
c
c simply a function of the daily average temperature and topographic
c height -- nothing fancy here
c

          psurf(i) = 101325.0 *
     >               (td(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork
csant- use the elevation of the station (enven tough, it doesnt have almost any impact on the results)
csant          psurf(i) = 101325.0 *
csant     >               (td(i) / (td(i) + 0.0065 * 552.)) ** rwork

c
c ---------------------------------------------------------------------- 
c (7) estimate today's relative humidity
c ---------------------------------------------------------------------- 
c
c the formulation of dry and wet relative humidities has been
c derived from the weather generator used in the epic crop model
c
c qdm : mean relative humidity 
c qdd : dry day relative humidity
c qdw : rainy day relative humidity
c qde : expected relative humidity (based on wet/dry decision)
c
c Verify the data set consistency when using interannual anomalies of
c relative humidity (values over 100 % or under 0 %)
c
          if (iday.eq.1) then
             xinq(i,it1w) = max (0.0, xinq(i,it1w))
             xinq(i,it1w) = min (100., xinq(i,it1w))
             xinq(i,it2w) = max (0.0, xinq(i,it2w))
             xinq(i,it2w) = min (100., xinq(i,it2w))
          end if
c
c mean relative humidity (%)
c
          qdm = xinq(i,it1w) + dt * (xinq(i,it2w) - xinq(i,it1w))
c 
c convert from percent to fraction
c
          qdm = qdm / 100.0
c
c adjust humidity depending on dry day / rainy day
c following logic of the EPIC weather generator code
c
          if (pwet .ne. 0.0) then
            qdd = (qdm - pwet * omqd) / (1.0 - pwet * omqd)
            if (qdd .lt. 0.2) then
              qdd = 0.2
              if (qdd .gt. qdm) qdm = qdd
            endif
            qdd = min(1.0, qdd)
            qdw = (qdm - (1.0 - pwet) * qdd) / pwet
          else
            qdd = qdm
            qdw = qdm
          endif
c
          if (iwet(i).eq.0) qde = qdd
          if (iwet(i).eq.1) qde = qdw 
c
c estimate lower and upper bounds of humidity distribution function
c following logic of the EPIC weather generator code
c
          qdup  = qde + (1.0 - qde) * exp (qde - 1.0)
          qdlow = qde * (1.0 - exp (-qde))
c
c randomly select humidity from triangular distribution function
c following logic of the EPIC weather generator code
c
          rn = ran2(seed)
c
          y  = 2.0 / (qdup - qdlow)
c
          b3 = qde  - qdlow
          b2 = qdup - qde
          b1 = rn / y
c
          x1 = y * b3 / 2.0 
c
          if (rn.gt.x1) then
            qd(i) = qdup  - sqrt (b2 * b2 - 2.0 * b2 * (b1 - 0.5 * b3))
          else
            qd(i) = qdlow + sqrt (2.0 * b1 * b3)
          endif
c
c adjust daily humidity to conserve monthly mean values
c
c note that this adjustment sometimes gives rise to humidity
c values greater than 1.0 -- which is corrected below
c
          amn = (qdup + qde + qdlow) / 3.0
          qd(i) = qd(i) * qde / amn
c
c constrain daily average relative humidity
c
          qd(i) = max (0.30, qd(i))
          qd(i) = min (0.99, qd(i))
c
c convert from relative humidity to specific humidity at
c daily mean temperature
c
          qd(i) = qd(i) * qsat(esat(td(i)), psurf(i))



csant ---------------------------------------------------------------------------------------- 
c * * * use station data to over-write the (CRU + weather generator) or (CRU + NCEP) * * *
csant ---------------------------------------------------------------------------------------- 


      if (iyear.ge.istyear .and .iyear.le.istend ) then

c by now, daily surface atmospheric pressure is function of the daily average temperature and topographic

       if(stintd(i).ge.-40) 
     >     psurf(i) = 101325.0 *
c     >            (stintd(i) / (td(i) + 0.0065 * 450?elev. metros?)) ** rwork
     >            (stintd(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork

csant- Talk  with Chris if is best to do this or follow the NCEP assimilation process
c daily relative humidity


c mean relative humidity (%)

      if(stinqd(i).ge.0) then

      qdm = stinqd(i)

c convert from percent to fraction
          qdm = qdm / 100.0

c constrain daily average relative humidity

          qd(i) = max (0.30, qdm)
          qd(i) = min (0.99, qdm)

c convert from relative humidity to specific humidity 

          qd(i) = qd(i) * qsat(esat(stintd(i)), psurf(i))

   	   endif

	endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Relative humidity Station Assimilation * * *
c ---------------------------------------------------------------------- 

c
c ---------------------------------------------------------------------- 
c (8) estimate today's daily average wind speed
c ---------------------------------------------------------------------- 
c
c first estimate the expected daily average wind speed (from monthly
c means)
c
          eud = xinwind(i,it1w) +
     >          dt * (xinwind(i,it2w) - xinwind(i,it1w))
c
c following logic of the EPIC weather generator
c select random wind speed following this equation
c
          ud(i) = 1.13989 * eud * (-log(ran2(seed)))**0.30 
c
c TET change 10/29/02
c Input windspeed is assumed to be measured at canopy height
c (ztop(i,2))
c IBIS now has siga = 0.991 which is ~93m height.
c Adjust windspeed according to log-wind profile.
c So let
c displacement height, d = 0.65(ztop(i,2))
c roughness length, zm = 0.1(ztop(i,2))
c Equation found in Bonan (2002) and other texts:
c u(z2)-u(z1)=(frictionvelocity/k)*log((z2-d)/(z1-d))
c Use log-wind to solve for frictionvelocity and substitute
c into above equation:
c u(z2) = u(z1)*(1.+(log((z2-d)/(z1-d))) / (log((z1-d)/zm)))
c Use canopyheight = z1 = ztop(i,2), and z2=93m
c and substitute d and zm to get equation dependent on canopy height:
csant- change z2=93m to z2=za(i) 
c	if(i.eq.1)print*,za(i),ztop(i,2),ztop(i,1)
csant- this esquation is doesnt fit to ztop(i,1), solve the equat above in the near future to get the wind write!!!!!

c          ud(i) = ud(i)*(1. + 0.79824*log((93.-0.65*ztop(i,2))/
           ud(i) = ud(i)*(1. + 0.79824*log((za(i)-0.65*ztop(i,2))/
     *                   (0.35*ztop(i,2))))

c	print*,i, ud(i),za(i),ztop(i,2),'ireccru ', ireccru
c
c Decided to use a canopy height of 20m over US after finding
c references of mature forest height between 10m and 35m (we are
c trying to simulate forests before human interference)
c with ztop(i,2) = 20m, the input windspeed is adjusted by a
c factor of 3.21
c
c TET changed constraint since it will be above 10 often
c
c constrain daily wind speeds to be between 2.5 and 50.0 m/sec
c
c          ud(i) = max (2.5, min (10.0, ud(i)))
          ud(i) = max (2.5, min (50.0, ud(i)))
c


csant ---------------------------------------------------------------------------------------- 
c * * * use station data to over-write the (CRU + weather generator) or (CRU + NCEP) * * *
csant ---------------------------------------------------------------------------------------- 

      if (iyear.ge.istyear .and .iyear.le.istend ) then

c
c daily average wind speed
c
          if(stinwindd(i).ge.0.) then
 
          ud(i) = stinwindd(i)

	  endif ! for wind

       endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Wind Station Assimilation * * *
c ---------------------------------------------------------------------- 


        else





c --------------------------------------------------------------------------------------- 
c * * * Disaggregated the CRU monthly value to daily value using NCEP daily anomaly * * *
c --------------------------------------------------------------------------------------- 

c
c use basic daily climate data, converting units
c
c daily total precipitation
c
c Here we multiply xinprecd, the daily fraction of precip calculated from
c the NCEP dataset, by xinprec, the total monthly amount of precip taken from
c the CRU05 dataset to obtain our derived daily precip amount. Also do a check
c to see if the daily precip exceeds 360mm (as is done in the daily weather 
c generator) ... no correction is made, only a warning is printed
c
csant - R          precip(i) = (xinprec(i,imonth) * ndaypm(imonth)) * xinprecd(i)
c
c          if (precip(i) .gt. 360) then
c            print *, 'WARNING: daily precip exceeds 360mm for'
c            print *, 'year, month, day, gridcell = '
c            print *, iyear, imonth, iday, i
c          endif
c
c daily average temperatures
c
c Here we add the NCEP temperature anomaly to the CRU05 monthly anomaly
c The trange NCEP anomaly must also be multiplied by the climatological
c CRU05 trange in order to determine tmax and tmin
c
csant - R          td(i) = xint(i,imonth) + 273.16 + xintd(i)
csant - R          trngm = min (44.0, (xintrng(i,imonth) * xintrngd(i)))
c
csant - R          tmax(i) = td(i) + 0.56 * trngm
csant - R          tmin(i) = td(i) - 0.44 * trngm
c
c         tmax(i) = xintmax(i) + 273.16 
c         tmin(i) = xintmin(i) + 273.16 
c
c daily average cloud cover
c
c Here we add the NCEP cloud anomaly to the monthly anomaly from CRU05
c before converting percentage of cover to fraction
c We also bound cloud cover fraction between 0 and 1
c
csant - R          cloud(i) = (xincld(i,imonth) + xincldd(i)) * 0.01
c
csant - R          cloud(i) = min (cloud(i), 1.)
csant - R          cloud(i) = max (0.0, cloud(i))
c
c compute surface atmospheric pressure
c
csant - R          psurf(i) = 101325.0 *
csant - R     >               (td(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork
c
c daily average specific humidity
c
c First we must convert relative humidity to a fraction and then convert
c the fraction to monthly mean specific humidity (based on the monthly
c mean temperature)
c
csant - R          humidfrac = xinq(i,imonth) / 100.
csant - R          sphumid = humidfrac * qsat(esat(273.16+xint(i,imonth)),psurf(i))
c
c Correct for covariances between relative humidity and temperature,
c so that one has the "true" monthly mean specific humidity (i.e., as
c would be calculated from the hourly specific humidity, rather than
c based on the monthly mean temperature). Correction factors for each
c month are based on data from the Trout Lake region of northern
c Wisconsin (for 1996-2000), where qcov = -1 + <RH*qs(Ta)>/<RH>*qs<Ta>.
csant - R         qcov(1) = 0.194
csant - R          qcov(2) = 0.142
csant - R          qcov(3) = 0.102
csant - R          qcov(4) = -0.026
csant - R          qcov(5) = -0.017
csant - R          qcov(6) = 0.005
csant - R          qcov(7) = -0.006
csant - R          qcov(8) = -0.006
csant - R          qcov(9) = 0.017
csant - R          qcov(10) = 0.046
csant - R          qcov(11) = 0.052
csant - R          qcov(12) = 0.118
csant - R          sphumid = sphumid * (1. + qcov(imonth))
c
c Convert monthly mean specific humidity to daily mean using NCEP
c daily anomalies
csant - R          qd(i) = sphumid * xinqd(i)
c
c daily average wind speed
c
c Here we multiply the NCEP fraction of windspeed by the CRU05
c climatological monthly windspeed
c
csant - R          ud(i) = xinwind(i,imonth) * xinwindd(i)
c

c TET change 10/29/02
c Input windspeed is assumed to be measured at canopy height
c (ztop(i,2))
c IBIS now has siga = 0.991 which is ~93m height.
c Adjust windspeed according to log-wind profile.
c So let
c displacement height, d = 0.65(ztop(i,2))
c roughness length, zm = 0.1(ztop(i,2))
c Equation found in Bonan (2002) and other texts:
c u(z2)-u(z1)=(frictionvelocity/k)*log((z2-d)/(z1-d))
c Use log-wind to solve for frictionvelocity and substitute
c into above equation:
c u(z2) = u(z1)*(1.+(log((z2-d)/(z1-d))) / (log((z1-d)/zm)))
c Use canopyheight = z1 = ztop(i,2), and z2=93m
c and substitute d and zm to get equation dependent on canopy height:
csant- change z2=93m to z2=za(i)
csant	if(i.eq.1)print*,za(i),ztop(i,2)
c          ud(i) = ud(i)*(1. + 0.79824*log((93.-0.65*ztop(i,2))/
csant - R           ud(i) = ud(i)*(1. + 0.79824*log((za(i)-0.65*ztop(i,2))/
csant - R     *                   (0.35*ztop(i,2))))
c
c Decided to use a canopy height of 20m over US after finding
c references of mature forest height between 10m and 35m (we are
c trying to simulate forests before human interference)
c with ztop(i,2) = 20m, the input windspeed is adjusted by a
c factor of 3.21
c

csant ---------------------------------------------------------------------------------------- 
c * * * use station data to over-write the (CRU + weather generator) or (CRU + NCEP) * * *
csant ---------------------------------------------------------------------------------------- 
c

      if (iyear.ge.istyear .and .iyear.le.istend ) then

      if(stinprecd(i).ge.0.) precip(i) = stinprecd(i)


      if(stintmax(i).ge.-40) tmax(i) = stintmax(i)
      if(stintmin(i).ge.-40) tmin(i) = stintmin(i)
      if(stintd(i).ge.-40) td(i) = stintd(i)


      if(stincldd(i).ge.0 .and.stincldd(i).le.1) 
     >  cloud(i) = stincldd(i)

c daily surface atmospheric pressure
c simply a function of the daily average temperature and topographic
c height -- nothing fancy here
c
      if(stintd(i).ge.-40) 
     >     psurf(i) = 101325.0 *
c     >            (stintd(i) / (td(i) + 0.0065 * 450?elev. metros?)) ** rwork
     >            (stintd(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork


c mean relative humidity (%)
c
      if(stinqd(i).ge.0) then

      qdm = stinqd(i)

c convert from percent to fraction
          qdm = qdm / 100.0

c constrain daily average relative humidity
          qd(i) = max (0.30, qdm)
          qd(i) = min (0.99, qdm)

c convert from relative humidity to specific humidity at
c daily mean temperature
          qd(i) = qd(i) * qsat(esat(stintd(i)), psurf(i))

	endif

c daily average wind speed

       if(stinwindd(i).ge.0.) ud(i) = stinwindd(i)


	endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Station Assimilation * * *
c ---------------------------------------------------------------------- 
csant ---------------------------------------------------------------------------------------- 
c * * * use CDC data to over-write the (CRU + weather generator) or (CRU + NCEP) * * *
csant ---------------------------------------------------------------------------------------- 
      if (iyear.ge.cdcyear) then

            if(cdcinprecd(i).ge.0.) then 
                precip(i) = cdcinprecd(i)
            endif 

      endif ! for Station data
c ---------------------------------------------------------------------- 
c * * * End of Prec CDC Assimilation * * *
c ---------------------------------------------------------------------- 





        end if !general if for data



c	if(i.eq.1)print*,iyear,jday,precip(i),td(i),	
c     > tmax(i), tmin(i),cloud(i),qd(i)

csant- make sure that the station value will be used only if read a new value
	stinprecd(i) = -999.0
	stintd(i)    = -999.0
	stintmax(i)  = -999.0
	stintmin(i)  = -999.0
	stincldd(i)  = -999.0
	stinqd(i)    = -999.0
	stinwindd(i) = -999.0
 	cdcinprecd(i)= -999.0

c
c ---------------------------------------------------------------------- 
c * * * other daily climate calculations * * *
c ---------------------------------------------------------------------- 
c
c calculated temperature extremes -- for vegetation limits (deg c)
c
c for this purpose, use the 10-day running mean temperature
c
        tcthis(i) = min (tcthis(i), (a10td(i) - 273.16))
        twthis(i) = max (twthis(i), (a10td(i) - 273.16))
c
c update this year's growing degree days
c

        gdd0this(i)  = gdd0this(i)  + max (0.0 ,(td(i) - 273.16)) 
        gdd0cthis(i) = gdd0cthis(i) + max (0.0 ,(td(i) - baset(15)))    ! wheat
        gdd5this(i)  = gdd5this(i)  + max (0.0 ,(td(i) - 278.16))
        gdd8this(i)  = gdd8this(i)  + max (0.0 ,(td(i) - baset(14)))    ! maize 
        gdd10this(i) = gdd10this(i) + max (0.0 ,(td(i) - baset(13)))    ! soybean
        gdd12this(i) = gdd12this(i) + max (0.0 ,(td(i) - baset(16)))    ! sugarcane 


c form calculations of number of growing degree days between frost events
c
c events (e.g., tmin(i) .le. -2.2 C) this is applicable to CRU data
c differences exist when using a combination of CRU/NCEP  
c -2.2 used as a threshold from Schwartz and Reiter, 2000.  International
c Journal of Climatology, 20: 929-932.  Changes in North American Spring 
c       
csant - this relationship were set for US conditions


	if(gdddy.eq.1) then !!!!update GDD (is not working for SA)

        if (tmin(i) .ge. 273.16) then
          consdays(i) = consdays(i) + 1
          maxcons(i)  = max(maxcons(i), consdays(i))
          if (maxcons(i) .eq. consdays(i)) then

            iniday(i) = cdays(i)+1 - maxcons(i)
          endif

          daygddc(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(14)))) 
          daygdds(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(13)))) 
          daygddw(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(15)))) 
        daygddsgc(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(16)))) 
c
        else
          consdays(i) = 0
        endif

c	if(i.eq.1)
c     >	print*,cdays(i),td(i)
c
         if (cdays(i).eq.365) then
c
c calculate total number of growing season GDD and number
c of days long
c
             if (iniday(i) .eq. 9999) then
                 iniday(i) = cdays (i)
                 maxcons(i) = 1
	     elseif (iniday(i) .eq. 0) then
	     iniday(i)=1
             endif      
csant- for what? endday
             endday(i) = iniday(i) + maxcons(i)-1

          do 125 k = iniday(i), endday(i)

             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
csant -didnt see use            gddfzwht(i)  =  gddfzwht(i)  + daygddw(i,k)
             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
             gsdays(i) = gsdays(i) + 1 
 125      continue


	print*,'INTO WEATHER.F ok',gddfzsgc(i) 

c        if (pcd(16).eq.1.and.pcm(16).eq.1) then

c           gddcorn(i,iyear+1) = gddfzcorn(i) 
c          gddsoy(i,iyear+1)  = gddfzsoy(i) 
csant- didnt see use        gddwht(i,iyear)  = gddfzwht(i) 
c           gddsgc(i,iyear+1)  = gddfzsgc(i)
c	print*,'365_1',iyear,iyear+1,jday,cdays(i),gddsgc(i,iyear+1)

c        elseif (pcd(16).eq.2.and.pcm(16).eq.1.and.jday.eq.366) then
c           gddcorn(i,iyear+1) = gddfzcorn(i) 
c           gddsoy(i,iyear+1)  = gddfzsoy(i) 
csant- didnt see use        gddwht(i,iyear)  = gddfzwht(i) 
c          gddsgc(i,iyear+1)  = gddfzsgc(i)

c	print*,'365',iyear,iyear+1,jday,cdays(i),gddsgc(i,iyear+1)

c	else
c           gddcorn(i,iyear) = gddfzcorn(i) 
c           gddsoy(i,iyear)  = gddfzsoy(i) 
csant- didnt see use        gddwht(i,iyear)  = gddfzwht(i) 
c           gddsgc(i,iyear)  = gddfzsgc(i)

c	print*,'365',iyear,iyear,jday,cdays(i),gddsgc(i,iyear)

c	endif


        elseif (cdays(i).eq.366) then

             if (iniday(i).eq.0) then
                 iniday(i) = 1
             endif      

             endday(i) = iniday(i) + maxcons(i)-1

             gddfzcorn(i) = 0
             gddfzsoy(i)  = 0
             gddfzsgc(i) =  0
             gsdays(i) =    0


          do  k = iniday(i), endday(i)
             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
             gsdays(i) = gsdays(i) + 1 
         enddo
csant- I changed here for the beginning of the year for simplicty, but both give the same result
c            if (pcd(16).eq.1.and.pcm(16).eq.1) then

c           gddcorn(i,iyear+1) = gddfzcorn(i) 
c           gddsoy(i,iyear+1)  = gddfzsoy(i) 
csant- didnt see use        gddwht(i,iyear)  = gddfzwht(i) 
c           gddsgc(i,iyear+1)  = gddfzsgc(i)
c	print*,'366_1',iyear,iyear+1,jday,cdays(i),gddsgc(i,iyear+1)
c	    else
c           gddcorn(i,iyear) = gddfzcorn(i) 
c           gddsoy(i,iyear)  = gddfzsoy(i) 
csant- didnt see use        gddwht(i,iyear)  = gddfzwht(i) 
c           gddsgc(i,iyear)  = gddfzsgc(i)

c	print*,'366',iyear,jday,cdays(i),gddsgc(i,iyear)
c            endif

        endif

	endif !for South America
c
c accumulate growing degree days for planted crops past planting
c
        do 150 j = scpft, ecpft 
c
c for crops except winter wheat
c be careful with rotations
c 
       if (croplive(i,j).eq.1.0.and.((j.eq.13.or.j.eq. 14.or.j.eq. 16)
     >       .or.    (iwheat .eq. 1 .and. j .eq. 15))) then
c
             gddplant(i,j) = gddplant(i,j) + max(0.0, min(td(i)
     >                                     - baset(j), mxtmp(j)))
c
             gddtsoi(i,j)  = gddtsoi(i,j) + max(0.0, min(tsoi(i,1)
     >                                    - baset(j), mxtmp(j)))
c
c if winter wheat is planted, reduce thermal time accumulation
c by vernalization factor (calculated in crops.f) until crop
c is fully vernalized (cold-hardened)
c
          else if (croplive(i,j) .eq. 1.0 .and. j .eq. 15 .and. 
     >             iwheat .eq. 2) then
c
             gddplant(i,j) = gddplant(i,j) + vf(i) * max(0.0, min(td(i)
     >                                     - baset(j), mxtmp(j)))
c
             gddtsoi(i,j)  = gddtsoi(i,j)  +  vf(i) * max(0.0, min(tsoi(i,1)
     >                                     - baset(j), mxtmp(j)))
c
        endif

csant- nao encontrei a solucao no momento, futuramente alterar essa funcao, que pode ser importante para rodadas com alteracoes climaticas.

c          do  k = iniday(i), endday(i)
c             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
c             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
c             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
c             gsdays(i) = gsdays(i) + 1 
c         enddo


	if(j.eq.13.or.j.eq.14) then

	  ik(i,j)=ik(i,j)+1
	 if(idpp(i,j).eq.1) then
	 ik(i,j)=1
	endif

       if (ik(i,j).le.(mxmat(j)-15)) then

             gddfzcorn(i) =  gddfzcorn(i) + min(30.0,max(0.0, (td(i) - baset(j))))
             gddfzsoy(i)  =  gddfzsoy(i)  + min(30.0,max(0.0, (td(i) - baset(j))))

	endif


	endif
	


	if(j.eq.16) then

	  ik(i,j)=ik(i,j)+1
	 if(cropy(i).eq.1.and.idpp(i,j).eq.1) then
	 print*,'GDD_start  SUGARCANE',i,iyear,jday
         gddpl15(i)=0	
	 ik(i,j)=1
	endif

       if (ik(i,j).le.(mxmat(16)-30)) then
       gddpl15(i)= gddpl15(i) + max(0.0, min(td(i)- baset(j), mxtmp(j)))
	endif

       if (ik(i,j).eq.(mxmat(16)-30)) then
	print*,'GDD expected for planting, before',i,iyear,jday,gddsgcp(i,1),gddpl15(i),mxmat(16)-30	
       gddsgcp(i,1)= (gddsgcp(i,1)+ gddpl15(i))/2
	print*,'GDD expected for planting, updated',i,iyear,jday,gddsgcp(i,1)	
	endif

	endif
	

c
 150  continue
c
 200  continue

c
c return to main program
c
      return
      end
c
c ---------------------------------------------------------------------
      subroutine diurnal (time, jday, plens, startp, endp, seed,
     >                    irrigate, ilens, starti, endi)
c ---------------------------------------------------------------------
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comwork.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
      include 'combcs.h'
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
     >     endi
c
c determine the length of a precipitation event (between 4 and 24 hours),
c and time to start and end precipitation event. plen is in timesteps, while
c plens, startp, and endp are in seconds
c
c
c local variables
c
      integer i,j,         ! loop indice
     >        jj,ij, 
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
     >     fracw,         ! fraction of energy in each waveband
     >     gamma,        !
     >     qmin,         !
     >     qmax,
     >     qsa,
     >     ran2,
     >     emb,
     >     ea,
     >     ec,
     >     dtair,
     >     dtcloud,cosz,
     >     dailyrad      !daily maximum total radiation
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
csant 9001 continue ! I didnt find the reason why have to back all, instead of just rain lengh calculations
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

csant- these is an first attempt to acheive some agrement between the rad from station data and from IBIS (A. Friend)
csant- though it looks working, one should come up with some publish solution in near future!! 	

	if(time.eq.0.) then

	ij=((24*3600/dtime)-1)

	do j = 0, ij	!integral for 1 day

c calculate the cosine of the solar zenith angle
c
        cosz = max (0.0, (sin(xlat) * sin(xdecl) +
     >                         cos(xlat) * cos(xdecl) 
     >    * cos( (2.0* pi*(j-12.0)/ 24.0) )))

        trans = 0.251 + 0.509 !* (1.0 - cloud(i)) - make trans = 1 to calculate the maximum hipotetical radiation.

          dailyrad  = dailyrad + (sw*cosz*trans)*(3600./10**6)   

	enddo

	if(stinrad(i).ge.0.0) then
	cloud(i)=0.76*(1-(stinrad(i)/dailyrad))/0.509  !invert the original equation for trans
	cloud(i)= max(0.,min(cloud(i),1.))
	if(jday.eq.1.and.i.eq.1)
     >	print*,'be sure that is reading solar radiation (MJ/m2.day) stinrad'
	endif

	dailyrad = 0.
	stinrad(i) = -999.0

c	if(i.eq.1) print*,jday,dailyrad,stinrad(i),stinrad(i)/dailyrad

	endif


        trans = 0.251 + 0.509 * (1.0 - cloud(i)) 
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
          fracw = 0.46 + 0.08 * float(ib - 1)
c
c calculate the direct and indirect solar radiation
c
          solad(i,ib) = sw * coszen(i) * fracw * trans *
     >                  (1. - fdiffuse)  
c
          solai(i,ib) = sw * coszen(i) * fracw * trans * fdiffuse
c
  120   continue

c	if(time.eq.13*dtime.and.i.eq.1)
csant      if(i.eq.1)print*,jday,time,solad(i,1)+solad(i,2)+solai(i,1)+solai(i,2)

csant- compare IBIS daily rad with station
csant-	if(time.eq.0) then
csant-	radd(i)= 0.0
csant-	else
csant-        radd(i)=radd(i) +
csant-     >  (solad(1,1)+solad(1,2)+solai(1,1)+solai(1,2))*(3600./10**6)
csant-	endif
csant-	if (time.eq.82800.and.i.eq.1)  write(223,*)jday,radd(i)

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
        ta(i) = tmax(i) * gamma + tmin(i) * (1.0 - gamma)

c      if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,ta(i)
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
        qmin = min (qd(i), 0.99 * qsat(esat(tmin(i)), psurf(i)))
        qmax = (qd(i) - 0.56 * qmin) / 0.44

csant	if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,psurf(i)
c
c if needed, adjust again to 99% at other times of the day (in which
c case the daily mean *specific* humidity is also not preserved)
c
        qsa  = 0.99 * qsat(esat(ta(i)), psurf(i))
c
c calculate the hourly specific humidity, using the above adjustments
c
        qa(i) = min (qsa, qmax * gamma + qmin * (1.0 - gamma))

csant      if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,qa(i)
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


csant - R        ua(i) = 1.13989 * ud(i) * (-log(ran2(seed)))**0.30 
        ua(i) = 1.13989 * ud(i) * (-log(ran2(rtime/23)))**0.30 


csant- *0.5 in order to acheive the USR observed wind speed (must be changed to run for other place)
csant-        ua(i) = ua(i) * 0.5

 	ua(i) = max (0.2, min (10.0, ua(i)))

c      if(i.eq.1)print*,'diurnal  ',jday,time,ua(i)
c
c fix wind speeds to always be above 2.5 m/sec and below 10.0 m/sec
c
csant- reducing the wind to acheive the mag of the observed dada over USR
csant-therefore, if run for any other place, have to see the statistic!!!!! 
csant -(obs. the wind and prec are the main problem with the use of the weather 
csant        ua(i) = max (2.5, min (10.0, ua(i)))


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
        fira(i) = (1. -  cloud(i)) * ea * stef * (ta(i) - dtair  )**4 +
     >                   cloud(i)  * ec * stef * (ta(i) - dtcloud)**4

c	if(time.eq.13*dtime.and.i.eq.1)
c     >	print*,jday,time,fira(i)
c
c ---------------------------------------------------------------------- 
c * * * snow and rain calculations * * *
c ---------------------------------------------------------------------- 
c
 9001  continue
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

        if (time.lt.dtime) then
c

           plen = plens / dtime


c	 if(i.eq.1)print*,jday,time,precip(i),' before ',plen

           plenmin = 1 +  int ((4.0 * 3600. - 1.) / dtime)
           plenmax = max (int (24.0 * 3600. / dtime), plenmin)
           checkP = 0
c
           do  while (((precip(i)/plen) .gt. 95).and.(plen.lt.plenmax))
              plen = plen + 1
              checkP = 1
           end do
c
           if (checkP.eq.1) then
c
csant             print *, 'WARNING: plen changed', i,
csant     $             int(precip(i)), int(plens/dtime), plen
              plens = dtime * plen

csant - R              startp = dtime * min (niter-plen,
csant - R      >             int(ran2(seed)*(niter-plen+1)))
              startp = dtime * min (niter-plen,
     >             int(ran2(rtime/23)*(niter-plen+1)))



              endp = startp + plen *dtime
              goto 9001
           end if
c
        end if
c
c if precipitation event then calculate
c
        if (time.ge.startp .and. time.lt.endp) then  
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
        endif

c	 if(i.eq.1)print*,jday,time,raina(i),precip(i),plen,ta(i)-273.15

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

c -----------------------------------------------------------------------------------
      subroutine dailymet (imonth, iday, seed, jday)
c -----------------------------------------------------------------------------------
c
c overview
c
c this routine generates daily weather conditions from hourly obsarvations - 
c
c
c specifically, this routine needs daily values of
c
c  - daily total precipitation
c  - daily maximum temperature
c  - daily minimum temperature
c  - daily average cloud cover
c  - daily average relative humidity
c  - daily average wind speed
c
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comatm.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsoi.h'
      include 'comcrop.h'
c
c Arguments
c
      integer seed,
     >        iyear,
     >        imonth,
     >        iday,
     >        jday   ! 1 if reading in daily weather data
     >                 ! 0 if using random/statistical weather generator      
c
c local variables
c
      integer it1w,      ! indice of previous month (interpolation)
     >        it2w,      ! indice of following month (interpolation)
     >        i,j, k     ! loop indice
c
      real rwork,          ! 
     >     omcloud,        ! cloud cover
     >     omqd,           ! humidity
     >     omtmax,         ! maximum temperature
     >     ran2,           ! function random number generator
     >     dt,             ! used for interpolation
     >     pwet,           ! monthly-average probability of rainy day
     >     pwd,            ! probability of a wet day after a dry day
     >     pww,            ! probability of a wet day after a wet day
     >     rndnum,         ! random number to decide if wet or dry day
     >     rainpwd,        ! average rainfall per wet day
     >     alpha,          ! parameter for gamma function
     >     beta,           ! parameter for gamma function
     >     aa,
     >     ab,
     >     tr1,
     >     tr2,
     >     rn1,rn2, rn3,rn,  !random numbers
     >     s1,
     >     s2,
     >     s12,
     >     z,
     >     tdm,            ! mean daily mean temperature
     >     trngm,          ! mean daily mean temperature
     >     tmaxm,          ! mean maximum temperature
     >     tminm,          ! mean minimum temperature
     >     tmaxd,          ! maximum temperatures for dry days
     >     tmaxw,          ! maximum temperatures for wet days
     >     tmaxe,          !'expected' maximum temperature for today
     >     tmine,          !'expected' minimum temperature for today
     >     tmaxs,          ! standard deviation in maximum temperature (K)
     >     tmins,          ! standard deviation in minimum temperature (K)
     >     cloudm,         ! mean cloud cover for today (fraction)
     >     cloudd,         ! dry day cloud cover
     >     cloudw,         ! wet day cloud cover
     >     cloude,         ! expected cloud cover today
     >     clouds,         ! standard deviation of cloud fraction
     >     v,
     >     tdum,           ! storage variable
     >     qdm,            ! mean relative humidity
     >     qdd,            ! dry day relative humidity
     >     qdw,            ! wet day relative humidity 
     >     qde,            ! expected relative humidity (based on wet/dry decision)
     >     qdup,           ! upper bound of humidity distribution function
     >     qdlow,          ! lower bound of humidity distribution function
     >     y,
     >     b3,
     >     b2,
     >     b1,
     >     x1,
     >     amn,
     >     eud             ! expected daily average wind speed from monthly mean
c 
      integer
     >     iyranom,
     >     iyrlast,
     >     nrun
c
      real
     >     precipfac,
     >     dif,
     >     humidfrac,
     >     sphumid

c
      include 'comsat.h'


c
c initialize this year's values of gdd
c
      if (iday.eq.1 .and. imonth.eq.1) then

c
        call const (tcthis, npoi,  100.0)
        call const (twthis, npoi, -100.0)
c
        call const (gdd0this, npoi, 0.0)
        call const (gdd5this, npoi, 0.0)
        call const (gdd0cthis, npoi, 0.0)
        call const (gdd8this, npoi, 0.0)
        call const (gdd10this, npoi, 0.0)
        call const (gdd12this, npoi, 0.0)
c
c initialize variables to zero at beginning of year
c for crop types that do not grow over two calendar years
c 
c constant for gdd based on 10 cm soil temperature (planting bed-
c used to calculate leaf emergence after planting
c
          do  i = 1, npoi
             do  j = 1, npft
c
              if (j .le. scpft-1) then  ! natural vegetation 
              ayanpp(i,j)     = 0.0
csant--             else if (croplive(i,j) .eq. 0 .and. j .ge. scpft) then !it is yearly, therefore doesnt matter if dead, right?
              else if ( j .ge. scpft) then
                ayanpp(i,j)     = 0.0
             endif
	     enddo
         enddo
c           
      endif
c
c initialize this crop year's values 


          do  i = 1, npoi

             do  j = scpft, ecpft

      if (iday.eq.pcd(i,j).and.imonth.eq.pcm(i,j)) then
c

	if(exist(i,13).eq.1.and.j.eq.13) then
        gddsoy(i,iyear)  = gddfzsoy(i) 
            consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddsoy(i,iyear)=0.0
	endif

	if(exist(i,14).eq.1.and.j.eq.14) then
        gddcorn(i,iyear) = gddfzcorn(i) 
    
        consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddcorn(i,iyear)=0.0
	endif

	if(exist(i,16).eq.1.and.j.eq.16) then
        gddsgc(i,iyear)  = gddfzsgc(i)
	
        consdays(i)=0
        iniday(i)=9999
        maxcons(i)=0
        gsdays(i)=0
        gddfzcorn(i)=0.0
        gddfzsoy(i)=0.0
        gddfzsgc(i)=0.0
	else
	gddsgc(i,iyear)=0.0
	endif

	print*,'into weather.f  dailymet',gddsgc(i,iyear-1),gddsgc(i,iyear)

csant- we dont need this if, the gddplant=0 after harvest and will be count only if croplive, right?
           if (croplive(i,j) .eq. 0 ) then
              gddplant(i,j)   = 0.0
              gddtsoi(i,j)    = 0.0
           endif
          
      endif
	
	     enddo
	enddo
c
c
c ---------------------------------------------------------------------- 
c * * * set daily climatic variables for entire domain * * *
c ---------------------------------------------------------------------- 
c
      do 200 i = 1, npoi
c
c ---------------------------------------------------------------------- 
c * * * other daily climate calculations * * *
c ---------------------------------------------------------------------- 
c
c calculated temperature extremes -- for vegetation limits (deg c)
c
c for this purpose, use the 10-day running mean temperature
c
        tcthis(i) = min (tcthis(i), (a10td(i) - 273.16))
        twthis(i) = max (twthis(i), (a10td(i) - 273.16))
c
c update this year's growing degree days
c

        gdd0this(i)  = gdd0this(i)  + max (0.0 ,(td(i) - 273.16)) 
        gdd0cthis(i) = gdd0cthis(i) + max (0.0 ,(td(i) - baset(15)))    ! wheat
        gdd5this(i)  = gdd5this(i)  + max (0.0 ,(td(i) - 278.16))
        gdd8this(i)  = gdd8this(i)  + max (0.0 ,(td(i) - baset(14)))    ! maize 
        gdd10this(i) = gdd10this(i) + max (0.0 ,(td(i) - baset(13)))    ! soybean
        gdd12this(i) = gdd12this(i) + max (0.0 ,(td(i) - baset(16)))    ! sugarcane 


csant*********** this is original not included here - in dailymet - 
csant*********** na verdade o gddfzcorn, e outros, nao estao mais sendo usados- 
csant quando limpar o codigo decidir se mantem ou remove isso

c form calculations of number of growing degree days between frost events
c
c events (e.g., tmin(i) .le. -2.2 C) this is applicable to CRU data
c differences exist when using a combination of CRU/NCEP  
c -2.2 used as a threshold from Schwartz and Reiter, 2000.  International
c Journal of Climatology, 20: 929-932.  Changes in North American Spring 
c       



	if(gdddy.lt.1) then !!!!update GDD (is not working for SA)


        if (tmin(i) .ge. 273.16) then
          consdays(i) = consdays(i) + 1
          maxcons(i)  = max(maxcons(i), consdays(i))
          if (maxcons(i) .eq. consdays(i)) then

            iniday(i) = cdays(i)+1 - maxcons(i)
          endif

          daygddc(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(14)))) 
          daygdds(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(13)))) 
          daygddw(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(15)))) 
        daygddsgc(i,cdays(i)) = min(30.0,max(0.0, (td(i) - baset(16)))) 
c
        else
          consdays(i) = 0
        endif
c
         if (cdays(i).eq.365) then
c
c calculate total number of growing season GDD and number
c of days long
c
             if (iniday(i) .eq. 9999) then
                 iniday(i) = cdays (i)
                 maxcons(i) = 1
	     elseif (iniday(i) .eq. 0) then
	     iniday(i)=1
             endif      
csant- for what? endday
             endday(i) = iniday(i) + maxcons(i)-1

          do 125 k = iniday(i), endday(i)

             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
csant -didnt see use            gddfzwht(i)  =  gddfzwht(i)  + daygddw(i,k)
             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
             gsdays(i) = gsdays(i) + 1 
 125      continue


        elseif (cdays(i).eq.366) then

             if (iniday(i).eq.0) then
                 iniday(i) = 1
             endif      

             endday(i) = iniday(i) + maxcons(i)-1

             gddfzcorn(i) = 0
             gddfzsoy(i)  = 0
             gddfzsgc(i) =  0
             gsdays(i) =    0


          do  k = iniday(i), endday(i)
             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
             gsdays(i) = gsdays(i) + 1 
         enddo
        endif

	endif
c***************************** fim da provavel limpesa ****

c
c accumulate growing degree days for planted crops past planting
c
        do 150 j = scpft, ecpft 
c
c for crops except winter wheat
c be careful with rotations
c 
          if (croplive(i,j) .eq. 1.0 .and. iwheat .ne. 2) then
c
             gddplant(i,j) = gddplant(i,j) + max(0.0, min(td(i)
     >                                     - baset(j), mxtmp(j)))
c
             gddtsoi(i,j)  = gddtsoi(i,j) + max(0.0, min(tsoi(i,1)
     >                                    - baset(j), mxtmp(j)))
c
c if winter wheat is planted, reduce thermal time accumulation
c by vernalization factor (calculated in crops.f) until crop
c is fully vernalized (cold-hardened)
c
          else if (croplive(i,j) .eq. 1.0 .and. j .eq. 15 .and. 
     >             iwheat .eq. 2) then
c
             gddplant(i,j) = gddplant(i,j) + vf(i) * max(0.0, min(td(i)
     >                                     - baset(j), mxtmp(j)))
c
             gddtsoi(i,j)  = gddtsoi(i,j)  +  vf(i) * max(0.0, min(tsoi(i,1)
     >                                     - baset(j), mxtmp(j)))
c
        endif


csant- nao encontrei a solucao no momento, futuramente alterar essa funcao, que pode ser importante para rodadas com alteracoes climaticas.

c          do  k = iniday(i), endday(i)
c             gddfzcorn(i) =  gddfzcorn(i) + daygddc(i,k)
c             gddfzsoy(i)  =  gddfzsoy(i)  + daygdds(i,k)
c             gddfzsgc(i) =  gddfzsgc(i)   + daygddsgc(i,k)
c             gsdays(i) = gsdays(i) + 1 
c         enddo


	if(j.eq.13.or.j.eq.14) then

	  ik(i,j)=ik(i,j)+1
	 if(idpp(i,j).eq.1) then
	 ik(i,j)=1
	endif

       if (ik(i,j).le.(mxmat(j)+5)) then

             gddfzcorn(i) =  gddfzcorn(i) + min(30.0,max(0.0, (td(i) - baset(j))))
             gddfzsoy(i)  =  gddfzsoy(i)  + min(30.0,max(0.0, (td(i) - baset(j))))

	endif


	endif
	


	if(j.eq.16) then

	  ik(i,j)=ik(i,j)+1
	 if(cropy(i).eq.1.and.idpp(i,j).eq.1) then
	 print*,'GDD_start  SUGARCANE',i,iyear,jday
         gddpl15(i)=0	
	 ik(i,j)=1
	endif

       if (ik(i,j).le.(mxmat(16)-30)) then
       gddpl15(i)= gddpl15(i) + max(0.0, min(td(i)- baset(j), mxtmp(j)))
	endif

       if (ik(i,j).eq.(mxmat(16)-30)) then
	print*,'GDD expected for planting, before',i,iyear,jday,gddsgcp(i,1),gddpl15(i),mxmat(16)-30	
       gddsgcp(i,1)= (gddsgcp(i,1)+ gddpl15(i))/2
	print*,'GDD expected for planting, updated',i,iyear,jday,gddsgcp(i,1)	
	endif

	endif
	
c
 150  continue
c
 200  continue
c
c return to main program
c
      return
      end
c
c
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
c        trans = cloud(i) / sw     ! cloud(i) is surface insolation
        trans = cloud(i) / (sw*coszen(i)) !csant - cloud is read as incidente solar radiation
	trans = max(0.,min(1.,trans))

c      if(i.eq.1)print*,jday,time,cloud(i),sw*coszen(i),trans
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
          wfrac = 0.46 + 0.08 * float(ib - 1)  !visible 0.46 and NIR 0.54
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

c	if(time.eq.13*dtime.and.i.eq.1)
csant      if(i.eq.1)print*,jday,time,solad(i,1)+solad(i,2)+solai(i,1)+solai(i,2)
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

c      if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,ta(i),ta(i)
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

csant	if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,psurf(i)

c      if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,qa(i),qa(i)
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
csant- this is observed wind -        ua(i) = max (2.5, min (10.0, ua(i)))

csant      if(i.eq.1)print*,jday,time,ua(i),ua(i)
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

c      if(time.eq.13*dtime.and.i.eq.1)print*,jday,time,psurf(i),'  1'
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

c	if(time.eq.13*dtime.and.i.eq.1)
c     >	print*,jday,time,fira(i)
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

	 if(precip(i).gt.0.0) startp=1

          if (ta(i)-273.15 .gt. 2.5) then
            raina(i) = precip(i) / plens
          else
            snowa(i) = precip(i) / plens
          endif

c      if(i.eq.1)print*,jday,time,raina(i),'  1 ',plens
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
