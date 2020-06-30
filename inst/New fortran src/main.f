c
c #    #    ##       #    #    #
c ##  ##   #  #      #    ##   #
c # ## #  #    #     #    # #  #
c #    #  ######     #    #  # #
c #    #  #    #     #    #   ##
c #    #  #    #     #    #    #
c
c ---------------------------------------------------------------
      program main
c ---------------------------------------------------------------
c
c common blocks
c 
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comdiag.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
      include 'comhour.h'
      include 'com1d.h'
      include 'comsoi.h'
      include 'comhyd.h'
      include 'combcs.h'
      include 'comsat.h'
c
c local variables
c
      integer icount,        ! number of times ran2 is called (for restart)
     >        icountdum,     !
     >        istep,         ! timestep counter (per day)
     >        iday,          ! daily loop counter
     >        imonth,        ! monthly loop counter
     >        istyrd,        ! first yr daily means exist (from precip file)
     >        istyrm,        ! first yr monthly anoms exist (from file)
     >        iwest,         ! 1st lon index for subset
     >        iy1,           ! first year for year loop
     >        iy2,           ! last year for year loop
     >        iyear,         ! yearly loop counter
     >        iyear0,        ! first year of simulation
     >        iyranom,       ! year to start reading monthly anomalies
     >        iyrlast,       ! last year of previous run (for restart)
     >        idiag,         ! number of diagnostic files requested
     >        idailyout,     ! 0: no daily output 1: daily output
     >        imonthout,     ! 0: no monthly output 1: monthly output
     >        iyearout,      ! 0: no yearly output 1: yearly output
     >        irestart,      ! 0: normal mode 1: restart mode
     >        irstyear,      ! actual calendar year that restart occurs
     >        iholdsoiln,    ! 0: don't save inorganic soil N values 1: keep inorganic soil N values 
     >        isimveg,       ! 0: static veg 1: dynam veg initialized w/ fixed
     >                       ! 2: dynam veg initialized w/ cold start
     >        isimfire,      ! 0: fixed fire  1: dynamic fire
     >        isimco2,       ! 0: fixed co2   1: changing co2
     >        jday,          ! julian day of the simulation
     >        jnorth,        ! 1st lat index for subset
     >        nanom,         ! # of years in the anomaly files
     >        niter,         ! total number of time iterations per day
     >        nday,          ! number of days since the start of the simulation
     >        ndayr,         !
     >        nrun,          ! # of years in this run
     >        nspinsoil,     ! year of simulation that soil c reaches equilibrium 
     >        plen,          ! length of precipitation event in timesteps (see plens)
     >        plenmax,       ! upper bound on plen
     >        plenmin,       ! lower bound on plen
     >        seed,          ! value used to initialize the random # generator
     >        spin,          ! counter for iterations used to spin up soilbgc
     >        spinmax,       ! maximum iterations used to spin up soilbgc
     >        eqyears,       !
     >        soilcspin,     ! 0: no spinup procedure for soil c  1: acceleration procedure used
     >        irrigate,      ! 0: no irrigation 1: irrigation used
     >        lun, ifile,    ! file indices
     >        i, j,n, ivar,    ! loop indices
     >        cropsums
c
      real    co2init,       ! initial co2 concentration in mol/mol
     >        o2init,        ! initial o2 concentration in mol/mol
     >        plens,         ! length of precipitation event in seconds
     >        startp,        ! time to start precipitation event (seconds since midnight)
     >        endp,          ! time to end precipitation event (seconds since midnight)
     >        ilens,         ! length of irrigation event in seconds
     >        starti,        ! time to start irrigation event (seconds since midnight)
     >        endi,          ! time to end irrigation event (seconds since midnight) 
     >        spincons,      ! # times soil gbc called each day during spinup
     >        spinfrac,      ! fraction of nspinsoil time with max iteration spinmax
     >        slope,         ! rate of decrease of number of iteration for soil C spinup 
     >        snorth,        ! north latitude for subsetting std grid
     >        ssouth,        ! south latitude for subsetting std grid
     >        swest,         ! west longitude for subsetting std grid
     >        seast,         ! east longitude for subsetting std grid
     >        ffact,         ! numerical multiplying factor applied to N fertilizer being applied after 2000
     >        test,          ! test on timestep 
     >        time,          ! time of day since midnight (in seconds)
     >        rn,            ! net radiation flux (SW and LW)
     >        pari,          ! incoming PAR
     >        apar,          ! APAR
     >        paro,          ! outgoing PAR
     >        swin, swout    !incoming and reflect solar radiation (W/m²) 

c
c External
c
      real ran2              ! Function : random number generator
c
c
      character*39 pathn
c number of days per month
c
      data ndaypm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data snorth, ssouth, swest, seast / 90., -90., -180., 180. /
      pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c ---------------------------------------------------------------
c                       j o b   c o n t r o l 
c ---------------------------------------------------------------
c
c open local input file called 'ibis.infile'
c and read in pertinent information
c
      lun = 12
      open (lun, status='old', file='ibis.infile')
c
      read (lun,*) irestart
      read (lun,*) irstyear
      read (lun,*) iyear0
      read (lun,*) nrun
      read (lun,*) iyranom
      read (lun,*) nanom
      read (lun,*) istyear
      read (lun,*) istend
      read (lun,*) imetyear
      read (lun,*) dmetyear
      read (lun,*) imetend
      read (lun,*) dmetend
      read (lun,*) cdcyear
      read (lun,*) soilcspin
      read (lun,*) iyearout      
      read (lun,*) imonthout
      read (lun,*) idailyout
      read (lun,*) isimveg
      read (lun,*) isimfire
      read (lun,*) isimco2
      read (lun,*) irrigate
      read (lun,*) isoybean
      read (lun,*) imaize
      read (lun,*) isgc
      read (lun,*) iwheat
      read (lun,*) ipast
      read (lun,*) gdddy
      read (lun,*) irotation
      read (lun,*) iholdsoiln
      read (lun,*) ffact
      read (lun,*) overveg
      read (lun,*) isoilay
      read (lun,*) co2init
      read (lun,*) o2init
      read (lun,*) dtime
      read (lun,*) idiag
      read (lun,*,end=99) snorth
      read (lun,*,end=99) ssouth
      read (lun,*,end=99) swest
      read (lun,*,end=99) seast
c
 99   close (lun)
c
	if(cdcyear.lt.iyear0)cdcyear=iyear0

c tell user about the simulation
c
      write (*,*) ' '
      write (*,*) '*********************************************'
      write (*,*) '* Agro-IBIS: Integrated BIosphere Simulator *'
      write (*,*) '* IBIS U.S./Agroecosystem  Version          *'
      write (*,*) '*                                           *'
      write (*,*) '* V1.0 - Chris Kucharik                     *'
      write (*,*) '* kucharik@wisc.edu                         *'
      write (*,*) '* Center for Sustainability and             *'
      write (*,*) '* the Global Environment                    *'
      write (*,*) '*                                           *'
      write (*,*) '* University of Wisconsin-Madison           *'
      write (*,*) '*                                           *'
      write (*,*) '* Sept 1, 2005                              *'
      write (*,*) '*********************************************'
      write (*,*) ' '
c
      if (irestart .eq. 1) then
        write (*,*) 'running in restart mode'
      end if
c
      write (*,*) ' '
      write (*,9000) nrun
      write (*,9005) iyranom
      write (*,*) ' '
      write (*,9010) xres, yres
      write (*,9020) nlon, nlat
      write (*,*) ' '
c
 9000 format (1x,'length of this simulation (years)   : ',i8)
 9005 format (1x,'year to begin using anomalies       : ',i8)
 9006 format (1x,'number of iterations per day        : ',i8)
 9010 format (1x,'model lon, lat resolution (degrees) : ',2f8.2)
 9020 format (1x,'model domain (nlon x nlat)          : ',i3,
     >        ' by ',i3)  
 9030 format (1x,'last year run in this sequence      : ',i8)
c
      open (222, status='unknown', file='quebragalho.dat')
      open (223, status='unknown', file='quebragalho2.dat')
      open (225, status='unknown', file='quebragalho3.dat')
      open (227, status='unknown', file='quebragalho4.dat')
      open (228, status='unknown', file='quebragalho5.dat')
      open (23, status='unknown', file='physiology.dat')
      open (27, status='unknown', file='nee.dat')

      open (40,file='output/out_hourly_tower.dat',status='unknown')
      open (41,file='output/out_daily_tower.dat',status='unknown')
      open (224, status='unknown', file='output/producao.dat')

c ---------------------------------------------------------------
c                     t i m e   c o n t r o l 
c ---------------------------------------------------------------
c
c determine the number of timesteps per day
c
      niter = int (86400.0 / dtime)
c
c test on length of the time step and total number of iteration per day
c
      test = amod (86400.0, dtime)
c
      if (test .gt. 1.e-20) then
        write (*,*) 'dtime ', dtime, ' should be divisible into 86400'  
        stop0
      else
        write (*,9006) niter
      end if
c
c check for restart conditions and set "iyrlast" accordingly, open
c ibis.annual in append mode
c
      if (irestart .eq. 1) then
        open (13, status='old', file='yearsrun.dat', err=9050)
        read (13,*) iyrlast
        close (13)
        goto 9059
 9050   write (*,*) 'warning: ibis.infile restart flag=1, indicating'
     >   //' restart, but yearsrun.dat not found'
        write (*,*) 'beginning from ', iyear0
        iyrlast = iyear0 - 1
 9059   open(20,file='ibis.out.global',status='unknown',access='append')
        open(30,file='ibis.out.vegtype',status='unknown',
     >       access='append')
      else
        iyrlast = iyear0 - 1
      end if
c
      write (*,9030) nrun + iyrlast
      write (*,*) ' '
c
c ---------------------------------------------------------------
c                   i n i t i a l i z a t i o n
c ---------------------------------------------------------------
c
c call to read in parameter files
c
      call rd_param 
c
c initial concentration of co2, o2
c
      co2conc = co2init
      o2conc  = o2init
c
c read global boundary condition datasets
c

      call readit (isimveg,snorth,ssouth,swest,seast,iwest,jnorth)

	print*,' teste de onde'
c
c check if diagnostic output is requested, if so read info from 'diag.infile'
c
      if (idiag .ne. 0) call inidiag(idiag)
c
c preliminary analysis of climate data
c
      if (irestart .eq. 0) then
        call climanl
      end if
c
c if first year of this run/restart is an anomaly year, then read in 
c anomalies for month 12 of previous year and month 1 of this year
c (note: rdanom reads imonth+1) 
c
      iy2 = iyrlast + nrun
      if (iyrlast+1 .ge. iyranom .and. (iyear .lt. imetyear .or.
     >    iyear .gt. imetend)) then
        call rdanom (11, iyrlast, iyranom, nanom, iy2, istyrm, iwest, jnorth)
        call rdanom (12, iyrlast, iyranom, nanom, iy2, istyrm, iwest, jnorth)
      end if
c
c 
      cropsums = 0
      cropsums = imaize +isoybean +iwheat +isgc +irotation 
c
c initialize the model
c
      call initial (isimveg, irestart, iyrlast)
c
c initialize crop variables
c have to be initialized when crops are replacing natural vegetation in
c a series of simulations
c
      if (cropsums .gt. 0)  call initialcrop
c
c initialize random number generator seed
c
      seed = -1
c
c ---------------------------------------------------------------
c              s t a r t   o f   t i m e   l o o p s
c ---------------------------------------------------------------
c
      write (*,*) ' '
      write (*,*) '********************************'
      write (*,*) '* start of the IBIS simulation *'
      write (*,*) '********************************'
      write (*,*) ' '
c
c reset elapsed number of days, accumulating past days if restart mode
c
      nday = 0
c
      if (irestart .eq. 1) then
        do 150 iyear = iyear0, iyrlast
          nday = nday + 365
          if (mod(iyear,4).eq.0) then
            if (mod(iyear,100).ne.0) then
              nday = nday + 1
            else if (mod(iyear/100,4).eq.0) then
              nday = nday + 1
            end if
          end if
 150    continue
      end if
      ndayr=nday
c
c start of yearly loop
c
      iy1 = iyrlast + 1
      iy2 = iyrlast + nrun
c
      do 200 iyear = iy1, iy2

csant      write(225,*) 'Corrente Year !!!!!!!!!!!!!!!!!!!! ',iyear
c
c reset julian date
c
        jday = 0
c
c determine the calendar for this year
c
c leap years occur every calendar year that is divisible by 4 (such as
c 1980, 1984, etc.) *except* for years which begin centuries not divisible
c by 400
c
c for example, 1700, 1800, and 1900 are *not* leap years, but 1600
c and 2000 *are* leap years
c
        ndaypm(2) = 28
        ndaypy = 365
        if (mod(iyear,4).eq.0) then
          if (mod(iyear,100).ne.0) then
            ndaypm(2) = 29
            ndaypy = 366
          else if (mod(iyear/100,4).eq.0) then
          ndaypm(2) = 29
          ndaypy = 366
          end if
        end if

c
c start of monthly loop
c 
         do 210 imonth = 1, 12
c
          if (iyear .lt. imetyear .or. iyear .gt. imetend) then
c           call rdanom (imonth, iyear, iyranom, nanom, iy2, istyrm, iwest, jnorth)  !csant - original for US anomaly
c	  print*,' main.f ',iyear,imonth, iwest, jnorth,nlonsub,nlatsub,npoi,nlon,nlat

	    ireccru = 0  !used in weather.f
csant - R         call rdcru2 (imonth, iyear, iyear0, iwest, jnorth)
c        call rdcru_reg (imonth, iyear, iyear0, iwest, jnorth)

csant - o ctl estava errado-  call rdcru_interpola(imonth,iyear,iyear0,iwest,jnorth)
          endif
c
c start of daily loop
c
         do 220 iday = 1, ndaypm(imonth)

c
c update user on model progress once a day
c
c            write (*,9100) iyear, imonth, iday
c 9100       format (1x,'starting - year/month/day: ',i4,'/',i2,'/',i2)
c
c update elapsed number of days and julian date
c
            nday = nday + 1
            jday = jday + 1

csant
          if (cropsums .gt. 0) then
c            update the crop calendar
csant

	  do j=1,npft
             do i=1,npoi

         if (iday.eq.pcd(i,j).and.imonth.eq.pcm(i,j).and.exist(i,j).eq.1) then
csant- start planting depending on the numbers of ratoons

	      if(nratoon.eq.4) then
c	   call startplanting (iyear0,iyear)  !important, this routine must be used only if all points are planted
         	   if(mod(i,6).eq.0.and.iyear.ge.iyear0)   ncyears(i)=ncyears(i)+1 
        	   if(mod(i,6).eq.1.and.iyear.ge.iyear0+1) ncyears(i)=ncyears(i)+1 
        	   if(mod(i,6).eq.2.and.iyear.ge.iyear0+2) ncyears(i)=ncyears(i)+1 
        	   if(mod(i,6).eq.3.and.iyear.ge.iyear0+3) ncyears(i)=ncyears(i)+1 
        	   if(mod(i,6).eq.4.and.iyear.ge.iyear0+4) ncyears(i)=ncyears(i)+1 
        	   if(mod(i,6).eq.5.and.iyear.ge.iyear0+5) ncyears(i)=ncyears(i)+1 

	     else

csant- start planting for all points at the same time
 	 	if(j.eq.16.and.pmask(i).eq.1) then
		if(iyear.gt.iyear0)   ncyears(i)=1
		else
   	        ncyears(i)=1
	        endif
            endif

c        call const (cdays, npoi, 0)
c        call const (pstart, npoi*npft, 999)

	   cdays(i)=0
	
	   pstart(i,j)=999

	   print*,iyear,jday,i,j,exist(i,j)

          endif

	  enddo
	enddo

   	do i=1,npoi
        cdays(i)=cdays(i)+1
        enddo

	endif
c***********************

c
c get daily means (if requested)
c and determine today's climatic conditions
c
c**************************** NCEP Anomaly (+ Station) ****************************
            if ( 
     > (iyear.lt.imetyear.or.iyear.gt.imetend.or. 
     >(iyear.eq.imetyear.and.jday.lt.dmetyear).or.
     >(iyear.eq.imetend.and.jday.gt.dmetend))  ) then



             if (iyear.ge.cdcyear.and.iyear.le.2006)
     >       call rdcdc (jday,imonth,iyear,cdcyear,iwest,jnorth) !CRU Data, NCEP (+ CDC prec)


            if (iyear.ge.istyear .and .iyear.le.istend ) then
              call rdstation2d  (iday,imonth,iyear, iyear0)
	    endif	


       call daily   (iyear, imonth, iday, jday, seed, 1, iyranom, iyrlast, nrun)

	else

	print*,'Stop  - no daily weather data - not using WGEN for R version' 
	print*,iyear, imonth, iday, jday
	stop


            end if

c
c determine the daily vegetation cover characteristics
c
            call pheno(jday)

c modified for croplands
c 1-17-02 CJK
c if rotation of crops is occuring, call on first day of year
csant- when start rotation irotations will be called at the beginning of the planting year! stead of normal callendar
            if (irotation .gt. 0 .and. jday .eq. 1) 
     >          call rotation(irestart, irstyear, iyear)
c
c determine if planting is able to take place for crop and location
c
      cropsums = 0
      cropsums = imaize + isoybean +iwheat +isgc + irotation 
c
      if (cropsums .gt. 0) 
     >    call planting(irestart, irstyear, iyear0,iyear,imonth,iday, jday,ffact)
c
c call daily crop phenology subroutine
c
      if (cropsums .gt. 0)
     >    call phenocrop(iyear, iyear0, imonth, iday, jday)
c
c call soil biogeochemistry model
c
c soil spin up procedure calls soil bgc model spincons times
c for each time step to spin up the process (accelerate C & N pools)
c
            spinfrac  = 0.75          ! fraction of nspinsoil time used to
c                                     ! spin up soil at maximum spin up rate
c
            spincons  = 40.0          ! number of times soilbgc subroutine is
c                                     ! called for each day in simulation during
c                                     ! acceleration procedure
c
            eqyears   = 15 !50            ! years used past spin up to slowly bring
c                                     ! entire soil to a steady equilibrium
c
            nspinsoil = iyear0 + 50 !150  ! year of simulation that soil c reaches equilibrium
c
c
            if (soilcspin .eq. 1) then

              if ((iyear - iyear0) .le.
     >            (spinfrac * (nspinsoil - iyear0 - eqyears))) then
                 spinmax = int(spincons)
c
              else if ((iyear - iyear0) .lt.
     >               (nspinsoil - iyear0 -  eqyears)) then
c
                slope   = spincons / ((nspinsoil - iyear0 - eqyears) -
     >                    (spinfrac * (nspinsoil - iyear0 - eqyears)))
c
                spinmax = int (spincons - (slope * ((iyear - iyear0) -
     >            (spinfrac * (nspinsoil - iyear0 - eqyears)))))
c
                spinmax = max(spinmax,1)
c
              else
c
                spinmax = 1
c
              endif
c
            else 
c
                spinmax = 1
c
            endif
c
            do 230 spin = 1, spinmax
              call soilbgc (iyear, iyear0, imonth, iday, jday, nspinsoil,
     >                      spin, spinmax)
 230        continue
c
c determine the length of a precipitation event (between 4 and 24 hours),
c and time to start and end precipitation event. plen is in timesteps, while
c plens, startp, and endp are in seconds
c
            plenmin = 1 +  int ((4.0 * 3600. - 1.) / dtime)
            plenmax = max (int (24.0 * 3600. / dtime), plenmin)
c
           
      if( (iyear.lt.imetyear.or.iyear.gt.imetend.or. 
     >(iyear.eq.imetyear.and.jday.lt.dmetyear).or.
     >(iyear.eq.imetend.and.jday.gt.dmetend))  ) then

csant - R              plen    = min (plenmax, int (
csant - R     >                     plenmin + ran2(seed) * (plenmax-plenmin+1) ))
              plen    = min (plenmax, int (
     >                     plenmin + 0.5 * (plenmax-plenmin+1) ))



cjk
            else
              plen = 1
            endif
c
c plen = 1 for hourly precipitation events
c 
            plens   = dtime * plen
csant - R            startp  = dtime * min (niter-plen, 
csant - R     >                             int(ran2(seed)*(niter-plen+1)))
            startp  = dtime * min (niter-plen, 
     >                             int(0.5*(niter-plen+1)))



            endp    = startp + plens
c
c calculate irrigation amount, timing, and duration
c if applicable to model run
c
c assume irrigation duration for a day is 12 hours long between 
c 6 am and 6 pm local time - this might be changed if managed irrigation can
c take place at night (e.g. crops)  
c
             ilens  = dtime * (12.0 * 3600. / dtime)   
             starti = dtime * (6.0  * 3600. / dtime)
             endi   = starti + ilens 
c
c only call irrigation if turned on and crops are planted
c
             if (irrigate .eq. 1) then 
c
               call irrigation(iday,imonth) 
c
             endif
c
c **********   start of hourly loop    *********
c
            do 240 istep = 1, niter
c
c calculate the time of day since midnight (in seconds)
c
              time = (istep - 1) * dtime
c
c determine climatic conditions for the given timestep
c also apply irrigated water
c
cjk 
c
c        if((iyear.ge.imetyear.and.jday.ge.dmetyear).and.iyear.le.imetend)then
csant                call inimet (iyear,imonth,iday,nday,time,seed)
c	 if(iyear.eq.imetend.and.jday.gt.dmetend) goto 144

       if ( (iyear.eq.imetyear.and.jday.ge.dmetyear) .or.
     >	    (iyear.gt.imetyear.and.iyear.lt.imetend) .or.
     >      (iyear.eq.imetend .and.jday.le.dmetend)     ) then 

	  
c                call methourly (iyear,jday,time,seed)
                call methourlyfays (iyear,jday,time,seed)

                call diurnalmet(time, jday, plens, startp, endp, seed,
     >                      irrigate, ilens, starti, endi)
	      else

c 144  continue
                call diurnal (time, jday, plens, startp, endp, seed,
     >                      irrigate, ilens, starti, endi)
              endif


c
c -------------------------------------------------------------------
c added for Green-Ampt infiltration model
              t_sec = time
              t_startp = startp
c -------------------------------------------------------------------
c call the land surface model
c
              call lsxmain(time,jday)
c
c accumulate some variables every timestep
c
              call sumnow
c
              call sumday   (istep, plens, iyear, jday)          
              call summonth (istep, iday, imonth)
              call sumyear  (istep, iday, imonth)
c
c
c call to nitrogen stress routine for crops
c 
         call nitrostress(istep, iday,imonth)
c       
         call leaching(irestart, irstyear, istep,iday,imonth,iyear,
     >                 iholdsoiln, iyear0)
c
c write out diagnostics
c
              if (idiag .ne. 0) then
c
                do 250 i = 1, idiag
                  if (iyear.ge.diagstart(i) .and.
     >                iyear.le.diagend(i)   .and. 
     >                ndiagpt(i).ge.1)  then  
                    if (mod(istep,nfreq(i)).eq.0) then
                      call wdiag (i, iyear, imonth, iday, istep)
                    end if
                  end if
 250            continue
c  
              end if
c  
c write out hourly canopy photosynthesis rates
c 
c           open(33,file='canopy.ps.dat',status='unknown')
c           if (iyear .eq. 1998 .and. (imonth .gt. 3 .and. imonth .lt. 10)) then
c              write(33,270) iyear, imonth, iday, istep, ancub(4)*1.e+06,
c     >        ancuc(4)*1.e+06, totlaiu(4), plai(4,5), lai(4,2)
c              write(33,270) iyear, imonth, iday, istep, agcc4(4)*1.e+06,
c     >        ancc4(4)*1.e+06, plai(4,4), plai(4,5), plai(4,11), plai(4,12)
c               write(33,270) iyear, imonth, iday, istep, ancc4(4)*1.e+06*totlail(4),
c     >                       lai(4,1), frac(4,14), fl(4) 
c            endif
c 270        format(i5,i3,i3,i4,4f7.2)
 
c
csant-  write out vars to validate agains flux tower (FOR THE POINT 1, TOP LEFT GRID POINT)
c energy budget of the surface

               rn = solad(1,1) * (1 - asurd(1,1)) + solad(1,2) * (1 -
     >           asurd(1,2)) + solai(1,1) * (1 - asuri(1,1)) +
     >           solai(1,2) * (1 - asuri(1,2)) + fira(1) - firb(1)

	swin = solad(1,1) + solad(1,2) + solai(1,1) + solai(1,2)

	swout= solad(1,1) * asurd(1,1) + solad(1,2) * asurd(1,2) 
     >       + solai(1,1) * asuri(1,1) + solai(1,2) * asuri(1,2)

c
c              pari = (solad(1,1) + solai(1,1)) * 4.59e-06   !to convert from W/m^2 to mol of Photons/m^2*s
              pari = (solad(1,1) + solai(1,1)) * 4.59        !to convert from W/m^2 to micro_mol of Photons/m^2*s
c
c              apar = (topparl(1) + topparu(1)) * 4.59e-06   !to convert from W/m^2 to mol de Photons/m^2*s
              apar = (topparl(1) + topparu(1)) * 4.59        !to convert from W/m^2 to micro_mol of Photons/m^2*s
c
	              
              paro = (solad(1,1) * asurd(1,1) + solai(1,1) * asuri(1,1))* 4.59 ! W/m^2 to micro_mol of Photons/m^2*s


          if((iyear.ge.(iyear0+3)).and.(iyear.le.(iyear0+6))) then
 
             write(40,9200) iyear, jday, istep-1, 
     >        swin, swout, pari, paro,  apar, rn,-fsena(1),-fvapa(1)*hvap,-soihfl(1),!solsoi(1)
     >          -tneetot(1)*1e6            ,plai(1,16)      !tneetot em  micro mol_CO2.m-2.s-1
	

		if(istep-1.ge.8.and.istep-1.le.18) then

	  write(228,*)jday,plai(1,16), - fvapa(1)*(dtime),-tneetot(1)*1e6,tl(1)-273.16,td(1)-273.18

	 use(4,istep-1)=-tneetot(1)*1e6   !- fvapa(1)*(dtime)
	
		elseif(istep-1.eq.19) then
        if(plai(1,14)*grnfraccrop(1,14).gt.0.1)
     >	write(27,172)iyear,jday,idpp(1,14),idpe(1,14),plai(1,14)*grnfraccrop(1,14)
     > ,use(4,8),use(4,9),use(4,10),use(4,11),use(4,12)
     > ,use(4,13),use(4,14),use(4,15),use(4,16),use(4,17),use(4,18)

		endif

 172    format (2(i4,1x),16(1x,f6.2))	



csant- writing daily values.
c        use(5,istep)=(rn+swout-swin)!tsoi(1,1)-273.16!-soihfl(1)!rn!-fvapa(1)*hvap!-fsena(1)!paro/pari!-tneetot(1)*1e6!
c	   if(istep.eq.24) then   !istep eq. 24 is 23h
c
c       write(29,182)jday,plai(1,16),
c     >use(5,1),use(5,2),use(5,3),use(5,4),use(5,5),use(5,6),use(5,7),use(5,8),use(5,9),use(5,10),use(5,11)
c     >   , use(5,12),use(5,13),use(5,14)   !continuacao
c     >,use(5,15),use(5,16),use(5,17),use(5,18),use(5,19),use(5,20),use(5,21),use(5,22),use(5,23),use(5,24)
c 182    format (1x,i3,1x,f4.2,24(1x,f7.2))




	endif
c  
 9200         format (3(i4,','),10(f9.3,','),f3.1)       
	
             flx(1)= flx(1) + swin   *(dtime/10**6)  !passando de W/m2 para MJ/dia
             flx(2)= flx(2) + swout  *(dtime/10**6)  !passando de W/m2 para MJ/dia

c             flx(3)= flx(3) + pari   * 4.59        !to convert from W/m^2 to micro_mol of Photons/m^2*s
c             flx(4)= flx(4) + paro   * 4.59        !to convert from W/m^2 to micro_mol of Photons/m^2*s
c             flx(5)= flx(5) + apar   * 4.59        !to convert from W/m^2 to micro_mol of Photons/m^2*s

c             flx(3)= flx(3) + pari   * 4.59e-06   !to convert from W/m^2 to mol of Photons/m^2*s
c             flx(4)= flx(4) + paro   * 4.59e-06   !to convert from W/m^2 to mol of Photons/m^2*s
c             flx(5)= flx(5) + apar   * 4.59e-06   !to convert from W/m^2 to mol of Photons/m^2*s

             flx(3)= flx(3) + pari   *(dtime/10**6) !passando de W/m2 para MJ/dia
             flx(4)= flx(4) + paro   *(dtime/10**6) !passando de W/m2 para MJ/dia
             flx(5)= flx(5) + apar   *(dtime/10**6) !passando de W/m2 para MJ/dia

c

             flx(6)= flx(6) + rn   *(dtime/10**6)

             flx(7)= flx(7) - fsena(1)  *(dtime/10**6)

             flx(8)= flx(8) - fvapa(1)*hvap *(dtime/10**6)

             flx(9) = flx(9) - soihfl(1)*(dtime/10**6)
		
             flx(10) = flx(10) -tneetot(1)*1e6 
csant - should be *1e6*0.432 to convert from mol_CO2.m-2.s-1 to Kg_C.ha-1.dtime or *43.2 to Kg_C.m-2.s-1    

             flx(11) = flx(11)+ solsoi(1)   *(dtime/10**6)

             flx(12) = flx(12)+ gsuvap(1) * dtime
             flx(13) = flx(13)+ gtrans(1) * dtime

             flx(14) = flx(14)+ tsoi(1,1) - 273.18
             flx(15) = flx(15)+ tsoi(1,2) - 273.18 
             flx(16) = flx(16)+ tsoi(1,5) - 273.18
	
             flx(17) = flx(17)+ wsoi(1,2)
             flx(18) = flx(18)+ wsoi(1,4)
             flx(19) = flx(19)+ wsoi(1,6)
             flx(20) = flx(20)+ wsoi(1,8)
             flx(21) = flx(21)+ wsoi(1,10)

             
c
c ---------------------------------------------------------------
c               e n d   o f   t i m e   l o o p s
c ---------------------------------------------------------------
c
c end of the hourly loop
c
 240        continue



	do i=10,21
	flx(i) = flx(i)/24.
	enddo

c
c	if(iyear.ge.2004) then
       if (iyear.ge.imetyear.and.iyear.le.imetend) then 


      print*,iyear,jday, cbior(1,16)/cfrac(16),cbiol(1,16)/cfrac(16),
     > cbios(1,16)/cfrac(16),cbiog(1,16)/cgrain(16)

	write(41,9201)iyear,jday,flx(1),flx(2),flx(3),flx(4),
     > flx(5),flx(6),flx(7),flx(8),flx(9),flx(11),flx(10),
     > plai(1,16)*grnfraccrop(1,16),plai(1,16),
     > cbior(1,16)/cfrac(16),cbiol(1,16)/cfrac(16),
     > cbios(1,16)/cfrac(16),cbiog(1,16)/cgrain(16), flx(12),flx(13),
     > flx(14),flx(15),flx(16),flx(17),flx(18),flx(19),flx(20),flx(21)



	endif
9201    format (i4,',',i4,10(',',f5.1),',',f7.3,2(',',f5.2)
     > ,4(',',f5.3),5(',',f5.1),5(',',f5.3))  




             flx(1)= 0.0 
             flx(2)= 0.0
             flx(3)= 0.0
             flx(4)= 0.0
             flx(5)= 0.0
             flx(6)= 0.0
             flx(7)= 0.0
             flx(8)= 0.0
             flx(9)= 0.0
             flx(10)= 0.0     
             flx(11)= 0.0     
             flx(12)= 0.0     
             flx(13)= 0.0     
             flx(14)= 0.0     
             flx(15)= 0.0     
             flx(16)= 0.0     
             flx(17)= 0.0     
             flx(18)= 0.0     
             flx(19)= 0.0     
             flx(20)= 0.0     
             flx(21)= 0.0    
c write out daily output
c
            if (idailyout.eq.1) then
c             write (*,*) ' '
c             write (*,*) 'writing daily output'
c             write (*,*) ' '
              call wdaily (nday, iyear, iyear0, ndayr, jday, irstyear) 
            endif
c
c end of the daily loop
c
 220      continue
c
c write out monthly output (use 1st day of this month because of GrADS bug)
c
          if (imonthout.eq.1) then
csant            write (*,*) ' '
csant            write (*,*) 'writing monthly output'
csant            write (*,*) ' '
            call wmonthly (nday-ndaypm(imonth)+1, imonth, iyear, 
     >                     iyear0, irstyear, jday)
          endif
c
c end of the monthly loop
c
 210    continue
c
c	write(222,*)'   year ',iyear
	write(225,*)'   year ',iyear
	write(227,*)'   year ',iyear
	write(23,*)'   year ',iyear



c get new o2 and co2 concentrations for this year
c
        if (isimco2.eq.1) call co2 (co2init, co2conc, iyear)
c
c perform vegetation dynamics
c
        if (isimveg.ne.0) call dynaveg (isimfire)
c
c calculate simple annual diagnostic variables for the globe
c
        call gdiag (iyear, iyear0)
c
c calculate simple annual diagnostic variables for each vegetation type
c
        call vdiag (iyear, iyear0)
c
c write out annual output
c
        if (iyearout.eq.1) then
c         write (*,*) ' '
c         write (*,*) 'writing annual output'
c         write (*,*) ' '
          call wyearly (nday, iyear, iyear0, irstyear)
        endif
c
c write restart files
c
        call wrestart (nday, iyear, iyear0)
c
c update iyrlast value and file yearsrun.dat
c
        iyrlast = iyrlast + 1
        open(12,file='yearsrun.dat',status='unknown')
        write(12,*) iyrlast
        close(12)
c
c end of the yearly loop
c
 200  continue
c
c end of the simulation
c
      write (*,*) ' '
      write (*,*) '*** end of run ***'
      write (*,*) ' '
c
      stop0
      end
c
c
c ---------------------------------------------------------------
      subroutine lsxmain(time,jday)
c ---------------------------------------------------------------
c
c common blocks
c 
      include 'implicit.h'
c
      include 'compar.h'
      include 'com1d.h'
      include 'comatm.h'
      include 'comsoi.h'
c
c Local variables
c
      integer ib,   ! waveband number (1= visible, 2= near-IR)
     >        i,    ! loop indice
     >        iday,jday,
     >        imonth,
     >        iyear,
     >        iyear0

	real time
c
c ---------------------------------------------------------------
c added for Green-Ampt infiltration model
c      call initsw
c ---------------------------------------------------------------
c set physical soil quantities
c
      call setsoi
c
c calculate areal fractions wetted by intercepted h2o
c
      call fwetcal
c
c set up for solar calculations
c
      call solset
c
c solar calculations for each waveband
c
      do 100 ib = 1, nband
c
c solsur sets surface albedos for soil and snow
c solalb performs the albedo calculations
c solarf uses the unit-incident-flux results from solalb
c to obtain absorbed fluxes sol[u,s,l,g,i] and 
c incident pars sunp[u,l]
c
        call solsur (ib)
        call solalb (ib)
        call solarf (ib)
c
 100  continue
c
c calculate ir fluxes
c
      call irrad
c
c step intercepted h2o
c
      call cascade
c
c re-calculate wetted fractions, changed by cascade
c
      call fwetcal
c
c step vegetation canopy temperatures implicitly
c and calculate sensible heat and moisture fluxes
c
      call canopy(time,jday)
c
c step intercepted h2o due to evaporation
c
      call cascad2
c
c arbitrarily set veg temps & intercepted h2o for no-veg locations
c
      call noveg
c
c set net surface heat fluxes for soil and snow models
c
      do 110 i = 1, npoi
c
        heatg(i) = solg(i) + firg(i) - fseng(i) -
     >             hvasug(i)*fvapg(i)
c
        heati(i) = soli(i) + firi(i) - fseni(i) -
     >             hvasui(i)*fvapi(i)
c
 110  continue
c
c step snow model
c
      call snow
c
c step soil model
c
      call soilctl
c
c return to main program
c
      return
      end
c 
