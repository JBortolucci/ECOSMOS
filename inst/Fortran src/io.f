c io.f  last update 11.06.03 C. Kucharik
c       for new climate datasets
c
c    #     ####
c    #    #    #
c    #    #    #
c    #    #    #
c    #    #    #
c    #     ####
c This file contains these subroutines
c wrestart
c wdaily
c wmonthly
c wyearly
c readit
c restart
c coldstart
c rdanom
c inird
c rdday
c diaginit
c wdiag
c
c See end of file for information on how to add new code to read a
c file or write a file.
c
c ---------------------------------------------------------------------
      subroutine wrestart (nday, iyear, iyear0)
c ---------------------------------------------------------------------
c
c this subroutine writes the restart values of:
c
c  fsnocov = fractional snow cover
c  tsno    = temperature of snow
c  hsno    = snow depth
c  tsoi    = soil temperature
c  wisoi   = soil ice content
c  wsoi    = soil moisture content
c  smsoil  = immobile inorganic nitrogen in soil
c  smsoln  = mobile inorganic nitrogen in soil
c  cbiol   = carbon in leaf biomass pool
c  cbiow   = carbon in woody biomass pool
c  cbior   = carbon in fine root biomass pool
c  cnroot  = c/n ratio of crop roots
c  cntops  = c/n ratio of tops of crop plants
c  sapfrac = sapwood fraction
c  decompl = litter decomposition factor
c  decomps = soil decomposition factor
c  clitlm  = leaf metabolic litter
c  clitls  = leaf structural litter
c  clitll  = leaf lignin litter
c  clitrm  = root metabolic litter
c  clitrs  = root structural litter
c  clitrl  = root lignin litter
c  clitwm  = woody metabolic litter
c  clitws  = woody structural litter
c  clitwl  = woody lignin litter
c  falll   = annual leaf litterfall
c  fallr   = annual fine root turnover 
c  fallw   = annual wood litterfall
c  totcmic = total microbial carbon
c  csoislop= slow soil carbon, protected humus
c  csoislon= slow soil carbon, nonprotected humus
c  csoipas = passive soil carbon
c  aplantn = available plant nitrogen
c  gdd0    = growing degree days 0
c  gdd0c   = growing degree days 0 for wheat between April 1 and Sept 30
c  gdd5    = growing degree days 5
c  gdd8    = growing degree days 8 
c  gdd10   = growing degree days 10 
c  gdd12   = growing degree days 12 
c  tc      = coldest monthly temperature
c  tw      = warmest monthly temperature
c  wipud   = ice content of puddles per soil area
c  wpud    = liquid content of puddles per soil area
c  agddu   = annual accumulated growing degree days for bud burst, upper canopy
c  agddl   = annual accumulated growing degree days for bud burst, lower canopy
c  tempu   = cold-phenology trigger for trees
c  templs  = cold-phenology trigger for shrubs
c  greenfracl3 = fraction of green vegetation in C3 grasses
c  greenfracl4 = fraction of green vegetation in C4 grasses
c  Tavgann = average annual air temperature (purely from climatology)
c  PPTavgann = average annual precipitation (purely from climatology)
c  a10td    = 10-day avg daily temp
c  a10ts    = 10-day avg daily soil (1) temp
c  a10ancub = 10-day average canopy photosynthesis rate - broadleaf
c  a10ancuc = 10-day average canopy photosynthesis rate - conifers
c  a10ancls = 10-day average canopy photosynthesis rate - shrubs
c  a10ancl4 = 10-day average canopy photosynthesis rate - c4 grasses
c  a10ancl3 = 10-day average canopy photosynthesis rate - c3 grasses
c  a10scalparamu = 10-day average canopy scaling parameter - upper canopy
c  a10scalparaml = 10-day average canopy scaling parameter - lower canopy
c  a10daylightu = 10-day average daylight - upper canopy
c  a10daylightl = 10-day average daylight - lower canopy
c  a11soiltd = 11-day average surface soil temperature
c  a3tdmin = 3-day average daily minimum air temperature
c (NOTE: a10ancuc is not used at this point, so its wrestart entry 
c is commented out)
c ---------------------------------------------------------------------
c
      include 'implicit.h'
c
c instantaneous output for restarts
c
      include 'compar.h'
      include 'comsoi.h'
      include 'comsno.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer nday,         ! number of days run since iyear0
     >        iyear,        ! this calendar year
     >        iyear0        ! initial year

c
c local variables
c
      integer lf,           ! number of characters in directory name
     >        n, k,         ! loop indices
     >        idies,        ! file indice (?) for netcdf
     >        istat,        ! error flag for netcdf      
     >        nyears        ! years of run iyear0
c
      integer istart(4),
     >        icount(4)         ! for writing restart vars
c
      character*21 tunits
      character*20 fdir         ! used to construct odd/even file names
      character*10 cdate        ! date to use in history attribute in files
      character*10 tdate        ! character date for time step
      character*80 dimnames(4)  ! names of dimensions for restart vars
      character*80 pftdef(npft) ! plant functional type defs (not used now)
      character*80 filen        ! file name
c
      real slayers(nsnolay),    ! index for snow layers
     >     depthsoi(nsoilay),   ! soil layer depths
     >     pindex(npft),        ! index for pfts
     >     ftime,               ! floating point time value
     >     tweight              ! time weight (# days/sample)
c
c External
c
      integer lenchr,           ! Function: Find length of character string
     > NF_PUT_ATT_TEXT,         ! netcdf function
     > NF_GLOBAL                ! netcdf function
c
c use workspace for local real variables
c
      equivalence (slayers(1),work(1)),(depthsoi(1),work(ndim4+1))
      equivalence (ftime,work(2*ndim4+1)), (tweight,work(3*ndim4+1))
      equivalence (pindex(1),work(4*ndim4+1))
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, 
     >     icount / nlon,nlat,1,1 /
      icount(1) = nlonsub
      icount(2) = nlatsub
c
      tweight = 1.
c
c check to see if iyear is odd or even, construct appropriate file name root
c
      if (mod(iyear,2) .eq. 0) then
         fdir = 'restart/even'
      else
         fdir = 'restart/odd'
      end if
      lf = lenchr(fdir)
c
c tdate is december of this year, max year = 999
c
      tdate='DEC000'//char(0)//char(0)//char(0)//char(0)
      nyears = iyear - iyear0 + 1
      if (nyears .lt. 10) then
         write(tdate(6:6),'(i1)') nyears
       else if (nyears .lt. 100) then
         write(tdate(5:6),'(i2)') nyears
      else
         write(tdate(4:6),'(i3)') nyears
      end if
c
c initialize snow layer indicies, pft names, etc
c
      if (nyears .le. 2) then
c         call date(cdate)
         ftime = nday
c
c time units is days since Dec 31 of the year before iyear0
c
         tunits = 'days since 0000-12-31'
         write(tunits(12:15),'(i4)') iyear0-1
c
         do 5 n = 1, nsnolay
            slayers(n) = float(n)
 5       continue
c
         depthsoi(1) = hsoi(1)
         do 10 n = 2, nsoilay
            depthsoi(n) = depthsoi(n-1)+hsoi(n)
 10      continue
c
c define pft by index
c
         do 12 n = 1, npft
            pindex(n) = n
 12      continue
c
c and by character label
c
         pftdef(1) = 'trbrevtr - tropical broadleaf evergreen trees'
     >    //char(0)
         pftdef(2) = 
     >    'trbrdetr - tropical broadleaf drought-deciduous trees'
     >    //char(0)
         pftdef(3) = 
     >    'wtbrevtr - warm-temperate broadleaf evergreen trees'
     >    //char(0)
         pftdef(4) = 'tecoevtr - temperate conifer evergreen trees'
     >    //char(0)
         pftdef(5) = 
     >    'tebrdetr - temperate broadleaf cold-deciduous trees'//char(0)
         pftdef(6) = 'bocoevtr - boreal conifer evergreen trees'
     >    //char(0)
         pftdef(7) = 'bocodetr - boreal conifer cold-deciduous trees'
     >    //char(0)
         pftdef(8) = 
     >    'bobrdetr - boreal broadleaf cold-deciduous trees'//char(0)
         pftdef(9) = 'evsh - evergreen shrubs'//char(0)
         pftdef(10) = 'desh - deciduous shrubs'//char(0)
         pftdef(11) = 'c4gr - warm (c4) grasses'//char(0)
         pftdef(12) = 'c3gr - cool (c3) grasses'//char(0)
         pftdef(13) = 'c3 crop - soybean'//char(0)
         pftdef(14) = 'c4 crop - maize'//char(0)
         pftdef(15) = 'c3 crop - wheat'//char(0)
         pftdef(16) = 'c4 crop - sugarcane'//char(0)
c
         dimnames(1) = 'longitude'
         dimnames(2) = 'latitude'
c
c dimnames(3) is set for each variable seperately
c
         dimnames(4) = 'time'
      end if
cc
cc dummy variable example, 3-d - copy & modify for new variable.
cc If new variable is 4-d, see tsno to see how 4-d differs from 3-d.
cc
c      filen = fdir(1:lf)//'dummyv.nc'
c      if (nyears .le. 2) then
c         call inifile(idies,filen,
c     >    'restart file for dummyv',
c     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'dummyv',
c     >    'instantaneous dummyv','dummyvs-units',3,dimnames,
c     >    OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (dummyv, cdummy)
c      icount(3) = 1
c      call writevar(filen,'dummyv',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wrestart, dummyv'
c         stop 1
c      end if
c
c fractional snow cover
c
      filen = fdir(1:lf)//'fsnocov.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for fractional snow cover',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'fsnocov',
     >    'instantaneous fractional snow cover','fraction',3,dimnames,
     >    OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (fi, cdummy)
      icount(3) = 1
      call writevar(filen,'fsnocov',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, fsnocov'
         stop 1
      end if
c
c temperature of snow layers
c
      filen = fdir(1:lf)//'tsno.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for snow temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'snowlayer','snow layers top to bottom','',nsnolay,slayers,
     >    'down',tunits,'gregorian',istat)
         dimnames(3) = 'snowlayer'
         call inivar(idies,'tsno',
     >    'instantaneous snow cover temperature','degK',4,dimnames,
     >    OCEAN,istat)
         call endini(idies,istat)
      end if
      do 15 k = 1, nsnolay
         call vec2arr (tsno(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 15   continue
      icount(3) = nsnolay
      call writevar(filen,'tsno',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, tsno'
         stop 1
      end if
c
c thickness of snow layers
c
      filen = fdir(1:lf)//'hsno.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for snow layer thickness',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'snowlayer','snow layers top to bottom','',nsnolay,slayers,
     >    'down',tunits,'gregorian',istat)
         dimnames(3) = 'snowlayer'
         call inivar(idies,'hsno','instantaneous snow layer thickness',
     >    'meters',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 20 k = 1, nsnolay
         call vec2arr (hsno(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 20   continue
      icount(3) = nsnolay
      call writevar(filen,'hsno',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, hsno'
         stop 1
      end if
c
c temperature of soil layers
c
      filen = fdir(1:lf)//'tsoi.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for soil temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'soillayer','depth of soil layer bottom','meter',nsoilay,
     >    depthsoi,'down',tunits,'gregorian',istat)
         dimnames(3) = 'soillayer'
         call inivar(idies,'tsoi','instantaneous soil temperature',
     >    'degK',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 25 k = 1, nsoilay
         call vec2arr (tsoi(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 25   continue
      icount(3) = nsoilay
      call writevar(filen,'tsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, tsoi'
         stop 1
      end if
c
c ice content of soil
c
      filen = fdir(1:lf)//'wisoi.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for soil ice content',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'soillayer','depth of soil layer bottom','meter',nsoilay,
     >    depthsoi,'down',tunits,'gregorian',istat)
         dimnames(3) = 'soillayer'
         call inivar(idies,'wisoi',
     >    'instantaneous fraction of soil pore space containing ice',
     >    'fraction',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 30 k = 1, nsoilay
         call vec2arr (wisoi(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 30   continue
      icount(3) = nsoilay
      call writevar(filen,'wisoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, wisoi'
         stop 1
      end if
c
c water content of soil
c
      filen = fdir(1:lf)//'wsoi.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for soil water content',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'soillayer','depth of soil layer bottom','meter',nsoilay,
     >    depthsoi,'down',tunits,'gregorian',istat)
         dimnames(3) = 'soillayer'
         call inivar(idies,'wsoi',
     >    'instantaneous fraction of soil pore space containing water',
     >    'fraction',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 35 k = 1, nsoilay
         call vec2arr (wsoi(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 35   continue
      icount(3) = nsoilay
      call writevar(filen,'wsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, wsoi'
         stop 1
      end if
c
c immobile inorganic nitrogen in soil - available to plants, but not leachable 
c
      filen = fdir(1:lf)//'smsoil.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for immobile soil inorganic nitrogen',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'soillayer','depth of soil layer bottom','meter',nsoilay,
     >    depthsoi,'down',tunits,'gregorian',istat)
         dimnames(3) = 'soillayer'
         call inivar(idies,'smsoil',
     >    'instantaneous immobile inorganic soil nitrogen',
     >    'kg/m2',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 36 k = 1, nsoilay
         call vec2arr (smsoil(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 36   continue
      icount(3) = nsoilay
      call writevar(filen,'smsoil',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, smsoil'
         stop 1
      end if
c
c mobile inorganic nitrogen in soil - available to plants, and potentially leachable 
c
      filen = fdir(1:lf)//'smsoln.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for leachable soil inorganic nitrogen',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'soillayer','depth of soil layer bottom','meter',nsoilay,
     >    depthsoi,'down',tunits,'gregorian',istat)
         dimnames(3) = 'soillayer'
         call inivar(idies,'smsoln',
     >    'instantaneous mobile inorganic soil nitrogen',
     >    'kg/m2',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 37 k = 1, nsoilay
         call vec2arr (smsoln(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 37   continue
      icount(3) = nsoilay
      call writevar(filen,'smsoln',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, smsoln'
         stop 1
      end if
c
c carbon in leaf biomass pool
c
      filen = fdir(1:lf)//'cbiol.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for carbon in leaf biomass pool',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant functional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'cbiol',
     >    'instantaneous carbon in leaf biomass pool','kg/m^2',
     >    4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 40 k = 1, npft
         call vec2arr (cbiol(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 40   continue
      icount(3) = npft
      call writevar(filen,'cbiol',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, cbiol'
         stop 1
      end if
c
c carbon in wood
c
      filen = fdir(1:lf)//'cbiow.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for carbon in wood biomass pool',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant functional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'cbiow',
     >    'instantaneous carbon in wood biomass pool','kg/m^2',
     >    4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 45 k = 1, npft
         call vec2arr (cbiow(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 45   continue
      icount(3) = npft
      call writevar(filen,'cbiow',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, cbiow'
         stop 1
      end if
c
c carbon in root
c
      filen = fdir(1:lf)//'cbior.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for carbon in root biomass pool',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant functional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'cbior',
     >    'instantaneous carbon in root biomass pool','kg/m^2',
     >    4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 50 k = 1, npft
         call vec2arr (cbior(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 50   continue
      icount(3) = npft
      call writevar(filen,'cbior',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, cbior'
         stop 1
      end if
c
c c/n ratio of crop roots 
c
      filen = fdir(1:lf)//'cnroot.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for cn ratio of crop roots',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant functional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'cnroot',
     >    'end of year cn ratio of crop roots','dimensionless',
     >    4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 52 k = 1, npft
         call vec2arr (cnroot(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 52   continue
      icount(3) = npft
      call writevar(filen,'cnroot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, cnroot'
         stop 1
      end if
c
c c/n ratio of tops of crop plants 
c
      filen = fdir(1:lf)//'cntops.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for cn ratio of crop tops',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant functional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'cntops',
     >    'end of year cn ratio of crop tops','dimensionless',
     >    4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 54 k = 1, npft
         call vec2arr (cntops(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 54   continue
      icount(3) = npft
      call writevar(filen,'cntops',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, cntops'
         stop 1
      end if
c
c sapwood fraction
c
      filen = fdir(1:lf)//'sapfrac.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for sapwood fraction',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'sapfrac','instantaneous sapwood fraction',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (sapfrac, cdummy)
      icount(3) = 1
      call writevar(filen,'sapfrac',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, sapfrac'
         stop 1
      end if
c
c litter decomposition factor 
c
      filen = fdir(1:lf)//'decompl.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for litter decomposition factor',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'decompl',
     >    'litter decomposition factor','dimensionless',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (decompl, cdummy)
      icount(3) = 1
      call writevar(filen,'decompl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, decompl'
         stop 1
      end if
c
c soil decomposition factor 
c
      filen = fdir(1:lf)//'decomps.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for soil decomposition factor',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'decomps',
     >    'soil decomposition factor','dimensionless',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (decomps, cdummy)
      icount(3) = 1
      call writevar(filen,'decomps',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, decomps'
         stop 1
      end if
c
c leaf metabolic litter
c
      filen = fdir(1:lf)//'clitlm.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for leaf metabolic litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitlm',
     >    'instantaneous leaf metabolic litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitlm, cdummy)
      icount(3) = 1
      call writevar(filen,'clitlm',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitlm'
         stop 1
      end if
c
c leaf structural carbon litter
c
      filen = fdir(1:lf)//'clitls.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for leaf structural litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitls',
     >    'instantaneous leaf structural litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitls, cdummy)
      icount(3) = 1
      call writevar(filen,'clitls',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitls'
         stop 1
      end if
c
c leaf lignin carbon litter
c
      filen = fdir(1:lf)//'clitll.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for leaf lignin litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitll',
     >    'instantaneous leaf lignin litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitll, cdummy)
      icount(3) = 1
      call writevar(filen,'clitll',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitll'
         stop 1
      end if
c
c root metabolic litter
c
      filen = fdir(1:lf)//'clitrm.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for root metabolic litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitrm',
     >    'instantaneous root metabolic litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitrm, cdummy)
      icount(3) = 1
      call writevar(filen,'clitrm',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitrm'
         stop 1
      end if
c
c root structural litter
c
      filen = fdir(1:lf)//'clitrs.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for root structural litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitrs',
     >    'instantaneous root structural litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitrs, cdummy)
      icount(3) = 1
      call writevar(filen,'clitrs',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitrs'
         stop 1
      end if
c
c root lignin litter
c
      filen = fdir(1:lf)//'clitrl.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for root lignin litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitrl',
     >    'instantaneous root lignin litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitrl, cdummy)
      icount(3) = 1
      call writevar(filen,'clitrl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitrl'
         stop 1
      end if
c
c woody metabolic litter
c
      filen = fdir(1:lf)//'clitwm.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for woody metabolic litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitwm',
     >    'instantaneous woody metabolic litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitwm, cdummy)
      icount(3) = 1
      call writevar(filen,'clitwm',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitwm'
         stop 1
      end if
c
c woody structural litter
c
      filen = fdir(1:lf)//'clitws.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for woody structural litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitws',
     >    'instantaneous woody structural litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitws, cdummy)
      icount(3) = 1
      call writevar(filen,'clitws',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitws'
         stop 1
      end if
c
c woody lignin litter
c
      filen = fdir(1:lf)//'clitwl.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for woody lignin litter',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'clitwl',
     >    'instantaneous woody lignin litter carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (clitwl, cdummy)
      icount(3) = 1
      call writevar(filen,'clitwl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, clitwl'
         stop 1
      end if
c
c annual leaf litterfall
c
      filen = fdir(1:lf)//'falll.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for annual leaf litterfall',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'falll',
     >    'annual leaf litterfall carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (falll, cdummy)
      icount(3) = 1
      call writevar(filen,'falll',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, falll'
         stop 1
      end if
c
c annual fine root turnover 
c
      filen = fdir(1:lf)//'fallr.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for annual fine root turnover',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'fallr',
     >    'annual fine root turnover carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (fallr, cdummy)
      icount(3) = 1
      call writevar(filen,'fallr',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, fallr'
         stop 1
      end if
c
c annual wood turnover 
c
      filen = fdir(1:lf)//'fallw.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for annual woody turnover',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'fallw',
     >    'annual wood turnover carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (fallw, cdummy)
      icount(3) = 1
      call writevar(filen,'fallw',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, fallw'
         stop 1
      end if
c
c total microbial carbon
c
      filen = fdir(1:lf)//'totcmic.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for total microbial carbon',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'totcmic',
     >    'instantaneous total microbial carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (totcmic, cdummy)
      icount(3) = 1
      call writevar(filen,'totcmic',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, totcmic'
         stop 1
      end if
c
c slow soil carbon, protected humus
c
      filen = fdir(1:lf)//'csoislop.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for slow soil carbon, protected humus',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'csoislop',
     >    'instantaneous slow soil carbon protected humus',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (csoislop, cdummy)
      icount(3) = 1
      call writevar(filen,'csoislop',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, csoislop'
         stop 1
      end if
c
c slow soil carbon, nonprotected humus
c
      filen = fdir(1:lf)//'csoislon.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for slow soil carbon, nonprotected humus',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'csoislon',
     >    'instantaneous slow soil carbon nonprotected humus',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (csoislon, cdummy)
      icount(3) = 1
      call writevar(filen,'csoislon',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, csoislon'
         stop 1
      end if
c
c passive soil carbon
c
      filen = fdir(1:lf)//'csoipas.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for passive soil carbon',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'csoipas',
     >    'instantaneous passive soil carbon','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (csoipas, cdummy)
      icount(3) = 1
      call writevar(filen,'csoipas',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, csoipas'
         stop 1
      end if
c
c available plant nitrogen 
c
      filen = fdir(1:lf)//'aplantn.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for available plant nitrogen soil pool',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'aplantn',
     >    'instantaneous plant available soil nitrogen ','kg/m^2',
     >    3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (aplantn, cdummy)
      icount(3) = 1
      call writevar(filen,'aplantn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, aplantn'
         stop 1
      end if
c
c
c growing degree days
c
      filen = fdir(1:lf)//'gdd0.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for growing degree days above 0 deg_C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd0',
     >    'instantaneous growing degree days above 0 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd0, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd0',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd0'
         stop 1
      end if
c
c growing degree days - wheat
c
      filen = fdir(1:lf)//'gdd0c.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for wheat - April 1 - Sept 30 GDD above 0 C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd0c',
     >    'instantaneous growing degree days above 0 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd0c, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd0c',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd0c'
         stop 1
      end if
c
      filen = fdir(1:lf)//'gdd5.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for growing degree days above 5 deg_C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd5',
     >    'instantaneous growing degree days above 5 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd5, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd5',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd5'
         stop 1
      end if
c
      filen = fdir(1:lf)//'gdd8.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for growing degree days above 8 deg_C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd8',
     >    'instantaneous growing degree days above 8 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd8, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd8',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd8'
         stop 1
      end if
c
      filen = fdir(1:lf)//'gdd10.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for growing degree days above 10 deg_C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd10',
     >    'instantaneous growing degree days above 10 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd10, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd10',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd10'
         stop 1
      end if
c
      filen = fdir(1:lf)//'gdd12.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for growing degree days above 12 deg_C',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd12',
     >    'instantaneous growing degree days above 12 deg_C',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd12, cdummy)
      icount(3) = 1
      call writevar(filen,'gdd12',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, gdd12'
         stop 1
      end if

c
c coldest monthly temperature
c
      filen = fdir(1:lf)//'tc.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for coldest monthly temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'tc',
     >    'instantaneous coldest monthly temperature',
     >    'degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (tc, cdummy)
      icount(3) = 1
      call writevar(filen,'tc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, tc'
         stop 1
      end if
c
c warmest monthly temperature
c
      filen = fdir(1:lf)//'tw.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for warmest monthly temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'tw',
     >    'instantaneous warmest monthly temperature',
     >    'degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (tw, cdummy)
      icount(3) = 1
      call writevar(filen,'tw',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, tw'
         stop 1
      end if
c
c ice content of puddles
c
      filen = fdir(1:lf)//'wipud.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for ice content of puddles',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'wipud',
     >    'instantaneous ice content of puddles',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (wipud, cdummy)
      icount(3) = 1
      call writevar(filen,'wipud',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, wipud'
         stop 1
      end if
c
c liquid content of puddles
c
      filen = fdir(1:lf)//'wpud.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for liquid content of puddles',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'wpud',
     >    'instantaneous liquid water content of puddles',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (wpud, cdummy)
      icount(3) = 1
      call writevar(filen,'wpud',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, wpud'
         stop 1
      end if
c
c annual accumulated growing degree days for bud burst, upper canopy
c
      filen = fdir(1:lf)//'agddu.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for upper canopy growing degree days',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'agddu',
     >    'instantaneous growing degree days uc',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (agddu, cdummy)
      icount(3) = 1
      call writevar(filen,'agddu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, agddu'
         stop 1
      end if
c
c annual accumulated growing degree days for bud burst, lower canopy
c
      filen = fdir(1:lf)//'agddl.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for lower canopy growing degree days',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'agddl',
     >    'instantaneous growing degree days lc',
     >    'days degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (agddl, cdummy)
      icount(3) = 1
      call writevar(filen,'agddl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, agddl'
         stop 1
      end if
c
c cold-phenology trigger for trees
c
      filen = fdir(1:lf)//'tempu.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for cold phenology trigger for trees',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'tempu',
     >    'cold phenology trigger for trees',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (tempu, cdummy)
      icount(3) = 1
      call writevar(filen,'tempu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, tempu'
         stop 1
      end if
c
c cold-phenology trigger for shrubs
c
      filen = fdir(1:lf)//'templs.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for cold phenology trigger for shrubs',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'templs',
     >    'cold phenology trigger for shrubs',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (templs, cdummy)
      icount(3) = 1
      call writevar(filen,'templs',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, templs'
         stop 1
      end if
c
c fraction of green vegetation in C3 grasses
c
      filen = fdir(1:lf)//'greenfracl3.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for fraction of green vegetation in C3 grasses',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'greenfracl3',
     >    'fraction of green vegetation in C3 grasses',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (greenfracl3, cdummy)
      icount(3) = 1
      call writevar(filen,'greenfracl3',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, greenfracl3'
         stop 1
      end if
c
c fraction of green vegetation in C4 grasses
c
      filen = fdir(1:lf)//'greenfracl4.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for fraction of green vegetation in C4 grasses',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'greenfracl4',
     >    'fraction of green vegetation in C4 grasses',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (greenfracl4, cdummy)
      icount(3) = 1
      call writevar(filen,'greenfracl4',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, greenfracl4'
         stop 1
      end if
c
c average annual air temperature (purely from climatology)
c
      filen = fdir(1:lf)//'Tavgann.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for average annual air temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'Tavgann',
     >    'average annual air temperature',
     >    'deg C',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (Tavgann, cdummy)
      icount(3) = 1
      call writevar(filen,'Tavgann',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, Tavgann'
         stop 1
      end if
c
c average annual precipitation (purely from climatology)
c
      filen = fdir(1:lf)//'PPTavgann.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for average annual precipitation',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'PPTavgann',
     >    'average annual precipitation',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (PPTavgann, cdummy)
      icount(3) = 1
      call writevar(filen,'PPTavgann',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, PPTavgann'
         stop 1
      end if
c
c 10-day average daily air temperature
c
      filen = fdir(1:lf)//'a10td.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average daily air T',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10td',
     >    '10-day average daily air T',
     >    'K',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10td, cdummy)
      icount(3) = 1
      call writevar(filen,'a10td',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10td'
         stop 1
      end if
c
c 10-day average soil temperature average of first layer
c
      filen = fdir(1:lf)//'a10ts.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average daily soil T',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10ts',
     >    '10-day average daily air T',
     >    'K',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10ts, cdummy)
      icount(3) = 1
      call writevar(filen,'a10ts',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10ts'
         stop 1
      end if
c
c 10-day average canopy photosynthesis rate - broadleaf
c
      filen = fdir(1:lf)//'a10ancub.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy photosynth. rate,'
     >    //' broadleaf',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10ancub',
     >    '10-day average canopy photosynth. rate, broadleaf',
     >    'mol_co2 m-2 s-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10ancub, cdummy)
      icount(3) = 1
      call writevar(filen,'a10ancub',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10ancub'
         stop 1
      end if
ccc
ccc 10-day average canopy photosynthesis rate - conifer
ccc
cc      filen = fdir(1:lf)//'a10ancuc.nc'
cc      if (nyears .le. 2) then
cc         call inifile(idies,filen,
cc     >    'restart file for 10-day average canopy photosynth. rate,'
cc     >    //' conifer',
cc     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
cc     >    latscale,'','none',
cc     >    'none',1,0.,'',tunits,'gregorian',istat)
cc         dimnames(3) = 'time'
cc         call inivar(idies,'a10ancuc',
cc     >    '10-day average canopy photosynth. rate, conifer',
cc     >    'mol_co2 m-2 s-1',3,dimnames,OCEAN,istat)
cc         call endini(idies,istat)
cc      end if
cc      call vec2arr (a10ancuc, cdummy)
cc      icount(3) = 1
cc      call writevar(filen,'a10ancuc',istart,icount,cdummy,ftime,
cc     > tweight,tdate,istat)
cc      if (istat .ne. 0) then
cc         write(*,*) 'ERROR in wrestart, a10ancuc'
cc         stop 1
cc      end if
c
c 10-day average canopy photosynthesis rate - shrubs
c
      filen = fdir(1:lf)//'a10ancls.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy photosynth. rate,'
     >    //' shrubs',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10ancls',
     >    '10-day average canopy photosynth. rate, shrubs',
     >    'mol_co2 m-2 s-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10ancls, cdummy)
      icount(3) = 1
      call writevar(filen,'a10ancls',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10ancls'
         stop 1
      end if
c
c 10-day average canopy photosynthesis rate - c4 grasses
c
      filen = fdir(1:lf)//'a10ancl4.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy photosynth. rate,'
     >    //' c4 grasses',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10ancl4',
     >    '10-day average canopy photosynth. rate, c4 grasses',
     >    'mol_co2 m-2 s-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10ancl4, cdummy)
      icount(3) = 1
      call writevar(filen,'a10ancl4',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10ancl4'
         stop 1
      end if
c
c 10-day average canopy photosynthesis rate - c3 grasses
c
      filen = fdir(1:lf)//'a10ancl3.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy photosynth. rate,'
     >    //' c3 grasses',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10ancl3',
     >    '10-day average canopy photosynth. rate, c3 grasses',
     >    'mol_co2 m-2 s-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10ancl3, cdummy)
      icount(3) = 1
      call writevar(filen,'a10ancl3',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10ancl3'
         stop 1
      end if
c
c 10-day average canopy scaling parameter - upper canopy
c
      filen = fdir(1:lf)//'a10scalparamu.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy scaling parameter,'
     >    //' upper canopy',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10scalparamu',
     >    '10-day average canopy scaling parameter, upper canopy',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10scalparamu, cdummy)
      icount(3) = 1
      call writevar(filen,'a10scalparamu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10scalparamu'
         stop 1
      end if
c
c 10-day average canopy scaling parameter - lower canopy
c
      filen = fdir(1:lf)//'a10scalparaml.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average canopy scaling parameter,'
     >    //' lower canopy',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10scalparaml',
     >    '10-day average canopy scaling parameter, lower canopy',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10scalparaml, cdummy)
      icount(3) = 1
      call writevar(filen,'a10scalparaml',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10scalparaml'
         stop 1
      end if
c
c 10-day average daylight - upper canopy
c
      filen = fdir(1:lf)//'a10daylightu.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average daylight,'
     >    //' upper canopy',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10daylightu',
     >    '10-day average daylight, upper canopy',
     >    'W m-2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10daylightu, cdummy)
      icount(3) = 1
      call writevar(filen,'a10daylightu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10daylightu'
         stop 1
      end if
c
c 10-day average daylight - lower canopy
c
      filen = fdir(1:lf)//'a10daylightl.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 10-day average daylight,'
     >    //' lower canopy',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a10daylightl',
     >    '10-day average daylight, lower canopy',
     >    'W m-2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a10daylightl, cdummy)
      icount(3) = 1
      call writevar(filen,'a10daylightl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a10daylightl'
         stop 1
      end if
c
c 11-day average surface soil temperature
c
      filen = fdir(1:lf)//'a11soiltd.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 11-day average surface soil temperature,'
     >    //' surface soil',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a11soiltd',
     >    '11-day average surface soil temperature',
     >    'deg K',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a11soiltd, cdummy)
      icount(3) = 1
      call writevar(filen,'a11soiltd',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a11soiltd'
         stop 1
      end if
c
c 3-day average daily minimum air temperature
c
      filen = fdir(1:lf)//'a3tdmin.nc'
      if (nyears .le. 2) then
         call inifile(idies,filen,
     >    'restart file for 3-day average daily minimum air temperature,'
     >    //' air temperature',
     >    'ibis wrestart',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'a3tdmin',
     >    '3-day average daily minimum air temperature',
     >    'deg K',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (a3tdmin, cdummy)
      icount(3) = 1
      call writevar(filen,'a3tdmin',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wrestart, a3tdmin'
         stop 1
      end if
c
      return
c
      end
c
c
c ---------------------------------------------------------------------
      subroutine wdaily (nday, iyear, iyear0, ndayr, jday, irstyear)
c ---------------------------------------------------------------------
c
c writes out daily files
c
c ---------------------------------------------------------------------
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comsoi.h'
      include 'comatm.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer nday,   ! number of days run since iyear0
     >        ndayr,  ! 
     >        jday,
     >        irstyear,
     >        iyear,  ! this calendar year
     >        iyear0  ! very first year ever for this run sequence
c
      integer NF_PUT_ATT_TEXT,   ! netcdf function
     >        NF_GLOBAL          ! netcdf function
c
c local variables
c
      integer  mstep,         ! this time step for netcdf file
     >        i,j,k,l,m,      ! loop indice
     >        idies,          ! netcdf file indice
     >        istat           ! netcdf error flag

      integer istart(4), icount(4) ! for writing vars
      real    pindex(npft)         ! index used for pfts and canopies
      real    dindex(nsoilay)      ! index used for soil layers
      real    writeout2(npoi,npft)
c
      character*10 cdate        ! date to use in history attribute in files
      character*10 tdate        ! character date for time step
      character*13 canopies(2)  ! canopy definitions
      character*21 tunits       ! time units
      character*80 dimnames(4)  ! names of dimensions for vars
      character*80 pftdef(npft) ! plant functional type defs (not currently used)
      character*80 filen        ! file name
c
      real ftime,              ! real form of nday
     >     tweight             ! number of days in daily average = 1.
c
c use workspace for local real variables
c
      equivalence (ftime,work(1)),(tweight,work(ndim4+1))
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /,
     >     icount / nlon,nlat,1,1 /
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c     current time value, step, time weight
c
c The following has been edited to allow a restart with no initial
c daily output file (ie. output file is created upon restart)
c
      ftime = nday
c     mstep = nday
      mstep = nday - ndayr
      tweight = 1.
c
c tdate is julian day (3 char) followed by iyear (4 char)
c
      tdate='0000000'//char(0)//char(0)//char(0)
      if (nday .lt. 10) then
         write(tdate(3:3),'(i1)') nday
      else if (nday .lt. 100) then
         write(tdate(2:3),'(i2)') nday
      else
         write(tdate(1:3),'(i3)') nday
      end if
      if (iyear .ge. 1000) then
         write(tdate(4:7),'(i4)') iyear
      else if (iyear .lt. 10) then
         write(tdate(7:7),'(i1)') iyear
      else if (iyear .lt. 100) then
         write(tdate(6:7),'(i2)') iyear
      else
         write(tdate(5:7),'(i3)') iyear
      end if
c
c first time only
c
      if (mstep.eq.1) then
c
c initialize snow layer indices, pft names, etc
c
c         call date(cdate)
c
c time units is days since Dec 31 of the year before iyear0
c
         tunits = 'days since 0000-12-31'
         write(tunits(12:15),'(i4)') iyear0-1
c
         dimnames(1) = 'longitude'
         dimnames(2) = 'latitude'
c        dimnames(3) is set for each variable seperately
c        dimnames(4) not used in this subr for now, but define for future use
         dimnames(4) = 'time'

c
c define plant functional types, canopies with indices
c
         do i = 1, npft
           pindex(i) = i
         enddo
c and with characters
         pftdef(1) = 'trbrevtr - tropical broadleaf evergreen trees'
     >    //char(0)
         pftdef(2) = 
     >    'trbrdetr - tropical broadleaf drought-deciduous trees'
     >    //char(0)
         pftdef(3) = 
     >    'wtbrevtr - warm-temperate broadleaf evergreen trees'
     >    //char(0)
         pftdef(4) = 'tecoevtr - temperate conifer evergreen trees'
     >    //char(0)
         pftdef(5) = 
     >    'tebrdetr - temperate broadleaf cold-deciduous trees'//char(0)
         pftdef(6) = 'bocoevtr - boreal conifer evergreen trees'
     >    //char(0)
         pftdef(7) = 
     >    'bocodetr - boreal conifer cold-deciduous trees'//char(0)
         pftdef(8) = 
     >    'bobrdetr - boreal broadleaf cold-deciduous trees'//char(0)
         pftdef(9) = 'evsh - evergreen shrubs'//char(0)
         pftdef(10) = 'desh - deciduous shrubs'//char(0)
         pftdef(11) = 'c4gr - warm (c4) grasses'//char(0)
         pftdef(12) = 'c3gr - cool (c3) grasses'//char(0)
         pftdef(13) = 'c3 crop - soybean'//char(0)
         pftdef(14) = 'c4 crop - corn'//char(0)
         pftdef(15) = 'c3 crop - wheat'//char(0)
         pftdef(16) = 'c4 crop - sugarcane'//char(0)
         canopies(1) = 'lower canopy'//char(0)
         canopies(2) = 'upper canopy'//char(0)
c
      end if
cc
cc dummy variable example, 3-d - copy & modify for new variable.
cc
c      filen = 'output/daily/dummyv.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average dummyv',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'dummyv','average dummyv',
c     >    'dummyvs-units',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (addummyv, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'dummyv',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, dummyv'
c         stop 1
c      end if
c
c daily windspeed
cc
c      filen = 'output/daily/ud.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average windspeed',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'ud','average windspeed',
c     >    'm/s',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adud, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'ud',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, ud'
c         stop 1
c      end if
c
c
c rainfall
c
c      filen = 'output/daily/rain.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average rainfall',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'rain','average rainfall',
c     >    'mm/day',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adrain, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'rain',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, rain'
c         stop 1
c      end if
c
c cloudiness
c
c      filen = 'output/daily/cloud.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average cloudiness',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'cloud','average cloudiness',
c     >    '%',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (cloud, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'cloud',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, cloud'
c         stop 1
c      end if
c
c rh
c
c      filen = 'output/daily/rh.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average relative humidity',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'rh','average rh',
c     >    '%',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adrh, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'rh',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, rh'
c         stop 1
c      end if
c
c snowfall
c
c      filen = 'output/daily/snow.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average snowfall',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'snow','average snowfall',
c     >    'mm/day',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adsnow, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'snow',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, snow'
c         stop 1
c      end if
cc
cc aet
cc
c      filen = 'output/daily/aet.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average aet',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'aet','average aet',
c     >    'mm/day',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adaet, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'aet',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, aet'
c         stop 1
c      end if
cc
cc trunoff
cc
      filen = 'output/daily/trunoff.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily average total runoff',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'trunoff','average total runoff',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (adtrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'trunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, trunoff'
         stop 1
      end if
c
c srunoff
c
      filen = 'output/daily/srunoff.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily average surface runoff',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'srunoff','average surface runoff',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (adsrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'srunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, srunoff'
         stop 1
      end if
c
c drainage
c
      filen = 'output/daily/drainage.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily average drainage',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'drainage','average drainage',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (addrainage, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'drainage',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, drainage'
         stop 1
      end if
cc
cc wsoi
cc
c      filen = 'output/daily/wsoi.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average soil moisture',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'wsoi','average soil moisture',
c     >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c     call vec2arr (adwsoi, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'wsoi',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, wsoi'
c         stop 1
c      end if
cc
cc wisoi
cc
c      filen = 'output/daily/wisoi.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average soil ice',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'wisoi','average soil ice',
c     >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adwisoi, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'wisoi',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, wisoi'
c         stop 1
c      end if
cc
cc snod
cc
      filen = 'output/daily/snod.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily average snow depth',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'snod','average snow depth',
     >    'meters',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (adsnod, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'snod',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, snod'
         stop 1
      end if
cc
cc snof
cc
      filen = 'output/daily/snof.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily average snow fraction',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'snof','average snow fraction',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (adsnof, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'snof',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, snof'
         stop 1
      end if
cc
cc co2ratio
cc
c      filen = 'output/daily/co2ratio.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily average ratio of root to total soil co2 flux',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'co2ratio','average co2 ratio',
c     >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adco2ratio, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'co2ratio',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, co2ratio'
c         stop 1
c      end if
cc
cc co2mic
cc
c      filen = 'output/daily/co2mic.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily flux of carbon due to soil microbe co2 flux',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'co2mic','soil microbe carbon flux',
c     >    'kg/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adco2mic, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'co2mic',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, co2mic'
c         stop 1
c      end if
c
c nitrate concentrations in solution 
c
c      do 54 l = 1, nsoilay
c        dindex(l) = l  
c 54   continue
c    
c      filen = 'output/daily/nconc.nc'
c       if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'solute nitrate concentration',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','soil depth','cm',nsoilay,dindex,'',
c     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'depth-definition',
c     >    nsoilay*80,soildef)
c         dimnames(3) = 'level'
c         call inivar(idies,'csoln','nitrate concentration each layer',
c     >    'layer',4,dimnames,OCEAN,istat)
c      end if
c      do 12 m = 1, nsoilay 
c         call vec2arr (csoln(1,m), cdummy((m-1)*nlonsub*nlatsub + 1))
c 12   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = nsoilay 
c      call writevar(filen,'csoln',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, csoln'
c         stop 1
c      end if
c
c crop daily lai 
c 'lev' replaced 'pft' so we could look at output with GrADS
c
c
c      filen = 'output/daily/plai.nc'
c
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily pft lai',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','plant functional type','pft',npft,pindex,'',
c     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
c     >    npft*80,pftdef)
c         dimnames(3) = 'level'
c         call inivar(idies,'plai','plai for each pft',
c     >    'm2/m2',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c       end if
c      do 220 k = 1, npft
c         call vec2arr (plai(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 220    continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = npft
c      call writevar(filen,'plai',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, plai'
c         stop 1
c      end if
c
c biomass, by pft, and category 
c additions by CJK 3/5/99 for crop output variables
c 'lev' replaced 'pft' so we could look at output with GrADS
c
csant- out put to compare with  (2* to transform dry-C /IBIS/ to Dry-biomass )
csant 	if(iyear.ge.iyear0)
csant     > write(223,*)iyear,jday,2*(biomass(3,16)-cbior(3,16)),
csant     > 2*(cbios(3,16)+cbiog(3,16)),2*cbiog(3,16),plai(3,16)

      filen = 'output/daily/biomass.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'daily biomass of carbon',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','plant fuctional type','pft',(ecpft-scpft+1),pindex,'',
     >    'level','plant fuctional type','pft',1,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'level'
         call inivar(idies,'biomass','biomass status for each pft (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'cbior','total fine root biomass for each  pft (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'cbiol','total biomass of leaves for each  pft (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'cbios','total stem biomass for each crop pft (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'cbiog','total grain biomass for each crop pft (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'bp','total biomass production for each crop (kg_C m-2)',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'rp','root biomass production for each crop (kg_C m-2)',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'lp','leaf biomass production for each crop (kg_C m-2)',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'ag','frac. of C allocat to grain for each crop pft',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'gdd',' accumulated growing degree days past planting - pft',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'lai',' total leaf area index for each crop pft',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'al','frac. of C allocat to leaf for each crop pft',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'tmax','Daily Max temperature',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'tmin','Daily Minimum temperature',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'td','Daily temperature',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
       call inivar(idies,'prec','Daily Precipitation',
     >    '%',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call endini(idies,istat)
      end if

csant- I changed here to goes only through crops.(ecpft-scpft+1)
csant-      do 320 k =scpft,ecpft
c		k=16
c        call vec2arr (biomass(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
c        call vec2arr (biomass(1,k),  cdummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (cbior(1,k),    ddummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (cbiol(1,k),    edummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (cbios(1,k),    fdummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (cbiog(1,k),    gdummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (aybprod(1,k),  hdummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (ayrprod(1,k),  idummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (aylprod(1,k),  jdummy((k-16)*nlonsub*nlatsub + 1))
c       call crop_back(cropout,writeout2,k,33)
c        call vec2arr (writeout2(1,k),kdummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (gddplant(1,k), ldummy((k-16)*nlonsub*nlatsub + 1))
c        call vec2arr (plai(1,k),     mdummy((k-16)*nlonsub*nlatsub + 1))
c       call crop_back(cropout,writeout2,k,31)
c       call vec2arr (writeout2(1,k),ndummy((k-16)*nlonsub*nlatsub + 1))
csant- 320   continue


      istart(3) = 1
      istart(4) = mstep
      icount(3) = 1 !csant- (ecpft-scpft+1)

csant-      

		k=16
	if(exist(1,13).eq.1)k=13
	if(exist(1,14).eq.1)k=14
	if(exist(1,15).eq.1)k=15
	if(exist(1,16).eq.1)k=16
		k=13

c        call vec2arr (biomass(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (biomass(1,k),    cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo


      call writevar(filen,'biomass',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, biomass'
         stop 1
      end if
c 
csant-      do k =scpft,ecpft

c        call vec2arr (cbior(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (cbior(1,k),    cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'cbior',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, cbior'
         stop 1
      end if
c
csant-      do k =scpft,ecpft

c        call vec2arr (cbiol(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (cbiol(1,k),    cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'cbiol',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
        write(*,*) 'ERROR in wdaily, cbiol'
         stop 1
      end if
 
csant-      do k =scpft,ecpft
c        call vec2arr (cbios(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (cbios(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'cbios',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, cbios'
         stop 1
      end if

csant-      do k =scpft,ecpft
c        call vec2arr (cbiog(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (cbiog(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo
 
      call writevar(filen,'cbiog',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, cbiog'
         stop 1
      endif

csant-      do k =scpft,ecpft
c        call vec2arr (aybprod(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (aybprod(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'bp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, bp'
         stop 1
      endif
c
csant-      do k =scpft,ecpft
c        call vec2arr (ayrprod(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr  (ayrprod(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo


      call writevar(filen,'rp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, rp'
         stop 1
      endif
c
csant-      do k =scpft,ecpft
c        call vec2arr (aylprod(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (aylprod(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'lp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, lp'
         stop 1
      endif
c
csant-      do k =scpft,ecpft
c        call vec2arr (aybprod(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
         call crop_back(cropout,writeout2,k,33)
         call vec2arr (writeout2(1,k),cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo
      call writevar(filen,'ag',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, arepr'
         stop 1
      endif
c
csant-      do k =scpft,ecpft
c        call vec2arr (gddplant(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (gddplant(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo
      call writevar(filen,'gdd',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, gddplant'
         stop 1
      endif
c
csant-      do k =scpft,ecpft
c        call vec2arr (plai(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
        call vec2arr (plai(1,k),  cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo

      call writevar(filen,'lai',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, plai'
         stop 1
      endif
csant-      do k =scpft,ecpft
c        call vec2arr (aybprod(1,k), cdummy((k-13 sould be 13 instead of 16- just to save memory now!!!!!!!!!!!)*nlonsub*nlatsub + 1))
       call crop_back(cropout,writeout2,k,35)
       call vec2arr (writeout2(1,k),cdummy((k-13)*nlonsub*nlatsub + 1))
c	enddo
      call writevar(filen,'al',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, al'
         stop 1
      endif

        call vec2arr (tmax,  cdummy((k-13)*nlonsub*nlatsub + 1))

      call writevar(filen,'tmax',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, tmax'
         stop 1
      endif

        call vec2arr (tmin,  cdummy((k-13)*nlonsub*nlatsub + 1))

      call writevar(filen,'tmin',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, tmin'
         stop 1
      endif

        call vec2arr (td,  cdummy((k-13)*nlonsub*nlatsub + 1))

      call writevar(filen,'td',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, td'
         stop 1
      endif


        call vec2arr (precip,  cdummy((k-13)*nlonsub*nlatsub + 1))

      call writevar(filen,'prec',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, tmin'
         stop 1
      endif

c**************** end biomass.nc *********************
c
cc gdd8
cc
c      filen = 'output/daily/gdd8.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'accumulation of gdd base 8 C',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'gdd8','accumulation of gdd base 8 C',
c     >    'deg C',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (gdd8this, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'gdd8',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, gdd8'
c         stop 1
c      end if
cc
c phenology data 
c additions by CJK 12/22/99 for crop output variables
c
c      filen = 'output/daily/phenology.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'phenological variables',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'pft','plant fuctional type','_',npft,pindex,'',
c     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
c     >    npft*80,pftdef)
c         dimnames(3) = 'pft'
c         call inivar(idies,'gddplant','accumulated gdd since
c     >     each crop pft planting ',
c     >    'gdd',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'pft'
c         call inivar(idies,'hui','heat unit index',
c     >    'dimensionless',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'pft'
c         call inivar(idies,'croplive','switch to tell if
c     >     crop planting has taken place',
c     >    'binary',4,dimnames,OCEAN,istat)
c      endif
c      do 350 k = 1, npft
c         call vec2arr (gddplant(1,k),
c     >                 cdummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (hui(1,k), ddummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (croplive(1,k),
c     >                 edummy((k-1)*nlonsub*nlatsub + 1))
c
c 350   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = npft
c      call writevar(filen,'gddplant',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, gddplant'
c         stop 1
c      end if
c 
c      call writevar(filen,'hui',istart,icount,ddummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, hui'
c         stop 1
c      end if
c 
c      call writevar(filen,'croplive',istart,icount,edummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, croplive'
c         stop 1
c      end if
c 
c bottom and top of vegetation canopies
c added CJK 3/11/99 to analyze crops/grasses
c 'lev' replaced 'canopy' so we could look at output with GrADS
c
c      filen = 'output/daily/zcanopy.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily height of vegetation canopies',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','lev','canopy',2,pindex,'up',
c     >    tunits,'gregorian',istat)
c add global attribute to define canopies with text, use netcdf low-level com
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'canopy_def',
c     >    26+5,'1='//canopies(1)//' 2='//canopies(2))
c         dimnames(3) = 'level'
c         call inivar(idies,'zbot',
c     >    'bottom heights of lower and upper canopies',
c     >    'meters',4,dimnames,OCEAN,istat)
c         call inivar(idies,'ztop',
c     >    'top heights of lower and upper canopies',
c     >    'meters',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      do 20 k = 1, 2
c         call vec2arr (zbot(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 20   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = 2
c      call writevar(filen,'zbot',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, zbot'
c         stop 1
c      end if
c      do 25 k = 1, 2
c         call vec2arr (ztop(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 25   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = 2
c      call writevar(filen,'ztop',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, ztop'
c         stop 1
c      end if
c
cc templs 
cc
c      filen = 'output/daily/templs.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'index based on gdd for growth/sensecence',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'templs','index based on gdd for growth/sensescence',
c     >    'dimensionless',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (templs, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'templs',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, templs'
c         stop 1
c      end if
c
c bottom and top height of lower and upper canopies
c
c      filen = 'output/daily/zcanopy.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily height of vegetation canopies',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'canopy','canopy','_',2,pindex,'up',
c     >    tunits,'gregorian',istat)
c add global attribute to define canopies with text, use netcdf low-level com
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'canopy_def',
c     >    26+5,'1='//canopies(1)//' 2='//canopies(2))
c         dimnames(3) = 'canopy'
c         call inivar(idies,'zbot',
c     >    'bottom heights of lower and upper canopies',
c     >    'meters',4,dimnames,OCEAN,istat)
c         call inivar(idies,'ztop',
c     >    'top heights of lower and upper canopies',
c     >    'meters',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      do 20 k = 1, 2
c         call vec2arr (zbot(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 20   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = 2
c      call writevar(filen,'zbot',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, zbot'
c         stop 1
c      end if
c      do 25 k = 1, 2
c         call vec2arr (ztop(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 25   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = 2
c      call writevar(filen,'ztop',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, ztop'
c         stop 1
c      end if
c
c upper and lower canopy daily lai
c
c      filen = 'output/daily/laicanopy.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily lai-leaf area index of upper and lower vegetation canopies',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    '','','',1,0.0,'',
c     >    tunits,'gregorian',istat)
c         dimnames(3) = 'time'
cc         call inivar(idies,'laiu',
c     >    'daily lai of upper canopy',
c     >    'm2/m2',3,dimnames,OCEAN,istat)
c       call inivar(idies,'lail',
c     >    'daily lai of lower canopy',
c     >    'm2/m2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c
c      call vec2arr (lai(1,1), cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'laiu',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c        write(*,*) 'ERROR in wdaily, laiu'
c         stop 1
c      end if
c      call vec2arr (lai(1,2), cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'lail',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, lail'
c         stop 1
c      end if
c
c crop daily lai 
c 'lev' replaced 'pft' so we could look at output with GrADS
c
c      filen = 'output/daily/plai.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily pft lai',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','plant functional type','pft',npft,pindex,'',
c     >    tunits,'gregorian',istat)
cc add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
c     >    npft*80,pftdef)
c         dimnames(3) = 'level'
c         call inivar(idies,'plai','plai for each pft',
c     >    'm2/m2',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      do 222 k = 1, npft
c         call vec2arr (plai(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 222    continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = npft
c      call writevar(filen,'plai',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, plai'
c         stop 1
c      end if
c
c total plant nitrogen uptake and stress factor by pft
c CJK 2-24-00 for crop output variables
c 'lev' replaced 'pft' so we could look at output with GrADS
c
c      filen = 'output/daily/plantn.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily crop nitrogen variabiles',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','plant functional type','pft',npft,pindex,'',
c     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
c     >    npft*80,pftdef)
c         dimnames(3) = 'level'
c         call inivar(idies,'stressn','nitrogen stress factor for each pft',
c     >    'dimensionless',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'totnuptake','total nitrogen uptake 
c     >     for each  pft',
c     >    'kg/m^2',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'fnleaf','nitrogen fraction in leaf 
c     >     for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'fnstem','nitrogen fraction in stem 
c     >     for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'fnroot','nitrogen fraction in root 
c     >     for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'fngrain','nitrogen fraction in grain 
c     >     for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'fnplant','nitrogen fraction in entire 
c     >     plant for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'tnplant','nitrogen total in plant 
c     >     for each  pft',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      do 325 k = 1, npft
c         call vec2arr (stressn(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (totnuptake(1,k), ddummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (fnleaf(1,k), edummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (fnstem(1,k), fdummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (fnroot(1,k), gdummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (fngrain(1,k), hdummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (fnplant(1,k), idummy((k-1)*nlonsub*nlatsub + 1))
c         call vec2arr (tnplant(1,k), jdummy((k-1)*nlonsub*nlatsub + 1))
c 325   continue
c
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = npft
c      call writevar(filen,'stressn',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, stressn'
c         stop 1
c      end if
c 
c      call writevar(filen,'totnuptake',istart,icount,ddummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, totnuptake'
c         stop 1
c      end if
c
c      call writevar(filen,'fnleaf',istart,icount,edummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, fnleaf'
c         stop 1
c      end if
c
c      call writevar(filen,'fnstem',istart,icount,fdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, fnstem'
c         stop 1
c      end if
c
c      call writevar(filen,'fnroot',istart,icount,gdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, fnroot'
c         stop 1
c      end if
c
c      call writevar(filen,'fngrain',istart,icount,hdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, fngrain'
c         stop 1
c      end if
c
c      call writevar(filen,'fnplant',istart,icount,idummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, fnplant'
c         stop 1
c      end if
c
c      call writevar(filen,'tnplant',istart,icount,jdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, tnplant'
c         stop 1
c      end if
c 
c daily rate of nitrate leaching 
c 
      filen = 'output/daily/leachr.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'rate of nitrogen leached from profile (kg N m-2 y-1)',
     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'dnileach','rate of nitrogen leaching',
     >    'kg n ha-1 y-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (dnileach, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'dnileach',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wdaily, dnileach'
         stop 1
      end if
c
c instantaneous plant available inorganic nitrogen 
c 
c      filen = 'output/daily/aplantn.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'inorganic plant available inorganic nitrogen (kg N m-2 )',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'aplantn','inorganic plant available nitrogen',
c     >    'kg n m-2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (aplantn, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'aplantn',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, aplantn'
c         stop 1
c      end if
c
c cumulative nitrogen leaching 
c 
c      filen = 'output/daily/cumnleach.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'accumulated nitrogen leached from profile at specific depth (kg N m-2)',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'ftot','cumulative nitrogen leaching',
c     >    'kg n ha-1 ',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (ftot, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'ftot',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, ftot'
c         stop 1
c      end if
c
c cumulative drainage 
c 
c      filen = 'output/daily/cumdrainage.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'accumulated drainage from profile at specific depth (mm h20)',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'drntot','cumulative drainage',
c     >    'mm h20 ',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (drntot, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'drntot',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, drntot'
c         stop 1
c      end if
c
c daily plant transpiration  
c 
c      filen = 'output/daily/trans.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily rate of canopy transpiration (upper and lower) (mm/day)',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'adtrans','daily canopy transpiration',
c     >    'mm/day',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adtrans, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'adtrans',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, adtrans'
c         stop 1
c      end if
c
c 
c daily total evaporation  
c 
c      filen = 'output/daily/evap.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily rate of evaporation (mm/day)',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'adevap','daily evaporation',
c     >    'mm/day',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adevap, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'adevap',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, adevap'
c         stop 1
c      end if
c
c 
c daily ratio of transpiration to total ET  
c 
c      filen = 'output/daily/tratio.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'daily ratio of trans to total ET (dimensionless)',
c     >    'ibis wdaily',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'adtratio','ratio of trans to total ET',
c     >    'dimensionless',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (adtratio, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'adtratio',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wdaily, adtratio'
c         stop 1
c      end if
c
      return
      end
c
c
c ---------------------------------------------------------------------
      subroutine wmonthly (nday, imonth, iyear, iyear0, irstyear, jday)
c ---------------------------------------------------------------------
c
c writes out monthly files
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comsum.h'
      include 'comwork.h'
      include 'comcrop.h'
c
c Arguments
c
      integer nday, i,   ! number of days run since iyear0
     >        jday,
     >        imonth,  ! this month
     >        iyear,   ! this calendar year
     >        irstyear,   ! this calendar year
     >        iyear0   ! very first year ever for this run
c
c Local variables
c
      integer mstep,   ! time step in netcdf file
     >        idies,   ! netcdf file indice
     >        istat    ! netcdf error flag
c
      integer istart(4), icount(4) ! for writing vars
c
      character*10 cdate       ! date to use in history attribute in files
      character*10 tdate       ! character date for time step
      character*21 tunits      ! units for time
      character*80 dimnames(4) ! names of dimensions for vars
      character*80 filen       ! file name
      character*3  chmon(12)   ! month abbreviations
c
      real ftime, vecp(npoi),             ! real form of nday
     >     tweight             ! number of days in monthly average
c
c use workspace for local real variables
c
      equivalence (ftime,work(1)),(tweight,work(ndim4+1))
c
c ---------------------------------------------------------------------
c
      data chmon / 'JAN','FEB','MAR','APR','MAY','JUN',
     >             'JUL','AUG','SEP','OCT','NOV','DEC' /
      data istart / 1,1,1,1 /,
     >     icount / nlon,nlat,1,1 /
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c     current time value, step, time weight
c
c Second line for mstep added to allow monthly output added to
c restart run (no monthly output in original run)
c Please choose one line only and comment out the other
c
      ftime = nday
      mstep = 12*(iyear-iyear0) + imonth
c     mstep = 12*(iyear-irstyear) + imonth
      tweight = ndaypm(imonth)
c
c tdate is this month (3 char), followed by this year (4 char)
c
      tdate=chmon(imonth)//'0000'//char(0)//char(0)//char(0)
      if (iyear .ge. 1000) then
         write(tdate(4:7),'(i4)') iyear
      else if (iyear .lt. 10) then
         write(tdate(7:7),'(i1)') iyear
      else if (iyear .lt. 100) then
         write(tdate(6:7),'(i2)') iyear
      else
         write(tdate(5:7),'(i3)') iyear
      end if
c
c first time only
c
      if (mstep.eq.1) then
c
c initialize snow layer indices, pft names, etc
c
c         call date(cdate)
c
c time units is days since Dec 31 of the year before iyear0
c
         tunits = 'days since 0000-12-31'
         write(tunits(12:15),'(i4)') iyear0-1
c
         dimnames(1) = 'longitude'
         dimnames(2) = 'latitude'
c        dimnames(3) is set for each variable seperately
c        dimnames(4) not used in this subr for now, but define for future use
         dimnames(4) = 'time'
      end if
cc
cc dummy variable example, 3-d - copy & modify for new variable.
cc
c      filen = 'output/monthly/dummyv.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'monthly average dummyv',
c     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'dummyv','average dummyv',
c     >    'dummyvs-units',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (amdummyv, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'dummyv',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, dummyv'
c         stop 1
c      end if
c
c temperature
c
      filen = 'output/monthly/temp.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average air temperature',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'temp','average air temperature',
     >    'C',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amtemp, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'temp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, temp'
         stop 1
      end if
c
c rainfall
c
      filen = 'output/monthly/rain.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average rainfall',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'rain','average rainfall',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amrain, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rain',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, rain'
         stop 1
      end if
c
c cloudiness

      filen = 'output/monthly/cloud.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average cloudiness',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'cloud','average cloudiness',
     >    '%',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amcloud, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'cloud',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, cloud'
         stop 1
      end if
c
c rh
c
      filen = 'output/monthly/rh.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average relative humidity',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'rh','average rh',
     >    '%',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amrh, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rh',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, rh'
         stop 1
      end if
c
c snowfall
c
      filen = 'output/monthly/snow.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average snowfall',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'snow','average snowfall',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amsnow, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'snow',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, snow'
         stop 1
      end if
c
c specific humidity
c
c      filen = 'output/monthly/qa.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'monthly average specific humidity',
c     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'qa','average specific humidity',
c     >    'kg-h2o/kg-air',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (amqa, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'qa',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, qa'
c         stop 1
c      end if
c
c evapotranspiration
c
      filen = 'output/monthly/aet.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average aet',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'aet','average evapotranspiration',
     >    'mm/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amaet, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'aet',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, aet'
         stop 1
      end if
c
      filen = 'output/monthly/transETratio.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average transpiration:ET ratio',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'transET','average trans:ET ratio',
     >    'dimensionless',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amtratio, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'transET',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, transET'
         stop 1
      end if
c
c trunoff, srunoff, drainage
c
      filen = 'output/monthly/runoff.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average total runoff',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'rain','monthly average rainfall rate (mm/day)',
     >    'mm/day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'trunoff','average total runoff',
     >    'mm/day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'srunoff','average surface runoff',
     >    'mm/day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'drainage','average drainage',
     >    'mm/day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totnleach','average total n leaching',
     >    'kg/ha/day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'no3leach','average no3 leaching',
     >    'kg/ha/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if

      call vec2arr (amrain, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rain',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, amrain'
         stop 1
      end if
      call vec2arr (amtrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'trunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, trunoff'
         stop 1
      end if
      call vec2arr (amsrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'srunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, srunoff'
         stop 1
      end if
      call vec2arr (amdrainage, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'drainage',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, drainage'
         stop 1
      end if
      call vec2arr (amtotnleach, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totnleach',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, totnleach'
         stop 1
      end if
      call vec2arr (amno3leach, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'no3leach',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, no3leach'
         stop 1
      end if
c
c soil temperature
c
      filen = 'output/monthly/tsoi.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average soil temperature',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'tsoi','average soil temperature',
     >    'degC',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amtsoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'tsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, tsoi'
         stop 1
      end if
c
c soil moisture, ice, volumetric water content, plant available water
c
      filen = 'output/monthly/wsoi.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average soil moisture',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'wsoi','average soil moisture',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'wisoi','average soil ice',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'vwc','average volumetric water content',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'awc',
     >    'average plant available water content',
     >    'cm',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amwsoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'wsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, wsoi'
         stop 1
      end if
      call vec2arr (amwisoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'wisoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, wisoi'
         stop 1
      end if
      call vec2arr (amvwc, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'vwc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, vwc'
         stop 1
      end if
      call vec2arr (amawc, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'awc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, awc'
         stop 1
      end if
c
c snow depth
c
      filen = 'output/monthly/snod.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average snow depth',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'snod','average snow depth',
     >    'meters',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amsnod, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'snod',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, snod'
         stop 1
      end if
c
c snow fraction
c
      filen = 'output/monthly/snof.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average snow fraction',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'snof','average snow fraction',
     >    'm^2/m^3',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amsnof, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'snof',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, snof'
         stop 1
      end if
c
c solar radiation
c
      filen = 'output/monthly/solar.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average incident solar radiation',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'solar','average incident solar radiation',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amsolar, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'solar',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, solar'
         stop 1
      end if
c
c albedo
       filen = 'output/monthly/albedo.nc'
       if (mstep .eq. 1) then
        call inifile(idies,filen,
     >    'monthly average albedo',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'albedo','average albedo',
     >    'fraction',3,dimnames,OCEAN,istat)
          call endini(idies,istat)
       end if
       call vec2arr (amalbedo, cdummy)
       istart(3) = mstep
       icount(3) = 1
       call writevar(filen,'albedo',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
       if (istat .ne. 0) then
        write(*,*) 'ERROR in wmonthly, albedo'
        stop 1
       end if
c


c downward and upward infrared radiation
c
c      filen = 'output/monthly/ir.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'monthly average infrared radiation',
c     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'irdown','average downward IR',
c     >    'W/m^2',3,dimnames,OCEAN,istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'irup','average upward IR',
c     >    'W/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (amirdown, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'irdown',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, irdown'
c         stop 1
c      end if
c      call vec2arr (amirup, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'irup',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, irup'
c         stop 1
c      end if
c
c Net Radiation
c
      filen = 'output/monthly/rnet.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average net radiation',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'rn','net radiation flux',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (rnet, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, sens'
         stop 1
      end if


c sensible heat flux
c
      filen = 'output/monthly/sens.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average sensible heat flux',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'sens','average sensible heat flux',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amsens, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'sens',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, sens'
         stop 1
      end if
c
c latent heat flux
c
      filen = 'output/monthly/latent.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly average latent heat flux',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'latent','average latent heat flux',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amlatent, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'latent',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, latent'
         stop 1
      end if
c
c leaf area index upper and lower
c
      filen = 'output/monthly/lai.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >   'monthly average leaf area index for upper and lower canopies',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'laiu','average lai upper canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'lail','average lai lower canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amlaiu, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'laiu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, laiu'
         stop 1
      end if
      call vec2arr (amlail, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'lail',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, lail'
         stop 1
      end if
c
c total net primary productivity
c
      filen = 'output/monthly/npptot.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'monthly total net primary productivity of carbon',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'npptot','total npp of carbon',
     >    'kg m-2 month-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (amnpptot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'npptot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, npptot'
         stop 1
      end if
c
c co2ratio
c
c      filen = 'output/monthly/co2ratio.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'monthly ratio of root respiration to total soil respiration',
c     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'co2ratio',
c     >    'ratio of root to soil respiration',
c     >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (amco2ratio, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'co2ratio',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, co2ratio'
c         stop 1
c      end if
c

csant  Grid information
 	if(imonth.eq.1.and. iyear.eq. iyear0) then

	do i=1,npoi
	vecp(i)=i
	print*,i,vecp(i),pmask(i)
	enddo
c
      filen = 'output/monthly/grid_info.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'npoi',
     >    'ibis wmonthly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'npoi','Point number',
     >    'none',3,dimnames,OCEAN,istat)

         call inivar(idies,'garea','Grid area',
     >    'm^2',3,dimnames,OCEAN,istat)

         call inivar(idies,'ppmask','Grid area',
     >    'none',3,dimnames,OCEAN,istat)

         call endini(idies,istat)
      end if
      call vec2arr (vecp, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'npoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, npoi'
         stop 1
      end if
      call vec2arr (garea, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'garea',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, garea'
         stop 1
      end if
	

      call vec2arr (pmask, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'ppmask',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wmonthly, pmask'
         stop 1
      end if

	endif

      return
      end
c
c
c ---------------------------------------------------------------------
      subroutine wyearly (nday,iyear,iyear0, irstyear)
c ---------------------------------------------------------------------
c
c writes out yearly files
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comsoi.h'
      include 'comatm.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer nday,   ! number of days run since iyear0
     >        iyear,  ! this calendar year
     >        irstyear,  ! 
     >        iyear0  ! very first year ever for this run
c
c local variables
c
      integer mstep,  ! time for this year step
     >        idies,  ! netcdf file indice
     >        istat,  ! netcdf error flag
     >        i,k,l,m ! loop indices        
c
      integer istart(4), icount(4) ! for writing vars
      real    pindex(npft)         ! index used for pfts and canopies
      real    dindex(nsoilay)      ! index used for soil layers
      real    writeout(npoi),ky
      real    writeout2(npoi,npft),avgyld(4)
c
      character*10 cdate        ! date to use in history attribute in files
      character*10 tdate        ! character date for time step
      character*13 canopies(2)  ! canopy definitions
      character*21 tunits       ! time units
      character*80 dimnames(4)  ! names of dimensions for vars
      character*80 pftdef(npft) ! plant functional type defs (not currently used)
      character*80 filen        ! file name
c
      real ftime,               ! real form of nday
     >     tweight              ! number of days in yearly average
c
c External
c 
      integer NF_PUT_ATT_TEXT,   ! netcdf function
     >        NF_GLOBAL          ! netcdf function
c
c use workspace for local real variables
c
      equivalence (ftime,work(1)),(tweight,work(ndim4+1))
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /,
     >     icount / nlon,nlat,1,1 /
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c current time value and step: make ftime Jan 1 of this year 
c instead of Dec 31
c
c Second line for mstep added to allow yearly output added to
c restart run (no yearly output in original run)
c Please choose one line and comment out the other
c
      ftime = nday - ndaypy + 1
      tweight = ndaypy
      mstep = iyear - iyear0 + 1
c     mstep = iyear - irstyear + 1
c
c tdate is ANN (3 char) followed by this year (4 char)
c
      tdate='ANN0000'//char(0)//char(0)//char(0)
      if (iyear .ge. 1000) then
         write(tdate(4:7),'(i4)') iyear
      else if (iyear .lt. 10) then
         write(tdate(7:7),'(i1)') iyear
      else if (iyear .lt. 100) then
         write(tdate(6:7),'(i2)') iyear
      else
         write(tdate(5:7),'(i3)') iyear
      end if
c
c first time only
c
      if (mstep .eq. 1) then
c
c         call date(cdate)
c
c time units is days since Dec 31 of the year before iyear0
c
         tunits = 'days since 0000-12-31'
         write(tunits(12:15),'(i4)') iyear0-1
c
c dimension names
c
         dimnames(1) = 'longitude'
         dimnames(2) = 'latitude'
c        dimnames(3) is set for each variable seperately
         dimnames(4) = 'time'
c
c define plant functional types, canopies with indices
c
         do i = 1, npft
           pindex(i) = i
         enddo
c and with characters
         pftdef(1) = 'trbrevtr - tropical broadleaf evergreen trees'
     >    //char(0)
         pftdef(2) = 
     >    'trbrdetr - tropical broadleaf drought-deciduous trees'
     >    //char(0)
         pftdef(3) = 
     >    'wtbrevtr - warm-temperate broadleaf evergreen trees'
     >    //char(0)
         pftdef(4) = 'tecoevtr - temperate conifer evergreen trees'
     >    //char(0)
         pftdef(5) = 
     >    'tebrdetr - temperate broadleaf cold-deciduous trees'//char(0)
         pftdef(6) = 'bocoevtr - boreal conifer evergreen trees'
     >    //char(0)
         pftdef(7) = 
     >    'bocodetr - boreal conifer cold-deciduous trees'//char(0)
         pftdef(8) = 
     >    'bobrdetr - boreal broadleaf cold-deciduous trees'//char(0)
         pftdef(9) = 'evsh - evergreen shrubs'//char(0)
         pftdef(10) = 'desh - deciduous shrubs'//char(0)
         pftdef(11) = 'c4gr - warm (c4) grasses'//char(0)
         pftdef(12) = 'c3gr - cool (c3) grasses'//char(0)
         pftdef(13) = 'c3 crop - soybean'//char(0)
         pftdef(14) = 'c4 crop - corn'//char(0)
         pftdef(15) = 'c3 crop - wheat'//char(0)
         pftdef(16) = 'c4 crop - sugarcane'//char(0)

         canopies(1) = 'lower canopy'//char(0)
         canopies(2) = 'upper canopy'//char(0)
c
      end if
cc
cc dummy variable example, 4-d var whose 3rd dim is a character dim (pft)
cc copy and modify for a new variable
cc
c      filen = 'output/yearly/dummyv.nc'
c      if (mstep .eq. 1) then
c         call inifilec(idies,filen,
c     >    'annual dummyv',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'pft','plant fuctional type','_',npft,80,pftdef,
c     >    tunits,'gregorian',istat)
c         dimnames(3) = 'pft'
c         call inivar(idies,'dummyv','dummyv for each pft',
c     >    'dummyvs-units',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      do 5 k = 1, npft
c         call vec2arr (aydummyv(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 5    continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = npft
c      call writevar(filen,'dummyv',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, dummyv'
c         stop 1
c      end if
cc
cc dummy variable example, 3-d
cc
c      filen = 'output/yearly/dummyv.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'dummyv','annual dummyv',cdate,nlonsub,lonscale,
c     >    nlatsub,latscale,'','none','none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'dummyv','annual dummyv',
c     >    'dummyvs-units',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (aydummyv, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'dummyv',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, dummyv'
c         stop 1
c      end if
c
c net primary productivity, by pft and total
c
      filen = 'output/yearly/npp.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual net primary productivity of carbon',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant fuctional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'npp','npp of carbon for each pft',
     >    'kg m-2 year-1',4,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'npptot','total npp',
     >    'kg m-2 year-1',3,dimnames,OCEAN,istat)
         call inivar(idies,'anpptot','total above-ground npp',
     >    'kg m-2 year-1',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 5 k = 1, npft
         call vec2arr (aynpp(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 5    continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = npft
      call writevar(filen,'npp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, npp'
         stop 1
      end if
      call vec2arr (aynpptot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'npptot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, npptot'
         stop 1
      end if
      call vec2arr (ayanpptot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'anpptot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, anpptot'
         stop 1
      end if
c
c evapotranspiration
c
      filen = 'output/yearly/aet.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual average evapotranspiration',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'aet','average evapotranspiration',
     >    'mm/year',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (ayaet, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'aet',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, aet'
         stop 1
      end if
c
c trunoff, srunoff, drainage, rratio, tratio
c
      filen = 'output/yearly/runoff.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual runoff',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'trunoff','total runoff',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'srunoff','surface runoff',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totirrig','total irrigation water applied',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'ayprcp','total annual precipitation',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'drainage','drainage',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'rratio','average runoff ratio',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'tratio','average transpiration ratio',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (aytrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'trunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, trunoff'
         stop 1
      end if
      call vec2arr (aysrunoff, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'srunoff',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, srunoff'
         stop 1
      end if
      call vec2arr (totirrig, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totirrig',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totirrig'
         stop 1
      end if
      call vec2arr (ayprcp, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'ayprcp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, ayprcp'
         stop 1
      end if
      call vec2arr (aydrainage, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'drainage',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, drainage'
         stop 1
      end if
      call vec2arr (ayrratio, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rratio',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, rratio'
         stop 1
      end if
      call vec2arr (aytratio, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'tratio',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, tratio'
         stop 1
      end if
c
c gdd information - climate and current year 
c
      filen = 'output/yearly/gdd.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual gdd info',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd8clim','climate gdd8',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd10clim','climate gdd10',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd12clim','climate gdd12',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gdd0clim','climate gdd0',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'currgdd8','current year gdd8',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'currgdd10','current year gdd10',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'currgdd12','current year gdd12',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'currgdd0','current year gdd0',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gddfzcorn','current year gdd between freeze events',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gddfzsoy','current year gdd between freeze events',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gddfzsgc','current year gdd between freeze events',
     >    'degrees C',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'gsdays','number of days between freeze events',
     >    'days',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'iniday','last day in spring for freeze',
     >    'day',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'endday','first day in fall for freeze',
     >    'day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (gdd8, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gdd8clim',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gdd8clim'
         stop 1
      end if
      call vec2arr (gdd10, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gdd10clim',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gdd10clim'
         stop 1
      end if
      call vec2arr (gdd12, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gdd12clim',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gdd12clim'
         stop 1
      end if
      call vec2arr (gdd0c, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gdd0clim',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gdd0clim'
         stop 1
      end if
      call vec2arr (gdd8this, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'currgdd8',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, currgdd8'
         stop 1
      end if
      call vec2arr (gdd10this, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'currgdd10',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, currgdd10'
         stop 1
      end if
      call vec2arr (gdd12this, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'currgdd12',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, currgdd12'
         stop 1
      end if
      call vec2arr (gdd0cthis, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'currgdd0',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, currgdd0'
         stop 1
      end if
      call arr3_arr2 (gddcorn, writeout ,iyear)
      call vec2arr (writeout, cdummy)
csant-      call vec2arr (gddfzcorn, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gddfzcorn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gddfzcorn'
         stop 1
      end if
      call arr3_arr2 (gddsoy, writeout ,iyear)
      call vec2arr (writeout, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gddfzsoy',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gddfzsoy'
         stop 1
      end if
      call arr3_arr2 (gddsgc, writeout ,iyear)
csant-	print*,'writeout(1)',writeout(1)
      call vec2arr (writeout, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gddfzsgc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gddfzsgc'
         stop 1
      end if
      call vec2arr (gsdays, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'gsdays',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gsdays'
         stop 1
      end if
      call vec2arr (iniday, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'iniday',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, iniday'
         stop 1
      end if
      call vec2arr (endday, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'endday',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, endday'
         stop 1
      end if
c
c soil moisture, soil ice, volumetric water content, plant available water
c
      filen = 'output/yearly/wsoi.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual average soil moisture',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'wsoi','average soil moisture',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'wisoi','average soil ice',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'vwc','average volumetric water content',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'awc','average volumetric water content',
     >    'cm',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (aywsoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'wsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, wsoi'
         stop 1
      end if
      call vec2arr (aywisoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'wisoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, wisoi'
         stop 1
      end if
      call vec2arr (ayvwc, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'vwc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, vwc'
         stop 1
      endif
      call vec2arr (ayawc, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'awc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, awc'
         stop 1
      endif
c
c soil temperature
c
c      filen = 'output/yearly/tsoi.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual average soil temperature',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'tsoi','average soil temperature',
c     >    'degrees C',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (aytsoi, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'tsoi',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, tsoi'
c         stop 1
c      endif
cc
cc solar radiation
cc
c      filen = 'output/yearly/solar.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual average solar incident radiation',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'solar','average solar radiation',
c     >    'W/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (aysolar, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'solar',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, solar'
c         stop 1
c      end if
cc
cc albedo
cc
c     filen = 'output/yearly/albedo.nc'
c     if (mstep .eq. 1) then
c        call inifile(idies,filen,
c    >    'annual average albedo',
c    >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c    >    'none',1,0.,'',tunits,'gregorian',istat)
c        dimnames(3) = 'time'
c        call inivar(idies,'albedo','average albedo',
c    >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c     end if
c     call vec2arr (ayalbedo, cdummy)
c     istart(3) = mstep
c     icount(3) = 1
c     call writevar(filen,'albedo',istart,icount,cdummy,ftime,
c    > tweight,tdate,istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in wyearly, albedo'
c        stop 1
c     end if
cc
cc upward and downward infrared radiation
cc
c      filen = 'output/yearly/ir.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual average solar infrared radiation',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'irdown','average downward ir',
c     >    'W/m^2',3,dimnames,OCEAN,istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'irup','average upward ir',
c     >    'W/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (ayirdown, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'irdown',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, irdown'
c         stop 1
c      end if
c      call vec2arr (ayirup, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'irup',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, irup'
c         stop 1
c      end if
c
c sensible heat flux
c
      filen = 'output/yearly/sens.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual average sensible heat flux',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'sens','average sensible heat flux',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (aysens, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'sens',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, sens'
         stop 1
      end if
cc
cc latent heat flux
cc
      filen = 'output/yearly/latent.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual average latent heat flux',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'latent','average latent heat flux',
     >    'W/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (aylatent, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'latent',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, latent'
         stop 1
      end if
c
c lai, by pft, total upper canopy, total lower canopy
c
      filen = 'output/yearly/plai.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual leaf area index',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant fuctional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'plai','leaf area index for each pft',
     >    'fraction',4,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totlaiu','total lai for upper canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totlail','total lai for lower canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 10 k = 1, npft
         call vec2arr (plai(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 10   continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = npft
      call writevar(filen,'plai',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, plai'
         stop 1
      end if
      call vec2arr (totlaiu, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totlaiu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totlaiu'
         stop 1
      end if
      call vec2arr (totlail, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totlail',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totlail'
         stop 1
      end if
c
c biomass, by pft, upper canopy, lower canopy
c additions by CJK 3/5/99 for crop output variables
c this includes looking at total biomass production (not remaining standing)
c aboveground and total, along with biomass in grain and harvest index as a
c function of plant functional type 
c 'lev' replaced 'pft' so we could look at output with GrADS
c
      filen = 'output/yearly/biomass.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual biomass of carbon',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'level','plant fuctional type','pft',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'level'
         call inivar(idies,'biomass','biomass for each pft',
     >    'kg/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totbiou','total biomass for upper canopy',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totbiol','total biomass for lower canopy',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
          call endini(idies,istat)
      end if
      do 15 k = 1, npft
         call vec2arr (biomass(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 15   continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = npft
      call writevar(filen,'biomass',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, biomass'
         stop 1
      end if
c 
      call vec2arr (totbiou, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totbiou',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totbiou'
         stop 1
      end if
      call vec2arr (totbiol, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totbiol',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totbiol'
        stop 1
      end if


cwrite out series of production on output/production.dat
	 do i=1,4
         avgyld(i) = 0.
	enddo


	ky=0.
	 	do i=1,npoi

	 if(cropout(i,16,6).gt.20.0)then

	print*,'point harvested this year',i,' temp ',cropout(i,7,31)-273.16

	   ky=ky+1.
    	   avgyld(1) = avgyld(1) + cropout(i,16,6) !productiond

c	 if(i.ge.9.and.i.le.11.and.cropout(i,16,6).gt.20.)then
c	print*,i
c	   k=k+1
c    	   avgyld = avgyld + cropout(i,16,6)
c	 endif


   	   avgyld(2) = avgyld(2) + cropout(i,1,31)-273.16  !td
    	   avgyld(3) = avgyld(3) + cropout(i,2,31)	   !prec
    	   avgyld(4) = avgyld(4) + cropout(i,3,31)-273.16  !tmax

	 endif

		enddo
	print*,iyear,'   Total of ',ky,' points harvested, Avg Prod =', avgyld(1)

 	do i=1,4

	   if(ky.ge.1.) then
	         avgyld(i) = avgyld(i)/ky
	     endif

	enddo


	print*,iyear,'   Total of ',ky,' points harvested, Avg Prod =', avgyld(1)


c	do i=1,1
       if(iyear.ge.1990) write(224,43)iyear,avgyld(1),avgyld(2),avgyld(3)!,cropout(1,16,6)
csant     > ,cropout(2,16,6),cropout(3,16,6),cropout(4,16,6),cropout(5,16,6)
csant     > ,cropout(6,16,6),cropout(7,16,6),cropout(8,16,6),cropout(9,16,6)
csant     > ,cropout(10,16,6),cropout(11,16,6),cropout(12,16,6)    

c      write(224,43)iyear,avgyld(1),avgyld(2),avgyld(4),cropout(1,2,31)
c     > ,cropout(2,2,31),cropout(3,2,31),cropout(4,2,31),cropout(5,2,31)
c     > ,cropout(6,2,31),cropout(7,2,31),cropout(8,2,31),cropout(9,2,31)
c     > ,cropout(10,2,31),cropout(11,2,31),cropout(12,2,31)    

c	enddo
 43   format(i4,15(1x,f6.2))        


c crops output
c 'lev' replaced 'pft' so we could look at output with GrADS
c
      filen = 'output/yearly/crops.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual crop growth variables',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'level','crop type','pft',(ecpft-scpft+1),pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'level'

         call inivar(idies,'cropy','crop cycle number in a cycle (1s planting - last ratoon)',
     >    'day of year',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'plantdate','crop planting date -day',
     >    'day of year',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'hdate','crop harvest date - day',
     >    'day of year',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dpp',' number of days of crop cycle - day',
     >    'number of days',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'harvidx','harvest index - fraction',
     >    'fraction',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'croplaimx','maximum crop lai- m^2/m^2',
     >    'm^2/m^2',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'grainn','nitrogen removed by grain - kg/ha',
     >    'kg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'cropyld','crop yield - grain(bu/ac) or stalk (t/ha)',
     >    'bu/ac',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmyield','crop yield dry matter - Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmleaf','leaf dry matter - Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmstem','stem dry matter - Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmroot','root dry matter - Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmresidue','aboveground residue dry matter -Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'dmcrop','total crop dry matter - Mg/ha',
     >    'Mg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'residuen','total nitrogen in residue - kg/ha',
     >    'kg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'nconcl','leaf nitrogen concentration - %',
     >    'percent',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'nconcs','stem nitrogen concentration - %',
     >    'percent',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'nconcr','root nitrogen concentration - %',
     >    'percent',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'nconcg','grain nitrogen concentration - %',
     >    'percent',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'cropn','total crop nitrogen uptake - kg/ha',
     >    'kg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'cropfixn','nitrogen fixation - kg/ha',
     >    'kg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'cntops','cn ratio of aboveground residue ',
     >    'dimensionless',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'cnroot','cn ratio of  fineroots ',
     >    'dimensionless',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'fertilizer','fertilizer applied - kg/ha',
     >    'kg/ha',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'grainday','grainday - day',
     >    'day',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'hybrid','hybridgdd - degrees C',
     >    'degrees C',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'gddplant','accumulated growing degree days between planting and harvest',
     >    'degrees C',4,dimnames,OCEAN,istat)
          dimnames(3) = 'level'
         call inivar(idies,'crmclim','CRM rating for climatology -days',
     >    'days',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'crmact','best CRM rating for this year -days',
     >    'days',4,dimnames,OCEAN,istat)
         dimnames(3) = 'level'
         call inivar(idies,'crmplant','CRM rating for this year based on planting date -d',
     >    'days',4,dimnames,OCEAN,istat)
          call endini(idies,istat)

      end if

c      do 18 k = scpft, ecpft

c          call crop_back(cropout,writeout2,k,1)
c         call vec2arr (writeout2(1,k), adummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,2)
c         call vec2arr (writeout2(1,k), bdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,3)
c         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,4)
c         call vec2arr (writeout2(1,k), ddummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,5)
c         call vec2arr (writeout2(1,k), edummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,6)
c         call vec2arr (writeout2(1,k), fdummy((k-13)*nlonsub*nlatsub + 1)) 
c           call crop_back(cropout,writeout2,k,7)
c         call vec2arr (writeout2(1,k), gdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,8)
c         call vec2arr (writeout2(1,k), hdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,9)
c         call vec2arr (writeout2(1,k), idummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,10)
c         call vec2arr (writeout2(1,k), jdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,11)
c         call vec2arr (writeout2(1,k), kdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,12)
c        call vec2arr (writeout2(1,k), ldummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,13)
c         call vec2arr (writeout2(1,k), mdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,14)
c         call vec2arr (writeout2(1,k), ndummy((k-13)*nlonsub*nlatsub + 1)) 
c           call crop_back(cropout,writeout2,k,15)
cc        call vec2arr (writeout2(1,k), odummy((k-13)*nlonsub*nlatsub + 1))
c          call crop_back(cropout,writeout2,k,16)
c        call vec2arr (writeout2(1,k), pdummy((k-13)*nlonsub*nlatsub + 1))
c         call crop_back(cropout,writeout2,k,17)
c         call vec2arr (writeout2(1,k), qdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,18)
c         call vec2arr (writeout2(1,k), rdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,19)
c         call vec2arr (writeout2(1,k), sdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,20)
c         call vec2arr (writeout2(1,k), tdummy((k-13)*nlonsub*nlatsub + 1)) 
c           call crop_back(cropout,writeout2,k,21)
c         call vec2arr (writeout2(1,k), udummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,22)
c         call vec2arr (writeout2(1,k), vdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,23)
c        call vec2arr (writeout2(1,k), wdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,24)
c         call vec2arr (writeout2(1,k), xdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,25)
c         call vec2arr (writeout2(1,k), ydummy((k-13)*nlonsub*nlatsub + 1)) 
c           call crop_back(cropout,writeout2,k,26)
c         call vec2arr (writeout2(1,k), zdummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,27)
c         call vec2arr (writeout2(1,k), z2dummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,28)
c         call vec2arr (writeout2(1,k), z3dummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,29)
c         call vec2arr (writeout2(1,k), z4dummy((k-13)*nlonsub*nlatsub + 1))
c           call crop_back(cropout,writeout2,k,30)
c         call vec2arr (writeout2(1,k), z5dummy((k-13)*nlonsub*nlatsub + 1))


csant         call vec2arr (hdate(1,k),     adummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (pdate(1,k),     bdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (harvidx(1,k),   cdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (croplaimx(1,k), ddummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (grainn(1,k),    edummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (cropyld(1,k),   fdummy((k-1)*nlonsub*nlatsub + 1))
csant        call vec2arr (dmyield(1,k),   gdummy((k-1)*nlonsub*nlatsub + 1))
csant    call vec2arr (dmleaf(1,k),    hdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (dmstem(1,k),    idummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (dmroot(1,k),    jdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (dmresidue(1,k), kdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (dmcrop(1,k),    ldummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (residuen(1,k),  mdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (nconcl(1,k),    ndummy((k-1)*nlonsub*nlatsub + 1))
csantsant         call vec2arr (nconcs(1,k),    odummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (nconcr(1,k),    pdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (nconcg(1,k),    qdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (cropn(1,k),     rdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (cropfixn(1,k),  sdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (cntops(1,k),    tdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (cnroot(1,k),    udummy((k-1)*nlonsub*nlatsub + 1))
c sant        call vec2arr (fertinput(1,k), vdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (gddmaturity(1,k), wdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (crmclim(1,k), xdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (crmact(1,k), ydummy((k-1)*nlonsub*nlatsub + 1))
c sant        call vec2arr (crmplant(1,k), zdummy((k-1)*nlonsub*nlatsub + 1))
csant         call vec2arr (grainday(1,k), z2dummy((k-1)*nlonsub*nlatsub + 1))
c 18   continue
c 
      istart(3) = 1
      istart(4) = mstep
      icount(3) = (ecpft-scpft+1)

      do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,29)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo 

      call writevar(filen,'cropy',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cropy'
         stop 1
      end if
c
      do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,1)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'plantdate',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, plantdate'
         stop 1
      end if
c 
      do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,2)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'hdate',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, hdate'
         stop 1
      end if
c 
      do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,28)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'dpp',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dpp'
         stop 1
      end if
c
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,3)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'harvidx',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, harvidx'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,4)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'croplaimx',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, croplaimx'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,5)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'grainn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, grainn'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,6)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1)) 
	enddo
      call writevar(filen,'cropyld',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cropyld'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,7)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'dmyield',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmyield'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,8)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'dmleaf',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmleaf'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,9)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo

      call writevar(filen,'dmstem',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmstem'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,10)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo

      call writevar(filen,'dmroot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmroot'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,11)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'dmresidue',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmresidue'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,12)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'dmcrop',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, dmcrop'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,13)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'residuen',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, residuen'
         stop 1
      end if
c 
      do k = scpft, ecpft
          call crop_back(cropout,writeout2,k,14)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1)) 
	enddo
      call writevar(filen,'nconcl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nconcl'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,15)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'nconcs',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nconcs'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,16)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'nconcr',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nconcr'
         stop 1
      end if
c 
        do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,17)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'nconcg',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nconcg'
         stop 1
      end if
c 
         do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,18)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'cropn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cropn'
         stop 1
      end if
c
        do k = scpft, ecpft
          call crop_back(cropout,writeout2,k,19)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'cropfixn',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cropfixn'
         stop 1
      end if
c
        do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,20)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1)) 
	enddo
      call writevar(filen,'cntops',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cntops'
         stop 1
      end if
c 
         do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,21)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'cnroot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, cnroot'
         stop 1
      end if
c 
        do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,22)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'fertilizer',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, fertilizer'
         stop 1
      end if
c 
        do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,27)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'grainday',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, grainday'
         stop 1
      end if
c 
       do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,23)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'hybrid',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, hybrid'
         stop 1
      end if
c 
       do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,30)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'gddplant',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, gddplant'
         stop 1
      end if
c 
       do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,24)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'crmclim',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, crmclim'
         stop 1
      end if
c 
      do k = scpft, ecpft
         call crop_back(cropout,writeout2,k,25)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1)) 
	enddo
      call writevar(filen,'crmact',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, crmact'
         stop 1
      end if
c 
      do k = scpft, ecpft
           call crop_back(cropout,writeout2,k,26)
         call vec2arr (writeout2(1,k), cdummy((k-13)*nlonsub*nlatsub + 1))
	enddo
      call writevar(filen,'crmplant',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, crmplant'
         stop 1
      end if
c 
c soil texture 
c
c      do 55 l = 1, nsoilay
c        dindex(l) = l  
c 55   continue
c    
c      filen = 'output/yearly/soitext.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'soil texture',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
c     >    'level','soil depth','cm',nsoilay,dindex,'',
c     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
c         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'depth-definition',
c     >    nsoilay*80,soildef)
c         dimnames(3) = 'level'
c         call inivar(idies,'soisand','sand fraction for each layer',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         dimnames(3) = 'level'
c         call inivar(idies,'soiclay','clay fraction for each layer',
c     >    'fraction',4,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c       end if
c      do 12 m = 1, nsoilay 
c         call vec2arr (soisand(1,m), cdummy((m-1)*nlonsub*nlatsub + 1))
c         call vec2arr (soiclay(1,m), ddummy((m-1)*nlonsub*nlatsub + 1))
c 12   continue
c      istart(3) = 1
c      istart(4) = mstep
c      icount(3) = nsoilay 
c      call writevar(filen,'soisand',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, soisand'
c         stop 1
c      end if
c      call writevar(filen,'soiclay',istart,icount,ddummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, soiclay'
c         stop 1
c      end if
c
c soil carbon: rootbio, totalit, totrlit, totcsoi, totcmic
c
      filen = 'output/yearly/csoi.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total soil carbon',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'rootbio','total live root biomass carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totalit','total above ground litter carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totrlit','total below ground litter carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totcsoi','total soil carbon w/o litter',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totcmic','total microbial carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (ayrootbio, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'rootbio',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, rootbio'
         stop 1
      end if
      call vec2arr (ayalit, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totalit',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totalit'
         stop 1
      end if
      call vec2arr (ayblit, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totrlit',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totrlit'
         stop 1
      end if
      call vec2arr (aycsoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totcsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totcsoi'
         stop 1
      end if
      call vec2arr (aycmic, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totcmic',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totcmic'
         stop 1
      end if
c
c litterfall: falll, fallr, fallw
c
      filen = 'output/yearly/litterfall.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual litterfall',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'falll',
     >    'total annual leaf litterfall',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'fallr',
     >    'total below ground annual root turnover',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'fallw','total wood litterfall',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (falll, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'falll',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, falll'
         stop 1
      end if
      call vec2arr (fallr, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'fallr',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, fallr'
         stop 1
      end if
      call vec2arr (fallw, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'fallw',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, fallw'
         stop 1
      end if
c
c soil nitrogen: totanlit, totrnlit, totnsoi, nmintot
c and residue carbon : nitrogen ratio for crops
c
c nitrogen leaching
c
c nitrogen uptake by natural vegetation
c
      filen = 'output/yearly/nsoi.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total soil nitrogen',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'totanlit',
     >    'total above ground litter nitrogen',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totrnlit',
     >    'total below ground litter nitrogen',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totnsoi','total soil organic nitrogen w/o litter',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'nmin','net nitrogen mineralization',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'nimmob','total nitrogen immobilized',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'ndepos','total nitrogen deposition',
     >    'kg/m2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'nfixnat','nitrogen fixation by natural vegetation',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'totnleach','annual total inorganic nitrogen leaching',
     >    'kg/hectare',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'no3leach','annual total nitrate leaching',
     >    'kg/hectare',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'nbalance','annual inorganic nitrogen balance',
     >    'kg/hectare',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'drainage','annual total drainage',
     >    'mm/year',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'no3conc','flow-weighted mean nitrate concentration',
     >    'mg/liter',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'natvegnup','total annual inorganic nitrogen uptake by natural veg',
     >    'kg/m2/yr',3,dimnames,OCEAN,istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'insoiln','inorganic, immobile soil n to specified layer',
c     >    'kg/ha/day',3,dimnames,OCEAN,istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'insolutn','inorganic, mobile soil n in solution to specified layer',
c     >    'kg/ha/day',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (ayanlit, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totanlit',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totanlit'
         stop 1
      end if
      call vec2arr (aybnlit, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totrnlit',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totrnlit'
         stop 1
      end if
      call vec2arr (aynsoi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totnsoi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totnsoi'
         stop 1
      end if
      call vec2arr (aynmintot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'nmin',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nmin'
         stop 1
      end if
      call vec2arr (ayimmtot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'nimmob',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nimmob'
         stop 1
      end if
      call vec2arr (ydeposn, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'ndepos',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, ndepos'
         stop 1
      end if
      call vec2arr (yfixsoin, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'nfixnat',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nfixnat'
         stop 1
      end if
      call vec2arr (ftot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totnleach',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totnleach'
         stop 1
      end if
      call vec2arr (yno3leach, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'no3leach',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, no3leach'
         stop 1
      end if
      call vec2arr (snbalance, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'nbalance',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, nbalance'
         stop 1
      end if
      call vec2arr (drntot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'drainage',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, drainage'
         stop 1
      end if
c
      call vec2arr (concn, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'no3conc',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, no3conc'
         stop 1
      end if
c
      call vec2arr (totnvegn, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'natvegnup',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, natvegnup'
         stop 1
      end if
c
c      call vec2arr (tsnimm, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'insoiln',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, insoiln'
c         stop 1
c      end if
c      call vec2arr (tsnmob, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'insolutn',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wmonthly, insolutn'
c         stop 1
c      end if
cc
cc total litter
cc
c      filen = 'output/yearly/totlit.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual total litter carbon',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'totlit','total litter carbon',
c     >    'kg/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (totlit, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'totlit',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, totlit'
c         stop 1
c      end if
cc
cc total wood litter
cc
c      filen = 'output/yearly/clitw.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual total wood litter carbon',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'clitw','total wood litter carbon',
c     >    'kg/m^2',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (clitw, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'clitw',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, clitw'
c         stop 1
c      end if
cc
cc total litterfall
cc
      filen = 'output/yearly/totfall.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total litterfall carbon',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'totfall','total litterfall',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (totfall, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'totfall',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, totfall'
         stop 1
      end if
c
cc total soil carbon in slow pool
cc
      filen = 'output/yearly/csoislo.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total soil carbon in slow pool',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'csoislo','total soil carbon in slow pool',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (csoislo, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'csoislo',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, csoislo'
         stop 1
      end if
cc
cc total soil carbon in passive pool
cc
      filen = 'output/yearly/csoipas.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total soil carbon in pasive pool',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'csoipas','total soil carbon in passive pool',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (csoipas, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'csoipas',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, csoipas'
         stop 1
      end if
c
c co2 carbon exchange: net ecosystem, microbial resp, root resp, soil resp
c
      filen = 'output/yearly/co2fluxes.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual total carbon from exchange of co2',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'neetot',
     >    'total net ecosystem echange carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'co2mic','total microbe respiration carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'co2root','total root respiration carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         dimnames(3) = 'time'
         call inivar(idies,'co2soi','total soil respiration carbon',
     >    'kg/m^2',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (ayneetot, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'neetot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, neetot'
         stop 1
      end if
      call vec2arr (ayco2mic, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'co2mic',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, co2mic'
         stop 1
      end if
      call vec2arr (ayco2root, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'co2root',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, co2root'
         stop 1
      end if
      call vec2arr (ayco2soi, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'co2soi',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, co2soi'
         stop 1
      end if
c
c fire disturbance regime
c
c      filen = 'output/yearly/disturbf.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual fire disturbance regime',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
c     >    latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'disturbf','fire disturbance regime',
c     >    'fraction/year',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c       end if
c      call vec2arr (disturbf, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'disturbf',istart,icount,cdummy,ftime,
c      > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, disturbf'
c         stop 1
c      end if
c
c vegetation type
c
      filen = 'output/yearly/vegtype0.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual vegetation type - ibis classification',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,
     >    latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'vegtype0','vegetation type',
     >    '_',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (vegtype0, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'vegtype0',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, vegtype0'
         stop 1
      end if
c
c fractional cover of upper and lower canopies
c
      filen = 'output/yearly/fcover.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual fractional cover of canopies','ibis wyearly',
     >    cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
     >    'none',1,0.,'',tunits,'gregorian',istat)
         dimnames(3) = 'time'
         call inivar(idies,'fu','fractional cover of upper canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         call inivar(idies,'fl','fractional cover of lower canopy',
     >    'fraction',3,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      call vec2arr (fu, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'fu',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, fu'
         stop 1
      end if
      call vec2arr (fl, cdummy)
      istart(3) = mstep
      icount(3) = 1
      call writevar(filen,'fl',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, fl'
         stop 1
      end if
cc
cc sapwood fraction
cc
c      filen = 'output/yearly/sapfrac.nc'
c      if (mstep .eq. 1) then
c         call inifile(idies,filen,
c     >    'annual sapwood fraction',
c     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,'','none',
c     >    'none',1,0.,'',tunits,'gregorian',istat)
c         dimnames(3) = 'time'
c         call inivar(idies,'sapfrac','sapwood fraction',
c     >    'fraction',3,dimnames,OCEAN,istat)
c         call endini(idies,istat)
c      end if
c      call vec2arr (sapfrac, cdummy)
c      istart(3) = mstep
c      icount(3) = 1
c      call writevar(filen,'sapfrac',istart,icount,cdummy,ftime,
c     > tweight,tdate,istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in wyearly, sapfrac'
c         stop 1
c      end if
c
c bottom and top of vegetation canopies
c
      filen = 'output/yearly/zcanopy.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual height of vegetation canopies',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'canopy','canopy','_',2,pindex,'up',
     >    tunits,'gregorian',istat)
c add global attribute to define canopies with text, use netcdf low-level com
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'canopy_def',
     >    26+5,'1='//canopies(1)//' 2='//canopies(2))
         dimnames(3) = 'canopy'
         call inivar(idies,'zbot',
     >    'bottom heights of lower and upper canopies',
     >    'meters',4,dimnames,OCEAN,istat)
         call inivar(idies,'ztop',
     >    'top heights of lower and upper canopies',
     >    'meters',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 20 k = 1, 2
         call vec2arr (zbot(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 20   continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = 2
      call writevar(filen,'zbot',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, zbot'
         stop 1
      end if
      do 25 k = 1, 2
         call vec2arr (ztop(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 25   continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = 2
      call writevar(filen,'ztop',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, ztop'
         stop 1
      end if
c
c existence of pfts
c
      filen = 'output/yearly/exist.nc'
      if (mstep .eq. 1) then
         call inifile(idies,filen,
     >    'annual existence of each plant functional type',
     >    'ibis wyearly',cdate,nlonsub,lonscale,nlatsub,latscale,
     >    'pft','plant fuctional type','_',npft,pindex,'',
     >    tunits,'gregorian',istat)
c add global attribute to define pfts with text, use netcdf low-level command
         istat = NF_PUT_ATT_TEXT(idies,NF_GLOBAL,'pft_definition',
     >    npft*80,pftdef)
         dimnames(3) = 'pft'
         call inivar(idies,'exist','existence for each pft',
     >    '_',4,dimnames,OCEAN,istat)
         call endini(idies,istat)
      end if
      do 30 k = 1, npft
         call vec2arr (exist(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
 30   continue
      istart(3) = 1
      istart(4) = mstep
      icount(3) = npft
      call writevar(filen,'exist',istart,icount,cdummy,ftime,
     > tweight,tdate,istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in wyearly, exist'
         stop 1
      end if
c
      return
      end
c
c
c ---------------------------------------------------------------------
      subroutine readit (isimveg,snorth,ssouth,swest,seast,iwest,jnorth)
c ---------------------------------------------------------------------
c
c reads in initialization files and initializes some fields
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comsoi.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer isimveg,  ! dynamic vegetation (1) or static (0)
     >        iwest,    !
     >        jnorth    !       
c
      real snorth, ssouth, swest, seast
c
c Local variables
c

      integer  istat,    ! netcdf error flag
     > i, j, ntime,      ! loop indices
     > jj, ii, 
     > ndim,             ! number of dimensions
     > nlpoints,         ! number of land points
     > ldim
c
      real xlat,lats(nlat),lons(nlon)
c
      integer jsouth, ieast
      real xmask(nlon,nlat)
      equivalence ( xmask(1,1), work(1) )
c
      character*80 filen
      character*39 pathn
c
      integer istart(4), icount(4) ! for reading vars
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
c
c 2-d surface and vegetation arrays
c

	istart(1)=1
	istart(2)=1
	istart(3)=1
	istart(4)=1


      icount(1) = nlon
      icount(2) = nlat
      icount(3) = 1
      icount(4) = 1


c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c land mask, latitudes, and longitudes
c
c     filen = pathn//'surta.nc'


	print*,'read surta casattt',istart,icount

      filen = 'input/surta.nc'
      aname = 'surta'

      call readvar(filen,aname,'level',istart,icount,
     > xmask,lonscale,latscale,cdummy(1),cdummy(ndim4),istat)
      if (istat.lt.0) then
         write (*,9000)
         print *, 'while reading surta'
         stop 1
      end if
c
c
      if (abs(abs(lonscale(1)-lonscale(2))-xres).gt.0.001 .or.
     >    abs(abs(latscale(1)-latscale(2))-yres).gt.0.001) then
         write (*,9000)
         write(*,*) 'resolution mismatch!'
         write(*,*) 'xres, yres in compar.h = ', xres, yres
         write(*,*) 'xres, yres from input/surta.nc = ',
     >   abs(lonscale(1)-lonscale(2)), abs(latscale(1)-latscale(2))
         stop 1
      end if
c
c subset the grid if not default whole grid
c
      if (snorth.lt.latscale(1) .or. ssouth.gt.latscale(nlat) .or.
     >     swest.gt.lonscale(1) .or.  seast.lt.lonscale(nlon)) then
        jnorth = 0
        if (snorth .lt. (latscale(nlat)+latscale(nlat-1))/2.) then
          jnorth = nlat
        else if (snorth .ge. (latscale(1)+latscale(2))/2.) then
          jnorth = 1
        else
          do 1 j = nlat-1,1,-1
            if (snorth .ge. (latscale(j)+latscale(j+1))/2.) jnorth = j
 1        continue
        end if
        jsouth = 0
        if (ssouth .lt. (latscale(nlat)+latscale(nlat-1))/2.) then
          jsouth = nlat
        else if (ssouth .ge. (latscale(1)+latscale(2))/2.) then
          jsouth = 1
        else
          do 2 j = nlat-1,1,-1
            if (ssouth .ge. (latscale(j)+latscale(j+1))/2.) jsouth = j
 2        continue
        end if
c
        iwest = 0
        if (swest .lt. (lonscale(1)+lonscale(2))/2.) then
          iwest = 1
        else if (swest .ge. (lonscale(nlon)+lonscale(nlon-1))/2.) then
          iwest = nlon
        else
          do 3 i = 2, nlon
            if(swest .ge. (lonscale(i)+lonscale(i-1))/2.) iwest=i
 3        continue
        end if
        ieast = 0
        if (seast .lt. (lonscale(1)+lonscale(2))/2.) then
          ieast = 1
        else if (seast .ge. (lonscale(nlon)+lonscale(nlon-1))/2.) then
          ieast = nlon
        else
          do 4 i = 2, nlon
            if(seast .ge. (lonscale(i)+lonscale(i-1))/2.) ieast=i
 4        continue
        end if
        nlonsub = ieast - iwest + 1
        nlatsub = jsouth - jnorth + 1
        istart(1) = iwest
        icount(1) = nlonsub
        istart(2) = jnorth
        icount(2) = nlatsub
      else
        iwest = 1
        ieast = nlon
        jnorth = 1
        jsouth = nlat
        nlonsub = nlon
        nlatsub = nlat
      end if

csant       print *, swest, seast, snorth, ssouth,nlonsub,nlatsub
csantagora       print *, iwest, ieast, jnorth, jsouth
csant  	stop
c
c
c initialize lonindex, latindex for use in arr2vec, vec2arr, etc.
c and calculate the approximate the area of the land gridcells
c
      nlpoints = 0
c
c here, i/j refers to entire grid (1 to nlon/nlat), 
c ii/jj refers to subgrid (1 to nlonsub/nlatsub)
c
      do 5 j = jnorth, jsouth
c
        jj = j - jnorth + 1
        do 6 i = iwest, ieast
c
          ii = i - iwest + 1
          lmask(ii,jj) = nint(xmask(i,j))
c
          if (lmask(ii,jj).eq.1) then
c
            nlpoints = nlpoints + 1
            lonindex(nlpoints) = ii
            latindex(nlpoints) = jj
            xlat = latscale(j) * pi / 180.0
            garea(nlpoints) = yres * 111400.0 * xres * 111400.0 *
     >                        cos(xlat)

          end if
c
 6      continue
 5    continue


            nlpoints =0
 	do j = 1,nlat
	lats(j)=snorth + 0.25 - 0.5*(j-1)
	enddo
	do i=1,nlon
	lons(i)=swest  +0.25 + 0.5*(i-1)
	enddo

        do  j = jnorth, jsouth
        jj = j - jnorth + 1
        do i = iwest, ieast
          ii = i - iwest + 1

          if (lmask(ii,jj).eq.1) then
            nlpoints = nlpoints + 1	

	 if(lats(jj).gt.-9.5.and.lons(ii).gt.-37.5)then
	 pmask(nlpoints)=1
	 elseif(lats(jj).gt.-17.and.lats(jj).le.-9.5.and.lons(ii).gt.-41)then
	 pmask(nlpoints)=1
	 else
	 pmask(nlpoints )=2
	 endif

	endif

	enddo
	enddo

        do j = scpft, ecpft 
		do i=1,npoi
		if(j.eq.16.and.pmask(i).eq.1) then
	print*,' points wth dif. P. dates (dif. btw Southeast and Northeas Brazil) ',i
	  	pmmin(i,j)=pmmin(i,j)+3
	   pcm(i,j)=mod(pmmin(i,j)+(mxmat(j)/30),12.)+1.
      print*,'point ',i,' sugarcane crop year star/ends - month:',pcm(i,16)
      print*,'sugarcane for the point ',i,' will be harvest between the months',
     > mod(pmmin(i,16)+(mxmat(16)/30.)-3,12.)+1,mod(pmmin(i,16)+(mxmat(16)/30)-1,12.)+1
      print*,' '

		endif

		enddo
	enddo




cc      print *, jnorth, jsouth, iwest, ieast
cc      print *, latscale(jnorth),latscale(jsouth),lonscale(iwest),lonscale(ieast)
cc      print *, ((lmask(i,j),i=1,nlonsub),j=1,nlatsub)
cc      print *, istart
cc      print *, icount
c
      do 7 j = jnorth, jsouth
        jj = j - jnorth + 1
        latscale(jj) = latscale(j)
 7    continue
c
      do 8 i = iwest, ieast
        ii = i - iwest + 1
        lonscale(ii) = lonscale(i)
 8    continue
c
cc      print *, (lonscale(i),i=1,nlonsub)
cc      print *, (latscale(i),i=1,nlatsub)
cc      print *, lonindex
cc      print *, latindex
c
      if (nlpoints .ne. npoi) then
         write(*,9000)
         write(*,*) 'number of land points in input/surta.nc'
         write(*,*) 'does not match number of land points in compar.h'
         write(*,*) 'in surta =',nlpoints,' in compar.h =',npoi
         stop 1
      else
         write (*,9010) 
         write (*,9020) nlpoints
         write (*,9010) 
      end if
cc
cc dummy variable example, 4-d, but 3rd dim ('level') = 1
cc copy and chanve for a new variable
cc
c      filen = 'input/dummyv.nc'
c      aname = 'dummyv'
c      call readvar(filen,aname,'level',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c         write(*,9000)
c         print *, 'while reading dummyv'
c         stop 1
c      end if
c      call arr2vec (cdummy, xindummy)
c
c tminavgann 
c
c     filen = pathn//'tminavgann.nc'
csant- ---- have to change urgently!!!!---- or creat a global data set or use another var....
csant- no problem for instance, since it affects only natural vegetation.
csant      filen = 'input/tminavgann.nc'
csant      aname = 'tminavgann'
csant      call readvar(filen,aname,'level',istart,icount,
csant     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant      if (istat.lt.0) then
csant         write(*,9000)
csant         print *, 'while reading tminavgann'
csant         stop 1
csant      end if
csant      call arr2vec (cdummy, tminavgann)
	do i = 1, npoi
	tminavgann(i)=0.0
	enddo
csant- this relations above is has no reanson, just to goes through now, change it!!

c
c average crop planting date - held constant
c
c      filen = pathn//'pdate.ave.nc'
c      aname = 'pdate'
c      call readvar(filen,aname,'level',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c         write(*,9000)
c         print *, 'while reading pdate'
c         stop 1
c      end if
c      call arr2vec (cdummy, xinavepdate)
c
c average corn hybrid- held constant
c
c      filen = pathn//'hybrid.ave.nc'
c      aname = 'hybrid'
c      call readvar(filen,aname,'level',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c         write(*,9000)
c         print *, 'while reading hybrid'
c         stop 1
c      end if
c      call arr2vec (cdummy, xinavehybrid)
c
c topography
c
c     filen = pathn//'topo.nc'

	print*,'read surta casattt2',istart,icount

      filen = 'input/topo.nc'
      aname = 'topo'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading topo'
         stop 1
      end if
      call arr2vec (cdummy, xintopo)
c
c fixed vegetation map
c
	print*,'read surta casattt3',istart,icount

      if (isimveg .le. 1) then
c        filen = pathn//'vegtype.nc'
         filen = 'input/vegtype.nc'
         aname = 'vegtype'
         call readvar(filen,aname,'level',istart,icount,cdummy,
     >    work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
         if (istat.lt.0) then
            write(*,9000)
            print *, 'while reading vegtype'
            stop 1
         end if
         call arr2vec (cdummy, xinveg)
      end if
cc 2-d soil array
cc
c     filen = 'nput/soil.nc'
c     aname = 'soil'
c     call readvar(filen,aname,'level',istart,icount,
c    > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c     if (istat.lt.0) then
c        write(*,9000)
c        print *, 'while reading soil'
c        stop 1
c     end if
c     call arr2vec (cdummy, soita)
c
c delta t
c
c     filen = pathn//'deltat.nc'
      filen = 'input/deltat.nc'
      aname = 'deltat'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading deltat'
         stop 1
      end if
      call arr2vec (cdummy, deltat)
c
c 3-d soil texture array
c
c icount(3) is the 11 layers used in soil_text.nc
ccsant- to use again these, I have to generate maps global maps for soil_text.nc!!!!!!!!!!!!1
csant      icount(3) = 11 
csant      icount(4) = 1
c     filen = pathn//'soil_text.nc'
csant      filen = 'input/soil_text.nc'
csant      aname = 'domtext'
csant      call readvar(filen,aname,'layer',istart,icount,
csant     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant      if (istat.lt.0) then
csant         write(*,9000)
csant         print *, 'while reading soil_text'
csant         stop 1
csant      end if
csant      do 14 j = 1, nsoilay
csant        call arr2vec (cdummy((j-1)*nlonsub*nlatsub + 1), domtext(1,j))
csant 14   continue
c
      icount(3) = 6 
      icount(4) = 1
c     filen = pathn//'soita.sand.nc'
      filen = 'input/soita.sand.nc'
      aname = 'sandpct'
      call readvar(filen,aname,'layer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading soita.sand'
         stop 1
      end if
      do 12 j = 1, 6
csant- I change here, i dont why was clay(1,j) instead of sand(1,j)!!!
        call arr2vec (cdummy((j-1)*nlonsub*nlatsub + 1), sand(1,j))
 12   continue
c
      icount(3) = 6 
      icount(4) = 1
c     filen = pathn//'soita.clay.nc'
      filen = 'input/soita.clay.nc'
      aname = 'claypct'
      call readvar(filen,aname,'layer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading soita.clay'
         stop 1
      end if
      do 13 j = 1, nsoilay
        call arr2vec (cdummy((j-1)*nlonsub*nlatsub + 1), clay(1,j))
 13   continue
c
c 3-d climate arrays
c
      icount(3) = 1
      icount(4) = 12
c
c     filen = pathn//'wetd.mon.nc'
      filen = 'input/wetd.mon.nc'
      aname = 'wetd'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading wetd'
         stop 1
      end if
      do 15 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmwet(1,ntime))
 15   continue
c
c     filen = pathn//'temp.mon.nc'
      filen = 'input/temp.mon.nc'
      aname = 'temp'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading temp'
         stop 1
      end if
      do 20 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmt(1,ntime))
 20   continue
c
c     filen = pathn//'trange.mon.nc'
      filen = 'input/trange.mon.nc'
      aname = 'trange'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading trange'
         stop 1
      end if
      do 25 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmtrng(1,ntime))
 25   continue
c
c     filen = pathn//'prec.mon.nc'
      filen = 'input/prec.mon.nc'
      aname = 'prec'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading prec'
         stop 1
      end if
      do 30 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmprec(1,ntime))
 30   continue
c
c     filen = pathn//'wspd.mon.nc'
      filen = 'input/wspd.mon.nc'
      aname = 'wspd'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading wspd'
         stop 1
      end if
      do 35 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    xinwind(1,ntime))
 35   continue
c
c     filen = pathn//'cld.mon.nc'
      filen = 'input/cld.mon.nc'
      aname = 'cld'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading cld'
         stop 1
      end if
      do 40 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmcld(1,ntime))
 40   continue
c
c     filen = pathn//'rh.mon.nc'
      filen = 'input/rh.mon.nc'
      aname = 'rh'
      call readvar(filen,aname,'level',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat.lt.0) then
         write(*,9000)
         print *, 'while reading rh'
         stop 1
      end if
      do 45 ntime = 1,12
         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
     >    clmq(1,ntime))
 45   continue
c
c
c input datasets for crop N-fertilizer usage 
c values for fertilizer start in 1950 and go through 1999
c maize
csant- dont have this file in global scale!!
csant      icount(3) = 1
csant      icount(4) = 51
c     filen = pathn//'frate.corn.nc'
csant      filen = 'input/frate.corn.nc'
csant      aname = 'frate'
csant      call readvar(filen,aname,'level',istart,icount,
csant     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant      if (istat.lt.0) then
csant         write(*,9000)
csant         print *, 'while reading n rate'
csant         stop 1
csant      end if
      do 46 ntime = 1,51
	do i=1,npoi
csant- the file above have this valeu constant along all US, so for instance I keep it (but have to be changed)
	fertmaize(i,ntime) = 5.76626
	fertsgc(i,ntime) = 2*5.76626
	enddo
csant         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
csant     >    fertmaize(1,ntime))
c- for instance, the sugarcane fertilizer is the same as maize
csant         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
csant     >    fertsgc(1,ntime))

 46   continue
c


c soybeans
c
csant      icount(3) = 1
csant      icount(4) = 51
c     filen = pathn//'frate.soy.nc'
csant      filen = 'input/frate.soy.nc'
csant      aname = 'frate'
csant      call readvar(filen,aname,'level',istart,icount,
csant     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant      if (istat.lt.0) then
csant         write(*,9000)
csant         print *, 'while reading n rate'
csant         stop 1
csant      end if
        do 47 ntime = 1,51
	do i=1,npoi
csant- the file above have this valeu constant along all US, so for instance I keep it (but have to be changed)
	fertsoy(i,ntime) =  2.24803
	enddo
csant         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
csant     >    fertsoy(1,ntime))
 47   continue
c
c wheat
c
csant-      icount(3) = 1
csant-      icount(4) = 51
c     filen = pathn//'frate.wheat.nc'
csant-      filen = 'input/frate.wheat.nc'
csant-      aname = 'frate'
csant-      call readvar(filen,aname,'level',istart,icount,
csant-     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant-      if (istat.lt.0) then
csant-         write(*,9000)
csant-         print *, 'while reading n rate'
csant-         stop 1
csant-      end if
      do 48 ntime = 1,51
	do i=1,npoi
csant- the file above have this valeu constant along all US, so for instance I keep it (but have to be changed)
	fertwheat(i,ntime) = 3.15461
	enddo
csant-         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
csant-     >    fertwheat(1,ntime))
 48   continue
 
c time-dependent nitrogen deposition fertilizer dataset 
c begins in 1940, through 2000 
c
csant      icount(3) = 1
csant      icount(4) = 60 
c     filen = pathn//'ndep.1940.2000.nc'
csant      filen = 'input/ndep.1940.2000.nc'
csant      aname = 'ndep'
csant      call readvar(filen,aname,'level',istart,icount,
csant     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
csant      if (istat.lt.0) then
csant         write(*,9000)
csant         print *, 'while reading ndep '
csant         stop 1
csant      end if
      do 53  ntime = 1,60
	do i=1,npoi
csant- the file above have this valeu constant along all US, so for instance I keep it (but have to be changed)
	ndepfact(i,ntime) =  0.5
	enddo
csant         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
csant     >    ndepfact(1,ntime))
 53   continue
c
c time-dependent planting date information for corn from
c control runs 
c i, time
c begins in 1940, through 1960
c
c       istart(1) = 1  
c       istart(2) = 1   
c       istart(3) = 14       ! maize pft
c       istart(4) = 1  
c
c      icount(1) = 53       ! nlons in output
c      icount(2) = 34       ! nlats in output
c       icount(1) = 2       ! nlons in output
c       icount(2) = 2       ! nlats in output
c       icount(3) = 1        ! number of pfts
c       icount(4) = 21       ! number of years to read 
c
c      filen = pathn//'crops.input.nc'
c      aname = 'plantdate'
c      call readvar(filen,aname,'level',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c          write(*,9000)
c         print *, 'while reading plantdate'
c         stop 1
c      end if
c      do 533  ntime = 1, icount(4)
c         call arr2vec (cdummy((ntime-1)*nlonsub*nlatsub + 1),
c     >    xinpdate(1,ntime))
c 533   continue
c
c read in gdd information - use gddfzcorn to determine typical hybrid  
c to plant for the whole period in holding constant 
c
c      istart(3) = 1        ! maize pft
c      icount(3) = 1
c      istart(4) = 6577 
c      icount(4) = 1 
c
c      filen = pathn//'gdd.input.nc'
c      aname = 'gddfzcorn'
c      call readvar(filen,aname,'level',istart,icount,
c     > adummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c         write(*,9000)
c         print *, 'while reading gddfzcorn '
c         stop 1
c      end if
c      do 540  ntime = 1,  icount(4) 
c         call arr2vec (adummy((ntime-1)*nlonsub*nlatsub + 1),
c     >    xinhybrid(1,ntime))
c          write(*,*) xinhybrid(1,ntime)
c 540   continue
c
c copy all 5 climatology fields to clm+anom fields for spin up
c
      ndim = npoi*12
      call scopy (ndim, clmt, xint)
      call scopy (ndim, clmtrng, xintrng)
      call scopy (ndim, clmprec, xinprec)
      call scopy (ndim, clmcld, xincld)
      call scopy (ndim, clmq, xinq)
      call scopy (ndim, clmwet, xinwet)
c
 9000 format (1x,'ERROR in subroutine readit')
 9010 format (1x,' ')
 9020 format (1x,'number of land points: ', i10)
c
c return to main program
c
      return
      end
c 
c
c ---------------------------------------------------------------------
      subroutine restart (iyrlast)
c ---------------------------------------------------------------------
c
c reads in restart files, initializes some variables
c
c this subroutine reads the restart values of:
c
c  fsnocov = fractional snow cover
c  tsno    = temperature of snow
c  hsno    = snow depth
c  tsoi    = soil temperature
c  wisoi   = soil ice content
c  wsoi    = soil moisture content
c  smsoil = immobile soil inorganic nitrogen pool for plant uptake
c  smsoln = mobile soil inorganic nitrogen pool for plant uptake, and leachable 
c  cbiol   = carbon in leaf biomass pool
c  cbiow   = carbon in woody biomass pool
c  cbior   = carbon in fine root biomass pool
c  cntops  = c/n ratio of plant tops (crops)
c  cnroot  = c/n ratio of crop roots
c  sapfrac = sapwood fraction
c  decompl = litter decomposition factor
c  decomps = soil decomposition factor
c  clitlm  = leaf metabolic litter
c  clitls  = leaf structural litter
c  clitll  = leaf lignin litter
c  clitrm  = root metabolic litter
c  clitrs  = root structural litter
c  clitrl  = root lignin litter
c  clitwm  = woody metabolic litter
c  clitws  = woody structural litter
c  clitwl  = woody lignin litter
c  falll   = annual leaf litterfall 
c  fallr   = annual fine root turnover
c  fallw   = annual woody turnover
c  totcmic = total microbial carbon
c  csoislop= slow soil carbon, protected humus
c  csoislon= slow soil carbon, nonprotected humus
c  csoipas = passive soil carbon
c  gdd0    = growing degree days 0
c  gdd0c   = growing degree days 0 for wheat - accumulated April 1 - Sept 30
c  gdd5    = growing degree days 5
c  gdd8    = growing degree days 8 
c  gdd10   = growing degree days 10 
c  aplantn = available plant nitrogen
c  tc      = coldest monthly temperature
c  tw      = warmest monthly temperature
c  wipud   = ice content of puddles per soil area
c  wpud    = liquid content of puddles per soil area
c  agddu   = annual accumulated growing degree days for bud burst, upper canopy
c  agddl   = annual accumulated growing degree days for bud burst, lower canopy
c  tempu   = cold-phenology trigger for trees
c  templs  = cold-phenology trigger for shrubs
c  greenfracl3 = fraction of green vegetation in C3 grasses
c  greenfracl4 = fraction of green vegetation in C4 grasses
c  Tavgann = average annual air temperature
c  PPTavgann = average annual precipitation
c  a10td    = 10-day avg daily temp
c  a10ts    = 10-day avg daily soil (1) temp
c  a10ancub = 10-day average canopy photosynthesis rate - broadleaf
c  a10ancuc = 10-day average canopy photosynthesis rate - conifer
c  a10ancls = 10-day average canopy photosynthesis rate - shrubs
c  a10ancl4 = 10-day average canopy photosynthesis rate - c4 grasses
c  a10ancl3 = 10-day average canopy photosynthesis rate - c3 grasses
c  a10scalparamu = 10-day average canopy scaling parameter - upper canopy
c  a10scalparaml = 10-day average canopy scaling parameter - lower canopy
c  a10daylightu = 10-day average daylight - upper canopy
c  a10daylightl = 10-day average daylight - lower canopy
c  a11soiltd = 11-day average surface soil temperature
c  a3tdmin = 3-day daily minimum air temperature
c (NOTE: a10ancuc is not used at this point, so its restart entry 
c is commented out)
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comsoi.h'
      include 'comsno.h'
      include 'combcs.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c Arguments
c
      integer iyrlast
c
c Local variables
c
      integer istart(4), icount(4) ! for reading restart vars
c
      character*20 fdir  ! used to construct odd/even file names
      character*80 filen ! file name
c
      integer  lf,           ! number of characters in directory name
     >         i,            ! loop indices
     >         istat,        ! error flag for netcdf
     >         nlevel        ! loop indice on the soil layer
c
c External
c
      integer lenchr,       ! Function: Find length of character string
     >  NF_PUT_ATT_TEXT,    ! netcdf function
     >  NF_GLOBAL           ! netcdf function
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c check to see if iyrlast is odd or even and read from the appropriate
c files for that year
c
      if (mod(iyrlast,2) .eq. 0) then
         fdir = 'restart/even'
      else
         fdir = 'restart/odd'
      end if
      lf = lenchr(fdir)
cc
cc dummy variable example, 3-d - copy and change for ne variable
cc
c      icount(3) = 1
c      filen = fdir(1:lf)//'dummyv.nc'
c      call readvar(filen,'dummyv','',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in restart, dummyvv'
c         stop 1
c      end if
c      call arr2vec (cdummy, dummyv)
cc
cc dummy variable example, 4-d, 3rd dim (snowlayer) = nsnolay
cc
c      icount(3) = nsnolay
c      filen = fdir(1:lf)//'dummyv.nc'
c      call readvar(filen,'dummyv','snowlayer',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in restart, dummyv'
c         stop 1
c      end if
c      do 5 nlevel = 1,nsnolay
c         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
c     >    dummyv(1,nlevel))
c 5    continue
c
c fsnocov
c
      filen = fdir(1:lf)//'fsnocov.nc'
      icount(3) = 1
      call readvar(filen,'fsnocov','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, fsnocov'
         stop 1
      end if
      call arr2vec (cdummy, fi)
c
c nsnolay variables: tsno and hsno
c
      icount(3) = nsnolay
c
      filen = fdir(1:lf)//'tsno.nc'
      call readvar(filen,'tsno','snowlayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, tsno'
         stop 1
      end if
      do 5 nlevel = 1,nsnolay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    tsno(1,nlevel))
 5    continue
c
      filen = fdir(1:lf)//'hsno.nc'
      call readvar(filen,'hsno','snowlayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, hsno'
         stop 1
      end if
      do 10 nlevel = 1,nsnolay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    hsno(1,nlevel))
 10   continue
c
c nsoilay variables: tsoi, wisoi, wsoi
c and soil nitrogen
c
      icount(3) = nsoilay
c
      filen = fdir(1:lf)//'tsoi.nc'
      call readvar(filen,'tsoi','soillayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, tsoi'
         stop 1
      end if
      do 15 nlevel = 1,nsoilay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    tsoi(1,nlevel))
 15   continue
c
      filen = fdir(1:lf)//'wisoi.nc'
      call readvar(filen,'wisoi','soillayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, wisoi'
         stop 1
      end if
      do 20 nlevel = 1,nsoilay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    wisoi(1,nlevel))
 20   continue
c
      filen = fdir(1:lf)//'wsoi.nc'
      call readvar(filen,'wsoi','soillayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, wsoi'
         stop 1
      end if
      do 25 nlevel = 1,nsoilay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    wsoi(1,nlevel))
 25   continue
c
c soil nitrogen variables
c
      filen = fdir(1:lf)//'smsoil.nc'
      call readvar(filen,'smsoil','soillayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, smsoil'
         stop 1
      end if
      do 26 nlevel = 1,nsoilay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    smsoil(1,nlevel))
 26   continue
c
      filen = fdir(1:lf)//'smsoln.nc'
      call readvar(filen,'smsoln','soillayer',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, smsoln'
         stop 1
      end if
      do 27 nlevel = 1,nsoilay
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    smsoln(1,nlevel))
 27   continue
c
c npft variables
c
      icount(3) = npft
c
      filen = fdir(1:lf)//'cbiol.nc'
      call readvar(filen,'cbiol','pft',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, cbiol'
         stop 1
      end if
      do 30 nlevel = 1,npft
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    cbiol(1,nlevel))
 30   continue
c
      filen = fdir(1:lf)//'cbiow.nc'
      call readvar(filen,'cbiow','pft',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, cbiow'
         stop 1
      end if
      do 35 nlevel = 1,npft
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    cbiow(1,nlevel))
 35   continue
c
      filen = fdir(1:lf)//'cbior.nc'
      call readvar(filen,'cbior','pft',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, cbior'
         stop 1
      end if
      do 40 nlevel = 1,npft
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    cbior(1,nlevel))
 40   continue
c
      filen = fdir(1:lf)//'cntops.nc'
      call readvar(filen,'cntops','pft',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, cntops'
         stop 1
      end if
      do 42 nlevel = 1,npft
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    cntops(1,nlevel))
 42   continue
c
      filen = fdir(1:lf)//'cnroot.nc'
      call readvar(filen,'cnroot','pft',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, cnroot'
         stop 1
      end if
      do 45 nlevel = 1,npft
         call arr2vec (cdummy((nlevel-1)*nlonsub*nlatsub + 1),
     >    cnroot(1,nlevel))
 45   continue
c
c single level variables
c
      icount(3) = 1
c
      filen = fdir(1:lf)//'sapfrac.nc'
      call readvar(filen,'sapfrac','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, sapfrac'
         stop 1
      end if
      call arr2vec (cdummy, sapfrac)
c
      filen = fdir(1:lf)//'decompl.nc'
      call readvar(filen,'decompl','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, decompl'
         stop 1
      end if
      call arr2vec (cdummy, decompl)
c
      filen = fdir(1:lf)//'decomps.nc'
      call readvar(filen,'decomps','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, decomps'
         stop 1
      end if
      call arr2vec (cdummy, decomps)
c
      filen = fdir(1:lf)//'clitlm.nc'
      call readvar(filen,'clitlm','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitlm'
         stop 1
      end if
      call arr2vec (cdummy, clitlm)
c
      filen = fdir(1:lf)//'clitls.nc'
      call readvar(filen,'clitls','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitls'
         stop 1
      end if
      call arr2vec (cdummy, clitls)
c
      filen = fdir(1:lf)//'clitll.nc'
      call readvar(filen,'clitll','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitll'
         stop 1
      end if
      call arr2vec (cdummy, clitll)
c
      filen = fdir(1:lf)//'clitrm.nc'
      call readvar(filen,'clitrm','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitrm'
         stop 1
      end if
      call arr2vec (cdummy, clitrm)
c
      filen = fdir(1:lf)//'clitrs.nc'
      call readvar(filen,'clitrs','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitrs'
         stop 1
      end if
      call arr2vec (cdummy, clitrs)
c
      filen = fdir(1:lf)//'clitrl.nc'
      call readvar(filen,'clitrl','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitrl'
         stop 1
      end if
      call arr2vec (cdummy, clitrl)
c
      filen = fdir(1:lf)//'clitwm.nc'
      call readvar(filen,'clitwm','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitwm'
         stop 1
      end if
      call arr2vec (cdummy, clitwm)
c
      filen = fdir(1:lf)//'clitws.nc'
      call readvar(filen,'clitws','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitws'
         stop 1
      end if
      call arr2vec (cdummy, clitws)
c
      filen = fdir(1:lf)//'clitwl.nc'
      call readvar(filen,'clitwl','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, clitwl'
         stop 1
      end if
      call arr2vec (cdummy, clitwl)
c
      filen = fdir(1:lf)//'falll.nc'
      call readvar(filen,'falll','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, falll'
         stop 1
      end if
      call arr2vec (cdummy, falll)
c
      filen = fdir(1:lf)//'fallr.nc'
      call readvar(filen,'fallr','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, fallr'
         stop 1
      end if
      call arr2vec (cdummy, fallr)
c
      filen = fdir(1:lf)//'fallw.nc'
      call readvar(filen,'fallw','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, fallw'
         stop 1
      end if
      call arr2vec (cdummy, fallw)
c
      filen = fdir(1:lf)//'totcmic.nc'
      call readvar(filen,'totcmic','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, totcmic'
         stop 1
      end if
      call arr2vec (cdummy, totcmic)
c
      filen = fdir(1:lf)//'csoislop.nc'
      call readvar(filen,'csoislop','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, csoislop'
         stop 1
      end if
      call arr2vec (cdummy, csoislop)
c
      filen = fdir(1:lf)//'csoislon.nc'
      call readvar(filen,'csoislon','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, csoislon'
         stop 1
      end if
      call arr2vec (cdummy, csoislon)
c
      filen = fdir(1:lf)//'csoipas.nc'
      call readvar(filen,'csoipas','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, csoipas'
         stop 1
      end if
      call arr2vec (cdummy, csoipas)
c
      filen = fdir(1:lf)//'gdd0.nc'
      call readvar(filen,'gdd0','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd0'
         stop 1
      end if
      call arr2vec (cdummy, gdd0)
c
      filen = fdir(1:lf)//'gdd0c.nc'
      call readvar(filen,'gdd0c','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd0c'
         stop 1
      end if
      call arr2vec (cdummy, gdd0c)
c
      filen = fdir(1:lf)//'gdd5.nc'
      call readvar(filen,'gdd5','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd5'
         stop 1
      end if
      call arr2vec (cdummy, gdd5)
c
      filen = fdir(1:lf)//'gdd8.nc'
      call readvar(filen,'gdd8','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd8'
         stop 1
      end if
      call arr2vec (cdummy, gdd8)
c
      filen = fdir(1:lf)//'gdd10.nc'
      call readvar(filen,'gdd10','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd10'
         stop 1
      end if
      call arr2vec (cdummy, gdd10)
c
      filen = fdir(1:lf)//'gdd12.nc'
      call readvar(filen,'gdd12','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, gdd12'
         stop 1
      end if
      call arr2vec (cdummy, gdd12)
c
      filen = fdir(1:lf)//'aplantn.nc'
      call readvar(filen,'aplantn','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, aplantn'
         stop 1
      end if
      call arr2vec (cdummy, aplantn)
c
      filen = fdir(1:lf)//'tc.nc'
      call readvar(filen,'tc','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, tc'
         stop 1
      end if
      call arr2vec (cdummy, tc)
c
      filen = fdir(1:lf)//'tw.nc'
      call readvar(filen,'tw','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, tw'
         stop 1
      end if
      call arr2vec (cdummy, tw)
c
      filen = fdir(1:lf)//'wipud.nc'
      call readvar(filen,'wipud','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, wipud'
         stop 1
      end if
      call arr2vec (cdummy, wipud)
c
      filen = fdir(1:lf)//'wpud.nc'
      call readvar(filen,'wpud','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, wpud'
         stop 1
      end if
      call arr2vec (cdummy, wpud)
c
      filen = fdir(1:lf)//'agddu.nc'
      call readvar(filen,'agddu','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, agddu'
         stop 1
      end if
      call arr2vec (cdummy, agddu)
c
      filen = fdir(1:lf)//'agddl.nc'
      call readvar(filen,'agddl','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, agddl'
         stop 1
      end if
      call arr2vec (cdummy, agddl)
c
      filen = fdir(1:lf)//'tempu.nc'
      call readvar(filen,'tempu','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, tempu'
         stop 1
      end if
      call arr2vec (cdummy, tempu)
c
      filen = fdir(1:lf)//'templs.nc'
      call readvar(filen,'templs','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, templs'
         stop 1
      end if
      call arr2vec (cdummy, templs)
c
      filen = fdir(1:lf)//'greenfracl3.nc'
      call readvar(filen,'greenfracl3','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, greenfracl3'
         stop 1
      end if
      call arr2vec (cdummy, greenfracl3)
c
      filen = fdir(1:lf)//'greenfracl4.nc'
      call readvar(filen,'greenfracl4','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, greenfracl4'
         stop 1
      end if
      call arr2vec (cdummy, greenfracl4)
c
      filen = fdir(1:lf)//'Tavgann.nc'
      call readvar(filen,'Tavgann','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, Tavgann'
         stop 1
      end if
      call arr2vec (cdummy, Tavgann)
c
      filen = fdir(1:lf)//'PPTavgann.nc'
      call readvar(filen,'PPTavgann','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, PPTavgann'
         stop 1
      end if
      call arr2vec (cdummy, PPTavgann)
c
      filen = fdir(1:lf)//'a10td.nc'
      call readvar(filen,'a10td','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10td'
         stop 1
      end if
      call arr2vec (cdummy, a10td)
c
      filen = fdir(1:lf)//'a10ts.nc'
      call readvar(filen,'a10ts','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10ts'
         stop 1
      end if
      call arr2vec (cdummy, a10ts)
c
      filen = fdir(1:lf)//'a10ancub.nc'
      call readvar(filen,'a10ancub','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10ancub'
         stop 1
      end if
      call arr2vec (cdummy, a10ancub)
ccc
cc      filen = fdir(1:lf)//'a10ancuc.nc'
cc      call readvar(filen,'a10ancuc','',istart,icount,
cc     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
cc      if (istat .ne. 0) then
cc         write(*,*) 'ERROR in restart, a10ancuc'
cc         stop 1
cc      end if
cc      call arr2vec (cdummy, a10ancuc)
c
      filen = fdir(1:lf)//'a10ancls.nc'
      call readvar(filen,'a10ancls','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10ancls'
         stop 1
      end if
      call arr2vec (cdummy, a10ancls)
c
      filen = fdir(1:lf)//'a10ancl4.nc'
      call readvar(filen,'a10ancl4','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10ancl4'
         stop 1
      end if
      call arr2vec (cdummy, a10ancl4)
c
      filen = fdir(1:lf)//'a10ancl3.nc'
      call readvar(filen,'a10ancl3','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10ancl3'
         stop 1
      end if
      call arr2vec (cdummy, a10ancl3)
c
      filen = fdir(1:lf)//'a10scalparamu.nc'
      call readvar(filen,'a10scalparamu','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10scalparamu'
         stop 1
      end if
      call arr2vec (cdummy, a10scalparamu)
c
      filen = fdir(1:lf)//'a10scalparaml.nc'
      call readvar(filen,'a10scalparaml','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10scalparaml'
         stop 1
      end if
      call arr2vec (cdummy, a10scalparaml)
c
      filen = fdir(1:lf)//'a10daylightu.nc'
      call readvar(filen,'a10daylightu','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10daylightu'
         stop 1
      end if
      call arr2vec (cdummy, a10daylightu)
c
      filen = fdir(1:lf)//'a10daylightl.nc'
      call readvar(filen,'a10daylightl','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a10daylightl'
         stop 1
      end if
      call arr2vec (cdummy, a10daylightl)
c
      filen = fdir(1:lf)//'a11soiltd.nc'
      call readvar(filen,'a11soiltd','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a11soiltd'
         stop 1
      end if
      call arr2vec (cdummy, a11soiltd)
c
      filen = fdir(1:lf)//'a3tdmin.nc'
      call readvar(filen,'a3tdmin','',istart,icount,
     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in restart, a3tdmin'
         stop 1
      end if
      call arr2vec (cdummy, a3tdmin)
c
c calculate tcmin
c
      do i = 1, npoi
         tcmin(i) = tc(i) + deltat(i)
      enddo
c
      call existence
c
      return
      end
c
c ---------------------------------------------------------------------
      subroutine coldstart
c ---------------------------------------------------------------------
c  
      include 'implicit.h'
c
      include 'compar.h'
      include 'comsoi.h'
      include 'comsno.h'
c
c initialize some model variables for cold start conditions
c
      call const (fi, npoi, 0.0)
c
      call const (hsno, npoi*nsnolay, 0.0)
      call const (tsno, npoi*nsnolay, 273.16)
c
      call const (tsoi, npoi*nsoilay, 278.16)
      call const (wsoi, npoi*nsoilay, 0.50)
      call const (wisoi, npoi*nsoilay, 0.00)
c
c return to main program
c
      return
      end
c
c
c ---------------------------------------------------------------------
      subroutine rdanom(imonth,iyear,iyranom,nanom,iy2,istyr,iwest,jnorth)
c ---------------------------------------------------------------------
c
c reads in anomalies for imonth+1
c
c this subroutine reads the monthly anomaly values of:
c
c temp - mean temperature (degC)
c trng - temperature range  (degC)
c prec - precipitation rate (mm/day)
c cld  - cloudiness (percent)
c rh   - relative humidity (percent)
c wspd - wind speed (not read in yet, saved for future use)
c wetd - wet days per month (not read in yet, saved for future use)
c
c and adds them to the climatological value for the month
c
c  
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comveg.h'
      include 'comwork.h'
c
c Arguments
c
      integer imonth, ! month
     >        iyear,  ! year
     >        iyranom,! year to start reading anoms
     >        iy2,    ! final year of the run
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth, ! 1st lat index for subset
     >        nanom   ! # of years in the anomaly files
c
c Local variables
c
      real anom(npoi)
      equivalence(anom(1),cdummy(1))
c
      integer imon,   ! month + 1
     >        iyr,    ! number of years after begin of data file(istyr)
     >        istat,  ! error flag for netcdf
     >        i,      ! loop indice on land points
     >        jyear   ! iyr divided by nanom (for looping through anomaly files)
c
      integer istart(4), icount(4) ! for reading rdanom vars
      character*80 filen
      character*39 pathn  

c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      istart(1) = iwest
      istart(2) = jnorth
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c determine which (if any) month to read
c If prior to November of the year before an anomaly year, then return.
c Else read anomalies for the following month.
c
      if (iyear .lt. iyranom-1) then
         return
      else if (iyear .eq. iyranom-1 .and. imonth .lt. 11) then
         return
c
c If timestep equals December of final year in anomaly file and also equals
c the final year of the run, then set January anomalies to zero and return.
c If not the final year of the run, then loop back to start of anomaly file
c (see jyear).
c
      else if (iyear .eq. istyr+nanom-1 .and. imonth .eq. 12) then
        if (iyear .eq. iy2) then
c
          print *, 'WARNING: last month of run; no anomalies for January'
          print *, 'Using climatologies for month year ='
          print *, imonth+1,iyear+1
          do 4 i = 1, npoi
            xint(i,1) = clmt(i,1)
c           xintrng(i,1) = clmtrng(i,1)
            xinprec(i,1) = clmprec(i,1)
            xincld(i,1) = clmcld(i,1)
            xinq(i,1) = clmq(i,1)
c           xinwind(i,1) = clmw(i,1)
c           xinwet(i,1) = clmwet(i,1)
 4        continue
          return
        end if
      end if
c
c have to correlate with NCEP so years match
c
      istyr = 1948 
      iyr = iyear-istyr
      imon = imonth + 1
      if (imon .eq. 13) then
         imon = 1
         iyr = iyr + 1
      end if
      nanom = 53 
      jyear = iyr/nanom
c     istart(4) = (iyr - nanom*jyear)*12 + imon
      istart(4) = (iyr - nanom*jyear)*12 + imon + (12*47)
c
      if (iyr.gt.0 .and. (iyr - nanom*jyear).eq.0) then
        print *, 'WARNING: Attempted to read past last month in anomaly file'
        print *, 'Looping back to the beginning of the file'
      end if
c
c      if (istart(4) .gt. 0) then
c        print *, 'rdanom reading month year step ='
c        print *, imon,iyr+istyr-nanom*jyear,istart(4)
c      else
c        print *, 'WARNING, anomalies begin in year ',istyr
c        print *, 'Not reading in anomalies for month year ='
c        print *, imon,iyr+istyr
c        return
c      end if
cc
cc     dummy variable example, 4-d, whose 3rd dim (level) = 1
cc
c      aname = 'dummyv'
c      filen = 'input/anom/dummyv.danom.nc'
c      call readvar(filen,aname,'level',istart,icount,
c     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c     > cdummy(3*nlonsub+1),istat)
c      if (istat .ne. 0) then
c         write(*,*) 'ERROR in rdanom, dummyv'
c         stop 1
c      end if
c      call arr2vec( work, anom )
c      do 10 i = 1, npoi
c         xindummyv(i,imon) = max (clmtrng(i,imon) + anom(i), 0.1)
c 10   continue
c
c     mean temperature
c
      aname = 'temp'
c     filen = pathn//'anom/temp.CRU.1901_2000.US.nc'
      filen = 'input/temp.CRU.1901_2000.US.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
        write(*,*) 'ERROR in rdanom, temp'
        stop 1
      end if
      call arr2vec( work, anom )
      do 5 i = 1, npoi
c old CRU TS 1 dataset         xint(i,imon) = clmt(i,imon) + anom(i)
         xint(i,imon) = anom(i)
 5    continue
cc
cc     temperature range
cc
      aname = 'dtr'
c     filen = pathn//'anom/dtr.CRU.1901_2000.US.nc'
      filen = 'input/dtr.CRU.1901_2000.US.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdanom, trange'
         stop 1
      end if
      call arr2vec( work, anom )
      do 10 i = 1, npoi
c        xintrng(i,imon) = max (clmtrng(i,imon) + anom(i), 0.1)
         xintrng(i,imon) = max (anom(i), 0.1)
 10   continue
c
c     precipitation rate
c
      aname = 'prate'
c     filen = pathn//'anom/prate.CRU.1901_2000.US.nc'
      filen = 'input/prate.CRU.1901_2000.US.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdanom, prec'
         stop 1
      end if
      call arr2vec( work, anom )
      do 15 i = 1, npoi
c        xinprec(i,imon) = clmprec(i,imon) + anom(i)
         xinprec(i,imon) = anom(i)
 15   continue
c
c     cloudiness
c
      aname = 'cld'
c     filen = pathn//'anom/cld.CRU.1901_2000.US.nc'
      filen = 'input/cld.CRU.1901_2000.US.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdanom, cld'
         stop 1
      end if
      call arr2vec( work, anom )
      do 20 i = 1, npoi
c        xincld(i,imon) = clmcld(i,imon) + anom(i)
         xincld(i,imon) = anom(i)
 20   continue
c
c     relative humidity
c
      aname = 'rh'
c     filen = pathn//'anom/rh.CRU.1901_2000.US.nc'
      filen = 'input/rh.CRU.1901_2000.US.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdanom, rh'
         stop 1
      end if
      call arr2vec( work, anom )
      do 25 i = 1, npoi
c        xinq(i,imon) = clmq(i,imon) + anom(i)
         xinq(i,imon) = anom(i)
 25   continue
c
c     wind speed
c
c     aname = 'wspd'
c     filen = 'input/anom/wspd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wspd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 30 i = 1, npoi
c        xinwind(i,imon) = clmw(i,imon) + anom(i)
c30   continue
c
c     wet days
c
c     aname = 'wetd'
c     filen = 'input/anom/wetd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wetd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 35 i = 1, npoi
c        xinwet(i,imon) = clmwet(i,imon) + anom(i)
c35   continue
c
c
      return
      end
c
c
c
c
c ---------------------------------------------------------------------
      subroutine rdcru_interpola(imonth,iyear,iyear0,iwest,jnorth)
c ---------------------------------------------------------------------
c
c reads in CRU for imonth+1
c
c this subroutine reads the monthly values of:
c
c temp - mean temperature (degC)
c tmax - temperature tmax  (degC)
c tmin - temperature tmin  (degC)
c prec - precipitation rate (mm/day)
c cld  - cloudiness (percent)
c rh   - relative humidity (percent)				!faltando.............
c wspd - wind speed (not read in yet, saved for future use)	!faltando.............
c wetd - wet days per month (not read in yet, saved for future use)	!faltando.............
c
c and adds them to the climatological value for the month
c
c  
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comveg.h'
      include 'comwork.h'
c
c Arguments
c
      integer imonth, ! month
     >        iyear,  ! year
     >        iyranom,! year to start reading anoms
     >        iy2,    ! final year of the run
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth, ! 1st lat index for subset
     >        nanom, iyear0   ! # of years in the anomaly files
c
c Local variables
c
      real anom(npoi),incru1(nlon,nlat),incru5(nlon,nlat),np
     > ,incru2(nlon,nlat),incru3(nlon,nlat),incru4(nlon,nlat)

      equivalence(anom(1),cdummy(1))
c
      integer imon,   ! month + 1
     >        iyr,    ! number of years after begin of data file(istyr)
     >        istat,  ! error flag for netcdf
     >        i,k,j,ii,jj, jc, mm,ik,jk,kk,      ! loop indice on land points
     >        jyear   ! iyr divided by nanom (for looping through anomaly files)
c
      integer istart(4), icount(4) ! for reading rdanom vars
      character*80 filen
      character*39 pathn  

c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      istart(1) = iwest
      istart(2) = jnorth
      icount(1) = nlonsub
      icount(2) = nlatsub


csant	  print*,'iwest	', iwest, jnorth,nlonsub,nlatsub,nlon,nlat

c
c     mean temperature
c

CRU file star  1901:1:1:0 / end 2002:12:1:0   (number of months 1224)

       open(unit=11,file='input/cru_tmp_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=12,file='input/cru_tmx_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=13,file='input/cru_tmn_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=14,file='input/cru_prec_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=15,file='input/cru_cld_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)



	if(iyear.eq.iyear0.and.imonth.eq.1) then 

	do mm =1,12

          ireccru= (iyear-1901)*12 + mm 
	     read (12,REC=ireccru) incru2!(i,j) 
	     read (13,REC=ireccru) incru3!(i,j) 
        k=0

	  do j = jnorth,jnorth+nlatsub-1

	jc= 360 - j + 1  !o ponto 360 do modelo e' igual ao ponto 1 do CRU

        jj = j - jnorth + 1

	     do i = iwest,iwest+nlonsub-1

          ii = i - iwest + 1

	if(j.eq.1) then
		if(lmask(ii,jj).eq.1) then
                k=k+1

	 np=0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 

                   do kk = 1,2
	    if(kk.eq.1.and.incru4(i,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc)
	   elseif(kk.eq.2.and.incru4(i+1,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc)
	   endif
  	         enddo
	    if(np.gt.0) then
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
	    else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	   stop
	   endif


		endif !for lmask eq. 1

	elseif(i.eq.720.and.j.lt.1) then
		if(lmask(ii,jj).eq.1) then
                k=k+1

	 np=0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 

                     do kk = 1,4
	    if(kk.eq.1.and.incru4(1,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(1,jc)
	   elseif(kk.eq.2.and.incru4(1,jc+1).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(1,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(1,jc+1)
	   elseif(kk.eq.3.and.incru4(720,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(720,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(720,jc)
	   elseif(kk.eq.4.and.incru4(720,jc+1).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(720,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(720,jc+1)
	   endif
 	               enddo

	       if(np.gt.0) then
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
       xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
	       else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	    stop
	      endif

		endif !for lmask . i.eq.720

	elseif(i.ne.720.and.j.ne.1) then 
	     if(lmask(ii,jj).eq.1) then
	     k=k+1

csant verifica	xintmxc(k,mm)=incru2(i,jc)
csant verifica	xintmmc(k,mm)=incru3(i,jc)
csant verifica 	xintrng(k,imonth) = xintmxc(k,imonth) - xintmmc(k,imonth)
csant	xintmxc(k,imonth)=(incru2(i,jc)+incru2(i,jc+1)+incru2(i+1,jc)+incru2(i+1,jc+1))/4.
csant	xintmmc(k,imonth)=(incru3(i,jc)+incru3(i,jc+1)+incru3(i+1,jc)+incru3(i+1,jc+1))/4.
csant	xintrng(k,imonth) = xintmxc(k,imonth) - xintmmc(k,imonth)
	 np=0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 

                           do kk = 1,4
	    if(kk.eq.1.and.incru4(i,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc)
	   elseif(kk.eq.2.and.incru4(i,jc+1).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc+1)
	   elseif(kk.eq.3.and.incru4(i+1,jc).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc)
	   elseif(kk.eq.4.and.incru4(i+1,jc+1).gt.-1.) then
        np = np + 1
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc+1)
	   endif
                	enddo

	        if(np.gt.0) then
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
   	        else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	   stop
	        endif

       if(k.eq.1)print*,'CRU. Ponto',k,' lon,lat ',i,jc,'time',iyear,imonth,' rec ',ireccru

	     endif !for lmask

	 else
	 print*,'error in read CRU data'
	endif  !for point verification
c		print*,iyear,imonth,irec,k,i,jc,xint(k,imonth)

	     enddo !for i
	   enddo   !for j 

	enddo !all months

	endif !first year



	if(iyear.ge.1901.and.iyear.le.2002) then

          ireccru= (iyear-1901)*12 + imonth 

	     read (11,REC=ireccru) incru1!(i,j) 
	     read (12,REC=ireccru) incru2!(i,j) 
	     read (13,REC=ireccru) incru3!(i,j) 
	     read (14,REC=ireccru) incru4!(i,j) 
	     read (15,REC=ireccru) incru5!(i,j) 


        k=0

	do j = jnorth,jnorth+nlatsub-1

	jc= 360 - j + 1  !o ponto 360 do modelo e' igual ao ponto 1 do CRU

        jj = j - jnorth + 1

	   do i = iwest,iwest+nlonsub-1

          ii = i - iwest + 1

	if(j.eq.1) then        !contour condition for north boundary
		if(lmask(ii,jj).eq.1) then
                k=k+1

	 np=0.
	xint(k,imonth)   = 0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 
	xinprec(k,imonth)= 0.
	xincld(k,imonth) = 0.

                     do kk = 1,2
	    if(kk.eq.1.and.incru4(i,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i,jc)
	   elseif(kk.eq.2.and.incru4(i+1,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i+1,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i+1,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i+1,jc)
	   endif
	             enddo

	         if(np.gt.0) then
	xint   (k,imonth) = xint   (k,imonth)/np
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
	xincld (k,imonth) = xincld (k,imonth)/np
        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
	xinprec(k,imonth)=xinprec(k,imonth)/(np*float(ndaypm(imonth)))
	         else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	   stop
	        endif


	    endif !lmask

	elseif(i.eq.720.and.j.lt.1) then
		if(lmask(ii,jj).eq.1) then
                k=k+1

	 np=0.
	xint(k,imonth)   = 0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 
	xinprec(k,imonth)= 0.
	xincld(k,imonth) = 0.

        		do kk = 1,4
	    if(kk.eq.1.and.incru4(1,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(1,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(1,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(1,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(1,jc)
	   elseif(kk.eq.2.and.incru4(1,jc+1).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(1,jc+1)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(1,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(1,jc+1)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(1,jc+1)
	xincld (k,imonth) = xincld (k,imonth) + incru5(1,jc+1)
	   elseif(kk.eq.3.and.incru4(720,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(720,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(720,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(720,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(720,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(720,jc)
	   elseif(kk.eq.4.and.incru4(720,jc+1).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(720,jc+1)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(720,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(720,jc+1)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(720,jc+1)
	xincld (k,imonth) = xincld (k,imonth) + incru5(720,jc+1)
	   endif
			enddo

  		    if(np.gt.0) then
	xint   (k,imonth) = xint   (k,imonth)/np
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
	xincld (k,imonth) = xincld (k,imonth)/np
        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
	xinprec(k,imonth)=xinprec(k,imonth)/(np*float(ndaypm(imonth)))
		    else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	   stop
	          endif

		endif !lmask

	elseif(i.ne.720.and.j.ne.1) then 

	     if(lmask(ii,jj).eq.1) then
	     k=k+1
csant-usado para verificar         xint(k,imonth) = incru1(i,jc)!(incru(i,jc)+incru(i,jc+1)+incru(i+1,jc)+incru(i+1,jc+1))/4.
csant-usado para verificar	xintmxc(k,imonth)=incru2(i,jc)
csant-usado para verificar	xintmmc(k,imonth)=incru3(i,jc)
csant-usado para verificar 	xintrng(k,imonth) = xintmxc(k,imonth) - xintmmc(k,imonth)
csant-usado para verificar	xinprec(k,imonth)= incru4(i,jc) /((float(ndaypm(imonth))) )
csant-usado para verificar	xincld(k,imonth)=incru5(i,jc)
csant- nao verifica se tem ponto undef	xint(k,imonth)   =(incru1(i,jc)+incru1(i,jc+1)+incru1(i+1,jc)+incru1(i+1,jc+1))/4.
csant- nao verifica se tem ponto undef	xintmxc(k,imonth)=(incru2(i,jc)+incru2(i,jc+1)+incru2(i+1,jc)+incru2(i+1,jc+1))/4.
csant- nao verifica se tem ponto undef	xintmmc(k,imonth)=(incru3(i,jc)+incru3(i,jc+1)+incru3(i+1,jc)+incru3(i+1,jc+1))/4.
csant- nao verifica se tem ponto undef	xintrng(k,imonth) = xintmxc(k,imonth) - xintmmc(k,imonth)
csant- nao verifica se tem ponto undef	xinprec(k,imonth)=(incru4(i,jc)+incru4(i,jc+1)+incru4(i+1,jc)+incru4(i+1,jc+1))
csant- nao verifica se tem ponto undef     >                                        /(4.*float(ndaypm(imonth)))
csant- nao verifica se tem ponto undef	xincld(k,imonth)=(incru5(i,jc)+incru5(i,jc+1)+incru5(i+1,jc)+incru5(i,jc+1))/4.

	 np=0.
	xint(k,imonth)   = 0.
	xintmxc(k,imonth)= 0.
	xintmmc(k,imonth)= 0.
	xintrng(k,imonth)= 0. 
	xinprec(k,imonth)= 0.
	xincld(k,imonth) = 0.

		        do kk = 1,4
	    if(kk.eq.1.and.incru4(i,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i,jc)
	   elseif(kk.eq.2.and.incru4(i,jc+1).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i,jc+1)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i,jc+1)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i,jc+1)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i,jc+1)
	   elseif(kk.eq.3.and.incru4(i+1,jc).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i+1,jc)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i+1,jc)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i+1,jc)
	   elseif(kk.eq.4.and.incru4(i+1,jc+1).gt.-1.) then
        np = np + 1
	xint   (k,imonth) = xint   (k,imonth) + incru1(i+1,jc+1)
	xintmxc(k,imonth) = xintmxc(k,imonth) + incru2(i+1,jc+1)
	xintmmc(k,imonth) = xintmmc(k,imonth) + incru3(i+1,jc+1)
        xinprec(k,imonth) = xinprec(k,imonth) + incru4(i+1,jc+1)
	xincld (k,imonth) = xincld (k,imonth) + incru5(i+1,jc+1)
	   endif
			enddo

		    if(np.gt.0) then
	xint   (k,imonth) = xint   (k,imonth)/np
	xintmxc(k,imonth) = xintmxc(k,imonth)/np
	xintmmc(k,imonth) = xintmmc(k,imonth)/np
	xincld (k,imonth) = xincld (k,imonth)/np
        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
	xinprec(k,imonth)=xinprec(k,imonth)/(np*float(ndaypm(imonth)))
		    else
 	print*,'Warning> no CRU data for this point',k,i,jc,'time',iyear,imonth
	   stop
		   endif
	

	endif !for lmaks

	 else
	 print*,'error in read CRU data'
	endif  !for point verification
c		print*,iyear,imonth,ireccru,k,i,jc,xinprec(k,imonth)
	    enddo !for i
	enddo     !for j

	else

	print*,'WARNING > no CRU input file ',iyear,imonth
	endif  !for years


csant         xintmax(i,imon) 
csant         xintmin(i,imon) 
csant        xintrng(i,imon) = xintmax(i,imon) -xintmin(i,imon)                !0.56 * trngm + 0.44 * trngm
c     precipitation rate
c
csant         xinprec(i,imon) = anom(i)/(float(ndaypm(imonth))

c     cloudiness
csant         xincld(i,imon) = anom(i)

c
c     relative humidity
csa         xinq(i,imon) = anom(i)
c
c     wind speed
c
c     aname = 'wspd'
c     filen = 'input/anom/wspd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wspd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 30 i = 1, npoi
c        xinwind(i,imon) = clmw(i,imon) + anom(i)
c30   continue
c
c     wet days
c
c     aname = 'wetd'
c     filen = 'input/anom/wetd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wetd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 35 i = 1, npoi
c        xinwet(i,imon) = clmwet(i,imon) + anom(i)
c35   continue
c
c
      return
      end
c
c
c
c
c
c ---------------------------------------------------------------------
      subroutine rdcru2(imonth,iyear,iyear0,iwest,jnorth)
c ---------------------------------------------------------------------
c
c reads in CRU for imonth+1
c
c this subroutine reads the monthly values of:
c
c temp - mean temperature (degC)
c tmax - temperature tmax  (degC)
c tmin - temperature tmin  (degC)
c prec - precipitation rate (mm/day)
c cld  - cloudiness (percent)
c rh   - relative humidity (percent)				
c wspd - wind speed (not read in yet, saved for future use)	!faltando.............
c wetd - wet days per month (not read in yet, saved for future use)	!faltando.............
c
c and adds them to the climatological value for the month
c
c  
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comsat.h'
      include 'comatm.h'
c
c Arguments
c
      integer imonth, ! month
     >        iyear,  ! year
     >        iyranom,! year to start reading anoms
     >        iy2,    ! final year of the run
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth, ! 1st lat index for subset
     >        nanom, iyear0   ! # of years in the anomaly files
c
c Local variables
c
      real anom(npoi),incru1(nlon,nlat),incru5(nlon,nlat),np
     > ,incru2(nlon,nlat),incru3(nlon,nlat),incru4(nlon,nlat)
     >,incru6(nlon,nlat),incru7(nlon,nlat)

      equivalence(anom(1),cdummy(1))
c
      integer imon,   ! month + 1
     >        iyr,    ! number of years after begin of data file(istyr)
     >        istat,  ! error flag for netcdf
     >        i,k,j,ii,jj, jc, mm,ik,jk,kk,      ! loop indice on land points
     >        jyear   ! iyr divided by nanom (for looping through anomaly files)
c
      integer istart(4), icount(4) ! for reading rdanom vars
      character*80 filen
      character*39 pathn  

c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      istart(1) = iwest
      istart(2) = jnorth
      icount(1) = nlonsub
      icount(2) = nlatsub


csant	  print*,'iwest	', iwest, jnorth,nlonsub,nlatsub,nlon,nlat

c
c     mean temperature
c

CRU file star  1901:1:1:0 / ends 2006:12:1:0   (number of months 1272)

       open(unit=11,file='input/cru_tmp_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=12,file='input/cru_tmx_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

c       open(unit=13,file='input/cru_tmn_v3.bin',
c     > status='old',form='unformatted'
c     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=14,file='input/cru_prec_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=15,file='input/cru_cld_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=16,file='input/cru_vap_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=17,file='input/cru_wet_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

	if(iyear.eq.iyear0.and.imonth.eq.1) then 


	do mm =1,12

          ireccru= (iyear-1901)*12 + mm 
	     read (11,REC=ireccru) incru1  !(i,j) 
	     read (12,REC=ireccru) incru2  !(i,j) 
c	     read (13,REC=ireccru) incru3  !(i,j) 
        k=0

	  do j = jnorth,jnorth+nlatsub-1  !jnorth+nlatsub-1 = jsouth

	jc= 360 - j + 1  !  360-j+1 = j-jnorth+1 

        jj = j - jnorth + 1

	     do i = iwest,iwest+nlonsub-1 !iwest+nlonsub-1 = ieast 

          ii = i - iwest + 1

		if(lmask(ii,jj).eq.1) then
                k=k+1

	           if(incru4(i,jc).gt.-1.) then
      	xintmxc(k,imonth) =  incru2(i,jc)
csant - "certo"	xintmmc(k,imonth) =  incru3(i,jc)
	xintmmc(k,imonth) =  incru1(i,jc)-(incru2(i,jc)-incru1(i,jc))


	   if (mm.eq.1) then !get coords for n points
	   lon_npoi(k)=lonscale(ii)
	   lat_npoi(k)=latscale(jj)
c	   print*,i,j,k,lon_npoi(k),lat_npoi(k)
	   endif
	
		   else
 	print*,'CRU and Model Grid doesnt match',k,i,jc,'time',iyear,imonth
	   stop
		   endif

		endif !for lmask eq. 1



	     enddo !for i
	   enddo   !for j 
	enddo !all months

	endif !first year


csant	if(iyear.ge.1901.and.iyear.le.2006) then !alterei para usar o XAVIER
	if(iyear.ge.1901.and.iyear.le.2013) then

	if(iyear.le.2006) then
          ireccru= (iyear-1901)*12 + imonth 
	else
	print*,'ERRO: o ano de 2006 está sendo imputado nos anos > 2006!!!'
          ireccru= (2006-1901)*12 + imonth 
	endif


	     read (11,REC=ireccru) incru1!(i,j) 
	     read (12,REC=ireccru) incru2!(i,j) 
c	     read (13,REC=ireccru) incru3!(i,j) 
	     read (14,REC=ireccru) incru4!(i,j) 
	     read (15,REC=ireccru) incru5!(i,j) 
	     read (16,REC=ireccru) incru6  !(i,j) 
	     read (17,REC=ireccru) incru7  !(i,j) 

        k=0

	do j = jnorth,jnorth+nlatsub-1

	jc= 360 - j + 1  !o ponto 360 do modelo e' igual ao ponto 1 do CRU

        jj = j - jnorth + 1

	   do i = iwest,iwest+nlonsub-1

          ii = i - iwest + 1

	   if(lmask(ii,jj).eq.1) then
                k=k+1

	           if(incru4(i,jc).gt.-1.) then
	xint   (k,imonth) =  incru1(i,jc)
	xintmxc(k,imonth) =  incru2(i,jc)
csant - para conservar td, fazemos a conta (funcao gamma!!)
	xintmmc(k,imonth) =  (incru1(i,jc)- 0.44*incru2(i,jc))/0.56
csant- e nao-	xintmmc(k,imonth) =  incru1(i,jc)-(incru2(i,jc)-incru1(i,jc))

        xinprec(k,imonth) =  incru4(i,jc)/(float(ndaypm(imonth)))
	xincld (k,imonth) =  incru5(i,jc) - 20.

c	if(k.eq.1)print*,'WARNING: Change CRU3.0 cloud cover based on INMET Climatologies'


        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
c
csant vapour pressure     hecta-Pascals, therefore - I have to convert first to specific humidity (kg-h2o/kg-air)
csant - metodo 1- es=6,178 exp[17,2693882t/(t + 237,3)]. - pressao de saturacao em mb ou hPa
c
	xinq(k,imonth)=100.*incru6(i,jc)/(6.178*exp(17.2693882*xint(k,imonth)/(xint(k,imonth)+ 237.3))) !xinq RH em %
c	if(k.eq.1) print*,xinq(k,imonth)
csant - metodo 2 - converte de pressao de vapor para umidade especifica
c       xinq(k,imonth)=100.*(0.622*incru6(i,jc)*100./psurf(k))/(qsat(esat(xint(k,imonth)+273.16),psurf(k)))
c	if(k.eq.1) print*,xinq(k,imonth)

c!seria assim se entreasse com umidade especifica- xinq(k,imonth)=incru6(i,jc)/(1000.*qsat(esat(xint(k,imonth)+273.16),psurf(k)))
c     r = 0.622 e / p  - onde 0.622 razao entre o vapor a a atmosfera padrao
c	if(k.eq.1) print*,0.622*incru6(i,jc)*100./psurf(k)

csant - 0.1, file from BADC  
       xinwet(k,imonth) = 0.1*incru7(i,jc)


		   else
 	print*,'CRU and Model Grid doesnt match',k,i,jc,'time',iyear,imonth
	   stop
		   endif

		endif !for lmask eq. 1

c		print*,iyear,imonth,ireccru,k,i,jc,xinprec(k,imonth)
c       if(k.eq.1)print*,'CRU. Ponto',k,' lon,lat ',i,jc,'time',iyear,imonth,' rec ',ireccru
c
c	if(k.eq.1)stop



	    enddo !for i
	enddo     !for j

	else

	print*,'WARNING > no CRU input file ',iyear,imonth
	endif  !for years


csant         xintmax(i,imon) 
csant         xintmin(i,imon) 
csant        xintrng(i,imon) = xintmax(i,imon) -xintmin(i,imon)                !0.56 * trngm + 0.44 * trngm
c     precipitation rate
c
csant         xinprec(i,imon) = anom(i)/(float(ndaypm(imonth))

c     cloudiness
csant         xincld(i,imon) = anom(i)

c
c     relative humidity
csa         xinq(i,imon) = anom(i)
c
c     wind speed
c
c     aname = 'wspd'
c     filen = 'input/anom/wspd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wspd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 30 i = 1, npoi
c        xinwind(i,imon) = clmw(i,imon) + anom(i)
c30   continue
c
c     wet days
c
c     aname = 'wetd'
c     filen = 'input/anom/wetd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wetd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 35 i = 1, npoi
c        xinwet(i,imon) = clmwet(i,imon) + anom(i)
c35   continue
c

      return
      end
c
c
c

c ---------------------------------------------------------------------
      subroutine rdcru_reg(imonth,iyear,iyear0,iwest,jnorth)
c ---------------------------------------------------------------------
c
c reads in CRU for imonth+1
c
c this subroutine reads the monthly values of:
c
c temp - mean temperature (degC)
c tmax - temperature tmax  (degC)
c tmin - temperature tmin  (degC)
c prec - precipitation rate (mm/day)
c cld  - cloudiness (percent)
c rh   - relative humidity (percent)				
c wspd - wind speed (not read in yet, saved for future use)	!faltando.............
c wetd - wet days per month (not read in yet, saved for future use)	!faltando.............
c
c and adds them to the climatological value for the month
c
c  
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comveg.h'
      include 'comwork.h'
      include 'comsat.h'
      include 'comatm.h'
c
c Arguments
c
      integer imonth, ! month
     >        iyear,  ! year
     >        iyranom,! year to start reading anoms
     >        iy2,    ! final year of the run
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth, ! 1st lat index for subset
     >        nanom, iyear0   ! # of years in the anomaly files
c
c Local variables
c
      real anom(npoi),incru1(nlon,nlat),incru5(nlon,nlat),np
     > ,incru2(nlon,nlat),incru3(nlon,nlat),incru4(nlon,nlat)
     >,incru6(nlon,nlat),incru7(nlon,nlat)

      equivalence(anom(1),cdummy(1))
c
      integer imon,   ! month + 1
     >        iyr,    ! number of years after begin of data file(istyr)
     >        istat,  ! error flag for netcdf
     >        i,k,j,ii,jj, jc, mm,ik,jk,kk,kj,anoi,      ! loop indice on land points
     >        jyear   ! iyr divided by nanom (for looping through anomaly files)
c
      integer istart(4), icount(4) ! for reading rdanom vars
      character*80 filen
      character*39 pathn  

c----------------------------- REGMC4 -------------------------------
	integer npi,npj,tempo,ndi,ndj,ireg,jreg,esquema
	parameter (npi=95,npj=91)    !CRU3.0 SUB-GRID - 0.5 degree !sub-domain 
	real NDPc(12),fator
	real preg(npi,npj,1548),treg(npi,npj,1548),dum(npi),dum2(npi)
	real tfut(npi,npj,12),tpre(npi,npj,12),pfut(npi,npj,12),ppre(npi,npj,12)

c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
c ---------------------------------------------------------------------
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      istart(1) = iwest
      istart(2) = jnorth
      icount(1) = nlonsub
      icount(2) = nlatsub




c ----------------------------------i-----------------------------------
C******** INICIA A LEITURA DOS ARQUIVOS DO_ REGCM4 ********************

c---------importante cada vez que alterar o dominio usado tem que acertar as grades aqui
	ndi= 201 !encontrar as grades do CRU completo e do RegCM
	ndj= 101 !encontrar as grades do CRU completo e do RegCM


       esquema=1 !CRU
c       esquema = 2   !use Monthly REGCM4 Prec and Temp
c       esquema=3 !use Monthly REGCM4 Prec and Temp - BIAS REMOVED!!
c        esquema=4 !use Monthly CRU and REGCM4 Future Anomaly - Prec and Temp
c       esquema=5 !use Monthly REG and REGCM4 Future Anomaly - Prec and Temp



c	print*,iyear,iyear0

	     IF(iyear.eq.iyear0.and.imonth.eq.1) THEN 

     	data NDPc  /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/


	close(440)
	close(441)

	open (440,FILE='HAD_t2m_CRUgrid.txt',FORM='FORMATTED')

	open (441,FILE='HAD_tpr_CRUgrid.txt',FORM='FORMATTED')

	fator = 1. ! 86400 ! 1.0 p/ rodadas do RegCM e 86400 p/ GCM, RegCM esta' em mm/dia e o GCM em kg/m2.s

	do mm=1,12
            do j=1,npj
	   do ii=1,npi
	tpre(ii,j,mm)=0.
	ppre(ii,j,mm)=0.
	tfut(ii,j,mm)=0.
	pfut(ii,j,mm)=0.
	  enddo
         enddo
       enddo
	

	do anoi=1970,2098
	 do mm=1,12

	 tempo= (12*(anoi-1970))+mm

            do kj=1,npj

csant - leitira da linha com espaco entre os characteres
	 read(441,*) dum
	 read(440,*) dum2
csant  - para o GRADS e no NETCDF os arquivos estao do Sul p/ o Norte, aqui de cima p/ baixo!
c	 print*,anoi,mm,((tempo-1)*npj)+kj,kj,dum(1),dum(20),dum(91)

  	   j=kj  !npj+1-kj

	   do ii=1,npi
	   preg(ii,j,tempo)=-9999.0
	   preg(ii,j,tempo)=dum(ii)*NDPc(mm)*fator
	   treg(ii,j,tempo)=-9999.0
	   treg(ii,j,tempo)=dum2(ii)-273.15

	if(anoi.ge.1975.and.anoi.le.2004) then
	tpre(ii,j,mm)=tpre(ii,j,mm)+treg(ii,j,tempo)
	ppre(ii,j,mm)=ppre(ii,j,mm)+preg(ii,j,tempo)
	endif

	if(anoi.ge.2069.and.anoi.le.2098) then
	tfut(ii,j,mm)=tfut(ii,j,mm)+treg(ii,j,tempo)
	pfut(ii,j,mm)=pfut(ii,j,mm)+preg(ii,j,tempo)
	endif


	if((dum(ii).lt.0.0).or.(dum2(ii).lt.200)) then
	print*,' ou a chuva esta menor que zero ou temp. nao estao em Kelvin!!'
	print*,ii,dum(ii),dum2(ii)
	stop
	endif

c	if(ii.eq.1.and.kj.eq.1)print*,year,mm,preg(ii,kj,mm)/NDPc(mm)

	   enddo
	

	  enddo!lats
c	stop
         enddo!meses
       enddo!anos
	

	do mm=1,12
            do j=1,npj
	   do ii=1,npi
	tpre(ii,j,mm)=tpre(ii,j,mm)/30.
	ppre(ii,j,mm)=ppre(ii,j,mm)/30.
	tfut(ii,j,mm)=tfut(ii,j,mm)/30.
	pfut(ii,j,mm)=pfut(ii,j,mm)/30.

c	print*,mm,tpre(ii,j,mm),ppre(ii,j,mm),tfut(ii,j,mm),pfut(ii,j,mm)

	  enddo
         enddo
       enddo
	
c	stop

		ENDIF


C******** FINALIZA A LEITURA DOS ARQUIVOS DO_ REGCM4 ****************
c -------------------------------------------------------------------
c*****************  SEGUE a SEQUENCIA NORMAL ANTERIOR *************


CRU file star  1901:1:1:0 / ends 2006:12:1:0   (number of months 1272)

       open(unit=11,file='input/cru_tmp_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=12,file='input/cru_tmx_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

c       open(unit=13,file='input/cru_tmn_v3.bin',
c     > status='old',form='unformatted'
c     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=14,file='input/cru_prec_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=15,file='input/cru_cld_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=16,file='input/cru_vap_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)

       open(unit=17,file='input/cru_wet_v3.bin',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*720*360)



	if(iyear.eq.iyear0.and.imonth.eq.1) then 

	do mm =1,12

          ireccru= (iyear-1901)*12 + mm 
	     read (11,REC=ireccru) incru1  !(i,j) 
	     read (12,REC=ireccru) incru2  !(i,j) 
c	     read (13,REC=ireccru) incru3  !(i,j) 
        k=0

	  do j = jnorth,jnorth+nlatsub-1  !jnorth+nlatsub-1 = jsouth

	jc= 360 - j + 1  !  360-j+1 = j-jnorth+1 

        jj = j - jnorth + 1

	     do i = iwest,iwest+nlonsub-1 !iwest+nlonsub-1 = ieast 

          ii = i - iwest + 1

		if(lmask(ii,jj).eq.1) then
                k=k+1

	           if(incru4(i,jc).gt.-1.) then
      	xintmxc(k,imonth) =  incru2(i,jc)
csant - "certo"	xintmmc(k,imonth) =  incru3(i,jc)
	xintmmc(k,imonth) =  incru1(i,jc)-(incru2(i,jc)-incru1(i,jc))


		   else
 	print*,'CRU and Model Grid doesnt match',k,i,jc,'time',iyear,imonth
	   stop
		   endif

		endif !for lmask eq. 1



	     enddo !for i
	   enddo   !for j 

	enddo !all months

	endif !first year



csant	if(iyear.ge.1901.and.iyear.le.2006) then !alterei para usar o XAVIER
	if(iyear.ge.1901.and.iyear.le.2013) then

	if(iyear.le.2006) then
          ireccru= (iyear-1901)*12 + imonth 
	else
	print*,'ERRO: o ano de 2006 está sendo imputado nos anos > 2006!!!'
          ireccru= (2006-1901)*12 + imonth 
	endif



	     read (11,REC=ireccru) incru1!(i,j) 
	     read (12,REC=ireccru) incru2!(i,j) 
c	     read (13,REC=ireccru) incru3!(i,j) 
	     read (14,REC=ireccru) incru4!(i,j) 
	     read (15,REC=ireccru) incru5!(i,j) 
	     read (16,REC=ireccru) incru6  !(i,j) 
	     read (17,REC=ireccru) incru7  !(i,j) 



        k=0

	do j = jnorth,jnorth+nlatsub-1

	jc= 360 - j + 1  !o ponto 360 do modelo e' igual ao ponto 1 do CRU

        jj = j - jnorth + 1

	   do i = iwest,iwest+nlonsub-1

          ii = i - iwest + 1

	   if(lmask(ii,jj).eq.1) then
                k=k+1

	           if(incru4(i,jc).gt.-1.) then
	xint   (k,imonth) =  incru1(i,jc)
	xintmxc(k,imonth) =  incru2(i,jc)
csant - para conservar td, fazemos a conta (funcao gamma!!)
	xintmmc(k,imonth) =  (incru1(i,jc)- 0.44*incru2(i,jc))/0.56
csant- e nao-	xintmmc(k,imonth) =  incru1(i,jc)-(incru2(i,jc)-incru1(i,jc))

        xinprec(k,imonth) =  incru4(i,jc)/(float(ndaypm(imonth)))
	xincld (k,imonth) =  incru5(i,jc) - 20.

c	if(k.eq.1)print*,'WARNING: Change CRU3.0 cloud cover based on INMET Climatologies'


        xintrng(k,imonth)= xintmxc(k,imonth) - xintmmc(k,imonth)
c
csant vapour pressure     hecta-Pascals, therefore - I have to convert first to specific humidity (kg-h2o/kg-air)
csant - metodo 1- es=6,178 exp[17,2693882t/(t + 237,3)]. - pressao de saturacao em mb ou hPa
c
	xinq(k,imonth)=100.*incru6(i,jc)/(6.178*exp(17.2693882*xint(k,imonth)/(xint(k,imonth)+ 237.3))) !xinq RH em %
c	if(k.eq.1) print*,xinq(k,imonth)
csant - metodo 2 - converte de pressao de vapor para umidade especifica
c       xinq(k,imonth)=100.*(0.622*incru6(i,jc)*100./psurf(k))/(qsat(esat(xint(k,imonth)+273.16),psurf(k)))
c	if(k.eq.1) print*,xinq(k,imonth)

c!seria assim se entreasse com umidade especifica- xinq(k,imonth)=incru6(i,jc)/(1000.*qsat(esat(xint(k,imonth)+273.16),psurf(k)))
c     r = 0.622 e / p  - onde 0.622 razao entre o vapor a a atmosfera padrao
c	if(k.eq.1) print*,0.622*incru6(i,jc)*100./psurf(k)

csant - 0.1, file from BADC  
       xinwet(k,imonth) = 0.1*incru7(i,jc)

c---------------------------------------------------------
c**********  Atribuindo dados do REgCM4 ******************

        if(iyear.ge.1970.and.iyear.le.2098) then

	   tempo= (12*(iyear-1970))+imonth

c  o ponto 360 do modelo e' igual ao ponto 1 do CRU 
c  o ponto 201 do CRU  e' o ponto 1 do RegCM
	jreg= 360 - j + 1 - ndj + 1
c  o ponto 201 do CRU  e' o ponto 1 do RegCM
          ireg =  i - ndi + 1

       if(ireg.ge.1.and.ireg.le.95.and.jreg.ge.1.and.jreg.le.91) then

	
c        a max e min ele vai pegar pelo xintrng(k,imonth)


	if(esquema.eq.1) then !use CRU
	xint   (k,imonth) =  incru1(i,jc) 
        xinprec(k,imonth) =  incru4(i,jc)/(float(ndaypm(imonth)))
	
	elseif (esquema.eq.2) then !use Monthly REGCM4 Prec and Temp
	xint   (k,imonth) =  treg(ireg,jreg,tempo) !incru1(i,jc) 
        xinprec(k,imonth) = preg(ireg,jreg,tempo) /(float(ndaypm(imonth)))

	elseif(esquema.eq.3) then !use Monthly REGCM4 Prec and Temp - BIAS REMOVED!!
        xint(k,imonth)=treg(ireg,jreg,tempo)
c     > -(tmreg(ireg,jreg,imonth)-tmcru(i,jc,imonth)) 
        xinprec(k,imonth) =(preg(ireg,jreg,tempo)/(float(ndaypm(imonth))))  
c     > - (tmreg(ireg,jreg,imonth)-tmcru(i,jc,imonth))

	elseif(esquema.eq.4) then !use Monthly CRU and REGCM4 Future Anomaly - Prec and Temp

       xint(k,imonth)=incru1(i,jc)+(tfut(ireg,jreg,imonth)-tpre(ireg,jreg,imonth)) 

       xinprec(k,imonth)=incru4(i,jc)*(pfut(ireg,jreg,imonth)/ppre(ireg,jreg,imonth))
       xinprec(k,imonth)=xinprec(k,imonth)/(float(ndaypm(imonth)))
c------------------------------------------------------------------------------------------------------------

	elseif(esquema.eq.5) then !use Monthly REG and REGCM4 Future Anomaly - Prec and Temp
       xint(k,imonth)=treg(ireg,jreg,tempo)+(tfut(ireg,jreg,imonth)-tpre(ireg,jreg,imonth)) 
       xinprec(k,imonth)=preg(ireg,jreg,tempo)*(pfut(ireg,jreg,imonth)/ppre(ireg,jreg,imonth))
       xinprec(k,imonth)=xinprec(k,imonth)/(float(ndaypm(imonth)))

	else
	print*,'nao ha esse esquema numero ',esquema
	stop
	endif

	if(ireg.eq.45.and.jreg.eq.27)then
	print*,iyear,imonth,tempo
	print*,ireg,jreg,xint(k,imonth),xinprec(k,imonth),
     > tfut(ireg,jreg,imonth),tpre(ireg,jreg,imonth),pfut(ireg,jreg,imonth),ppre(ireg,jreg,imonth)
	endif

	else
	print*,'nao achou o ponto do RegCM4'
	stop
	endif !pontos
         endif   !dos anos
   


		   else
 	print*,'CRU and Model Grid doesnt match',k,i,jc,'time',iyear,imonth
	   stop
		   endif

		endif !for lmask eq. 1

c		print*,iyear,imonth,ireccru,k,i,jc,xinprec(k,imonth)
c       if(k.eq.1)print*,'CRU. Ponto',k,' lon,lat ',i,jc,'time',iyear,imonth,' rec ',ireccru
c
c	if(k.eq.1)stop



	    enddo !for i
	enddo     !for j

	else

	print*,'WARNING > no CRU input file ',iyear,imonth
	endif  !for years


csant         xintmax(i,imon) 
csant         xintmin(i,imon) 
csant        xintrng(i,imon) = xintmax(i,imon) -xintmin(i,imon)                !0.56 * trngm + 0.44 * trngm
c     precipitation rate
c
csant         xinprec(i,imon) = anom(i)/(float(ndaypm(imonth))

c     cloudiness
csant         xincld(i,imon) = anom(i)

c
c     relative humidity
csa         xinq(i,imon) = anom(i)
c
c     wind speed
c
c     aname = 'wspd'
c     filen = 'input/anom/wspd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wspd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 30 i = 1, npoi
c        xinwind(i,imon) = clmw(i,imon) + anom(i)
c30   continue
c
c     wet days
c
c     aname = 'wetd'
c     filen = 'input/anom/wetd.danom.nc'
c     call readvar(filen,aname,'level',istart,icount,
c    > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
c    > cdummy(3*nlonsub+1),istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR in rdanom, wetd'
c        stop 1
c     end if
c     call arr2vec( work, anom )
c     do 35 i = 1, npoi
c        xinwet(i,imon) = clmwet(i,imon) + anom(i)
c35   continue
c

      return
      end
c
c
c


c ---------------------------------------------------------------------
      subroutine inird(file,istyr)
c ---------------------------------------------------------------------
c
c This subroutine obtains the year+1 of the year in the units attribute
c of a file.  Low-level netcdf commands are used.
c
      include 'implicit.h'
c
      include 'netcdf.inc'
c
c Arguments
c
      character*(*) file
      integer istyr       
c
c Local Variables
c
      integer idies, istat, idtime, lf1
c
c      integer NF_OPEN,        ! netcdf function
c     >        NF_NOWRITE,     ! '
c     >        NF_NOERR,       ! '
c     >        NF_INQ_VARID,   ! '
c     >        NF_GET_ATT_TEXT ! '
c              
      character*80 units
c ---------------------------------------------------------------------
c
c open file
c
      istat = NF_OPEN(file,NF_NOWRITE,idies)
      if (istat .ne. NF_NOERR) then
         print *, 'Error in inird while trying to open file'
         print *, file
         print *, NF_STRERROR(istat)
         istyr = -1
         return
      end if
c
c get units attribute for time
c
      istat = NF_INQ_VARID(idies,'time',idtime)
      if (istat .ne. NF_NOERR) then
         print *, 'Error in inird while trying to get time id'
         print *, NF_STRERROR(istat)
         istyr = -1
         return
      end if
      units = ' '
      istat = NF_GET_ATT_TEXT(idies,idtime,'units',units)
      if (istat .ne. NF_NOERR) then
         print *, 'Error in inird while trying to get time units'
         print *, NF_STRERROR(istat)
         istyr = -1
         return
      end if
c
c put character units year into integer variable, add 1
c
      lf1 = index(units,'since') + 6
      read(units(lf1:lf1+3),'(i4)') istyr
c      read(units(12:15),'(i4)') istyr
      istyr = istyr + 1
      return
      end
c
c ---------------------------------------------------------------------
      subroutine rdday(jday, imonth, iyear, istyr, nanomd, iwest, jnorth)
c ---------------------------------------------------------------------
c
c This subroutine reads in daily fields..
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comwork.h'
c
c Arguments
c
      integer jday,   ! day of the year
     >        imonth, ! month
     >        iyear,  ! year
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth  ! 1st lat index for subset
c
c Local variables
c
      integer i,      ! loop indice on years after istyr
     >        istat,  ! error flag for netcdf
     >        jyear,  ! iyear / nanomd 
     >        iyrdiff,
     >        iyrloop,
     >        nanomd  ! number of years in daily anomaly file
c
      character*80 filen
      character*39 pathn
      integer istart(4), icount(4)
c
c ---------------------------------------------------------------------
c
c     pathn = '/Volumes/wet/kucharik/IBIS_input/input/'
c
      data istart / 1,1,1,1 /, icount / nlon,nlat,1,1 /
      istart(1) = iwest
      istart(2) = jnorth
      icount(1) = nlonsub
      icount(2) = nlatsub
c
c
c calculate counter for looping through anomaly data for NCEP 
c
      iyrdiff = iyear - istyr
        jyear = iyrdiff/nanomd
      iyrloop = (iyrdiff - nanomd*jyear) + istyr 

      if (iyear .lt. istyr) then
        print *, 'daily data begins in year ', istyr
        print *, 'not reading in daily data'
        return
      end if
c
c count how many days since beginning of daily data
c daily data begin on Jan 1, istyr
c
      if (iyrloop .eq. istyr) then
        istart(4) = jday
      else
        istart(4) = 0
        do 10 i = istyr, iyrloop-1
          istart(4) = istart(4) + 365
          if (mod(i,4).eq.0) then
            if (mod(i,100).ne.0) then
              istart(4) = istart(4) + 1
            else if (mod(i/100,4).eq.0) then
              istart(4) = istart(4) + 1
            end if
          end if
 10     continue
        istart(4) = istart(4) + jday
      end if
c
c      if (istart(4) .gt. 0) then
c        print *, 'rdday reading day month year step ='
c        print *, jday, imonth, iyrloop,istart(4)
c      else
c        print *, 'WARNING, anomalies begin in year ',istyr
c        print *, 'Not reading in anomalies for day month year ='
c        print *, jday, imonth, iyrloop
c        return
c      end if
c
c read daily precip
c
      aname = 'prec'
c     filen = pathn//'daily/prate_us.fanom.1948_2002.nc'
      filen = 'input/prate_us.fanom.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, prec'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xinprecd(1) )
c
c read daily temp
c
      aname = 'temp'
c     filen = pathn//'daily/tmp2m_us.danom.1948_2002.nc'
      filen = 'input/tmp2m_us.danom.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, temp'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xintd(1) )
c
c read daily temp max
c
      aname = 'tmax'
c     filen = pathn//'daily/tmax_us.1948_2002.nc'
      filen = 'input/tmax_us.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, tmax'
         if (iyear .gt. 1997) then
           print *, 'Attempted to read past last day in file?'
         end if
         return
      end if
      call arr2vec( work, xintmax(1) )
c
c read daily temp min 
c
      aname = 'tmin'
c     filen = pathn//'daily/tmin_us.1948_2002.nc'
      filen = 'input/tmin_us.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, tmin'
         if (iyear .gt. 1997) then
           print *, 'Attempted to read past last day in file?'
         end if
         return
      end if
      call arr2vec( work, xintmin(1) )
c
c
c read daily trange
c
      aname = 'trange'
c     filen = pathn//'daily/trange_us.fanom.1948_2002.nc'
      filen = 'input/trange_us.fanom.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, trange'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xintrngd(1) )
c
c read daily cloudiness
c
      aname = 'cld'
c     filen = pathn//'daily/tcdcclm_us.danom.1948_2002.nc'
      filen = 'input/tcdcclm_us.danom.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, cld'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xincldd(1) )
c
c read daily windspeed
c
      aname = 'wspd'
c     filen = pathn//'daily/wspd_us.fanomc.1948_2002.nc'
      filen = 'input/wspd_us.fanomc.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, wspd'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xinwindd(1) )
c
c read daily humidity
c
      aname = 'sphum'
c     filen = pathn//'daily/spfh2m_us.fanom.1948_2002.nc'
      filen = 'input/spfh2m_us.fanom.1948_2002.nc'
      call readvar(filen,aname,'level',istart,icount,
     > work,cdummy(1),cdummy(nlonsub+1),cdummy(2*nlonsub+1),
     > cdummy(3*nlonsub+1),istat)
      if (istat .ne. 0) then
         write(*,*) 'ERROR in rdday, sphum'
c         if (iyear .gt. 1997) then
c           print *, 'Attempted to read past last day in file?'
c         end if
         return
      end if
      call arr2vec( work, xinqd(1) )
c
c
      return
      end

c
c ---------------------------------------------------------------------
      subroutine rdstation(jday, imonth, iyear, istyear,file_wth )
c ---------------------------------------------------------------------
c
c This subroutine reads in daily fields..
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comwork.h'
c
c Arguments
c
      integer jday,   ! day of the year
     >        imonth, ! month
     >        iyear,  ! year
     >        istyear  ! 1st year in data files
c
c Local variables
c
      integer i,      ! loop indice on years after istyr
     >        istat,  ! error flag for netcdf
     >        jyear,  ! iyear / nanomd 
     >        iyrdiff,
     >        iyrloop,
     >        nanomd  ! number of years in daily anomaly file
c
      character*80 filen
      character*39 pathn
      character file_wth*12  	
      integer istart, icount, metyear,metjday,tt

	real radn(npoi),inradn,inprec,intmax,intmin,pula,
     > urin,windin

	character  namemet*6 ,datac*6    
      


	if(iyear.eq.istyear.and.jday.eq.1) then

	close(132)
	filen='input_dssat/'//file_wth
      open (132,file=filen,status='old')


   	    do i=1,4	
	    read(132,*)
	    enddo
	
	   do i=1,60*365	


       read(132,*) datac,inradn,intmax,intmin,inprec

	read(datac,'(I2)')tt
	read(datac(3:5),'(I3)')metjday

	if(tt.gt.60)then
	tt=tt+1900
	else
	tt=tt+2000
	endif

	metyear=tt
	

c	print*,'read in file wth, try find date',iyear,jday,metyear,metjday
	

	   if(iyear.eq.metyear.and.jday.eq.metjday) goto 134
	
   	  enddo
	print*,'warning: didnt find the date in the station input file!'

	endif



       read(132,*) datac,inradn,intmax,intmin,inprec
	read(datac,'(I2)')tt
	read(datac(3:5),'(I3)')metjday

	if(tt.gt.60)then
	tt=tt+1900
	else
	tt=tt+2000
	endif

	metyear=tt
	


c	print*,inprec,

 134   continue	

	do i = 1, npoi

	stintmax(i)  = intmax + 273.16
	stintmin(i)  = intmin + 273.16
csant- the same as in the weather generator (weather.f)
c	stintd(i)    = 0.44 * stintmax(i) + 0.56 * stintmin(i)
csant- as recomended by OMM
	stintd(i)    = 0.5 * stintmax(i) + 0.5 * stintmin(i)
	stinprecd(i) = inprec
	stinrad(i)   = inradn
c	stinrad(i)   = -9999
	stinqd(i)    = -9999
	stinwindd(i) = -9999
	
c	print*,file_wth,iyear,jday,stinrad(i)


	if(intmax.gt.60.or.intmax.lt.-60) print*,'Warning:  ERRO in read station radiation!!!!'
	enddo

c	print*,iyear,jday,stinprecd(1)


      return
      end
c


c ---------------------------------------------------------------------
      subroutine rdstation2d(iday, imonth, iyear,iyear0 )
c ---------------------------------------------------------------------
c
c This subroutine reads in daily fields..
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comwork.h'
c
c Arguments
c
      integer iday,   ! day of the year
     >        imonth, ! month
     >        iyear,  ! year
     >        iyear0  ! 1st simulation year
c
c Local variables
c
      integer i,      ! loop indice on years after istyr
     >        istat,  ! error flag for netcdf
     >        jyear,  ! iyear / nanomd 
     >        iyrdiff,
     >        iyrloop,
     >        nanomd  ! number of years in daily anomaly file
c
      character*80 filen
      character*39 pathn
      character*40 file_wth_TN,file_wth_TX,file_wth_PR,file_wth_RS 
      character*7 lonc7,latc7
      character*6 lonc6,latc6
      character    metdate*10	

      integer istart, icount, metyear,metjday,inm,ind,iyc,imc,idc,anov
	real invar1,invar2,invar3,invar4	

	real radn(npoi),insrad(npoi,31,12,60),inprec(npoi,31,12,60),
     > intmax(npoi,31,12,60),intmin(npoi,31,12,60),pula,
     > urin,windin

	character  namemet*6 ,datac*6    
      
c	data ndaypm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/


	
	anov=iyear-1979

	if(iyear.lt.1980.or.iyear.gt.2013) then
	print*,'Xavier data goes from 1980 to 2013 - ',iyear
	stop
	endif



	if(iyear.eq.iyear0.and.imonth.eq.1.and.iday.eq.1) then


	do i = 1, npoi


	close(32)
	close(33)
	close(34)
	close(35)
	
	if(lon_npoi(i).gt.-10.or.lat_npoi(i).gt.0.0)then
	print*,'acertar rotina que cria leitura de arquivos'
	stop
	endif

	if(lat_npoi(i).gt.0.0.and.lat_npoi(i).le.-10.0)then
	  write(latc6,'(f6.3)')(lat_npoi(i)+0.125)
	  write(lonc7,'(f7.3)')(lon_npoi(i)-0.125)	

	  file_wth_TN='Xavier_Tmin_'
     > //lonc7(1:3)//lonc7(5:7)//latc6(1:2)//latc6(4:6)//'.csv'
	  file_wth_TX='Xavier_Tmax_'
     > //lonc7(1:3)//lonc7(5:7)//latc6(1:2)//latc6(4:6)//'.csv'
	  file_wth_PR='Xavier_prec_'
     > //lonc7(1:3)//lonc7(5:7)//latc6(1:2)//latc6(4:6)//'.csv'
	  file_wth_RS='Xavier_Rs_'
     > //lonc7(1:3)//lonc7(5:7)//latc6(1:2)//latc6(4:6)//'.csv'

	elseif(lat_npoi(i).le.-10.0)then
	  write(latc7,'(f7.3)')(lat_npoi(i)+0.125)
 	  write(lonc7,'(f7.3)')(lon_npoi(i)-0.125)	
	  file_wth_TN='Xavier_Tmin_'
     > //lonc7(1:3)//lonc7(5:7)//latc7(1:3)//latc7(5:7)//'.csv'
	  file_wth_TX='Xavier_Tmax_'
     > //lonc7(1:3)//lonc7(5:7)//latc7(1:3)//latc7(5:7)//'.csv'
	  file_wth_PR='Xavier_prec_'
     > //lonc7(1:3)//lonc7(5:7)//latc7(1:3)//latc7(5:7)//'.csv'
	  file_wth_RS='Xavier_Rs_'
     > //lonc7(1:3)//lonc7(5:7)//latc7(1:3)//latc7(5:7)//'.csv'

	endif
	

	filen='input_xavier/'//file_wth_TN
        open (32,file=filen,status='old')
	filen='input_xavier/'//file_wth_TX
        open (33,file=filen,status='old')
	filen='input_xavier/'//file_wth_PR
        open (34,file=filen,status='old')
	filen='input_xavier/'//file_wth_RS
        open (35,file=filen,status='old')


	print*,npoi,lat_npoi(i),lon_npoi(i),file_wth_TN

	    read(32,*)
	    read(33,*)
	    read(34,*)
	    read(35,*)
	
        do iyc=1980,2013
	 do imc=1,12

        ndaypm(2) = 28
        if (mod(iyc,4).eq.0) then
          if (mod(iyc,100).ne.0) then
            ndaypm(2) = 29
          else if (mod(iyc/100,4).eq.0) then
          ndaypm(2) = 29
          end if
        end if

	  do idc = 1, ndaypm(imc)

c mydate;year;month;day;var
           read(32,*)metdate,metyear,inm,ind,invar1 
           read(33,*)metdate,metyear,inm,ind,invar2 
           read(34,*)metdate,metyear,inm,ind,invar3 
           read(35,*)metdate,metyear,inm,ind,invar4 

	if(ind.ne.idc.or.inm.ne.imc.or.metyear.ne.iyc) then
	print*,'dates in do and file dif'
	print*,ind,idc,inm,imc,metyear,iyc
	stop
	endif 

	intmin(i,ind,inm,(metyear-1979))=invar1
	intmax(i,ind,inm,(metyear-1979))=invar2
	inprec(i,ind,inm,(metyear-1979))=invar3
	insrad(i,ind,inm,(metyear-1979))=invar4

c	   print*,metdate,metyear,inm,ind,invar
	
c	datac,inradn,intmax,intmin,inprec
	  enddo
	 enddo
	enddo

	enddo !for all points

	
	endif



	do i = 1, npoi

	stintmax(i)  = intmax(i,iday,imonth,anov) + 273.16
	stintmin(i)  = intmin(i,iday,imonth,anov) + 273.16
csant- the same as in the weather generator (weather.f)
c	stintd(i)    = 0.44 * stintmax(i) + 0.56 * stintmin(i)
csant- as recomended by OMM
	stintd(i)    = 0.5 * stintmax(i) + 0.5 * stintmin(i)
	stinprecd(i) = inprec(i,iday,imonth,anov)
	stinrad(i)   = insrad(i,iday,imonth,anov)
c	stinrad(i)   = -9999
	stinqd(i)    = -9999
	stinwindd(i) = -9999
	


	if(intmax(i,iday,imonth,anov).gt.60.or.intmax(i,iday,imonth,anov).lt.-60) 
     >  print*,'Warning:  ERRO in read station radiation!!!!'
	enddo


	print*,'Using Xavier data set - ',iday,imonth,anov

      return
      end
c


c ---------------------------------------------------------------------
      subroutine rdcdc (jday,imonth,iyear,cdcyear, iwest, jnorth)
c ---------------------------------------------------------------------
c
c This subroutine reads in daily fields..
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'combcs.h'
      include 'comwork.h'
c
c Arguments
c
      integer jday,k,j,jc,jj,ii,   ! day of the year
     >        imonth, ! month
     >        iyear,  ! year
     >        istyear,cdcyear  ! 1st year in data files
c
c Local variables
c
      integer i, nday,icdc,jcdc,      ! loop indice on years after istyr
     >        istat,  ! error flag for netcdf
     >        jyear,  ! iyear / nanomd 
     >        iyrdiff,
     >        iyrloop,
     >        nanomd,  ! number of years in daily anomaly file
     >        istyr,  ! 1st year in data files
     >        iwest,  ! 1st lon index for subset
     >        jnorth, ! 1st lat index for subset
     >        nanom, iyear0   ! # of years in the anomaly files
c
c Local variables
c
      real incdc(61,76)
c
      integer istart, icount, metyear,metjday

	real radn(npoi),inradn,inprec,intmax,intmin,pula,
     > urin,windin


	if(iyear.gt.1978.and.iyear.le.2004) then

       open(unit=16,file='/home/santiago/Dados/cdc/SA.daily.prcp.le.b01011978.e12312004',
     > status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*61*76)

	elseif(iyear.eq.2005.or.(iyear.eq.2006.and.jday.lt.212)) then

	   close (16)
	
        if(iyear.eq.2005.and.jday.eq.1)ireccdc = 0

       open(unit=16,file='/home/santiago/Dados/cdc/SA.daily.prcp.le.b01012005.e07312006',
     >  status='old',form='unformatted'
     > ,ACCESS='DIRECT',RECL=4*61*76)
	
	else 
	     print*,'NO CDC input for year ',iyear
	     stop 
	endif


	if((iyear.ge.1978.and.iyear.le.2005).or.(iyear.eq.2006.and.jday.lt.212)) then


     
      nday = 0
c
      if (iyear.eq.cdcyear.and.jday.eq.1) then
        do i = 1978, cdcyear-1
          nday = nday + 365
           if (mod(i,4).eq.0) then
              if (mod(i,100).ne.0) then
               nday = nday + 1
              else if (mod(iyear/100,4).eq.0) then
               nday = nday + 1
              end if
           end if
 	enddo
      ireccdc = nday
	print*,'STARTING READ CDC ',iyear,jday,' ireccdc = ',ireccdc
      end if

      ireccdc = ireccdc+1

	     read (16,REC=ireccdc) incdc   !(i,j) 

        k=0

	do j = jnorth,jnorth+nlatsub-1

	jc= 360 - j + 1  !o ponto 360 do modelo e' igual ao ponto 1 do CRU

	jcdc = (jc/2.) - 29 !o ponto 61 do CRU e' o ponto 1 do CDC, e o pont 210 do CRU e' o ponto 61 (CRU 0.5  e CDC 1.0 grau)

        jj = j - jnorth + 1

	   do i = iwest,iwest+nlonsub-1

          ii = i - iwest + 1

	 icdc= (i/2.) - 89


	if(icdc.le.0.or.icdc.gt.61.or.jcdc.le.0.or.jcdc.gt.76) then
	print*,'Warning, no CDC input for this region'
	stop
	endif

c	if(i.eq.iwest+nlonsub-1.and.j.eq.jnorth+nlatsub-1)
c      print*,jcdc,icdc,incdc(icdc,jcdc)	!jc,jcdc,i,icdc

  	      if(lmask(ii,jj).eq.1) then
             
                 k=k+1

         if(incdc(icdc,jcdc).gt.-1.) then

cvelho        xinprec(k,imonth) =  inccdc(i,jc)/(float(ndaypm(imonth)))

 	 cdcinprecd(k) =  incdc(icdc,jcdc)

	      if (jday.eq.365)
     > print*,'time',ireccdc,' ponto IBIS',k,'CDC(i,j)=',icdc,jcdc,'valor=',cdcinprecd(k)


		   else
 	print*,'CDC and Model Grids dont match',k,i,jc,'time',iyear,imonth
	   stop
		   endif

		endif !for lmask eq. 1

	    enddo !for i
	enddo     !for j

	else

	print*,'WARNING > no CDC input file ',iyear,imonth
	endif  !for years



      return
      end
c
c ---------------------------------------------------------------------
      subroutine methourly (iyear, jday, time, seed)
c ---------------------------------------------------------------------
c
c initializes surface meteorology from hourly data  
c avoids the need to go through much of subroutine 'daily' in weather.f 
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comwork.h'
      include 'comatm.h'
      include 'comhyd.h'
      include 'comsum.h'
      include 'combcs.h'
      include 'comhour.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
      integer nhours,
     >        npts,
     >        seed,
     >        jday,
     >        iyear,
     >        imonth,
     >        iday,
     >        ihtime,
     >        id1,
     >        id2,
     >        id3,  
     >        n,
     >        i,j,
     >        ihprecip,
     >        iyr,yfile,dfile,tfile,
     >        iyrinc
c      
      real time,nr,    !nr - means that we are not reading these var
     >     rwork,
     >     var3sum,
     >     var3max,
     >     var3min,prec_30m_1h,
     >     chtime,
     >     psurfi,
     >     tdave,
     >     shum,
     >     elevm, 
     >     ran2 
c
      character*100 header
      character*100 metfile
      character*30  hstg
      character*39  pathn   ! has to be exact length of input string
      
c
      save nhours,
     >     var3sum,
     >     var3max,
     >     var3min
c
      include 'comsat.h'
c
c      pathn = 'input_1d/'
c      metfile = pathn//'met_USR_30min.dat'
       metfile = 'input_1d/met_USR_30min.dat'
c      npts = 8760  ! number of hourly points in non-leap year 
c
c        if (mod(iyear,4) .eq. 0) then
c           if (mod(iyear,100) .ne.0) then
c            npts = 8784
c           else if (mod(iyear/100,4).eq.0) then
c            npts = 8784
c           endif
c        endif
c

csant- var1 to 6 are: prec, insolation, air temp, relat. Hum., wind veloc., atm pressure
		n=1   !every time step reads the vars

	if(iyear.eq.imetyear.and.jday.eq.dmetyear.and.time.eq.0) then

            open(232,file = metfile) !first time - open the file and let opened

	     do j = 1, 12*48*365  !find the time in the file
        read(232,*) yfile,dfile,tfile,
     > var1(n),var2(n),var3(n),var4(n),var5(n),var6(n)

      if(iyear.eq.yfile.and.jday.eq.dfile.and.time.eq.tfile)
     > goto 1212   !nice, find the date...
      	     enddo

	print*,'didnt find the time in the met inpunt file!'
	stop

	else
	prec_30m_1h= 0.
	          do j =1,2           !find the time, depens on IBIS dtime
        read(232,*) yfile,dfile,tfile,
     > var1(n),var2(n),var3(n),var4(n),var5(n),var6(n)

	prec_30m_1h= prec_30m_1h + var1(n) ! in this case prec is 30m, so sum for 1 hour

      if(iyear.eq.yfile.and.jday.eq.dfile.and.time.eq.tfile)
     > goto 1212   !nice, find the date...
  	          enddo
     	print*,'WARNING: model and file dates dont match!!'
	print*,iyear,yfile,jday,dfile,time,tfile*dtime
	stop


	endif

 1212  continue

	var1(n) = prec_30m_1h
c
c	if(time.eq.13*dtime) print*,iyear,jday,time,var3(n)

        rwork = (grav / rair / 0.0065)
c
c set elevation for the arlington grid cells
c based on elevation reading (meters) at morrisonville
c
        elevm = 552.
c
c
          if (time .eq. 0.0) then
                    var3sum = 0.0
                    nhours  =  0
csant-              call const (tmin, npoi, 400.0)
casnt-              call const (tmax, npoi, 200.0)

           call const (var3max, npoi, 200.0)
           
	    call const (var3min, npoi, 400.0)

         endif              
c
                  nhours  = nhours + 1

csant                  var3min = min(tmin(1), var3(n) + 273.16)
csant                  var3max = max(tmax(1), var3(n) + 273.16)

                  var3min = min(var3min, var3(n) + 273.16)

                  var3max = max(var3max, var3(n) + 273.16)


                  psurfi  = 101325.0 * ((var3(n) + 273.16) /
     >                      ((var3(n) + 273.16) + 0.0065 * 
     >                        elevm)) ** rwork

                  call const (psurf, npoi, psurfi)         ! surface pressure

                  call const (psurf, npoi, var6(n)*100)    ! reading surface pressure in hPa
	
c
c for precipitation, value at time n is precip accumulated since last
c time step, so we need to get the precip value for the time step n+1 
csant- I change it, but I have to talk with Chris to check!!!

csant                  ihprecip = min(n+1, npts)  ! use precip from the next time step
                  call const (precip, npoi, var1(n)* dtime / 3600.)             ! precipitation (mm)  

	
                  call const (ta, npoi, var3(n) + 273.16)  ! air temperature
 
                 var3sum = var3sum + var3(n) + 273.16	   ! daily air temperature 

                  call const (cloud, npoi, var2(n))        ! insolation
c
                  if (var4(n) .lt. 1.) then
                    print *, 'rh too low! ', var4(n)
                    print *, 'month, day, hour ', imonth, iday, ihtime,n
                    stop 33
                  endif 
c               
c convert relative humidity to specific humidity
c
                  shum  = var4(n) / 100. * qsat(esat(var3(n) + 273.16),
     >                      psurfi) 
c
                  call const (qd, npoi, shum)             ! specific humidity 
                  call const (ua, npoi, var5(n))           ! wind velocity
c

                  if (time .eq. (24.*3600.) - dtime) then

csant -  I think that this should happen only in the end of the day,
csant -- other wise the tmin that sum a10tmin, ill be the current value till get the daily real.
                  call const (tmin, npoi, var3min)         ! minimum temp
                  call const (tmax, npoi, var3max)         ! maximum temp

                     var3sum = var3sum / float(nhours)
c
c three ways to compute daily average temp
c * average of hours        :  tdave = var3sum
c * weighted of min and max :  tdave = 0.44 * var3max + 0.56 * var3min
c * average of min and max  :  tdave = (var3min + var3max) / 2 
c
                     tdave = var3sum
                     call const (td, npoi, tdave)
c                    call dailymet(imonth, iday, seed, tdave, 0)
                     call dailymet(imonth, iday, seed, jday)
		endif

c
c return to main program
c
      return
      end



c ---------------------------------------------------------------------
      subroutine methourlyfays (iyear, jday, time, seed)
c ---------------------------------------------------------------------
c
c initializes surface meteorology from hourly data  
c avoids the need to go through much of subroutine 'daily' in weather.f 
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comwork.h'
      include 'comatm.h'
      include 'comhyd.h'
      include 'comsum.h'
      include 'combcs.h'
      include 'comhour.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
      integer nhours,
     >        npts,
     >        seed,
     >        jday,
     >        iyear,
     >        imonth,
     >        iday,
     >        ihtime,
     >        id1,
     >        id2,
     >        id3,  
     >        n,
     >        i,j,
     >        ihprecip,
     >        iyr,yfile,dfile,tfile,
     >        iyrinc
c      
      real time,nr,    !nr - means that we are not reading these var
     >     rwork,
     >     var3sum,
     >     var3max,
     >     var3min,prec_30m_1h,
     >     chtime,
     >     psurfi,
     >     tdave,
     >     shum,
     >     elevm, 
     >     ran2 
c
      character*100 header
      character*100 metfile
      character*30  hstg
      character*39  pathn   ! has to be exact length of input string
      
c
      save nhours,
     >     var3sum,
     >     var3max,
     >     var3min
c
      include 'comsat.h'
c
c      pathn = 'input_1d/'
c      metfile = pathn//'met_USR_30min.dat'

c       metfile = 'input_1d/met_USR_30min.dat'
       metfile = 'input_1d/met_FAYS_60min.dat'
c      npts = 8760  ! number of hourly points in non-leap year 
c
c        if (mod(iyear,4) .eq. 0) then
c           if (mod(iyear,100) .ne.0) then
c            npts = 8784
c           else if (mod(iyear/100,4).eq.0) then
c            npts = 8784
c           endif
c        endif
c

csant- var1 to 6 are: prec, irradiance, air temp, relat. Hum., wind veloc., atm pressure
		n=1   !every time step reads the vars

	if(iyear.eq.imetyear.and.jday.eq.dmetyear.and.time.eq.0) then

            open(232,file = metfile) !first time - open the file and let opened

	     do j = 1, 1000000  !find the time in the file
        read(232,*) yfile,dfile,tfile,
     > var1(n),var2(n),var3(n),var4(n),var5(n),var6(n)


	print*,iyear, jday, time,' file find-> ',yfile,dfile,tfile

      if(iyear.eq.yfile.and.jday.eq.dfile.and.time.eq.tfile)
     > goto 1212   !nice, find the date...
      	     enddo

	print*,'didnt find the time in the met inpunt file!'
	stop

	else


        read(232,*) yfile,dfile,tfile,
     > var1(n),var2(n),var3(n),var4(n),var5(n),var6(n)

	print*,iyear, jday, time,' file in-> ',yfile,dfile,tfile


	prec_30m_1h=  var1(n) ! in this case prec is 30m, so sum for 1 hour

      if(iyear.eq.yfile.and.jday.eq.dfile.and.time.eq.tfile)
     > goto 1212   !nice, find the date...

     	print*,'WARNING: model and file dates dont match!!'
	print*,iyear,yfile,jday,dfile,time,tfile
	stop


	endif

 1212  continue

	var1(n) = prec_30m_1h
c
c	if(time.eq.13*dtime) print*,iyear,jday,time,var3(n)

        rwork = (grav / rair / 0.0065)
c
c set elevation for the arlington grid cells
c based on elevation reading (meters) at morrisonville
c
        elevm = 552.
c
c
          if (time .eq. 0.0) then
                    var3sum = 0.0
                    nhours  =  0
csant-              call const (tmin, npoi, 400.0)
casnt-              call const (tmax, npoi, 200.0)

           call const (var3max, npoi, 200.0)
           
	    call const (var3min, npoi, 400.0)

         endif              
c
                  nhours  = nhours + 1

csant                  var3min = min(tmin(1), var3(n) + 273.16)
csant                  var3max = max(tmax(1), var3(n) + 273.16)

                  var3min = min(var3min, var3(n) + 273.16)

                  var3max = max(var3max, var3(n) + 273.16)


                  psurfi  = 101325.0 * ((var3(n) + 273.16) /
     >                      ((var3(n) + 273.16) + 0.0065 * 
     >                        elevm)) ** rwork

                  call const (psurf, npoi, psurfi)         ! surface pressure

                  call const (psurf, npoi, var6(n)*100)    ! reading surface pressure in hPa
	
c
c for precipitation, value at time n is precip accumulated since last
c time step, so we need to get the precip value for the time step n+1 
csant- I change it, but I have to talk with Chris to check!!!

csant                  ihprecip = min(n+1, npts)  ! use precip from the next time step
                  call const (precip, npoi, var1(n)* dtime / 3600.)             ! precipitation (mm)  

	
                  call const (ta, npoi, var3(n) + 273.16)  ! air temperature
 
                 var3sum = var3sum + var3(n) + 273.16	   ! daily air temperature 

                  call const (cloud, npoi, max(var2(n),0.0))        ! insolation
c
                  if (var4(n) .lt. 1.) then
                    print *, 'rh too low! ', var4(n)
                    print *, 'month, day, hour ', imonth, iday, ihtime,n
                    stop 33
                  endif 
c               
c convert relative humidity to specific humidity
c
                  shum  = var4(n) / 100. * qsat(esat(var3(n) + 273.16),
     >                      psurfi) 
c
                  call const (qd, npoi, shum)             ! specific humidity 
                  call const (ua, npoi, var5(n))           ! wind velocity
c

                  if (time .eq. (24.*3600.) - dtime) then

csant -  I think that this should happen only in the end of the day,
csant -- other wise the tmin that sum a10tmin, ill be the current value till get the daily real.
                  call const (tmin, npoi, var3min)         ! minimum temp
                  call const (tmax, npoi, var3max)         ! maximum temp

                     var3sum = var3sum / float(nhours)
c
c three ways to compute daily average temp
c * average of hours        :  tdave = var3sum
c * weighted of min and max :  tdave = 0.44 * var3max + 0.56 * var3min
c * average of min and max  :  tdave = (var3min + var3max) / 2 
c
                     tdave = var3sum
                     call const (td, npoi, tdave)
c                    call dailymet(imonth, iday, seed, tdave, 0)
                     call dailymet(imonth, iday, seed, jday)
		endif

c
c return to main program
c
      return
      end
c



c
c -------------------------------------------------------------------------
c HOW TO READ/WRITE NETCDF FILES IN IBIS
c -------------------------------------------------------------------------
c Reading/writing files in ibis is done through subroutines in
c ies-io.f.  The most important concept to understand is the
c relationship between the locations of points in an n-dimensional
c array and the values of istart and icount.  In FORTRAN, values in an
c array are stored so that the first dimension varies the fastest.  In
c C, the last dimension varies the fastest.  If you were to use ncdump
c on a netcdf file whose variable 'mydata' has 4 dimensions (latitude,
c longitude, level, time), the variable would be shown as follows:
c mydata(time, level, latitude, longitude)
c because ncdump was written in C and reflects C's ordering of dimensions.
c In this example time varies the the slowest and longitude the fastest.
c If you were to define this variable in a FORTRAN program so that
c time again varies the slowest and longitude the fastest, it would
c look like this:
c     real mydata(nlons, nlats, nlevels, ntimes)
c where nlons, nlats, nlevels, and ntimes are integer parameters of
c some specified size.  Looping through the data in the order of
c storage in memory would look like this:
c     do l = 1, ntimes
c       do k = 1, nlevels
c         do j = 1, nlats
c           do i = 1, nlons
c             mydata(i,j,k,l) = ...
c           enddo
c         enddo
c       enddo
c     enddo
c Since ies-io.f is FORTRAN code, keep in mind the FORTRAN
c representation will be flipped in comparison to what you see from
c ncdump.
c The netcdf interface reads and writes data using two integer vectors,
c istart, and icount.  Istart refers to the starting point for reading/writing
c along each dimension.  If you have a 4-d variable as above and want
c to write starting at the first point, istart would be a vector of
c length 4 and have the values (1,1,1,1). Icount refers to how far
c along each dimension data will be read/written.  If you wish to
c write the entire array in the sample FORTRAN code above, icount
c would be a vector of length 4 and have the values
c (nlons,nlats,nlevels,ntimes).
c Things get a little more complicated when you want to read/write
c only a portion of the variable.  In ibis we typically read/write a
c single time step, but possibly more than one level/pft and either an
c entire or partial lat/lon grid. Below are examples of istart and
c icount for various situations of reading/writing.
c 1) an entire lat/lon grid for only one (6th) time step and one (2nd) pft:
c istart is (1,1,2,6), icount is (nlons,nlats,1,1)
c
c 2) entire lat/lon arrays for all 9 pfts at one (6th) time step:
c istart is (1,1,1,6), icount is (nlons,nlats,npfts,1)
c
c 3) a single lat/lon point (at the ilon-th longitude and the ilat-th
c latitude) of a 3-d variable at all 100 time steps:
c istart is (ilon,ilat,1), icount is (1,1,100)
c Note that if istart and icount have been declared length 4, the 4th
c value in each is ignored when referencing a 3-d variable.
c
c 4) a subsection of a lat/lon grid, 20 points wide in longitude, 15
c points high in latitude, starting at point (ilon,ilat), at one (18th)
c level and 12 times, beginning at time step itime:
c istart is (ilon,ilat,18,itime) icount is (20,15,1,12)
c
c HOW TO ADD NEW CODE TO READ A FILE:
c To read a file, use subroutine readvar in ies-io.f. This subroutine
c assumes that the variable being read has dimensions called longitude,
c latitude, possibly time, and possibly another dimension, which is
c named in the call.  Only the bare essentials are returned.
c
c General call:
c     call readvar(filen,varname,name3d,istart,icount,values,
c    > alons,alats,vals3d,times,ierror)
c
c INPUT
c     filen - character*(*) - file name from which to read data
c     varname - character*(*) - name of variable from which to read
c     name3d - character*(*) - name of 3rd, nontime dimension variable.
c      Ignored if varname variable is only 3-d.
c     istart - integer(*) - starting points along each dimension of
c      the variable, for example the 1st point a 4-d variable is (1,1,1,1) 
c      and the 3rd level and 2nd time step of a 4d variable is (1,1,3,2).
c     icount - integer(*) - number of points along each dimension to read,
c      for example, to read in a single lat/lon grid from a 4-d variable,
c      icount would be (nlon,nlat,1,1)
c OUTPUT
c     values - real(*) - returned real values of the designated hyperslab
c     alons - real(*) - longitude vector for those points read in
c     alats - real(*) - latitude vector for those points read in
c     vals3d - real(*) - vector of values of the 3rd dimension (unchanged if
c      variable is 2- or 3-d, or if 3rd dimension has character values).
c     times - real(*) - time vector vector for those points read in (unchanged
c      if variable is 2-d).
c     ierror - integer - error code = 0 if no error, < 0 if error occurred
c
c Example: Read in an entire lat/lon grid for one (9th) pft (dimension 
c name is 'pft') and one (24th) time
c     parameter (nlons = 360, nlats=180)
c     real x(nlons,nlats), alons(nlons), alats(nlats), xjunk, time
c     integer istart(4), icount(4), ierr
c     istart(1) = 1
c     istart(2) = 1
c     istart(3) = 9
c     istart(4) = 24
c     icount(1) = nlons
c     icount(2) = nlats
c     icount(3) = 1
c     icount(4) = 1
c     call readvar('myfile.nc','myvar','pft',istart,icount,x,alons,alats,
c    > xjunk,time,ierr)
c     if (ierr .lt. 0) then
c       print *, 'Error occurred in readvar'
c     end if
c Note that above, I used a junk variable for the values of the 3rd
c dimension, because in ibis, the pft dimension is a character
c dimension.  If the 3rd dimension type is character, readvar does not
c return the value.
c
c Ibis-specific example: read in a variable which has 3rd dimension
c 'layer', using cdummy to store full array before extracting land
c points into the variable used directly in ibis.  The variable work is
c used to store returned info that won't be used later.
c Previously declared
c      istart(1) = 1        !begin at 1st longitude point
c      istart(2) = 1        !begin at 1st latitude point
c      istart(3) = 1        !begin at 1st layer
c      istart(4) = 1        !begin at 1st time
c      icount(1) = nlon     !read nlon points along longitude dimension
c      icount(2) = nlat     !read nlat points along latitude dimension
c Read in data in file input/soita.nc to ibis variable tex
c      icount(3) = nsoilay  !read nsoilay points along layer dimension
c      icount(4) = 1        !read one time step
c      filen = 'input/soita.nc'
c      aname = 'tex'
c      call readvar(filen,aname,'layer',istart,icount,
c     > cdummy,work(1),work(ndim4),work(2*ndim4),work(3*ndim4),istat)
c      if (istat.lt.0) then
c         write(*,9000)
c         print *, 'while reading soita'
c         stop 1
c      end if
c For each lat/lon grid, strip off ocean points. tex only has land points.
c      do 12 j = 1, nsoilay
c        call arr2vec (cdummy((j-1)*nlonsub*nlatsub + 1), tex(1,j))
c 12   continue
c
c HOW TO ADD NEW CODE TO WRITE TO A FILE:
c Writing to a file involves several steps.  First, if the file does
c not yet exist, you must create the file using inifile or inifilec.
c Then, you must create one or more variables using inivar.  Then after
c all initializing is done, declare the end of the initialization phase
c using endini.
c Finally, once the file and variable exist, you can write data into
c the variable using writevar.
c Creating and writing to files in seperate steps may seem
c complicated at first, but this 4-step method allows much flexibility.
c Not only do you have the choice of creating a variable without a 3rd
c dimension, with a real 3rd dimension, or with a character 3rd
c dimension, you may also have more than one variable within the same
c file.
c Note that when using ies-io.f routines if you have more
c than one variable in the file, and both have 4 dimensions, that 3rd
c dimension (level, pft, etc.) must be shared by both variables.  For
c example, you may have one variable whose 3rd dimension is pft and
c another variable which does not have a 3rd non-time dimension in the
c same file, but you may not have in the same file one variable whose
c 3rd dimension is pft and another variable whose 3rd dimension is level.
c
c 1) Initialize a file
c If you wish a file to contain only latitude, longitude, and time
c dimensions, or want it to also have a 3rd real dimension, use
c inifile.  If you want to the file to have a 3rd character dimension,
c use inifilec.
c General call for inifile:
c      call inifile(idies,filen,title,source,history,nlon,alons,
c     > nlat,alats,name3rd,long3rd,units3rd,n3rd,vals3rd,pos3rd,tunits,
c     > calendar,ierror)
c
c INPUT
c     filen - character*(*) - name for new file
c     title - character*(*) - title for file
c     source - character*(*) - source for file
c     history - character*(*) - date of creation, and possibly author
c     nlon - integer - number of point along longitude direction
c     alons - real(nlon) - values of longitudes
c     nlat - integer - number of point along latitude direction
c     alats - real(nlat) - values of latitudes
c     name3rd - character*(*) - name of 3rd dimension - use '' if you
c      don't want this dimension ('' is the empty string)
c     long3rd - character*(*) - long name for 3rd dimension variable
c      (ignored if nam3rd='')
c     n3rd - integer - size of 3rd dimension (ignored if name3rd='')
c     vals3rd - real(n3rd) - values along 3rd dimension (ignored if 
c      name3rd='')
c     pos3rd - character*(*) - if the 3rd dimension exists and refers to
c      a measured level or height, use 'up' if values increase with distance
c      above earth (as in height), or 'down' if values decrease with distance 
c      (as in pressure) (ignored if name3rd=''). If 3rd dimension does not
c      refer to a height or level, use ''.
c     tunits - character*(*) - units for time, must be in the form of days
c      since yyyy-mm-dd tt:tt:tt, where yyyy is a year or 0000, mm is a
c      month, dd is a day, and tt:tt:tt is (optional) time or 00:00:00.
c     calendar - character*(*) - type of calendar.  Choose from 'noleap',
c      'gregorian','n ka BP', etc.  Use iescal (in ies.f) if orbital 
c      parameters differ from modern.
c OUTPUT
c     idies - integer - id number of new file for later use
c     ierror - integer - error code, 0 = no error, < 0 = an error occured
c
c Example: Create a file which will hold fractional snow
c cover (only 3-d) and snow thickness for each layer (4-d).
c previously defined: 
c
c     parameter (nlons = 360, nlats = 180, nsnolay = 6)
c     real lonscale(nlons), latscale(nlats), slayers(nsnolay)
c     real snowc(nlons,nlats), snowh(nlons,nlats,nsnolay)
c     integer istat
c     character*80 cdate, tunits
c     alats = ...   ! create values for latitude somehow
c     alons = ...   ! create values for longitude somehow
c     slayers = ... ! create values for snow layers somehow
c     cdate = 'created on 6/29/97'
c     tunits = 'days since 1969-12-31'
c Now initialize a file with a 3rd real variable
c     filen = 'snow.nc'
c     call inifile(idies,filen,
c    > 'file for snow variables',
c    > 'C Molling, program.f v1.01',cdate,nlons,lonscale,nlats,latscale,
c    > 'snowlayer','snow layers top to bottom','',nsnolay,slayers,
c    > 'down',tunits,'gregorian',istat)
c     if (istat .lt. 0)then
c       print *, 'Error in inifile'
c     end if
c Note above, the empty string ('') is used because the 3rd dimension,
c snowlayer, does not have any units.  The 'positive' attribute for
c the 3rd dimension is 'down' because snowlayer 1 is at the top and the last
c layer is at the bottom (distance above the center of the earth is
c decreasing as snowlayer increases).  The calendar is gregorian
c because there are leap years included.  If all years are 365 days, you
c should use 'noleap'.  Units of time are days dince a date of the
c form yyyy-mm-dd.  Time units should use this form to be compatible with
c GrADS.  Other units should be compatible with Udunits (see
c www.unidata.ucar.edu).  The returned variable idies will be used in
c the subroutine that initializes a variable.
c
c General call for inifilec
c     call inifilec(idies,filen,title,source,history,nlon,alons,
c    > nlat,alats,name3rd,long3rd,units3rd,n3rd,len3rd,chars3rd,
c    > tunits,calendar,ierror)
c
c INPUT
c     filen - character*(*) - name for new file
c     title - character*(*) - title for file
c     source - character*(*) - source for file
c     history - character*(*) - date of creation, and possibly author
c     nlon - integer - number of point along longitude direction
c     alons - real(nlon) - values of longitudes
c     nlat - integer - number of point along latitude direction
c     alats - real(nlat) - values of latitudes
c     name3rd - character*(*) - name of 3rd dimension
c     long3rd - charqcter*(*) - long name for 3rd dimension variable
c     n3rd - integer - size of 3rd dimension
c     len3rd - integer length of chracter strings in vals3rd
c     chars3rd - character*len3rd(n3rd) - values along 3rd dimension
c     tunits - character*(*) - units for time, must be in the form of days
c      since yyyy-mm-dd tt:tt:tt, where yyyy is a year or 0000, mm is a
c      month, dd is a day, and tt:tt:tt is (optional) time or 00:00:00
c     calendar - charater*(*) - type of calendar.  Choose from 'noleap',
c      'gregorian','n ka BP', etc.  Use iescal if orbital parameters
c      differ from modern.
c OUTPUT
c     idies - integer - id number of new file for later use
c     ierror - integer - error code, 0 = no error, < 0 = an error occured
c
c Example: Create a file that has a character 3rd dimension.
c Defined previously - most variables as in previous example, plus...
c      parameter (npft = 9)
c      character*80 pftdef(npft)
c      pftdef(1) = 'boreal evergreens'
c      pftdef(2) = ...
c      etc...
c Initialize file with inifilec
c      filen = 'exist.nc'
c      call inifilec(idies,filen,
c     > 'annual existence of each plant functional type',
c     > 'ibis wyearly',cdate,nlon,lonscale,nlat,latscale,
c     > 'pft','plant fuctional type','',npft,80,pftdef,
c     > tunits,'gregorian',istat)
c The '80' above refers to the length of the character strings in pftdef.
c         dimnames(3) = 'pft'
c         call inivar(idies,'exist','existence for each pft',
c     >    '',4,dimnames,OCEAN,istat)
c End initialization phase
c         call endini(idies,istat)
c
c 2) Initialize a variable and end initialization phase
c After you initialize a file, you need to initialize a variable.
c Initializing reserves space for data to be written into the file, and
c saves information about the data (such as units, descriptive name, and
c a missing value).  You can use inivar to initialize any variable of 1
c or more dimensions, provided that those dimensions already exist in
c the file.  You may initialize more than one variable in a single
c file, as stated above.
c If you have a special value that denotes 'missing', such as using
c 1.e+36 to denote ocean grid cells, use this value for valmissing.  If
c you use 0. for valmissing, it is ignored.  Pick a value for valmissing
c which is well outside the valid range for the data.  For example,
c -99. would be a fine missing value for pressure, but not a good value
c for topography.
c The character array dimnames is used to store the
c names of the dimensions upon which the new variable will depend.
c For example, a 3-d variable would only depend on longitude, latitude,
c and time.  A 4-d variable would depend on those and also pft, or
c snowlater, or level, or some other dimension.  The dimension names
c must be in the same order of varying: fastest to slowest.  See the
c discussions for istart and icount above.  For example, the 4-d
c variable to go in the file snow.nc (above inifile example)  would have
c dimnames as follows
c      dimnames(1) = 'longitude'
c      dimnames(2) = 'latitude'
c      dimnames(3) = 'snowlayer'
c      dimnames(4) = 'time'
c
c General call for inivar:
c      call inivar(idies,varname,longname,units,ndims,dimnames,
c     > valmissing,ierror)
c
c INPUT
c     idies - integer - id number of a new file from a previous call
c      to inifile, inifilec, or iesopen
c     varname - charcter*(*) - name for new variable
c     longname - character*(*) - descriptive name for variable
c     units - character*(*) - units for variable, compatible with udunits
c     ndims - integer - number of dimensions for this variable
c     dimnames - character*(*)(ndims) - name of each dimension, in order
c     valmissing - real - missing value, ignored if valmissing=0.
c OUTPUT
c     ierror - integer - error code, 0 = no error, < 0 = error occurred
c
c Example: Define a 4-d and a 3-d variable to go into the file snow.nc.
c defined previously
c        character*80 dimnames(4)
c        real OCEAN
c        dimnames(1) = 'longitude'
c        dimnames(2) = 'latitude'
c        OCEAN = 9.e+20
c define 4-d variable
c        dimnames(3) = 'snowlayer'
c        dimnames(4) = 'time'
c        call inivar(idies,'hsno','snow layer thickness',
c     >   'meters',4,dimnames,OCEAN,istat)
c define 3-d variable in same file
c        dimnames(3) = 'time'
c        call inivar(idies,'snowf','snow cover fraction',
c     >   'fraction',3,dimnames,OCEAN,istat)
c end initialization phase
c         call endini(idies,istat)
c Notice that you need to change the value of dimnames(3) for the 3-d
c variable.  The value in dimnames(4) gets ignored.
c
c 3) End the initialization phase for the file
c When you are done initializing the file and the variables in the file, you
c must call endini.  This subroutine merely closes the file.  But by closing
c the file, this is a signal to the computer to write out all changes to this
c file, synchronizing the instructions in the program with what is written on
c the hard disk.  Since most computers buffer, that is save up, data until a
c there is a large chunk to write, it is essential that all buffered
c information for a netcdf file be written to disk before attempting to write
c data to the file.  The subroutine endini accomplishes this.  The subroutine
c endini should be called after initializing the last variable in the netcdf
c file (inivar) and before calling writevar.  See the example above.
c
c 4) Write to a variable
c After you have completed the initialization phase by initializing the file
c and initializing one or more variables in the file, you can write data 
c into the space reserved for each variable with subroutine writevar.
c You need to supply istart and icount, just as when you read a
c variable.  It is perfectly legal to write values in only a portion of the
c space reserved for the variable.  If nothing is written to a
c particular grid point, it has a special fill value supplied by netcdf
c when the variable was initialized. Notice that for ease of use, I
c have designed writevar to also write in the values of the time steps
c you are writing as well as the weighting for that time step.
c Time weighting refers to the number of days in that time sample.
c For example, some monthly means will be a mean value over 31 days,
c while others will be over only 30, or 28, or 29 days.  I assumed that
c most persons will calculate data and write it out one time step at a
c time, so it is nice to write out the time values and weight as you
c go along.  Another time-related item written is the character label
c for the time step.  Since after a few years, it's hard to keep track
c of what month is represented by a large number of days, I invented the
c date variable.  Date is a 10-character long string in which you can
c put a time label.  For example the time value 4745 may not be
c informative, but the data label 'JAN1913   ' is.  I usually use 9
c characters plus a null character at the end (char(0)).  Use strings
c like 'DJF001005 ' to denote time averages (Winter years 1 through 5).
c
c General call
c     call writevar(filen,varname,istart,icount,values,
c    > times,tweights,dates,ierror)
c
c INPUT
c     filen - character*(*) - file name to which to write data
c     varname - character*(*) - name of variable to which to write
c     istart - integer(*) - starting points along each dimension of
c      the variable, for example the 1st point a 4-d variable is (1,1,1,1) 
c      and the 3rd level and 2nd time step of a 4d variable is (1,1,3,2).
c     icount - integer(*) - number of points along each dimension to write,
c      for example, to write a single lat/lon grid to a 4-d variable,
c      icount would be (nlon,nlat,1,1).
c     values - real(*) - real values of the designated hyperslab
c     times - real(*) - time vector vector for those points written (ignored
c      if variable is 2-d).
c     tweights - real(*) - number of days in each time step in times (ignored
c      if variable is 2-d).
c     dates - character*10(*) - character labels for each time step in times,
c      last character should be a null (dates ignored if variable 2-d).
c OUTPUT
c     ierror - integer - error code = 0 if no error, < 0 if error occurred
c
c Example: write data to the 4-d variable initialized above
c previously defined
c     parameter (ndim3=nlons*nlats*nsnolay)
c     real cdummy(ndim3), hsno(npts,nsnolay)
c     real time, timewght
c     integer istart(4), icount(4)
c     character*10 cdate
c     time = 730.
c     timewght = 365.
c     istart(1) = 1
c     istart(2) = 1
c     istart(3) = 1
c     istart(4) = 2
c     icount(1) = nlons
c     icount(2) = nlats
c     icount(3) = nsnolay
c     icount(4) = 1
c     cdate = 'ANN1980  '//char(0)
c put land-only data in hsno into land and sea lat/lon grid in workspace
c     do 20 k = 1, nsnolay
c        call vec2arr (hsno(1,k), cdummy((k-1)*nlonsub*nlatsub + 1))
c 20  continue
c     call writevar('snow.nc','hsno',istart,icount,cdummy,time,
c    > timewght,cdate,istat)
c     if (istat .ne. 0) then
c        write(*,*) 'ERROR writing hsno'
c        stop 1
c     end if
c
c Some other thoughts:
c It's a good idea to end all strings stored in netcdf file with a
c null character.  Some less-robust C programs may fail when
c encountering non-null-ended strings, because in C, all strings end
c with a null character.
c Udunits provides several ways to express multiplication, powers,
c etc.  I like to use ^ to denote exponents (as in m^2 instead of just
c m2) because programs like Matlab see ^ as a LaTeX command to write the
c next character as a superscript.  Likewise I try to avoid using _,
c because this is the LaTeX command to make the next character a
c subscript.  So instead of using deg_C, I use degC, to prevent the C
c from becoming a subscript.
c The format of the netcdf files written by ies-io.f subroutines is
c meant to be compatible with the COARDS and CSM conventions (see
c www.cgd.ucar.edu:80/csm/experiments/output/format.html).
c Theoretically, you should be able to use NCO (see
c www.cgd.ucar.edu:80/cms/nco/index.html) and NCL (see
c ngwww.ucar.edu/ngdoc/ng4.1alpha/ug/ncl/ncloview.html) on the files.
c A few other packages that work with this format are GrADS
c (grads.iges.org/grads/head.html) and Ncview
c (meteora.ucsd.edu/~pierce/ncview_home_page.html).
c The routines in ies-io.f are just the beginning.  You can put many
c more things in a netcdf file by using routines in ies.f or by using
c the low-level netcdf commands directly.  Use one of the pre-existing
c subroutines in ies-io.f as a template and go on from there.  You are
c free to change or distribute my code as long as you 1) acknowlege me,
c Christine C. Molling (cmolling@facstaff.wisc.edu) as the author of
c the original code and 2) do not sell it.
c Of course if there is anything wrong with the code, or you somehow
c encounter damage to your reputation/wallet because of its use, you
c are forbidden to sue me or anyone else.
c Good Luck!
