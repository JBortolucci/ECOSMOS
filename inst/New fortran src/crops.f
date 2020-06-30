c Last modified by C. Kucharik 7.06.05
c 
c program crops.f
c ----------------------------------------------------------------------------
      subroutine initialcrop
c ----------------------------------------------------------------------------
c
c called from main.f or physiology.f
c
c subroutine to initialize crop related variables that are dependent  
c on a crop growing season basis.  Variables are initialized at the end
c of the growing season after harvest date and after data is written out (io.f)  
c
c additionally, this routine needs to be called at (1) first time
c crops are grown in a model run (a restart), or (2) when
c crops are replacing natural vegetation (which could also be a restart)
c
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
      integer iy, i, ic
c
c reset variables after harvest date and data is written 
c to io.f and crop diagnostic output files
c have to make sure there is no memory in the system
c of upper canopy or lower canopy vegetation when crops
c replace natural vegetation!
c
               call const(plai,      npoi*npft, 0.01)
               call const(thrlai,    npoi*npft, 0.0)
               call const(peaklai,   npoi*npft, 0.0)
               call const(cbiol,     npoi*npft, 0.0)
               call const(cbios,     npoi*npft, 0.0)    
               call const(cbior,     npoi*npft, 0.0)   
               call const(cbiow,     npoi*npft, 0.0) 
               call const(cbiog,     npoi*npft, 0.0)  
               call const(hui,       npoi*npft, 0.0) 
               call const(aleaf,          npft, 0.0) 
               call const(aroot,          npft, 0.0)
               call const(astem,          npft, 0.0)      
               call const(arepr,          npft, 0.0)  
               call const(awood,          npft, 0.0)
               call const(aybprod,   npoi*npft, 0.0) 
               call const(ayrprod,   npoi*npft, 0.0)  
               call const(ayabprod,  npoi*npft, 0.0) 
               call const(aylprod,   npoi*npft, 0.0) 
               call const(harvidx,   npoi*npft, 0.0) 
               call const(leafout,   npoi*npft, 0.0) 
               call const(htmx,      npoi*1,    0.0) 
               call const(cumlvs,    npoi*npft, 0.0) 
               call const(plaimx,    npoi*npft, 0.0) 
               call const(dpgf,      npoi*npft, 0.0)
               call const(biomass,   npoi*npft, 0.0) 
               call const(totnuptake,npoi*npft, 0.0) 
               call const(tnplant,   npoi*npft, 0.0) 
               call const(totnfix,   npoi*npft, 0.0) 
               call const(idpp,      npoi*npft, 0.0) 
               call const(idpe,      npoi*npft, 0.0) 
               call const(fixn,      npoi*npft, 0.0) 
               call const(gddplant,  npoi*npft, 0.0) 
               call const(crmclim,   npoi*npft, 0.0) 
               call const(crmact,    npoi*npft, 0.0) 
               call const(crmplant,    npoi*npft, 0.0) 
               call const(grainday,  npoi*npft, 9999.0)
               call const(gddtsoi,   npoi*npft, 0.0) 
               call const(fertinput, npoi*npft, 0.0) 
               call constint (pstart, npoi*npft, 999)
               call constint (cdays, npoi, 0)
               call constint (cropy, npoi, 0)
               call constint (ik, npoi, 0)
               call const (cropout, npoi*npft*60, -999. )
     
c grid cell variables - not dependent on pft
c
		qgalho    = 0
               call const(sai,     npoi*2, 0.0)
               call const(fu,      npoi,   0.0)
               call const(lai,     npoi*2, 0.0)
               call const(zbot,    npoi*2, 0.0) 
               call const(ztop,    npoi, 0.0)  
               call const(totbiou, npoi,   0.0) 
               call const(totbiol, npoi,   0.0) 
               call const(totlaiu, npoi,   0.0) 
               call const(totlail, npoi,   0.0) 
               call const(vf,      npoi,   0.0) 
               call constint(ncyears, npoi,   0) 

c
c
      return
      end

c
c ----------------------------------------------------------------------------
        subroutine startplanting (iyear0,iyear)
c ----------------------------------------------------------------------------
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comcrop.h'
c
      integer i,iyear0,iyear       

	if (nratoon.eq.4) then
	do i=1,npoi
       if(mod(i,6).eq.0.and.iyear.ge.iyear0)   ncyears(i)=ncyears(i)+1 
       if(mod(i,6).eq.1.and.iyear.ge.iyear0+1) ncyears(i)=ncyears(i)+1 
       if(mod(i,6).eq.2.and.iyear.ge.iyear0+2) ncyears(i)=ncyears(i)+1 
       if(mod(i,6).eq.3.and.iyear.ge.iyear0+3) ncyears(i)=ncyears(i)+1 
       if(mod(i,6).eq.4.and.iyear.ge.iyear0+4) ncyears(i)=ncyears(i)+1 
       if(mod(i,6).eq.5.and.iyear.ge.iyear0+5) ncyears(i)=ncyears(i)+1 
	enddo
	endif

        return
 
       end


c
c ----------------------------------------------------------------------------
      subroutine rotation(irestart, irstyear, iyear)
c ----------------------------------------------------------------------------
c
c subroutine used to plant different crop types in different years
c to simulate typical crop rotations in the United States 
c could be modified in future if rotations would include natural
c vegetation types such as grasslands or biofuel (switchgrass) crops
c
c currently three main types of rotations are used
c if iwheat eq:
c 2: maize/soybean rotation (alternating)
c 3: maize/soybean/spring wheat
c 4: soybean/winter wheat/maize
c note: by doing continuous winter wheat, land is fallow 
c only from harvest (june-july) through planting (Sept-Nov).
c
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c local variables
c
      real
     >  xfrac
c
      integer
     >  iyear,
     >  irestart,
     >  irstyear,
     >  iyrrot,
     >  i,
     >  idiv
c
c begin grid
c in this case, irotation also is the number of crops in a specified rotation
c
c assumes that natural vegetation existence arrays are set to 0.0
c in climate.f (existence) 
c
      if (irestart .eq. 1) then
          iyrrot = irstyear
      else 
          iyrrot = 1950  ! base year to start crop rotations
      endif
c
c look at the fraction remainer to determine which crop should
c be planted
c
      if (irotation .eq. 1) idiv = 3
      if (irotation .eq. 2) idiv = 2
      if (irotation .eq. 3) idiv = 3
      if (irotation .eq. 4) idiv = 3
c
      xfrac = mod ((iyear - iyrrot), idiv)  

      do 100 i = 1, npoi
c
c 2:
c two-crop rotation (standard soybean/corn)
c alternate between even / odd years
c
        if (irotation .eq. 2) then
          if (xfrac .eq. 0) then
            isoybean = 1
            imaize   = 0
            iwheat   = 0
            exist(i,13) = 1.0
            exist(i,14) = 0.0
            exist(i,15) = 0.0
c
          else
            isoybean = 0
            imaize   = 1
            iwheat   = 0
            exist(i,13) = 0.0
            exist(i,14) = 1.0
            exist(i,15) = 0.0
          endif
c 3:
c rotation with 3 crops (corn, soybean, spring wheat) 
c
        elseif (irotation .eq. 3) then
          if (xfrac .eq. 0) then 
            isoybean = 0
            imaize   = 1
            iwheat   = 0
            exist(i,13) = 0.0
            exist(i,14) = 1.0
            exist(i,15) = 0.0
c
          elseif (xfrac .eq. 1.0) then
            isoybean = 1
            imaize   = 0
            iwheat   = 0
            exist(i,13) = 1.0
            exist(i,14) = 0.0
            exist(i,15) = 0.0
c     
          else
            isoybean = 0
            imaize   = 0
            iwheat   = 1
            exist(i,13) = 0.0
            exist(i,14) = 0.0
            exist(i,15) = 1.0
          endif
c
c 4:
c 3 crop rotation with winter wheat and soybean planted in same year
c winter wheat harvested in year 2
c maize grown in year 3
c 
        elseif (irotation .eq. 4) then 
c
c soybean planted/harvested  
c winter wheat planted
c
          if (xfrac .eq. 0.0) then 
            isoybean = 1
            imaize   = 0
            iwheat   = 2
            exist(i,13) = 1.0
            exist(i,14) = 0.0
            exist(i,15) = 1.0
c
c winter wheat harvested in year 2
c
          elseif (xfrac .eq. 1.0) then
            isoybean = 0
            imaize   = 0
            iwheat   = 2
            exist(i,13) = 0.0
            exist(i,14) = 0.0
            exist(i,15) = 1.0
c     
c maize planted/harvested in year 3
c
          else
            isoybean = 0
            imaize   = 1
            iwheat   = 0
            exist(i,13) = 0.0
            exist(i,14) = 1.0
            exist(i,15) = 0.0
          endif
c
        endif
 100    continue
c
      return
      end
c
c ----------------------------------------------------------------------------
      subroutine planting(irestart, irstyear, iyear0,iyear,imonth, iday, jday,ffact)
c ----------------------------------------------------------------------------
c
c subroutine to determine planting dates for crops based on observations
c and/or climatic constraints
c
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsno.h'
      include 'comnitr.h'
      include 'comcrop.h'
      include 'combcs.h'

c
c local variables
c
      real   
csant     >  ptemp(npft),
csant     >  pmmin(npft),
csant     >  pdmin(npft),
     >  pmmax(npft),
     >  pdmax(npft),
csant     >  pmintemp(npft),
     >  sumhy(npoi,npft),
     >  sumdp(npoi,npft),
     >  yc,
     >  ffact
c
      integer
     >  imonth,
     >  iday,
     >  jday,
     >  iyear,
     >  iyear0,
     >  irestart,
     >  irstyear,
     >  i, j, k, n,
     >  npts,  
     >  iy
c
      character*100   fertdata,
     >                header
c
      fertdata = 'hist.fert.1945.1996'   ! data file that contains historical
c                                        ! changes in fertilizer usage across upper Midwest US  
c
c read in historical fertilizer data one time into array
c file has quantities in kg/ha, so these are changed to
c kg/m2 
c read in restart scenario also **
c **
c
      if ((iyear .eq. 1945 .or. iyear .eq. irstyear)
     >     .and. imonth .eq. 1 .and. iday .eq. 1) then
        npts = 55
        open(15, file = fertdata) 
        read(15, *) header
        do n = 1, npts 
          read(15, *, end=20) iy, cfertmaize(iy),
     >                        cfertsoy(iy)

          cfertsgc(iy)=cfertmaize(iy)

          cfertmaize(iy) = cfertmaize(iy) * 1e-04
          cfertsgc(iy)   = cfertsgc(iy) * 1e-04
          cfertsoy(iy)   = cfertsoy(iy)   * 1e-04
c	print*,cfertmaize(iy), cfertsgc(iy)
c	pause
c 
c temporarily assign wheat fertilizer usage to be 3 times that of
c soybeans
          cfertwheat(iy) = cfertsoy(iy) * 3.0
        enddo
 20     continue 
      close(15)
      endif

csant- This numbers are now at params.crp
c
c minimum temperature required for crop planting and vegetative growth
c from EPIC model parameterizations
c
c the planting range in the US for maize is typically from April 10 - May 10
c the most active planting date in  US for soybean is typically May 15 - June 20 
c spring wheat planting dates are typically early April through mid-May - in line with maize 
c winter wheat is from Sept. 1 through early Nov., and is typically planted within 10-14
c days of the first likely frost event.
csant- have to make                   if (j .eq. 15) whtdop(i,iyear)   = jdaythese numbers extern
csant      ptemp(13)   = 284.16          ! soybean minimum 10 day average temperature for planting (K)
csant      ptemp(14)   = 283.16          ! maize minimum   10 day average temperature for planting (K)
csant      ptemp(16)   = 283.16          ! sugarcane minimum   10 day average temperature for planting (K)
csant      pmintemp(13)= 279.16          ! soybean minimum 10 day average min temp for planting (K) 
c     pmintemp(14)= 277.16          ! maize minimum 10 day average min temp for planting (K) 
csant      pmintemp(14)= 279.16          ! maize minimum 10 day average min temp for planting (K) 
csant      pmintemp(16)= 279.16          ! sugarcane minimum 10 day average min temp for planting (K) 
csant      pmmin(13)   = 5               ! soybean earliest month to plant (month)
csant      pmmin(14)   = 4               ! maize earliest month to plant (month)
csant      pmmin(16)   = 4               ! sugarcane earliest month to plant (month)
csant      pdmin(13)   = 1               ! soybean earliest day in earliest month to plant (day)
csant      pdmin(14)   = 1               ! maize earliest day in earliest month to plant (day)
csant      pdmin(16)   = 1               ! sugarcane earliest day in earliest month to plant (day)
c
c spring wheat planting date requirements
c note: iwheat = 1 (spring), iwheat = 2 (winter) 
c
csant      if (iwheat .eq. 1) then
csant        ptemp(15)   = 280.16        ! spring wheat minimum 10 day average temperature for planting (K) 
csant        pmintemp(15)= 272.16        ! spring wheat minimum 10 day average min temp threshold for planting (K)
csant        pmmin(15)   = 4             ! spring wheat earliest month to plant (month)
csant        pdmin(15)   = 1             ! spring wheat earliest day in earliest month to plant (day)
c
c typically winter wheat is planted when average minimum temperature gets to about 40 F
c and is planted no later than November 
c
csant      else if (iwheat .eq. 2) then  ! winter wheat
csant        pmintemp(15) = 278.16       ! average 5-day minimum temperature needed to plant winter wheat (~ 40 F) 
csant        pmmin(15)    = 9            ! earliest month in which winter wheat is planted (month)
csant        pdmin(15)    = 1            ! earliest day (of earliest month) in which winter wheat is planted (day) 
csant        pmmax(15)    = 11           ! latest possible month to plant winter wheat (month)
csant        pdmax(15)    = 30           ! latest day in latest month to plant winter wheat (day) 
csant      endif
c

csant-- externally ,ptemp(16),pcm(16),pcd(16),cgrain(16),fnopt(16)


        
      do 100 i = 1, npoi

csant- write half second year prec and temperature to see possible relationship with produc. 
	if(jday.eq.1) then
	cropout(i,7,31) =  td(i)
	cropout(i,8,31) =  precip(i)
	cropout(i,9,31) =  (tmax(i))
	elseif(jday.gt.1.and.jday.lt.181) then
	cropout(i,7,31) = cropout(i,7,31) + td(i)
	cropout(i,8,31) = cropout(i,8,31) + precip(i)
	cropout(i,9,31) = cropout(i,9,31) + (tmax(i))
c	if(i.eq.1)print*,'==',iyear,jday,td(i)-273.16,precip(i)
	elseif(jday.eq.181) then
	cropout(i,7,31) = (cropout(i,7,31)+td(i)          ) /181.
	cropout(i,8,31) = (cropout(i,8,31)+precip(i)      ) /181.
	cropout(i,9,31) = (cropout(i,9,31)+(tmax(i))      ) /181.
c	if(i.eq.1)print*,'==',cropout(i,1,31)-273.16,cropout(i,2,31)
	endif
c                                     teste
c	if(jday.eq.1) then
c	cropout(i,7,31) =  tmax(i) !  td(i)
c	cropout(i,8,31) =  tmin(i)  ! precip(i)
c	cropout(i,9,31) =  (tmax(i)-tmin(i))
c	elseif(jday.gt.1.and.jday.lt.365) then
c	cropout(i,7,31) = cropout(i,7,31) +   tmax(i)  ! td(i)
c	cropout(i,8,31) = cropout(i,8,31) +   tmin(i)  ! precip(i)
c	cropout(i,9,31) = cropout(i,9,31) + (tmax(i)-tmin(i))
c	if(i.eq.1)print*,iyear,jday,td(i)-273.16,precip(i)
c	elseif(jday.eq.365) then
c	cropout(i,7,31) = (cropout(i,7,31)+  tmax(i)        ) /365.  !td(i) ) /365.
c	cropout(i,8,31) = (cropout(i,8,31)+ tmin(i)	) /365.	      !precip(i)      ) /365.
c	cropout(i,9,31) = (cropout(i,9,31)+(tmax(i)-tmin(i)) ) /365.
c	if(i.eq.1)print*,iyear,jday,cropout(i,7,31)-273.16,cropout(i,8,31)
c	endif


csant- write anual prec (half first to half second years)and temperature to see possible relationship with produc. 
	if(jday.eq.182) then
	cropout(i,10,31) =  td(i)
	cropout(i,11,31) =  precip(i)
	cropout(i,12,31) =  (tmax(i))
	elseif(jday.ne.181.and.jday.ne.182.and.jday.ne.366) then
	cropout(i,10,31) = cropout(i,10,31) + td(i)
	cropout(i,11,31) = cropout(i,11,31) + precip(i)
	cropout(i,12,31) = cropout(i,12,31) + (tmax(i))
c	if(i.eq.1)print*,'=ANO,ANO,ANO,ANO=',iyear,jday
	elseif(jday.eq.181) then
	cropout(i,4,31) = (cropout(i,10,31)+td(i) ) /365.
	cropout(i,5,31) = (cropout(i,11,31)+precip(i)) / 365.
	cropout(i,6,31) = (cropout(i,12,31)+tmax(i)) / 365.
	cropout(i,10,31) =  0.0
	cropout(i,11,31) =  0.0
	cropout(i,12,31) =  0.0
c	if(i.eq.1)print*,'=ANO,ANO,ANO,ANO=',cropout(i,4,31)-273.16,cropout(i,5,31)
	endif
csant- write half first year (must displace down output in this case) prec and temperature to see possible relationship with produc. 
	if(jday.eq.182) then
	cropout(i,13,31) =  td(i)
	cropout(i,14,31) =  precip(i)
	cropout(i,15,31) =  (tmax(i))
	elseif(jday.gt.182.and.jday.lt.365) then
	cropout(i,13,31) = cropout(i,13,31) + td(i)
	cropout(i,14,31) = cropout(i,14,31) + precip(i)
	cropout(i,15,31) = cropout(i,15,31) + (tmax(i))
c	if(i.eq.1)print*,'==',iyear,jday,td(i)-273.16,precip(i)
	elseif(jday.eq.365) then
	cropout(i,13,31) = (cropout(i,13,31)+td(i)          ) /184.
	cropout(i,14,31) = (cropout(i,14,31)+precip(i)      ) /184.
	cropout(i,15,31) = (cropout(i,15,31)+ (tmax(i)) ) /184.
	elseif(jday.eq.1) then
	cropout(i,1,31) = cropout(i,13,31) 
	cropout(i,2,31) = cropout(i,14,31)
	cropout(i,3,31) = cropout(i,15,31)
	cropout(i,13,31) =  0.0
	cropout(i,14,31) =  0.0
	cropout(i,15,31) =  0.0
c	if(i.eq.1)print*,'==',cropout(i,1,31)-273.16,cropout(i,2,31)
	endif




       do 50 j = scpft, ecpft       ! crop plant functional types only
c
c in order to only allow a crop to be planted once each year
c initialize cropplant = 0., but hold it = 1 through the end of
c the year
c
c initialize other variables that are calculated for crops
c on an annual basis in cropresidue subroutine
c 
csant- Put a if to make distinct the kind of cultures and regions (ill be inplementaded transforming the pdim and pmmin in 2d vars)
          if  ((j.eq.13 .or. j.eq.14. or. j.eq.16) .or.
     >         (j .eq. 15 .and. iwheat .eq. 1))  then 
c

csant- find the minimum day of planting relative to the planting calendar.
csant- .and.cdays(i).lt.367 ->  planting only after planting year start. 
	
	if (iday.eq.pdmin(i,j).and.imonth.eq.pmmin(i,j).and.croplive(i,j).ne.1.0
     >  .and. exist(i,j).eq.1 .and. ncyears(i).ge.1) pstart(i,j)=cdays(i)
	

        if(pstart(i,j).eq.cdays(i).and.exist(i,j).eq.1.and.croplive(i,j).ne.1.0)
     > print*,'start planting for ',i,j,iyear,jday,cdays(i),'the min date is ',pdmin(i,j),pmmin(i,j)
	
	  if(iyear.gt.iyear0.and.j.lt.16.and.
     >  pstart(i,j).ne.999.and.(pstart(i,j)+mxmat(j)).ge.365) then
	print*,j,pstart(i,j),mxmat(j),(pstart(i,j)+mxmat(j))
     	print *, 'WARNING: annual crop should not exceed end of planting year without harvest'
	print *, 'pmmin/pdmin + mxmat should be < 365 days or before than  pcm/pcd'
	print *, 'fixed it at params.crp'
	            endif
	
c 
       if (iday.eq.pcd(i,j).and.imonth.eq.pcm(i,j))  then 
c
c make sure variables aren't changed at beginning of the year
c for a crop the is currently planted (e.g. winter wheat)
c if else if statement were removed in the next sequence, then
c winter wheat grown continuously would amount to a wheat/fallow
c rotation because wheat would only be planted every other year 
c 

            cropout(i,j,1)=pdate(i,j)

csant- I put after harvest, ok?  idop(i,j)      = 999
            pdate(i,j)     = 0.0

            if (croplive(i,j) .eq. 0)  then
              cropplant(i,j) = 0.0
		if(j.eq.16)   cropy(i) = 0


csant-              pdate(i,j)     = 0.0
csant-              idop(i,j)      = 999
c
c for continuous, annual winter wheat type crop
csant - to passing in the if above the wheat shoul not be eq to 1??
            else if (croplive(i,j) .eq. 1 .and.
     >               j .eq. 15            .and.
     >               iwheat .eq. 2)        then
              cropplant(i,j) = 0.0
            endif
c
        if(j.eq.16.and.i.eq.15) 
     > 	print*,'end of crop year',iyear,jday,croplive(i,j),cropplant(i,j),cropout(i,j,28)	  


	         
            harvdate(i,j)  = 999

            	          cropout(i,j,2)=hdate(i,j)
            hdate(i,j)     = 0.0
            	          cropout(i,j,3)=harvidx(i,j)
            harvidx(i,j)   = 0.0
            	          cropout(i,j,4)=croplaimx(i,j)
            croplaimx(i,j) = 0.0
            	          cropout(i,j,5)=grainn(i,j)
            grainn(i,j)    = 0.0
            	          cropout(i,j,6)=cropyld(i,j)
            cropyld(i,j)   = 0.0
            	          cropout(i,j,7)=dmyield(i,j)
            dmyield(i,j)   = 0.0
            	          cropout(i,j,8)=dmleaf(i,j) 
            dmleaf(i,j)    = 0.0
            	          cropout(i,j,9)=dmstem(i,j)
            dmstem(i,j)    = 0.0
            	          cropout(i,j,10)=dmroot(i,j)
            dmroot(i,j)    = 0.0
            	          cropout(i,j,11)=dmresidue(i,j)
            dmresidue(i,j) = 0.0
            	          cropout(i,j,12)=dmcrop(i,j) 
            dmcrop(i,j)    = 0.0
            	          cropout(i,j,13)=residuen(i,j)
            residuen(i,j)  = 0.0
            	          cropout(i,j,14)=nconcl(i,j)
            nconcl(i,j)    = 0.0
            	          cropout(i,j,15)=nconcs(i,j)
            nconcs(i,j)    = 0.0
            	          cropout(i,j,16)=nconcr(i,j)
            nconcr(i,j)    = 0.0
            	          cropout(i,j,17)=nconcg(i,j)
            nconcg(i,j)    = 0.0
            	          cropout(i,j,18)=cropn(i,j)
            cropn(i,j)     = 0.0
            	          cropout(i,j,19)=cropfixn(i,j)
            cropfixn(i,j)  = 0.0
            	          cropout(i,j,20)=cntops(i,j)
            cntops(i,j)    = 40.0
            	          cropout(i,j,21)=cnroot(i,j)
            cnroot(i,j)    = 60.0
            	          cropout(i,j,22)=fertinput(i,j)

            fertinput(i,j) = 0.0
			
csant-			cropout(i,j,23)=gddmaturity(i,j)
			cropout(i,j,23)=cropout(i,j,50)
		cropout(i,j,50)=0


  			cropout(i,j,24)=crmclim(i,j)
  			cropout(i,j,25)=crmact(i,j)
  			cropout(i,j,26)=crmplant(i,j) 
			cropout(i,j,27)=cropout(i,j,52)
		cropout(i,j,52)=0

                        cropout(i,j,28)=idppout(i,j)
	    idppout(i,j)=0.0
			cropout(i,j,29)=cropy(i)
			cropout(i,j,30)=cropout(i,j,51)
		cropout(i,j,51)=0


	
          endif ! planting calendar - pcd(j) and pcm(j)
	
	endif   ! crop type - (j)


c
c********************** Start Planting Block ***********************
c
        if (exist(i,j) .eq. 1. .and. croplive(i,j) .ne. 1.0 .and.
     >      cropplant(i,j) .eq. 0.0) then 
c
c gdd needed for * chosen crop and a likely hybrid (for that region) *
c to reach full physiological maturity
c
c based on accumulated seasonal average growing degree days from April 1 - Sept 30 (inclusive)
c for corn and soybeans in the United States -
c decided upon by what the typical average growing season length is
c
c and the gdd needed to reach maturity in those regions
c
c first choice is used for spring wheat and/or soybeans and maize
c
c         idop(i,j)       = max(90, int(xinpdate(i,iyear-iyear0+1)))  ! has to be later or equal to April 1  
c
c         idop(i,j)       = int(xinavepdate(i))
c
csant - differentiate tropical and subtropical cultures and sugarcane (or maybe annual and peren in future) .
c*********************************************************************************
c******************** Start of Sub-Tropical Crops ********************************
c*********************************************************************************

          if  ((j.eq.13 .or. j.eq.14) .or.
     >         (j .eq. 15 .and. iwheat .eq. 1))  then 
c
             if (iyear .le. iyear0+1) then         
csant - WHY ask here if imonth and iday ge. the minimum date, because this is asked above
csant- that is cropplant.eq.0 only if this condition, of date, is attained. right??
               if (a10td(i)         .gt. ptemp(j)     .and.         ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

c                jday             .eq. idop(i,j)    .and.               
c     >          gdd8(i)          .ge. gddmin(j))    then         ! impose limit on growing season length needed
c
                  croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  idop(i,j)         = jday     ! has to be later or equal to April 1  
                  if (j .eq. 13) soydop(i,iyear)   = jday
                  if (j .eq. 14) corndop(i,iyear)  = jday
                  if (j .eq. 15) whtdop(i,iyear)   = jday
c
                  gddmaturity(i,13) = max(950., min (gdd10(i), hybgdd(j))) 
                  gddmaturity(i,14) = max(950., min (gdd8(i)  * 1.0, hybgdd(j))) 
                  gddmaturity(i,15) = max(950., min (gdd0c(i), hybgdd(j))) 
c
c                 gddmaturity(i,14) = hybgdd(14) 
c
               endif 
c
             elseif (iyear .ge. iyear0+2 .and. iyear .le. iyear0+5) then       ! after initial spinup for crop average planting dates
c
               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

                     croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     idop(i,j)         = jday     ! has to be later or equal to April 1  
                     if (j .eq. 13) soydop(i,iyear)   = jday
                     if (j .eq. 14) corndop(i,iyear)  = jday
                     if (j .eq. 15) whtdop(i,iyear)   = jday
c
c                     gddmaturity(i,13) = max(950., min (gddcorn(i,iyear-1) *0.80, hybgdd(j)))  ! assign hybrid based on last year 
                     gddmaturity(i,13) = min (gddsoy(i,iyear-1), hybgdd(j))  ! assign hybrid based on last year 
                     gddmaturity(i,14) = max(950., min (gddcorn(i,iyear-1) *1.0, hybgdd(j)))  ! assign hybrid based on last year 
                     gddmaturity(i,15) = max(950., min (gddcorn(i,iyear-1) *1.2,  hybgdd(j)))  ! assign hybrid based on last year 

c	print*,'Plantando ponto ',i,' cultura ',j,' gddsoy',gddsoy(i,iyear-1),'gddmat',gddmaturity(i,13)

c                 gddmaturity(i,14) = hybgdd(14) 
c
               endif
c
             else 

               sumdp(i,j) = 0
               sumhy(i,j) = 0
c
c insert here - do iy from iyear-5 to  iyear-1 --> previous 5 year mean of hybrids planted
c keep planting date flexible for that year's weather conditions 
c
               yc = 5.0
c              yc = 11.0
c              do iy = 1949, 1959          ! 11 year averaging spinup for crops
               do iy = iyear-5,iyear-1     ! hybrid based on previous 5 year average - farm management 
c
                 if     (j .eq. 13) then 
                   sumhy(i,j) = sumhy(i,j) + gddsoy(i,iy)   !gddcorn(i,iy) * 0.8
                   sumdp(i,j) = sumdp(i,j) + float(soydop(i,iy))
                 elseif (j .eq. 14) then 
                   sumhy(i,j) = sumhy(i,j) + gddcorn(i,iy) * 1.0
                   sumdp(i,j) = sumdp(i,j) + float(corndop(i,iy))
                 elseif (j .eq. 15) then
                   sumhy(i,j) = sumhy(i,j) + gddcorn(i,iy) * 1.2
                   sumdp(i,j) = sumdp(i,j) + float(whtdop(i,iy))
                 endif
               enddo
c
c               xinavehybrid(i) = sumhy/yc
c
                avehybrid(i,j)    = sumhy(i,j) / yc


               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

                     croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     idop(i,j)       = jday  
                     gddmaturity(i,j) = max(950.,min(avehybrid(i,j), hybgdd(j)))   ! using constant hybrid for location over entire period 

c	 print*,iyear,'<cultura>',j,' gddsoy',gddsoy(i,iyear-1),'gddmat',gddmaturity(i,13),avehybrid(i,j),sumhy(i,j), yc

c                    gddmaturity(i,14) = hybgdd(14) 
                endif


             endif   ! planting/year option

	endif	!end of crop option

        if(pstart(i,j).eq.cdays(i).and.exist(i,j).eq.1)
     >	print*,iyear,jday,'Plponto ',i,' Cul ',j,' gdd',gddcorn(i,iyear-1),'gddmat',gddmaturity(i,j)

c*********************************************************************************
c******************** end of Sub-Tropical Crops **********************************
c*********************************************************************************


c*********************************************************************************
c******************** Start of Tropical Crops ************************************
c*********************************************************************************
		if(j.gt.17) then    !here I will put the restrition to Tropical (like Temp of the coldest month > 10) 


          if  ((j.eq.13 .or. j.eq.14) .or.
     >         (j .eq. 15 .and. iwheat .eq. 1))  then 
c
             if (iyear .le. iyear0+1) then         
c
               if (a10td(i)         .gt. ptemp(j)     .and.         ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

c                jday             .eq. idop(i,j)    .and.               
c     >          gdd8(i)          .ge. gddmin(j))    then         ! impose limit on growing season length needed
c
                  croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  idop(i,j)         = jday     ! has to be later or equal to April 1  
                  if (j .eq. 13) soydop(i,iyear)   = jday
                  if (j .eq. 14) corndop(i,iyear)  = jday
                  if (j .eq. 15) whtdop(i,iyear)   = jday
c
                  gddmaturity(i,13) = max(950., min (gdd10(i), hybgdd(j))) 
                  gddmaturity(i,14) = max(950., min (gdd8(i)  * 0.90, hybgdd(j))) 
                  gddmaturity(i,15) = max(950., min (gdd0c(i), hybgdd(j))) 
c
c                 gddmaturity(i,14) = hybgdd(14) 
c
               endif 
c
             elseif (iyear .ge. iyear0+2 .and. iyear .le. iyear0+5) then       ! after initial spinup for crop average planting dates
c
               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

                     croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     idop(i,j)         = jday     ! has to be later or equal to April 1  
                     if (j .eq. 13) soydop(i,iyear)   = jday
                     if (j .eq. 14) corndop(i,iyear)  = jday
                     if (j .eq. 15) whtdop(i,iyear)   = jday
c
csant                gddmaturity(i,13) = max(950., min (gddcorn(i,iyear-1), hybgdd(j)))  ! assign hybrid based on last year 
                     gddmaturity(i,13) = max(950., min (gddsoy(i,iyear-1), hybgdd(j)))  ! assign hybrid based on last year 
                     gddmaturity(i,14) = max(950., min (gddcorn(i,iyear-1) *1.0, hybgdd(j)))  ! assign hybrid based on last year 
                     gddmaturity(i,15) = max(950., min (gddcorn(i,iyear-1) *1.2,  hybgdd(j)))  ! assign hybrid based on last year 

c
c                 gddmaturity(i,14) = hybgdd(14) 
c
               endif
c
             else 

               sumdp(i,j) = 0
               sumhy(i,j) = 0
c
c insert here - do iy from iyear-5 to  iyear-1 --> previous 5 year mean of hybrids planted
c keep planting date flexible for that year's weather conditions 
c
               yc = 5.0
c              yc = 11.0
c              do iy = 1949, 1959          ! 11 year averaging spinup for crops
               do iy = iyear-5,iyear-1     ! hybrid based on previous 5 year average - farm management 
c
                 if     (j .eq. 13) then 
                   sumhy(i,j) = sumhy(i,j) + gddsoy(i,iy)  !csant - gddcorn(i,iy) * 0.8
                   sumdp(i,j) = sumdp(i,j) + float(soydop(i,iy))
                 elseif (j .eq. 14) then 
                   sumhy(i,j) = sumhy(i,j) + gddcorn(i,iy) * 0.9
                   sumdp(i,j) = sumdp(i,j) + float(corndop(i,iy))
                 elseif (j .eq. 15) then
                   sumhy(i,j) = sumhy(i,j) + gddcorn(i,iy) * 1.2
                   sumdp(i,j) = sumdp(i,j) + float(whtdop(i,iy))
                 endif
               enddo
c
c               xinavehybrid(i) = sumhy/yc
c
                avehybrid(i,j)    = sumhy(i,j) / yc


               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     >             cdays(i)         .ge. pstart(i,j))    then     ! impose earliest planting date 

                     croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     idop(i,j)       = jday  
                     gddmaturity(i,j) = max(950.,min(avehybrid(i,j), hybgdd(j)))   ! using constant hybrid for location over entire period 

c                    gddmaturity(i,14) = hybgdd(14) 
                endif

             endif   ! planting/year option

	endif	!end of crop option

	endif  !end Tropical or Sub-Tropical option
c*********************************************************************************
c******************** end of Tropical Crops **************************************
c*********************************************************************************


c*********************************************************************************
c************************ Start of Sugarcane Crop ********************************
c*********************************************************************************
c		if (mod(iyear-iyear0,nratoon+2) .eq.0) then

		if (cropy(i).eq.0) then

          if  (j.eq.16)  then 
c
             if (iyear .le. iyear0+1) then         
c
               if (a10td(i)         .gt. ptemp(j)     .and.         ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     > cdays(i).ge.pstart(i,j).and.cdays(i).le.(pstart(i,j)+180) )then     ! impose earliest planting date 
csant- to avoid that a crop that had be killed by frost be planted again in the same year. 
c                jday             .eq. idop(i,j)    .and.               
c     >          gdd8(i)          .ge. gddmin(j))    then         ! impose limit on growing season length needed
c
                  croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                  idop(i,j)         = jday    
		  cropy(i)=1
               sgcdop(i,iyear)   = jday
c
               gddmaturity(i,16) = max(gdd12(i)*0.8, min (gdd12(i) , hybgdd(j)))

              gddmaturity(i,j) = gddmaturity(i,j)*(gddsgcp(i,1)/gddsgcp(i,2))

        print*,'Plant Sug.Cane in ',i,iyear,imonth,jday,hybgdd(j),gddmaturity(i,j),gddsgcp(i,1),gddsgcp(i,2)

               endif 


             elseif (iyear .ge. iyear0+2 .and. iyear .le. iyear0+5) then       ! after initial spinup for crop average planting dates
c
               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     > cdays(i).ge.pstart(i,j).and.cdays(i).le.(pstart(i,j)+180) )then     ! impose earliest planting date 

                     croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
                     idop(i,j)         = jday  
		     cropy(i)=1
                     sgcdop(i,iyear)  = jday
c
                     gddmaturity(i,16) = max(gdd12(i)*0.8, min (gddsgc(i,iyear-1), hybgdd(j)))  ! assign hybrid based on last year

  	 	   gddmaturity(i,j) = gddmaturity(i,j)*(gddsgcp(i,1)/gddsgcp(i,2))  
        print*,'Plant Sug.Cane in ',i,iyear,imonth,jday,hybgdd(j),gddmaturity(i,j),gddsgcp(i,1),gddsgcp(i,2)

               endif
c
             else 


               if (a10td(i)         .gt. ptemp(j)     .and.              ! 10-day average soil temperature
     >             a10tmin(i)       .gt. pmintemp(j)  .and. 
     > cdays(i).ge.pstart(i,j).and.cdays(i).le.(pstart(i,j)+180) )then     ! impose earliest planting date 
csant- cdays.lt.pstar+90 is necessary to not deslocate the crop cycle, and harvest in summer, for example. 
csant- and also to avoid planting at the end of the last crop year - like cdays = 307 in the start of the year
         
         croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
         cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
           cropy(i)=1
           idop(i,j)       = jday  



               sumdp(i,j) = 0
               sumhy(i,j) = 0
               yc = 5.0
               do iy = iyear-5,iyear-1     ! hybrid based on previous 5 year average - farm management 
                   sumhy(i,j) = sumhy(i,j) + gddsgc(i,iy)
         print*,'Sug.Cane 5y',i,iyear,imonth,jday,gddsgc(i,iy)
               enddo

                avehybrid(i,j)    = sumhy(i,j) / yc


           gddmaturity(i,j) = max(gdd12(i)*1.0,min(avehybrid(i,j), hybgdd(j)))    

        print*,'Plant Sug.Cane gdd',i,iyear,imonth,jday,gddmaturity(i,j),gdd12(i),avehybrid(i,j),hybgdd(j)

	   gddmaturity(i,j) = gddmaturity(i,j)*(gddsgcp(i,1)/gddsgcp(i,2))  


        print*,'Plant Sug.Cane in ',i,iyear,imonth,jday,hybgdd(j),gddmaturity(i,j),gddsgcp(i,1),gddsgcp(i,2)

                endif

          endif  ! (year check)
          endif   ! (end crop check) 


         endif  !(recycle crop check)
c*********************************************************************************
c************************ end of Sugarcane Crop **********************************
c*********************************************************************************

csant- should we cut off this  or it can be usefful in future
c
c              gddmaturity(i,13) = min (gdd10(i), hybgdd(j)) 
c              gddmaturity(i,14) = max(950., min (gdd8(i) * 0.85, hybgdd(j))) 
c              gddmaturity(i,15) = min (gdd0c(i), hybgdd(j))  ! for spring wheat  
c
c plant everywhere if not planted by middle of june  
c based on approximate planting dates designated by the USDA
c
c             else if (imonth .ge. 6 .and. iday .ge. 15) then
c                  croplive(i,j)   = 1.0        ! initialize freeze kill function to 1.0 - crops living 
c                  cropplant(i,j)  = 1.0        ! initialize freeze kill function to 1.0 - crops living 
c                  idop(i,j)       = jday
c                  gddmaturity(i,13) = min (gdd10(i), hybgdd(j)) 
c                  gddmaturity(i,14) = max(950., min (gdd8(i) * 0.85, hybgdd(j))) 
cc                 gddmaturity(i,14) = min (hybgdd(j) , hybgdd(j)) 
c                  gddmaturity(i,15) = min (gdd0c(i), hybgdd(j))  ! for spring wheat  
c
c winter wheat : use gdd0c as a limit to plant winter wheat
c
           if (j .eq. 15 .and. iwheat .eq. 2) then   ! plant winter wheat
c
c add check to only plant winter wheat after other crops (soybean, maize)
c have been harvested 
c
c *** remember order of planting is crucial - in terms of which crops you want
c to be grown in what order ***    
c
c in this case, corn or soybeans are assumed to be planted before
c wheat would be in any particular year that both pfts are allowed
c to grow in the same grid cell (e.g., double-cropping)  
c
            if (iyear .eq. iyear0) then
c
             if (a5tmin(i)        .le. pmintemp(j) .and.
     >           imonth           .ge. pmmin(i,j)    .and.
     >           iday             .ge. pdmin(i,j)    .and.
     >           (harvdate(i,13)  .ne. 999         .or.
     >            harvdate(i,14)  .ne. 999         .or.
     >            irotation       .eq. 0)          .and. 
     >            gdd0c(i)        .ge. gddmin(j))         then
c
                   croplive(i,j)     = 1.0     
                   cropplant(i,j)    = 1.0     
                   idop(i,j)         = jday
                   whtdop(i,iyear)   = jday
c                  gddmaturity(i,15) = hybgdd(j) 
                   gddmaturity(i,15) = max(950., min (gdd0c(i) * 0.90, hybgdd(j))) 
c
c plant winter wheat at latest possible date 
c and after all other crops were harvested for that year
c
             elseif (imonth  .ge. pmmax(j)     .and.
     >               iday    .ge. pdmax(j)     .and.
     >              (harvdate(i,13)  .ne. 999  .or.
     >               harvdate(i,14)  .ne. 999  .or. 
     >               irotation       .eq. 0)   .and. 
     >               gdd0c(i).ge. gddmin(j)) then  
c
                       croplive(i,j)     = 1.0     
                       cropplant(i,j)    = 1.0     
                       idop(i,j)         = jday
c                      gddmaturity(i,15) = hybgdd(j) 
                       gddmaturity(i,15) = max(950., min (gdd0c(i), hybgdd(j))) 
             endif
c
           elseif (iyear .gt. iyear0 .and. iyear .lt. iyear0+5) then       ! after initial spinup for crop average planting dates
c
             if (a5tmin(i)        .le. pmintemp(j) .and.
     >           imonth           .ge. pmmin(i,j)    .and.
     >           iday             .ge. pdmin(i,j)    .and.
     >           (harvdate(i,13)  .ne. 999         .or.
     >            harvdate(i,14)  .ne. 999         .or.
     >            irotation       .eq. 0)          .and. 
     >            gdd0c(i)        .ge. gddmin(j))         then
c
                   croplive(i,j)     = 1.0     
                   cropplant(i,j)    = 1.0     
                   idop(i,j)         = jday
                   whtdop(i,iyear)   = jday
                   gddmaturity(i,15) = max(950., min (gddcorn(i,iyear-1) * 1.20, hybgdd(j)))  ! assign hybrid based on last year 
c
c plant winter wheat at latest possible date 
c and after all other crops were harvested for that year
c
             elseif (imonth  .ge. pmmax(j)     .and.
     >               iday    .ge. pdmax(j)     .and.
     >              (harvdate(i,13)  .ne. 999  .or.
     >               harvdate(i,14)  .ne. 999  .or. 
     >               irotation       .eq. 0)   .and. 
     >               gdd0c(i).ge. gddmin(j)) then  
c
                       croplive(i,j)     = 1.0     
                       cropplant(i,j)    = 1.0     
                       idop(i,j)         = jday
                       gddmaturity(i,15) = max(950., min (gddcorn(i,iyear-1) * 1.20, hybgdd(j)))  ! assign hybrid based on last year 
             endif
c
           else
               sumdp(i,j) = 0
               sumhy(i,j) = 0
c
c insert here - do iy from iyear-5 to  iyear-1 --> previous 5 year mean of hybrids planted
c keep planting date flexible for that year's weather conditions 
c
               yc = 5.0
c              yc = 11.0
c              do iy = 1949, 1959          ! 11 year averaging spinup for crops
               do iy = iyear-5,iyear-1     ! hybrid based on previous 5 year average - farm management 
                   sumhy(i,j) = sumhy(i,j) + gddcorn(i,iy) * 1.2
                   sumdp(i,j) = sumdp(i,j) + float(whtdop(i,iy))
               enddo
c
                avehybrid(i,j)    = sumhy(i,j) / yc
c
c
                if (a5tmin(i)        .le. pmintemp(j) .and.
     >           imonth           .ge. pmmin(i,j)    .and.
     >           iday             .ge. pdmin(i,j)    .and.
     >           (harvdate(i,13)  .ne. 999         .or.
     >            harvdate(i,14)  .ne. 999         .or.
     >            irotation       .eq. 0)          .and. 
     >            gdd0c(i)        .ge. gddmin(j))         then
c
                   croplive(i,j)     = 1.0     
                   cropplant(i,j)    = 1.0     
                   idop(i,j)         = jday
                   gddmaturity(i,15) = max(950., min (avehybrid(i,j), hybgdd(j)))  ! assign hybrid based on last year 
c
c plant winter wheat at latest possible date 
c and after all other crops were harvested for that year
c
             elseif (imonth  .ge. pmmax(j)     .and.
     >               iday    .ge. pdmax(j)     .and.
     >              (harvdate(i,13)  .ne. 999  .or.
     >               harvdate(i,14)  .ne. 999  .or. 
     >               irotation       .eq. 0)   .and. 
     >               gdd0c(i).ge. gddmin(j)) then  
c
                       croplive(i,j)     = 1.0     
                       cropplant(i,j)    = 1.0     
                       idop(i,j)         = jday
                       gddmaturity(i,15) = max(950., min (avehybrid(i,j), hybgdd(j)))  ! assign hybrid based on last year 
             endif
           endif  ! (year check)
          endif
c
        endif  ! crop existence

c
c add fertilizer nitrogen input for each crop planted (kg m-2)
c on the planting date
c either input here, or read from a gridded dataset
c is treated a single, broadcast pulse at planting to the top soil layer
c this fertilizer is assumed to be ammonium nitrate, in a 50/50 ratio
c of NH4/NO3
c
c define amount of fertilizer added when crop is planted
c use historical changes for years between 1945-1996 (Alexander et al.,) 
c only add fertilizer on day of planting - use croplive funtion to
c make sure that in successive years, idop from previous year doesn't
c get applied here.
c
c also, don't cycle through all crop types - only the one that
c is planted this year...otherwise it will zero out the fertilizer
c values for pfts 13, 14 if going through all pfts through 15 
c
c
           if (jday .eq. idop(i,j) .and. croplive(i,j) .eq. 1.0
     >         .and. exist(i,j) .eq. 1.) then 
c
              if (iyear .lt. 1950) then
                 fertnitro(i,13) = 0.0002       ! soybeans - kg_n m-2 y-1
                 fertnitro(i,14) = 0.0009       ! maize    - kg_n m-2 y-1
                 fertnitro(i,15) = 0.0002       ! wheat    - kg_n m-2 y-1
                 fertnitro(i,16) = 0.0009       ! sugarcane - kg_n m-2 y-1
c
              else if (iyear .gt. 2000) then
c
                 fertnitro(i,13) = fertsoy(i,51)   * ffact * 1e-04 ! soybeans - kg_n m-2 y-1
                 fertnitro(i,14) = fertmaize(i,51) * ffact * 1e-04 ! maize    - kg_n m-2 y-1
                 fertnitro(i,15) = fertwheat(i,51) * ffact * 1e-04 ! wheat    - kg_n m-2 y-1
                 fertnitro(i,16) = fertsgc(i,51)   * ffact * 1e-04 ! sugarcane - kg_n m-2 y-1
c
              else
                 fertnitro(i,13) = fertsoy(i,iyear+1-1950)   * 1e-04       ! soybeans - kg_n m-2 y-1
                 fertnitro(i,14) = fertmaize(i,iyear+1-1950) * 1e-04       ! maize    - kg_n m-2 y-1
                 fertnitro(i,15) = fertwheat(i,iyear+1-1950) * 1e-04       ! wheat    - kg_n m-2 y-1
                 fertnitro(i,16) = fertsgc(i,iyear+1-1950) * 1e-04       ! sugarcane - kg_n m-2 y-1
c 
               endif
c
c constant 2000 level
c
c             fertnitro(i,13) = fertsoy(i,51)   * 1e-04       ! soybeans - kg_n m-2 y-1
c             fertnitro(i,13) = 0.0                           ! soybeans - kg_n m-2 y-1
c             fertnitro(i,14) = fertmaize(i,51) * 1e-04       ! maize    - kg_n m-2 y-1
c             fertnitro(i,15) = fertwheat(i,51) * 1e-04       ! wheat    - kg_n m-2 y-1
c
c take care of new fertilizer data that doesn't exist for Canada
c use the old values from the calculated fertilizer use over the US (historical)
c
              if (fertnitro(i,13) .gt. 2e+03 .and. iyear .le. 1996) then
                   fertnitro(i,13) = cfertsoy(iyear) 
              else if (fertnitro(i,13) .gt. 2e+03 .and. iyear .le. 2000) then
                   fertnitro(i,13) = cfertsoy(1996) 
              else if (fertnitro(i,13) .gt. 2e+03 .and. iyear .gt. 2000) then
                   fertnitro(i,13) = cfertsoy(1996) * ffact 
              endif
c
              if (fertnitro(i,14) .gt. 2e+03 .and. iyear .le. 1996) then
                   fertnitro(i,14) = cfertmaize(iyear) 
              else if (fertnitro(i,14) .gt. 2e+03 .and. iyear .le. 2000) then
                   fertnitro(i,14) = cfertmaize(1996) 
              else if (fertnitro(i,14) .gt. 2e+03 .and. iyear .gt. 2000) then
                   fertnitro(i,14) = cfertmaize(1996) * ffact 
              endif
c
              if (fertnitro(i,15) .gt. 2e+03 .and. iyear .le. 1996) then
                   fertnitro(i,15) = cfertwheat(iyear) 
              else if (fertnitro(i,15) .gt. 2e+03 .and. iyear .le. 2000) then
                   fertnitro(i,15) = cfertwheat(1996) 
              else if (fertnitro(i,15) .gt. 2e+03 .and. iyear .gt. 2000) then
                   fertnitro(i,15) = cfertwheat(1996) * ffact 
              endif
c
              if (fertnitro(i,16) .gt. 2e+03 .and. iyear .le. 1996) then
                   fertnitro(i,16) = cfertsgc(iyear) 
              else if (fertnitro(i,16) .gt. 2e+03 .and. iyear .le. 2000) then
                   fertnitro(i,16) = cfertsgc(1996) 
              else if (fertnitro(i,16) .gt. 2e+03 .and. iyear .gt. 2000) then
                   fertnitro(i,16) = cfertsgc(1996) * ffact 
              endif
c
c
c
c constant mid 1990s levels for grid cells outside of US
c
c               if (fertnitro(i,13) .gt. 1e+03) fertnitro(i,13) = cfertsoy(1996) 
c               if (fertnitro(i,14) .gt. 1e+03) fertnitro(i,14) = cfertmaize(1996) 
c               if (fertnitro(i,15) .gt. 1e+03) fertnitro(i,15) = cfertwheat(1996) 
c
c
c or constant fertilizer application each year
c
c                fertnitro(i,13) = 0.0025      ! soybeans - kg_n m-2 y-1
c                fertnitro(i,14) = 0.0180      ! maize    - kg_n m-2 y-1
c                fertnitro(i,15) = 0.0080      ! wheat    - kg_n m-2 y-1
c
c assign annual fertilizer input in kg/ha
c
                fertinput(i,j)  = fertnitro(i,j) * 1.e+04
c
           else if (exist(i,j) .eq. 1.0) then 
c
             fertnitro(i,j) = 0.0       
c
           endif   ! planting date

csant- as we dont have a historical data set, assuming constant.
	if(idpp(i,j).eq.1.and.j.eq.16) then
	 fertnitro(i,16)= 0.025   !kg_n m-2 y-1 or 100 Kg/ha
csant	print*,'     fertnitro   ',i,j,jday,fertnitro(i,16) 
	elseif (idpp(i,j).ne.1.and.j.eq.16) then
	fertnitro(i,16)= 0.0
	endif
c
 50     continue
c
c end of loop
c
 100    continue
c
c return to main program
c 
      return
      end
c
c ---------------------------------------------------------------------------------
      subroutine vernalization(iyear,imonth, iday, jday)
c ---------------------------------------------------------------------------------
c
c * * * only call subroutine for winter wheat * * *
c
c subroutine calculates vernalization and photoperiod effects on gdd accumulation
c in winter wheat varieties. Thermal time accumulation is reduced in 1st period until
c plant is fully vernalized. During this time of emergence to spikelet formation,  
c photoperiod can also have a drastic effect on plant development.
c
c
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsno.h'
      include 'comnitr.h'
      include 'comcrop.h'
c
c local variables
c
        real  p1d, p1v,
     >        tcrown,
     >        vd,vd1, vd2,
     >        tkil, tbase,
     >        hti 
c
        integer  i,
     >           iyear,
     >           imonth,
     >           iday,
     >           jday
 
c photoperiod factor calculation      
c genetic constant - can be modified
c
        p1d = 0.004  ! average for genotypes from Ritchey, 1991.
c                    ! Modeling plant and soil systems: Wheat phasic development
        p1v = 0.003  ! average for genotypes from Ritchey, 1991.  
c
        do 100 i = 1, npoi
c
c only go through routine if winter wheat has been planted, is living,
c and the factor is not already 1.0
c 
         if (croplive(i,15) .eq. 1.0 .and. vf(i) .ne. 1.0) then 

c
c from CERES-wheat J.T. Ritchey
c
c       jj   = latindex(i)
c       xlat = latscale(jj) 
c       s1 = sin(xlat * 0.01745)
c       c1 = cos(xlat * 0.01745)
c       dec  = 0.4093 * sin(0.0172 * (jday - 82.2)) 
c       dlv  = ((-s1 * sin(dec) - 0.1047)/(c1 * cos(dec)))
c       if (dlv .lt. -0.87) dlv = -0.87
c       hrlt = 7.639 * acos(dlv)  
c       df   = 1 - p1d * (20.0 - hrlt)**2
c
c daylength calculation in IBIS is in minutes - convert to hours
c
          df(i)   = 1 - p1d * (20.0 - daylength(i)/60.0)**2
c
c for all equations - temperatures must be in degrees (C)
c calculate temperature of crown of crop (e.g., 3 cm soil temperature)
c snow depth in centimeters
c
          if (td(i) .lt. 273.16) then
             tcrown = 2.0 + (td(i) - 273.16) * (0.4 + 0.0018 *
     >                (min(adsnod(i)*100., 15.0) - 15.0)**2)
          else
             tcrown = td(i) - 273.16 
          endif
c 
c vernalization factor calculation
c if vf(i) = 1.  then plant is fully vernalized - and thermal time
c accumulation in phase 1 will be unaffected 
c cumulative vd will need to be reset to 0 at planting of crop
c
          if (vf(i) .ne. 1.0) vd = 0.
          if (vf(i) .ne. 1.0 .and. tmax(i) .gt. 273.16) then
             if (tmin(i) .le. 288.16) then
               vd1      = 1.4 - 0.0778 * (tcrown)
               vd2      = 0.5 + 13.44/((tmax(i) - tmin(i) + 3.0)**2) * tcrown
               vd       = max(0.0, min(1., vd1, vd2))
               cumvd(i) = cumvd(i) + vd
             endif
c
             if (cumvd(i) .lt. 10 .and. tmax(i) .gt. 303.16) then
               cumvd(i) = cumvd(i) - 0.5 * (tmax(i) - 303.16)   
             endif 
             cumvd(i) = max(0.0, cumvd(i))    ! must be greater than 0 
c
             vf(i) = 1.0 - p1v * (50.0 - cumvd(i))
             vf(i) = max(0.0, min(vf(i), 1.0))       ! must be between 0 - 1
          endif
c             
c calculate cold hardening of plant
c determines for winter wheat varieties whether the plant has completed
c a period of cold hardening to protect it from freezing temperatures.  If  it has
c not, then exposure could result in death or killing of plants.
c
c there are two distinct phases of hardening 
c
       tbase = 0.0
c
         if (tmin(i) .le. 270.16 .or. hdidx(i) .ne. 0) then
           hti =  1.0
           if (hdidx(i) .ge. hti) then   ! done with phase 1
             hdidx(i) = hdidx(i) + 0.083
             if (hdidx(i) .gt. hti * 2.0) hdidx(i) = hti * 2.0  
           endif
c
           if (tmax(i) .ge. tbase + 283.16) then
             hdidx(i) = hdidx(i) - 0.02 * (tmax(i) - 283.16)
             if (hdidx(i) .gt. hti) hdidx(i) = hdidx(i) - 0.02 * (tmax(i) - 283.16) 
             hdidx(i) = max(0.0, hdidx(i))
           endif
c
         elseif (tcrown .ge. tbase - 1.0) then
           if (tcrown .le. tbase + 8.0) then
             hdidx(i) = hdidx(i) + 0.1 - ((tcrown - tbase + 3.5)**2 / 506.0)
             if (hdidx(i) .ge. hti .and. tcrown .le. tbase + 0.0) then
               hdidx(i) = hdidx(i) + 0.083
               if (hdidx(i) .gt. hti * 2.0) hdidx(i) = hti * 2.0
             endif
           endif 
c
           if (tmax(i) .ge. tbase + 283.16) then
             hdidx(i) = hdidx(i) - 0.02 * (tmax(i) - 283.16)
             if (hdidx(i) .gt. hti) hdidx(i) = hdidx(i) - 0.02 * (tmax(i) - 283.16) 
             hdidx(i) = max(0.0, hdidx(i))
           endif
         endif
c      
c calculate what the wheat killing temperature 
c there is a linear inverse relationship between 
c hardening of the plant and the killing temperature or
c threshold that the plant can withstand 
c when plant is fully-hardened (hdidx = 2), the killing threshold is -18 C
c
c will have to develop some type of relationship that reduces LAI and
c biomass pools in response to cold damaged crop 
c
         if (tmin(i) .le. 267.16) then
           tkil = (tbase - 6.0) - 6.0 * hdidx(i)         
           if (tkil .ge. tcrown) then
             if ((0.95 - 0.02 * (tcrown - tkil)**2) .ge. 0.02) then
               write (*,*)  'crop damaged by cold temperatures'
             else
               croplive(i,15) = 0.0   ! kill winter wheat
               write (*,*)  '95% of crop killed by cold temperatures'
             endif
           endif
         endif
c
        else if (croplive(i,13) .eq. 1 .or. croplive(i,14) .eq. 1) then
          vf(i) = 1.0
c
        endif  ! croplive, vf check
c
 100   continue 
c
       return
       end
c
c ---------------------------------------------------------------------
      subroutine phenocrop(iyear, iyear0,imonth, iday, jday)
c ---------------------------------------------------------------------
c 
c subroutine which determine the phenological development of each crop
c type including allocation changes of photosynthate to 
c leaf, stem, root, and reproductive organs (pod, seed, grain) 
c leaf area index increase and decline
c subsequent carbon in biomass pools (leaf,stem,root,wood,reproductive)  
c
c Conversion factors for carbon to dry matter are from Penning DeVries
c et al., 1983 and Penning DeVries et al., 1989
c 
c values are fraction of carbon in dry matter
c
c leaves: 0.459
c stem  : 0.494
c root  : 0.467
c
c maize cob and grain : 0.491
c soybean pod and seed: 0.527 
c
c all of the phenology changes are based on the total number of gdd needed
c to change to the next phase - based on fractions of the total gdd typical
c for  that region based on the April 1 - Sept 30 window of development  
c
c Phase 1: Planting to leaf emergence
c Phase 2: Leaf emergence to beginning of grain fill (general LAI accumulation)
c Phase 3: Grain fill to physiological maturity and subsequently harvest (LAI decline)  
c
      include 'implicit.h'
c
c common blocks
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsno.h'
      include 'comnitr.h'
      include 'comcrop.h'
      include 'compft.h'
c
c
      real huileaf(npft),         ! heat unit index needed to attain leaf emergence after planting 
     >     huigrain(npft)         ! heat unit index needed to reach vegetative maturity 
*     >     laicons(npft),        ! constant used in lai senescence equation
*     >     allconsl(npft),       ! constant used in dynamic allocation equations for leaves (decline in allocation)
*     >     allconss(npft),       ! constant used in dynamic allocation equations for stem (decline in allocation) 
*     >     laimx(npft),          ! maximum lai designated for crops from EPIC and EPICphase  models
*     >     tkill(npft),          ! minimum daily temperature threshold used to kill crops
*     >     mxmat(npft),          ! maximum length of growing season to harvest/physiological maturity 
*     >     mxdgfi(npft),         ! maximum number of days needed to reach grain fill stage
*     >     mxgddgf(npft)         ! maximum gdd past initiation of grain fill to reach phys maturity/harvest
c
      real 
     >     laidecl(npoi,npft)     ! decline in leaf area for crop
*     >     tauroot(npft),        ! time constant for root turnover 
*     >     arooti(npft),         ! initial allocation to crop fine roots 
*     >     arootf(npft),         ! end of growing season final allocation to crop fine roots
*     >     aleaff(npft),         ! end of growing season final allocation to crop leaf area
*     >     astemf(npft),         ! end of growing season final allocation to crop stem biomass
*     >     fleafi(npft),         ! initial fraction of aboveground carbon allocation (stem/leaf) allocated to leaf 
*     >     fleaf(npft),          ! fraction of aboveground carbon allocation (stem/leaf) allocated to leaf 
*     >     declfact(npft)        ! factor helping to control LAI decline at end of season
c
      real pc,leaftemp,
     >     ddays,
     >     ddfac,
     >     tthreshold,
*    >     xminlai, 
     >     dtt,
     >     explf,
     >     tix,
     >     xn,
     >     plag,
     >     abiol,
     >     crmcorn,
     >     crmsgc,sipf3,sipf4,sipf6,
     >     aspecla,ccf5
c
      integer
     >     iyear,
     >     iyear0,
     >     imonth, 
     >     iday,
     >     jday, 
     >     nplants,
     >     i, j, k, l, n  
c
c phenology for additional leaf drop - if drought related or temperature related at
c end of growing season     
c
       ddays      = 7.0
       ddfac      = 1.0 / ddays
       tthreshold = 273.16
c
c number of corn plants per square meter
c this is only important if using the leaf expansion equations
c of Ritchie, that is temperature dependent.  Our standard procedure
c here however is to use the allocation of C to leaf (aleaf) and
c specific leaf area (specla) to accumulate LAI during the season
c
       nplants    = 7 
c
      call vernalization(iyear,imonth,iday,jday)
c
c begin global grid
c
      do 100 i = 1, npoi
c
       if (icropsum(i) .gt. 0.0) then
c
        aplantn(i) = 0.0
c
c only want to look at top 1.5 meters for plant available inorganic n
c
        do 115  k = 1, nsoilay 
c
          aplantn(i) = aplantn(i) + smsoil(i,k) + smsoln(i,k)
c
 115    continue

c
c crop phenology (gdd thresholds) controlled by gdd needed for maturity (physiological) which
c is based on the average gdd accumulation and hybrids in United States from April 1 - Sept 30  
c
c Phase 1:
c ==========
c threshold for attaining leaf emergence (based on fraction of gdd(i) -- climatological average)
c Hayhoe and Dwyer, 1990, Can. J. Soil Sci 70:493-497
c Carlson and Gage, 1989, Agric. For. Met., 45: 313-324 
c J.T. Ritchie, 1991: Modeling Plant and Soil systems
c
           huileaf(13)  = lfemerg(13)  * gddmaturity(i,13) 
           huileaf(14)  = lfemerg(14)  * gddmaturity(i,14) 
           huileaf(15)  = lfemerg(15)  * gddmaturity(i,15)  ! typically between 3-7% in wheat 

csant- for a first guess, Ive put 0.12 for planting and 0.03 for ratoon (but need be adjusted)
csant- (based on lat 27, lon -81; and 45 days for planting and 15 days of delay for plt and ratoon, Modeling...pfd/fig.1)
	   if(cropy(i).eq.1) then
           huileaf(16)  = lfemerg(16)  * gddmaturity(i,16) 
	   else
	   huileaf(16)  = lfemerg(16)*(1./6.)* gddmaturity(i,16) !*(100./350.)* gddmaturity(i,16) 
	   endif
c
c Phase 2:
c ==========
c from leaf emergence to beginning of grain-fill period 
c this hypothetically occurs at the end of tassling, not the beginning 
c tassel initiation typically begins at 0.5-0.55 * gddmaturity 
c
c calculate linear relationship between huigrain fraction and relative
c maturity rating for maize
c
           crmcorn      = max(73., min((gddmaturity(i,14)+ 53.683)/13.882,135.))
           huigrain(14) = -0.002  * (crmcorn - 73.) + grnfill(14)
           huigrain(14) = min(max(huigrain(14),grnfill(14) - 0.1), grnfill(14)) 
           huigrain(14) = huigrain(14)   * gddmaturity(i,14)  ! from Cabelguenne et al. 1999
c
           crmsgc      = max(73., min((gddmaturity(i,16)+ 53.683)/13.882,135.))
           huigrain(16) = -0.002  * (crmsgc - 73.) + grnfill(16)
           huigrain(16) = min(max(huigrain(16),grnfill(16) - 0.1), grnfill(16)) 
           huigrain(16) = huigrain(16)   * gddmaturity(i,16)  ! from Cabelguenne et al. 1999
c
c 
           huigrain(13) = grnfill(13)    * gddmaturity(i,13)  ! from Cabelguenne et al. 1999 
c          huigrain(14) = grnfill(14)    * gddmaturity(i,14)  ! from Cabelguenne et al. 1999
                      

csant- why are these parameters reset here??
         if(iwheat.gt.0) then
             huigrain(15) = grnwht(iwheat) * gddmaturity(i,15) 
             fleafi(15)   = fleafiwht(iwheat)  ! wheat          
             mxgddgf(15)  = mgddgf(iwheat)  ! wheat
             mxmat(15)    = mxmatwht(iwheat)   ! wheat
         endif
c
c crop phenology
c daily routine  
c
c set fraction of vegetation that is green to 1.0
c used in radiation.f - T. Twine's grassland algorithm
c if modified, do on a daily basis in cropupdate subroutine
c
c allocation rules for crops based on maturity and linear decrease of amount allocated
c to roots over course of the growing season
c
         do 80 j = scpft, ecpft 
c

	cropout(i,j,35)=0	!aleaf(j)
	cropout(i,j,32)=0	!aroot(j)
	cropout(i,j,33)=0	!arepr(j)
	cropout(i,j,34)=0	!astem(j)


           if (croplive(i,j) .eq. 1.0) then
csant- not all, specially at the end of the season (future put some function here)
               grnfraccrop(i,j) = 1.0  
c
c calculate fraction allocated to leaf (from J. Norman allocation curve)
c bfact and fleafi are set in params.crp
c
               fleaf(j) = fleafi(j) * (exp(-bfact(j)) - exp(-bfact(j) *
     >                               gddplant(i,j)/huigrain(j))) / (exp(-bfact(j))-1) 
c
c calculate accumulated growing degree days since planting (gddplant) 
c determine if growing degree days calculated from top layer soil temperature
c are enough for leaf emergence to occur 
c
             hui(i,j)       = gddplant(i,j)        
csant- Ive changed leafout for instance, just to have more external control during the test phase.
	if(j.eq.16) then
		leafout(i,j)   = gddplant(i,j)
	else
                leafout(i,j)   = gddtsoi(i,j)  
	endif
c
             laidecl(i,j) = 0.0   
c
c calculate days past planting
c
c             if (iwheat .eq. 2 .and. croplive(i,j) .eq. 1) then
c               idpp(i,j)  = jday + (ndaypy - idop(i,j))s
c             else
c               idpp(i,j)  = jday - idop(i,j)
c             endif
c
                idpp(i,j) = idpp(i,j) + 1

        if (leafout(i,j).ge.huileaf(j)) idpe(i,j) = idpe(i,j) + 1
c
c crop phenology from leaf emergence to start of leaf decline   
c determine allocation coefficients to help calculate increase in lai  
c and fine root biomass
c
             if (leafout(i,j) .ge. huileaf(j) .and.
     >           hui(i,j) .lt. huigrain(j).and.j.ne.16) then  
c
c Phase 1 completed:
c ==================
c if hui is less than the number of gdd needed for filling of grain
c leaf emergence also has to have taken place for lai changes to occur and carbon
c assimilation 

c allocation rules for crops based on maturity and linear decrease of amount allocated
c to roots over course of the growing season
c
               awood(j) = 0.0
c           
c check to see if lai is at maximum allowable 
c

               if (peaklai(i,j) .eq. 1) then 
                 aleaf(j) = 0.0
                 arepr(j) = 0.0
c CJK 9-23-04    astem(j) = 0.0
c                aroot(j) = 1. - arepr(j) - aleaf(j) - astem(j)
c **********
c
                 aroot(j) = min(1.0, (arooti(j) - (arooti(j) - arootf(j))
     >                        * min(1.0,hui(i,j)/gddmaturity(i,j)))) 
                 aroot(j) = max(0.0, aroot(j))
                 astem(j) = 1.0 - aroot(j) - aleaf(j) - arepr(j)
c                 
               else
                 aroot(j) = min(1.0, (arooti(j) - (arooti(j) - arootf(j))
     >                        * min(1.0,hui(i,j)/gddmaturity(i,j)))) 
                 aroot(j) = max(0.0, aroot(j))
                 aleaf(j) = max(0.0,(1.0 - aroot(j)) * fleaf(j)) 
                 aleaf(j) = max(aleaff(j),aleaf(j)) 
                 astem(j) = 1.0 - aroot(j) - aleaf(j)
                 arepr(j) = 0.0

c
               endif
c
c calculate actual lai increase based on npp and allocation rules in ibis 
c only increment lai if it hasn't reached maximum allowable value 
c

               if (peaklai(i,j) .eq. 0) then
                 tlai(i,j) = plai(i,j) + (specla(j) * aleaf(j)
     >                        * max(0.0, adnpp(i,j)))

                 if (tlai(i,j) .ge. laimx(j)) then

csant!replaced by below                    aleaf(j) = min(1.0 - aroot(j), laimx(j) -
c     >                        plai(i,j)) / (specla(j) * adnpp(i,j)) 

          aleaf(j) = min(1.0 - aroot(j), (laimx(j)-plai(i,j)) / (specla(j)*adnpp(i,j))  )

csant                    aleaf(j) = max(0.0, aleaf(j))
                    aleaf(j) = min(1.0 - aroot(j), max(0.0, aleaf(j)) )

                    astem(j) = 1.0 - aroot(j) - aleaf(j) 

c
c CJK other possible source of over allocation
c above astem could be set to 0.0
c
                    peaklai(i,j) = 1
c


                 endif

c
c original phenology - apply to soybeans
c if maize - employ leaf expansion routine based on a modified
c ceres gdd approach
c
c                if     (j .eq. 13) then
c
                   plai(i,j)    = plai(i,j) + (specla(j) * aleaf(j)
     >                          * max(0.0, adnpp(i,j)))
c
c               elseif (j .eq. 14) then 
c
c 
c                 anpplai      = specla(j) * aleaf(j) * max(0., adnpp(i,j))
c
c==============================================================================
c addition of ceres-maize logic for leaf expansion and relationship to lai
c as a function of daily gdd accumulation
c==============================================================================
c
c calculate daily thermal time
c
c                   dtt = td(i) - baset(j)
c
c                   if (cumlvs(i,j) .lt. 5) then
c                     pc = 1.0
c                    explf = 73.5 
c                    explf = 63.0 
c                   else 
c                     pc = 1.0
c                    explf = 48.3 
c                     explf = 40.0
c                   endif
c
c                   cumlvs(i,j) = cumlvs(i,j) + max(0.0, dtt)/(explf * pc) 
c                   tix = max(0.0, dtt)/(explf * pc)
c
c calculate the number of the newest emerging leaf
c
c                   xn  = cumlvs(i,j) + 1.0
c
c ceres-maize and other models impose a stronger drop-off in
c leaf area accumulations for higher leaf numbers, but this
c requires the knowledge of the TLNO - or total number of
c leaves for the plant - which is something we don't or won't have
c in this model version
c
c                   if (xn .lt. 4) then
c                     plag = 5.55 * xn * tix 
c                   else if (xn .ge. 4 .and. xn .le. 13) then 
c                     plag = 0.261 * xn * xn * xn * tix 
c                   else
c                     plag = 611.5 * tix
c                   endif
c
c convert leaf area expanded (cm2) per plant to m2 basis
c
c                   plag = max(0.0, plag * nplants * 0.0001)
c
c                   abiol     = max(0.001, aleaf(j) * max(0.0, adnpp(i,j)))
c
c calculate specific leaf area just for added leaf area and biomass this time step
c
c                  aspecla   = plag / abiol
c
c impose limit on specific leaf area - limiting conditions for carbon assimilated
c
c                   if (aspecla .gt. 30.0) aspecla = 30.0
c
c                  plai(i,j) = plai(i,j) + abiol * aspecla 
c
c                  endif
c
c==============================================================================
c
               endif !of peak lai
c
c hold ending allocation values to stem and leaf for use by equations after shift to
c reproductive phenology stage begins
c
             astemi(i,j) = astem(j)
             aleafi(j) = aleaf(j)

c
c shift allocation either when enough gdd are accumulated or maximum number
c of days has elapsed since planting
c
c            else if ((hui(i,j) .ge. huigrain(j) .or. idpp(i,j) .ge. mxdgfi(j)) 

             else if (hui(i,j) .ge. huigrain(j).and.j.ne.16 
     >                 .and. croplive(i,j) .eq. 1.) then   !csant- dont need croplive here (it is in the croplive if!)

csant- now the control is "only" internal, through the gddmaturity, not all internal as we control gddmat.
              dpgf(i,j) = max(0.0, gddplant(i,j) - huigrain(j))             
c
c keep day of year that grain fill begins
c
              grainday(i,j) = min(grainday(i,j), real(jday))
c
c Phase 2 completed:
c
               awood(j) = 0.0
               aroot(j) = min(1.0, (arooti(j) - (arooti(j) - arootf(j))
     >                    * min(1.0, hui(i,j)/gddmaturity(i,j)))) 
               aroot(j) = max(0.0, aroot(j))

               if (thrlai(i,j) .lt. 0.0001) thrlai(i,j) = plai(i,j)

c
c lai plateau -- reached threshold -- according to heat unit index  (accumulated GDD logic)        
c set lai and hui threshold  (to be used in leaf phenology (senescence) calculations)
c shift aboveground allocation to grain/fruit
c
c
               templai(i,j)   = plai(i,j) 
c
c 
c lai decline based thermal time accumulation past grain fill 
c add check in to make sure huigrain is <= gddplant in this equation.  If it
c isn't because the days past planting is used to shift grain allocation, set
c huigrain value to the gddplant value when the shift took place so LAI starts
c the proper decline.
c 
                plai(i,j) = max(thrlai(i,j) * (1.0-min(max((gddplant(i,j)-
     >                      huigrain(j)),0.0)/(0.55*gddmaturity(i,j)), 1.0)
     >                      **laicons(j)), xminlai)

csant-the max(gddplant(i,j)-huigrain(j),0.0)  is necessary in case of idpp(i,j) .ge. mxdgfi(j)
csant- however, now is comented  elseif ((hui(i,j) .ge. huigrain(j) .or. idpp(i,j) .ge. mxdgfi(j)) 
c
c calculate decrease in lai for purpose of updating aboveground biomass pools
c
                 laidecl(i,j)   = max(0.0, templai(i,j) - plai(i,j)) 

                 if (astemi(i,j) .gt. astemf(j)) then
c
	 astem(j)=astemi(i,j)	


c	 if(i.eq.3)print*,'phase 2 ',iyear,jday,astem(j),astemi(3,j)

                   astem(j) = max(astem(j) * (1.0-min((hui(i,j)-
     >                       huigrain(j))/((gddmaturity(i,j)*declfact(j))
     >                      -huigrain(j)),1.0)**
     >                       allconss(j)),astemf(j)) 
                   astem(j) = max(0.0,astem(j)) 

c	 if(i.eq.3)print*,'phase 2 ',jday,astem(j)

	astemi(i,j)=astem(j)

                 endif

 35    format(2(1x,i4),5(1x,f6.3))
c
c CJK 9-23-04
c ***********
c
c                 astem(j) = 0.0  ! CJK 9-23-04
c                 aroot(j) = 0.0  ! CJK 9-23-04 
c
c	if(i.eq.15.and.j.eq.16)
c     >	print*,astem(j),hui(i,j),huigrain(j),gddmaturity(i,j)

c	print*,iyear,jday,aleafi(j),aleaff(j),aleaf(j)

                 if (aleafi(j) .gt. aleaff(j)) then
c
                    aleaf(j) = max(aleaf(j) * (1.0-min((hui(i,j)-
     >                       huigrain(j))/((gddmaturity(i,j)*declfact(j))
     >                      -huigrain(j)),1.0)**
     >                       allconsl(j)),aleaff(j)) 
                    aleaf(j) = max(0.0,aleaf(j)) 
c
                 endif

                 arepr(j)   = 1.0 - aroot(j) - astem(j) - aleaf(j) - awood(j)


             endif !of crop phase for annual crops

c****************************************************************************************************
c***************** Start Allocation to Perenial (Sugarcane) crops ********************
c****************************************************************************************************
       if (leafout(i,j).lt.huileaf(j).and.j.eq.16)  then

	 gddemerg(i) = 0.0
           awood(j)  = 0.0
	   aroot(j)  = 0.0
	   aerial(j) = 0.0
           aleaf(j)  = 0.0
	   astem(j)  = 0.0
	   arepr(j ) = 0.0
	       rm(i) = 0.0 

	   else if (leafout(i,j).ge.huileaf(j).and.j.eq.16)  then
	
c Phase 1 completed:
c ==================

       if(gddemerg(i).eq.0) gddemerg(i)=gddplant(i,j)
   

           awood(j)  = 0.0
	   aroot(j)  = 0.00001
	   aerial(j) = 0.00001
           aleaf(j)  = 0.00001
	   astem(j)  = 0.00001
	   arepr(j ) = 0.00001

       rm(i)= min(100., 100.*(gddplant(i,j)-gddemerg(i)) / (gddmaturity(i,j)-gddemerg(i)) )

csant- scheme based on CANEGRO model (Singels et al. 2005) 
csant - if sugarcane is planted, it takes long to construct the root sistem, if ratoon leafs develop faster
	if(cropy(i).eq.1) then
       aerial(j) = ( 1-arootf(j) ) * min(1.0, ( 1-exp(-rootd*0.2*rm(i)) ) ) 
	else
       aerial(j) = ( 1-arootf(j) ) * min(1.0, ( 1-exp(-rootd*    rm(i)) ) ) 
	endif

       aroot(j) = 1.-aerial(j)

       af1 = max(0.0, rm(i)*sf1 - sf1*ipf1)

       af2 = max(0.0, 1.0 - ( exp( -(ecf2*rm(i) - ecf2*ipf2) ) ) )

       astem(j) = aerial(j) * min(1.0, max(af1,af2))

casnt- make sure that aleaf(j) is at least .eq. to aleaff(j) 
       astem(j) = min( max(0.,aerial(j)-aleaff(j)) ,astem(j) ) 
       aleaf(j) = aerial(j)-astem(j) 

csant*****************  leaf/Stalk allocation **************************************************************
csant- Adjust the leaf/Stalk allocation in function os Temperature (physiological effect)

csant. all these relations, propoused below, have to be tested !!!
csant- the original, however I limited it to start after a certain stalk size!
csant- using these if the temp. effect is almos half dumped - 

c       if(rm(i).gt.50.) then
        astem(j) = astem(j) + min( aleaf(j) ,
     > (astem(j)/aerial(j))*ldf*aleaf(j)*min(1.,exp(tmld*ecf7)/exp((td(i)-273.16)*ecf7) ) )

csant: see if in future I can validate this relation.
csant:        astem(j) = astem(j) + ( plai(i,j)/laimx(j) )*
csant:       ldf*aleaf(j)*min(1,exp(tmld*ecf7)/exp((td(i)-273.16)*ecf7) )

c       endif

casnt- make sure that aleaf(j) is at least .eq. to aleaff(j) 

	if(plai(i,j)*grnfraccrop(i,j).lt.2..and.rm(i).lt.75.and.rm(i).gt.10) then !agora
       astem(j) = min( max(0.,aerial(j)-0.5) ,astem(j) ) !if green lai < 2. then aleaf is equal to 0.5 
	else
       astem(j) = min( max(0.,aerial(j)-aleaff(j)) ,astem(j) ) 
	endif


       aleaf(j) = aerial(j)-astem(j) 
csant*****************  leaf/Stalk allocation **************************************************************



csant- for sugarcane astem is the structural carbon, and arepr is the sucrose carbon

	sipf3=ipf1+(100.-ipf1)*(ipf3/100.)
       af3 = max(0.0, rm(i)*sf3 - sf3*sipf3)

c	sipf4=ipf2+(100.-ipf2)*(ipf4/100.)
	sipf4=ipf1+(100.-ipf1)*(ipf4/100.)

       af4 = max(0.0, 1.0- ( exp( -(ecf4*rm(i)-ecf4*sipf4) ) ) )

       arepr(j) = astem(j) * min(1., max(af3,af4))

       arepr(j) = min(aerial(j)-aleaf(j), arepr(j) )

       astem(j)= astem(j)- arepr(j) 

csant- Adjust the Sucrose/Stalk allocation in function os Temperature (physiological effect)

       af5= min(1., max(0., 1. -( exp((td(i)-273.16)*ecf5)/exp(tf5*ecf5) ) ) )
     >    + min(0., min(0., ( exp(tf5*ecf5)/exp((td(i)-273.16)*ecf5) ) -1 ) )

      sipf6=sipf4+(100.-sipf4)*(ipf6/100.)

      af6 = max(0.0, 1.0- ( exp(ecf6*sipf6)/exp(ecf6*rm(i)) ) )

csant- astem já é q4(da planilha),  ou seja, o resto do astem-sucrose, ou - astem(j)= astem(j)- arepr(j)

	ccf5=arepr(j)

       arepr(j) = arepr(j)+ astem(j)*wf5*af5*af6

      arepr(j) = min(aerial(j)-aleaf(j), arepr(j) )

      astem(j)= astem(j) - (arepr(j) - ccf5)       


c**************************************************************************************************
csant -     STAR of leaf adjust  -> adjust aleaf; check for LAI max and apply the leaf turn over **
c**************************************************************************************************

c calculate actual lai increase based on npp and allocation rules in ibis 
c only increment lai if it hasn't reached maximum allowable value 
c
	leaftemp=aleaf(j)

csant-               if (peaklai(i,j) .eq. 0) then

                 tlai(i,j) = plai(i,j) + (specla(j) * aleaf(j)
     >                        * max(0.0, adnpp(i,j)))

  
         if (tlai(i,j) .ge. laimx(j)) then

csant-                    aleaf(j) = min(1.0 - aroot(j), laimx(j) -
csant-     >                        plai(i,j)) / (specla(j) * adnpp(i,j)) 
csant- talk with Chris, and if its correct Iv to change in the annual crops (above) and in the others versions of the code

           aleaf(j) =  min(   aleaf(j) , 
     >	(laimx(j)-plai(i,j)) / (specla(j)*adnpp(i,j))   )!1-aroot is not / by specla(j)*adnpp(i,j)  


        aleaf(j) = max(0.0, aleaf(j))

	aroot(j) = aroot(j) + ( (leaftemp-aleaf(j)) * aroot(j)/(astem(j)+arepr(j)+aroot(j)) )
	astem(j) = astem(j) + ( (leaftemp-aleaf(j)) * astem(j)/(astem(j)+arepr(j)+aroot(j)) )
	arepr(j) = arepr(j )+ ( (leaftemp-aleaf(j)) * arepr(j)/(astem(j)+arepr(j)+aroot(j)) )

csant                peaklai(i,j) = 1

          endif  ! laimx


c if maize - employ leaf expansion routine based on a modified CERES gdd approach
csant- I clean the code comented here (but keeps in the annual crops), but in future 
csant- should be usefull take a look and considere these approuche, or look for other specific for sugarcane
csant- I remove here, lai decline is apllyed in the carbon and after the new - adnpp - is added in the cbiol below 
csant                   plai(i,j)    = plai(i,j) + (specla(j) * aleaf(j)
csant     >                          * max(0.0, adnpp(i,j)))

csant-               endif !of peak lai


       aroot(j) = max(0.0, aroot(j))
       aleaf(j) = max(0.0, aleaf(j))
       astem(j) = max(0.0, astem(j))
       arepr(j) = max(0.0, arepr(j))

c
c keep day of year that sucrose (grain fill) begins
	if(arepr(j).gt.0.001.and.grainday(i,j).gt.999)
     >      grainday(i,j) = min(grainday(i,j), real(jday))

csant__________________________ LAI DECLINE __________________________________

csant_old_decai- LAI decline is based on the LAI at the initial of the phase 3 
csant_old_decai-	if(rm(i).gt.laidcs) then 

csant_old_decai-              if (thrlai(i,j) .lt. 0.0001) 
csant_old_decai-     >         thrlai(i,j) = plai(i,j)
c
               templai(i,j)   = (cbiol(i,j)*specla(j))

csant_old_decai-                plai(i,j) = max(thrlai(i,j) * (1.0-min(max((gddplant(i,j)-
csant_old_decai-     >                      huigrain(j)),0.0)/(0.55*gddmaturity(i,j)), 1.0)
csant_old_decai-     >                      **laicons(j)), xminlai)

csant-   *****   New scheme to be tested in future ******* 
csant- LAI decline is exponential proportional to the LAI (in the sence that if LAI increases de the decline is hight) 
csant- the ( exp((plai(i,j)-laidc)/laidc) / exp((laimx(j)-laidc)/laidc) ) ranges exponentially from 0 (LAI=0) to 1 (LAI=laimx) 
csant. the idea is that if LAI aproaches 5 or 6, the shadon will be great, increase the mortality
csant. however, I have to get some data sets to validate this hypothesis

	plai(i,j) = (cbiol(i,j)*specla(j)) - (cbiol(i,j)*specla(j))*( (1./tauleaf(j)) )
c     >*min(1,(exp(((cbiol(i,j)*specla(j))-laidc)/laidc)/exp((laimx(j)-laidc)/laidc))))   


csant- as Iam not using the exp(..plai) function, I pass the lai decline direct to the cbiol (as for root)
csant-   *****   New scheme to be tested in future ******* 

csant- test the APSIM (parametrization) - lai declines linear for temp lower than 10C till zero if td = 0C
	if(td(i).le.278.16.and.td(i).ge.268.16) then
	print*,'td(i) <    5 C',iyear,jday,i,td(i)-273.16,plai(i,j)
		      plai(i,j) = plai(i,j)* max(0.4,min(1.,0.5+((td(i)-268.16)/20.) )) !let at least 10%
	print*,'plai reduction',iyear,jday,i,max(0.4,min(1.,0.5+((td(i)-268.16)/20.))),plai(i,j)
	elseif(td(i).lt.268.16) then
	plai(i,j) = 0.01
	print*,'temp < -5, sugarcane die (from APSIM) ',iyear,jday,i
	endif
c	print*, 'testar a inclusao da morte se td<5'
c	stop

	plai(i,j) = max(0.01,plai(i,j))
c
c calculate decrease in lai for purpose of updating aboveground biomass pools
c
                 laidecl(i,j)   = max(0.0, templai(i,j) - plai(i,j)) 
	
csan_old_decai-	 endif !LAI decline
csant___________________ END of the new LAI decline __________________________________ 

c**************************************************************************************************
csant -                  END of leaf adjusts  
c**************************************************************************************************


             endif !of crop phase for sugarcane
c
c********************* End Allocation to Perenial (Sugarcane) crops *******************************

c
c keep track of total biomass production for the entire year, and the
c aboveground value to calculate harvest index
c
       aybprod(i,j) = aybprod(i,j)  
     >                             +  aleaf(j) * max(0.0,adnpp(i,j))            
     >                             +  astem(j) * max(0.0,adnpp(i,j))            
     >                             +  aroot(j) * max(0.0,adnpp(i,j))            
     >                             +  awood(j) * max(0.0,adnpp(i,j))            
     >                             +  arepr(j) * max(0.0,adnpp(i,j))
c-above
       ayabprod(i,j) = ayabprod(i,j)  
     >                             +  aleaf(j) * max(0.0,adnpp(i,j))            
     >                             +  astem(j) * max(0.0,adnpp(i,j))            
     >                             +  arepr(j) * max(0.0,adnpp(i,j))
            

	cropout(i,j,35)=aleaf(j)
csant	cropout(i,j,32)=aroot(j)
	cropout(i,j,33)=arepr(j)
csant	cropout(i,j,34)=astem(j)


	if(i.eq.1) qgalho=qgalho+1  !write out in 10 days interval.
        if(i.eq.1.and.qgalho.eq.10) then

	  if(iyear.gt.2014)then
        write(222,42)j,iyear,jday,idpp(i,j),idpe(i,j),huigrain(j),gddmaturity(i,j),hui(i,j)
     >,plai(i,j),aroot(j),aleaf(j),astem(j),arepr(j)
     > ,cbior(i,j)/cfrac(j),cbiol(i,j)/cfrac(j),cbios(i,j)/cfrac(j)
     > ,cbiog(i,j)/cfrac(j),biomass(i,j)/cfrac(j)
	  endif
	qgalho=0

	endif

c 42   format(1x,f6.2,1x,f6.4,1x,f6.3,1x,f6.1,1x,f6.4,1x,f6.4,1x,f6.4,1x,f6.4)!,1x,f7.5)        

 42     format (3(i4,1x),5(f5.0,1X),15(f6.3,1x))        

c
c keep track of annual total root production carbon
c
               ayrprod(i,j) = ayrprod(i,j) +
     >                        aroot(j) * max(0.0,adnpp(i,j))
c
c keep track of total carbon allocated to leaves for litterfall calculation
c
               aylprod(i,j) = aylprod(i,j) 
     >                                     + aleaf(j) * max (0.0, adnpp(i,j))
c
c update carbon reservoirs using an analytical solution
c to the original carbon balance differential equation

csant. use this expression when the decay is applyed on LAI, instead of Cbiol
        cbiol(i,j) = cbiol(i,j) + aleaf(j) * max (0.0, adnpp(i,j))
     >                          - (laidecl(i,j)/specla(j))  
csant. use the eq. bellow for tauleaf acting in the carbon or the above for in LAI
csant. if laidecl=taulaf*plai, the result should be the same.
c	    cbiol(i,j) = cbiol(i,j) * exp(-1./tauleaf(j))  +
c     >                 aleaf(j) * tauleaf(j) * max (0.,adnpp(i,j)) *
c     >                 (1. - exp(-1./tauleaf(j)))
c
        cbiog(i,j) = cbiog(i,j) + arepr(j) * max (0.0, adnpp(i,j))
c
        cbios(i,j) = cbios(i,j) + astem(j) * max (0.0, adnpp(i,j))

csant-to calculate the root decay today (see below)
	fallrsgc(i,3) = cbior(i,j) + aroot(j) * max(0.0,adnpp(i,j))

        cbior(i,j) = cbior(i,j) * exp(-1.0/tauroot(j)) + 
     >                      aroot(j) * tauroot(j) * max(0.0,adnpp(i,j)) *
     >                      (1.0 - exp(-1.0/tauroot(j)))
c
	fallrsgc(i,3) = fallrsgc(i,3) - cbior(i,j)


               cbiow(i,j) = 0.0 
               cbior(i,j) = max(0.0, cbior(i,j)) 
               cbiol(i,j) = max(0.0, cbiol(i,j)) 
c??from vegetation          cbiol(i,j) = max (exist(i,j) * xminlai / specla(j),
c??why not     >                      cbiol(i,j))

               cbios(i,j) = max(0.0, cbios(i,j))
               cbiog(i,j) = max(0.0, cbiog(i,j)) 

c
c update vegetation's physical characteristics
c
               plai(i,j)    = cbiol(i,j) * specla(j) 

	  if(j.eq.16.and.rm(i).gt.1.) then   !sencon try as function of GDD, age and self-shade
csant- this is just a first attempt to consider dead leaves attached (that is important for sugarcane)
csant- next step should be find a real observed value, and try to specified the 
csant -relfectance transmitta for brown leaves (specified in radiation.f)
c      plai(i,j) = cbiol(i,j)*specla(j) + cbiol(i,j)*specla(j) *
c     >  (cbiol(i,j)*specla(j)/laimx(j))*min(0.2,max(0.0,0.3*rm(i)) )   !até 30 % como folha morta
       plai(i,j) = cbiol(i,j)*specla(j) + (aylprod(i,j)- cbiol(i,j) ) *specla(j)* 0.15 

      grnfraccrop(i,j)= cbiol(i,j)*specla(j)/plai(i,j) 

	endif


               biomass(i,j) = cbiol(i,j) + cbiog(i,j) + cbior(i,j)
     >                      + cbios(i,j) + cbiow(i,j)
c
c keep track of aboveground annual npp 
c
               ayanpp(i,j)  = (aleaf(j) + arepr(j) + astem(j)
     >                         + awood(j)) * adnpp(i,j) + ayanpp(i,j)
c
c---------------------------------------------------------------------------------
c check for climatic and phenological limits on maturity, growth, and harvest date
c
c check to see if minimum temperature has fallen below freeze
c kill threshold for 3 consecutive days and if lai is above a minimum, plant will
c be damaged/killed.  This function is more for spring freeze events
c or for early fall freeze events
c
c currently simulates too many grid cells that are killed by
c freezing temperatures 
c
c spring wheat is affected by this, winter wheat kill function
c is determined in crops.f - is a more elaborate function of
c cold hardening of the plant
c
               if (tmin(i) .le. tkill(j)) then
                 ccdays(i,j) = ccdays(i,j) + 1
               else
                 ccdays(i,j) = 0
               endif
c===============================================================
c removed on March 12 2002 - C. Kucharik
c until it can be a bit more refined, or used at a smaller scale.       
c we really have no way of validating this routine
c too difficult to implement on 0.5 degree scale grid cells 
c
                if (ccdays(i,j) .ge. 1    .and. 
     >             hui(i,j)    .ge. 0.6*gddmaturity(i,j)   .and.
c     >             plai(i,j)   .ge. 0.25 .and. 
c     >             imonth      .ge. 7    .and.
c     >             iday        .ge. 1    .and.
     >             croplive(i,j) .eq. 1  .and.
     >             ((j .eq. 13 .or. j .eq. 14 .or. j .eq. 16) .or.
     >               j .eq. 15 .and.   iwheat  .eq. 1)) then
                     croplive(i,j)     = 0.0
		print*,'tkill!!!!!',i,iyear,jday,idpp(i,j)
                     harvdate(i,j)     = jday
c                    grnfraccrop(i,j)  = 0.0   ! turn all vegetation to brown
               endif
c
c ================================================================
c if accumulated gdd past grain fill initiation exceeds limit
c or number of days past planting reaches a maximum, the crop has 
c reached physiological maturity and plant is harvested 
c
c crop could either be live or dead at this stage - these limits
c could lead to reaching physiological maturity or determining
c a cropy(i)est date for a crop that was killed by an early frost (see above)
c
c orig CJK Jan 27 03 if (dpgf(i,j) .ge. mxgddgf(j) .or.
c    >              idpp(i,j) .ge. mxmat(j)) then
c
	if(j.eq.16) then   !sugarcane



           if(cropy(i).eq.1) then  !planted

csant- the months here keeped in the harvest season window and prevent the shift of the harvest season 
csant- (ex. if always harv. ocurre at maximun date, after 4 ratoon the harv. will be 4th months late than the 1th)	

                if ( 
     > (  (hui(i,j).ge.gddmaturity(i,j)) .and. (idpp(i,j).ge.mxmat(j)-15)  )!-60)  )
     >    .or. 
     > (  (idpp(i,j).ge.mxmat(j)- 0) .and.   !30) .and.
     >    (iday.eq.pdmin(i,j).and.imonth.eq.(mod(pmmin(i,j)+(mxmat(j)/30.)-1,12.)+1))  ) !maximum harvest date
     >	        )then


                       croplive(i,j)     = 0.0
                       grnfraccrop(i,j)  = 0.0           ! turn all vegetation to brown
                       if (harvdate(i,j) .eq. 999) harvdate(i,j) = jday 
                       plai(i,j)         = 0.25          ! simulates remaining stubble/mulch
       print*,'Sug.Cane - 1st cycle = ',cropy(i),iyear,jday,idpp(i,j),hui(i,j),gddmaturity(i,j)
                endif


	
     	  else   !ratoon


                if ( 
     > (  (hui(i,j).ge.gddmaturity(i,j)) .and. (idpp(i,j).ge.365) )
     >    .or.       idpp(i,j).ge.395        
     >    .or.   (  (idpp(i,j).ge.335) .and.
     >    (iday.eq.pdmin(i,j).and.imonth.eq.(mod(pmmin(i,j)+(mxmat(j)/30.)-1,12.)+1))  ) !maximum expected harvest date
     >	        )then


                       croplive(i,j)     = 0.0
                       grnfraccrop(i,j)  = 0.0           ! turn all vegetation to brown
                       if (harvdate(i,j) .eq. 999) harvdate(i,j) = jday 
                       plai(i,j)         = 0.25          ! simulates remaining stubble/mulch

       print*,'Sug.Cane - ratoon = ',cropy(i),iyear,jday,idpp(i,j),hui(i,j),gddmaturity(i,j)

                endif

	 endif

		
	else  !other cultures 


c                if (hui(i,j) .ge. gddmaturity(i,j) .or.
c     >              idpp(i,j) .ge. mxmat(j)) then
                if ( (hui(i,j) .ge. gddmaturity(i,j) .or.idpp(i,j) .ge. mxmat(j))
     >	.and.   idpp(i,j).ge. (mxmat(j)-30)    ) then
                       croplive(i,j)     = 0.0
                       grnfraccrop(i,j)  = 0.0           ! turn all vegetation to brown
                       if (harvdate(i,j) .eq. 999) harvdate(i,j) = jday 
csant- but, as harvdate is eq jday, it will be equal to 0.001 at cropupdate, right?
                       plai(i,j)         = 0.25          ! simulates remaining stubble/mulch
                endif



	endif


c---------------------------------------------------------------------------------
           endif ! croplive
          
c
 80     continue
c
       endif  ! crop existence (cropsum)
 100    continue
c
c call to update crop lai, fractions, etc.
c
        call cropupdate(jday)
c
        call cropresidue(iyear,iyear0,jday) 
c
        if (iday .eq. 31 .and. imonth .eq. 12) then 
          call cropoutput(iday,imonth,iyear,jday) 
        endif 
c
c return to main program
c 
      return
      end
c
c ---------------------------------------------------------------------
      subroutine cropupdate(jday)
c ---------------------------------------------------------------------
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comsno.h'
      include 'comcrop.h'
      include 'comnitr.h'
      include 'compft.h'
c
*     real  laimx(npft)
c
      real  
*     >      xminlai,
*     >      ztopmxsoy,   ! maximum height of soybean canopy
*     >      ztopmxwht,   ! maximum height of wheat canopy
*     >      ztopmxmze,   ! maximum height of maize canopy
     >      avglail
c
      integer
     > i, j ,jday
c 
*      xminlai    = 0.010
c
c maximum crop height values (from EPIC and CERES models) 
c
*      ztopmxsoy   = 0.75  ! maximum height (m) of soybeans
*      ztopmxwht   = 1.2   ! maximum height (m) of wheat 
*      ztopmxmze   = 2.5   ! maximum height (m) of maize
c     
c maximum allowable lai
c 
*      laimx(13)   = 6.0      ! maximum lai of soybeans
*      laimx(14)   = 5.0      ! maximum lai of maize 
*      laimx(15)   = 7.0      ! maximum lai of wheat 
c
c begin global grid
c
      do 100 i = 1, npoi
       if (icropsum(i) .gt. 0.0) then
c
c maintain minimum value of leaf carbon in crops that exist
c
        do 90 j = scpft, ecpft 
         if (exist(i,j) .eq. 1.0 .and. croplive(i,j) .eq. 1.0) then
c 
          cbiol(i,j)   = max((exist(i,j) * xminlai/specla(j)),
     >                   cbiol(i,j))
c         plai(i,j)    = cbiol(i,j) * specla(j) 
csant- this carbon "doesnt exist" - is only to avois erros, right? 
csant  ------(and forgot to sum cbios!!!)cropout(i,j,30)
csant          biomass(i,j) = cbiol(i,j) + cbiog(i,j) + cbior(i,j)
csant-     >                   + cbiow(i,j)
c
c check for maximum plant leaf area index
c
          if (plai(i,j) .gt. plaimx(i,j)) plaimx(i,j) = plai(i,j)

         endif
c
 90   continue
c
c crop canopy single sided leaf area index (area-weighted)
c
       avglail = plai(i,13) +plai(i,14) +plai(i,15) +plai(i,16)           
c
c crop canopy fractions
c
        frac(i,13)  = plai(i,13)  /
     >               max (avglail, epsilon)
c
        frac(i,14)  = plai(i,14)  /
     >               max (avglail, epsilon)
c
        frac(i,15)  = plai(i,15)  /
     >               max (avglail, epsilon)
c
        frac(i,16)  = plai(i,16)  /
     >               max (avglail, epsilon)

c
c calculate total crop leaf are index
c
       totlail(i) = plai(i,13) +plai(i,14) +plai(i,15) +plai(i,16)
c
c
        fl(i) = totlail(i) / 1.  !1.6 works better for point (not local) simulations
c
c constrain fractional cover
c
c       fl(i) = max(0.25, min(0.975, fl(i)))
        fl(i) = max(0.025, min(0.975, fl(i)))
c
c calculate the crop canopy leaf area index using the fractional vegetation cover
c
        lai(i,1) = avglail / fl(i)
c
c C. Kucharik  04.02.01
c calculate greenness fraction of crop canopy
c if plant optical properties were ever changed - due to browning of
c vegetation in the future on a daily basis, do it here...this will effect leaf nir and vis
c transmittance and reflectance in radiation.f
c greenfracl(i) is lower canopy total green fraction - which is also being calculated
c for natural vegetation in vegetation.f 
c
       greenfracl(i) = 0.0
       do 80 j = scpft, ecpft

        greenfracl(i) = greenfracl(i) + frac(i,j) * grnfraccrop(i,j)  
	
 80    continue
c
c calculate total crop canopy biomass
c
       totbiol(i)=biomass(i,13)+biomass(i,14)+biomass(i,15)+biomass(i,16) 
c
c calculate crop bottom/top height
c ztop is calculated in phenology subroutine based leaf area index 
c is the weighted mean of plai of each crop pft in the grid cell
c will only be important if we allow more than one crop pft to exist
c within the same grid cell
c
        zbot(i,1) = 0.02
c
        ztop(i,1) = plai(i,13)/lai(i,1) * ztopmxsoy *
     >              (min(plai(i,13)/(laimx(13)-1.0),1.0))**2 +
     >              plai(i,14)/lai(i,1) * ztopmxmze *
     >              (min(plai(i,14)/(laimx(14)-1.5),1.0))**2 +                              
     >              plai(i,15)/lai(i,1) * ztopmxwht *
     >              (min(plai(i,15)/(laimx(15)-1.0),1.0))**2 +                              
     >              plai(i,16)/lai(i,1) * ztopmxsgc * min(1.,(rm(i)/50.))*
     >              (min(plaimx(i,16)/(laimx(16)),1.0))**2 
c
        htmx(i,1) = max(htmx(i,1),ztop(i,1))
        ztop(i,1) = max(0.05, max(htmx(i,1),ztop(i,1)))

	if(i.eq.9.and.j.eq.16)write(227,*)' ',jday,plai(i,j),laimx(j)
     >,grnfraccrop(i,j),(laimx(j)/(plai(i,j)+2)),ztop(i,1),fallr(i)
c
c calculate stem area index for crops 
c
      sai(i,1)=0.20*plai(i,13)+0.10*plai(i,14)+0.20*plai(i,15)+0.10*plai(i,16) 
c
c calculate annual aboveground npp total
c
       ayanpptot(i) = ayanpp(i,13) +ayanpp(i,14) +ayanpp(i,15) +ayanpp(i,16)
c
c end of loop
c
        endif  ! existence
 100  continue
c
c return to main program
c 
      return
      end
c
c ---------------------------------------------------------------------
      subroutine cropresidue(iyear,iyear0,jday)
c ---------------------------------------------------------------------
c
c routine that calculates the amount of residue that is input back to
c the soil at the end of growing season from crop at harvest
c
c also keeps track of effects of residue and plant nitrogen uptake on
c the nitrogen budget
c
c
c common blocks
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c
      real     
*     >         maxhi(npft),        ! Maximum harvest index allowed for crops
*     >         fyield(npft),       ! Adjustment factor for fraction of cbiog that
                                   ! is just grain - measured in the field 
*     >         cgrain(npft),       ! carbon fraction in grain
*     >         convfact(npft),     ! conversion factor to go to bu/acre 
     >         dumg,
     >         rdm,
     >         fdml,
     >         fdms,
     >  sumhy(npoi,npft),
     >         fnresidue, yc
c
      integer
     >         i, j,
     >         iyear,
     >         iyear0,
     >         jday,iy
c
c carbon in grain and conversion factors 
c
*      cgrain(13)   = 0.45
*      cgrain(14)   = 0.45
*      cgrain(15)   = 0.45
c
*      convfact(13) = 150.0
*      convfact(14) = 159.46
*      convfact(15) = 150.0
c
c set maximum allowable harvest indicies
c
*      maxhi(13)  = 0.38 ! soybean (from Cabelguenne et al. 1999) / CSANT = 0.5 , (Yield components, dry matter, LAI and LAD...PDF)
c
*      maxhi(14)  = 0.60 ! maize  (from EPIC and EPICphase  models) 
c
*      maxhi(15)  = 0.50 ! wheat  (from EPIC, EPIC phase, and CERES-wheat)  
c
c fraction of total alloction in reproductive stage (cbiog) that
c is actual harvested grain yield
c
*      fyield(13) = 0.85 ! from Lal et al. 1999 
*                        ! Agr. For. Met. 93:  53-70.
*      fyield(14) = 1.00
*      fyield(15) = 0.85 ! major storage organ is the ear, in which 85% is the grain
*                        ! Penning deVries, 1989 
c
c begin global grid
c
      do 100 i = 1, npoi
c
        if (icropsum(i) .gt. 0.0) then
c
c zero out litter fall rates
c
        falll(i) = 0.0
        fallw(i) = 0.0 
        fallr(i) = 0.0     

      if(croplive(i,16).eq.1) then

          fallrsgc(i,2) = max( fallrsgc(i,2)-fallrsgc(i,1)*(1./90.) , 0.) 	


          if(fallrsgc(i,2).gt.0) fallr(i)=fallr(i)+fallrsgc(i,1)*(1./90.)   !remove all old root in 60 days after harvest


                  fallr(i) = fallr(i) + fallrsgc(i,3)  !decay today 

csant - segundo o Humberto as folhas permanecem atachadas,       falll(i) = falll(i) + falllsgc(i,3)  !decay today   


        endif

c
        do 90 j = scpft, ecpft
c
c calculate CRM values from Pioneer regression relationships 
c Jan 28 03 CJK
c
          crmclim(i,j)    = max(73., min((gddmaturity(i,j)+53.683)/13.882,135.))
c         crmact(i,14)    = max(73., min((gdd8this(i)+53.683)/13.882,135.))
c
c crmact should designate what CRM rating resulted from GDD accumulation between
c killing frost/freeze events (-2.2 C)  
c
          crmact(i,14)    = max(73., min((gddfzcorn(i)+53.683)/13.882,135.))
c
          crmact(i,16)    = max(73., min((gddfzsgc(i)+53.683)/13.882,135.))
c
c gddplant gets reinitialized to 0.0 at maturity date so save value here
c
          if (gddplant(i,j) .gt. 0.0 .and. croplive(i,j) .eq. 1.) then
            crmplant(i,j)   = max(73., min((gddplant(i,j)+53.683)/13.882,135.))
          endif
c
c only write out values at harvest date, and re-initialize crop variables
c at this time - this allows for the same crop (e.g., wheat) to be grown
c across two consecutive calendar years   
c	
csant- as I pass idpp, I can treck witch year if(idop(i,j).lt.367)  pdate(i,j) = idop(i,j)

          if (exist(i,j) .eq. 1.0 .and. harvdate(i,j) .eq. jday) then
c
            pdate(i,j) = idop(i,j)
            idppout(i,j) = idpp(i,j)
            hdate(i,j) = harvdate(i,j)

 
           ayabprod(i,j) = max(cbiol(i,j), ayabprod(i,j))
c
c added so division by zero doesn't take place in those places where
c production was zero 
c
	if(j.ne.16) then

c adjust actual grain yield value (params.crp)
c
            dumg         = cbiog(i,j) 
            cbiog(i,j)   = cbiog(i,j) * fyield(j) 

c add excess (i.e. pod of soybeans) to stem storage pool of plant 
c
	cbios(i,j) = max(0.0, cbios(i,j) + (dumg - cbiog(i,j)))
c
c impose upper limit on harvest index as per JMN suggestion 
c if harvest index is > allowable, put excess to stem which will add to more
c litterfall and adjust the grain accordingly
c might have to revisit logic with respect to what actually get harvested, versus
c what is left in the field after harvest as litter input to the soil 
c 
            harvidx(i,j)   =  cbiog(i,j) / ayabprod(i,j)
c
            croplaimx(i,j) = plaimx(i,j)
c
            if (harvidx(i,j) .gt. maxhi(j)) then
c
              harvidx(i,j) = maxhi(j) 
              dumg         = cbiog(i,j) 
              cbiog(i,j)   = maxhi(j) * ayabprod(i,j)
c 
c add excess to stem of plant
c
              cbios(i,j)   = cbios(i,j) + (dumg - cbiog(i,j))
c
            endif

	endif ! for instance, i dont see reasons to enter here.
c
c
c calculate n in grain (kg/ha)
c
            grainn(i,j)   = (cbiog(i,j) / cfrac(j)) * fngrain(i,j) * 1e+04

c
c calculate crop yield in bu/acre or if sugarcane in ton/ha of fresh weight
	if(j.eq.16)then
csant- fyield= 0.8 :lost of top and botton at harvest 
csant-  1/0.3 = 30% of DM related to the Fresh weight 
csant- 10 =  kg/m2 to tonne(t)/ha
csant-    cgrain(16)=0.42105 ?? to convert 1g of C to Xg of C12_H22_O11 (C=12;H=1;O=16g/mol)
csant - 1.07 -to consider around 7 % of non C chemical substances (minerals, starch, protein and other metabolites)

        cropyld(i,j)=(cbiog(i,j)+cbios(i,j))*fyield(j)*(1./0.3)*10.0*(1./cgrain(j))*1.07 

	else
c            cropyld(i,j) = cbiog(i,j) * convfact(j) / cgrain(j) ! conversion factor to go to bu/acre, (1 t/ha = 15.93 bu/acre)
            cropyld(i,j) = (cbiog(i,j)*10.)/ cgrain(j) !  then (1 Kg/m2 = 10 t/ha)
	endif

c
c calculate dry matter material (Mg/ha); yield in Mg/ha dry matter (sucrose carbon)
c
        dmyield(i,j)   = cbiog(i,j)* 10.0 /cgrain(j)  
        dmstem(i,j)    = cbios(i,j)* 10.0 /cgrain(j)   
        dmleaf(i,j)    = cbiol(i,j)* 10.0 /cgrain(j)   
        dmroot(i,j)    = cbior(i,j)* 10.0 /cgrain(j)   
        dmcrop(i,j)    = dmyield(i,j) + dmstem(i,j) + dmleaf(i,j) + dmroot(i,j)

	if(j.eq.16)then
        dmyield(i,j) = dmyield(i,j) * fyield(j) 
        dmstem(i,j)  = dmstem(i,j)  * fyield(j)
	endif

c
c calculate above ground residue dry matter (Mg/ha) at harvest
c
             if(j.eq.16)then
       dmresidue(i,j) = dmleaf(i,j)+( (cbios(i,j)+cbiog(i,j))*(1-fyield(j))*10.0/cgrain(j) ) !meristem + the base not harvested
		else
       dmresidue(i,j) = dmleaf(i,j)  + dmstem(i,j)
              endif
c
c calculate aboveground residue dry matter total (including along the grow season)
c
       rdm  = dmresidue(i,j) + (aylprod(i,j)*10.0 /cgrain(j)) - dmleaf(i,j) !-dmleaf(i,j); since dmleaf(i,j) is accounted in aylprod    
c
c calculate fractions for leaf and stem
c
             fdml = (aylprod(i,j)*10.0 /cgrain(j)) / rdm 

             fdms = (dmresidue(i,j)-dmleaf(i,j)) / rdm
c
            fnresidue  = fnleaf(i,j) * fdml + fnstem(i,j) * fdms  

c
c calculate amount of N in aboveground residue (kg/ha) 
c
            residuen(i,j)  = (fnleaf(i,j) * dmleaf(i,j) 
     >           + fnstem(i,j) * (dmresidue(i,j)-dmleaf(i,j))  ) * 1e+04
c
c assign leaf, stem, root, and grain nitrogen concentrations
c to new variables (in percent)
c
            nconcl(i,j)    = fnleaf(i,j)   * 100.0
            nconcs(i,j)    = fnstem(i,j)   * 100.0
            nconcr(i,j)    = fnroot(i,j)   * 100.0
            nconcg(i,j)    = fngrain(i,j)  * 100.0
c   
c assign total nitrogen plant uptake to new variable for
c purposes of outputting data at end of year (kg/ha)
c
            cropn(i,j)     = totnuptake(i,j) * 1.e+04  
c
c assign total nitrogen fixation for crop to new variable for
c purposes of outputting this data at end of calendar year
c units of kg/ha
c
            cropfixn(i,j)  = totnfix(i,j) * 1.e+04
c
c carbon nitrogen ratio of plant residue goes to biogeochem.f
c and fine roots
c 
            if (fnresidue .gt. 0.0) then
              cntops(i,j) = min(cfrac(j) / fnresidue, 200.0) 
            else
              cntops(i,j) = 60.0
            endif
c
            if (fnroot(i,j) .gt. 0.0) then
              cnroot(i,j) = min(cfrac(j) / fnroot(i,j), 200.0) 
            else
              cnroot(i,j) = 80.0
            endif
c  
c assume that stem and leaf are both included in leaf litterfall value
c these annual total values (falll, fallr, fallw) cannot be changed 
c on a daily basis because biogeochem.f uses the annual total, split
c between each day of the year equally 
c carbon returned as residue
c

             if(j.eq.16)then

	        if(firecane.eq.1) then
            falll(i) = falll(i)              + (cbios(i,j)+cbiog(i,j))*(1-fyield(j))/2.  !meristem 
		else
csant-     falll(i) = falll(i) + cbiol(i,j) + (cbios(i,j)+cbiog(i,j))*(1-fyield(j))  !leaf + meristem and base not harvested
         falll(i) = falll(i) + aylprod(i,j) + (cbios(i,j)+cbiog(i,j))*(1-fyield(j))  !leaf + meristem and base not harvested
		endif

	    else		
            falll(i) = falll(i) + aylprod(i,j) +  cbios(i,j) 
	    endif



       if(j.eq.16.and.cropy(i).le.nratoon) then

            fallr(i) = fallr(i) + cbior(i,j) * 0.30

            fallrsgc(i,1) =  cbior(i,j) * 0.70
	    fallrsgc(i,2) =  cbior(i,j) * 0.70

	elseif(j.eq.16) then     !plant again 
            fallr(i) = fallr(i) + cbior(i,j) 
            fallrsgc(i,1) =  cbior(i,j) * 0.
	    fallrsgc(i,2) =  cbior(i,j) * 0.

	else
            fallr(i) = fallr(i) +  ayrprod(i,j) 
	endif


            fallw(i) = fallw(i) +  cbiow(i,j)





c
c re-initialize crop variables
c for current crop at harvest date 
c
            plai(i,j)        = 0.01
            thrlai(i,j)      = 0.0
            peaklai(i,j)     = 0.0                    
            ccdays(i,j)      = 0.0
            cbiol(i,j)       = 0.0
            cbior(i,j)       = 0.0	
            cbios(i,j)       = 0.0
            cbiog(i,j)       = 0.0
            cbiow(i,j)       = 0.0
            hui(i,j)         = 0.0
            aybprod(i,j)     = 0.0
            ayrprod(i,j)     = 0.0
            ayabprod(i,j)    = 0.0
            aylprod(i,j)     = 0.0
            leafout(i,j)     = 0.0
            htmx(i,1)        = 0.0
            cumlvs(i,j)      = 0.0
            plaimx(i,j)      = 0.0
            dpgf(i,j)        = 0.0
            biomass(i,j)     = 0.0
            totnuptake(i,j)  = 0.0
            tnplant(i,j)     = 0.0
            totnfix(i,j)     = 0.0
            idpp(i,j)        = 0.0
            idpe(i,j)        = 0.0
            gddplant(i,j)    = 0.0
            gddtsoi(i,j)     = 0.0
            sai(i,1)         = 0.0
            fu(i)            = 0.0
            lai(i,1)         = 0.0
            zbot(i,1)        = 0.0
            ztop(i,1)        = 0.0
            totbiol(i)       = 0.0
            totlail(i)       = 0.0  
            vf(i)            = 0.0  ! vernalization factor for winter wheat
            arepr(j)         = 0.0
            idop(i,j)        = 999
            cropout(i,j,50)=gddmaturity(i,j)
	    cropout(i,j,51)= gddplant(i,j)
  	    cropout(i,j,52)=grainday(i,j)
	    grainday(i,j)  = 9999.


c___________________________________________________________________
c******************** sugarcane ratoon   ***************************

        if(j.eq.16.and.cropy(i).eq.1) then
c	gddsgcp(i,1)=cropout(i,j,50)
	else
             if(iyear .le. iyear0+5) then
  	      gddsgcp(i,2)=(gddsgcp(i,2)+cropout(i,j,50))/2.  !changing faster, due to the dif between GDDclim and GDD.
	     else
	      gddsgcp(i,2)=(gddsgcp(i,2)*(nratoon-1)+cropout(i,j,50))/nratoon
	     endif
	endif

c	print*,'Sug.Cane gddsgcp(1)/gddsgcp(2)',gddsgcp(i,1),gddsgcp(i,2),gddsgcp(i,1)/gddsgcp(i,2)


        if(j.eq.16.and.cropy(i).le.nratoon) then

            cropy(i) = cropy(i)+1	
	   
            croplive(i,j)     = 1.0     
            idop(i,j)         = jday+1    !next day, and it can add fertilizer nitrogen in the next time step


c_______________ Check if cycle was complete __________________________
        if(ccdays(i,j).ge.1.and.cropy(i).eq.2.and.idppout(i,j).lt.mxmat(j)-90)then  !ccdays - temperature has fallen below freeze 
                 croplive(i,j) = 0.    
                 idop(i,j)     = 999
  	         cropy(i)      = 0     ! sugarcane planted didt get the minimum of development, start to planting again
	print*,'sugarcane planted didt get the minimum of development, start to planting again'
	stop
        elseif(ccdays(i,j).ge.1.and.cropy(i).gt.2.and.idppout(i,j).le.300)then !I am not sure if plant again or ratoon can survive!
                 croplive(i,j) = 0.    
                 idop(i,j)     = 999
  	         cropy(i)      = 0     ! ratoon didt get the minimum days to development, start to planting again
	print*,'ratoon didt get the minimum days to development, start to planting again'
	stop
	endif
c_______________ Check if cycle was complete __________________________




               if (iyear .le. iyear0+1) then

	     gddmaturity(i,16) = max(2000., min (gdd12(i) , hybgdd(j)))
	print*,'Sug.Cane,i,iyear,gdd12,gddmaturity',i,iyear,gdd12(i),gddmaturity(i,16)

               elseif (iyear .ge. iyear0+2 .and. iyear .le. iyear0+5) then

            gddmaturity(i,16) = max(2000., min (gddsgc(i,iyear-1), hybgdd(j))) !hybrid based on last year
	print*,'Sug.Cane,i,iyear,gddsgc,gddmaturity',i,iyear,gddsgc(i,iyear-1),gddmaturity(i,16)

                else 

               sumhy(i,j) = 0

		 yc = 5.0

                  do iy = iyear-5,iyear-1     ! hybrid based on previous 5 year average - farm management 
                   sumhy(i,j) = sumhy(i,j) + gddsgc(i,iy) 
                   enddo

                avehybrid(i,j)    = sumhy(i,j) / yc
             gddmaturity(i,j) = max(2000.,min(avehybrid(i,j), hybgdd(j)))   ! using constant hybrid for location over entire period 
	if(i.eq.9)
     >	print*,'Sug.Cane,i,iyear,avehybrid,gddmaturity',i,iyear,avehybrid(i,j),gddmaturity(i,16)

	        endif	

        else

c	print*,'end of crop cycle',i,iyear,cropy(i)
	
	cropy(i) = 0
	

	endif  ! sugarcane ratoon
c************************************************************************		


		

           endif  ! harvest = jday

 90     continue
c
       endif ! crop existence
c  
 100  continue       
c
c call to map out vegetation classes
c subroutine is within vegetation.f
c
      call vegmap
c
c return to main program
c

      return
      end
c
c ---------------------------------------------------------------------
      subroutine irrigation(iday, imonth)
c ---------------------------------------------------------------------
c
c routine that calculates the amount of water that is applied to a
c managed ecosystem on a daily basis (mm/day)
c
c based on average daily water content in the soil
c
c this amount will be evenly applied in timestep increments (start and stop time)
c based on the duration of the event - similar to the way precipitation
c events are handled 
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c local variables
c
      integer
     >  iday,
     >  imonth,
     >  i, j, k
c
      real
     >  awcmax,
     >  awc 

c
      do 100 i = 1, npoi
c
        if (iday .eq. 1 .and. imonth .eq. 1) then
          totirrig(i) = 0.0
        endif
c
        awcmax = 0.0
        awc    = 0.0 
c
        do 110 k = 1, 5  ! top 5 soil layers = 100 cm 
c
c calculate the maximum available water in top 1 m (root zone) at field capacity
c
          awcmax = awcmax + max(0.0, (sfield(i,k) - swilt(i,k))) *
     >                       hsoi(k) * poros(i,k) * 100
c
c calculate actual amount of water available to plant - current water content
c based on daily average water and ice content in soil layers down to a meter 
c 
          awc    = awc    + max(0.0, (adwisoilay(i,k)     +
     >                      (1. - adwisoilay(i,k))        *
     >                      adwsoilay(i,k)) - swilt(i,k)) *
     >                      hsoi(k) * poros(i,k)          * 100
c
 110    continue    
c
c    irrigation will occur if :
c
c  * the crop has been planted
c  * the minimum daily temperature is > 5 C
c  * the 5-day running mean temperature is > 10 C 
c  * the actual soil water content in the top 1 m of soil
c    is less than or equal to 50% of the maximum value at field capacity,
c
          if (awc .le. 0.50 * awcmax 
     >       .and. (tmin(i) .gt. 278.16 .and. a5td(i) .gt. 283.16)
     >       .and. (icropsum(i) .gt. 0.)) then
c            .and. (croplive(i,j) .gt. 0)  ! have to add in that irrigation only occurs when crop is planted! 
c
c irrigation (applied above the canopy)  is used to make up the difference to field capacity
c convert awc values from cm to mm - this is a per day value 
c so it is consistent with the values used in weather.f (diurnal) for precipitation 
c
c set upper limit based on literature search typical of what a farmer could
c apply in a typical day of irrigation and what rates of application are
c 
            xirrig(i) = min(150.0, max(0.0, awcmax - awc) * 10.0)
c 
          else 
c
            xirrig(i) = 0.0
c
          endif
c
c
 100  continue
c
c return to main program
c
      return
      end
c
c ---------------------------------------------------------------------
      subroutine nitrostress(istep,iday,imonth) 
c ---------------------------------------------------------------------
c
c subroutine calculates the effects of the amount of available inorganic
c nitrogen on carbon assimilation in crops 
c
c strictly speaking, stressn* is multiplied to the vmax parameters 
c used in the photosynthesis calculations
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'com1d.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c local variables
c
      real 
*    >      smax,           ! maximum value stressn can have
     >      tnsupply,       ! total nitrogen supply from soil to plant 
     >      gfn,            ! function controlling nitrogen uptake
*    >      cnmax,          ! maximum allowable residue c/n ratio
     >      wsupply,        ! supply of water to plant from transpiration stream (kg/h20/m2/s)
*    >      alphacc,
*    >      gnmin,
     >      awc,
     >      sumnval,
     >      fnmin,
     >      wc,
     >      dmplant,
     >      fdmleaf,
     >      fdmstem,
     >      fdmroot,
     >      fdmgrain,
     >      dmres,
     >      flres,
     >      fsres,
     >      tngrain,
     >      tnleaf,
     >      tnstem,
     >      tnroot, 
     >      fnsmax,
     >      fnrmax,
     >      fnpmax, 
     >      f1, f2, f3
c
c biological fixation
c
      real
     >      gs,
     >      fxg,
     >      rdepth,
     >      fc,
     >      wp,
     >      sm, 
     >      fxw,
     >      rd,
     >      fxn,
     >      fxr,
*    >      availn,
     >      fnresidue
c
      real  plantn(npoi)    ! plant available nitrogen total
c
*      real  fnlfmx(npft),
*     >      fngrmx(npft),
*     >      sratio(npft),
*     >      rratio(npft),
*     >      fnopt(npft)
c
      integer iday,
     >        imonth,
     >        istep,
     >        iter,
     >        iter2,
     >        i, j, k
c
*        cfrac(13)    = 0.50  ! fraction of dry matter that is carbon   
*        cfrac(14)    = 0.50    
*        cfrac(15)    = 0.45    
c
*        fnlfmx(13)   = 0.025 ! average leaf nitrogen content  
*        fnlfmx(14)   = 0.013 ! average leaf nitrogen content
c
*        if (iwheat .eq. 1) fnlfmx(15)   = 0.008 ! average leaf nitrogen content  
*        if (iwheat .eq. 2) fnlfmx(15)   = 0.010 ! average leaf nitrogen content 
c
         if(iwheat.gt.0) then
            fnlfmx(15) = fnlfmxw(iwheat) 
c
c        fnlfmx(15)   = 0.060 ! average leaf nitrogen content  
c
*        fngrmx(13)   = 0.040 ! maximum allowed grain nitrogen fraction
*        fngrmx(14)   = 0.017
c
*        if (iwheat .eq. 1) fngrmx(15)   = 0.0300 ! maximum allowed grain nitrogen fraction
*        if (iwheat .eq. 2) fngrmx(15)   = 0.0225 ! maximum allowed grain nitrogen fraction
c
            fngrmx(15) = fngrmxw(iwheat)
c
*        sratio(13)   = 0.40  ! typical ratio of stem to leaf nitrogen content
*        sratio(14)   = 0.05  
*        sratio(15)   = 0.40  ! typical ratio of stem to leaf nitrogen content
c
*        rratio(13)   = 0.75  ! typical ratio of root to leaf nitrogen content
*        rratio(14)   = 0.75  ! typical ratio of root to leaf nitrogen content
*        rratio(15)   = 1.00  ! typical ratio of root to leaf nitrogen content
c  
c
*        fnopt(13) = 0.0075   ! minimum leaf nitrogen concentration - stress onset - soybean
c        fnopt(14) = 0.03125  ! leaf nitrogen concentration - stress onset - maize 
*        fnopt(14) = 0.02850  ! leaf nitrogen concentration - stress onset - maize 
c
*        if (iwheat .eq. 1) fnopt(15) = 0.0175   ! stress onset for leaf nitrogen - spring wheat 
*        if (iwheat .eq. 2) fnopt(15) = 0.0100   ! stress onset for leaf nitrogen - winter wheat 
c
            fnopt(15) = fnoptw(iwheat)
         endif
c
      do 100 i = 1, npoi
c
*        alphac         = 0.0005   ! minimum uptake rate applied to nitrogen (mm/timestep) 
*        gnmin         = 0.001    ! minimum nitrogen fraction allowed in grain  
*        smax          = 1.05     ! maximum nitrogen stress factor value     
*        availn        = 1.0      ! scaling variable to adjust for plant capability in capturing
                                 ! nitrogen from pools - above 1.0 means plant can take
                                 ! some up in excess of what is in the transpiration stream 
        fngrain(i,13) = 0.035  
        fngrain(i,14) = 0.013
        fngrain(i,15) = 0.020  
        fngrain(i,16) = 0.013
c
        tnsupply  = 0.0
        awc       = 0.0
        sumnval   = 0.0
c
c initialize layer nitrogen uptake
c
        do 180 k = 1, nsoilay
c
          tnuptake(i,k) = 0.0
          anuptake(i,k) = 0.0
c
 180    continue
c
        do 200 j = scpft, ecpft 
c
          if (exist(i,j) .eq. 1.0) then
            if (croplive(i,j) .eq. 1.0) then 
c
*             cnmax       = 95 
              fnmin       = cfrac(j) / cnmax
              stressn(i,j)= 1.0
              gfn         = 1.0
c
c calculate the total nitrogen supply rate (kg/m2/day) for each soil layer based on
c 1) the total daily water uptake for crops (lower canopy) (mm/day - stats.f)
c 2) total available nitrogen pool to roots (soil solution) (kg-no3/m2)
c 3) and available water content (mm)
c
c NOTE:  at this time, logic in IBIS cannot be used to determine
c the uptake of nitrogen for each specific pft (mixed in each grid
c cell because upsoil is for the entire lower canopy...will have
c to weight it for now on the lai of that pft [frac(i,j)] 
c
c since it is being called each timestep, use instantaneous values
c of soil ice and moisture
c  
        do 210 k = 1, nsoilay 
c
c calculate water content in each layer - based on EPIC parameterizations
c that look at actual water content in mm and not available water 
c
              wc          =  max(0.0, (wisoi(i,k)   +
     >                       (1.0 - wisoi(i,k))      *
     >                       wsoi(i,k))) * hsoi(k)  * 
     >                       poros(i,k)             * 1000
c
c
c alphac is minimum uptake rate to account for nitrogen usage
c even when transpiration is small (plant still able to take up
c nitrogen)
c
c allow plant to take up nitrogen in excess of the
c transpiration stream at low rates early in the
c season 
c
c supply of nitrogen to crops is from roots corresponding to [l]ower
c canopy in model - since this routine is being called each timestep
c use the value of upsoil(i,k) from canopy.f rather than the value from
c stats.f which is the daily average (adupsoil)   
c upsoil is in units of mm/m2/s of transpiration
c
             wsupply      =  upsoil(i,k) * dtime 
c
c make sure that water content is not zero - 
c set to small limit
c
             wc = max(1.0, wc)
c
c the total nitrogen uptake from the layer comes from the total n
c in the layer both in solution and in soil - leachable n is only
c that portion that is in the solution
c
c value of tnuptake for this layer is used in subroutine in solute
c leaching algorithm as a net sink of nitrogen to the layer 
c
c only allow uptake in layers that have roots
c make sure nitrogen uptake only occurs while crop is active
c the minimum rate will only be applied when the plant is not
c experiencing moisture stress
c
              if (froot(k,1) .gt. 0.005 .and. tnpptot(i) .gt. 0.0) then
                tnuptake(i,k) = max(alphac * stressl(i,k), wsupply) * availn *
     >                          (smsoil(i,k) + smsoln(i,k)) 
              else
                tnuptake(i,k) = 0.0
              endif
c
 210    continue
c
c            endif
           
        if (aybprod(i,j) .gt. 0.0  .and.
     >      aylprod(i,j) .gt. 0.0) then
c
c for the purpose of dealing with total nitrogen uptake,
c we have to use year to date total carbon production
c in these equations because some root and leaf biomass
c has been adjusted due to phenology in the model
c
            dmplant   =  aybprod(i,j)  / cfrac(j)
            fdmleaf   =  (aylprod(i,j) / cfrac(j)) / dmplant
            fdmstem   =  (cbios(i,j)   / cfrac(j)) / dmplant
            fdmroot   =  (ayrprod(i,j) / cfrac(j)) / dmplant
            fdmgrain  =  (cbiog(i,j)   / cfrac(j)) / dmplant
c
            dmres     =  (aylprod(i,j) + cbios(i,j)) / cfrac(j) 
            flres     =  (aylprod(i,j) / cfrac(j)) / dmres
            fsres     =  (cbios(i,j)   / cfrac(j)) / dmres  
c
            fnplant(i,j)   =  max(0.0, totnuptake(i,j) / dmplant) 
c
c maintain minimum nitrogen concentration in leaf and stem (potential residue)
c
            iter  = 0
            iter2 = 0
c
c           write(*,*) fnleaf(i,j), fnresidue, cbiog(i,j) 
 400        fnleaf(i,j) = (fnplant(i,j) - fngrain(i,j) * fdmgrain) /
     >                    (fdmleaf + sratio(j) * fdmstem +
     >                     rratio(j) * fdmroot)
c
            fnleaf(i,j) = max(0.0, fnleaf(i,j))
            fnstem(i,j) = fnleaf(i,j) * sratio(j)
            fnroot(i,j) = fnleaf(i,j) * rratio(j)
c
            fnresidue   = fnleaf(i,j) * flres + fnstem(i,j) * fsres 
            if (fnresidue .gt. fnmin) iter2 = 1
            if (fnresidue .le. fnmin) iter  = 1 

c            if (iter .eq. 1 .and. fngrain(i,j) .gt. gnmin
c     >                      .and. fdmgrain     .gt. 0.0
c     >                      .and. iter2        .eq. 0) then 
c                    fngrain(i,j) = max(gnmin, fngrain(i,j) * 0.99)
c                    write(*,*) 'taking from grain', fngrain(i,j)
c                    goto 400
c         
            if (iter2 .eq. 1 .and. fngrain(i,j) .lt. fngrmx(j) 
     >                      .and. fdmgrain     .gt. 0.0
     >                      .and. iter         .eq. 0) then 
                    fngrain(i,j) = min(fngrmx(j), fngrain(i,j) * 1.01)
                    goto 400
            endif
c
c ------------------------------------------------------------------------------
c calculate nitrogen content in various pools
c
            tngrain = fngrain(i,j) * fdmgrain * dmplant
            tnleaf  = fnleaf(i,j)  * fdmleaf  * dmplant      
            tnstem  = fnstem(i,j)  * fdmstem  * dmplant      
            tnroot  = fnroot(i,j)  * fdmroot  * dmplant      
c
            tnplant(i,j) = tngrain + tnleaf + tnstem + tnroot 
c
            fnsmax   = sratio(j)  * fnlfmx(j)
            fnrmax   = rratio(j)  * fnlfmx(j) 
c
            fnpmax   = fnlfmx(j) * fdmleaf + fnsmax * fdmstem +
     >                 fnrmax * fdmroot + fngrmx(j) * fdmgrain               
c
c
c calculate function controlling rate of nitrogen uptake
c based on plant nitrogen concentration and maximum value
c there is a chance early in growing season that fnplant
c could be higher than fnpmax - thus maximize the below
c fraction to be .le. 1.0 
c
             gfn = 1. - min(1.0, fnplant(i,j) / fnpmax) ** 1.0 
c
c calculate the annual running total of the actual nitrogen
c uptake for all pfts in lower canopy (crops)
c
c adjust nitrogen uptake by plant by gfn factor - equal in each layer
c
             do 310 k = 1, nsoilay 
               anuptake(i,k) = tnuptake(i,k) * gfn
               tnsupply      = tnsupply + anuptake(i,k)
 310         continue 
c
             totnuptake(i,j) = totnuptake(i,j) + tnsupply              
c
             totnuptake(i,j) = max(0.0, min((1.0 - cfrac(j)) * dmplant, totnuptake(i,j))) 
c
c calculate stress parameter using rectangular hyperbola which
c relates leaf nitrogen concentration to vmax        
c
c rectangular hyperbola 
c f1 and f2  control the shape of the stress response function 
c which spans from 0 to 1.0 for leaf n concentrations from 0 to 4 percent 
c
c s-shaped curve nitrogen limitation effect from epic model
c
              f1    = 8.5 
              f2    = 11.0 
c
c ratio of leaf nitrogen concentration to the optimal maximum
c for corn/maize
csant- I think that is best not use nitro stress for now, but after the model is ready include again.
             f3 = 2 * (fnleaf(i,j) / fnopt(j))
c
c             stressn(i,j) = min (smax, (f3 / (f3 + exp(f1 - f2 * f3))))
c
c             stressn(i,j) = max (0.10, stressn(i,j)) 
c
c----------------------------------------------------------------------
c biological fixation of nitrogen through symbiosis in soybeans
c----------------------------------------------------------------------
c
c Key reference:
c M. Cabelguenne et al., Agricultural systems 60: 175-196, 1999.
c this module is taken from the new epicphase model
c
c the amount of daily n-fixation by the plant is based on a fraction
c of the total daily n-uptake.  It is controlled by three main factors:
c
c * growth stage of the crop (0-1) *
c * soil moisture            (0-1) * 
c * nitrogen in rooting zone (0-1) * 
c
c the growth stage factor (fxp) inhibits fixation in young plants
c and old plants, and peaks between 30-55% of the crop cycle
c
c the soil water content factor (fxw) reduces n-fixation when the 
c water content in the top 0.3 m is less than 85% of field capacity
c
c the soil nitrogen (plant available) factor (fxn) reduces n-fixation
c when the nitrogen amount in the root zone is greater than 100 kg/ha  
c
c  
             if (j .eq. 13) then
c
c calculate growth stage and factor (fraction of total average gdd)
c
               gs = hui(i,j) / gddmaturity(i,j)
c
               if     (gs .le. 0.15 .or.  gs .ge. 0.75) then
                 fxg = 0.0
               elseif (gs .gt. 0.15 .and. gs .le. 0.30) then
                 fxg = 6.67 * gs - 1.0
               elseif (gs .gt. 0.30 .and. gs .le. 0.55) then
                 fxg = 1.0
               else
                 fxg = 3.75 - 5.0 * gs   
               endif
c
c calculate effect of soil moisture in top 25-30 cm
c
c              rdepth = 1. / (hsoi(1) + hsoi(2) + hsoi(3))  
               rdepth = 1. / (hsoi(1) + hsoi(2))  
c
               fc = 0.0
               wp = 0.0
               sm = 0.0
               plantn(i) = 0.0
c
               do 220 k = 1, 2
                 fc = fc + sfield(i,k) * hsoi(k)
                 wp = wp + swilt(i,k)  * hsoi(k)
                 sm = sm + wsoi(i,k)   * hsoi(k)
c
c calculate available plant nitrogen total in these layers
c
                 plantn(i) = plantn(i) + smsoil(i,k) + smsoln(i,k)
c
 220           continue 
c
               fc = fc * rdepth
               wp = wp * rdepth
               sm = sm * rdepth
               sm = min(sm, 0.85 * (fc - wp) + wp)
c
               fxw = (sm - wp) / (0.85 * (fc - wp)) 
c
c calculate effect of plant available nitrogen pool
c 
               rd = 1.0   ! rooting depth in meters

               if     (plantn(i) .gt. 0.0300) then
                 fxn = 0.0
               elseif (plantn(i) .le. 0.0100) then
                 fxn = 1.0
               else
                 fxn = 1.5 - 0.005 * (plantn(i) * 10000) / rd
               endif
c
c equation for fxn has to be in kg/ha nitrogen and meters for rd 
c
c CJK replaced 2/1/2006  
c the 1.5 at the end of the following equation was to increase
c the annual n-fixation rates by 50% because of simulated errors
c and a low bias compared to observations
c
               fxr  = min(1.0, fxw, fxn) * fxg 
c
               fixn(i,j) = fxr * tnsupply 
c              fixn(i,j) = 0.0 
c 
c fixn is thus calculated each timestep
c
c update plant available nitrogen pool for fixation from soybean
c
             else  ! non-soybean crop - no fixation
c
               fixn(i,j) = 0.0
c
             endif    ! soybeans only fix nitrogen
c
             totnfix(i,j) = totnfix(i,j) + fixn(i,j)
c
             do 240 k = 1, nsoilay 
c
c critical: what quantity do we add the nitrogen fixation
c to?  previous? depends on what order subroutines are called
c 
               smsoil(i,k) = smsoil(i,k) + fixn(i,j) * froot(k,1)
c
 240         continue
c
           endif   ! production gt 0
         endif    ! crop plant  
        endif     ! crop existence 
c
 200  continue    ! crop pft 
c
 100  continue    ! grid cell 
c
c return to program 
c
      return
      end
c ---------------------------------------------------------------------
      subroutine cropoutput(iday,imonth,iyear,jday) 
c ---------------------------------------------------------------------
c
c output crop variables for a single grid cell - diagnostic output 
c
c common blocks
c
      include 'implicit.h'
c
      include 'compar.h'
      include 'comatm.h'
      include 'comsoi.h'
      include 'comsum.h'
      include 'comveg.h'
      include 'comcrop.h'
      include 'comnitr.h'
c
c local variables
c
      integer
     >         iday,
     >         jday,
     >         imonth,
     >         iyear,
     >         i,j,k,l,n
c
      real
     >         tnsoi,
     >         nuptake,
     >         hi,
     >         grain,
     >         fyld,
     >         yld

c define soil layer for which you want output for
c calculate total immobile/mobile N pool totals to
c specified soil depth (isoilay input)
c
      do 100 i = 1, npoi 
c 
                tsnimm(i) = 0.
                tsnmob(i) = 0.
                tnsoi     = 0.
c
c total soil inorganic nitrogen pools available to plant for uptake
c
                do 330 k = 1, isoilay
                  tsnimm(i) = tsnimm(i) + smsoil(i,k)     ! immobile pool
                  tsnmob(i) = tsnmob(i) + smsoln(i,k)     ! mobile pool (e.g., nitrate) 
                  tnsoi = tnsoi + smsoil(i,k) + smsoln(i,k)  
 330            continue
c
c output data for model studies
c cropn is in kg/ha, totnvegn is not 
c
                  nuptake = 0.0
                  do 335 j = scpft, ecpft
                    nuptake = nuptake + cropn(i,j)
 335              continue
                    nuptake = nuptake + totnvegn(i) * 1.e+04
c
c write out variables 
c
                  do 380 n = scpft, ecpft
c
                    if (exist(i,n) .eq. 1.0 .and. cropplant(i,n) .eq. 1.
     >                  .and. i .eq. 1 .and. harvdate(i,n) .ne. 999) then 
c
c grid cell 1 corresponds to that which is closest to lat, lon for ARL
c
                       open(16, file = 'crop.output.dat',status = 'unknown')
                       write(16,340) 
     >                             iyear,
     >                             cropyld(i,n),                 
c
c     >                             iyear,
c     >                             n,
c     >                             tnsoi*1.e04,
     >                             fertinput(i,n),
c    >                             ydeposn(i)*1.e04,
     >                             aynmintot(i)*1.e04,
c    >                             ayimmtot(i)*1.e04,
     >                             nuptake, 
c     >                             yno3leach(i),
c     >                             concn(i),
c    >                             snbalance(i),
c     >                             cropyld(i,n),
c    >                             dmyield(i,n),
     >                             harvidx(i,n),
c     >                             dmleaf(i,n),
c     >                             dmstem(i,n),
c     >                             dmroot(i,n),
c     >                             dmyield(i,n),
c     >                             dmcrop(i,n),
c     >                             cntops(i,n),
c    >                             cnroot(i,n),
     >                             nconcl(i,n),
     >                             nconcs(i,n),
     >                             nconcr(i,n),
     >                             nconcg(i,n),
     >                             grainn(i,n),
c     >                             drntot(i),
c    >                             ayprcp(i),
     >                             croplaimx(i,n),
     >                             cropfixn(i,n),
c    >                             totcsoi(i),
     >                             idop(i,n),
     >                             harvdate(i,n) 
                     endif
 340                format(i6,5f8.2,4f8.4,3f8.2,i5,i5)
c 340                format(i6,f8.2)
c
 380    continue  ! loop through all pfts
c
 100    continue
c
      return
      end
