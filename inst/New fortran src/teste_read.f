      integer jday,   ! day of the year
     >        imonth, ! month
     >        iyear,  ! year
     >        istyr  ! 1st year in data files
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
      integer istart, icount,npoi
	parameter (npoi=3)
		real radn(npoi),stinprecd(npoi),stintmax(npoi),stintmin(npoi)

	character 
     >  date*10     !Date (dd/mm/yyyy)
      

      open (22,file='input_1d/met_Ingham.txt',status='unknown')


csant- I am not using it now, but may be in future can use
csant- if is going use -> than istini can be specified at ibis.infile
c count how many days since beginning of daily data
c daily data begin on Jan 1 of the initial istini 
c     istart = 0
c
c     do i= istini, iyear-1 		
c           istart = istart + 365
c          if (mod(i,4).eq.0) then
c            if (mod(i,100).ne.0) then
c              istart = istart + 1
c            else if (mod(i/100,4).eq.0) then
c              istart = istart + 1
c            end if
c          end if
c     enddo 
c
c	do i=1,istart	
c	read(22,*)
c	enddo



	read(32,*) date,radn(1),stinprecd(1),stintmax(1),stintmin(1)


	do i = 1, npoi

	stintmax(i) = stintmax(1)
	stintmin(i) = stintmin(1)
	stintd(1)= 0.44 * xintmax(i) + 0.56 * xintmin(i)
	stinprecd(i) = stinprecd(1)

	enddo

	print*,date,jday,stinprecd(1)
c read daily cloudiness
c
c    xincldd(1) 
c
c read daily windspeed
c   xinwindd(1) 
c
c read daily humidity
c    xinqd(1) )
c

      return
      end
