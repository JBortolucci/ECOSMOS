
      dailyWrapper <- function (arguments_vector, 
integer_vector, 
double_vector,
npoi_int_matrix, 
npoi_double_matrix,
npft_matrix,
ndaypm,
ik,
iwetday,
ayanpp,
croplive,
daygddc,
daygdds,
daygddsgc,
daygddw,
exist,
gddcorn,
gddplant,
gddsgc,
gddsgcp,
gddsoy,
gddtsoi,
idpp,
pcd,
pcm,
precipday,
tsoi,
xincld,
xinprec,
xinq,
xint,
xintrng,
xinwet,
xinwind,
xinwindd,
xstore,
ztop) {

            integer          arguments_vector[9]
            integer          integer_vector[11] 
            double precision double_vector(2)
            
            integer          npoi_int_matrix(6, integer_vector[1])
            double precision npoi_double_matrix(41, integer_vector[1])
            double precision npft_matrix(3, integer_vector[2])

            integer          ndaypm(12)
            integer          ik(integer_vector[1], integer_vector[2])
            integer          iwetday(integer_vector[1],31)
            double precision ayanpp(integer_vector[1],integer_vector[2])
            double precision croplive(integer_vector[1],integer_vector[2])
            double precision daygddc(integer_vector[1],366)
            double precision daygdds(integer_vector[1],366)
            double precision daygddsgc(integer_vector[1],366)
            double precision daygddw(integer_vector[1],366)
            double precision exist(integer_vector[1],integer_vector[2])
            double precision gddcorn(integer_vector[1],2100) 
            double precision gddplant(integer_vector[1],integer_vector[2])
            double precision gddsgc(integer_vector[1],2100)
            double precision gddsgcp(integer_vector[1],2)
            double precision gddsoy(integer_vector[1],2100)
            double precision gddtsoi(integer_vector[1],integer_vector[2])       
            double precision idpp(integer_vector[1],integer_vector[2])
            double precision pcd(integer_vector[1],integer_vector[2])
            double precision pcm(integer_vector[1],integer_vector[2])
            double precision precipday(integer_vector[1],31)
            double precision tsoi(integer_vector[1],integer_vector[5])
            double precision xincld(integer_vector[1],12)
            double precision xinprec(integer_vector[1],12)
            double precision xinq(integer_vector[1],12)
            double precision xint(integer_vector[1],12)
            double precision xintrng(integer_vector[1],12)
            double precision xinwet(integer_vector[1],12)
            double precision xinwind(integer_vector[1],12)
            double precision xinwindd(integer_vector[1])
            double precision xstore(integer_vector[1],3)
            double precision ztop(integer_vector[1],2)

            integer npoi_int_line1(integer_vector[1])
            integer npoi_int_line2(integer_vector[1])
            integer npoi_int_line3(integer_vector[1])
            integer npoi_int_line4(integer_vector[1])
            integer npoi_int_line5(integer_vector[1])
            integer npoi_int_line6(integer_vector[1])

            double precision npoi_double_line1(integer_vector[1])
            double precision npoi_double_line2(integer_vector[1])
            double precision npoi_double_line3(integer_vector[1])
            double precision npoi_double_line4(integer_vector[1])
            double precision npoi_double_line5(integer_vector[1])
            double precision npoi_double_line6(integer_vector[1])
            double precision npoi_double_line7(integer_vector[1])
            double precision npoi_double_line8(integer_vector[1])
            double precision npoi_double_line9(integer_vector[1])
            double precision npoi_double_line10(integer_vector[1])
            double precision npoi_double_line11(integer_vector[1])
            double precision npoi_double_line12(integer_vector[1])
            double precision npoi_double_line13(integer_vector[1])
            double precision npoi_double_line14(integer_vector[1])
            double precision npoi_double_line15(integer_vector[1])
            double precision npoi_double_line16(integer_vector[1])
            double precision npoi_double_line17(integer_vector[1])
            double precision npoi_double_line18(integer_vector[1])
            double precision npoi_double_line19(integer_vector[1])
            double precision npoi_double_line20(integer_vector[1])
            double precision npoi_double_line21(integer_vector[1])
            double precision npoi_double_line22(integer_vector[1])
            double precision npoi_double_line23(integer_vector[1])
            double precision npoi_double_line24(integer_vector[1])
            double precision npoi_double_line25(integer_vector[1])
            double precision npoi_double_line26(integer_vector[1])
            double precision npoi_double_line27(integer_vector[1])
            double precision npoi_double_line28(integer_vector[1])
            double precision npoi_double_line29(integer_vector[1])
            double precision npoi_double_line30(integer_vector[1])
            double precision npoi_double_line31(integer_vector[1])
            double precision npoi_double_line32(integer_vector[1])
            double precision npoi_double_line33(integer_vector[1])
            double precision npoi_double_line34(integer_vector[1])
            double precision npoi_double_line35(integer_vector[1])
            double precision npoi_double_line36(integer_vector[1])
            double precision npoi_double_line37(integer_vector[1])
            double precision npoi_double_line38(integer_vector[1])
            double precision npoi_double_line39(integer_vector[1])
            double precision npoi_double_line40(integer_vector[1])
            double precision npoi_double_line41(integer_vector[1])

            double precision npft_line1(integer_vector[2])
            double precision npft_line2(integer_vector[2])
            double precision npft_line3(integer_vector[2])

            integer i

# 	      Inicializando es double de tamanho npoi
            DO 102, i <- 1, integer_vector[1], 1
                       npoi_double_line1(i) <- npoi_double_matrix(1,i)
                       npoi_double_line2(i) <- npoi_double_matrix(2,i)
                       npoi_double_line3(i) <- npoi_double_matrix(3,i)
                       npoi_double_line4(i) <- npoi_double_matrix(4,i)
                       npoi_double_line5(i) <- npoi_double_matrix(5,i)
                       npoi_double_line6(i) <- npoi_double_matrix(6,i)
                       npoi_double_line7(i) <- npoi_double_matrix(7,i)
                       npoi_double_line8(i) <- npoi_double_matrix(8,i)
                       npoi_double_line9(i) <- npoi_double_matrix(9,i)
                       npoi_double_line10(i) <- npoi_double_matrix(10,i)
                       npoi_double_line11(i) <- npoi_double_matrix(11,i)
                       npoi_double_line12(i) <- npoi_double_matrix(12,i)
                       npoi_double_line13(i) <- npoi_double_matrix(13,i)
                       npoi_double_line14(i) <- npoi_double_matrix(14,i)
                       npoi_double_line15(i) <- npoi_double_matrix(15,i)
                       npoi_double_line16(i) <- npoi_double_matrix(16,i)
                       npoi_double_line17(i) <- npoi_double_matrix(17,i)
                       npoi_double_line18(i) <- npoi_double_matrix(18,i)
                       npoi_double_line19(i) <- npoi_double_matrix(19,i)
                       npoi_double_line20(i) <- npoi_double_matrix(20,i)
                       npoi_double_line21(i) <- npoi_double_matrix(21,i)
                       npoi_double_line22(i) <- npoi_double_matrix(22,i)
                       npoi_double_line23(i) <- npoi_double_matrix(23,i)
                       npoi_double_line24(i) <- npoi_double_matrix(24,i)
                       npoi_double_line25(i) <- npoi_double_matrix(25,i)
                       npoi_double_line26(i) <- npoi_double_matrix(26,i)
                       npoi_double_line27(i) <- npoi_double_matrix(27,i)
                       npoi_double_line28(i) <- npoi_double_matrix(28,i)
                       npoi_double_line29(i) <- npoi_double_matrix(29,i)
                       npoi_double_line30(i) <- npoi_double_matrix(30,i)
                       npoi_double_line31(i) <- npoi_double_matrix(31,i)
                       npoi_double_line32(i) <- npoi_double_matrix(32,i)
                       npoi_double_line33(i) <- npoi_double_matrix(33,i)
                       npoi_double_line34(i) <- npoi_double_matrix(34,i)
                       npoi_double_line35(i) <- npoi_double_matrix(35,i)
                       npoi_double_line36(i) <- npoi_double_matrix(36,i)
                       npoi_double_line37(i) <- npoi_double_matrix(37,i)
                       npoi_double_line38(i) <- npoi_double_matrix(38,i)
                       npoi_double_line39(i) <- npoi_double_matrix(39,i)
                       npoi_double_line40(i) <- npoi_double_matrix(40,i)
                       npoi_double_line41(i) <- npoi_double_matrix(41,i)
102         CONTINUE



            DO 103, i <- 1, integer_vector[1], 1
                       npoi_int_line1(i) <- npoi_int_matrix(1,i)
                       npoi_int_line2(i) <- npoi_int_matrix(2,i)
                       npoi_int_line3(i) <- npoi_int_matrix(3,i)
                       npoi_int_line4(i) <- npoi_int_matrix(4,i)
                       npoi_int_line5(i) <- npoi_int_matrix(5,i)
                       npoi_int_line6(i) <- npoi_int_matrix(6,i)
103         CONTINUE

# 	      Inicializando es double de tamanho npoi
            DO 104, i <- 1, integer_vector[2], 1
                       npft_line1(i) <- npft_matrix(1,i)
                       npft_line2(i) <- npft_matrix(2,i)
                       npft_line3(i) <- npft_matrix(3,i)
104         CONTINUE


            call daily(arguments_vector[1], arguments_vector[2], arguments_vector[3], arguments_vector[4],
arguments_vector[5], arguments_vector[6], arguments_vector[7], arguments_vector[8],
arguments_vector[9],
integer_vector[1], integer_vector[2], integer_vector[3], integer_vector[4],
integer_vector[5], integer_vector[6], integer_vector[7], integer_vector[8],
integer_vector[9], integer_vector[10], integer_vector[11],
double_vector(1), double_vector(2),
npoi_int_line1, npoi_int_line2, npoi_int_line3, npoi_int_line4, npoi_int_line5,
npoi_int_line6,
npoi_double_line1, npoi_double_line2, npoi_double_line3, npoi_double_line4,
npoi_double_line5, npoi_double_line6, npoi_double_line7, npoi_double_line8,
npoi_double_line9, npoi_double_line10, npoi_double_line11, npoi_double_line12, 
npoi_double_line13, npoi_double_line14, npoi_double_line15, npoi_double_line16,
npoi_double_line17, npoi_double_line18, npoi_double_line19, npoi_double_line20,
npoi_double_line21, npoi_double_line22, npoi_double_line23, npoi_double_line24,
npoi_double_line25, npoi_double_line26, npoi_double_line27, npoi_double_line28,
npoi_double_line29, npoi_double_line30, npoi_double_line31, npoi_double_line32,
npoi_double_line33, npoi_double_line34, npoi_double_line35, npoi_double_line36,
npoi_double_line37, npoi_double_line38, npoi_double_line39, npoi_double_line40,
npoi_double_line41,
npft_line1, npft_line2, npft_line3,
ndaypm,
ik,
iwetday,
ayanpp,
croplive,
daygddc,
daygdds,
daygddsgc,
daygddw,
exist,
gddcorn,
gddplant,
gddsgc,
gddsgcp,
gddsoy,
gddtsoi,
idpp,
pcd,
pcm,
precipday,
tsoi,
xincld,
xinprec,
xinq,
xint,
xintrng,
xinwet,
xinwind,
xinwindd,
xstore,
ztop)



}

# ---------------------------------------------------------------------
      daily <- function (iyear, imonth, iday, jday, seed, jdaily, iyranom, iyrlast, nrun,
npoi,
npft,
scpft,
ecpft,
nsoilay,
cdcyear,
gdddy,
ireccru,
istend,
istyear,
iwheat,
grav,
rair,
cropy,
cdays,
endday,
iniday,
iwetdaysum,
iwet,
a10td,
cloud,
consdays,
gdd0cthis,
gdd0this,
gdd10this,
gdd12this,
gdd5this,
gdd8this,
cdcinprecd,
gddfzcorn,
gddfzsgc,
gddfzsoy,
gddpl15,
gsdays,
maxcons,
precip,
precipdaysum,
psurf,
qd,
stincldd,
stinprecd,
stinqd,
stintd,
stintmax,
stintmin,
stinwindd,
tcthis,
td,
tmax,
tmin,
twthis,
ud,
vf,
xincldd,
xinqd,
xintd,
xintopo,
za,
xinprecd,
xintrngd,
baset,
mxmat,
mxtmp,
ndaypm,
ik,
iwetday, #75
ayanpp,
croplive,
daygddc,
daygdds,
daygddsgc,
daygddw,
exist,
gddcorn,
gddplant,
gddsgc,
gddsgcp,
gddsoy,
gddtsoi,
idpp,
pcd,
pcm,
precipday,
tsoi,
xincld,
xinprec,
xinq,
xint,
xintrng,
xinwet,
xinwind,
xinwindd,
xstore,
ztop) {
     
# ---------------------------------------------------------------------

# overview

# this routine generates daily weather conditions from monthly - mean
# climatic parameters

# specifically, this routine generates daily values of

# - daily total precipitation
# - daily maximum temperature
# - daily minimum temperature
# - daily average cloud cover
# - daily average relative humidity
# - daily average wind speed

# in order to generate daily weather conditions, the model uses a series
# of 'weather generator' approaches, which generate random combinations of
# weather conditions based upon the climatological conditions

# in general, this weather generator is based upon the so - called Richardson
# weather generator

# appropriate references include:

# Geng, S., F.W.T. Penning de Vries, and L. Supit, 1985:  A simple
# method for generating rainfall data, Agricultural and Forest
# Meteorology, 36, 363 - 376

# Richardson, C. W. and Wright, D. A., 1984: WGEN: A model for 
# generating daily weather variables: U. S. Department of
# Agriculture, Agricultural Research Service.

# Richardson, C., 1981: Stochastic simulation of daily
# precipitation, temperature, and solar radiation. Water Resources 
# Research 17, 182 - 190

# common blocks

      implicit none

#      include 'compar.h'
#      include 'combcs.h'
#      include 'comatm.h'
#      include 'comsum.h'
#      include 'comveg.h'
#      include 'comsoi.h'
#      include 'comcrop.h'
#      include 'com1d.h'



#      integer variables

       integer          npoi
       integer          npft
       integer          scpft
       integer          ecpft
       integer          nsoilay
       integer          cdcyear
       integer          gdddy
       integer          ireccru
       integer          istend
       integer          istyear      
       integer          iwheat

#      double variabels

       double precision grav
       double precision rair

#      npoi integer matrix

       integer          cropy(npoi)
       integer          cdays(npoi)
       integer          endday(npoi)
       integer          iniday(npoi)
       integer          iwetdaysum(npoi)
       integer          iwet(npoi)

#      npoi double matrix

       double precision a10td(npoi)
       double precision cloud(npoi)
       double precision consdays(npoi)
       double precision gdd0cthis(npoi)
       double precision gdd0this(npoi)
       double precision gdd10this(npoi)
       double precision gdd12this(npoi)
       double precision gdd5this(npoi)
       double precision gdd8this(npoi)
       double precision cdcinprecd(npoi)
       double precision gddfzcorn(npoi)
       double precision gddfzsgc(npoi)
       double precision gddfzsoy(npoi)
       double precision gddpl15(npoi)
       double precision gsdays(npoi)
       double precision maxcons(npoi)
       double precision precip(npoi)
       double precision precipdaysum(npoi)
       double precision psurf(npoi)
       double precision qd(npoi)
       double precision stincldd(npoi)
       double precision stinprecd(npoi)
       double precision stinqd(npoi)
       double precision stintd(npoi)
       double precision stintmax(npoi)
       double precision stintmin(npoi)
       double precision stinwindd(npoi)
       double precision tcthis(npoi)
       double precision td(npoi)
       double precision tmax(npoi)
       double precision tmin(npoi)
       double precision twthis(npoi)
       double precision ud(npoi)
       double precision vf(npoi)
       double precision xincldd(npoi)
       double precision xinqd(npoi)
       double precision xintd(npoi)
       double precision xintopo(npoi)
       double precision za(npoi)
       double precision xinprecd(npoi)
       double precision xintrngd(npoi)

#      npft double matrix

       double precision baset(npft)
       double precision mxmat(npft)
       double precision mxtmp(npft)

#      others

       integer          ndaypm(12)
       integer          ik(npoi,npft)
       integer          iwetday(npoi,31)
       double precision ayanpp(npoi,npft)
       double precision croplive(npoi,npft)
       double precision daygddc(npoi,366)
       double precision daygdds(npoi,366)
       double precision daygddsgc(npoi,366)
       double precision daygddw(npoi,366)
       double precision exist(npoi,npft)
       double precision gddcorn(npoi,2100) 
       double precision gddplant(npoi,npft)
       double precision gddsgc(npoi,2100)
       double precision gddsgcp(npoi,2)
       double precision gddsoy(npoi,2100)
       double precision gddtsoi(npoi,npft)       
       double precision idpp(npoi,npft)
       double precision pcd(npoi,npft)
       double precision pcm(npoi,npft)
       double precision precipday(npoi,31)
       double precision tsoi(npoi,nsoilay)
       double precision xincld(npoi,12)
       double precision xinprec(npoi,12)
       double precision xinq(npoi,12)
       double precision xint(npoi,12)
       double precision xintrng(npoi,12)
       double precision xinwet(npoi,12)
       double precision xinwind(npoi,12)
       double precision xinwindd(npoi)
       double precision xstore(npoi,3)
       double precision ztop(npoi,2)

# Arguments

      integer seed,
iyear,
imonth,
iday,
jday,
jdaily   # 1 if reading in daily weather data
# 0 if using random / statistical weather generator      

# local variables

      integer it1w,      # indice of previous month (interpolation)
it2w,      # indice of following month (interpolation)
i,j, k     # loop indice

      real rwork,          # 
omcloud,        # cloud cover
omqd,           # humidity
omtmax,         # maximum temperature
ran2,           # function random number generator
dt,             # used for interpolation
pwet,           # monthly - average probability of rainy day
pwd,            # probability of a wet day after a dry day
pww,            # probability of a wet day after a wet day
rndnum,         # random number to decide if wet or dry day
rainpwd,        # average rainfall per wet day
alpha,          # parameter for gamma function
beta,           # parameter for gamma function
aa,
ab,
tr1,
tr2,
rn1,rn2, rn3,rn,  #random numbers
s1,
s2,
s12,
z,
tdm,            # mean daily mean temperature
trngm,          # mean daily mean temperature
tmaxm,          # mean maximum temperature
tminm,          # mean minimum temperature
tmaxd,          # maximum temperatures for dry days
tmaxw,          # maximum temperatures for wet days
tmaxe,          #'expected' maximum temperature for today
tmine,          #'expected' minimum temperature for today
tmaxs,          # standard deviation in maximum temperature (K)
tmins,          # standard deviation in minimum temperature (K)
cloudm,         # mean cloud cover for today (fraction)
cloudd,         # dry day cloud cover
cloudw,         # wet day cloud cover
cloude,         # expected cloud cover today
clouds,         # standard deviation of cloud fraction
v,
tdum,           # storage variable
qdm,            # mean relative humidity
qdd,            # dry day relative humidity
qdw,            # wet day relative humidity 
qde,            # expected relative humidity (based on wet / dry decision)
qdup,           # upper bound of humidity distribution function
qdlow,          # lower bound of humidity distribution function
y,
b3,
b2,
b1,
x1,
amn,
eud             # expected daily average wind speed from monthly mean
 

      real a(3,3),
b(3,3)

      real
ee(3), 
r(3),
rr(3),
x(3)

      integer
iyranom,
iyrlast,
nrun



      real
precipfac,
dif,
humidfrac,
sphumid,
qcov(12)

# define autocorrelation matrices for Richardson generator

# note that this matrix should be based upon a statistical
# analysis of regional weather patterns

# for global simulations, we use 'nominal' values

      data a / 0.600,  0.500,  0.005,
0.010,  0.250,  0.005, 
0.020,  0.125,  0.250 /

      data b / 0.500,  0.250, - 0.250,
0,  0.500,  0.250, 
0,  0,  0.500 /
      


# ---------------------------------------------------------------------- 
# *  *  * initial setup for daily climate calculations *  * *
# ---------------------------------------------------------------------- 

# define working variables

      rwork <- (grav / rair / 0.0065)

# 'omega' parameters used to calculate differences in expected
# climatic parameters on wet and dry days

# following logic of weather generator used in the EPIC crop model

# omcloud -- cloud cover
# omqd    -- humidity
# omtmax  -- maximum temperature

      omcloud <- 0.90    # originally 0.90
      omqd <- 0.50    # originally 0.50
      omtmax <- 0.75    # originally 0.75

# calculate weighting factors used in interpolating climatological
# monthly - mean input values to daily - mean values

# this is a simple linear interpolation technique that takes into
# account the length of each month

      if(jdaily == 0) {
        if(float(iday) < float(ndaypm(imonth) + 1) / 2) {
          it1w <- imonth - 1
          it2w <- imonth 
          dt <- (float(iday) - 0.5) / ndaypm(imonth) + 0.5
        } else {
          it1w <- imonth 
          it2w <- imonth + 1
          dt <- (float(iday) - 0.5) / ndaypm(imonth) - 0.5
        }

        if (it1w < 1) it1w <- 12
        if (it2w > 12) it2w <- 1
      }

# initialize this year's values of gdd0, gdd5, tc, tw

      if(iday == 1  && imont == 1) {


        call const (tcthis, npoi,  100)
        call const (twthis, npoi, - 100)

        call const (gdd0this, npoi, 0)
        call const (gdd5this, npoi, 0)
        call const (gdd0cthis, npoi, 0)
        call const (gdd8this, npoi, 0)
        call const (gdd10this, npoi, 0)
        call const (gdd12this, npoi, 0)

# initialize variables to zero at beginning of year
# for crop types that do not grow over two calendar years
# 
# constant for gdd based on 10 cm soil temperature (planting bed-
# used to calculate leaf emergence after planting

          do  i <- 1, npoi
             do  j <- 1, npft

              if(j <= scpft - 1) {  # natural vegetation 
              ayanpp(i,j) <- 0
#sant--             else if (croplive(i,j) == 0  && j  >= scpft) then !it is yearly, therefore doesnt matter if dead, right?
              } else if(                           j >= scpft) {
                ayanpp(i,j) <- 0
             }
	     }
         }
#           
      }

# initialize this crop year's values 


          do  i <- 1, npoi

             do  j <- scpft, ecpft

      if(iday == pcd(i,j) && imont == pcm(i,j)) {

	if(j == 13) {
	if(exist(i,13) == 1) {
        gddsoy(i,iyear) <- gddfzsoy(i) 
    
        consdays(i) <- 0
        iniday(i) <- 9999
        maxcons(i) <- 0
        gsdays(i) <- 0
        gddfzcorn(i) <- 0
        gddfzsoy(i) <- 0
        gddfzsgc(i) <- 0
	} else {
	gddsoy(i,iyear) <- 0
	}
	}

	if(j == 14) {
	if(exist(i,14) == 1) {
        gddcorn(i,iyear) <- gddfzcorn(i) 
        consdays(i) <- 0
        iniday(i) <- 9999
        maxcons(i) <- 0
        gsdays(i) <- 0
        gddfzcorn(i) <- 0
        gddfzsoy(i) <- 0
        gddfzsgc(i) <- 0
	} else {
	gddcorn(i,iyear) <- 0
	}
	}

	if(j == 16) {
	if(exist(i,16) == 1) {
        gddsgc(i,iyear) <- gddfzsgc(i)
	
	print(paste0('INTO WEATHER.F ',i,iyear,jday,pcd(i,j),pcm(i,j),gddsgc(i,iyear)))
    
        consdays(i) <- 0
        iniday(i) <- 9999
        maxcons(i) <- 0
        gsdays(i) <- 0
        gddfzcorn(i) <- 0
        gddfzsoy(i) <- 0
        gddfzsgc(i) <- 0
	} else {
	gddsgc(i,iyear) <- 0
	}
	}

#sant - we dont need this if, the gddplant <- 0 after harvest and will be count only if croplive, right?
           if(croplive(i,j) == 0 ) {
              gddplant(i,j) <- 0
              gddtsoi(i,j) <- 0
           }
          
      }
	
	     }
	}



# ---------------------------------------------------------------------- 
# *  *  * set daily climatic variables for entire domain *  * *
# ---------------------------------------------------------------------- 

      for(i in 1: npoi) { 

# ---------------------------------------------------------------------- 
# *  *  * use weather generator to create daily statistics *  * *
# ---------------------------------------------------------------------- 

        if(jdaily == 0) {

# ---------------------------------------------------------------------- 
# (1) determine if today will rain or not (following Geng et al.)
# ---------------------------------------------------------------------- 

# implement simple first - order Markov - chain precipitation generator logic
# based on Geng et al. (1986), Richardson and Wright (1984),
# and Richardson (1981) 

# basically, this allows for the probability of today being a wet day 
# (a day with measureable precipitation) to be a function of what
# yesterday was (wet or dry)

# the logic here is that it is more likely that a wet day will follow
# another wet day -- allowing for 'storm events' to persist

# calculate monthly - average probability of rainy day 

          pwet <- max (1, xinwet(i,imonth)) / ndaypm(imonth)

# estimate the probability of a wet day after a dry day

          pwd <- 0.75 * pwet

# estimate the probability of a wet day after a wet day

          pww <- 0.25 + pwd


# Beginning of block of code that figures out daily precip for
# entire month on the first day of each month

          if(iday == 1) {

# Verify the dataset consistency especially when using interannual anomalies
# of precipitations (negative values, or too few wet days in a rainy month)

            xinprec(i, imonth) <- max(0.01, xinprec(i, imonth))
            xinwet(i, imonth) <- max(1, xinwet(i, imonth))

}

# Initialize monthly parameters back to zero

            iwetdaysum(i) <- 0
            precipdaysum(i) <- 0

            for(j in 1: 31) { 
              iwetday(i,j) <- 0
              precipday(i,j) <- 0
}

# Loop through number of days in this month and determine precip

            for(j in 1: ndaypm(imonth)) { 

# decide if today is a wet day or a dry day using a random number

              rndnum <- ran2(seed)
              


# If it is the first day of the month do not look at previous day

          if(j == 1) {
                if(rndnum <= pwd) {
                  iwetday(i,j) <- 1
                  iwetdaysum(i) <- iwetdaysum(i) + 1
                } else {
                  iwetday(i,j) <- 0
                }

         } else {

# If it is not the first day, look at yesterday's wet / dry index to help
# determine if today is wet or dry

#	if(i == 1)print(paste0('change all code, have to confirm it...'))
             if(iwetday(i,j - 1) == 0) {
                  if(rndnum <= pwd) {
                  iwetday(i,j) <- 1
                  iwetdaysum(i) <- iwetdaysum(i) + 1
		} else {
		iwetday(i,j) <- 0
                  }
             } else {
                  if(rndnum <= pww) {
		iwetday(i,j) <- 1
                  iwetdaysum(i) <- iwetdaysum(i) + 1
		} else {
		iwetday(i,j) <- 0
                  }

             }

          }

# ---------------------------------------------------------------------- 
# (2) determine today's precipitation amount (following Geng et al.)
# ---------------------------------------------------------------------- 

# if it is going to rain today

              if(iwetday(i,j) == 1) {

# calculate average rainfall per wet day

                rainpwd <- xinprec(i,imonth) * ndaypm(imonth) /
max (0.1, xinwet(i,imonth))

# randomly select a daily rainfall amount from a probability density
# function of rainfall

# method i --

# use the following technique from Geng et al. and Richardson
# to distribute rainfall probabilities
# 
# pick a random rainfall amount from a two - parameter gamma function
# distribution function

# estimate two parameters for gamma function (following Geng et al.)
# 
                beta <- max (1, - 2.16 + 1.83 * rainpwd)
                alpha <- rainpwd / beta

# determine daily precipitation amount from gamma distribution function
# (following WGEN code of Richardson and Wright (1984))

                aa <- 1 / alpha 
                ab <- 1 / (1 - alpha)

                tr1 <- exp( - 18.42 / aa)
                tr2 <- exp( - 18.42 / ab)

 12             rn1 <- ran2(seed)
                rn2 <- ran2(seed)

# CD: rewrote parts of prehistoric code in fortran 77 

                if((rn1 - tr1) <= 0) {
                  s1 <- 0
                } else { 
                  s1 <- rn1 ** aa
                }

                if((rn2 - tr2) <= 0) { 
                  s2 <- 0
                } else { 
                  s2 <- rn2 ** ab
                }
           
#               if (rn1 - tr1) 61, 61, 62
# 61            s1 <- 0
#               go to 63
# 62            s1 <- rn1 ** aa

# 63            if (rn2 - tr2) 64, 64, 65
# 64            s2 <- 0
#               go to 66
# 65            s2 <- rn2 ** ab


 66             s12 <- s1 + s2

                if (s12 - 1)  13, 13, 12
 13             z <- s1 / s12

                rn3 <- ran2(seed)

                precipday(i,j) <-  - z  * log(rn3) * beta


# bound daily precipitation to "realistic" range
# 
# lower end is determined by definition of a 'wet day' (at least
# 0.25 mm of total precipitation)

# upper end is to prevent ibis from blowing up

                precipday(i,j) <- max (precipday(i,j),0.25) # min <- 0.25 mm / day
                precipday(i,j) <- min (precipday(i,j),150) # max <- 150 mm / day

# Back to beginning of month loop, this is the end of it

              }

# Add today's precip to the monthly summation


              precipdaysum(i) <- precipdaysum(i) + precipday(i,j)

}

# Adjust daily precip amounts (using precipfac) so that the monthly
# summation equals the input precip amount, when using interannual
# anomalies


              if((precipdaysum(i) == 0) .AND.
(xinprec(i,imonth) > 0)) {
                    rndnum <- 1 + (float(ndaypm(imonth)) - 1) *  
 ran2(seed)

                    iwetday(i,nint(rndnum)) <- 1
                    precipday(i,nint(rndnum)) <- xinprec(i,imonth) *  
 float(ndaypm(imonth))
                    precipdaysum(i) <- precipday(i,nint(rndnum))
                    iwetdaysum(i) <- 1
                 }

                 precipfac <- (xinprec(i,imonth) * float(ndaypm(imonth))) /  
 max(0.01,precipdaysum(i))

                 for(j in 1:ndaypm(imonth)) { 

                    precipday(i,j) <- precipday(i,j) * precipfac

                    if(precipday(i,j) > 140) { 

#                       if (xinwet(i,imonth) < ndaypm(imonth)) then
#                          xinwet(i,imonth) <- xinwet(i,imonth) + 1
#                          pwet <- xinwet(i,imonth) / ndaypm(imonth)
#                          pwd <- 0.75 * pwet
#                          pww <- 0.25 + pwd
#                          print(paste0('WARNING: goto 9000a - SANTIAGO - ', i,j,imonth,iyear,'<-data', int(xinwet(i))
# > ,imonth)), iwetdaysum(i), int(precipday(i,j)),precipfac
#                          goto 9000
#sant                     ------ have to validate it ------
                       if(iwetdaysum(i) <= xinwet(i,imonth)) {
                          pwet <- min(0.95,pwet + ( (xinwet(i,imonth) - iwetdaysum(i)) / ndaypm(imonth) )  )

		        if(xinwet(i,imonth) >= 25) {
                          pwd <- 0.95 * pwet
                          pww <- 0.05 + pwd
		        } else if(xinwet(i,imonth) >= 15 && xinwet(i,imonth) < 25) {
                          pwd <- 0.85 * pwet
                          pww <- 0.15 + pwd
			} else {
                          pwd <- 0.75 * pwet
                          pww <- 0.25 + pwd
			}

                          print(paste0('goto 9000a - ',i,j,imonth,iyear,'<-data', int(xinwet(i))
,imonth)), iwetdaysum(i), int(precipday(i,j)),precipfac,pwd,pww
#			pause
                          goto 9000
                       } else {
                          print(paste0( 'WARNING: goto 9000b', i, int(xinwet(i))
,imonth)), iwetdaysum(i), int(precipday(i,j))
                          goto 9000
                       }
                    }
#    
}
#    
# Verification of the weather generator algorithm

                 iwetdaysum(i) <- 0
                 precipdaysum(i) <- 0

                 for(j in 1:ndaypm(imonth)) { 
                    precipdaysum(i) <- precipdaysum(i) + precipday(i,j)
                    iwetdaysum(i) <- iwetdaysum(i) + iwetday(i,j)
}

                 dif <- precipdaysum(i) - xinprec(i,imonth) *  
 float(ndaypm(imonth))
#    
                 if ((dif <  -0.1) || (dif > 0.1)) print(paste0())
'ERROR in DAILY:', i, precipdaysum(i),
xinprec(i,imonth) * float(ndaypm(imonth)),
iwetdaysum(i), xinwet(i,imonth)
#    
# end of the verification

           }               #end of the iday loop
#    
# Relate today's iwetday and precipday to iwet and precip that will be
# used below

           iwet(i) <- iwetday(i,iday)
           precip(i) <- precipday(i,iday)

#	if(i == 1)write(222, * )precip(i)


		if(iwetdaysum(i) > ndaypm(imonth)) {
		print(paste0('ERROR - iwetdaysum(i) >= ndaypm(imonth)'))
		stop
		}



	if(precip(i) > 140)print(paste0('weather. - need to fix it!',precip(i) ,iwetdaysum(i)))

           precip(i) <- min (precip(i),140) # max <- 150 mm / day

#sant ---------------------------------------------------------------------------------------- 
# *  *  * use CDC data to over - write the (CRU + weather generator) *  * *
#sant ---------------------------------------------------------------------------------------- 
      if(iyear >= cdcyear) {

            if(cdcinprecd(i) >= 0) { 
                precip(i) <- cdcinprecd(i)

#sant, certo !!                  if(cdcinprecd(i) == 0) then
                  if(cdcinprecd(i) < 0.35) {  # teste
      		    iwet(i) <- 0 
		  } else if(cdcinprecd(i) < 0) {
	print(paste0('CDC prec in less than 0'))
	stop
		  } else {
		    iwet(i) <- 1
		  } 		


            } 

      } # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Prec CDC Assimilation *  * *
# ---------------------------------------------------------------------- 
#sant ---------------------------------------------------------------------------------------- 
# *  *  * use station data to over - write the (CRU + weather generator) *  * *
#sant ---------------------------------------------------------------------------------------- 

      if(iyear >= istyear .and .iyear <= istend ) {

            if(stinprecd(i) >= 0) { 
                precip(i) <- stinprecd(i)
               
                  if(stinprecd(i) == 0) {
      		    iwet(i) <- 0 
		  } else {
		    iwet(i) <- 1
		  } 		


            } 

      } # for Station data

#	if(i == 1)print(paste0(iyear,jday,precip(i)))
# ---------------------------------------------------------------------- 
# (3) estimate expected minimum and maximum temperatures
# ---------------------------------------------------------------------- 

# first determine the expected maximum and minimum temperatures
# (from climatological means) for this day of the year

# mean daily mean temperature (K)

          tdm <- xint(i,it1w) +
dt * (xint(i,it2w) - xint(i,it1w)) + 273.16

# mean daily temperature range (K)

          trngm <- xintrng(i,it1w) +
dt * (xintrng(i,it2w) - xintrng(i,it1w))

# mean minimum and maximum temperatures

          tmaxm <- tdm + 0.56 * trngm
          tminm <- tdm - 0.44 * trngm


# modify maximum temperatures for wet and dry days

       if(pwet != 0) {

          if(iyear >= cdcyear && cdcinprecd(i) >= 0) { #csant so assim os valores sao sim., ou Tmax media fica 1 graus mais baixa	
            tmaxd <- tmaxm + pwet * omtmax * trngm # + 1.2
	     tmaxw <- tmaxd - omtmax * trngm
        if(i == 1  && jday == 1)print(paste0('WARNING / CDC,    CHANGE Tmax to match with CRU - at least for SP'))


           } else { #original
        tmaxd <- tmaxm + pwet * omtmax * trngm
	tmaxw <- tmaxd - omtmax * trngm
          if(i == 1  && jday == 1)print(paste0('Original....'))
	    }		 	
       } else {
       tmaxd <- tmaxm # - 2 ! + 3.2
       tmaxw <- tmaxm
       }



# set the 'expected' maximum and minimum temperatures for today

# note that the expected minimum temperatures are the same for
# both wet and dry days

          if (iwet(i) == 0) tmaxe <- tmaxd
          if (iwet(i) == 1) tmaxe <- tmaxw

          tmine <- tminm

# estimate variability in minimum and maximum temperatures

# tmaxs : standard deviation in maximum temperature (K)
# tmins : standard deviation in minimum temperature (K)

# Regression is based on analysis of 2 - m air temperature data from the
# NCEP / NCAR reanalysis (1958 - 1997) for 294 land points over central
# North America (24N - 52N, 130W - 60W, 0.5 - degree resolution): Daily max
# and min temperatures were calculated for each land point from daily
# mean temperature and temperature range. Anomalies were calculated
# by subtracting similar max and min temperatures calculated from
# monthly mean temperature and range (interpolated to daily). The 40
# years of anomalies were then binned by month and the standard
# deviation calculated for each month. The 294 x 12 standard
# deviations were then regressed against the 3528 long - term monthly
# mean temperatures.

# note: the values are bound to be greater than 1 K 
#       (at the very least they must be bound so they don't go below zero)

          tmaxs <- max (1, - 0.0713 * (tdm - 273.16) + 4.89)
          tmins <- max (1, - 0.1280 * (tdm - 273.16) + 5.73)

# ---------------------------------------------------------------------- 
# (4) estimate expected cloud cover
# ---------------------------------------------------------------------- 

# the formulation of dry and wet cloud cover has been
# derived from the weather generator used in the epic crop model

# cloudm : mean cloud cover for today
# cloudd : dry day cloud cover
# cloudw : wet day cloud cover
# cloude : expected cloud cover today

# Verify the data set consistency when using interannual anomalies of
# cloudiness (values under 0 % or over 100 %)

          if(iday == 1) {
             xincld(i,it1w) <- max (0, xincld(i,it1w))
             xincld(i,it1w) <- min (100, xincld(i,it1w))
             xincld(i,it2w) <- max (0, xincld(i,it2w))
             xincld(i,it2w) <- min (100, xincld(i,it2w))
          }

# monthly mean cloud cover (%)

          cloudm <- xincld(i,it1w) +
dt * (xincld(i,it2w) - xincld(i,it1w))
# 
# convert from percent to fraction

          cloudm <- cloudm / 100

# adjust cloud cover depending on dry day / rainy day
# following logic of the EPIC weather generator code

          if(pwet != 0) {
            cloudd <- (cloudm - pwet * omcloud) / (1 - pwet * omcloud)
            cloudd <- min (1, max (0, cloudd))
            cloudw <- (cloudm - (1 - pwet) * cloudd) / pwet
          } else {
            cloudd <- cloudm
            cloudw <- cloudm
          }

          if (iwet(i) == 0) cloude <- cloudd
          if (iwet(i) == 1) cloude <- cloudw

# estimate variability in cloud cover for wet and dry days
# following numbers proposed by Richardson

# clouds : standard deviation of cloud fraction
# 
          if (iwet(i) == 0) clouds <- 0.24 * cloude
          if (iwet(i) == 1) clouds <- 0.48 * cloude

# ---------------------------------------------------------------------- 
# (5) determine today's temperatures and cloud cover using
#     first - order serial autoregressive technique
# ---------------------------------------------------------------------- 

# use the Richardson (1981) weather generator approach to simulate the
# daily values of minimum / maximum temperature and cloud cover

# following the implementation of the Richardson WGEN weather generator
# used in the EPIC crop model

# this approach uses a multivariate generator, which assumes that the
# perturbation of minimum / maximum temperature and cloud cover are
# normally distributed and that the serial correlation of each
# variable may be described by a first - order autoregressive model

# generate standard deviates for weather generator


#	print(paste0(seed,ran2(seed)))

          do j <- 1, 3
 31         rn1 <- ran2(seed)
            rn2 <- ran2(seed)
            v <- sqrt ( - 2 * log(rn1)) * cos(6.283185 * rn2)
            if (abs(v) > 2.5) go to 31
            ee(j) <- v
          }

# zero out vectors

          do j <- 1, 3
            r(j) <- 0
            rr(j) <- 0
          }

# update working vectors

          do j <- 1, 3
            do k <- 1, 3
              r(j) <- r(j) + b(j,k) * ee(j)
              rr(j) <- rr(j) + a(j,k) * xstore(i,k)
            }
          }

# solve for x() perturbation vector and save current vector
# into the xim1() storage vector (saved for each point)
# 
          do j <- 1, 3
            x(j) <- r(j) + rr(j)
            xstore(i,j) <- x(j)
          }

# determine today's minimum and maximum temperature

          tmax(i) <- tmaxe  + tmaxs * x(1)
          tmin(i) <- tmine  + tmins * x(2)


# if tmin > tmax, then switch the two around

          if(tmin(i) > tmax(i)) {
            tdum <- tmax(i)
            tmax(i) <- tmin(i)
            tmin(i) <- tdum
          }

# daily average temperature

          td(i) <- 0.44 * tmax(i) + 0.56 * tmin(i)

	  if(ireccru >= 1) {

#sant	print(paste0('teste para ver se bate!!'))
#	tmax(i) <- xintmxc(i,imonth) + 273.16
#	tmin(i) <- xintmmc(i,imonth) + 273.16
#          td(i) <- 0.44 * tmax(i) + 0.56 * tmin(i)
#san - confere
	  }


# determine today's cloud cover

          cloud(i) <- cloude  + clouds * x(3)

# constrain cloud cover to be between 0 and 100%

          cloud(i) <- max (0, min (1, cloud(i)))

#sant ---------------------------------------------------------------------------------------- 
# *  *  * use station data to over - write the (CRU + weather generator) or (CRU + NCEP) *  * *
#sant ---------------------------------------------------------------------------------------- 

      if(iyear >= istyear .and .iyear <= istend ) {


         if(stintmax(i) >=  -40) tmax(i) <- stintmax(i)

         if(stintmin(i) >=  -40) tmin(i) <- stintmin(i)

         if(stintd(i) >=  -40)     td(i) <- stintd(i)

        if(stincldd(i) >= 0  && stincldd(i) <= 1) 
cloud(i) <- stincldd(i)

# constrain cloud cover to be between 0 and 100%

          cloud(i) <- max (0, min (1, cloud(i)))

	} # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Temperature and Cloud cover Station Assimilation *  * *
# ---------------------------------------------------------------------- 


# ---------------------------------------------------------------------- 
# (6) estimate today's surface atmospheric pressure
# ---------------------------------------------------------------------- 

# simply a function of the daily average temperature and topographic
# height -- nothing fancy here


          psurf(i) <- 101325 *
(td(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork
#sant - use the elevation of the station (enven tough, it doesnt have almost any impact on the results)
#sant          psurf(i) <- 101325 *
#sant > (td(i) / (td(i) + 0.0065 * 552)) ** rwork


# ---------------------------------------------------------------------- 
# (7) estimate today's relative humidity
# ---------------------------------------------------------------------- 

# the formulation of dry and wet relative humidities has been
# derived from the weather generator used in the epic crop model

# qdm : mean relative humidity 
# qdd : dry day relative humidity
# qdw : rainy day relative humidity
# qde : expected relative humidity (based on wet / dry decision)

# Verify the data set consistency when using interannual anomalies of
# relative humidity (values over 100 % or under 0 %)

          if(iday == 1) {
             xinq(i,it1w) <- max (0, xinq(i,it1w))
             xinq(i,it1w) <- min (100, xinq(i,it1w))
             xinq(i,it2w) <- max (0, xinq(i,it2w))
             xinq(i,it2w) <- min (100, xinq(i,it2w))
          }

# mean relative humidity (%)

          qdm <- xinq(i,it1w) + dt * (xinq(i,it2w) - xinq(i,it1w))
# 
# convert from percent to fraction

          qdm <- qdm / 100

# adjust humidity depending on dry day / rainy day
# following logic of the EPIC weather generator code

          if(pwet != 0) {
            qdd <- (qdm - pwet * omqd) / (1 - pwet * omqd)
            if(qdd < 0.2) {
              qdd <- 0.2
              if (qdd > qdm) qdm <- qdd
            }
            qdd <- min(1, qdd)
            qdw <- (qdm - (1 - pwet) * qdd) / pwet
          } else {
            qdd <- qdm
            qdw <- qdm
          }

          if (iwet(i) == 0) qde <- qdd
          if (iwet(i) == 1) qde <- qdw 

# estimate lower and upper bounds of humidity distribution function
# following logic of the EPIC weather generator code

          qdup <- qde  + (1 - qde) * exp (qde  - 1)
          qdlow <- qde  * (1 - exp ( - qde))

# randomly select humidity from triangular distribution function
# following logic of the EPIC weather generator code

          rn <- ran2(seed)

          y <- 2 / (qdup - qdlow)

          b3 <- qde  - qdlow
          b2 <- qdup - qde
          b1 <- rn / y

          x1 <- y * b3 / 2 

          if(rn > x1) {
            qd(i) <- qdup - sqrt (b2 * b2 - 2 * b2 * (b1 - 0.5 * b3))
          } else {
            qd(i) <- qdlow + sqrt (2 * b1 * b3)
          }

# adjust daily humidity to conserve monthly mean values

# note that this adjustment sometimes gives rise to humidity
# values greater than 1 -- which is corrected below

          amn <- (qdup + qde  + qdlow) / 3
          qd(i) <- qd(i) * qde  / amn

# constrain daily average relative humidity

          qd(i) <- max (0.30, qd(i))
          qd(i) <- min (0.99, qd(i))

# convert from relative humidity to specific humidity at
# daily mean temperature

          qd(i) <- qd(i) * qsat(esat(real(td(i))), real(psurf(i)))



#sant ---------------------------------------------------------------------------------------- 
# *  *  * use station data to over - write the (CRU + weather generator) or (CRU + NCEP) *  * *
#sant ---------------------------------------------------------------------------------------- 


      if(iyear >= istyear .and .iyear <= istend ) {

# by now, daily surface atmospheric pressure is function of the daily average temperature and topographic

       if(stintd(i) >=  -40) 
psurf(i) <- 101325 *
# > (stintd(i) / (td(i) + 0.0065 * 450?elev. metros?)) ** rwork
(stintd(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork

#sant - Talk  with Chris if is best to do this or follow the NCEP assimilation process
# daily relative humidity


# mean relative humidity (%)

      if(stinqd(i) >= 0) {

      qdm <- stinqd(i)

# convert from percent to fraction
          qdm <- qdm / 100

# constrain daily average relative humidity

          qd(i) <- max (0.30, qdm)
          qd(i) <- min (0.99, qdm)

# convert from relative humidity to specific humidity 

          qd(i) <- qd(i) * qsat(esat(real(stintd(i))), real(psurf(i)))

   	   }

	} # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Relative humidity Station Assimilation *  * *
# ---------------------------------------------------------------------- 


# ---------------------------------------------------------------------- 
# (8) estimate today's daily average wind speed
# ---------------------------------------------------------------------- 

# first estimate the expected daily average wind speed (from monthly
# means)

          eud <- xinwind(i,it1w) +
dt * (xinwind(i,it2w) - xinwind(i,it1w))

# following logic of the EPIC weather generator
# select random wind speed following this equation

          ud(i) <- 1.13989 * eud * (-log(ran2(seed))) ** 0.30 

# TET change 10 / 29 / 02
# Input windspeed is assumed to be measured at canopy height
# (ztop(i,2))
# IBIS now has siga <- 0.991 which is ~93m height.
# Adjust windspeed according to log - wind profile.
# So let
# displacement height, d <- 0.65(ztop(i,2))
# roughness length, zm <- 0.1(ztop(i,2))
# Equation found in Bonan (2002) and other texts:
# u(z2) - u(z1) <- (frictionvelocity / k) * log((z2 - d) / (z1 - d))
# Use log - wind to solve for frictionvelocity and substitute
# into above equation:
# u(z2) <- u(z1) * (1 + (log((z2 - d) / (z1 - d))) / (log((z1 - d) / zm)))
# Use canopyheight <- z1 <- ztop(i,2), and z2 <- 93m
# and substitute d and zm to get equation dependent on canopy height:
#sant - change z2 <- 93m to z2 <- za(i) 
#	if(i == 1)print(paste0(za(i),ztop(i,2),ztop(i,1)))
#sant - this esquation is doesnt fit to ztop(i,1), solve the equat above in the near future to get the wind write!!!!!

#          ud(i) <- ud(i) * (1 + 0.79824 * log((93 - 0.65 * ztop(i,2))/
           ud(i) <- ud(i) * (1 + 0.79824 * log((za(i) - 0.65 * ztop(i,2))/* 
                   (0.35 * ztop(i,2))))

#	print(paste0(i, ud(i),za(i),ztop(i,2),'ireccru ', ireccru))

# Decided to use a canopy height of 20m over US after finding
# references of mature forest height between 10m and 35m (we are
# trying to simulate forests before human interference)
# with ztop(i,2) <- 20m, the input windspeed is adjusted by a
# factor of 3.21

# TET changed constraint since it will be above 10 often

# constrain daily wind speeds to be between 2.5 and 50 m / sec

#          ud(i) <- max (2.5, min (10, ud(i)))
          ud(i) <- max (2.5, min (50, ud(i)))



#sant ---------------------------------------------------------------------------------------- 
# *  *  * use station data to over - write the (CRU + weather generator) or (CRU + NCEP) *  * *
#sant ---------------------------------------------------------------------------------------- 

      if(iyear >= istyear .and .iyear <= istend ) {


# daily average wind speed

          if(stinwindd(i) >= 0) {
 
          ud(i) <- stinwindd(i)

	  } # for wind

       } # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Wind Station Assimilation *  * *
# ---------------------------------------------------------------------- 


        } else {


# --------------------------------------------------------------------------------------- 
# *  *  * Disaggregated the CRU monthly value to daily value using NCEP daily anomaly *  * *
# --------------------------------------------------------------------------------------- 


# use basic daily climate data, converting units

# daily total precipitation

# Here we multiply xinprecd, the daily fraction of precip calculated from
# the NCEP dataset, by xinprec, the total monthly amount of precip taken from
# the CRU05 dataset to obtain our derived daily precip amount. Also do a check
# to see if the daily precip exceeds 360mm (as is done in the daily weather 
# generator) ... no correction is made, only a warning is printed

          precip(i) <- (xinprec(i,imonth) * ndaypm(imonth)) * xinprecd(i)

#          if (precip(i) > 360) then
#            print(paste0( 'WARNING: daily precip exceeds 360mm for'))
#            print(paste0( 'year, month, day, gridcell <- '))
#            print(paste0( iyear, imonth, iday, i))
#          }

# daily average temperatures

# Here we add the NCEP temperature anomaly to the CRU05 monthly anomaly
# The trange NCEP anomaly must also be multiplied by the climatological
# CRU05 trange in order to determine tmax and tmin

          td(i) <- xint(i,imonth) + 273.16 + xintd(i)
          trngm <- min (44, (xintrng(i,imonth) * xintrngd(i)))

          tmax(i) <- td(i) + 0.56 * trngm
          tmin(i) <- td(i) - 0.44 * trngm

#         tmax(i) <- xintmax(i) + 273.16 
#         tmin(i) <- xintmin(i) + 273.16 

# daily average cloud cover

# Here we add the NCEP cloud anomaly to the monthly anomaly from CRU05
# before converting percentage of cover to fraction
# We also bound cloud cover fraction between 0 and 1

          cloud(i) <- (xincld(i,imonth) + xincldd(i)) * 0.01

          cloud(i) <- min (cloud(i), 1)
          cloud(i) <- max (0, cloud(i))

# compute surface atmospheric pressure

          psurf(i) <- 101325 *
(td(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork

# daily average specific humidity

# First we must convert relative humidity to a fraction and then convert
# the fraction to monthly mean specific humidity (based on the monthly
# mean temperature)

          humidfrac <- xinq(i,imonth) / 100
          sphumid <- humidfrac * qsat(esat(273.16 + real(xint(i,imonth))),real(psurf(i)))

# Correct for covariances between relative humidity and temperature,
# so that one has the "true" monthly mean specific humidity (i.e., as
# would be calculated from the hourly specific humidity, rather than
# based on the monthly mean temperature). Correction factors for each
# month are based on data from the Trout Lake region of northern
# Wisconsin (for 1996 - 2000), where qcov <-  - 1  + <RH * qs(Ta)> / <RH> * qs < Ta > .
          qcov(1) <- 0.194
          qcov(2) <- 0.142
          qcov(3) <- 0.102
          qcov(4) <-  - 0.026
          qcov(5) <-  - 0.017
          qcov(6) <- 0.005
          qcov(7) <-  - 0.006
          qcov(8) <-  - 0.006
          qcov(9) <- 0.017
          qcov(10) <- 0.046
          qcov(11) <- 0.052
          qcov(12) <- 0.118
          sphumid <- sphumid * (1 + qcov(imonth))

# Convert monthly mean specific humidity to daily mean using NCEP
# daily anomalies
          qd(i) <- sphumid * xinqd(i)

# daily average wind speed

# Here we multiply the NCEP fraction of windspeed by the CRU05
# climatological monthly windspeed

          ud(i) <- xinwind(i,imonth) * xinwindd(i)


# TET change 10 / 29 / 02
# Input windspeed is assumed to be measured at canopy height
# (ztop(i,2))
# IBIS now has siga <- 0.991 which is ~93m height.
# Adjust windspeed according to log - wind profile.
# So let
# displacement height, d <- 0.65(ztop(i,2))
# roughness length, zm <- 0.1(ztop(i,2))
# Equation found in Bonan (2002) and other texts:
# u(z2) - u(z1) <- (frictionvelocity / k) * log((z2 - d) / (z1 - d))
# Use log - wind to solve for frictionvelocity and substitute
# into above equation:
# u(z2) <- u(z1) * (1 + (log((z2 - d) / (z1 - d))) / (log((z1 - d) / zm)))
# Use canopyheight <- z1 <- ztop(i,2), and z2 <- 93m
# and substitute d and zm to get equation dependent on canopy height:
#sant - change z2 <- 93m to z2 <- za(i)
#sant	if(i == 1)print(paste0(za(i),ztop(i,2)))
#          ud(i) <- ud(i) * (1 + 0.79824 * log((93 - 0.65 * ztop(i,2))/
           ud(i) <- ud(i) * (1 + 0.79824 * log((za(i) - 0.65 * ztop(i,2))/* 
                   (0.35 * ztop(i,2))))

# Decided to use a canopy height of 20m over US after finding
# references of mature forest height between 10m and 35m (we are
# trying to simulate forests before human interference)
# with ztop(i,2) <- 20m, the input windspeed is adjusted by a
# factor of 3.21


#sant ---------------------------------------------------------------------------------------- 
# *  *  * use station data to over - write the (CRU + weather generator) or (CRU + NCEP) *  * *
#sant ---------------------------------------------------------------------------------------- 


      if(iyear >= istyear .and .iyear <= istend ) {

      if(stinprecd(i) >= 0) precip(i) <- stinprecd(i)


      if(stintmax(i) >=  -40) tmax(i) <- stintmax(i)
      if(stintmin(i) >=  -40) tmin(i) <- stintmin(i)
      if(stintd(i) >=  -40) td(i) <- stintd(i)


      if(stincldd(i) >= 0  && stincldd(i) <= 1) 
cloud(i) <- stincldd(i)

# daily surface atmospheric pressure
# simply a function of the daily average temperature and topographic
# height -- nothing fancy here

      if(stintd(i) >=  -40) 
psurf(i) <- 101325 *
# > (stintd(i) / (td(i) + 0.0065 * 450?elev. metros?)) ** rwork
(stintd(i) / (td(i) + 0.0065 * xintopo(i))) ** rwork


# mean relative humidity (%)

      if(stinqd(i) >= 0) {

      qdm <- stinqd(i)

# convert from percent to fraction
          qdm <- qdm / 100

# constrain daily average relative humidity
          qd(i) <- max (0.30, qdm)
          qd(i) <- min (0.99, qdm)

# convert from relative humidity to specific humidity at
# daily mean temperature
          qd(i) <- qd(i) * qsat(esat(real(stintd(i))), real(psurf(i)))

	}

# daily average wind speed

       if(stinwindd(i) >= 0) ud(i) <- stinwindd(i)


	} # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Station Assimilation *  * *
# ---------------------------------------------------------------------- 
#sant ---------------------------------------------------------------------------------------- 
# *  *  * use CDC data to over - write the (CRU + weather generator) or (CRU + NCEP) *  * *
#sant ---------------------------------------------------------------------------------------- 
      if(iyear >= cdcyear) {

            if(cdcinprecd(i) >= 0) { 
                precip(i) <- cdcinprecd(i)
            } 

      } # for Station data
# ---------------------------------------------------------------------- 
# *  *  * End of Prec CDC Assimilation *  * *
# ---------------------------------------------------------------------- 





        } #general if for data



#	if(i == 1)print(paste0(iyear,jday,precip(i),td(i),	))
# > tmax(i), tmin(i),cloud(i),qd(i)

#sant - make sure that the station value will be used only if read a new value
	stinprecd(i) <-  - 999
	stintd(i) <-  - 999
	stintmax(i) <-  - 999
	stintmin(i) <-  - 999
	stincldd(i) <-  - 999
	stinqd(i) <-  - 999
	stinwindd(i) <-  - 999
 	cdcinprecd(i) <-  - 999


# ---------------------------------------------------------------------- 
# *  *  * other daily climate calculations *  * *
# ---------------------------------------------------------------------- 

# calculated temperature extremes -- for vegetation limits (deg c)

# for this purpose, use the 10 - day running mean temperature

        tcthis(i) <- min (tcthis(i), (a10td(i) - 273.16))
        twthis(i) <- max (twthis(i), (a10td(i) - 273.16))

# update this year's growing degree days


        gdd0this(i) <- gdd0this(i) + max (0 ,(td(i) - 273.16)) 
        gdd0cthis(i) <- gdd0cthis(i) + max (0 ,(td(i) - baset(15)))    # wheat
        gdd5this(i) <- gdd5this(i) + max (0 ,(td(i) - 278.16))
        gdd8this(i) <- gdd8this(i) + max (0 ,(td(i) - baset(14)))    # maize 
        gdd10this(i) <- gdd10this(i) + max (0 ,(td(i) - baset(13)))    # soybean
        gdd12this(i) <- gdd12this(i) + max (0 ,(td(i) - baset(16)))    # sugarcane 


# form calculations of number of growing degree days between frost events

# events (e.g., tmin(i) <=  -2.2 C) this is applicable to CRU data
# differences exist when using a combination of CRU / NCEP  
# - 2.2 used as a threshold from Schwartz and Reiter, 2000  International
# Journal of Climatology, 20: 929 - 932  Changes in North American Spring 
#       
#sant - this relationship were set for US conditions


	if(gdddy == 1) { #!!!update GDD (is not working for SA)

        if(tmin(i) >= 273.16) {
          consdays(i) <- consdays(i) + 1
          maxcons(i) <- max(maxcons(i), consdays(i))
          if(maxcons(i) == consdays(i)) {

            iniday(i) <- cdays(i) + 1  - maxcons(i)
          }

          daygddc(i,cdays(i)) <- min(30,max(0, (td(i) - baset(14)))) 
          daygdds(i,cdays(i)) <- min(30,max(0, (td(i) - baset(13)))) 
          daygddw(i,cdays(i)) <- min(30,max(0, (td(i) - baset(15)))) 
        daygddsgc(i,cdays(i)) <- min(30,max(0, (td(i) - baset(16)))) 

        } else {
          consdays(i) <- 0
        }

#	if(i == 1)
# > print(paste0(cdays(i),td(i)))

         if(cdays(i) == 365) {

# calculate total number of growing season GDD and number
# of days long

             if(iniday(i) == 9999) {
                 iniday(i) <- cdays (i)
                 maxcons(i) <- 1
	     } else if(iniday(i) == 0) {
	     iniday(i) <- 1
             }      
#sant - for what? endday
             endday(i) <- iniday(i) + maxcons(i) - 1

          for(k in iniday(i): endday(i)) { 

             gddfzcorn(i) <- gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i) <- gddfzsoy(i) + daygdds(i,k)
#sant - didnt see use            gddfzwht(i) <- gddfzwht(i) + daygddw(i,k)
             gddfzsgc(i) <- gddfzsgc(i) + daygddsgc(i,k)
             gsdays(i) <- gsdays(i) + 1 
}


	print(paste0('INTO WEATHER.F ok',gddfzsgc(i) ))

#        if (pcd(16) == 1  && pcm(16) == 1) then

#           gddcorn(i,iyear + 1) <- gddfzcorn(i) 
#          gddsoy(i,iyear + 1) <- gddfzsoy(i) 
#sant - didnt see use        gddwht(i,iyear) <- gddfzwht(i) 
#           gddsgc(i,iyear + 1) <- gddfzsgc(i)
#	print(paste0('365_1',iyear,iyear + 1,jday,cdays(i),gddsgc(i,iyear + 1)))

#        elseif (pcd(16) == 2  && pcm(16) == 1  && jday == 366) then
#           gddcorn(i,iyear + 1) <- gddfzcorn(i) 
#           gddsoy(i,iyear + 1) <- gddfzsoy(i) 
#sant - didnt see use        gddwht(i,iyear) <- gddfzwht(i) 
#          gddsgc(i,iyear + 1) <- gddfzsgc(i)

#	print(paste0('365',iyear,iyear + 1,jday,cdays(i),gddsgc(i,iyear + 1)))

#	} else {
#           gddcorn(i,iyear) <- gddfzcorn(i) 
#           gddsoy(i,iyear) <- gddfzsoy(i) 
#sant - didnt see use        gddwht(i,iyear) <- gddfzwht(i) 
#           gddsgc(i,iyear) <- gddfzsgc(i)

#	print(paste0('365',iyear,iyear,jday,cdays(i),gddsgc(i,iyear)))

#	}


        } else if(cdays(i) == 366) {

             if(iniday(i) == 0) {
                 iniday(i) <- 1
             }      

             endday(i) <- iniday(i) + maxcons(i) - 1

             gddfzcorn(i) <- 0
             gddfzsoy(i) <- 0
             gddfzsgc(i) <- 0
             gsdays(i) <- 0


          do  k <- iniday(i), endday(i)
             gddfzcorn(i) <- gddfzcorn(i) + daygddc(i,k)
             gddfzsoy(i) <- gddfzsoy(i) + daygdds(i,k)
             gddfzsgc(i) <- gddfzsgc(i) + daygddsgc(i,k)
             gsdays(i) <- gsdays(i) + 1 
         }
#sant - I changed here for the beginning of the year for simplicty, but both give the same result
#            if (pcd(16) == 1  && pcm(16) == 1) then

#           gddcorn(i,iyear + 1) <- gddfzcorn(i) 
#           gddsoy(i,iyear + 1) <- gddfzsoy(i) 
#sant - didnt see use        gddwht(i,iyear) <- gddfzwht(i) 
#           gddsgc(i,iyear + 1) <- gddfzsgc(i)
#	print(paste0('366_1',iyear,iyear + 1,jday,cdays(i),gddsgc(i,iyear + 1)))
#	    } else {
#           gddcorn(i,iyear) <- gddfzcorn(i) 
#           gddsoy(i,iyear) <- gddfzsoy(i) 
#sant - didnt see use        gddwht(i,iyear) <- gddfzwht(i) 
#           gddsgc(i,iyear) <- gddfzsgc(i)

#	print(paste0('366',iyear,jday,cdays(i),gddsgc(i,iyear)))
#            }

        }

	} #for South America

# accumulate growing degree days for planted crops past planting

        for(j in scpft: ecpft ) { 

# for crops except winter wheat
# be careful with rotations
# 
       if(croplive(i,j) == 1 && ((j == 13 || j  == 14 || j  == 16) ||  
     (iwheat == 1  && j  == 15))) {

             gddplant(i,j) <- gddplant(i,j) + max(0, min(td(i) -  
 baset(j), mxtmp(j)))

             gddtsoi(i,j) <- gddtsoi(i,j) + max(0, min(tsoi(i,1) -  
 baset(j), mxtmp(j)))

# if winter wheat is planted, reduce thermal time accumulation
# by vernalization factor (calculated in crops.f) until crop
# is fully vernalized (cold - hardened)

          } else if(croplive(i,j) == 1 && j  == 15 &&  
iwheat == 2) {

             gddplant(i,j) <- gddplant(i,j) + vf(i) * max(0, min(td(i) -  
 baset(j), mxtmp(j)))

             gddtsoi(i,j) <- gddtsoi(i,j) + vf(i) * max(0, min(tsoi(i,1) -  
 baset(j), mxtmp(j)))

        }

#sant - nao encontrei a solucao no momento, futuramente alterar essa funcao, que pode ser importante para rodadas com alteracoes climaticas.

#          do  k <- iniday(i), endday(i)
#             gddfzcorn(i) <- gddfzcorn(i) + daygddc(i,k)
#             gddfzsoy(i) <- gddfzsoy(i) + daygdds(i,k)
#             gddfzsgc(i) <- gddfzsgc(i) + daygddsgc(i,k)
#             gsdays(i) <- gsdays(i) + 1 
#         }


	if(j == 13 || j  == 14) {

	  ik(i,j) <- ik(i,j) + 1
	 if(idpp(i,j) == 1) {
	 ik(i,j) <- 1
	}

       if(ik(i,j) <= (mxmat(j) - 15)) {

             gddfzcorn(i) <- gddfzcorn(i) + min(30,max(0, (td(i) - baset(j))))
             gddfzsoy(i) <- gddfzsoy(i) + min(30,max(0, (td(i) - baset(j))))

	}


	}
	


	if(j == 16) {

	  ik(i,j) <- ik(i,j) + 1
	 if(cropy(i) == 1  && idpp(i,j) == 1) {
	 print(paste0('GDD_start  SUGARCANE',i,iyear,jday))
         gddpl15(i) <- 0	
	 ik(i,j) <- 1
	}

       if(ik(i,j) <= (mxmat(16) - 30)) {
       gddpl15(i) <- gddpl15(i) + max(0, min(td(i) - baset(j), mxtmp(j)))
	}

       if(ik(i,j) == (mxmat(16) - 30)) {
	print(paste0('GDD expected for planting, before',i,iyear,jday,gddsgcp(i,1),gddpl15(i),mxmat(16) - 30	))
       gddsgcp(i,1) <- (gddsgcp(i,1) + gddpl15(i)) / 2
	print(paste0('GDD expected for planting, updated',i,iyear,jday,gddsgcp(i,1)	))
	}

	}
	


}

}
      

      

# return to main program

return()
}



# ---------------------------------------------------------------------
      const <- function (arr, nar, value) {
# ---------------------------------------------------------------------

# sets all elements of double precision vector arr to value

      implicit none

# Arguments

      integer nar
#     
      double precision value
      double precision arr(nar)

# Local variables

      integer j

      for(j in 1: nar) { 
        arr(j) <- value
}

return()
}

# ---------------------------------------------------------------------
      real function cvmgt (x,y,l)
# ---------------------------------------------------------------------

# chooses between two things.  Used in canopy.f

      implicit none

      logical l
       double precision x, y

      if(l) {
        cvmgt <- x
      } else {
        cvmgt <- y
      }

return()
}


# --------------------------------------------------------------------
      real function ran2 (idum)
# --------------------------------------------------------------------

      implicit none

      integer idum,im1,im2,imm1,ia1,ia2,iq1,iq2,ir1,ir2,ntab,ndiv

      real am, eps, rnmx

      parameter (im1 <- 2147483563, 
im2 <- 2147483399,
am <- 1 / im1,
imm1 <- im1 - 1,
ia1 <- 40014,
ia2 <- 40692,
iq1 <- 53668,
iq2 <- 52774, 
ir1 <- 12211,
ir2 <- 3791,
ntab <- 32,
ndiv <- 1 + imm1 / ntab,
eps <- 1e-7,
rnmx <- 1 - eps)

      integer idum2,j,k,iv(ntab),iy

      save iv,iy,idum2

      data idum2 / 123456789 / , iv / ntab * 0/, iy / 0/

      if(idum <= 0) {
        idum <- max( - idum,1)
        idum2 <- idum
        for(j in ntab + 8:1, - 1) { 
          k <- idum / iq1
          idum <- ia1 * (idum - k*iq1) - k*ir1
          if (idum < 0) idum <- idum + im1
          if (j <= ntab) iv(j) <- idum
}
        iy <- iv(1)
      }

      k <- idum / iq1
      idum <- ia1 * (idum - k*iq1) - k*ir1
      if (idum < 0) idum <- idum + im1

      k <- idum2 / iq2
      idum2 <- ia2 * (idum2 - k*iq2) - k*ir2
      if (idum2 < 0) idum2 <- idum2 + im2

      j <- 1 + iy / ndiv
      iy <- iv(j) - idum2
      iv(j) <- idum
      if(iy < 1)iy <- iy + imm1

      ran2 <- min(am * iy,rnmx)

return()
}

