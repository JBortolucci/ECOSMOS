c
c  ####    ####      #    #
c #       #    #     #    #
c  ####   #    #     #    #
c      #  #    #     #    #
c #    #  #    #     #    #
c  ####    ####      #    ######
c
c ---------------------------------------------------------------------

      subroutine setsoiWrapper(dim_params, double_params,
     >                         npoi_matrix,                      
     >                         consoi,
     >                         fracclay,
     >                         fracsand,
     >                         fracsilt,
     >                         poros,
     >                         qglif,
     >                         tsno,
     >                         tsoi,
     >                         wisoi,
     >                         wsoi)

                  implicit none

                  integer dim_params(3)

                  double precision double_params(9)

                  double precision npoi_matrix(5, dim_params(1))

                  double precision npoi_line1(dim_params(1))
                  double precision npoi_line2(dim_params(1))
                  double precision npoi_line3(dim_params(1))
                  double precision npoi_line4(dim_params(1))
                  double precision npoi_line5(dim_params(1))

                  double precision consoi(dim_params(1),dim_params(2))
                  double precision fracclay(dim_params(1),dim_params(2))
                  double precision fracsand(dim_params(1),dim_params(2))
                  double precision fracsilt(dim_params(1),dim_params(2))
                  double precision poros(dim_params(1),dim_params(2))
                  double precision qglif(dim_params(1),4)
                  double precision tsno(dim_params(1),dim_params(3))
                  double precision tsoi(dim_params(1),dim_params(2)) 
                  double precision wisoi(dim_params(1),dim_params(2))
                  double precision wsoi(dim_params(1),dim_params(2))

                  integer i

c 	  Inicializando vetores double de tamanho npoi
                DO 102, i = 1, dim_params(1), 1
                        npoi_line1(i) = npoi_matrix(1, i)
                        npoi_line2(i) = npoi_matrix(2, i)
                        npoi_line3(i) = npoi_matrix(3, i)
                        npoi_line4(i) = npoi_matrix(4, i)
                        npoi_line5(i) = npoi_matrix(5, i)
102             CONTINUE

                call setsoi(dim_params(1), dim_params(2), dim_params(3),
     >                      double_params(1), double_params(2), double_params(3),
     >                      double_params(4), double_params(5), double_params(6),
     >                      double_params(7), double_params(8), double_params(9),
     >                      npoi_line1, npoi_line2, npoi_line3, npoi_line4,
     >                      npoi_line5,
     >                      consoi,
     >                      fracclay,
     >                      fracsand,
     >                      fracsilt,
     >                      poros,
     >                      qglif,
     >                      tsno,
     >                      tsoi,
     >                      wisoi,
     >                      wsoi)

c 	 RETORNANDO VALORES
                DO 888, i = 1, dim_params(1), 1
                        npoi_matrix(1, i) = npoi_line1(i)
                        npoi_matrix(2, i) = npoi_line2(i)
                        npoi_matrix(3, i) = npoi_line3(i)
                        npoi_matrix(4, i) = npoi_line4(i)
                        npoi_matrix(5, i) = npoi_line5(i)
888             CONTINUE

      end

c JAIR: rotina1 - ok

      subroutine setsoi(npoi,
     >                  nsoilay,
     >                  nsnolay,
     >                  epsilon,
     >                  wpudmax,
     >                  zwpmax,
     >                  tmelt,
     >                  hvap, 
     >                  hsub,
     >                  cvap,
     >                  ch2o,
     >                  cice,
     >                  hvasug,  
     >                  hvasui,
     >                  ta,
     >                  wipud,
     >                  wpud,
     >                  consoi,
     >                  fracclay,
     >                  fracsand,
     >                  fracsilt,
     >                  poros,
     >                  qglif,
     >                  tsno,
     >                  tsoi,
     >                  wisoi,
     >                  wsoi)
c ---------------------------------------------------------------------
c
c sets diagnostic soil quantities
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c
c Local variables
c
      integer i, k,           ! loop indices
     >        msand,          ! % of sand in grid point
     >        mclay           ! % of clay in grid point
c
      double precision fsand,             ! fraction of sand in grid point
     >     fsilt,             ! fraction of silt in grid point
     >     fclay,             ! fraction of clay in grid point
     >     forganic,          ! fraction of organic soil in grid point 
     >     powliq,            ! liquid water content in fraction of soil depth
     >     powice,            ! ice water content in fraction of soil depth
     >     zcondry,           ! dry-soil conductivity
     >     zvap,              ! latent heat of vaporisation at soil temp
     >     zsub,              ! latent heat of sublimation at soil temp
     >     zwpud,             ! fraction of soil surface covered by puddle
     >     zwsoi,             ! volumetric water content of top soil layer 
     >     rwork1,
     >     rwork2

c GLOBAL VARIABLES

      integer          npoi
      integer          nsoilay
      integer          nsnolay

      double precision epsilon
      double precision wpudmax
      double precision zwpmax
      double precision tmelt
      double precision hvap 
      double precision hsub
      double precision cvap
      double precision ch2o
      double precision cice

      double precision hvasug(npoi)
      double precision hvasui(npoi)
      double precision ta(npoi)
      double precision wipud(npoi)
      double precision wpud(npoi)

      double precision consoi(npoi,nsoilay)
      double precision fracclay(npoi,nsoilay)
      double precision fracsand(npoi,nsoilay)
      double precision fracsilt(npoi,nsoilay)
      double precision poros(npoi,nsoilay)
      double precision qglif(npoi,4)
      double precision tsno(npoi,nsnolay)
      double precision tsoi(npoi,nsoilay) 
      double precision wisoi(npoi,nsoilay)
      double precision wsoi(npoi,nsoilay)

      double precision t, tair
      double precision hvapf, hsubf

      hvapf(t,tair) = hvap + cvap*(tair-273.16) - ch2o*(t-273.16)
      hsubf(t,tair) = hsub + cvap*(tair-273.16) - cice*(t-273.16)

c
c     include 'comsat.h'    
c
c set soil layer quantities
c
      do 100 k = 1, nsoilay
c
        do 110 i = 1, npoi
c
c Convert input sand and clay percents to fractions
c
c Change by TET
c          if (k.le.4) then
c            msand = nint(sand(i,k))
c            mclay = nint(clay(i,k))
c
c          else
c            msand = nint(sand(i,4))
c            mclay = nint(clay(i,4))
c          endif
c 
c          fsand = 0.01 * msand
c          fclay = 0.01 * mclay
c          fsilt = 0.01 * (100 - msand - mclay)
c
c update thermal conductivity (w m-1 k-1)
c
c based on c = c1**v1 * c2**v2 * c3**v3 * c4**v4 where c1,c2..
c are conductivities of soil grains, air, liquid and ice
c respectively, and v1,v2... are their volume fractions 
c (so v1 = 1-p where p is the porosity, and v1+v2+v3+v4 = 1).
c then condry = c1**(1-p) * c2**p  is the dry-soil
c conductivity, and c = condry * (c3/c2)**v3 * (c4/c2)**v4, 
c where c2 = conductivity of air = .025 w m-1 k-1.
c however this formula agrees better with williams+smith
c table 4 for wet (unfrozen) sand and clay if c2 is decreased
c to ~.005. (for peat in next section, ok if c2 = .025).
c also see lachenbruch etal,1982,jgr,87,9301 and refs therein.
c

          powliq = poros(i,k) * wsoi(i,k) * (1. - wisoi(i,k))
          powice = poros(i,k) * wisoi(i,k)
c
c          zcondry = fsand * 0.300 +
c     >              fsilt * 0.265 +
c     >              fclay * 0.250
c
          zcondry = fracsand(i,k) * 0.300 +
     >              fracsilt(i,k) * 0.265 +
     >              fracclay(i,k) * 0.250 ! + 
c    >              forganic * 0.026      ! for future use CJK
c
          consoi(i,k) = zcondry * ((0.56*100.)**powliq)
     >                          * ((2.24*100.)**powice)
c
 110    continue
c
 100  continue
c
c set qglif - the fraction of soil sfc evaporation from soil liquid,
c soil ice, puddle liquid, and puddle ice (relative to total sfc evap)
c
c zwpud:   fraction of surface area covered by puddle (range: 0 - zwpmax)
c zwpmax:  maximum value of zwpud (currently assumed to be 0.5)
c 1-zwpud: fraction of surface area covered by soil (range: (1-zwpmax) - 1.0)
c zwsoi:   volumetric water content of top soil layer (range: 0 - 1.0)
c
c qglif(i,1): fraction of soil evap (fvapg) from soil liquid
c qglif(i,2): fraction of soil evap (fvapg) from soil ice
c qglif(i,3): fraction of soil evap (fvapg) from puddle liquid
c qglif(i,4): fraction of soil evap (fvapg) from puddle ice
c
      do 200 i = 1, npoi
c
*       zwpmax = 0.5
        zwpud = max (0.0, min (zwpmax, zwpmax*(wpud(i)+wipud(i))/wpudmax) )
        zwsoi = (1. - wisoi(i,1)) * wsoi(i,1) + wisoi(i,1)
c
        if (zwsoi.ge.epsilon) then
c
          rwork1 = 1./zwsoi
c
          if (zwpud.ge.epsilon) then
            rwork2 = 1./(wpud(i) + wipud(i))
            qglif(i,1) = (1. - zwpud) * (1. - wisoi(i,1)) * wsoi(i,1) * rwork1
            qglif(i,2) = (1. - zwpud) * wisoi(i,1) * rwork1
            qglif(i,3) = zwpud * wpud(i) * rwork2
            qglif(i,4) = zwpud * wipud(i) * rwork2
          else
            qglif(i,1) = (1. - wisoi(i,1)) * wsoi(i,1) * rwork1
            qglif(i,2) = wisoi(i,1) * rwork1
            qglif(i,3) = 0.0
            qglif(i,4) = 0.0
          endif
c
        else
c
c for a 100% dry soil surface, assign all soil evap to the puddles.
c Note that for small puddle sizes, this could lead to negative
c puddle depths. However, for a 100% dry soil with small puddles,
c evaporation is likely to be very small or less than zero
c (condensation), so negative puddle depths are not likely to occur.
c
          if (zwpud.ge.epsilon) then
            rwork2 = 1./(wpud(i) + wipud(i))
            qglif(i,1) = 0.0
            qglif(i,2) = 0.0
            qglif(i,3) = zwpud * wpud(i) * rwork2
            qglif(i,4) = zwpud * wipud(i) * rwork2
          else
            if (tsoi(i,1).ge.tmelt) then
c
c above freezing
c
              qglif(i,1) = 0.
              qglif(i,2) = 0.
              qglif(i,3) = 1.
              qglif(i,4) = 0.
c
            else
c
c below freezing
c
              qglif(i,1) = 0.
              qglif(i,2) = 0.
              qglif(i,3) = 0.
              qglif(i,4) = 1.
            endif
          endif
c
        endif

c
c set latent heat values
c
c       hvapf(t,tair) = hvap + cvap*(tair-273.16) - ch2o*(t-273.16)
c       hsubf(t,tair) = hsub + cvap*(tair-273.16) - cice*(t-273.16)

        zvap = hvapf (tsoi(i,1), ta(i)) !JAIR: Substituido 
        zsub = hsubf (tsoi(i,1), ta(i)) !JAIR: Substituido
c        zvap = hvap + cvap*(ta(i) - 273.16) - ch2o * (tsoi(i,1) - 273.16)
c        zsub = hsub + cvap*(ta(i) - 273.16) - cice * (tsoi(i,1) - 273.16)
        
        hvasug(i) = (qglif(i,1) + qglif(i,3)) * zvap +
     >              (qglif(i,2) + qglif(i,4)) * zsub 
c
        hvasui(i) = hsubf(tsno(i,1),ta(i)) ! JAIR: Substituido
c        hvasui(i) = hsub + cvap*(ta(i)-273.16) - cice*(tsno(i,1) - 273.16)
c
 200  continue   



c
      return
      end


      subroutine soilctlWrapper(dim_params, 
     >                          double_params,
     >                          npoi_matrix,
     >                          ibex ,
     >                          csoi ,
     >                          hsoi ,
     >                          hydraul ,
     >                          poros ,
     >                          porosflo, 
     >                          qglif, 
     >                          rhosoi, 
     >                          tsoi, 
     >                          upsoil, 
     >                          upsoiu ,
     >                          wflo, 
     >                          wisoi, 
     >                          wsoi, 
     >                          bex, 
     >                          suction,
     >                          consoi, 
     >                          hflo, 
     >                          sice, 
     >                          swater)

            

            integer dim_params(2)

            double precision double_params(9)

            double precision npoi_matrix(14, dim_params(1))

            double precision npoi_line1(dim_params(1))
            double precision npoi_line2(dim_params(1))
            double precision npoi_line3(dim_params(1))
            double precision npoi_line4(dim_params(1))
            double precision npoi_line5(dim_params(1))
            double precision npoi_line6(dim_params(1))
            double precision npoi_line7(dim_params(1))
            double precision npoi_line8(dim_params(1))
            double precision npoi_line9(dim_params(1))
            double precision npoi_line10(dim_params(1))
            double precision npoi_line11(dim_params(1))
            double precision npoi_line12(dim_params(1))
            double precision npoi_line13(dim_params(1))
            double precision npoi_line14(dim_params(1))

            integer          ibex(dim_params(1),dim_params(2))
            double precision csoi(dim_params(1),dim_params(2))
            double precision hsoi(dim_params(2)+1)
            double precision hydraul(dim_params(1),dim_params(2))    
            double precision poros(dim_params(1),dim_params(2))
            double precision porosflo(dim_params(1),dim_params(2))
            double precision qglif(dim_params(1),4)
            double precision rhosoi(dim_params(1),dim_params(2))
            double precision tsoi(dim_params(1),dim_params(2))
            double precision upsoil(dim_params(1),dim_params(2))
            double precision upsoiu(dim_params(1),dim_params(2))
            double precision wflo(dim_params(1),dim_params(2)+1)
            double precision wisoi(dim_params(1),dim_params(2))
            double precision wsoi(dim_params(1),dim_params(2))
            double precision bex(dim_params(1),dim_params(2))
            double precision suction(dim_params(1),dim_params(2))
            double precision consoi(dim_params(1),dim_params(2))
            double precision hflo(dim_params(1),dim_params(2)+1)
            double precision sice(dim_params(1),dim_params(2))
            double precision swater(dim_params(1),dim_params(2))

            integer i

c 	  Inicializando vetores double de tamanho npoi
            DO 102, i = 1, dim_params(1), 1
                  npoi_line1(i) = npoi_matrix(1, i)
                  npoi_line2(i) = npoi_matrix(2, i)
                  npoi_line3(i) = npoi_matrix(3, i)
                  npoi_line4(i) = npoi_matrix(4, i)
                  npoi_line5(i) = npoi_matrix(5, i)
                  npoi_line6(i) = npoi_matrix(6, i)
                  npoi_line7(i) = npoi_matrix(7, i)
                  npoi_line8(i) = npoi_matrix(8, i)
                  npoi_line9(i) = npoi_matrix(9, i)
                  npoi_line10(i) = npoi_matrix(10, i)
                  npoi_line11(i) = npoi_matrix(11, i)
                  npoi_line12(i) = npoi_matrix(12, i)
                  npoi_line13(i) = npoi_matrix(13, i)
                  npoi_line14(i) = npoi_matrix(14, i)
102         CONTINUE

            

            call soilctl(dim_params(1), dim_params(2), 
     >                   double_params(1), double_params(2), double_params(3),
     >                   double_params(4), double_params(5), double_params(6),
     >                   double_params(7), double_params(8), double_params(9),
     >                   npoi_line1,
     >                   npoi_line2 ,
     >                   npoi_line3 ,
     >                   npoi_line4 ,
     >                   npoi_line5 ,
     >                   npoi_line6 ,
     >                   npoi_line7 ,
     >                   npoi_line8 ,
     >                   npoi_line9 ,
     >                   npoi_line10,
     >                   npoi_line11,
     >                   npoi_line12,
     >                   npoi_line13,
     >                   npoi_line14,
     >                   ibex ,
     >                   csoi ,
     >                   hsoi ,
     >                   hydraul ,
     >                   poros ,
     >                   porosflo, 
     >                   qglif, 
     >                   rhosoi, 
     >                   tsoi, 
     >                   upsoil, 
     >                   upsoiu ,
     >                   wflo, 
     >                   wisoi, 
     >                   wsoi, 
     >                   bex, 
     >                   suction,
     >                   consoi, 
     >                   hflo, 
     >                   sice, 
     >                   swater)

c 	  RETORNANDO VALORES
            DO 777, i = 1, dim_params(1), 1
                  npoi_matrix(1, i) = npoi_line1(i)
                  npoi_matrix(2, i) = npoi_line2(i)
                  npoi_matrix(3, i) = npoi_line3(i)
                  npoi_matrix(4, i) = npoi_line4(i)
                  npoi_matrix(5, i) = npoi_line5(i)
                  npoi_matrix(6, i) = npoi_line6(i)
                  npoi_matrix(7, i) = npoi_line7(i)
                  npoi_matrix(8, i) = npoi_line8(i)
                  npoi_matrix(9, i) = npoi_line9(i)
                  npoi_matrix(10, i) = npoi_line10(i)
                  npoi_matrix(11, i) = npoi_line11(i)
                  npoi_matrix(12, i) = npoi_line12(i)
                  npoi_matrix(13, i) = npoi_line13(i)
                  npoi_matrix(14, i) = npoi_line14(i)
777         CONTINUE
      end



c JAIR: rotina2

c ---------------------------------------------------------------------
      subroutine soilctl(npoi,
     >                   nsoilay,
     >                   dtime,
     >                   epsilon,
     >                   ch2o,
     >                   cice,
     >                   hfus,
     >                   rhow,
     >                   tmelt,
     >                   wpudmax,
     >                   bperm,
     >                   fvapg ,
     >                   fwpud ,
     >                   fwtop ,
     >                   gadjust,
     >                   gdrain ,
     >                   grunof ,
     >                   heatg ,
     >                   raing ,
     >                   soihfl ,
     >                   tl ,
     >                   traing, 
     >                   tu ,
     >                   wipud, 
     >                   wpud ,
     >                   ibex ,
     >                   csoi ,
     >                   hsoi ,
     >                   hydraul ,
     >                   poros ,
     >                   porosflo, 
     >                   qglif, 
     >                   rhosoi, 
     >                   tsoi, 
     >                   upsoil, 
     >                   upsoiu ,
     >                   wflo, 
     >                   wisoi, 
     >                   wsoi, 
     >                   bex, 
     >                   suction,
     >                   consoi, 
     >                   hflo, 
     >                   sice, 
     >                   swater)
c ---------------------------------------------------------------------
c
c steps soil/seaice model through one timestep
c
      implicit none
c
c      include 'compar.h'
c      include 'comhyd.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
      integer i, k          ! loop indices
c
      double precision zfrez,           ! factor decreasing runoff fraction for tsoi < tmelt
     >     zrunf,           ! fraction of rain that doesn't stay in puddle (runoff fraction)
     >     zmin,            ! minimum value of zrunf
     >     zmax,            ! maximum value of zrunf
     >     grun1,           ! temporary storage for grunof
     >     rwork,           ! 
     >     wipre,           ! storing variable
     >     zdpud,           ! used to compute transfer from puddle to infiltration
     >     cx,              ! average specific heat for soil, water and ice
     >     chav,            ! average specific heat for water and ice
     >     zwsoi      
c
      double precision
     >  owsoi(npoi,nsoilay),    ! old value of wsoi
     >  otsoi(npoi,nsoilay),    ! old value of tsoi
     >  c0pud(npoi,nsoilay),    ! layer heat capacity due to puddles (=0 except for top)
     >  c1pud(npoi,nsoilay),    ! updated av. specifilayer heat capacity due to  puddle
     >  fhtop(npoi),            ! heat flux through soil surface (for soilheat)
     >  fsqueez(npoi),          ! excess amount of water (soilh2o) 
     >  dh(npoi),               ! correction if water at tsoi < tmelt or ice at temp > tmelt
     >  dw(npoi),               ! '
     >  zporos(npoi),
     >  dtsw					! Added for Green-Ampt, delta volumetric water content
c
      integer layerno  ! Added for Green-Ampt, layer number of wetting front

c globals

      integer          npoi
      integer          nsoilay
      double precision dtime
      double precision epsilon
      double precision ch2o
      double precision cice
      double precision hfus
      double precision rhow
      double precision tmelt
      double precision wpudmax
      double precision bperm
      double precision fvapg(npoi)
      double precision fwpud(npoi)
      double precision fwtop(npoi)
      double precision gadjust(npoi)
      double precision gdrain(npoi)
      double precision grunof(npoi)
      double precision heatg(npoi)
      double precision raing(npoi)
      double precision soihfl(npoi)
      double precision tl(npoi)
      double precision traing(npoi)
      double precision tu(npoi)
      double precision wipud(npoi)
      double precision wpud(npoi)
      integer          ibex(npoi,nsoilay)
      double precision csoi(npoi,nsoilay)
      double precision hsoi(nsoilay+1)
      double precision hydraul(npoi,nsoilay)    
      double precision poros(npoi,nsoilay)
      double precision porosflo(npoi,nsoilay)
      double precision qglif(npoi,4)
      double precision rhosoi(npoi,nsoilay)
      double precision tsoi(npoi,nsoilay)
      double precision upsoil(npoi,nsoilay)
      double precision upsoiu(npoi,nsoilay)
      double precision wflo(npoi,nsoilay+1)
      double precision wisoi(npoi,nsoilay)
      double precision wsoi(npoi,nsoilay)
      double precision bex(npoi,nsoilay)
      double precision suction(npoi,nsoilay)
      double precision consoi(npoi,nsoilay)
      double precision hflo(npoi,nsoilay+1)
      double precision sice(npoi,nsoilay)
      double precision swater(npoi,nsoilay)

c
c      include 'comsat.h'
c

      call const (c0pud, npoi*nsoilay, DBLE(0.0))
      call const (c1pud, npoi*nsoilay, DBLE(0.0))
c
c for soil, set soil infiltration rate fwtop (for 
c soilh2o) and upper heat flux fhtop (for soilheat)
c
c also step puddle model wpud, wipud
c
c procedure is:
c
c   (1) apportion raing btwn puddle liquid(wpud) or runoff(grunof)
c
c   (2) apportion evap/condens (fvapg) btwn infil rate(fwtop), soil
c       ice (wisoi(i,1)), puddle liq (wpud), or puddle ice (wipud)
c
c   (3) transfer some puddle liquid to fwtop
c
c   (4) compute upper heat flx fhtop: includes fwtop*ch2o*tsoi(i,1)
c       to be consistent with whflo in soilheat, and accounts for
c       changing rain temp from traing to tsoi(i,1) and runoff temp
c       from tsoi to max(tsoi(i,1),tmelt)
c
c   (5) transfer any excess puddle liq to runoff
c
      do 100 i = 1, npoi
c
c (1) apportion sfc-level rain between puddle liquid and runoff
c
c linear dependence of runoff fraction on wpud+wipud assumes
c uniform statistical distribution of sub-grid puddle
c capacities between 0 and wpudmax. runoff fraction is
c reduced linearly for tsoi < tmelt (by zfrez) in which case
c any rain will increase wpud which will be frozen to wipud
c below
c
        zfrez = max (0., min (1., (tsoi(i,1) - tmelt + .5) * 2.))
c
c always have some minimal amount of runoff (3%) even if
c puddles are dry or soil is cold, but don't allow more than
c a specified maximum (30%), since the rain must also be allowed
c to infiltrate (and more surface runoff is generated later in
c step (5) anyway). zmin was "tuned" based on drainage-to-total
c runoff ratios for the Trout Lake region (for wpudmax=200mm).
c zmax is based on the assumption that 70% of precip goes to ET
c (on average over the Upper Midwest); under saturated conditions,
c the remainder (30%) is assumed to go directly to surface runoff.
c Both zmin and zmax can be considered tunable, but it would
c probably be better to just adjust wpudmax.
c
        zmin = 0.03
c  CJK 1-16-03      zmax = 1.00
        zmax = 0.30   ! zmax change according to TET on 1-16-03 
        zrunf = zmin + (zmax - zmin) * zfrez * max (0.0,
     >          min (1., (wpud(i) + wipud(i)) / wpudmax))
c
        wpud(i) = wpud(i) + (1. - zrunf) * raing(i) * dtime
c
        grunof(i) = zrunf * raing(i)
c
c (2) apportion evaporation or condensation between 4 h2o stores:
c
        rwork = fvapg(i) * dtime
c
        if (fvapg(i).ge.0.) then
c
c evaporation: split according to qglif
c
          fwtop(i)   =            - qglif(i,1)*fvapg(i)
          wpud(i)    = wpud(i)    - qglif(i,3)*rwork
          wipud(i)   = wipud(i)   - qglif(i,4)*rwork
c
          wipre = wisoi(i,1)
          wisoi(i,1) = max (0., wipre - qglif(i,2)*rwork /
     >                          (rhow*poros(i,1)*hsoi(1)))
c
          if (1.-wisoi(i,1).gt.epsilon)
     >      wsoi(i,1) = wsoi(i,1)*(1.-wipre)/(1.-wisoi(i,1))
c
        else
c
c condensation: give all to puddles (to avoid wsoi, wisoi > 1)
c
          fwtop(i) = 0.
          wpud(i) = wpud(i)  - (qglif(i,1)+qglif(i,3))*rwork
          wipud(i)= wipud(i) - (qglif(i,2)+qglif(i,4))*rwork
c
        endif
c
c (3) transfer some puddle liquid to infiltration; can lead
c     to small amounts of negative wpud (in soilh2o) due to
c     round-off error
c
c ------------------------------------------------------------------
c the following is commented out by Kaiyuan Li for Green-ampt
c this is the origianl code for infiltration part
c
        zdpud = rhow * dtime * max (0., 1.-wisoi(i,1))**2 *
     >          hydraul(i,1)
c
c ------------------------------------------------------------------
c
c ------------------------------------------------------------------
c Added by Kaiyuan Li for incorporation of Green-ampt infiltration 
c calculate potential iinfiltration (actual infiltration is fwpud)
c The Green-Ampt equation adopted bolow is from Julien et al. 1995
c Water resources buttetin vol. 31, No. 3: 523 - 536, 1995
c
c      call delta_sw(fwpudtot(i), dtsw, layerno, i)   ! calculate delta soil water content
c
c ---- calculate potential infiltration zdpud
c       zdpud = 0.5 * ((hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime - 2.0 * fwpudtot(i))
c     >   +  sqrt((hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime - 2.0 * fwpudtot(i)) ** 2
c     >   +  8.0 * hydraul(i, 1) * max(0., 1.-wisoi(i,1))**2 * 1000.0 * dtime *
c     >      (cpwf(i, layerno) * 1000 * dtsw + fwpudtot(i))))           
c-----------------
c end Green Ampt
c       
c ---- calculate potential infiltration zdpud (Christine Molling's version, wrong!)        
c        dtsw = poros(i, 1)*(1.0 - wisoi(i, 1))*(1.0 - wsoi(i, 1))
c        zdpud = 0.5 * ((hydraul(i, 1) * 1000.0 * dtime - 2.0 * fwpudtot(i))
c     >   +  sqrt((hydraul(i, 1) * 1000.0 * dtime - 2.0 * fwpudtot(i)) ** 2
c     >   +  8.0 * hydraul(i, 1) * 1000.0 * dtime * (cpwf(i, 1) * 1000 * dtsw + fwpudtot(i))))
c ------------------------------------------------------------------
c
        fwpud(i) = max (0., min (wpud(i), zdpud)) / dtime
        c0pud(i,1) = ch2o*wpud(i) + cice*wipud(i)
c
c (4) compute upper soil heat flux
c
        fhtop(i) = heatg(i)
     >           + raing(i)*ch2o*(traing(i)-tsoi(i,1))
     >           - grunof(i)*ch2o*max(tmelt-tsoi(i,1), 0.)

        soihfl(i) = fhtop(i)
c
c update diagnostic variables
c
        gadjust(i) = 0.0
c
 100  continue
c
c reduce soil moisture due to transpiration (upsoi[u,l], from
c turvap).need to do that before other time stepping below since 
c specific heat of this transport is neglected
c
c first set porosflo, reduced porosity due to ice content, used
c as the effective porosity for uptake here and liquid hydraulics
c later in soilh2o. to avoid divide-by-zeros, use small epsilon
c limit; this will always cancel with epsilon or 0 in numerators
c
c also increment soil temperature to balance transpired water
c differential between temps of soil and leaf. physically
c should apply this to the tree, but would be awkward in turvap.
c 
c also, save old soil moisture owsoi and temperatures otsoi so
c implicit soilh2o and soilheat can aposteriori deduce fluxes.
c
      do 120 k = 1, nsoilay
c
        do 130 i = 1, npoi
c
          porosflo(i,k) = poros(i,k) * max (epsilon, (1.-wisoi(i,k)))
c
c next line just for ice whose poros(i,k) is 0.0
c
          porosflo(i,k) = max (porosflo(i,k), epsilon)
c
          wsoi(i,k) = wsoi(i,k) - dtime * (upsoiu(i,k) + upsoil(i,k)) /
     >                            (rhow * porosflo(i,k) * hsoi(k))
c
          cx = c0pud(i,k) + 
     >         (   (1.-poros(i,k))*csoi(i,k)*rhosoi(i,k)
     >           + poros(i,k)*(1.-wisoi(i,k))*wsoi(i,k)*ch2o*rhow
     >           + poros(i,k)*wisoi(i,k)*cice*rhow
     >         ) * hsoi(k)
c
          tsoi(i,k) = tsoi(i,k) - dtime * ch2o * 
     >                (  upsoiu(i,k)*(tu(i)-tsoi(i,k))
     >                 + upsoil(i,k)*(tl(i)-tsoi(i,k)) ) / cx
c
          owsoi(i,k)  = wsoi(i,k)
          otsoi(i,k)  = tsoi(i,k)
c
 130    continue
c
 120  continue

c
c step soil moisture calculations
c   
      call soilh2o (owsoi, fsqueez,
     >              bex,
     >              bperm,
     >              dtime,
     >              epsilon,
     >              fwpud,
     >              fwtop, 
     >              hsoi,
     >              hydraul,
     >              ibex,
     >              npoi,
     >              nsoilay,
     >              poros,
     >              porosflo,
     >              rhow,
     >              suction,
     >              wflo,
     >              wisoi,
     >              wpud,
     >              wsoi)

c
c update drainage and puddle
c
      do 200 i = 1, npoi
c
        gdrain(i)  = wflo(i,nsoilay+1)
        c1pud(i,1) = ch2o*wpud(i) + cice*wipud(i)
c
c --------------------------------------------------------------------------------
c added for Green-Ampt infiltration model
c         if (raing(i) .lt. 0.000001/3600.0)  then
c            fwpudtot(i) = 0
c         else
c           fwpudtot(i) = fwpudtot(i) + (fwpud(i) - fsqueez(i)) * dtime
c         end if
c ---------------------------------------------------------------------------------
c
 200  continue
c
c step temperatures due to conductive heat transport
c
     

      call soilheat (otsoi, owsoi, c0pud, fhtop, c1pud,
     >                     ch2o,
     >                     cice,
     >                     consoi ,
     >                     csoi ,
     >                     dtime,
     >                     hflo ,
     >                     hsoi ,
     >                     npoi,
     >                     nsoilay,
     >                     poros ,
     >                     rhosoi ,
     >                     rhow,
     >                     tsoi ,
     >                     wflo ,
     >                     wisoi ,
     >                     wsoi)
      
c
c set wsoi, wisoi to exactly 0 or 1 if differ by negligible 
c amount (needed to avoid epsilon errors in loop 400 below)
c

      call wadjust(dtime,
     >             epsilon,
     >             gadjust,
     >             hsoi,
     >             npoi,
     >             nsoilay,
     >             poros,
     >             rhow,
     >             wisoi,
     >             wsoi)

      
c
c heat-conserving adjustment for liquid/ice below/above melt
c point. uses exactly the same logic as for intercepted veg h2o
c in steph2o2. we employ the fiction here that soil liquid and
c soil ice both have density rhow, to avoid "pot-hole"
c difficulties of expansion on freezing. this is done by 
c dividing all eqns through by rhow(*hsoi).
c
c the factor (1-wsoi(old))/(1-wisoi(new)) in the wsoi increments
c results simply from conservation of h2o mass; recall wsoi is
c liquid content relative to ice-reduced pore space.
c
      do 400 k = 1, nsoilay
        do 410 i = 1, npoi
c
c next line is just to avoid divide-by-zero for ice with
c poros = 0
c
          zporos(i) = max (poros(i,k), epsilon)
          rwork = c1pud(i,k)/rhow/hsoi(k)
     >           + (1.-zporos(i))*csoi(i,k)*rhosoi(i,k)/rhow
c
          chav = rwork
     >           + zporos(i)*(1.-wisoi(i,k))*wsoi(i,k)*ch2o
     >           + zporos(i)*wisoi(i,k)*cice
c
c if liquid exists below melt point, freeze some to ice
c
c (note that if tsoi>tmelt or wsoi=0, nothing changes.)
c (also note if resulting wisoi=1, either dw=0 and prev
c wisoi=1, or prev wsoi=1, so use of epsilon is ok.)
c
          zwsoi = min (1., wsoi(i,k))
c
          dh(i) = chav * (tmelt-tsoi(i,k))
          dw(i) = min ( zporos(i)*(1.-wisoi(i,k))*zwsoi,
     >                  max (0.,dh(i)/hfus) )
c
          wisoi(i,k) = wisoi(i,k) +  dw(i)/zporos(i)
          wsoi(i,k)  = wsoi(i,k)  - (dw(i)/zporos(i))*(1.-zwsoi)
     >                              / max (epsilon,1.-wisoi(i,k))
c
          chav = rwork
     >           + zporos(i)*(1.-wisoi(i,k))*wsoi(i,k)*ch2o
     >           + zporos(i)*wisoi(i,k)*cice
c
          tsoi(i,k) = tmelt - (dh(i)-hfus*dw(i)) / chav
c
c if ice exists above melt point, melt some to liquid
c
c note that if tsoi<tmelt or wisoi=0, nothing changes
c
c also note if resulting wisoi=1, dw=0 and prev wisoi=1,
c so use of epsilon is ok
c
          dh(i) = chav * (tsoi(i,k) - tmelt)
          dw(i) = min ( zporos(i)*wisoi(i,k), max (0., dh(i)/hfus) )
c
          wisoi(i,k) = wisoi(i,k) -  dw(i)/zporos(i)
          wsoi(i,k)  = wsoi(i,k)  + (dw(i)/zporos(i))
     >                 * (1.-wsoi(i,k)) / max(epsilon,1.-wisoi(i,k))
c
          chav = rwork
     >           + zporos(i)*(1.-wisoi(i,k))*wsoi(i,k)*ch2o 
     >           + zporos(i)*wisoi(i,k)*cice
c
          tsoi(i,k) = tmelt + (dh(i)-hfus*dw(i)) / chav
c
c reset porosflo (although not used after this)
c
          porosflo(i,k) = zporos(i) * max (epsilon, 1.-wisoi(i,k))
c
  410   continue
  400 continue
c 
c set wsoi, wisoi to exactly 0 or 1 if differ by negligible 
c amount (roundoff error in loop 400 above can produce very
c small negative amounts)
c
      call wadjust(dtime,
     >             epsilon,
     >             gadjust ,
     >             hsoi ,
     >             npoi,
     >             nsoilay,
     >             poros ,
     >             rhow,
     >             wisoi ,
     >             wsoi)
c
c repeat ice/liquid adjustment for upper-layer puddles (don't 
c divide through by rhow*hsoi). upper-layer moistures wsoi,wisoi
c are already consistent with tsoi(i,1) > or < tmelt, and will 
c remain consistent here since tsoi(i,1) will not cross tmelt
c
      k = 1
c
      do 500 i = 1, npoi
c
c if any puddle liquid below tmelt, freeze some to puddle ice
c
        rwork = ( (1.-poros(i,k))*csoi(i,k)*rhosoi(i,k)
     >           + poros(i,k)*(1.-wisoi(i,k))*wsoi(i,k)*ch2o*rhow
     >           + poros(i,k)*wisoi(i,k)*cice*rhow
     >         ) * hsoi(k)
c
        chav = ch2o*wpud(i) + cice*wipud(i) + rwork
c
        dh(i) = chav * (tmelt-tsoi(i,k))
        dw(i) = min (wpud(i), max (0., dh(i)/hfus))
        wipud(i) = wipud(i) + dw(i)
        wpud(i)  = wpud(i)  - dw(i)
        chav = ch2o*wpud(i) + cice*wipud(i) + rwork
        tsoi(i,k) = tmelt - (dh(i)-hfus*dw(i)) / chav
c
c (5) transfer any excess puddle liq to runoff
c
c the following runoff formulation could give rise to very
c small amounts of negative runoff
c
        grun1 = (min (wpud(i),
     >           max (0., wpud(i) + wipud(i) - wpudmax))) / dtime
c
        grunof(i) = grunof(i) + grun1
c
        wpud(i) = wpud(i) - grun1 * dtime
c
c if any puddle ice above tmelt, melt it and send to puddle liquid
c (not apportioned between puddle and surface runoff, to avoid
c potential double shunting to runoff, i.e. duplicating step 1).
c 
        dh(i) = chav * (tsoi(i,k)-tmelt)
        dw(i) = min (wipud(i), max (0., dh(i)/hfus))
        wipud(i) = wipud(i) - dw(i)
        wpud(i)  = wpud(i) + dw(i)
        chav = ch2o*wpud(i) + cice*wipud(i) + rwork
        tsoi(i,k) = tmelt + (dh(i)-hfus*dw(i)) / chav
c  
  500 continue
c


      return
      end
c

c JAIR: rotina3 (chamada na rotina2) - OK

c ---------------------------------------------------------------------
      subroutine soilh2o (owsoi, fsqueez,
     >                    bex ,
     >                    bperm,
     >                    dtime,
     >                    epsilon,
     >                    fwpud ,
     >                    fwtop ,
     >                    hsoi ,
     >                    hydraul ,
     >                    ibex,
     >                    npoi,
     >                    nsoilay,
     >                    poros ,
     >                    porosflo ,
     >                    rhow,
     >                    suction ,
     >                    wflo ,
     >                    wisoi ,
     >                    wpud ,
     >                    wsoi)
c ---------------------------------------------------------------------
c
c sets up call to tridia to solve implicit soil moisture eqn,
c using soil temperatures in wsoi (in comsoi)
c
c lower bc can be no h2o flow or free drainage, set by bperm below
c
      implicit none
c
c      include 'compar.h'
c      include 'comsoi.h'
c      include 'com1d.h'
c
      integer          npoi
      integer          nsoilay
      double precision bex(npoi,nsoilay)
      double precision bperm
      double precision dtime
      double precision epsilon
      double precision fwpud(npoi)
      double precision fwtop(npoi)
      double precision hsoi(nsoilay+1)
      double precision hydraul(npoi,nsoilay)
      integer          ibex(npoi,nsoilay)
      double precision poros(npoi,nsoilay)
      double precision porosflo(npoi,nsoilay)
      double precision rhow
      double precision suction(npoi,nsoilay)
      double precision wflo(npoi,nsoilay+1)
      double precision wisoi(npoi,nsoilay)
      double precision wpud(npoi)
      double precision wsoi(npoi,nsoilay)

c Arguments : all are supplied except wflo (returned):
c
      double precision owsoi(npoi,nsoilay),  ! soil moistures at start of timestep
     >     fsqueez(npoi)         ! excess water at end of time step in soil column  
c
c local variables
c     
      integer k, i,    ! loop indices
     >        km1,
     >        kka, 
     >        kkb 
c
      integer 
     >  m(npoi),                ! exponents 
     >  n(npoi)
c
      double precision dmin,                ! minimum diffusivity for dry soils (m**2 s-1) 
     >     rimp,                ! implicit fraction of the calculation (0 to 1)
     >     zbex, z, dt, zz
c 
      double precision hsoim(nsoilay+1)  ! vertical distances between centers of layers
c
      double precision
     >  wsoim(npoi,nsoilay+1),    ! interpolated moisture values at layer boundaries
     >  wsoia(npoi,nsoilay+1),    ! 
     >  wsoib(npoi,nsoilay+1),    ! '
     >  weim(npoi,nsoilay+1),     ! '
     >  weip(npoi,nsoilay+1),     ! '
     >  a(npoi),                  ! intermediate terms (const for each pt)
     >  b(npoi),                  ! 
     >  bwn(npoi),                ! 
     >  bwn1(npoi),               ! 
     >  e(npoi,nsoilay+1),        ! intermediate terms in algebraic devel 
     >  f(npoi,nsoilay+1),        ! '
     >  g(npoi,nsoilay+1),        ! '
     >  d1(npoi,nsoilay),         ! diagonals of tridiagonal systems of equations 
     >  d2(npoi,nsoilay),         !  '
     >  d3(npoi,nsoilay),         !  '
     >  rhs(npoi,nsoilay),        ! right-hand sides of systems of equations
     >  w1(npoi,nsoilay),         ! work arrays needed by tridia
     >  w2(npoi,nsoilay)          !  '
c
      integer jb1, jb2

      save dmin, rimp
      data dmin, rimp /1.e-9, 1.0/

    
      ! JAIR: Zera variáveis w1 e w2
      do 666 jb1 = 1, npoi
            do 667 jb2 = 1, nsoilay
                  w1(jb1, jb2) = 0.0
                  w2(jb1, jb2) = 0.0
667         continue
666   continue 

c
c set lower boundary condition for the soil
c (permeability of the base)
c
c     bperm = 0.00  ! e.g. fully impermeable base
c     bperm = 1.00  ! e.g. fully permeable base
c
*     bperm = 0.10
c
c set level vertical distances, interpolated moistures, and
c interpolation weights
c
c top layer
c
      k = 1
c
      do 100 i = 1, npoi
c
        hsoim(k) = 0.5 * hsoi(k)
c
        weim(i,k) = 0.0
        weip(i,k) = 1.0
c
        wsoim(i,k) = wsoi(i,k)
        wsoia(i,k) = min (wsoim(i,k), 1.0)
        wsoib(i,k) = min (wsoim(i,k), 1.0)
c
  100 continue
c
c middle layers
c
      do 110 k = 2, nsoilay
c
        do 120 i = 1, npoi
c
          hsoim(k) = 0.5 * (hsoi(k-1) + hsoi(k))
c
          weim(i,k) = 0.5 * hsoi(k) / hsoim(k)
          weip(i,k) = 1.0 - weim(i,k)
c
          wsoim(i,k) = weim(i,k) * wsoi(i,k-1) + weip(i,k) * wsoi(i,k)
          wsoia(i,k) = min (wsoim(i,k), 1.0)
          wsoib(i,k) = min (wsoim(i,k), 1.0)
c
  120   continue
c
  110 continue
c
c bottom layer
c
      k = nsoilay + 1
c
      do 130 i = 1, npoi
c
        hsoim(k) = 0.5 * hsoi(k-1)
c
        weim(i,k) = 1.0
        weip(i,k) = 0.0
c
        wsoim(i,k) = wsoi(i,k-1)
        wsoia(i,k) = min (wsoim(i,k), 1.0)
        wsoib(i,k) = min (wsoim(i,k), 1.0)
c
  130 continue
c
c set intermediate quantities e,f,g. these are terms in the
c expressions for the fluxes at boundaries between layers,
c so are zero for k=1. use bwn1 to account for minimum 
c diffusivity dmin. bperm is used for k=nsoilay+1 to set the
c type of the lower bc.
c
c top layer
c
      k = 1
c
      call const (e(1,k), npoi, DBLE(0.0))
      call const (f(1,k), npoi, DBLE(0.0))
      call const (g(1,k), npoi, DBLE(0.0))

c
c middle layers
c
      do 200 k = 2, nsoilay
c
        do 210 i = 1, npoi
c
c now that hydraul, suction and ibex can vary with depth,
c use averages of surrounding mid-layer values
c
c (see notes 8/27/93)
c
          a(i) = weim(i,k) * hydraul(i,k-1) +
     >           weip(i,k) * hydraul(i,k  )
c
          b(i) = weim(i,k) * hydraul(i,k-1) * 
     >           suction(i,k-1) * bex(i,k-1) +
     >           weip(i,k) * hydraul(i,k  ) *
     >           suction(i,k  ) * bex(i,k  )
c
          zbex = weim(i,k) * bex(i,k-1) +
     >           weip(i,k) * bex(i,k  ) 
c
          m(i) = 2 * nint(zbex) + 3
          n(i) =     nint(zbex) + 2
c
          bwn1(i) = b(i) * (wsoib(i,k)**(n(i)-1))
          bwn(i)  = bwn1(i) * wsoib(i,k)
c
          if (bwn(i).lt.dmin) bwn1(i) = 0.0
          bwn(i) = max (bwn(i), dmin)
c
          e(i,k) =  (-1.+rimp*m(i))*a(i)*(wsoia(i,k)**m(i))
     >            + ((1.-rimp)*bwn(i) - rimp*n(i)*bwn1(i)*wsoib(i,k))
     >              * (wsoi(i,k)-wsoi(i,k-1)) / hsoim(k)
c
          f(i,k) = - rimp*m(i)*a(i)*(wsoia(i,k)**(m(i)-1))
     >             + rimp*n(i)*bwn1(i)
     >               * (wsoi(i,k)-wsoi(i,k-1)) / hsoim(k)
c
          g(i,k) = rimp*bwn(i)
c
  210     continue
c
  200 continue

      

c
c bottom layer
c
      k = nsoilay + 1
c
      do 220 i = 1, npoi
c
        a(i) = hydraul(i,nsoilay) 
        b(i) = hydraul(i,nsoilay)*suction(i,nsoilay)*ibex(i,nsoilay)
c
        m(i) = 2*ibex(i,nsoilay) + 3
        n(i) = ibex(i,nsoilay)   + 2
c
        e(i,k) = -a(i)*(wsoia(i,k)**m(i))*bperm
        f(i,k) = 0.0
        g(i,k) = 0.0
c
  220 continue


c deduce all e,f,g in proportion to the minimum of the two 
c adjacent layers' (1-wisoi), to account for restriction of flow
c by soil ice. this will cancel in loop 300  with the factor 
c 1-wisoi in (one of) the layer's porosflo, even if wisoi=1 by 
c the use of epsilon limit. so a layer with wisoi=1 will form a 
c barrier to flow of liquid, but still have a predicted wsoi
c
      do 230 k = 1, nsoilay+1
c
        kka = max (k-1,1)
        kkb = min (k,nsoilay)
c
        do 240 i=1,npoi
c
c multiply by an additional factor of 1-wisoi for stability
c
          z = max(0.,1.-max(wisoi(i,kka),wisoi(i,kkb)))**2
c
          e(i,k) = z * e(i,k)
          f(i,k) = z * f(i,k)
          g(i,k) = z * g(i,k)
c
  240   continue
c
  230 continue

c
c set matrix diagonals and right-hand sides
c
      do 300 k = 1, nsoilay
c
        do 310 i = 1, npoi
c
          dt = dtime / (porosflo(i,k)*hsoi(k))
          d1(i,k) = dt*(   f(i,k)*0.5*hsoi(k)/hsoim(k)
     >                   - g(i,k)/hsoim(k) )
          rhs(i,k) = wsoi(i,k) + dt*( e(i,k+1) - e(i,k) )
c
  310   continue
c
        if (k.eq.1) then
c
          do 320 i=1,npoi
c
            dt = dtime / (porosflo(i,k)*hsoi(k))
            rhs(i,k) = rhs(i,k) + dt*(fwtop(i)+fwpud(i))/rhow
c
  320     continue
c
        endif
c
        if (k.lt.nsoilay) then
c
          km1 = max (k-1,1)
c
          do 330 i=1,npoi
c
            dt = dtime / (porosflo(i,k)*hsoi(k))
            d2(i,k) = 1. + dt*( - f(i,k+1)*0.5*hsoi(k+1)/hsoim(k+1)
     >                          + f(i,k)  *0.5*hsoi(km1)/hsoim(k)
     >                          + g(i,k+1)/hsoim(k+1)
     >                          + g(i,k)  /hsoim(k) )
            d3(i,k) = dt*( - f(i,k+1)*0.5*hsoi(k)/hsoim(k+1)
     >                     - g(i,k+1)            /hsoim(k+1) )
c
  330     continue
c
        else if (k.eq.nsoilay) then
c
          do 340 i=1,npoi
c
            dt = dtime / (porosflo(i,k)*hsoi(k))
            d2(i,k) = 1. + dt*( - f(i,k+1)
     >                          + f(i,k)  *0.5*hsoi(k-1)/hsoim(k)
     >                          + g(i,k)  /hsoim(k) )
            d3(i,k) = 0.0
c
  340     continue
c
        endif
c
  300 continue
c
c solve the systems of equations
c
      call tridia (npoi, npoi, nsoilay, d1, d2, d3, rhs, wsoi, w1, w2)
      !call tridia (npoi, npoi, nsoilay, real(d1), real(d2), real(d3), real(rhs), real(wsoi), real(w1), real(w2))
c
      do 400 i = 1, npoi
c
        fsqueez(i) = 0.0
        wflo(i,nsoilay+1) = - rhow * e(i,nsoilay+1)
c
  400 continue
            
      
c
      do 500 k = nsoilay, 1, -1
c
        do 510 i = 1, npoi
c
          zz = rhow * poros(i,k) * 
     >         max(epsilon, (1.-wisoi(i,k))) * hsoi(k)         
c
          wsoi(i,k) = wsoi(i,k) + dtime * fsqueez(i) / zz 
          fsqueez(i) = max (wsoi(i,k)-1.,0.) * zz / dtime
          wsoi(i,k) = min (wsoi(i,k),1.)
c           
          wflo(i,k) = wflo(i,k+1) + (wsoi(i,k)-owsoi(i,k)) * zz / dtime
          
c
  510   continue
c
  500 continue
c
c step puddle liquid due to fsqueez and fwpud
c
c also subtract net puddle-to-top-layer flux from wflo(i,1),
c since puddle and top soil layer are lumped together in soilheat
c so upper wflo should be external flux only (evap/condens)

      do 600 i = 1, npoi
c
        wpud(i)   = wpud(i)   + (fsqueez(i) - fwpud(i)) * dtime
        wflo(i,1) = wflo(i,1) + (fsqueez(i) - fwpud(i))
c
  600 continue
c

      return
      end

c JAIR: rotina4 (chamada na rotina2) - OK

c ---------------------------------------------------------------------
      subroutine soilheat (otsoi, owsoi, c0pud, fhtop, c1pud,
     >                     ch2o,
     >                     cice,
     >                     consoi ,
     >                     csoi ,
     >                     dtime,
     >                     hflo ,
     >                     hsoi ,
     >                     npoi,
     >                     nsoilay,
     >                     poros ,
     >                     rhosoi ,
     >                     rhow,
     >                     tsoi ,
     >                     wflo ,
     >                     wisoi ,
     >                     wsoi)
     
c ---------------------------------------------------------------------
c
c        sets up call to tridia to solve implicit soil/ice heat 
c        conduction, using layer temperatures in tsoi (in comsoi).
c        the heat flux due to liquid flow previously calculated
c        in soilh2o is accounted for. lower bc is conductive flux = 0
c        for soil (although the flux due to liquid drainage flow can
c        be > 0)
c
c     implicit none
c
c      include 'compar.h'
c      include 'comsoi.h'

      integer          npoi
      integer          nsoilay
      double precision ch2o
      double precision cice
      double precision consoi(npoi,nsoilay)
      double precision csoi(npoi,nsoilay)
      double precision dtime
      double precision hflo(npoi,nsoilay+1)
      double precision hsoi(nsoilay+1)
      double precision poros(npoi,nsoilay)
      double precision rhosoi(npoi,nsoilay)
      double precision rhow
      double precision tsoi(npoi,nsoilay)
      double precision wflo(npoi,nsoilay+1)
      double precision wisoi(npoi,nsoilay)
      double precision wsoi(npoi,nsoilay)
c
c Arguments
c
      double precision
     >  otsoi(npoi,nsoilay),    ! soil/ice temperatures at start of timestep (redundant
c                                 with tsoi, but passed to be consistent with soilh2o)
     >  owsoi(npoi,nsoilay),    ! soil moistures at start of timestep (before soilh2o)
     >  c0pud(npoi,nsoilay),    ! layer heat capacity due to puddles (=0 except for top)
     >  c1pud(npoi,nsoilay),    ! updated c0pud
     >  fhtop(npoi)             ! heat flux into top layer from atmos
c
c local variables
c
      integer k, i, km1, kp1    ! loop indices
c
      double precision rimp,                ! implicit fraction of the calculation (0 to 1)
     >     rwork, rwork1,       ! work variables
     >     rwork2, rwork3,      ! work variables
     >     t
c
      double precision
     >  whflo(npoi,nsoilay+1),   ! downward heat fluxes across layer bdries due to h2o
     >                           ! movement calculated in soilh2o
     >  con(npoi,nsoilay+1),     ! conduction coeffs between layers
     >  c0(npoi,nsoilay),        ! specific heats at start of timestep
     >  c1(npoi,nsoilay),        ! specific heats at end of timestep
     >  d1(npoi,nsoilay),        ! diagonals of tridiagonal systems of equations
     >  d2(npoi,nsoilay),        !  ''
     >  d3(npoi,nsoilay),        !  ''
     >  rhs(npoi,nsoilay),       ! right-hand sides of systems of equations
     >  w1(npoi,nsoilay),        ! work arrays needed by tridia
     >  w2(npoi,nsoilay)
c
      data rimp /1.0/

c
c set conduction coefficient between layers, and heat fluxes
c due to liquid transport
c
c top layer
c
      k = 1
c
      do 100 i = 1, npoi
        con(i,k) = 0.0
        whflo(i,k) = wflo(i,k) * ch2o * tsoi(i,k)
  100 continue
c
c middle layers
c
      do 110 k = 2, nsoilay
c
        do 120 i = 1, npoi
c
          con(i,k) =  1. / (0.5 * (hsoi(k-1) / consoi(i,k-1) +
     >                             hsoi(k)   / consoi(i,k)))
c
          t = (hsoi(k) * tsoi(i,k-1) + hsoi(k-1) * tsoi(i,k)) /
     >        (hsoi(k-1)             + hsoi(k))
c
          whflo(i,k) = wflo(i,k) * ch2o * t
c
  120   continue
c
  110 continue
c
c bottom layer
c
      k = nsoilay + 1
c
      do 130 i = 1, npoi
        con(i,k) = 0.0
        whflo(i,k) = wflo(i,k) * ch2o * tsoi(i,k-1)
  130 continue
c
c set diagonals of matrix and right-hand side. use old and
c new heat capacities c0, c1 consistently with moisture fluxes
c whflo computed above, to conserve heat associated with 
c changing h2o amounts in each layer
c
      do 200 k = 1, nsoilay
c
        km1 = max (k-1,1)
        kp1 = min (k+1,nsoilay)
c
        do 210 i=1,npoi
c
          rwork1 = (1.-poros(i,k))*csoi(i,k)*rhosoi(i,k)
          rwork2 = poros(i,k)*(1.-wisoi(i,k))*ch2o*rhow
          rwork3 = poros(i,k)*wisoi(i,k)*cice*rhow
c
          c0(i,k) = c0pud(i,k) + 
     >              (   rwork1
     >                + rwork2 * owsoi(i,k)
     >                + rwork3
     >              ) * hsoi(k)
c
          c1(i,k) = c1pud(i,k) +  
     >              (   rwork1
     >                + rwork2 * wsoi(i,k)
     >                + rwork3
     >              ) * hsoi(k)
c
          rwork = dtime/c1(i,k)
c
          d1(i,k) =    - rwork * rimp * con(i,k)
          d2(i,k) = 1. + rwork * rimp * (con(i,k)+con(i,k+1))
          d3(i,k) =    - rwork * rimp * con(i,k+1)
c
          rhs(i,k) = (c0(i,k)/c1(i,k))*tsoi(i,k) + rwork
     >               * ( (1.-rimp)*con(i,k)  *(tsoi(i,km1)-tsoi(i,k))
     >                 + (1.-rimp)*con(i,k+1)*(tsoi(i,kp1)-tsoi(i,k))
     >                 + whflo(i,k) - whflo(i,k+1) )
c
  210   continue
c
        if (k.eq.1) then
          do 220 i=1,npoi
            rhs(i,k) = rhs(i,k) + (dtime/c1(i,k))*fhtop(i)
  220     continue
        endif
c
  200 continue
c
c solve systems of equations
c
      call tridia (npoi, npoi, nsoilay, d1, d2, d3, rhs, tsoi, w1, w2)
      !call tridia (npoi, npoi, nsoilay, real(d1), real(d2), real(d3), real(rhs), real(tsoi), real(w1), real(w2)) 
c
c deduce downward heat fluxes between layers
c
      call scopy (npoi, fhtop, hflo(1,1))
c
      do 300 k=1,nsoilay
        do 310 i=1,npoi
          hflo(i,k+1) = hflo(i,k) -
     >                  (c1(i,k)*tsoi(i,k) - c0(i,k)*otsoi(i,k)) / dtime
  310   continue
  300 continue
c

      return
      end


c JAIR: rotina5 (chamada na rotina2) - OK

c
c ---------------------------------------------------------------------
      subroutine wadjust(dtime,
     >                   epsilon,
     >                   gadjust ,
     >                   hsoi ,
     >                   npoi,
     >                   nsoilay,
     >                   poros ,
     >                   rhow,
     >                   wisoi ,
     >                   wsoi)
c ---------------------------------------------------------------------
c
c set wsoi, wisoi to exactly 0 if differ by negligible amount, 
c to protect epsilon logic in soilctl and soilh2o
c
c ice-liquid transformations in soilctl loop 400 can produce very
c small -ve amounts due to roundoff error, and very small -ve or +ve
c amounts can cause (harmless) "underflow" fpes in soilh2o
c
      implicit none
c
c     include 'compar.h'
c     include 'comhyd.h'
c     include 'comsoi.h'
c     include 'com1d.h'
c 
c local variables
c
      integer          npoi
      integer          nsoilay

      integer k, i
      double precision ztot0, ztot1

      double precision dtime
      double precision epsilon
      double precision gadjust(npoi)
      double precision hsoi(nsoilay+1)
      double precision poros(npoi,nsoilay)
      double precision rhow
      double precision wisoi(npoi,nsoilay)
      double precision wsoi(npoi,nsoilay)
c
      do 100 k = 1, nsoilay
        do 110 i = 1, npoi
c
c initial total soil water
c
         ztot0 = hsoi(k) * poros(i,k) * rhow *
     >           ((1. - wisoi(i,k)) * wsoi(i,k) + wisoi(i,k))
c
c set bounds on wsoi and wisoi
c
         if (wsoi(i,k).lt.epsilon)  wsoi(i,k)  = 0.0
         if (wisoi(i,k).lt.epsilon) wisoi(i,k) = 0.0
c
         wsoi(i,k)  = min (1., wsoi(i,k))
         wisoi(i,k) = min (1., wisoi(i,k))
c
         if (wisoi(i,k).ge.1-epsilon) wsoi(i,k) = 0.0
c
c for diagnosis of total adjustment
c
         ztot1 = hsoi(k) * poros(i,k) * rhow *
     >           ((1. - wisoi(i,k)) * wsoi(i,k) + wisoi(i,k))
c
         gadjust(i) = gadjust(i) + (ztot1 - ztot0) / dtime
c
  110   continue
  100 continue
c
      return
      end


c ---------------------------------------------------------------------
      subroutine const (arr, nar, value)
c ---------------------------------------------------------------------
c
c sets all elements of double precision vector arr to value
c
      implicit none
c
c Arguments
c
      integer nar
c     
      double precision value
      double precision arr(nar)
c
c Local variables
c
      integer j
c
      do 100 j = 1, nar
        arr(j) = value
 100  continue
c
      return
      end



c ---------------------------------------------------------------------
      subroutine tridia (ns, nd, ne, a, b, c, y, x, alpha, gamma)
c ---------------------------------------------------------------------
c
c     implicit none
c
c     include 'compar.h'
c
c     purpose:
c     to compute the solution of many tridiagonal linear systems.
c
c      arguments:
c
c      ns ..... the number of systems to be solved.
c
c      nd ..... first dimension of arrays (ge ns).
c
c      ne ..... the number of unknowns in each system.
c               this must be > 2. second dimension of arrays.
c
c      a ...... the subdiagonals of the matrices are stored
c               in locations a(j,2) through a(j,ne).
c
c      b ...... the main diagonals of the matrices are stored
c               in locations b(j,1) through b(j,ne).
c
c      c ...... the super-diagonals of the matrices are stored in
c               locations c(j,1) through c(j,ne-1).
c
c      y ...... the right hand side of the equations is stored in
c               y(j,1) through y(j,ne).
c
c      x ...... the solutions of the systems are returned in
c               locations x(j,1) through x(j,ne).
c
c      alpha .. work array dimensioned alpha(nd,ne)
c
c      gamma .. work array dimensioned gamma(nd,ne)
c
c       history:  based on a streamlined version of the old ncar
c                 ulib subr trdi used in the phoenix climate
c                 model of schneider and thompson (j.g.r., 1981).
c                 revised by starley thompson to solve multiple
c                 systems and vectorize well on the cray-1.
c                 later revised to include a parameter statement
c                 to define loop limits and thus enable cray short
c                 vector loops.
c
c       algorithm:  lu decomposition followed by solution.
c                   note: this subr executes satisfactorily
c                   if the input matrix is diagonally dominant
c                   and non-singular.  the diagonal elements are
c                   used to pivot, and no tests are made to determine
c                   singularity. if a singular or numerically singular
c                   matrix is used as input a divide by zero or
c                   floating point overflow will result.
c
c       last revision date:      4 february 1988
c
c
c Arguments
c
      integer ns,     ! number of systems to be solved.
     >        nd,     ! first dimension of arrays (ge ns)
     >        ne      ! number of unknowns in each system. (>2)
      
      double precision 
     >  a(nd,ne),     ! subdiagonals of matrices stored in a(j,2)...a(j,ne).
     >  b(nd,ne),     ! main diagonals of matrices stored in b(j,1)...b(j,ne).
     >  c(nd,ne),     ! super-diagonals of matrices stored in c(j,1)...c(j,ne-1).
     >  y(nd,ne),     ! right hand side of equations stored in y(j,1)...y(j,ne).
     >  x(nd,ne),     ! solutions of the systems returned in x(j,1)...x(j,ne).
     >  alpha(nd,ne), ! work array 
     >  gamma(nd,ne)  ! work array
c
c local variables
c
      integer nm1,    !
     >  j, i, ib      ! loop indices

c
      nm1 = ne-1
c
c obtain the lu decompositions
c
      do 10 j=1,ns
         alpha(j,1) = 1./b(j,1)
         gamma(j,1) = c(j,1)*alpha(j,1)
   10 continue
      do 11 i=2,nm1
         do 12 j=1,ns
            alpha(j,i) = 1./(b(j,i)-a(j,i)*gamma(j,i-1))
            gamma(j,i) = c(j,i)*alpha(j,i)
   12    continue
   11 continue
c
c solve
c
      do 20 j=1,ns
         x(j,1) = y(j,1)*alpha(j,1)
   20 continue
      do 21 i=2,nm1
         do 22 j=1,ns
            x(j,i) = (y(j,i)-a(j,i)*x(j,i-1))*alpha(j,i)
   22    continue
   21 continue
      do 23 j=1,ns
         x(j,ne) = (y(j,ne)-a(j,ne)*x(j,nm1))/
     >             (b(j,ne)-a(j,ne)*gamma(j,nm1))
   23 continue
      do 24 i=1,nm1
         ib = ne-i
         do 25 j=1,ns
            x(j,ib) = x(j,ib)-gamma(j,ib)*x(j,ib+1)
   25    continue
   24 continue
c
      return
      end
c

c ---------------------------------------------------------------------
      subroutine scopy (nt, arr, brr)
c ---------------------------------------------------------------------
c
c copies array arr to brr,for 1st nt words of arr
c
      implicit none
c
c Arguments
c
      integer nt     
      double precision arr(nt),    ! input
     >     brr(nt)     ! output
c
c Local variables
c
      integer ia
c
      do 100 ia = 1, nt
        brr(ia) = arr(ia)
 100  continue
c
      return
      end