c
c #####     ##    #####      #      ##     #####     #     ####   #    #
c #    #   #  #   #    #     #     #  #      #       #    #    #  ##   #
c #    #  #    #  #    #     #    #    #     #       #    #    #  # #  #
c #####   ######  #    #     #    ######     #       #    #    #  #  # #
c #   #   #    #  #    #     #    #    #     #       #    #    #  #   ##
c #    #  #    #  #####      #    #    #     #       #     ####   #    #
c
c ---------------------------------------------------------------------
      subroutine solsetWrapper(dim_params,
     >                         npoi_matrix,
     >                         indsol,
     >                         scalcoefl,
     >                         scalcoefu,
     >                         asurd,
     >                         asuri)

            integer          dim_params(3)

            double precision npoi_matrix(9, dim_params(1))

            double precision npoi_line1(dim_params(1))
            double precision npoi_line2(dim_params(1))
            double precision npoi_line3(dim_params(1))
            double precision npoi_line4(dim_params(1))
            double precision npoi_line5(dim_params(1))
            double precision npoi_line6(dim_params(1))
            double precision npoi_line7(dim_params(1))
            double precision npoi_line8(dim_params(1))
            double precision npoi_line9(dim_params(1))

            integer          indsol(dim_params(1))
            double precision scalcoefl(dim_params(1),4)
            double precision scalcoefu(dim_params(1),4)
            double precision asurd(dim_params(1),dim_params(2))
            double precision asuri(dim_params(1),dim_params(2))

c 	      Inicializando vetores double de tamanho npoi
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
102         CONTINUE
            
            call solset(dim_params(1), dim_params(2), dim_params(3),
     >                  npoi_line1, npoi_line2, npoi_line3, npoi_line4,
     >                  npoi_line5, npoi_line6, npoi_line7, npoi_line8,
     >                  npoi_line9,
     >                  indsol,
     >                  scalcoefl,
     >                  scalcoefu,
     >                  asurd,
     >                  asuri)
           
c 	      Inicializando vetores double de tamanho npoi
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
777         CONTINUE

      end 


      subroutine solset(npoi,
     >                  nband,
     >                  nsol,
     >                  coszen,
     >                  solg,
     >                  soli,
     >                  soll,
     >                  sols,
     >                  solsoi,
     >                  solu,
     >                  topparl,
     >                  topparu,
     >                  indsol,
     >                  scalcoefl,
     >                  scalcoefu,
     >                  asurd,
     >                  asuri)
c ---------------------------------------------------------------------
c
c zeros albedos and internal absorbed solar fluxes, and sets
c index for other solar routines. the index indsol, with number
c of points nsol, points to current 1d strip arrays whose coszen 
c values are gt 0 (indsol, nsol are in com1d)
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
      integer i

      integer          nband
      integer          npoi
      integer          nsol

      double precision coszen(npoi)
      double precision solg(npoi)
      double precision soli(npoi)
      double precision soll(npoi)
      double precision sols(npoi)
      double precision solsoi(npoi)
      double precision solu(npoi)
      double precision topparl(npoi)
      double precision topparu(npoi)

      integer          indsol(npoi)
      double precision scalcoefl(npoi,4)
      double precision scalcoefu(npoi,4)
      double precision asurd(npoi,nband)
      double precision asuri(npoi,nband)

c
c zero albedos returned just as a niceity
c
      call const (asurd, npoi*nband, DBLE(0.0))
      call const (asuri, npoi*nband, DBLE(0.0))
c
c zeros absorbed solar fluxes sol[u,s,l,g,i]1 since only points
c with +ve coszen will be set in solarf, and since
c sol[u,l,s,g,i]1 are summed over wavebands in solarf
c
c similarly zero par-related arrays set in solarf for turvap
c
      call const (solu, npoi, DBLE(0.0))
      call const (sols, npoi, DBLE(0.0))
      call const (soll, npoi, DBLE(0.0))
      call const (solg, npoi, DBLE(0.0))
      call const (soli, npoi, DBLE(0.0))
      call const (solsoi, npoi, DBLE(0.0))
c
      call const (topparu, npoi, DBLE(0.0))
      call const (topparl, npoi, DBLE(0.0))
c
c set canopy scaling coefficients for night-time conditions
c
      call const (scalcoefl, npoi*4, DBLE(0.0))
      call const (scalcoefu, npoi*4, DBLE(0.0))
c
c set index of points with positive coszen
c
      nsol = 0
c
      do 300 i = 1, npoi
        if (coszen(i).gt.0.) then
          nsol = nsol + 1
          indsol(nsol) = i
        endif
  300 continue
c

      return
      end
c


      subroutine solsurWrapper(local_params,
     >                          dim_params,
     >                          npoi_matrix,
     >                          indsol,
     >                          tmelt,
     >                          tsno,
     >                          wisoi,
     >                          wsoi)

            integer         local_params
      
            integer          dim_params(4)

            double precision npoi_matrix(7, dim_params(1))

            double precision npoi_line1(dim_params(1))
            double precision npoi_line2(dim_params(1))
            double precision npoi_line3(dim_params(1))
            double precision npoi_line4(dim_params(1))
            double precision npoi_line5(dim_params(1))
            double precision npoi_line6(dim_params(1))
            double precision npoi_line7(dim_params(1))

            integer          indsol(dim_params(1))
            double precision tmelt
            double precision tsno(dim_params(1),dim_params(4))
            double precision wisoi(dim_params(1),dim_params(3))
            double precision wsoi(dim_params(1),dim_params(3))

c 	      Inicializando vetores double de tamanho npoi
            DO 102, i = 1, dim_params(1), 1
                        npoi_line1(i) = npoi_matrix(1, i)
                        npoi_line2(i) = npoi_matrix(2, i)
                        npoi_line3(i) = npoi_matrix(3, i)
                        npoi_line4(i) = npoi_matrix(4, i)
                        npoi_line5(i) = npoi_matrix(5, i)
                        npoi_line6(i) = npoi_matrix(6, i)
                        npoi_line7(i) = npoi_matrix(7, i)
102         CONTINUE
            
            call solsur(local_params,
     >                  dim_params(1), dim_params(2), dim_params(3), dim_params(4),
     >                  npoi_line1, npoi_line2, npoi_line3, npoi_line4,
     >                  npoi_line5, npoi_line6, npoi_line7,
     >                  indsol,
     >                  tmelt,
     >                  tsno,
     >                  wisoi, 
     >                  wsoi)
            

c 	      Inicializando vetores double de tamanho npoi
            DO 777, i = 1, dim_params(1), 1
                        npoi_matrix(1, i) = npoi_line1(i)
                        npoi_matrix(2, i) = npoi_line2(i)
                        npoi_matrix(3, i) = npoi_line3(i)
                        npoi_matrix(4, i) = npoi_line4(i)
                        npoi_matrix(5, i) = npoi_line5(i)
                        npoi_matrix(6, i) = npoi_line6(i)
                        npoi_matrix(7, i) = npoi_line7(i)
777         CONTINUE

      end


c ---------------------------------------------------------------------
       subroutine solsur (ib,
     >                    npoi,
     >                    nsol,
     >                    nsoilay,
     >                    nsnolay,
     >                    albsan,
     >                    albsav,
     >                    albsnd,
     >                    albsni,
     >                    albsod,
     >                    albsoi,
     >                    coszen,
     >                    indsol,
     >                    tmelt,
     >                    tsno,
     >                    wisoi, 
     >                    wsoi)
c ---------------------------------------------------------------------
c
c sets surface albedos for soil and snow, prior to other
c solar calculations
c
c ib = waveband number
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'com1d.h'
c
c input variable
c
      integer ib    ! waveband number. 1 = visible, 2 = near IR
c
c local variables
c     
      integer j,    ! loop indice on number of points with >0 coszen
     >        i     ! indice of point in (1, npoi) array. 
c
      double precision a7svlo,  ! snow albedo at low threshold temp., visible
     >     a7snlo,  !                                   , near IR
     >     a7svhi,  !                 high              , visible
     >     a7snhi,  !                                   , near-IR
     >     t7shi,   ! high threshold temperature for snow albed
     >     t7slo,   ! low  threshold temperature for snow albedo
     >     dinc,    ! albedo correction du to soil moisture
     >     zw       ! liquid moisture content

      double precision x(npoi), zfac(npoi)
       
c     globals

       integer npoi
       integer nsol
       integer nsoilay
       integer nsnolay

       double precision albsan(npoi)
       double precision albsav(npoi)
       double precision albsnd(npoi)
       double precision albsni(npoi)
       double precision albsod(npoi)
       double precision albsoi(npoi)
       double precision coszen(npoi)

       integer          indsol(npoi)
       double precision tmelt
       double precision tsno(npoi,nsnolay)
       double precision wisoi(npoi,nsoilay)
       double precision wsoi(npoi,nsoilay)

c
c set the "standard" snow values:
c
      data a7svlo, a7svhi /0.90, 0.70/
      data a7snlo, a7snhi /0.60, 0.40/
c
c     t7shi ... high threshold temperature for snow albedo
c     t7slo ... low  threshold temperature for snow albedo
c
      t7shi = tmelt
      t7slo = tmelt - 15.0
c
c do nothing if all points in current strip have coszen le 0
c
      
      if (nsol.eq.0) then
        return
      endif

      
c
      if (ib.eq.1) then
c
c soil albedos (visible waveband)
c
        do 100 j = 1, nsol
c
          i = indsol(j)
c
c change the soil albedo as a function of soil moisture
c
          zw = wsoi(i,1) * (1.-wisoi(i,1))
c
          dinc = 1.0 + 1.0 * min (1., max (0.0, 1. - (zw /.50) ))
c
          albsod(i) = min (albsav(i) * dinc, .80)
          albsoi(i) = albsod(i)
c
  100   continue
c
c snow albedos (visible waveband)
c
        do 110 j = 1, nsol
c
          i = indsol(j)
c
          x(i) = (a7svhi*(tsno(i,1)-t7slo) + a7svlo*(t7shi-tsno(i,1)))
     >           / (t7shi-t7slo)
c
          x(i) = min (a7svlo, max (a7svhi, x(i)))
c
          zfac(i)   = max ( 0., 1.5 / (1.0 + 4.*coszen(i)) - 0.5 )
          albsnd(i) = min (0.99, x(i) + (1.-x(i))*zfac(i))
          albsni(i) = min (1., x(i))
c
  110   continue
c
      else
c
c soil albedos (near-ir waveband)
c
        do 200 j = 1, nsol
          i = indsol(j)
c
c lsx.2 formulation (different from lsx.1)
c
          zw = wsoi(i,1) * (1. - wisoi(i,1))
c
          dinc = 1.0 + 1.0 * min (1., max (0.0, 1.0 - (zw / .50)  ))
c
          albsod(i) = min (albsan(i) * dinc, .80)
          albsoi(i) = albsod(i)
c
  200   continue
c
c snow albedos (near-ir waveband)
c
        do 210 j = 1, nsol
c
          i = indsol(j)
c
          x(i) = (a7snhi*(tsno(i,1)-t7slo) + a7snlo*(t7shi-tsno(i,1)))
     >           / (t7shi-t7slo)
          x(i) = min (a7snlo, max (a7snhi, x(i)))
c
          zfac(i) = max ( 0., 1.5/(1.+4.*coszen(i)) - 0.5 )
c
          albsnd(i) = min (0.99, x(i) + (1.-x(i))*zfac(i))
          albsni(i) = min (1., x(i))
c
  210   continue
c
      endif
c


      return
      end
c
c


      subroutine solarfWrapper(local_params,
     >                          dim_params,
     >                          double_params,
     >                          npoi_matrix,
     >                          indsol,
     >                          lai, 
     >                          terml, 
     >                          termu,
     >                          solad,
     >                          solai,
     >                          sai, 
     >                          scalcoefl,
     >                          scalcoefu) 


            integer          local_params
                             
            integer          dim_params(3)

            double precision double_params

            double precision npoi_matrix(28, dim_params(1))

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
            double precision npoi_line15(dim_params(1))
            double precision npoi_line16(dim_params(1))
            double precision npoi_line17(dim_params(1))
            double precision npoi_line18(dim_params(1))
            double precision npoi_line19(dim_params(1))
            double precision npoi_line20(dim_params(1))
            double precision npoi_line21(dim_params(1))
            double precision npoi_line22(dim_params(1)) 
            double precision npoi_line23(dim_params(1))
            double precision npoi_line24(dim_params(1))
            double precision npoi_line25(dim_params(1))
            double precision npoi_line26(dim_params(1))
            double precision npoi_line27(dim_params(1))
            double precision npoi_line28(dim_params(1))

            integer          indsol(dim_params(1))
            double precision lai(dim_params(1),2)
            double precision terml(dim_params(1),7)
            double precision termu(dim_params(1),7)
            double precision solad(dim_params(1),dim_params(3))
            double precision solai(dim_params(1),dim_params(3))
            double precision sai(dim_params(1),2)
            double precision scalcoefl(dim_params(1),4)
            double precision scalcoefu(dim_params(1),4)

            integer i
            
c 	      Inicializando vetores double de tamanho npoi
            DO 102, i = 1, dim_params(1), 1
                        npoi_line1(i)  = npoi_matrix(1, i)
                        npoi_line2(i)  = npoi_matrix(2, i)
                        npoi_line3(i)  = npoi_matrix(3, i)
                        npoi_line4(i)  = npoi_matrix(4, i)
                        npoi_line5(i)  = npoi_matrix(5, i)
                        npoi_line6(i)  = npoi_matrix(6, i)
                        npoi_line7(i)  = npoi_matrix(7, i)
                        npoi_line8(i)  = npoi_matrix(8, i)
                        npoi_line9(i)  = npoi_matrix(9, i)
                        npoi_line10(i) = npoi_matrix(10, i)
                        npoi_line11(i) = npoi_matrix(11, i)
                        npoi_line12(i) = npoi_matrix(12, i)
                        npoi_line13(i) = npoi_matrix(13, i)
                        npoi_line14(i) = npoi_matrix(14, i)
                        npoi_line15(i) = npoi_matrix(15, i)
                        npoi_line16(i) = npoi_matrix(16, i)
                        npoi_line17(i) = npoi_matrix(17, i)
                        npoi_line18(i) = npoi_matrix(18, i)
                        npoi_line19(i) = npoi_matrix(19, i)
                        npoi_line20(i) = npoi_matrix(20, i)
                        npoi_line21(i) = npoi_matrix(21, i)
                        npoi_line22(i) = npoi_matrix(22, i)
                        npoi_line23(i) = npoi_matrix(23, i)
                        npoi_line24(i) = npoi_matrix(24, i)
                        npoi_line25(i) = npoi_matrix(25, i)
                        npoi_line26(i) = npoi_matrix(26, i)
                        npoi_line27(i) = npoi_matrix(27, i)
                        npoi_line28(i) = npoi_matrix(28, i)
102         CONTINUE


            call solarf (local_params,
     >                   dim_params(1), dim_params(2), dim_params(3),
     >                   double_params,
     >                   npoi_line1 ,
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
     >                   npoi_line15,
     >                   npoi_line16,
     >                   npoi_line17,
     >                   npoi_line18,
     >                   npoi_line19,
     >                   npoi_line20,
     >                   npoi_line21,
     >                   npoi_line22,
     >                   npoi_line23,
     >                   npoi_line24,
     >                   npoi_line25,
     >                   npoi_line26,
     >                   npoi_line27,
     >                   npoi_line28,
     >                   indsol, 
     >                   lai, 
     >                   terml, 
     >                   termu, 
     >                   solad, 
     >                   solai, 
     >                   sai, 
     >                   scalcoefl, 
     >                   scalcoefu) 

c 	      Inicializando vetores double de tamanho npoi
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
                        npoi_matrix(15, i) = npoi_line15(i)
                        npoi_matrix(16, i) = npoi_line16(i)
                        npoi_matrix(17, i) = npoi_line17(i)
                        npoi_matrix(18, i) = npoi_line18(i)
                        npoi_matrix(19, i) = npoi_line19(i)
                        npoi_matrix(20, i) = npoi_line20(i)
                        npoi_matrix(21, i) = npoi_line21(i)
                        npoi_matrix(22, i) = npoi_line22(i)
                        npoi_matrix(23, i) = npoi_line23(i)
                        npoi_matrix(24, i) = npoi_line24(i)
                        npoi_matrix(25, i) = npoi_line25(i)
                        npoi_matrix(26, i) = npoi_line26(i)
                        npoi_matrix(27, i) = npoi_line27(i)
                        npoi_matrix(28, i) = npoi_line28(i)
777         CONTINUE


            
      end


c ---------------------------------------------------------------------
      subroutine solarf (ib,
     >                   npoi,
     >                   nsol,
     >                   nband,
     >                   epsilon,
     >                   ablod,
     >                   abloi,
     >                   abupd,
     >                   abupi,
     >                   albsnd,
     >                   albsni,
     >                   albsod,
     >                   albsoi,
     >                   fl,
     >                   flodd,
     >                   flodi,
     >                   floii,
     >                   fu,
     >                   fupdd,
     >                   fupdi,
     >                   fupii,
     >                   sol2d,
     >                   sol2i,
     >                   sol3d,
     >                   sol3i,
     >                   solg,
     >                   soli,
     >                   soll,
     >                   sols,
     >                   solsoi,
     >                   solu,
     >                   topparl,
     >                   topparu,
     >                   indsol,
     >                   lai,
     >                   terml,
     >                   termu,
     >                   solad,
     >                   solai,
     >                   sai,
     >                   scalcoefl,
     >                   scalcoefu)

c ---------------------------------------------------------------------
c
c calculates solar fluxes absorbed by upper and lower stories,
c soil and snow
c
c zenith angles are in comatm array coszen, and must be the same
c as supplied earlier to solalb
c
c solarf uses the results obtained earlier by solalb and 
c stored in com1d arrays. the absorbed fluxes are returned in
c com1d arrays sol[u,s,l,g,i]
c
c the procedure is first to calculate the upper-story absorbed
c fluxes and fluxes below the upper story, then the lower-story
c absorbed fluxes and fluxes below the lower story, then fluxes
c absorbed by the soil and snow
c
c ib = waveband number
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c      include 'com1d.h'
c 
c Arguments
c 
      integer ib     ! waveband number (1= visible, 2= near-IR)
c
c local variables
c     
      integer j,    ! loop indice on number of points with >0 coszen
     >        i     ! indice of point in (1, npoi) array. 
c
      double precision x, y, xd, xi, 
     >     xaiu,    ! total single-sided lai+sai, upper
     >     xail     ! total single-sided lai+sai, lower

c globals

       integer npoi
       integer nsol
       integer nband
       
       double precision epsilon
       
       ! 28
       double precision ablod(npoi)
       double precision abloi(npoi)
       double precision abupd(npoi)
       double precision abupi(npoi)
       double precision albsnd(npoi)
       double precision albsni(npoi)
       double precision albsod(npoi)
       double precision albsoi(npoi)
       double precision fl(npoi)
       double precision flodd(npoi)
       double precision flodi(npoi)
       double precision floii(npoi)
       double precision fu(npoi)
       double precision fupdd(npoi)
       double precision fupdi(npoi)
       double precision fupii(npoi)
       double precision sol2d(npoi)
       double precision sol2i(npoi)
       double precision sol3d(npoi)
       double precision sol3i(npoi)
       double precision solg(npoi)
       double precision soli(npoi)
       double precision soll(npoi)
       double precision sols(npoi)
       double precision solsoi(npoi)
       double precision solu(npoi)
       double precision topparl(npoi)
       double precision topparu(npoi)
  
       integer          indsol(npoi)
       double precision lai(npoi,2)
       double precision terml(npoi,7)
       double precision termu(npoi,7)
       double precision solad(npoi,nband)
       double precision solai(npoi,nband)
       double precision sai(npoi,2)
       double precision scalcoefl(npoi,4)
       double precision scalcoefu(npoi,4)

c
c do nothing if all points in current strip have coszen le 0
c
      if (nsol.eq.0) return
c
c (f) calculate fluxes absorbed by upper leaves and stems,
c     and downward fluxes below upper veg, using unit-flux
c     results of solalb(c) (apportion absorbed flux between
c     leaves and stems in proportion to their lai and sai)
c
      do 600 j=1,nsol
c
        i = indsol(j)
        x = solad(i,ib)*abupd(i) + solai(i,ib)*abupi(i)
        y = lai(i,2) / max (lai(i,2)+sai(i,2), epsilon)
        solu(i) = solu(i) + x * y
        sols(i) = sols(i) + x * (1.-y)
        sol2d(i) = solad(i,ib)*fupdd(i)
        sol2i(i) = solad(i,ib)*fupdi(i) + solai(i,ib)*fupii(i)
c
  600 continue
c
c (g) areally average fluxes to lower veg, soil, snow
c
      do 700 j=1,nsol
c
        i = indsol(j)
        sol3d(i) = fu(i)*sol2d(i) + (1.-fu(i))*solad(i,ib)
        sol3i(i) = fu(i)*sol2i(i) + (1.-fu(i))*solai(i,ib)
c
  700 continue
c
c (h,i) calculate fluxes absorbed by lower veg, snow-free soil
c       and snow, using results of (g) and unit-flux results
c       of solalb(a)
c
      do 800 j=1,nsol
c
        i = indsol(j)
        soll(i) = soll(i) + sol3d(i)*ablod(i) + sol3i(i)*abloi(i)

c
        xd = (fl(i)*flodd(i) + 1.-fl(i)) * sol3d(i)
c
        xi = fl(i)*(sol3d(i)*flodi(i) + sol3i(i)*floii(i))
     >       + (1.-fl(i)) * sol3i(i)
c
        solg(i) = solg(i)
     >            + (1.-albsod(i))*xd + (1.-albsoi(i))*xi
c
	solsoi(i) = solsoi(i)+ xd+xi
c
        soli(i) = soli(i) 
     >            + (1.-albsnd(i))*sol3d(i)
     >            + (1.-albsni(i))*sol3i(i)
c
  800 continue
c
c estimate absorbed pars at top of canopy, toppar[u,l] and
c some canopy scaling parameters
c
c this neglects complications due to differing values of dead vs 
c live elements, averaged into rhoveg, tauveg in vegdat, and 
c modifications of omega due to intercepted snow in twoset
c
c do only for visible band (ib=1)
c
      if (ib.eq.1) then
c
        do 900 j = 1, nsol
c
          i = indsol(j)
c
c the canopy scaling algorithm assumes that the net photosynthesis
c is proportional to absored par (apar) during the daytime. during night,
c the respiration is scaled using a 10-day running-average daytime canopy
c scaling parameter.
c
c apar(x) = A exp(-k x) + B exp(-h x) + C exp(h x)
c
c some of the required terms (i.e. term[u,l] are calculated in the subroutine 'twostr'.
c in the equations below, 
c
c   A = scalcoefu(i,1) = term[u,l](i,1) * ipardir(0)
c   B = scalcoefu(i,2) = term[u,l](i,2) * ipardir(0) + term[u,l](i,3) * ipardif(0)
c   C = scalcoefu(i,3) = term[u,l](i,4) * ipardir(0) + term[u,l](i,5) * ipardif(0)
c   A + B + C = scalcoefu(i,4) = also absorbed par at canopy of canopy by leaves & stems
c
c upper canopy:
c
c total single-sided lai+sai
c
          xaiu = max (lai(i,2)+sai(i,2), epsilon)
c
c some terms required for use in canopy scaling:
c
          scalcoefu(i,1) = termu(i,1) * solad(i,ib)
c
          scalcoefu(i,2) = termu(i,2) * solad(i,ib) +
     >                     termu(i,3) * solai(i,ib)
c
          scalcoefu(i,3) = termu(i,4) * solad(i,ib) +
     >                     termu(i,5) * solai(i,ib)
c
          scalcoefu(i,4) = scalcoefu(i,1) + 
     >                     scalcoefu(i,2) + 
     >                     scalcoefu(i,3)
c
c apar of the "top" leaves of the canopy
c
          topparu(i) = scalcoefu(i,4) * lai(i,2) / xaiu
c
c lower canopy:
c
c total single-sided lai+sai
c
          xail = max (lai(i,1)+sai(i,1), epsilon)
c
c some terms required for use in canopy scaling:
c
          scalcoefl(i,1) = terml(i,1) * sol3d(i)
c
          scalcoefl(i,2) = terml(i,2) * sol3d(i) +
     >                     terml(i,3) * sol3i(i)
c
          scalcoefl(i,3) = terml(i,4) * sol3d(i) +
     >                     terml(i,5) * sol3i(i)
c
          scalcoefl(i,4) = scalcoefl(i,1) +
     >                     scalcoefl(i,2) +
     >                     scalcoefl(i,3)
c
c apar of the "top" leaves of the canopy
c
          topparl(i) = scalcoefl(i,4) * lai(i,1) / xail
c
  900   continue
c
      endif
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


      subroutine irradWrapper(dim_params,
     >                        double_params,
     >                        npoi_matrix,
     >                        lai,
     >                        sai)


            integer dim_params

            double precision double_params

            double precision npoi_matrix(15, dim_params)

            double precision npoi_line1(dim_params)
            double precision npoi_line2(dim_params)
            double precision npoi_line3(dim_params)
            double precision npoi_line4(dim_params)
            double precision npoi_line5(dim_params)
            double precision npoi_line6(dim_params)
            double precision npoi_line7(dim_params)
            double precision npoi_line8(dim_params)
            double precision npoi_line9(dim_params)
            double precision npoi_line10(dim_params)
            double precision npoi_line11(dim_params)
            double precision npoi_line12(dim_params)
            double precision npoi_line13(dim_params)
            double precision npoi_line14(dim_params)
            double precision npoi_line15(dim_params)

            double precision lai(dim_params, 2)
            double precision sai(dim_params, 2)

            integer i

c 	      Inicializando vetores double de tamanho npoi
            DO 102, i = 1, dim_params, 1
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
                        npoi_line15(i) = npoi_matrix(15, i)
102         CONTINUE

            call irrad(dim_params, double_params,
     >                 npoi_line1 ,
     >                 npoi_line2 ,
     >                 npoi_line3 ,
     >                 npoi_line4 ,
     >                 npoi_line5 ,
     >                 npoi_line6 ,
     >                 npoi_line7 ,
     >                 npoi_line8 ,
     >                 npoi_line9 ,
     >                 npoi_line10,
     >                 npoi_line11,
     >                 npoi_line12,
     >                 npoi_line13,
     >                 npoi_line14,
     >                 npoi_line15,
     >                 lai, sai)

c 	      Inicializando vetores double de tamanho npoi
            DO 777, i = 1, dim_params, 1
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
                        npoi_matrix(15, i) = npoi_line15(i)
777         CONTINUE

      end


c ---------------------------------------------------------------------
      subroutine irrad(npoi,
     >                 stef,
     >                 fi,
     >                 fira,
     >                 firb,
     >                 firg,
     >                 firi,
     >                 firl,
     >                 firs,
     >                 firu,
     >                 fl,
     >                 fu,
     >                 tg,
     >                 ti,
     >                 tl,
     >                 ts,
     >                 tu, 
     >                 lai,
     >                 sai)
c ---------------------------------------------------------------------
c
c calculates overall emitted ir flux, and net absorbed minus
c emitted ir fluxes for upper leaves, upper stems, lower story,
c soil and snow. assumes upper leaves, upper stems and lower
c story each form a semi-transparent plane, with the upper-leaf
c plane just above the upper-stem plane. the soil and snow 
c surfaces have emissivities of 0.95.
c
c the incoming flux is supplied in comatm array fira
c
c the emitted ir flux by overall surface system is returned in
c com1d array firb - the ir fluxes absorbed by upper leaves,
c upper stems, lower veg, soil and snow are returned in com1d 
c arrays firu, firs, firl, firg and firi
c 
c other com1d arrays used are:
c
c emu, ems, eml  = emissivities of the vegetation planes
c fup, fdown     = upward and downward fluxes below tree level
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
c Local arrays:
c
      integer i           ! loop indice
c
      double precision emisoil,       ! soil emissivity
     >     emisnow,       ! snow emissivity
     >     avmuir         ! average diffuse optical depth
c
      double precision emu(npoi),     ! ir emissivity of upper-leaves veg plane
     >     ems(npoi),     ! ir emissivity of upper-stems veg plane
     >     eml(npoi),     ! ir emissivity of lower-story veg plane
     >     emg(npoi),     ! ir emissivity (gray) of soil surface
     >     emi(npoi),     ! ir emissivity (gray) of snow surface
     >     fdown(npoi),   ! downward ir flux below tree level per overall area
     >     fdowng(npoi),  ! upward   ir flux below tree level per overall area
     >     fup(npoi),     ! downward ir flux below lower-story veg
     >     fupg(npoi),    ! upward   ir flux below lower-story veg
     >     fupgb(npoi),   ! upward   ir flux above bare soil surface
     >     fupi(npoi)     ! upward   ir flux above snow surface
    
        integer          npoi

        double precision stef

        double precision fi(npoi)
        double precision fira(npoi)
        double precision firb(npoi)
        double precision firg(npoi)
        double precision firi(npoi)
        double precision firl(npoi)
        double precision firs(npoi)
        double precision firu(npoi)
        double precision fl(npoi)
        double precision fu(npoi)
        double precision tg(npoi)
        double precision ti(npoi)
        double precision tl(npoi)
        double precision ts(npoi)
        double precision tu(npoi) 

        double precision lai(npoi,2)
        double precision sai(npoi,2)

c
c set emissivities of soil and snow
c
      data emisoil, emisnow
     >    /0.95, 0.95/
c
c use uniform value 1.0 for average diffuse optical depth
c (although an array for solar, all values are set to 1 in twoset).
c
c     save avmuir
      data avmuir /1./

      
c
      do 100 i=1,npoi
c
        emu(i) = 1. - exp ( -lai(i,2) / avmuir )
        ems(i) = 1. - exp ( -sai(i,2) / avmuir )
        eml(i) = 1. - exp ( -(lai(i,1)+sai(i,1)) / avmuir )
c
        emg(i) = emisoil
        emi(i) = emisnow
c
        fdown(i) =  (1.-fu(i)) * fira(i)
     >            + fu(i) * ( (1.-emu(i))*(1.-ems(i))*fira(i)
     >                       +    emu(i)* (1.-ems(i))*stef*(tu(i)**4)
     >                       +    ems(i)*stef*(ts(i)**4) )
c
        fdowng(i) = (1.-eml(i))*fdown(i)  + eml(i)*stef*(tl(i)**4)
c
        fupg(i)   = (1.-emg(i))*fdowng(i) + emg(i)*stef*(tg(i)**4)
c
        fupgb(i)  = (1.-emg(i))*fdown(i)  + emg(i)*stef*(tg(i)**4)
c
        fupi(i)   = (1.-emi(i))*fdown(i)  + emi(i)*stef*(ti(i)**4)
c
        fup(i) = (1.-fi(i))*(      fl(i)*(       eml(i) *stef*(tl(i)**4)
     >                                     + (1.-eml(i))*fupg(i) )
     >                        +(1.-fl(i))*fupgb(i)
     >                      )
     >         +     fi(i) * fupi(i)
c       


        firb(i) =   (1.-fu(i)) * fup(i)
     >            + fu(i)  * ( (1.-emu(i))*(1.-ems(i))*fup(i)
     >                        +    emu(i)*stef*(tu(i)**4)
     >                        +    ems(i)*(1.-emu(i))*stef*(ts(i)**4) )
c
        firu(i) =   emu(i)*ems(i)*stef*(ts(i)**4)
     >            + emu(i)*(1.-ems(i))*fup(i)
     >            + emu(i)*fira(i)
     >            - 2*emu(i)*stef*(tu(i)**4)
c
        firs(i) =   ems(i)*emu(i)*stef*(tu(i)**4)
     >            + ems(i)*fup(i)
     >            + ems(i)*(1.-emu(i))*fira(i)
     >            - 2*ems(i)*stef*(ts(i)**4)
c
        firl(i) =   eml(i)*fdown(i)
     >            + eml(i)*fupg(i)
     >            - 2*eml(i)*stef*(tl(i)**4)
c
        firg(i) =       fl(i)  * (fdowng(i) - fupg(i))
     >            + (1.-fl(i)) * (fdown(i)  - fupgb(i))
c
        firi(i) =   fdown(i) - fupi(i)
c
  100 continue
c
      return
      end


      subroutine solalbWrapper(local_params,
     >                         dim_params,
     >                         double_params,
     >                         npoi_matrix,
     >                         indsol,
     >                         orieh,
     >                         oriev,
     >                         asurd,
     >                         asuri,
     >                         lai,
     >                         sai,
     >                         terml,
     >                         termu)

                  integer local_params

                  integer dim_params(3)

                  double precision double_params(3)

                  integer indsol(dim_params(1))
                  double precision orieh(2)
                  double precision oriev(2)
                  double precision asurd(dim_params(1),dim_params(2))
                  double precision asuri(dim_params(1),dim_params(2))
                  double precision lai(dim_params(1),2)
                  double precision sai(dim_params(1),2)
                  double precision terml(dim_params(1),7)
                  double precision termu(dim_params(1),7)

                  double precision npoi_matrix(33, dim_params(1))
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
                  double precision npoi_line15(dim_params(1))
                  double precision npoi_line16(dim_params(1))
                  double precision npoi_line17(dim_params(1))
                  double precision npoi_line18(dim_params(1))
                  double precision npoi_line19(dim_params(1))
                  double precision npoi_line20(dim_params(1))
                  double precision npoi_line21(dim_params(1))
                  double precision npoi_line22(dim_params(1))
                  double precision npoi_line23(dim_params(1))
                  double precision npoi_line24(dim_params(1))
                  double precision npoi_line25(dim_params(1))
                  double precision npoi_line26(dim_params(1))
                  double precision npoi_line27(dim_params(1))
                  double precision npoi_line28(dim_params(1))
                  double precision npoi_line29(dim_params(1))
                  double precision npoi_line30(dim_params(1))
                  double precision npoi_line31(dim_params(1))
                  double precision npoi_line32(dim_params(1))
                  double precision npoi_line33(dim_params(1))

c 	      Inicializando vetores double de tamanho npoi
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
                        npoi_line15(i) = npoi_matrix(15, i)
                        npoi_line16(i) = npoi_matrix(16, i)
                        npoi_line17(i) = npoi_matrix(17, i)
                        npoi_line18(i) = npoi_matrix(18, i)
                        npoi_line19(i) = npoi_matrix(19, i)
                        npoi_line20(i) = npoi_matrix(20, i)
                        npoi_line21(i) = npoi_matrix(21, i)
                        npoi_line22(i) = npoi_matrix(22, i)
                        npoi_line23(i) = npoi_matrix(23, i)
                        npoi_line24(i) = npoi_matrix(24, i)
                        npoi_line25(i) = npoi_matrix(25, i)
                        npoi_line26(i) = npoi_matrix(26, i)
                        npoi_line27(i) = npoi_matrix(27, i)
                        npoi_line28(i) = npoi_matrix(28, i)
                        npoi_line29(i) = npoi_matrix(29, i)
                        npoi_line30(i) = npoi_matrix(30, i)
                        npoi_line31(i) = npoi_matrix(31, i)
                        npoi_line32(i) = npoi_matrix(32, i)
                        npoi_line33(i) = npoi_matrix(33, i)
102         CONTINUE

            call solalb (local_params,
     >                    dim_params(1), dim_params(2), dim_params(3),
     >                    double_params(1), double_params(2), double_params(3),
     >                    npoi_line1,
     >                    npoi_line2,
     >                    npoi_line3,
     >                    npoi_line4,
     >                    npoi_line5,
     >                    npoi_line6,
     >                    npoi_line7,
     >                    npoi_line8,
     >                    npoi_line9,
     >                    npoi_line10,
     >                    npoi_line11,
     >                    npoi_line12,
     >                    npoi_line13,
     >                    npoi_line14,
     >                    npoi_line15,
     >                    npoi_line16,
     >                    npoi_line17,
     >                    npoi_line18,
     >                    npoi_line19,
     >                    npoi_line20,
     >                    npoi_line21,
     >                    npoi_line22,
     >                    npoi_line23,
     >                    npoi_line24,
     >                    npoi_line25,
     >                    npoi_line26,
     >                    npoi_line27,
     >                    npoi_line28,
     >                    npoi_line29,
     >                    npoi_line30,
     >                    npoi_line31,
     >                    npoi_line32,
     >                    npoi_line33,
     >                         indsol,
     >                         orieh,
     >                         oriev,
     >                         asurd,
     >                         asuri,
     >                         lai,
     >                         sai,
     >                         terml,
     >                         termu)

c 	      Inicializando vetores double de tamanho npoi
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
                        npoi_matrix(15, i) = npoi_line15(i)
                        npoi_matrix(16, i) = npoi_line16(i)
                        npoi_matrix(17, i) = npoi_line17(i)
                        npoi_matrix(18, i) = npoi_line18(i)
                        npoi_matrix(19, i) = npoi_line19(i)
                        npoi_matrix(20, i) = npoi_line20(i)
                        npoi_matrix(21, i) = npoi_line21(i)
                        npoi_matrix(22, i) = npoi_line22(i)
                        npoi_matrix(23, i) = npoi_line23(i)
                        npoi_matrix(24, i) = npoi_line24(i)
                        npoi_matrix(25, i) = npoi_line25(i)
                        npoi_matrix(26, i) = npoi_line26(i)
                        npoi_matrix(27, i) = npoi_line27(i)
                        npoi_matrix(28, i) = npoi_line28(i)
                        npoi_matrix(29, i) = npoi_line29(i)
                        npoi_matrix(30, i) = npoi_line30(i)
                        npoi_matrix(31, i) = npoi_line31(i)
                        npoi_matrix(32, i) = npoi_line32(i)
                        npoi_matrix(33, i) = npoi_line33(i)
777         CONTINUE

      end


c ---------------------------------------------------------------------
      subroutine solalb (ib,
     >                   npoi, nband, nsol,
     >                   epsilon,
     >                   tmelt,
     >                   pi,
     >                   ablod,
     >                   abloi,
     >                   abupd,
     >                   abupi,
     >                   albsnd,
     >                   albsni,
     >                   albsod,
     >                   albsoi,
     >                   coszen,
     >                   dummy,
     >                   fi,
     >                   fl,
     >                   flodd,
     >                   flodi,
     >                   floii,
     >                   fu ,
     >                   fupdd,
     >                   fupdi,
     >                   fupii,
     >                   relod,
     >                   reloi,
     >                   reupd,
     >                   reupi,
     >                   fwetl,
     >                   fwets,
     >                   fwetu,
     >                   greenfracl,
     >                   rliql,
     >                   rliqs,
     >                   rliqu,
     >                   tl,
     >                   ts,
     >                   tu,
     >                   indsol,
     >                   orieh,
     >                   oriev,
     >                   asurd,
     >                   asuri,
     >                   lai,
     >                   sai,
     >                   terml,
     >                   termu)
c ---------------------------------------------------------------------
c
c calculates effective albedos of the surface system,
c separately for unit incoming direct and diffuse flux -- the 
c incoming direct zenith angles are supplied in comatm array 
c coszen, and the effective albedos are returned in comatm
c arrays asurd, asuri -- also detailed absorbed and reflected flux
c info is stored in com1d arrays, for later use by solarf
c
c the procedure is first to calculate the grass+soil albedos,
c then the tree + (grass+soil+snow) albedos. the labels
c (a) to (d) correspond to those in the description doc
c
       implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c      include 'com1d.h'
c 
c Arguments
c 
      integer ib     ! waveband number (1= visible, 2= near-IR)

c globals

      integer npoi, nband, nsol

      double precision epsilon
      double precision tmelt
      double precision pi
      double precision ablod(npoi)
      double precision abloi(npoi)
      double precision abupd(npoi)
      double precision abupi(npoi)
      double precision albsnd(npoi)
      double precision albsni(npoi)
      double precision albsod(npoi)
      double precision albsoi(npoi)
      double precision coszen(npoi)
      double precision dummy(npoi)
      double precision fi(npoi)
      double precision fl(npoi)
      double precision flodd(npoi)
      double precision flodi(npoi)
      double precision floii(npoi)
      double precision fu(npoi)
      double precision fupdd(npoi)
      double precision fupdi(npoi)
      double precision fupii(npoi)
      double precision relod(npoi)
      double precision reloi(npoi)
      double precision reupd(npoi)
      double precision reupi(npoi)
      double precision fwetl(npoi)
      double precision fwets(npoi)
      double precision fwetu(npoi)
      double precision greenfracl(npoi)
      double precision rliql(npoi)
      double precision rliqs(npoi)
      double precision rliqu(npoi)
      double precision tl(npoi)
      double precision ts(npoi)
      double precision tu(npoi)

      integer          indsol(npoi)
      double precision orieh(2)
      double precision oriev(2)
      double precision asurd(npoi,nband)
      double precision asuri(npoi,nband)
      double precision lai(npoi,2)
      double precision sai(npoi,2)
      double precision terml(npoi,7)
      double precision termu(npoi,7)



c
c local variables
c     
      integer j,    ! loop indice on number of points with >0 coszen
     >        i     ! indice of point in (1, npoi) array. 
c
c do nothing if all points in current strip have coszen le 0
c
      if (nsol.eq.0) return
c
c (a) obtain albedos, etc, for two-stream lower veg + soil
c     system, for direct and diffuse incoming unit flux
c

      do 100 j = 1, nsol
c
        i = indsol(j)
c
        asurd(i,ib) = albsod(i)
        asuri(i,ib) = albsoi(i)
c
100   continue
c
     
      
      call twostr (ablod, abloi, relod, reloi, flodd, dummy,
     >             flodi, floii,  asurd, asuri, 1, coszen, ib,
     >             epsilon,
     >             indsol,
     >             lai,
     >             nband,
     >             npoi,
     >             nsol,
     >             sai,
     >             terml,
     >             termu,
     >             fwetl,
     >             fwets,
     >             fwetu,
     >             greenfracl,
     >             orieh,
     >             oriev,
     >             pi,
     >             rliql,
     >             rliqs,
     >             rliqu,
     >             tl,
     >             tmelt,
     >             ts,
     >             tu)
        
c
c (b) areally average surface albedos (lower veg, soil, snow)
c
      do 200 j = 1, nsol
c
        i = indsol(j)
c
        asurd(i,ib) = fl(i)*(1.-fi(i))*relod(i)
     >              + (1.-fl(i))*(1.-fi(i))*albsod(i)
     >              + fi(i)*albsnd(i)    
c
        asuri(i,ib) = fl(i)*(1.-fi(i))*reloi(i)
     >              + (1.-fl(i))*(1.-fi(i))*albsoi(i)
     >              + fi(i)*albsni(i)    
c
200   continue

c
c (c) obtain albedos, etc, for two-stream upper veg + surface
c     system, for direct and diffuse incoming unit flux
c
      
      call twostr (abupd, abupi, reupd, reupi, fupdd, dummy,
     >             fupdi, fupii, asurd, asuri, 2, coszen, ib,
     >             epsilon,
     >             indsol,
     >             lai,
     >             nband,
     >             npoi,
     >             nsol,
     >             sai,
     >             terml,
     >             termu,
     >             fwetl,
     >             fwets,
     >             fwetu,
     >             greenfracl,
     >             orieh,
     >             oriev,
     >             pi,
     >             rliql,
     >             rliqs,
     >             rliqu,
     >             tl,
     >             tmelt,
     >             ts,
     >             tu)
      
      
     
c
c (d) calculate average overall albedos 
c
      do 300 j = 1, nsol
c
        i = indsol(j)
c
        asurd(i,ib) = fu(i)*reupd(i)
     >              + (1.-fu(i))*asurd(i,ib)
c
        asuri(i,ib) = fu(i)*reupi(i)
     >              + (1.-fu(i))*asuri(i,ib)
c
  300 continue
c
      return
      end



c ---------------------------------------------------------------------
      subroutine twoset (omega, betad, betai, avmu, gdir,
     >                   coszen, iv, ib,
     >                   epsilon,
     >                   fwetl,
     >                   fwets,
     >                   fwetu,
     >                   greenfracl,
     >                   indsol,
     >                   lai,
     >                   nband,
     >                   npoi,
     >                   nsol,
     >                   orieh,
     >                   oriev,
     >                   pi,
     >                   rliql,
     >                   rliqs,
     >                   rliqu,
     >                   sai,
     >                   tl,
     >                   tmelt,
     >                   ts,
     >                   tu)
c ---------------------------------------------------------------------
c
c sets two-stream parameters, given single-element transmittance
c and reflectance, leaf orientation weights, and cosine of the
c zenith angle, then adjusts for amounts of intercepted snow
c
c the two-stream parameters omega,betad,betai are weighted 
c combinations of the "exact" values for the 3 orientations:
c all vertical, all horizontal, or all random (ie, spherical)
c
c the vertical, horizontal weights are in oriev,orieh (comveg)
c
c the "exact" expressions are as derived in my notes(8/6/91,p.6).
c note that values for omega*betad and omega*betai are calculated
c and then divided by the new omega, since those products are 
c actually used in twostr. also those depend *linearly* on the
c single-element transmittances and reflectances tauveg, rhoveg,
c which are themselves linear weights of leaf and stem values 
c
c for random orientation, omega*betad depends on coszen according
c to the function in array tablemu
c
c the procedure is approximate since omega*beta[d,i] and gdir
c should depend non-linearly on the complete leaf-angle
c distribution. then we should also treat leaf and stem angle
c distributions separately, and allow for the cylindrical
c shape of stems (norman and jarvis, app.b; the expressions 
c below are appropriate for flat leaves)
c
       implicit none
c
c      include 'compar.h'
c      include 'comveg.h'
c      include 'com1d.h'
c

c globals

       double precision epsilon
       double precision fwetl(npoi)
       double precision fwets(npoi)
       double precision fwetu(npoi)
       double precision greenfracl(npoi)
       integer indsol(npoi)
       double precision lai(npoi,2)
       integer nband
       integer npoi
       integer nsol
       double precision orieh(2)
       double precision oriev(2)
       double precision pi
       double precision rliql(npoi)
       double precision rliqs(npoi)
       double precision rliqu(npoi)
       double precision sai(npoi,2)
       double precision tl(npoi)
       double precision tmelt
       double precision ts(npoi)
       double precision tu(npoi)



c Arguments (all quantities are returned unless otherwise note)
c
      integer ib,             ! waveband number (1= visible, 2= near-IR)
     >        iv              ! 1 for lower, 2 for upper story params (supplied)
c
      double precision omega(npoi),       ! fraction of intercepted radiation that is scattered
     >     betad(npoi),       ! fraction of scattered *direct* radiation that is
     >                        !  scattered into upwards hemisphere
     >     betai(npoi),       ! fraction of scattered downward *diffuse* radiation
     >                        ! that is scattered into upwards hemisphere (or fraction
     >                        ! of scattered upward diffuse rad. into downwards hemis)
     >     avmu(npoi),        ! average diffuse optical depth
     >     gdir(npoi),        ! average projected leaf area into solar direction
     >     coszen(npoi)       ! cosine of solar zenith angle (supplied)
c
c local variables
c
      integer j,    ! loop indice on number of points with >0 coszen
     >        i,    ! indice of point in (1, npoi) array. 
     >        ntmu, !
     >        itab
c
      double precision zrho, ztau, orand, ztab, rwork, y, o, x, betadsno, betaisno
      double precision rhovegvu, tauvegvu, rhovegiru, tauvegiru, rhovegvlg, rhovegvlb
      double precision tauvegvlg, tauvegvlb, rhovegirlb, rhovegirlg, tauvegirlg, tauvegirlb
c
      double precision otmp(npoi)
c
      parameter (ntmu=100)

c     TODO: Alterado dimensão da variável omegasno. Antes estava tamanho 'nband'
      double precision tablemu(ntmu+1), omegasno(2)

c     TODO: Verificar 'save' retirado
c     save tablemu, omegasno, betadsno, betaisno
c
      data tablemu /
     >   0.5000, 0.4967, 0.4933, 0.4900, 0.4867, 0.4833, 0.4800, 0.4767,
     >   0.4733, 0.4700, 0.4667, 0.4633, 0.4600, 0.4567, 0.4533, 0.4500,
     >   0.4467, 0.4433, 0.4400, 0.4367, 0.4333, 0.4300, 0.4267, 0.4233,
     >   0.4200, 0.4167, 0.4133, 0.4100, 0.4067, 0.4033, 0.4000, 0.3967,
     >   0.3933, 0.3900, 0.3867, 0.3833, 0.3800, 0.3767, 0.3733, 0.3700,
     >   0.3667, 0.3633, 0.3600, 0.3567, 0.3533, 0.3500, 0.3467, 0.3433,
     >   0.3400, 0.3367, 0.3333, 0.3300, 0.3267, 0.3233, 0.3200, 0.3167,
     >   0.3133, 0.3100, 0.3067, 0.3033, 0.3000, 0.2967, 0.2933, 0.2900,
     >   0.2867, 0.2833, 0.2800, 0.2767, 0.2733, 0.2700, 0.2667, 0.2633,
     >   0.2600, 0.2567, 0.2533, 0.2500, 0.2467, 0.2433, 0.2400, 0.2367,
     >   0.2333, 0.2300, 0.2267, 0.2233, 0.2200, 0.2167, 0.2133, 0.2100,
     >   0.2067, 0.2033, 0.2000, 0.1967, 0.1933, 0.1900, 0.1867, 0.1833,
     >   0.1800, 0.1767, 0.1733, 0.1700, 0.1667 /
c
      data omegasno /0.9, 0.7/
      data betadsno, betaisno /0.5, 0.5/

c
c Assign leaf optical properties (taken from Sellers et al., 1996
c and Bonan, 1995)
c These are reflectance and transmission parameters depending on what part
c of the spectrum is used, what part of the canopy is used (lower or upper),
c and whether the leaves are green or brown
c
      rhovegvlg = 0.10      ! vis leaf reflectance, lower story, green leaves
      rhovegvlb = 0.36      ! vis leaf reflectance, lower story, brown leaves
      rhovegvu = 0.10       ! vis leaf reflectance, upper story, green leaves
c
      rhovegirlg = 0.48     ! nir leaf reflectance, lower story, green leaves
      rhovegirlb = 0.58     ! nir leaf reflectance, lower story, brown leaves
      rhovegiru = 0.40      ! nir leaf reflectance, upper story, green leaves
c
      tauvegvlg = 0.07      ! vis leaf transmittance, lower story, green leaves
      tauvegvlb = 0.22      ! vis leaf transmittance, lower story, brown leaves
      tauvegvu = 0.05       ! vis leaf transmittance, upper story, green leaves
c
      tauvegirlg = 0.25     ! nir leaf transmittance, lower story, green leaves
      tauvegirlb = 0.38     ! nir leaf transmittance, lower story, brown leaves
      tauvegiru = 0.20      ! nir leaf transmittance, upper story, green leaves

       !double precision sai(npoi,2)
       !double precision tl(npoi)
       !double precision tmelt
       !double precision ts(npoi)
       !double precision tu(npoi) 

c
c set two-stream parameters omega, betad, betai, gdir and avmu
c as weights of those for 100% vert,horiz,random orientations
c
      do 100 j=1,nsol
        i = indsol(j)
c
c The following determines zrho (reflectance of an average leaf) and
c ztau (transmittance of an average leaf) for location i.
c rhoveg and tauveg are given above for both upper and lower
c canopies and for visible and near infrared wavebands. This new
c routine adjusts those initialized values for the lower canopy
c depending on how much of the canopy is green.
c zrho and ztau will be
c weighted by greenfracl (the fraction of lower canopy that is green)
c to allow values to go from full green values to full brown values.
c Note that zrho for near infrared is the same for both green and
c brown leaves but the calculation is given for consistency.
c
c iv is 1 for lower canopy and 2 for upper canopy
c ib is 1 for visible wavebands and 2 for near infrared wavebands
c
        if (iv.eq.2) then
          if (ib.eq.1) then
c
c visible values for the upper canopy
c
            zrho = rhovegvu
            ztau = tauvegvu
          else
c
c ir values for the upper canopy
c
            zrho = rhovegiru
            ztau = tauvegiru
          endif
        else
          if (ib.eq.1) then
c
c visible values for the lower canopy, weighted by how much of
c canopy is green
c
            zrho = greenfracl(i) * rhovegvlg +
     >             rhovegvlb * (1. - greenfracl(i))
c
            ztau = greenfracl(i) * tauvegvlg +
     >             tauvegvlb * (1. - greenfracl(i))
c
          else
c
c ir values for the lower canopy, weighted by how much of
c canopy is green
c
            zrho = greenfracl(i) * rhovegirlg +
     >             rhovegirlb * (1. - greenfracl(i))
c
            ztau = greenfracl(i) * tauvegirlg +
     >             tauvegirlb * (1. - greenfracl(i))
c
          endif
        endif
c
c weight for random orientation is 1 - those for vert and horiz
c
        orand = 1. - oriev(iv) - orieh(iv)
c

        omega(i) = zrho + ztau
c
c ztab is transmittance coeff - for random-orientation omega*betad,
c given by tablemu as a function of coszen
c
        itab = nint (coszen(i)*ntmu + 1)
        ztab = tablemu(itab)
        rwork = 1./omega(i)
c
        betad(i) = (  oriev(iv) * 0.5*(zrho + ztau)
     >              + orieh(iv) * zrho
     >              + orand       * ((1.-ztab)*zrho + ztab*ztau) )
     >             * rwork
c
        betai(i) = (  oriev(iv) * 0.5*(zrho + ztau)
     >              + orieh(iv) * zrho
     >              + orand       * ((2./3.)*zrho + (1./3.)*ztau) )
     >             * rwork
c
        gdir(i)  = oriev(iv) * (2./pi) *
     >             sqrt ( max (0., 1.-coszen(i)*coszen(i)) )
     >           + orieh(iv) * coszen(i)
     >           + orand       * 0.5
c
        avmu(i) = 1.
c
  100 continue
c
c adjust omega, betad and betai for amounts of intercepted snow
c (omegasno decreases to .6 of cold values within 1 deg of tmelt)
c
      if (iv.eq.1) then
c
c lower story
c
        do 210 j=1,nsol
          i = indsol(j)
          y = fwetl(i)*(1.-rliql(i))
          o = omegasno(ib)*(.6 + .4*max(0.,min(1.,(tmelt-tl(i))/1.0)))
          otmp(i)  = omega(i)
          rwork = y * o
          omega(i) =  (1-y)*otmp(i)          + rwork
          betad(i) = ((1-y)*otmp(i)*betad(i) + rwork*betadsno) /
     >               omega(i)  
          betai(i) = ((1-y)*otmp(i)*betai(i) + rwork*betaisno) /
     >               omega(i)  
  210   continue

      
c
      else
c
c upper story
c

        do 220 j=1,nsol
          i = indsol(j)
          x = lai(i,iv) / max (lai(i,iv)+sai(i,iv), epsilon)
          y = x * fwetu(i)*(1.-rliqu(i)) + (1-x) *fwets(i)*(1.-rliqs(i))
          o = (     x  * min (1., max (.6, (tmelt-tu(i))/0.1))
     >         + (1-x) * min (1., max (.6, (tmelt-ts(i))/0.1)) )
     >      *  omegasno(ib) 
c
          otmp(i)  = omega(i)
          rwork = y * o
          
          omega(i) =  (1-y)*otmp(i)          + rwork
          
          betad(i) = ((1-y)*otmp(i)*betad(i) + rwork*betadsno) /
     >               omega(i)
          betai(i) = ((1-y)*otmp(i)*betai(i) + rwork*betaisno) /
     >               omega(i)
c
  220   continue
c
      endif
      
      
c
      return
      end
c


c ------------------------------------------------------------------------
       subroutine twostr (abvegd, abvegi, refld, refli, fbeldd, fbeldi,
     >                    fbelid, fbelii, asurd, asuri, iv, coszen, ib,
     >                    epsilon,
     >                    indsol,
     >                    lai,
     >                    nband,
     >                    npoi,
     >                    nsol,
     >                    sai,
     >                    terml,
     >                    termu,
     >                    fwetl,
     >                    fwets,
     >                    fwetu,
     >                    greenfracl,
     >                    orieh,
     >                    oriev,
     >                    pi,
     >                    rliql,
     >                    rliqs,
     >                    rliqu,
     >                    tl,
     >                    tmelt,
     >                    ts,
     >                    tu)


c ------------------------------------------------------------------------
c
c solves canonical radiative transfer problem of two-stream veg
c layer + underlying surface of known albedo, for unit incoming
c direct or diffuse flux. returns flux absorbed within layer,
c reflected flux, and downward fluxes below layer. note that all
c direct fluxes are per unit horizontal zrea, ie, already 
c including a factor cos (zenith angle)
c
c the solutions for the twostream approximation follow Sellers (1985),
c and Bonan (1996) (the latter being the LSM documentation)
c
      implicit none
c
c      include 'compar.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
c Arguments
c
      integer ib,             ! waveband number (1= visible, 2= near-IR)
     >        iv              ! 1 for lower, 2 for upper story params (supplied)
c
      double precision abvegd(npoi),      ! direct flux absorbed by two-stream layer (returned)
     >     abvegi(npoi),      ! diffuse flux absorbed by two-stream layer (returned)
     >     refld(npoi),       ! direct flux reflected above two-stream layer (returned)
     >     refli(npoi),       ! diffuse flux reflected above two-stream layer (returned)
     >     fbeldd(npoi),      ! downward direct  flux below two-stream layer(returned)
     >     fbeldi(npoi),      ! downward direct  flux below two-stream layer(returned)
     >     fbelid(npoi),      ! downward diffuse flux below two-stream layer(returned)
     >     fbelii(npoi),      ! downward diffuse flux below two-stream layer(returned)
     >     asurd(npoi,nband), ! direct  albedo of underlying surface (supplied)
     >     asuri(npoi,nband), ! diffuse albedo of underlying surface (supplied)
     >     coszen(npoi)       ! cosine of direct zenith angle (supplied, must be gt 0)


c globals

      integer          nband
      integer          npoi
      integer          nsol
      integer          indsol(npoi)

      double precision epsilon
      double precision lai(npoi,2)
      double precision sai(npoi,2)
      double precision terml(npoi,7)
      double precision termu(npoi,7)
      
      ! twoset
      double precision fwetl(npoi)
      double precision fwets(npoi)
      double precision fwetu(npoi)
      double precision greenfracl(npoi)
      double precision orieh(2)
      double precision oriev(2)
      double precision pi
      double precision rliql(npoi)
      double precision rliqs(npoi)
      double precision rliqu(npoi)
      double precision tl(npoi)
      double precision tmelt
      double precision ts(npoi)
      double precision tu(npoi)

c
c local variables
c
      integer j,    ! loop indice on number of points with >0 coszen
     >        i     ! indice of point in (1, npoi) array. 
c
      double precision b, c, c0, d, f, h, k, q, p, sigma
c
      double precision ud1, ui1, ud2, ui2, ud3, xai, s1, s2, p1, p2, p3, p4, 
     >     rwork, dd1, di1, dd2, di2, h1, h2, h3, h4, h5, h6, h7, h8, 
     >     h9, h10, absurd, absuri
c
c [d,i] => per unit incoming direct, diffuse (indirect) flux
c
      double precision omega(npoi),       !
     >     betad(npoi),       !
     >     betai(npoi),       !
     >     avmu(npoi),        !
     >     gdir(npoi),        !
     >     tmp0(npoi)         !

     
c
c do nothing if all points in current strip have coszen le 0
c
      if (nsol.eq.0) return
c
c calculate two-stream parameters omega, betad, betai, avmu, gdir
c
        
      call twoset (omega, betad, betai, avmu, gdir, coszen, iv, ib,
     >                   epsilon,
     >                   fwetl,
     >                   fwets,
     >                   fwetu,
     >                   greenfracl,
     >                   indsol,
     >                   lai,
     >                   nband,
     >                   npoi,
     >                   nsol,
     >                   orieh,
     >                   oriev,
     >                   pi,
     >                   rliql,
     >                   rliqs,
     >                   rliqu,
     >                   sai,
     >                   tl,
     >                   tmelt,
     >                   ts,
     >                   tu)
      
       
c
      do 100 j=1,nsol
c
        i = indsol(j)
c
c the notations used here are taken from page 21 of Bonan's LSM documentation:
c Bonan, 1996: A Land Surface Model (LSM version 1.0) for ecological, hydrological,
c and atmospheric studies: Technical description and user's guide. NCAR Technical
c Note. NCAR/TN-417+STR, January 1996.
c
c some temporary variables are also introduced, which are from the original
c lsx model.
c
        b = 1. - omega(i) * (1.-betai(i))
        c = omega(i) * betai(i)
c
        tmp0(i) = b*b-c*c
c
        q = sqrt ( max(0.0, tmp0(i)) )
        k = gdir(i) / max(coszen(i), 0.01)
        p = avmu(i) * k
c
c next line perturbs p if p = q
c
        if ( abs(p-q) .lt. .001*p )
     >  p = (1.+sign(DBLE(.001),p-q)) * p
c
        c0 = omega(i) * p
        d = c0 * betad(i)
        f = c0 * (1.-betad(i))
        h = q / avmu(i)
c

        sigma = p*p - tmp0(i)
c
c direct & diffuse parameters are separately calculated
c
        ud1 = b - c/asurd(i,ib)
        ui1 = b - c/asuri(i,ib)
        ud2 = b - c*asurd(i,ib)
        ui2 = b - c*asuri(i,ib)
        ud3 = f + c*asurd(i,ib)
c
        xai = max (lai(i,iv) + sai(i,iv), epsilon)
c
        s1 = exp(-1.*h*xai)
        s2 = exp(-1.*k*xai)
c
        p1 = b + q
        p2 = b - q
        p3 = b + p
        p4 = b - p
        rwork = 1./s1
c
c direct & diffuse parameters are separately calculated
c
        dd1 = p1*(ud1-q)*rwork - p2*(ud1+q)*s1
        di1 = p1*(ui1-q)*rwork - p2*(ui1+q)*s1
        dd2 = (ud2+q)*rwork - (ud2-q)*s1
        di2 = (ui2+q)*rwork - (ui2-q)*s1
        h1 = -1.*d*p4 - c*f
        rwork = s2*(d-c-h1*(ud1+p)/sigma)
        h2 = 1./dd1*( (d-h1*p3/sigma)*(ud1-q)/s1 - 
     >       p2*rwork )
        h3 = -1./dd1*( (d-h1*p3/sigma)*(ud1+q)*s1 - 
     >       p1*rwork )
        h4 = -1.*f*p3 - c*d
        rwork = s2*(ud3-h4*(ud2-p)/sigma)
        h5 = -1./dd2*( h4*(ud2+q)/(sigma*s1) +
     >       rwork )
        h6 = 1./dd2*( h4*s1*(ud2-q)/sigma +
     >       rwork )
        h7 = c*(ui1-q)/(di1*s1)
        h8 = -1.*c*s1*(ui1+q)/di1
        h9 = (ui2+q)/(di2*s1)
        h10= -1.*s1*(ui2-q)/di2
c
c save downward direct, diffuse fluxes below two-stream layer
c
        fbeldd(i) = s2
        fbeldi(i) = 0.
        fbelid(i) = h4/sigma*s2 + h5*s1 + h6/s1
        fbelii(i) = h9*s1 + h10/s1
c
c save reflected flux, and flux absorbed by two-stream layer
c     
        refld(i) = h1/sigma + h2 + h3
        refli(i) = h7 + h8
        absurd = (1.-asurd(i,ib)) * fbeldd(i)
     >         + (1.-asuri(i,ib)) * fbelid(i)
        absuri = (1.-asuri(i,ib)) * fbelii(i)
c
        abvegd(i) = max (0., 1. - refld(i) - absurd)
        abvegi(i) = max (0., 1. - refli(i) - absuri)
c
c if no veg, make sure abveg (flux absorbed by veg) is exactly zero
c if this is not done, roundoff error causes small (+/-)
c sols, soll values in solarf and subsequent problems in turvap
c via stomata
c
        if (xai.lt.epsilon) abvegd(i) = 0.0
        if (xai.lt.epsilon) abvegi(i) = 0.0
c
c some terms needed in canopy scaling
c the canopy scaling algorithm assumes that the net photosynthesis
c is proportional to absored par (apar) during the daytime. during night,
c the respiration is scaled using a 10-day running-average daytime canopy
c scaling parameter.
c
c apar(x) = A exp(-k x) + B exp(-h x) + C exp(h x)
c
c in the equations below, 
c
c   k = term[u,l](i,6)
c   h = term[u,l](i,7)
c
c   A = term[u,l](i,1) * ipardir(0)
c   B = term[u,l](i,2) * ipardir(0) + term[u,l](i,3) * ipardif(0)
c   C = term[u,l](i,4) * ipardir(0) + term[u,l](i,5) * ipardif(0)
c
c calculations performed only for visible (ib=1)
c
      if (ib.eq.1) then
c
        if (iv.eq.1) then
          terml(i,1) = k * (1. + (h4-h1) / sigma)
          terml(i,2) = h * (h5 - h2)
          terml(i,3) = h * (h9 - h7)
          terml(i,4) = h * (h3 - h6)
          terml(i,5) = h * (h8 - h10)
          terml(i,6) = k
          terml(i,7) = h
        else
          termu(i,1) = k * (1. + (h4-h1) / sigma)
          termu(i,2) = h * (h5 - h2)
          termu(i,3) = h * (h9 - h7)
          termu(i,4) = h * (h3 - h6)
          termu(i,5) = h * (h8 - h10)
          termu(i,6) = k
          termu(i,7) = h
        endif
c
      end if
c
  100 continue
c
      return
      end
c



