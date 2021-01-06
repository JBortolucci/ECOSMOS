

  
c ---------------------------------------------------------------------
      double precision function cvmgt (x,y,l)
c ---------------------------------------------------------------------
c
c chooses between two things.  Used in canopy.f
c
c
      logical l
      double precision x, y
c
      if (l) then
        cvmgt = x
      else
        cvmgt = y
      endif
c
      return
      end


       subroutine fwetcalWrapper(dim_params, 
     >                           double_params,
     >                           npoi_matrix)
           
            
            integer          dim_params
            
            double precision double_params(8)

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

            integer i

c 	    Inicializando es double de tamanho npoi
            DO 102, i = 1, dim_params, 1
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
102         CONTINUE

            call fwetcal(dim_params, double_params(1), double_params(2),
     >                   double_params(3), double_params(4), double_params(5),
     >                   double_params(6), double_params(7), double_params(8),
     >                   npoi_line1,
     >                   npoi_line2,
     >                   npoi_line3,
     >                   npoi_line4,
     >                   npoi_line5,
     >                   npoi_line6,
     >                   npoi_line7,
     >                   npoi_line8,
     >                   npoi_line9,
     >                   npoi_line10,
     >                   npoi_line11,
     >                   npoi_line12,
     >                   npoi_line13,
     >                   npoi_line14,
     >                   npoi_line15)
            
c 	    Inicializando es double de tamanho npoi
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

c
c ---------------------------------------------------------------------
      subroutine fwetcal(npoi, 
     >                   tmelt,
     >                   epsilon,
     >                   wliqlmax,
     >                   wliqsmax,
     >                   wliqumax,
     >                   wsnolmax,
     >                   wsnosmax,
     >                   wsnoumax,
     >                   fwetl,
     >                   fwets, 
     >                   fwetu ,
     >                   rliql, 
     >                   rliqs,
     >                   rliqu,
     >                   tl, 
     >                   ts, 
     >                   tu, 
     >                   wliql, 
     >                   wliqs,
     >                   wliqu,
     >                   wsnol,
     >                   wsnos,
     >                   wsnou)
c ---------------------------------------------------------------------
c
c calculates fwet[u,s,l], the fractional areas wetted by 
c intercepted h2o (liquid and snow combined) -  the maximum value
c fmax (<1) allows some transpiration even in soaked conditions
c
c use a linear relation between fwet* and wliq*,wsno* (at least
c for small values), so that the implied "thickness" is constant
c (equal to wliq*max, wsno*max as below) and the typical amount
c evaporated in one timestep in steph2o will not make wliq*,wsno*
c negative and thus cause a spurious unrecoverable h2o loss
c
c (the max(w*max,.01) below numericaly allows w*max = 0 without
c blowup.) in fact evaporation in one timestep *does* sometimes
c exceed wliq*max (currently 1 kg/m2), so there is an additional
c safeguard in turvap that limits the wetted-area aerodynamic
c coefficients suw,ssw,slw -- if that too fails, there is an 
c ad-hoc adjustment in steph2o2 to reset negative wliq*,wsno*
c amounts to zero by taking some water vapor from the atmosphere.
c
c also sets rliq[u,s,l], the proportion of fwet[u,s,l] due to
c liquid alone. fwet,rliq are used in turvap, rliq in steph2o. 
c (so rliq*fwet, (1-rliq)*fwet are the fractional areas wetted
c by liquid and snow individually.) if fwet is 0, choose rliq
c = 1 if t[u,s,l] ge tmelt or 0 otherwize, for use by turvap and
c steph2o in case of initial dew formation on dry surface.
c 
       implicit none
c
c      include 'compar.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
      integer npoi  
      
      double precision tmelt 
      double precision epsilon
      double precision wliqlmax
      double precision wliqsmax
      double precision wliqumax
      double precision wsnolmax
      double precision wsnosmax
      double precision wsnoumax

      double precision fwetl(npoi)
      double precision fwets(npoi)
      double precision fwetu(npoi)
      double precision rliql(npoi)
      double precision rliqs(npoi)
      double precision rliqu(npoi)
      double precision tl(npoi)
      double precision ts(npoi)
      double precision tu(npoi)
      double precision wliql(npoi)
      double precision wliqs(npoi)
      double precision wliqu(npoi)
      double precision wsnol(npoi)
      double precision wsnos(npoi)
      double precision wsnou(npoi)

c local variables
c
      integer i           ! loop indice
c
      double precision fmax,          ! maximum water cover on two-sided leaf
     >     xliq,          ! fraction of wetted leaf (liquid only)
     >     xtot           ! fraction of wetted leaf (liquid and snow)
c
c maximum water cover on two-sided leaf
c
      parameter (fmax = 0.08)
c
c upper leaves
c
      do 100 i = 1, npoi
c
        xliq = wliqu(i) / max (wliqumax, 0.01)
        xtot = xliq + wsnou(i) / max (wsnoumax, 0.01)
c
        fwetu(i) = min (fmax, xtot)
        rliqu(i) = xliq / max (xtot, epsilon)
c
        if (fwetu(i).eq.0.0) then
          rliqu(i) = 1.0
          if (tu(i).lt.tmelt) rliqu(i) = 0.0
        endif
c
  100 continue

c
c upper stems
c
      do 200 i = 1, npoi
c
        xliq = wliqs(i) / max (wliqsmax, 0.01)
        xtot = xliq + wsnos(i) / max (wsnosmax, 0.01)
c
        fwets(i) = min (fmax, xtot)

        rliqs(i) = xliq / max (xtot, epsilon)
c

        if (fwets(i).eq.0.0) then
          rliqs(i) = 1.0
          if (ts(i).lt.tmelt) rliqs(i) = 0.0
        endif
c
  200 continue

        
c
c lower veg
c
      do 300 i = 1, npoi
c
        xliq = wliql(i) / max (wliqlmax, 0.01)
        xtot = xliq + wsnol(i) / max (wsnolmax, 0.01)
c
        fwetl(i) = min (fmax, xtot)
        rliql(i) = xliq / max (xtot, epsilon)

c	if(i.eq.1)print*,fwetl(i),xtot ,xliq,wliql(i)
c
        if (fwetl(i).eq.0.) then
          rliql(i) = 1.0
          if (tl(i).lt.tmelt) rliql(i) = 0.0
        endif
c
  300 continue
c

      return
      end


c ---------------------------------------------------------------------
      subroutine fstrat (tb, tt, ttfac, qb, qt, zb, zt, 
     >                   albm, albh, alt, u, rich, stram, strah, iter,
     >                   grav, npoi, vonk)
c ---------------------------------------------------------------------
c
c computes mixing-length stratification correction factors
c for momentum and heat/vapor, for current 1d strip, using
c parameterizations in louis (1979),blm,17,187. first computes
c richardson numbers. sets an upper limit to richardson numbers
c so lower-veg winds don't become vanishingly small in very
c stable conditions (cf, carson and richards,1978,blm,14,68)
c
c system (i) is as in louis(1979). system (vi) is improved as
c described in louis(1982), ecmwf workshop on planetary boundary
c layer parameterizations,november 1981,59-79 (qc880.4 b65w619)
c
c common blocks
c
       implicit none
c
c      include 'compar.h'
c
c global variables

       integer npoi
       double precision grav
       double precision vonk

c input variables
c
      integer iter   ! current iteration number
c
      double precision ttfac     ! pot. temp factor for ttop (relative to bottom,supplied)
c
      double precision
     > tb(npoi),     ! bottom temperature (supplied)
     > tt(npoi),     ! top temperature (supplied)
     > qb(npoi),     ! bottom specific humidity (supplied)
     > qt(npoi),     ! top specific humidity (supplied)
     > zb(npoi),     ! height of bottom (supplied)
     > zt(npoi),     ! height of top (supplied)
     > albm(npoi),   ! log (bottom roughness length) for momentum (supplied)
     > albh(npoi),   ! log (bottom roughness length) for heat/h2o (supplied)
     > alt(npoi),    ! log (z at top) (supplied)
     > u(npoi),      ! wind speed at top (supplied)
     > rich(npoi),   ! richardson number (returned)
     > stram(npoi),  ! stratification factor for momentum (returned)
     > strah(npoi),  ! stratification factor for heat/vap (returned)
     > stramx(npoi), !
     > strahx(npoi)  !
c
c local variables
c
      integer 
     > indp(npoi),   !
     > indq(npoi)    !
c
      integer i, j, np, nq
c
      double precision zht, zhb, xm, xh, rwork, ym, yh, z, w
c ---------------------------------------------------------------------      

      np = 0
      nq = 0
c
c do for all points
c
      do 100 i = 1, npoi
c
c calculate richardson numbers
c
        zht = tt(i)*ttfac*(1.+.622*qt(i))
        zhb = tb(i)*      (1.+.622*qb(i))
c
        rich(i) = grav * max (zt(i)-zb(i), 0.)
     >            * (zht-zhb) / (0.5*(zht+zhb) * u(i)**2)
c
c bound richardson number between -2.0 (unstable) to 1.0 (stable)
c
        rich(i) = max (-2.0, min (rich(i), 1.0))
c
 100  continue
c
c set up indices for points with negative or positive ri
c
      do 110 i = 1, npoi
c
        if (rich(i).le.0.) then
          np = np + 1
          indp(np) = i
        else
          nq = nq + 1
          indq(nq) = i
        endif
c
  110 continue
c
c calculate momentum and heat/vapor factors for negative ri
c
      if (np.gt.0) then
c
        do 200 j = 1, np
c
          i = indp(j)
c
          xm = max (alt(i)-albm(i), .5)
          xh = max (alt(i)-albh(i), .5)
c
          rwork = sqrt(-rich(i))
c
          ym = (vonk/xm)**2 * exp (0.5*xm) * rwork
          yh = (vonk/xh)**2 * exp (0.5*xh) * rwork
c
c system (vi)
c
          stramx(i) =   1.0 - 2*5*rich(i) / (1.0 + 75*ym)
          strahx(i) =   1.0 - 3*5*rich(i) / (1.0 + 75*yh)
c
  200   continue
c
      endif
c
c calculate momentum and heat/vapor factors for positive ri
c
      if (nq.gt.0) then
c
        do 300 j=1,nq
c
          i = indq(j)
c
c system (vi)
c
          z = sqrt(1.0 + 5 * rich(i))
c
          stramx(i) = 1.0 / (1.0 + 2*5*rich(i) / z)
          strahx(i) = 1.0 / (1.0 + 3*5*rich(i) * z)
c
  300   continue
c
      endif
c
c except for the first iteration, weight results with the
c previous iteration's values. this improves convergence by
c avoiding flip-flop between stable/unstable stratif, eg,
c with cold upper air and the lower surface being heated by
c solar radiation
c
      if (iter.eq.1) then
c
        do 400 i = 1, npoi
c
          stram(i) = stramx(i)
          strah(i) = strahx(i)
c
  400   continue
c
      else
c
        w = 0.5
c
        do 410 i = 1, npoi
c
          stram(i) = w * stramx(i) + (1.0 - w) * stram(i)
          strah(i) = w * strahx(i) + (1.0 - w) * strah(i)
c
  410   continue
c
      endif
c

      return
      end

      subroutine turcofWrapper(iter, time, jday,
     >                         dim_params,
     >                         double_params,
     >                         npoi_matrix,
     >                         dleaf,
     >                         dstem,
     >                         lai,
     >                         use)

                integer iter, jday
                double precision time

                integer dim_params

                double precision double_params(7)

                double precision dleaf(2)
                double precision dstem(2)
                double precision lai(dim_params,2)
                double precision use(4,24)

                double precision npoi_matrix(52, dim_params)

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
                double precision npoi_line16(dim_params)
                double precision npoi_line17(dim_params)
                double precision npoi_line18(dim_params)
                double precision npoi_line19(dim_params)
                double precision npoi_line20(dim_params)
                double precision npoi_line21(dim_params)
                double precision npoi_line22(dim_params)
                double precision npoi_line23(dim_params)
                double precision npoi_line24(dim_params)
                double precision npoi_line25(dim_params)
                double precision npoi_line26(dim_params)
                double precision npoi_line27(dim_params)
                double precision npoi_line28(dim_params)
                double precision npoi_line29(dim_params)
                double precision npoi_line30(dim_params)
                double precision npoi_line31(dim_params)
                double precision npoi_line32(dim_params)
                double precision npoi_line33(dim_params)
                double precision npoi_line34(dim_params)
                double precision npoi_line35(dim_params)
                double precision npoi_line36(dim_params)
                double precision npoi_line37(dim_params)
                double precision npoi_line38(dim_params)
                double precision npoi_line39(dim_params)
                double precision npoi_line40(dim_params)
                double precision npoi_line41(dim_params)
                double precision npoi_line42(dim_params)
                double precision npoi_line43(dim_params)
                double precision npoi_line44(dim_params)
                double precision npoi_line45(dim_params)
                double precision npoi_line46(dim_params)
                double precision npoi_line47(dim_params)
                double precision npoi_line48(dim_params)
                double precision npoi_line49(dim_params)
                double precision npoi_line50(dim_params)
                double precision npoi_line51(dim_params)
                double precision npoi_line52(dim_params)

                integer i

c	        Inicializando es double de tamanho npoi
                DO 102, i = 1, dim_params, 1
                       npoi_line1(i) = npoi_matrix(1,i)
                       npoi_line2(i) = npoi_matrix(2,i)
                       npoi_line3(i) = npoi_matrix(3,i)
                       npoi_line4(i) = npoi_matrix(4,i)
                       npoi_line5(i) = npoi_matrix(5,i)
                       npoi_line6(i) = npoi_matrix(6,i)
                       npoi_line7(i) = npoi_matrix(7,i)
                       npoi_line8(i) = npoi_matrix(8,i)
                       npoi_line9(i) = npoi_matrix(9,i)
                       npoi_line10(i) = npoi_matrix(10,i)
                       npoi_line11(i) = npoi_matrix(11,i)
                       npoi_line12(i) = npoi_matrix(12,i)
                       npoi_line13(i) = npoi_matrix(13,i)
                       npoi_line14(i) = npoi_matrix(14,i)
                       npoi_line15(i) = npoi_matrix(15,i)
                       npoi_line16(i) = npoi_matrix(16,i)
                       npoi_line17(i) = npoi_matrix(17,i)
                       npoi_line18(i) = npoi_matrix(18,i)
                       npoi_line19(i) = npoi_matrix(19,i)
                       npoi_line20(i) = npoi_matrix(20,i)
                       npoi_line21(i) = npoi_matrix(21,i)
                       npoi_line22(i) = npoi_matrix(22,i)
                       npoi_line23(i) = npoi_matrix(23,i)
                       npoi_line24(i) = npoi_matrix(24,i)
                       npoi_line25(i) = npoi_matrix(25,i)
                       npoi_line26(i) = npoi_matrix(26,i)
                       npoi_line27(i) = npoi_matrix(27,i)
                       npoi_line28(i) = npoi_matrix(28,i)
                       npoi_line29(i) = npoi_matrix(29,i)
                       npoi_line30(i) = npoi_matrix(30,i)
                       npoi_line31(i) = npoi_matrix(31,i)
                       npoi_line32(i) = npoi_matrix(32,i)
                       npoi_line33(i) = npoi_matrix(33,i)
                       npoi_line34(i) = npoi_matrix(34,i)
                       npoi_line35(i) = npoi_matrix(35,i)
                       npoi_line36(i) = npoi_matrix(36,i)
                       npoi_line37(i) = npoi_matrix(37,i)
                       npoi_line38(i) = npoi_matrix(38,i)
                       npoi_line39(i) = npoi_matrix(39,i)
                       npoi_line40(i) = npoi_matrix(40,i)
                       npoi_line41(i) = npoi_matrix(41,i)
                       npoi_line42(i) = npoi_matrix(42,i)
                       npoi_line43(i) = npoi_matrix(43,i)
                       npoi_line44(i) = npoi_matrix(44,i)
                       npoi_line45(i) = npoi_matrix(45,i)
                       npoi_line46(i) = npoi_matrix(46,i)
                       npoi_line47(i) = npoi_matrix(47,i)
                       npoi_line48(i) = npoi_matrix(48,i)
                       npoi_line49(i) = npoi_matrix(49,i)
                       npoi_line50(i) = npoi_matrix(50,i)
                       npoi_line51(i) = npoi_matrix(51,i)
                       npoi_line52(i) = npoi_matrix(52,i)
102             CONTINUE

                call turcof(iter, time, jday, dim_params,
     >                      double_params(1), double_params(2),double_params(3),
     >                      double_params(4), double_params(5),double_params(6),
     >                      double_params(7),
     >                      npoi_line1 ,
     >                      npoi_line2,
     >                      npoi_line3,
     >                      npoi_line4,
     >                      npoi_line5,
     >                      npoi_line6,
     >                      npoi_line7,
     >                      npoi_line8,
     >                      npoi_line9,
     >                      npoi_line10,
     >                      npoi_line11,
     >                      npoi_line12,
     >                      npoi_line13,
     >                      npoi_line14,
     >                      npoi_line15,
     >                      npoi_line16,
     >                      npoi_line17,
     >                      npoi_line18,
     >                      npoi_line19,
     >                      npoi_line20,
     >                      npoi_line21,
     >                      npoi_line22,
     >                      npoi_line23,
     >                      npoi_line24,
     >                      npoi_line25,
     >                      npoi_line26,
     >                      npoi_line27,
     >                      npoi_line28,
     >                      npoi_line29,
     >                      npoi_line30,
     >                      npoi_line31,
     >                      npoi_line32,
     >                      npoi_line33,
     >                      npoi_line34,
     >                      npoi_line35,
     >                      npoi_line36,
     >                      npoi_line37,
     >                      npoi_line38,
     >                      npoi_line39,
     >                      npoi_line40,
     >                      npoi_line41,
     >                      npoi_line42,
     >                      npoi_line43,
     >                      npoi_line44,
     >                      npoi_line45,
     >                      npoi_line46,
     >                      npoi_line47,
     >                      npoi_line48,
     >                      npoi_line49,
     >                      npoi_line50,
     >                      npoi_line51,
     >                      npoi_line52,
     >                      dleaf,
     >                      dstem,
     >                      lai,
     >                      use)

c	        Inicializando es double de tamanho npoi
                DO 777, i = 1, dim_params, 1
                       npoi_matrix(1,i) = npoi_line1(i)
                       npoi_matrix(2,i) = npoi_line2(i)
                       npoi_matrix(3,i) = npoi_line3(i)
                       npoi_matrix(4,i) = npoi_line4(i)
                       npoi_matrix(5,i) = npoi_line5(i)
                       npoi_matrix(6,i) = npoi_line6(i)
                       npoi_matrix(7,i) = npoi_line7(i)
                       npoi_matrix(8,i) = npoi_line8(i)
                       npoi_matrix(9,i) = npoi_line9(i)
                       npoi_matrix(10,i) = npoi_line10(i)
                       npoi_matrix(11,i) = npoi_line11(i)
                       npoi_matrix(12,i) = npoi_line12(i)
                       npoi_matrix(13,i) = npoi_line13(i)
                       npoi_matrix(14,i) = npoi_line14(i)
                       npoi_matrix(15,i) = npoi_line15(i)
                       npoi_matrix(16,i) = npoi_line16(i)
                       npoi_matrix(17,i) = npoi_line17(i)
                       npoi_matrix(18,i) = npoi_line18(i)
                       npoi_matrix(19,i) = npoi_line19(i)
                       npoi_matrix(20,i) = npoi_line20(i)
                       npoi_matrix(21,i) = npoi_line21(i)
                       npoi_matrix(22,i) = npoi_line22(i)
                       npoi_matrix(23,i) = npoi_line23(i)
                       npoi_matrix(24,i) = npoi_line24(i)
                       npoi_matrix(25,i) = npoi_line25(i)
                       npoi_matrix(26,i) = npoi_line26(i)
                       npoi_matrix(27,i) = npoi_line27(i)
                       npoi_matrix(28,i) = npoi_line28(i)
                       npoi_matrix(29,i) = npoi_line29(i)
                       npoi_matrix(30,i) = npoi_line30(i)
                       npoi_matrix(31,i) = npoi_line31(i)
                       npoi_matrix(32,i) = npoi_line32(i)
                       npoi_matrix(33,i) = npoi_line33(i)
                       npoi_matrix(34,i) = npoi_line34(i)
                       npoi_matrix(35,i) = npoi_line35(i)
                       npoi_matrix(36,i) = npoi_line36(i)
                       npoi_matrix(37,i) = npoi_line37(i)
                       npoi_matrix(38,i) = npoi_line38(i)
                       npoi_matrix(39,i) = npoi_line39(i)
                       npoi_matrix(40,i) = npoi_line40(i)
                       npoi_matrix(41,i) = npoi_line41(i)
                       npoi_matrix(42,i) = npoi_line42(i)
                       npoi_matrix(43,i) = npoi_line43(i)
                       npoi_matrix(44,i) = npoi_line44(i)
                       npoi_matrix(45,i) = npoi_line45(i)
                       npoi_matrix(46,i) = npoi_line46(i)
                       npoi_matrix(47,i) = npoi_line47(i)
                       npoi_matrix(48,i) = npoi_line48(i)
                       npoi_matrix(49,i) = npoi_line49(i)
                       npoi_matrix(50,i) = npoi_line50(i)
                       npoi_matrix(51,i) = npoi_line51(i)
                       npoi_matrix(52,i) = npoi_line52(i)
777             CONTINUE
     
      end

c JAIR: Old    - Chamada dentro de canopy
c JAIR: Update - Criar wrapper

c ---------------------------------------------------------------------
      subroutine turcof (iter,time,jday,
     >                   npoi,
     >                   cgrass,
     >                   cleaf,
     >                   cstem,
     >                   dtime,
     >                   vonk,
     >                   grav,
     >                   tfac,
     >                   cu,
     >                   dil,
     >                   diu,
     >                   q12,
     >                   q34,
     >                   qa,
     >                   rhoa,
     >                   richl,
     >                   richu,
     >                   sg,
     >                   si,
     >                   sl,
     >                   ss,
     >                   strahl,
     >                   strahu,
     >                   straml,
     >                   stramu,
     >                   su,
     >                   t12,
     >                   t34,
     >                   ta,
     >                   ustar,
     >                   exphl,
     >                   exphu,
     >                   expl,
     >                   expu,
     >                   fl,
     >                   cl,
     >                   alog1,
     >                   alog2,
     >                   alog3,
     >                   alog4,
     >                   aloga,
     >                   alogav,
     >                   alogl,
     >                   alogu,
     >                   bdl,
     >                   bdu,
     >                   z1 ,
     >                   z12 ,
     >                   z2 ,
     >                   z3,
     >                   z34,
     >                   z4,
     >                   za,
     >                   u1,
     >                   u12,
     >                   u2,
     >                   u3,
     >                   u34,
     >                   u4,
     >                   ua,
     >                   dleaf,
     >                   dstem,
     >                   lai,
     >                   use)

c ---------------------------------------------------------------------
c
c solves for wind speeds at various levels
c
c also computes upper and lower-region air-air transfer coefficients
c and saves them in com1d arrays cu and cl for use by turvap,
c and similarly for the solid-air transfer coefficients
c su, ss, sl, sg and si
c
       implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsoi.h'
c      include 'comsno.h'
c      include 'comveg.h'
c      include 'com1d.h'
c      include 'comcrop.h'
c
c Arguments (input)
c
       integer iter,jday          !current iteration number
c
c
c global variables

       integer npoi

       ! 7
       double precision cgrass
       double precision cleaf
       double precision cstem
       double precision dtime
       double precision vonk
       double precision grav
       double precision tfac

       ! 52 
       double precision cu(npoi)
       double precision dil(npoi)
       double precision diu(npoi)
       double precision q12(npoi)
       double precision q34(npoi)
       double precision qa(npoi)
       double precision rhoa(npoi)
       double precision richl(npoi)
       double precision richu(npoi)
       double precision sg(npoi)
       double precision si(npoi)
       double precision sl(npoi)
       double precision ss(npoi)
       double precision strahl(npoi)
       double precision strahu(npoi)
       double precision straml(npoi)
       double precision stramu(npoi)
       double precision su(npoi)
       double precision t12(npoi)
       double precision t34(npoi)
       double precision ta(npoi)
       double precision ustar(npoi)
       double precision exphl(npoi)
       double precision exphu(npoi)
       double precision expl(npoi)
       double precision expu(npoi)
       double precision fl(npoi)
       double precision cl(npoi)
       double precision alog1(npoi)
       double precision alog2(npoi)
       double precision alog3(npoi)
       double precision alog4(npoi)
       double precision aloga(npoi)
       double precision alogav(npoi)
       double precision alogl(npoi)
       double precision alogu(npoi)
       double precision bdl(npoi)
       double precision bdu(npoi)
       double precision z1(npoi)
       double precision z12(npoi)
       double precision z2(npoi)
       double precision z3(npoi)
       double precision z34(npoi)
       double precision z4(npoi)
       double precision za(npoi)
       double precision u1(npoi)
       double precision u12(npoi)
       double precision u2(npoi)
       double precision u3(npoi)
       double precision u34(npoi)
       double precision u4(npoi)
       double precision ua(npoi)

       double precision dleaf(2)
       double precision dstem(2)
       double precision lai(npoi,2)
       double precision use(4,24)

c Local variables
c
      integer i             ! loop indice
c
      double precision xfac,  time,     !
     >     x,               !
     >     rwork,           ! working variable
     >     cdmax,           ! max value for cd
     >     tauu,            !
     >     a,b,c,d,         !
     >     taul,            !
     >     ca,              ! to compute inverse air-air transfer coeffs
     >     cai, cbi, cci,   !
     >     cdi, cei, cfi,   !
     >     sg0,             ! to compute air-solid transfer coeff for soil
     >     si0              ! to compute air-solid transfer coeff for ice
c
      double precision yu(npoi), yl(npoi)
 
c
c set stratification factors for lower and upper regions
c using values from the previous iteration
c
      xfac = 1.0
c

      call fstrat (t34, t12, xfac, q34, q12, z3, z2, 
     >             alogl, alogl, alog2, u2, richl, 
     >             straml, strahl, iter,
     >             grav, npoi, vonk)

      call fstrat (t12, ta, tfac, q12, qa, z1, za, 
     >             alogu, alogu, aloga, ua, richu, 
     >             stramu, strahu, iter,
     >             grav, npoi, vonk)
   

c
c eliminate c/d from eq (28), tau_l/rho from (26),(27), to get
c lower-story roughness alogl. yl/bdl is (tau_l/rho)/(c+d)
c
c equation numbers correspond to lsx description section 4.e
c
      do 100 i = 1, npoi
c
        x = ((alog4(i)-alogav(i))/vonk)**2 * bdl(i)
c
        rwork = 1. / expl(i)
        yl(i) = ((x+1)*expl(i) + (x-1)*rwork)
     >        / ((x+1)*expl(i) - (x-1)*rwork)
c
        alogl(i) = alog3(i) - vonk * sqrt(yl(i)/bdl(i))
c
 100  continue 
c       
      
c eliminate tau_l/rho from (24),(25), tau_u/rho and a/b from
c (22),(23), to get upper-story roughness alogu
c 
c yu/bdu is (tau_u/rho)/(a+b)
c
      do 110 i = 1, npoi
c          
        x = ((alog2(i)-alogl(i))/vonk)**2 * bdu(i) / straml(i)
c
        rwork = 1. / expu(i)
        yu(i) = ((x+1)*expu(i) + (x-1)*rwork)
     >        / ((x+1)*expu(i) - (x-1)*rwork)
c
        alogu(i) = alog1(i) - vonk * sqrt(yu(i)/bdu(i))
c
 110  continue

c       
c define the maximum value of cd
c
      cdmax = 300.0 / (2.0 * dtime)
c
c get tauu (=tau_u/rho) from (21), a and b from (22),(23),
c taul (=tau_u/rho) from (25), c and d from (26),(27)
c
c changed the following to eliminate small errors associated with
c moving this code to single precision - affected c and d,
c which made u_ become undefined, as well as affecting some
c other variables
c
      do 200 i = 1, npoi
c
        tauu = (ua(i) * vonk/(aloga(i)-alogu(i)))**2 * stramu(i)
c
        ustar(i) = tauu ** 0.5        
c
csant	if(i.eq.1.and.iter.eq.3)write(222,*)jday,time/3600,ustar(i)

        a = 0.5 * tauu * (yu(i)+1)/bdu(i)
        b = 0.5 * tauu * (yu(i)-1)/bdu(i)
c
        taul = bdu(i) * (a/expu(i) - b*expu(i))
c
        c = 0.5 * taul * (yl(i)+1)/bdl(i)
        d = 0.5 * taul * (yl(i)-1)/bdl(i)
c
c evaluate wind speeds at various levels, keeping a minimum 
c wind speed of 0.01 m/s at all levels
c   
        u1(i)  = max (0.01, sqrt (max (0.0, (a+b))))
csant	if(i.eq.1)  print*,u1(i)
        u12(i) = max (0.01, sqrt (max (0.0, (a/exphu(i)+b*exphu(i)))))
        u2(i)  = max (0.01, sqrt (max (0.0, (a/expu(i) +b*expu(i)))))
        u3(i)  = max (0.01, sqrt (max (0.0, (c+d))))
        u34(i) = max (0.01, sqrt (max (0.0, (c/exphl(i)+d*exphl(i)))))
        u4(i)  = max (0.01, sqrt (max (0.0, (c/expl(i) +d*expl(i)))))

csant	if(i.eq.3.and.iter.eq.3.and.time.eq.12*3600) !depois
csant     > write(223,*)jday,u34(i)

c
 200  continue
        
c compute inverse air-air transfer coeffs
c
c use of inverse individual coeffs cai, cbi, cci, cdi, cei, cfi avoids
c divide-by-zero as vegetation vanishes - combine into
c upper-region coeff cu from za to z12, and lower-region coeff
c cl from z34 to z12, and also coeffs
c



      do 300 i = 1, npoi
c       
        ca = ua(i)*strahu(i)*vonk**2  /
     >       ((aloga(i)-alogu(i)) * (aloga(i)-alog1(i)))
c
        ca = min (cdmax, ca / (1. + ca * 1.0e-20))
c
        cai = 1.0 / (rhoa(i)*ca)
c
        cbi = diu(i) * (z1(i)-z12(i)) / (rhoa(i) * 0.5*(u1(i)+u12(i)))
        cci = diu(i) * (z12(i)-z2(i)) / (rhoa(i) * 0.5*(u12(i)+u2(i)))
c
        cdi = (alog2(i)-alogl(i)) * (alog2(i)-alog3(i)) /
     >        (rhoa(i)*u2(i)*strahl(i)*vonk**2)
c
        cei = dil(i) * (z3(i)-z34(i)) / (rhoa(i) * 0.5*(u3(i)+u34(i)))
        cfi = dil(i) * (z34(i)-z4(i)) / (rhoa(i) * 0.5*(u34(i)+u4(i)))
c
        cu(i) = 1.0 / (cai + cbi)
        cl(i) = 1.0 / (cci + cdi + cei)

c
c compute air-solid transfer coeffs for upper leaves, upper
c stems, lower story (su,ss,sl)
c
        su(i) = rhoa(i) * cleaf  * sqrt (u12(i) / dleaf(2))
        ss(i) = rhoa(i) * cstem  * sqrt (u12(i) / dstem(2))
        sl(i) = rhoa(i) * cgrass * sqrt (u34(i) / dleaf(1))



c	if(croplive(i,16).eq.1)
c     > sl(i) = rhoa(i) * 0.0071 * sqrt (u34(i) / dleaf(1))  ! as mudancas sao insignificativas!!!!

csant	if(i.eq.1)print*,u1(i),u12(i),u34(i)


	if(i .eq. 1 .and. iter .eq. 1) then
	if(time .lt. 23*3600) then
        
	use(3,1) = use(3,1) + 41.4 * rhoa(i)/ cai 	!csant - 41.4 transform m/s to mol/m2s
	use(3,2) = use(3,2) + 41.4 * rhoa(i)/ cbi 
	use(3,3) = use(3,3) + 41.4 * rhoa(i)/ cci
	use(3,4) = use(3,4) + 41.4 * rhoa(i)/ cdi
 	use(3,5) = use(3,5) + 41.4 * rhoa(i)/ cei
 	use(3,6) = use(3,6) + 41.4 * rhoa(i)/ cfi
	use(3,7) = use(3,7) + 41.4 * cu(i)/rhoa(i)  
	use(3,8) = use(3,8) + 41.4 * cl(i)/rhoa(i)

	else

c       write(227,133)jday,lai(i,1)*fl(i),use(3,1)/24.,use(3,2)/24.
c    >,use(3,3)/24.,use(3,4)/24.,use(3,5)/24.,use(3,6)/24.,use(3,7)/24.,use(3,8)/24.

c 133	format(i3,9(1x,f8.2))
	use(3,1) = 0. 
	use(3,2) = 0. 
	use(3,3) = 0. 
	use(3,4) = 0. 
	use(3,5) = 0. 
	use(3,6) = 0. 
	use(3,7) = 0. 
	use(3,8) = 0. 

	endif
	endif
 
c
c compute air-solid transfer coeffs for soil and snow (sg,si)
c
c old technique
c
c       sg0 = rhoa(i) * u4(i) * (vonk/(alog4(i)-alogg(i)))**2
c       si0 = rhoa(i) * u4(i) * (vonk/(alog4(i)-alogi(i)))**2
c
c replace above formulations which depend on the log-wind profile
c (which may not work well below a canopy), with empirical formulation
c of Norman's. In the original LSX, turcof.f solves for the winds at
c the various levels from the momentum equations. This gives the transfer
c coefficients for heat and moisture. Heat and moisture eqns are then solved 
c in subroutine turvap. Using the empirical formulation of John Norman is 
c not consistent with the earlier solution for u4 (based on a logarithmic 
c profile just above the ground. However, this is used here because it 
c improved a lot simulations of the sensible heat flux over the 
c HAPEX-MOBILHY and FIFE sites
c
        
        sg0 = rhoa(i) * (0.004 + 0.012 * u4(i))
        si0 = rhoa(i) * (0.003 + 0.010 * u4(i))

c
c modify the cofficient to deal with cfi (see above)
c
        sg(i) = 1.0 / (cfi + 1.0 / sg0)
        si(i) = 1.0 / (cfi + 1.0 / si0)

csant - reduce the flux to see if it has impact on evapo - that is too high in SUGARCANE (only) simulation.  
ccsan - the correct is implement a parametrization to account for the trash over the soil. 
csant        sg(i) =  sg(i) * 0.1 !it reduces the evapo from soil, but the main problem is with the transp


c
  300 continue

        
c
c JAF:  not necessary 
c
c if no veg, recalculate coefficients appropriately for a
c single logarithmic profile, and 2 fictitious levels just
c above soil/snow surface. these levels are arbitrary but are
c taken as z2 and z4, preset in vegdat to a few cm height
c for bare ground and ice. use strahu from above, which used
c t12 and alogu (ok after first iteration)
c
c     do 600 i = 1, npoi
c
c       if ((fu(i).eq.0.0).and.(fl(i).eq.0.0)) then
c
c         z = rhoa(i)*ua(i)*strahu(i)*vonk**2 / (aloga(i)-alogav(i))
c
c         ca    = z / (aloga(i)-alog2(i))
c         cu(i) = rhoa(i)*min (cdmax,
c    >                          ca / (1. + ca / 1.0e+20))
c
c         cl(i) = z / (alog2(i)-alog4(i))
c
c         sg(i) = z / (alog4(i)-alogg(i))
c         si(i) = z / (alog4(i)-alogi(i))
c
c         alogu(i) = alogav(i)
c
c       endif
c
c 600 continue
c

      return
      end


      subroutine caniniWrapper(jday,
     >                         dim_params, double_params,
     >                         npoi_matrix,
     >                         sai,
     >                         lai,
     >                         zbot,
     >                         ztop)

        implicit none

        integer jday
        integer dim_params
        double precision double_params(10)

        double precision npoi_matrix(38, dim_params)

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
        double precision npoi_line16(dim_params)
        double precision npoi_line17(dim_params)
        double precision npoi_line18(dim_params)
        double precision npoi_line19(dim_params)
        double precision npoi_line20(dim_params)
        double precision npoi_line21(dim_params)
        double precision npoi_line22(dim_params)
        double precision npoi_line23(dim_params)
        double precision npoi_line24(dim_params)
        double precision npoi_line25(dim_params)
        double precision npoi_line26(dim_params)
        double precision npoi_line27(dim_params)
        double precision npoi_line28(dim_params)
        double precision npoi_line29(dim_params)
        double precision npoi_line30(dim_params)
        double precision npoi_line31(dim_params)
        double precision npoi_line32(dim_params)
        double precision npoi_line33(dim_params)
        double precision npoi_line34(dim_params)
        double precision npoi_line35(dim_params)
        double precision npoi_line36(dim_params)
        double precision npoi_line37(dim_params)
        double precision npoi_line38(dim_params)

        double precision sai(dim_params, 2)
        double precision lai(dim_params, 2)
        double precision zbot(dim_params,2)
        double precision ztop(dim_params,2)

                integer i

c 	    Inicializando es double de tamanho npoi
            DO 102, i = 1, dim_params, 1
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
                        npoi_line29(i) = npoi_matrix(29, i)
                        npoi_line30(i) = npoi_matrix(30, i)
                        npoi_line31(i) = npoi_matrix(31, i)
                        npoi_line32(i) = npoi_matrix(32, i)
                        npoi_line33(i) = npoi_matrix(33, i)
                        npoi_line34(i) = npoi_matrix(34, i)
                        npoi_line35(i) = npoi_matrix(35, i)
                        npoi_line36(i) = npoi_matrix(36, i)
                        npoi_line37(i) = npoi_matrix(37, i)
                        npoi_line38(i) = npoi_matrix(38, i)
102         CONTINUE

            call canini(jday, dim_params, 
     >                  double_params(1), double_params(2), double_params(3),
     >                  double_params(4), double_params(5), double_params(6),
     >                  double_params(7), double_params(8), double_params(9),
     >                  double_params(10),
     >                  npoi_line1,
     >                  npoi_line2,
     >                  npoi_line3,
     >                  npoi_line4,
     >                  npoi_line5,
     >                  npoi_line6,
     >                  npoi_line7,
     >                  npoi_line8,
     >                  npoi_line9,
     >                  npoi_line10,
     >                  npoi_line11,
     >                  npoi_line12,
     >                  npoi_line13,
     >                  npoi_line14,
     >                  npoi_line15,
     >                  npoi_line16,
     >                  npoi_line17,
     >                  npoi_line18,
     >                  npoi_line19,
     >                  npoi_line20,
     >                  npoi_line21,
     >                  npoi_line22,
     >                  npoi_line23,
     >                  npoi_line24,
     >                  npoi_line25,
     >                  npoi_line26,
     >                  npoi_line27,
     >                  npoi_line28,
     >                  npoi_line29,
     >                  npoi_line30,
     >                  npoi_line31,
     >                  npoi_line32,
     >                  npoi_line33,
     >                  npoi_line34,
     >                  npoi_line35,
     >                  npoi_line36,
     >                  npoi_line37,
     >                  npoi_line38,
     >                  sai,
     >                  lai,
     >                  zbot,
     >                  ztop)

c 	    RETORNANDO VALOR PARA VARIAVEIS
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
                        npoi_matrix(34, i) = npoi_line34(i)
                        npoi_matrix(35, i) = npoi_line35(i)
                        npoi_matrix(36, i) = npoi_line36(i)
                        npoi_matrix(37, i) = npoi_line37(i)
                        npoi_matrix(38, i) = npoi_line38(i)
777         CONTINUE

      end

c JAIR: Old    - chamada dentro de canopy
c JAIR: Update - Fazer wrapper pra chamar individualmente

c ---------------------------------------------------------------------
      subroutine canini(jday, npoi,     
     >                  alaiml, 
     >                  alaimu, 
     >                  cair , 
     >                  cappa,
     >                  cvap,
     >                  grav,
     >                  rair,
     >                  rvap,
     >                  tfac ,
     >                  z0sno ,
     >                  alog1,
     >                  alog2,
     >                  alog3,
     >                  alog4,
     >                  aloga,
     >                  alogav,
     >                  alogg,
     >                  alogi,
     >                  alogl,
     >                  alogu,
     >                  bdl,
     >                  bdu,
     >                  cp,
     >                  dil,
     >                  displ,
     >                  dispu,
     >                  diu,
     >                  exphl,
     >                  exphu,
     >                  expl,
     >                  expu,
     >                  fi,
     >                  fl,
     >                  fu ,
     >                  psurf,
     >                  qa,
     >                  rhoa,
     >                  ta,
     >                  u2,
     >                  ua,
     >                  z0soi,
     >                  z1,
     >                  z12,
     >                  z2,
     >                  z3,
     >                  z34,
     >                  z4,
     >                  za,
     >                  sai,
     >                  lai,
     >                  zbot,
     >                  ztop)
c ---------------------------------------------------------------------
c
c initializes aerodynamic quantities that remain constant 
c through one timestep
c
c note that some quantities actually are
c constant as long as the vegetation amounts and fractional
c coverage remain unchanged, so could re-arrange code for
c efficiency - currently all arrays initialized here are in
c com1d which can be overwritten elsewhere
c
c rwork is used throughout as a scratch variable to reduce number of
c computations
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
c global variables  

      integer npoi 
      
      ! 10
      double precision alaiml   
      double precision alaimu
      double precision cair 
      double precision cappa
      double precision cvap
      double precision grav
      double precision rair
      double precision rvap
      double precision tfac 
      double precision z0sno 
      
      ! 38
      double precision alog1(npoi)
      double precision alog2(npoi)
      double precision alog3(npoi)
      double precision alog4(npoi)
      double precision aloga(npoi)
      double precision alogav(npoi)
      double precision alogg(npoi)
      double precision alogi(npoi)
      double precision alogl(npoi)
      double precision alogu(npoi)
      double precision bdl(npoi)
      double precision bdu(npoi)
      double precision cp(npoi)
      double precision dil(npoi)
      double precision displ(npoi)
      double precision dispu(npoi)
      double precision diu(npoi)
      double precision exphl(npoi)
      double precision exphu(npoi)
      double precision expl(npoi)
      double precision expu(npoi)
      double precision fi(npoi)
      double precision fl(npoi)
      double precision fu(npoi) 
      double precision psurf(npoi)
      double precision qa(npoi)
      double precision rhoa(npoi)
      double precision ta(npoi)
      double precision u2(npoi)
      double precision ua(npoi)
      double precision z0soi(npoi)
      double precision z1(npoi)
      double precision z12(npoi)
      double precision z2(npoi)
      double precision z3(npoi)
      double precision z34(npoi)
      double precision z4(npoi)
      double precision za(npoi)
  
      double precision sai(npoi,2)
      double precision lai(npoi,2)
      double precision zbot(npoi,2)
      double precision ztop(npoi,2)

c Local variables
c
      integer i,jday       ! loop indice
c
      double precision siga,      ! sigma level of atmospheric data
     >     pa,        ! pressure at level of atmospheric data
     >     x,         ! density of vegetation (without distinction between
     >                ! lai,sai)
     >     x1,        ! density of vegetation (different max)
     >     rwork,     ! difference between top and bottom of canopy 
     >     cvegl,     !
     >     dvegl,     ! diffusion coefficient for lower canopy
     >     bvegl,     ! e-folding depth in canopy for lower canopy
     >     cvegu,     !
     >     dvegu,     ! diffusion coefficient for upper canopy
     >     bvegu      ! e-folding depth in canopy for upper canopy
c
c define sigma level of atmospheric data
c
c currently, the value of siga is set to 0.999. This is roughly 10 meters
c above ground, which is the typical height for the CRU05 input wind speed data
c

      siga = 0.997
c
      tfac = 1.0 / (siga**cappa)
c
c atmospheric conditions at za
c za is variable, although siga = p/ps is constant
c
      do 100 i = 1, npoi
c
        pa = psurf(i) * siga
c
        rhoa(i) = pa / ( rair * ta(i) * 
     >            (1.0 + (rvap / rair - 1.0) * qa(i)) )
c
        cp(i) = cair * (1.0 + (cvap / cair - 1.0) * qa(i))
c
        za(i) = (psurf(i) - pa) / (rhoa(i) * grav)
c
c make sure that atmospheric level is higher than canopy top
c
        za(i) = max (za(i), ztop(i,2) + 1.0)
c
 100  continue 
c
c aerodynamic coefficients for the lower story
c
c cvegl (drag coeff for momentum) is proportional, and dvegl
c (diffusion coeff for momentum) inversely proportional,
c to x = density of vegetation (without distinction between
c lai,sai and fl*(1-fi)) - x is not allowed to be exactly
c zero to avoid divide-by-zeros, and for x>1 dvegl is 
c proportional to 1/x**2 so that roughness length tends to
c zero as x tends to infinity
c
c also the top, bottom and displacement heights z3(i),z4(i),
c displ(i) tend to particular values as the density tends to
c zero, to give same results as equations for no veg at all.
c
      do 200 i = 1, npoi
c
        x = fl(i) * (1.0 - fi(i)) * 2.0 * (lai(i,1) + sai(i,1)) / alaiml
c
        x  = min (x, 3.0)
        x1 = min (x, 1.0)
c
        rwork = max(ztop(i,1)-zbot(i,1),0.01)
        cvegl = (0.4 / rwork) *
     >           max(1.e-5, x)
c
        dvegl = (0.1 * rwork) / 
     >           max(1.e-5, x, x**2)
c
c e-folding depth in canopy
c
        bvegl = sqrt (2.0 * cvegl / dvegl )
c
c [(tau/rho)/u**2] for inf canopy
c
        bdl(i) = 0.5 * bvegl * dvegl
c
c 1 / diffusion coefficient
c
        dil(i) = 1. / dvegl
c
        rwork = (1.0 - x1) * (max (z0soi(i),z0sno) + 0.01) 
c
        z3(i) = x1 * ztop(i,1) + rwork
c
        z4(i) = x1 * zbot(i,1) + rwork
c
        z34(i) = 0.5 * (z3(i) + z4(i))
c
        exphl(i) = exp (0.5 * bvegl * (z3(i)-z4(i)))
        expl(i)  = exphl(i)**2
c
        displ(i) = x1 * 0.7 * z3(i)

csant	if(i.eq.1)print*,jday,ztop(i,1),x1,z3(i),displ(i),0.1*(z3(i)-z4(i))
c
 200  continue 
c
c aerodynamic coefficients for the upper story
c same comments as for lower story
c
      do 300 i = 1, npoi
c
        x = fu(i) * 2.0 * (lai(i,2)+sai(i,2)) / alaimu
c
        x  = min (x, 3.0)
        x1 = min (x, 1.0)
c
        rwork = max(ztop(i,2)-zbot(i,2),.01)
        cvegu = (0.4 / rwork) * 
     >           max(1.e-5,x)
c
        dvegu = (0.1 * rwork) / 
     >           max(1.e-5,x,x**2)
c
        rwork = 1. / dvegu
        bvegu  = sqrt (2.0 * cvegu * rwork)
        bdu(i) = 0.5 * bvegu * dvegu
        diu(i) = rwork
c
        rwork = (1.0 - x1) * (z3(i) + 0.01)
        z1(i) = x1 * ztop(i,2) + rwork
        z2(i) = x1 * zbot(i,2) + rwork
c
        z12(i) = 0.5 * (z1(i) + z2(i))
c
        exphu(i) = exp (0.5 * bvegu * (z1(i) - z2(i)))
        expu(i)  = exphu(i)**2
c
        dispu(i) = x1 * 0.7 * z1(i) + (1.0 - x1) * displ(i)
c
 300  continue 
c
c mixing-length logarithms
c
      do 400 i = 1, npoi

c
        alogg(i)  = dlog(z0soi(i))
        alogi(i)  = dlog(z0sno)
        alogav(i) = (1.0 - fi(i)) * alogg(i) + fi(i) * alogi(i)
c
c alog4 must be > z0soi, z0sno to avoid possible problems later 
c
        alog4(i) = dlog( max (z4(i), 1.1*z0soi(i), 1.1*z0sno))
        alog3(i) = dlog(z3(i)-displ(i))
        alog2(i) = dlog(z2(i)-displ(i))
        alog1(i) = dlog(z1(i)-dispu(i))
        aloga(i) = dlog(za(i)-dispu(i))
c
c initialize u2, alogu, alogl for first iteration's fstrat
c
        u2(i)    = ua(i)/exphu(i)
        alogu(i) = dlog(max(.01, .1*(z1(i)-z2(i))))
        alogl(i) = dlog(max(.01, .1*(z3(i)-z4(i))))
c
  400 continue
c

      return
      end

      subroutine novegWrapper(dim_params, npoi_matrix, lai, sai)
            
            implicit none

            integer          dim_params

            double precision npoi_matrix(14, dim_params)
            double precision lai(dim_params, 2)
            double precision sai(dim_params, 2)

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

            integer i

c 	    Inicializando es double de tamanho npoi
            DO 102, i = 1, dim_params, 1
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
102         CONTINUE

            call noveg(dim_params,
     >                 npoi_line1,
     >                 npoi_line2,
     >                 npoi_line3,
     >                 npoi_line4,
     >                 npoi_line5,
     >                 npoi_line6,
     >                 npoi_line7,
     >                 npoi_line8,
     >                 npoi_line9,
     >                 npoi_line10,
     >                 npoi_line11,
     >                 npoi_line12,
     >                 npoi_line13,
     >                 npoi_line14,
     >                 lai, sai)

             DO 109, i = 1, dim_params, 1
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
109         CONTINUE

      end

c ---------------------------------------------------------------------
      subroutine noveg(npoi,
     >                 fi,
     >                 fl,
     >                 fu,
     >                 tg,
     >                 ti,
     >                 tl,
     >                 ts,
     >                 tu,
     >                 wliql,
     >                 wliqs,
     >                 wliqu,
     >                 wsnol,
     >                 wsnos,
     >                 wsnou,
     >                 lai,
     >                 sai)
c ---------------------------------------------------------------------
c
c if no veg surfaces exist, set prog vars to nominal values
c
c (sensible fluxes fsen[u,s,l], latent fluxes fvap[u,s,l]*, 
c temperature t[u,s,l], and intercepted liquid, snow amounts 
c wliq[u,s,l], wsno[u,s,l] have been calculated for a unit 
c leaf/stem surface, whether or not one exists.)
c
      implicit none
c
c      include 'compar.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c
c global variables
      
      integer npoi

      double precision fi(npoi)
      double precision fl(npoi)
      double precision fu(npoi)
      double precision tg(npoi)
      double precision ti(npoi)
      double precision tl(npoi)
      double precision ts(npoi)
      double precision tu(npoi)
      double precision wliql(npoi)
      double precision wliqs(npoi)
      double precision wliqu(npoi)
      double precision wsnol(npoi)
      double precision wsnos(npoi)
      double precision wsnou(npoi)

      double precision lai(npoi,2)
      double precision sai(npoi,2)

c local variables
c
      integer i   ! loop indice
c
      double precision tav,   ! average temp for soil and snow 
     >     x,     ! total lai + sai
     >     y      ! fraction of lower canopy not snow covered 
c
      
      do 100 i = 1, npoi
c
        tav = (1.-fi(i))*tg(i) + fi(i)*ti(i)
c
        if (lai(i,2).eq.0. .or. fu(i).eq.0.) then
          tu(i) = tav
          wliqu(i) = 0.
          wsnou(i) = 0.
        endif
c
        if (sai(i,2).eq.0. .or. fu(i).eq.0.) then
          ts(i) = tav
          wliqs(i) = 0.
          wsnos(i) = 0.
        endif 

c
        x = 2.0 * (lai(i,1) + sai(i,1))
        y = fl(i)*(1.-fi(i))
c
        if (x .eq.0. .or. y.eq.0.) then
          tl(i) = tav 
          wliql(i) = 0.
          wsnol(i) = 0.
        endif
c
  100 continue
c

      return
      end


      

      subroutine cascadeWrapper(dim_params, double_params,
     >                          npoi_matrix,
     >                          lai, sai)
            

            integer          dim_params
            double precision double_params(23)
            double precision npoi_matrix(35, dim_params)
            double precision lai(dim_params, 2)
            double precision sai(dim_params, 2)

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
            double precision npoi_line16(dim_params)
            double precision npoi_line17(dim_params)
            double precision npoi_line18(dim_params)
            double precision npoi_line19(dim_params)
            double precision npoi_line20(dim_params)
            double precision npoi_line21(dim_params)
            double precision npoi_line22(dim_params)
            double precision npoi_line23(dim_params)
            double precision npoi_line24(dim_params)
            double precision npoi_line25(dim_params)
            double precision npoi_line26(dim_params)
            double precision npoi_line27(dim_params)
            double precision npoi_line28(dim_params)
            double precision npoi_line29(dim_params)
            double precision npoi_line30(dim_params)
            double precision npoi_line31(dim_params)
            double precision npoi_line32(dim_params)
            double precision npoi_line33(dim_params)
            double precision npoi_line34(dim_params)
            double precision npoi_line35(dim_params)

            integer i

c 	    Inicializando es double de tamanho npoi
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
                        npoi_line34(i) = npoi_matrix(34, i)
                        npoi_line35(i) = npoi_matrix(35, i)

102         CONTINUE

            call cascade(dim_params, 
     >                   double_params(1), double_params(2), double_params(3),
     >                   double_params(4), double_params(5), double_params(6),
     >                   double_params(7), double_params(8), double_params(9),
     >                   double_params(10), double_params(11), double_params(12),
     >                   double_params(13), double_params(14), double_params(15),
     >                   double_params(16), double_params(17), double_params(18),
     >                   double_params(19), double_params(20), double_params(21),
     >                   double_params(22), double_params(23),
     >                   npoi_line1,
     >                   npoi_line2,
     >                   npoi_line3,
     >                   npoi_line4,
     >                   npoi_line5,
     >                   npoi_line6,
     >                   npoi_line7,
     >                   npoi_line8,
     >                   npoi_line9,
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
     >                   npoi_line29,
     >                   npoi_line30,
     >                   npoi_line31,
     >                   npoi_line32,
     >                   npoi_line33,
     >                   npoi_line34,
     >                   npoi_line35,
     >                   lai, sai)

c 	    Inicializando es double de tamanho npoi
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
                        npoi_matrix(34, i) = npoi_line34(i)
                        npoi_matrix(35, i) = npoi_line35(i)
777         CONTINUE

      end

c ---------------------------------------------------------------------
      subroutine cascade(npoi,
     >                   cair,
     >                   hvap,
     >                   cvap,
     >                   ch2o,
     >                   cice,
     >                   epsilon,
     >                   hfus,
     >                   dtime,
     >                   tblowl,
     >                   tblows,
     >                   tblowu,
     >                   tdripl,
     >                   tdrips,
     >                   tdripu,
     >                   tmelt,
     >                   wliqlmax,
     >                   wliqmin,
     >                   wliqsmax,
     >                   wliqumax,
     >                   wsnolmax,
     >                   wsnomin,
     >                   wsnoumax,
     >                   wsnosmax,
     >                   fl,
     >                   fu,
     >                   pfluxl,
     >                   pfluxs,
     >                   pfluxu,
     >                   psurf,
     >                   qa,
     >                   raina,
     >                   raing,
     >                   rainl,
     >                   rainu,
     >                   snowa,
     >                   snowg,
     >                   snowl,
     >                   snowu,
     >                   t12,
     >                   t34,
     >                   ta,
     >                   tl,
     >                   traing,
     >                   trainl,
     >                   trainu,
     >                   ts,
     >                   tsnowg,
     >                   tsnowl,
     >                   tsnowu,
     >                   tu,
     >                   vzero,
     >                   wliql,
     >                   wliqs,
     >                   wliqu,
     >                   wsnol,
     >                   wsnos,
     >                   wsnou  ,
     >                   xirriga,
     >                   lai,
     >                   sai)
c ---------------------------------------------------------------------
c
c steps intercepted h2o due to drip, precip, and min/max limits
c
c calls steph2o for upper leaves, upper stems and lower veg in
c iurn, adjusting precips at each level
c
      implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comveg.h'
c      include 'com1d.h'
c      include 'comcrop.h'
c
c global variables

       integer npoi
       
       ! 23
       double precision cair
       double precision hvap
       double precision cvap
       double precision ch2o
       double precision cice
       double precision epsilon
       double precision hfus
       double precision dtime
       double precision tblowl
       double precision tblows
       double precision tblowu
       double precision tdripl
       double precision tdrips
       double precision tdripu
       double precision tmelt
       double precision wliqlmax
       double precision wliqmin
       double precision wliqsmax
       double precision wliqumax
       double precision wsnolmax
       double precision wsnomin
       double precision wsnoumax
       double precision wsnosmax


       ! 35 
       double precision fl(npoi)
       double precision fu(npoi)
       double precision pfluxl(npoi)
       double precision pfluxs(npoi)
       double precision pfluxu(npoi)
       double precision psurf(npoi)
       double precision qa(npoi)
       double precision raina(npoi)
       double precision raing(npoi)
       double precision rainl(npoi)
       double precision rainu(npoi)
       double precision snowa(npoi)
       double precision snowg(npoi)
       double precision snowl(npoi)
       double precision snowu(npoi)
       double precision t12(npoi)
       double precision t34(npoi)
       double precision ta(npoi)
       double precision tl(npoi)
       double precision traing(npoi)
       double precision trainl(npoi)
       double precision trainu(npoi)
       double precision ts(npoi)
       double precision tsnowg(npoi)
       double precision tsnowl(npoi)
       double precision tsnowu(npoi)
       double precision tu(npoi)
       double precision vzero(npoi)
       double precision wliql(npoi)
       double precision wliqs(npoi)
       double precision wliqu(npoi)
       double precision wsnol(npoi)
       double precision wsnos(npoi)
       double precision wsnou(npoi)  
       double precision xirriga(npoi)
       
       double precision lai(npoi,2)
       double precision sai(npoi,2)



c local variables
c
      integer i            ! loop indice
c
      double precision twet3           ! Function: wet bulb temperature (K)
      double precision twetbulb        ! wet bulb temperature (K)
c    
      double precision
     >  xai(npoi),         !lai and/or sai for veg component
                           ! (allows steph2o to work on any veg component)
     >  rain(npoi),        !rainfall at appropriate level (modified by steph2o)
     >  train(npoi),       !temperature of rain (modified by steph2o)  
     >  snow(npoi),        !snowfall at appropriate level (modified by steph2o)
     >  tsnow(npoi),       !temperature of snow (modified by steph2o)
     >  x1(npoi),        ! 
     >  x2(npoi),        ! 
     >  x3(npoi),        ! 
     >  x4(npoi)         ! 

c
c adjust rainfall and snowfall rates at above-tree level
c
c set wliqmin, wsnomin -- unlike wliq*max, wsno*max, these are
c part of the lsx numerical method and not from the vegetation
c database, and they are the same for all veg components
c
c the value 0.0010 should be small compared to typical precip rates
c times dtime to allow any intercepted h2o to be initiated, but
c not too small to allow evap rates to reduce wliq*, wsno* to
c that value in a reasonable number of time steps
c
      wliqmin = 0.0010 * (dtime/3600.) * (wliqumax / 0.2)
      wsnomin = 0.0010 * (dtime/3600.) * (wsnoumax / 2.0)
c
      do 50 i=1,npoi
        rainu(i) = raina(i)
c
c add amount for irrigation  - C. Kucharik 04/11/01
c
        rainu(i) = rainu(i) + xirriga(i)
c
c set rain temperature to the wet bulb temperature
c
        if (ta(i) .gt. tmelt) then
           twetbulb = twet3(ta(i),qa(i), psurf(i),      
     >                    cair,
     >                    hvap,
     >                    cvap,
     >                    ch2o)
        else
           twetbulb = tmelt
        endif
        trainu(i) = max (twetbulb, tmelt)
        x1(i) = 0.0
        x2(i) = max (t12(i), tmelt)
   50 continue
c
      call mix (rainu,trainu, rainu,trainu, x1,x2, vzero,vzero, npoi, epsilon)
c
      do 52 i=1,npoi
        snowu(i) = snowa(i)
        tsnowu(i) = min (ta(i), tmelt)
        x1(i) = 0.0
        x2(i) = min (t12(i), tmelt)
   52 continue
c
      call mix (snowu,tsnowu, snowu,tsnowu, x1,x2, vzero,vzero, npoi, epsilon)
c
c set up for upper leaves
c
      do 100 i = 1, npoi
        xai(i)   = 2.0 * lai(i,2)
        rain(i)  = rainu(i)
        train(i) = trainu(i)
        snow(i)  = snowu(i)
        tsnow(i) = tsnowu(i)
  100 continue
c
c step upper leaves
c
      call steph2o
     >  (tu,  wliqu,  wsnou,  xai,  pfluxu,  rain, train, snow, tsnow,
     >   tdripu, tblowu, wliqumax, wsnoumax, wliqmin, wsnomin,
     >   ch2o,
     >   cice,
     >   dtime,
     >   epsilon,
     >   hfus,
     >   npoi,
     >   tmelt,
     >   vzero)
c
c set up for upper stems
c the upper stems get precip as modified by the upper leaves
c
      do 200 i=1,npoi
        xai(i) = 2.0 * sai(i,2)
  200 continue
c
c step upper stems
c
   
      call steph2o
     >  (ts,  wliqs,  wsnos,  xai,  pfluxs,  rain, train, snow, tsnow,
     >   tdrips, tblows, wliqsmax, wsnosmax, wliqmin, wsnomin,
     >   ch2o,
     >   cice,
     >   dtime,
     >   epsilon,
     >   hfus,
     >   npoi,
     >   tmelt,
     >   vzero)
   
c
c adjust rainfall and snowfall rates at below-tree level
c allowing for upper-veg interception/drip/belowoff
c
      do 300 i=1,npoi
        x1(i) = fu(i)*rain(i)
        x2(i) = (1.-fu(i))*rainu(i)
        x3(i) = 0.0
        x4(i) = max (t34(i), tmelt)
  300 continue
c
      call mix (rainl,trainl, x1,train, x2,trainu, x3,x4, npoi, epsilon)
c
      do 310 i=1,npoi
        x1(i) = fu(i)*snow(i)
        x2(i) = (1.-fu(i))*snowu(i)
        x3(i) = 0.0
        x4(i) = min (t34(i), tmelt)
  310 continue
c
      call mix (snowl,tsnowl, x1,tsnow, x2,tsnowu, x3,x4, npoi, epsilon)
c
c set up for lower veg
c
      do 400 i = 1, npoi
        xai(i)   = 2.0 * (lai(i,1) + sai(i,1))
        rain(i)  = rainl(i)
        train(i) = trainl(i)
        snow(i)  = snowl(i)
        tsnow(i) = tsnowl(i)
  400 continue
c
c step lower veg
c
      call steph2o
     >  (tl,  wliql,  wsnol,  xai,  pfluxl,  rain, train, snow, tsnow,
     >   tdripl, tblowl, wliqlmax, wsnolmax, wliqmin, wsnomin,
     >   ch2o,
     >   cice,
     >   dtime,
     >   epsilon,
     >   hfus,
     >   npoi,
     >   tmelt,
     >   vzero)
c
c adjust rainfall and  snowfall rates at soil level,
c allowing for lower-veg interception/drip/blowoff
c
      do 500 i=1,npoi
        x1(i) = fl(i) * rain(i)
        x2(i) = (1.-fl(i)) * rainl(i)
  500 continue
c
      call mix (raing,traing, x1,train, x2,trainl, vzero,vzero, npoi, epsilon)
c
      do 510 i=1,npoi
        x1(i) = fl(i) * snow(i)
        x2(i) = (1.-fl(i)) * snowl(i)
  510 continue
c
      call mix (snowg,tsnowg, x1,tsnow, x2,tsnowl, vzero,vzero, npoi, epsilon)
c
      return
      end

     
      subroutine cascad2Wrapper(dim_params, double_params,
     >                          npoi_matrix,
     >                          lai,
     >                          sai)
            
            implicit none

            integer          dim_params
                
            double precision double_params(12)

            double precision lai(dim_params, 2)
            double precision sai(dim_params, 2)

            double precision npoi_matrix(22, dim_params)

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
            double precision npoi_line16(dim_params)
            double precision npoi_line17(dim_params)
            double precision npoi_line18(dim_params)
            double precision npoi_line19(dim_params)
            double precision npoi_line20(dim_params)
            double precision npoi_line21(dim_params)
            double precision npoi_line22(dim_params)
            
            integer i

c 	    Inicializando es double de tamanho npoi
            DO 102, i = 1, dim_params, 1
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
102         CONTINUE
           
           call cascad2(dim_params, double_params(1), double_params(2),
     >                  double_params(3), double_params(4), double_params(5),
     >                  double_params(6), double_params(7), double_params(8),
     >                  double_params(9), double_params(10),double_params(11),
     >                  double_params(12),
     >                  npoi_line1,
     >                  npoi_line2,
     >                  npoi_line3,
     >                  npoi_line4,
     >                  npoi_line5,
     >                  npoi_line6,
     >                  npoi_line7,
     >                  npoi_line8,
     >                  npoi_line9,
     >                  npoi_line10,
     >                  npoi_line11,
     >                  npoi_line12,
     >                  npoi_line13,
     >                  npoi_line14,
     >                  npoi_line15,
     >                  npoi_line16,
     >                  npoi_line17,
     >                  npoi_line18,
     >                  npoi_line19,
     >                  npoi_line20,
     >                  npoi_line21,
     >                  npoi_line22,
     >                  lai, sai)

c 	    Inicializando es double de tamanho npoi
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
                        npoi_matrix(16, i) = npoi_line16(i)
                        npoi_matrix(17, i) = npoi_line17(i)
                        npoi_matrix(18, i) = npoi_line18(i)
                        npoi_matrix(19, i) = npoi_line19(i)
                        npoi_matrix(20, i) = npoi_line20(i)
                        npoi_matrix(21, i) = npoi_line21(i)
                        npoi_matrix(22, i) = npoi_line22(i)
777         CONTINUE

      end


c ---------------------------------------------------------------------
      subroutine cascad2(npoi,
     >                   chl,
     >                   chs,
     >                   chu,
     >                   ch2o,
     >                   cice,
     >                   dtime,
     >                   epsilon,
     >                   hfus,
     >                   tmelt,
     >                   cvap,
     >                   hvap,
     >                   hsub,
     >                   ta,
     >                   fvapa,
     >                   fsena,
     >                   fi,
     >                   fl,
     >                   fu,
     >                   fvaplw,
     >                   fvaps,
     >                   fvapuw,
     >                   rliql,
     >                   rliqs,
     >                   rliqu,
     >                   tl,
     >                   ts,
     >                   tu,
     >                   wliql,
     >                   wliqs,
     >                   wliqu,
     >                   wsnol,
     >                   wsnos,
     >                   wsnou,
     >                   vzero,
     >                   lai,
     >                   sai)
c ---------------------------------------------------------------------
c
c at end of timestep, removes evaporation from intercepted h2o,
c and does final heat-conserving adjustment for any liquid/snow 
c below/above melt point. calls steph2o2 for upper leaves, 
c upper stems and lower veg in turn.
c
       implicit none
c
c      include 'compar.h'
c      include 'comsno.h'
c      include 'comveg.h'
c      include 'com1d.h'
c
c global variables
        
        integer  npoi

        double precision chl
        double precision chs
        double precision chu
        double precision ch2o
        double precision cice
        double precision dtime
        double precision epsilon
        double precision hfus
        double precision tmelt
        double precision cvap
        double precision hvap
        double precision hsub

        double precision ta(npoi)
        double precision fvapa(npoi)
        double precision fsena(npoi)
        double precision fi(npoi)
        double precision fl(npoi)
        double precision fu(npoi)
        double precision fvaplw(npoi)
        double precision fvaps(npoi)
        double precision fvapuw(npoi)
        double precision rliql(npoi)
        double precision rliqs(npoi)
        double precision rliqu(npoi)
        double precision tl(npoi)
        double precision ts(npoi)
        double precision tu(npoi)
        double precision wliql(npoi)
        double precision wliqs(npoi)
        double precision wliqu(npoi)
        double precision wsnol(npoi)
        double precision wsnos(npoi)
        double precision wsnou(npoi)
        double precision vzero(npoi)

        double precision lai(npoi,2)
        double precision sai(npoi,2)

        

c local variables
c
      integer i           ! loop indice
c
      double precision fveg(npoi),    ! fractional areal coverage of veg component
     >     xai(npoi)      ! lai and/or sai for veg component
c
c ---------------------------------------------------------------------
c
c set up for upper leaves
c
      do 100 i=1,npoi
        fveg(i) = fu(i)
        xai(i) = 2.0 * lai(i,2)
  100 continue
c
c step upper leaves
c
      
      call steph2o2 (tu,wliqu,wsnou,fveg,xai,rliqu,fvapuw,chu,
     >               ch2o,
     >               cice,
     >               dtime,
     >               fsena,
     >               fvapa,
     >               hfus,
     >               npoi,
     >               ta,
     >               tmelt,
     >               cvap,
     >               hvap,
     >               hsub)
c
c set up for upper stems
c
      do 200 i=1,npoi
        fveg(i) = fu(i)
        xai(i) = 2.0 * sai(i,2)
  200 continue
c
c step upper stems
c
        
      call steph2o2 (ts,wliqs,wsnos,fveg,xai,rliqs,fvaps,chs,
     >               ch2o,
     >               cice,
     >               dtime,
     >               fsena,
     >               fvapa,
     >               hfus,
     >               npoi,
     >               ta,
     >               tmelt,
     >               cvap,
     >               hvap,
     >               hsub)
          
c
c set up for lower veg
c
      do 400 i=1,npoi
        fveg(i) = (1.-fi(i))*fl(i)
        xai(i) = 2.0 * (lai(i,1) + sai(i,1))
  400 continue
c
c step lower veg
c
 
      call steph2o2 (tl,wliql,wsnol,fveg,xai,rliql,fvaplw,chl,
     >               ch2o,
     >               cice,
     >               dtime,
     >               fsena,
     >               fvapa,
     >               hfus,
     >               npoi,
     >               ta,
     >               tmelt,
     >               cvap,
     >               hvap,
     >               hsub)
c
      return
      end


c ---------------------------------------------------------------------
      subroutine steph2o
     >  (tveg,  wliq,  wsno,  xai,  pflux,  rain, train, snow, tsnow,
     >   tdrip, tblow, wliqmax, wsnomax, wliqmin, wsnomin,
     >   ch2o,
     >   cice,
     >   dtime,
     >   epsilon,
     >   hfus,
     >   npoi,
     >   tmelt,
     >   vzero)
c ---------------------------------------------------------------------
c
c steps intercepted h2o for one canopy component (upper leaves, 
c upper stems, or lower veg) through one lsx time step, adjusting
c for h2o sensible heat and phase changes. also modifies precip
c due to interception and drip,blowoff
c
c 
c
       implicit none
c
       integer npoi

       double precision ch2o
       double precision cice
       double precision dtime
       double precision epsilon
       double precision hfus

       double precision tmelt
       double precision vzero(npoi)
c
c Arguments (all arguments are supplied (unchanged) unless otherwise noted
c
      double precision tdrip,       ! e-folding time of liquid drip  tdrip[u,s,l]
     >     tblow,       ! e-folding time of snow blowoff tblow[u,s,l]
     >     wliqmax,     ! max amount of intercepted liquid wliq[u,s,l]max
     >     wsnomax,     ! max amount of intercepted snow   wsno[u,s,l]max
     >     wliqmin,     ! min amount of intercepted liquid (same name for u,s,l)
     >     wsnomin      ! min amount of intercepted snow (same name for u,s,l)
c
      double precision       
     >  tveg(npoi),     ! temperature of veg component t[u,s,l]
     >  wliq(npoi),     ! intercepted liquid amount wliq[u,s,l] (returned)
     >  wsno(npoi),     ! intercepted snow amount wsno[u,s,l] (returned)
     >  xai(npoi),      ! lai, sai, lai+sai for upper leaves/stems,lower veg
     >  pflux(npoi),    ! ht flux due to adjust of intercep precip (returned)
     >  rain(npoi),     ! rainfall rate. Input: above veg, Output: below veg
     >  train(npoi),    ! temperature of rain. (returned)
     >  snow(npoi),     ! snowfall rate. Input: above veg, output: below veg
     >  tsnow(npoi)     ! temperature of snow (returned)
c
c local variables:
c
      integer i         ! loop indice
c
      double precision rwork,       ! 1/dtime
     >     x,           ! work variable
     >     rwork2,      ! work variable: ch2o - cice
     >     dw           ! correction: freezing liguid or melting snow
c
      double precision fint(npoi),  ! precip fraction intercepted by unit leaf/stem area
     >     drip(npoi),  ! rate of liquid drip
     >     blow(npoi)   ! rate of snow blowoff
c
c ---------------------------------------------------------------------
c
c calculate fint, the intercepted precip fraction per unit
c leaf/stem area -- note 0.5 * lai or sai (similar to irrad)
c 
      do 50 i = 1, npoi
c
        if (xai(i).ge.epsilon) then
          fint(i) =  1.-exp(-0.5*xai(i)) 
        else
          fint(i) = 0.1
        endif
c
   50 continue
c
c step intercepted liquid and snow amounts due to drip/blow,
c intercepted rainfall/snowfall, and min/max limits. also 
c adjust temperature of intercepted precip to current veg temp,
c storing the heat needed to do this in pflux for use in turvap
c 
c without these pfluxes, the implicit turvap calcs could not
c account for the heat flux associated with precip adjustments,
c especially changes of phase (see below), and so could not
c handle equilibrium situations such as intercepted snowfall
c being continuously melted by warm atmos fluxes, with the veg 
c temp somewhat lower than the equil atmos temp to supply heat
c that melts the incoming snow; (turvap would just change veg 
c temp to atmos equil, with little sensible heat storage...then
c final phase adjustment would return veg temp to melt point)
c
c the use of the current (ie, previous timestep's) veg temp 
c gives the best estimate of what this timestep's final temp
c will be, at least for steady conditions
c
      rwork = 1. / dtime
c
      do 100 i=1,npoi
c    
c liquid
c
        drip(i) = xai(i)*wliq(i)/tdrip
        wliq(i) = wliq(i) * (1.-dtime/tdrip)
c
        wliq(i) = wliq(i) + dtime*rain(i)*fint(i)
        pflux(i) = rain(i)*fint(i) * (tveg(i)-train(i))*ch2o
        rain(i) = rain(i)*(1.-xai(i)*fint(i))
c
        x = wliq(i)
        wliq(i) = min (wliq(i), wliqmax)
        if (wliq(i).lt.wliqmin) wliq(i) = 0.
        drip(i) = drip(i) + xai(i)*(x-wliq(i))*rwork
c
c snow
c
        blow(i) = xai(i)*wsno(i)/tblow
        wsno(i) = wsno(i) * (1.-dtime/tblow)
c
        wsno(i) = wsno(i) + dtime*snow(i)*fint(i)
        pflux(i) = pflux(i) + snow(i)*fint(i) * (tveg(i)-tsnow(i))*cice
        snow(i) = snow(i)*(1.-xai(i)*fint(i))
c
        x = wsno(i)
        wsno(i) = min (wsno(i), wsnomax)
        if (wsno(i).lt.wsnomin) wsno(i) = 0. 
        blow(i) = blow(i) + xai(i)*(x-wsno(i))*rwork
c
  100 continue
c
c change phase of liquid/snow below/above melt point, and add
c required heat to pflux (see comments above). this will only
c affect the precip intercepted in this timestep, since original
c wliq, wsno must have been ge/le melt point (ensured in later
c call to cascad2/steph2o2)
c
      rwork2 = ch2o - cice
c
      do 300 i=1,npoi
c
c liquid below freezing
c
        dw = 0.
        if (tveg(i).lt.tmelt)  dw = wliq(i)
c
        pflux(i) = pflux(i)
     >           + dw * (rwork2*(tmelt-tveg(i)) - hfus) * rwork
        wliq(i) = wliq(i) - dw
        wsno(i) = wsno(i) + dw
c
c snow above freezing
c
        dw = 0.
        if (tveg(i).gt.tmelt)  dw = wsno(i)
c
        pflux(i) = pflux(i)
     >           + dw * (rwork2*(tveg(i)-tmelt) + hfus) * rwork
        wsno(i) = wsno(i) - dw
        wliq(i) = wliq(i) + dw
c
  300 continue
c
c adjust rainfall, snowfall below veg for interception 
c and drip, blowoff
c
      call mix (rain,train, rain,train, drip,tveg, vzero,vzero, npoi, epsilon)
      call mix (snow,tsnow, snow,tsnow, blow,tveg, vzero,vzero, npoi, epsilon)
c
      return
      end

c ---------------------------------------------------------------------
      subroutine steph2o2 (tveg,wliq,wsno,fveg,xai,rliq,fvapw,cveg,
     >                     ch2o,
     >                     cice,
     >                     dtime,
     >                     fsena,
     >                     fvapa,
     >                     hfus,
     >                     npoi,
     >                     ta,
     >                     tmelt,
     >                     cvap,
     >                     hvap,
     >                     hsub)
c ---------------------------------------------------------------------
c
c removes evaporation from intercepted h2o, and does final
c heat-conserving adjustment for any liquid/snow below/above
c melt point, for one veg component
c
       implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'com1d.h'
c
c global variables
      double precision ch2o	        
      double precision cice	        
      double precision dtime	        
      double precision fsena(npoi)	
      double precision fvapa(npoi)	
      double precision hfus	        	
      integer          npoi	        
      double precision ta(npoi)	
      double precision tmelt
      double precision cvap
      double precision hvap
      double precision hsub      

c Arguments (all arguments are supplied unless otherwise noted)
c
      double precision cveg        ! specific heat of veg component ch[u,s,l] 
c
      double precision      
     >  tveg(npoi),    ! temperature of veg component t[u,s,l] (returned)
     >  wliq(npoi),    ! intercepted liquid amount wliq[u,s,l] (returned)
     >  wsno(npoi),    ! intercepted snow amount wsno[u,s,l] (returned)
     >  fveg(npoi),    ! fractional areal coverage, fu or (1-fi)*fl
     >  xai(npoi),     ! lai, sai, lai+sai for upper leaves/stems,lower veg
     >  rliq(npoi),    ! ratio of area wetted by liquid to total wetted area
     >  fvapw(npoi)    ! wetted evap h2o flx per leaf/stem area fvap[uw,s,lw]
c
c local variables
c
      integer i        ! loopi indice
c
      double precision zm,         ! to compute corrective fluxes
     >     rwork,      ! 1/specific heat of fusion 
     >     chav        ! average specific heat for veg, liw and snow
c
      double precision dh(npoi),   ! correct heat flux for liquid below melt point and opposite
     >     dw(npoi)    ! correct water flux for liquid below melt point and opposite
c
c
c      include 'comsat.h'

            double precision asat0, asat1, asat2, asat3, asat4, asat5, asat6
c
      parameter (asat0 =  6.1078000,
     >           asat1 =  4.4365185e-1,
     >           asat2 =  1.4289458e-2,
     >           asat3 =  2.6506485e-4,
     >           asat4 =  3.0312404e-6,
     >           asat5 =  2.0340809e-8,
     >           asat6 =  6.1368209e-11 )
c
      double precision bsat0, bsat1, bsat2, bsat3, bsat4, bsat5, bsat6
c
      parameter (bsat0 =  6.1091780,
     >           bsat1 =  5.0346990e-1,
     >           bsat2 =  1.8860134e-2,
     >           bsat3 =  4.1762237e-4,
     >           bsat4 =  5.8247203e-6,
     >           bsat5 =  4.8388032e-8,
     >           bsat6 =  1.8388269e-10 )
c
      double precision csat0, csat1, csat2, csat3, csat4, csat5, csat6
c
      parameter (csat0 =  4.4381000e-1,
     >           csat1 =  2.8570026e-2,
     >           csat2 =  7.9380540e-4,
     >           csat3 =  1.2152151e-5,
     >           csat4 =  1.0365614e-7,
     >           csat5 =  3.5324218e-10,
     >           csat6 = -7.0902448e-13 )
c
      double precision dsat0, dsat1, dsat2, dsat3, dsat4, dsat5, dsat6
c
      parameter (dsat0 =  5.0303052e-1,
     >           dsat1 =  3.7732550e-2,
     >           dsat2 =  1.2679954e-3,
     >           dsat3 =  2.4775631e-5,
     >           dsat4 =  3.0056931e-7,
     >           dsat5 =  2.1585425e-9,
     >           dsat6 =  7.1310977e-12 )

      double precision t,        ! temperature argument of statement function 
     >     tair,     ! temperature argument of statement function 
     >     p1,       ! pressure argument of function 
     >     e1,       ! vapor pressure argument of function
     >     q1,       ! saturation specific humidity argument of function
     >     tsatl,    ! statement function
     >     tsati,    ! 
     >     esat,     !
     >     desat,    !
     >     qsat,     ! 
     >     dqsat,    ! 
     >     hvapf,    ! 
     >     hsubf,    !
     >     cvmgt     ! function

      tsatl(t) = min (100., max (t-273.16, 0.))
      tsati(t) = max (-60., min (t-273.16, 0.))

      esat (t) = 
     >  100.*(
     >    cvmgt (asat0, bsat0, t.ge.273.16)
     >    + tsatl(t)*(asat1 + tsatl(t)*(asat2 + tsatl(t)*(asat3
     >    + tsatl(t)*(asat4 + tsatl(t)*(asat5 + tsatl(t)* asat6)))))
     >    + tsati(t)*(bsat1 + tsati(t)*(bsat2 + tsati(t)*(bsat3
     >    + tsati(t)*(bsat4 + tsati(t)*(bsat5 + tsati(t)* bsat6)))))
     >  )

      desat (t) =
     >  100.*(
     >    cvmgt (csat0, dsat0, t.ge.273.16)
     >    + tsatl(t)*(csat1 + tsatl(t)*(csat2 + tsatl(t)*(csat3
     >    + tsatl(t)*(csat4 + tsatl(t)*(csat5 + tsatl(t)* csat6)))))
     >    + tsati(t)*(dsat1 + tsati(t)*(dsat2 + tsati(t)*(dsat3
     >    + tsati(t)*(dsat4 + tsati(t)*(dsat5 + tsati(t)* dsat6)))))
     >  )

       qsat (e1, p1) = 0.622 * e1 /
     >               max ( p1 - (1.0 - 0.622) * e1, 0.622 * e1 )

       dqsat (t, q1) = desat(t) * q1 * (1. + q1*(1./0.622 - 1.)) /
     >                 esat(t)  

c
c ---------------------------------------------------------------------
c
c step intercepted h2o due to evaporation/sublimation.
c (fvapw already has been multiplied by fwet factor in turvap,
c so it is per unit leaf/stem area.)
c
c due to linear fwet factors (see comments in fwetcal) and
c the cap on suw,ssw,slw in turvap, evaporation in one timestep
c should hardly ever make wliq or wsno negative -- but if this
c happens, compensate by increasing vapor flux from atmosphere, 
c and decreasing sensib heat flux from atmos (the former is
c dangerous since it could suck moisture out of a dry atmos,
c and both are unphysical but do fix the budget) tveg in hvapf
c and hsubf should be pre-turvap-timestep values, but are not
c  
      do 100 i = 1, npoi
c
        wliq(i) = wliq(i) - dtime *     rliq(i)  * fvapw(i)
        wsno(i) = wsno(i) - dtime * (1.-rliq(i)) * fvapw(i)
c
c check to see if predicted wliq or wsno are less than zero
c
        if ((wliq(i).lt.0. or. wsno(i).lt.0.)
     >      .and. fveg(i)*xai(i).gt.0. )  then
c
c         write (*,9999) i, wliq(i), wsno(i)
c9999     format(' ***warning: wliq<0 or wsno<0 -- steph2o2 9999',
c    >           ' i, wliq, wsno:',i4, 2f12.6)
c
c calculate corrective fluxes
c
          zm = max (-wliq(i), 0.) * fveg(i) * xai(i) / dtime
          fvapa(i) = fvapa(i) + zm
          fsena(i) = fsena(i) - zm * 
     >    hvap + cvap*(ta(i)-273.16) - ch2o*(tveg(i)-273.16) !hvapf(tveg(i),ta(i))
          wliq(i) = max (wliq(i), 0.)
c
          zm = max (-wsno(i), 0.) * fveg(i) * xai(i) / dtime
          fvapa(i) = fvapa(i) + zm
          fsena(i) = fsena(i) - zm * 
     >    hsub + cvap*(ta(i)-273.16) - cice*(tveg(i)-273.16) !hsubf(tveg(i),ta(i))
          wsno(i) = max (wsno(i), 0.)
c
        endif
c
  100 continue
c
c final heat-conserving correction for liquid/snow below/above
c melting point
c
      rwork = 1. / hfus
c
      do 200 i=1,npoi
c
        chav = cveg + ch2o*wliq(i) + cice*wsno(i)
c
c correct for liquid below melt point
c
c (nb: if tveg > tmelt or wliq = 0, nothing changes.)
c
        if (tveg(i).lt.tmelt .and. wliq(i).gt.0.0) then
          dh(i) = chav*(tmelt - tveg(i))
          dw(i) = min (wliq(i), max (0., dh(i)*rwork))
          wliq(i) = wliq(i) - dw(i)
          wsno(i) = wsno(i) + dw(i) 
          chav = cveg + ch2o*wliq(i) + cice*wsno(i)
          tveg(i) = tmelt - (dh(i)-hfus*dw(i))/chav
        endif
c
c correct for snow above melt point
c
c (nb: if tveg < tmelt or wsno = 0, nothing changes.)
c
        if (tveg(i).gt.tmelt .and. wsno(i).gt.0.0) then
          dh(i) = chav*(tveg(i) - tmelt)
          dw(i) = min (wsno(i), max (0., dh(i)*rwork))
          wsno(i) = wsno(i) - dw(i)
          wliq(i) = wliq(i) + dw(i)
          chav = cveg + ch2o*wliq(i) + cice*wsno(i)
          tveg(i) = tmelt + (dh(i)-hfus*dw(i))/chav
        endif
c
  200 continue
c
      return
      end


c ---------------------------------------------------------------------
      subroutine mix (xm,tm, x1,t1, x2,t2, x3,t3,npoi,epsilon)
c ---------------------------------------------------------------------
c
c calorimetrically mixes masses x1,x2,x3 with temperatures
c t1,t2,t3 into combined mass xm with temperature tm
c
c xm,tm may be returned into same location as one of x1,t1,..,
c so hold result temporarily in xtmp,ttmp below
c
c will work if some of x1,x2,x3 have opposite signs, but may 
c give unphysical tm's
c
      implicit none
c
c      include 'compar.h'
c

      integer npoi
      double precision epsilon

c Arguments (input except for xm, tm)
c
      double precision xm(npoi),     ! resulting mass  
     >     tm(npoi),     ! resulting temp
     >     x1(npoi),     ! mass 1
     >     t1(npoi),     ! temp 1
     >     x2(npoi),     ! mass 2
     >     t2(npoi),     ! temp 2
     >     x3(npoi),     ! mass 3
     >     t3(npoi)      ! temp 3
c
c local variables
c
      integer i          ! loop indice
c
      double precision xtmp,         ! resulting mass (storing variable)
     >                 ytmp,         !  "
     >                 ttmp          ! resulting temp
c
c ---------------------------------------------------------------------
c
      do 100 i=1,npoi
c
        xtmp = x1(i) + x2(i) + x3(i)
c
        ytmp = sign (max (abs(xtmp), epsilon), xtmp)
c
        if (abs(xtmp).ge.epsilon) then
          ttmp = (t1(i)*x1(i) + t2(i)*x2(i) + t3(i)*x3(i)) / ytmp
        else
          ttmp = 0.
          xtmp = 0.
        endif
c
        xm(i) = xtmp
        tm(i) = ttmp
c
  100 continue
c
      return
      end
c

      subroutine stomataWrapper(iter, time, jday,
     >                          dim_params, double_params,
     >                          npoi_matrix,
     >                          f1,
     >                          f2,
     >                          exist,
     >                          grnfraccrop,
     >                          hitemp,
     >                          lai,
     >                          lotemp,
     >                          sai,
     >                          scalcoefl,
     >                          scalcoefu,
     >                          stressn,
     >                          terml,
     >                          termu,
     >                          use,
     >                          vmax_pft,
     >                          croplive)

                implicit none

                integer iter, jday

                double precision time

                integer dim_params(4)

                double precision double_params(46)

                double precision npoi_matrix(64, dim_params(1))
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
                double precision npoi_line34(dim_params(1))
                double precision npoi_line35(dim_params(1))
                double precision npoi_line36(dim_params(1))
                double precision npoi_line37(dim_params(1))
                double precision npoi_line38(dim_params(1))
                double precision npoi_line39(dim_params(1))
                double precision npoi_line40(dim_params(1))
                double precision npoi_line41(dim_params(1))
                double precision npoi_line42(dim_params(1))
                double precision npoi_line43(dim_params(1))
                double precision npoi_line44(dim_params(1))
                double precision npoi_line45(dim_params(1))
                double precision npoi_line46(dim_params(1))
                double precision npoi_line47(dim_params(1))
                double precision npoi_line48(dim_params(1))
                double precision npoi_line49(dim_params(1))
                double precision npoi_line50(dim_params(1))
                double precision npoi_line51(dim_params(1))
                double precision npoi_line52(dim_params(1))
                double precision npoi_line53(dim_params(1))
                double precision npoi_line54(dim_params(1))
                double precision npoi_line55(dim_params(1))
                double precision npoi_line56(dim_params(1))
                double precision npoi_line57(dim_params(1))
                double precision npoi_line58(dim_params(1))
                double precision npoi_line59(dim_params(1))
                double precision npoi_line60(dim_params(1))
                double precision npoi_line61(dim_params(1))
                double precision npoi_line62(dim_params(1))
                double precision npoi_line63(dim_params(1))
                double precision npoi_line64(dim_params(1))

                double precision f1(dim_params(2))
                double precision f2(dim_params(2))
                double precision exist(dim_params(1),dim_params(2))
                double precision grnfraccrop(dim_params(1),dim_params(2))
                double precision hitemp(dim_params(2))
                double precision lai(dim_params(1),2)
                double precision lotemp(dim_params(2))
                double precision sai(dim_params(1),2)
                double precision scalcoefl(dim_params(1),4)
                double precision scalcoefu(dim_params(1),4)
                double precision stressn(dim_params(1),dim_params(2))
                double precision terml(dim_params(1),7)
                double precision termu(dim_params(1),7)
                double precision use(4,24)
                double precision vmax_pft(dim_params(2))
                double precision croplive(dim_params(1),dim_params(2))

                integer i

c 	        Inicializando es double de tamanho npoi
                DO 102, i = 1, dim_params(1), 1
                       npoi_line1(i) = npoi_matrix(1,i)
                       npoi_line2(i) = npoi_matrix(2,i)
                       npoi_line3(i) = npoi_matrix(3,i)
                       npoi_line4(i) = npoi_matrix(4,i)
                       npoi_line5(i) = npoi_matrix(5,i)
                       npoi_line6(i) = npoi_matrix(6,i)
                       npoi_line7(i) = npoi_matrix(7,i)
                       npoi_line8(i) = npoi_matrix(8,i)
                       npoi_line9(i) = npoi_matrix(9,i)
                       npoi_line10(i) = npoi_matrix(10,i)
                       npoi_line11(i) = npoi_matrix(11,i)
                       npoi_line12(i) = npoi_matrix(12,i)
                       npoi_line13(i) = npoi_matrix(13,i)
                       npoi_line14(i) = npoi_matrix(14,i)
                       npoi_line15(i) = npoi_matrix(15,i)
                       npoi_line16(i) = npoi_matrix(16,i)
                       npoi_line17(i) = npoi_matrix(17,i)
                       npoi_line18(i) = npoi_matrix(18,i)
                       npoi_line19(i) = npoi_matrix(19,i)
                       npoi_line20(i) = npoi_matrix(20,i)
                       npoi_line21(i) = npoi_matrix(21,i)
                       npoi_line22(i) = npoi_matrix(22,i)
                       npoi_line23(i) = npoi_matrix(23,i)
                       npoi_line24(i) = npoi_matrix(24,i)
                       npoi_line25(i) = npoi_matrix(25,i)
                       npoi_line26(i) = npoi_matrix(26,i)
                       npoi_line27(i) = npoi_matrix(27,i)
                       npoi_line28(i) = npoi_matrix(28,i)
                       npoi_line29(i) = npoi_matrix(29,i)
                       npoi_line30(i) = npoi_matrix(30,i)
                       npoi_line31(i) = npoi_matrix(31,i)
                       npoi_line32(i) = npoi_matrix(32,i)
                       npoi_line33(i) = npoi_matrix(33,i)
                       npoi_line34(i) = npoi_matrix(34,i)
                       npoi_line35(i) = npoi_matrix(35,i)
                       npoi_line36(i) = npoi_matrix(36,i)
                       npoi_line37(i) = npoi_matrix(37,i)
                       npoi_line38(i) = npoi_matrix(38,i)
                       npoi_line39(i) = npoi_matrix(39,i)
                       npoi_line40(i) = npoi_matrix(40,i)
                       npoi_line41(i) = npoi_matrix(41,i)
                       npoi_line42(i) = npoi_matrix(42,i)
                       npoi_line43(i) = npoi_matrix(43,i)
                       npoi_line44(i) = npoi_matrix(44,i)
                       npoi_line45(i) = npoi_matrix(45,i)
                       npoi_line46(i) = npoi_matrix(46,i)
                       npoi_line47(i) = npoi_matrix(47,i)
                       npoi_line48(i) = npoi_matrix(48,i)
                       npoi_line49(i) = npoi_matrix(49,i)
                       npoi_line50(i) = npoi_matrix(50,i)
                       npoi_line51(i) = npoi_matrix(51,i)
                       npoi_line52(i) = npoi_matrix(52,i)
                       npoi_line53(i) = npoi_matrix(53,i)
                       npoi_line54(i) = npoi_matrix(54,i)
                       npoi_line55(i) = npoi_matrix(55,i)
                       npoi_line56(i) = npoi_matrix(56,i)
                       npoi_line57(i) = npoi_matrix(57,i)
                       npoi_line58(i) = npoi_matrix(58,i)
                       npoi_line59(i) = npoi_matrix(59,i)
                       npoi_line60(i) = npoi_matrix(60,i)
                       npoi_line61(i) = npoi_matrix(61,i)
                       npoi_line62(i) = npoi_matrix(62,i)
                       npoi_line63(i) = npoi_matrix(63,i)
                       npoi_line64(i) = npoi_matrix(64,i)
102             CONTINUE

                call stomata(iter, time, jday, 
     >                       dim_params(1), dim_params(2), dim_params(3),
     >                       dim_params(4),
     >                       double_params(1), double_params(2), double_params(3),
     >                       double_params(4), double_params(5), double_params(6),
     >                       double_params(7), double_params(8), double_params(9),
     >                       double_params(10), double_params(11), double_params(12),
     >                       double_params(13), double_params(14), double_params(15),
     >                       double_params(16), double_params(17), double_params(18),
     >                       double_params(19), double_params(20), double_params(21),
     >                       double_params(22), double_params(23), double_params(24),
     >                       double_params(25), double_params(26), double_params(27),
     >                       double_params(28), double_params(29), double_params(30),
     >                       double_params(31), double_params(32), double_params(33),
     >                       double_params(34), double_params(35), double_params(36),
     >                       double_params(37), double_params(38), double_params(39),
     >                       double_params(40), double_params(41), double_params(42),
     >                       double_params(43), double_params(44), double_params(45),
     >                       double_params(46),
     >                       npoi_line1, npoi_line2, npoi_line3, npoi_line4, npoi_line5,
     >                       npoi_line6, npoi_line7, npoi_line8, npoi_line9, npoi_line10,
     >                       npoi_line11, npoi_line12, npoi_line13, npoi_line14, npoi_line15,
     >                       npoi_line16, npoi_line17, npoi_line18, npoi_line19, npoi_line20,
     >                       npoi_line21, npoi_line22, npoi_line23, npoi_line24, npoi_line25,
     >                       npoi_line26, npoi_line27, npoi_line28, npoi_line29, npoi_line30,
     >                       npoi_line31, npoi_line32, npoi_line33, npoi_line34, npoi_line35,
     >                       npoi_line36, npoi_line37, npoi_line38, npoi_line39, npoi_line40,
     >                       npoi_line41, npoi_line42, npoi_line43, npoi_line44, npoi_line45,
     >                       npoi_line46, npoi_line47, npoi_line48, npoi_line49, npoi_line50,
     >                       npoi_line51, npoi_line52, npoi_line53, npoi_line54, npoi_line55,
     >                       npoi_line56, npoi_line57, npoi_line58, npoi_line59, npoi_line60,
     >                       npoi_line61, npoi_line62, npoi_line63, npoi_line64,
     >                       f1,
     >                       f2,
     >                       exist,
     >                       grnfraccrop,
     >                       hitemp,
     >                       lai,
     >                       lotemp,
     >                       sai,
     >                       scalcoefl,
     >                       scalcoefu,
     >                       stressn,
     >                       terml,
     >                       termu,
     >                       use,
     >                       vmax_pft,
     >                       croplive)

                DO 777, i = 1, dim_params(1), 1
                       npoi_matrix(1,i) = npoi_line1(i)
                       npoi_matrix(2,i) = npoi_line2(i)
                       npoi_matrix(3,i) = npoi_line3(i)
                       npoi_matrix(4,i) = npoi_line4(i)
                       npoi_matrix(5,i) = npoi_line5(i)
                       npoi_matrix(6,i) = npoi_line6(i)
                       npoi_matrix(7,i) = npoi_line7(i)
                       npoi_matrix(8,i) = npoi_line8(i)
                       npoi_matrix(9,i) = npoi_line9(i)
                       npoi_matrix(10,i) = npoi_line10(i)
                       npoi_matrix(11,i) = npoi_line11(i)
                       npoi_matrix(12,i) = npoi_line12(i)
                       npoi_matrix(13,i) = npoi_line13(i)
                       npoi_matrix(14,i) = npoi_line14(i)
                       npoi_matrix(15,i) = npoi_line15(i)
                       npoi_matrix(16,i) = npoi_line16(i)
                       npoi_matrix(17,i) = npoi_line17(i)
                       npoi_matrix(18,i) = npoi_line18(i)
                       npoi_matrix(19,i) = npoi_line19(i)
                       npoi_matrix(20,i) = npoi_line20(i)
                       npoi_matrix(21,i) = npoi_line21(i)
                       npoi_matrix(22,i) = npoi_line22(i)
                       npoi_matrix(23,i) = npoi_line23(i)
                       npoi_matrix(24,i) = npoi_line24(i)
                       npoi_matrix(25,i) = npoi_line25(i)
                       npoi_matrix(26,i) = npoi_line26(i)
                       npoi_matrix(27,i) = npoi_line27(i)
                       npoi_matrix(28,i) = npoi_line28(i)
                       npoi_matrix(29,i) = npoi_line29(i)
                       npoi_matrix(30,i) = npoi_line30(i)
                       npoi_matrix(31,i) = npoi_line31(i)
                       npoi_matrix(32,i) = npoi_line32(i)
                       npoi_matrix(33,i) = npoi_line33(i)
                       npoi_matrix(34,i) = npoi_line34(i)
                       npoi_matrix(35,i) = npoi_line35(i)
                       npoi_matrix(36,i) = npoi_line36(i)
                       npoi_matrix(37,i) = npoi_line37(i)
                       npoi_matrix(38,i) = npoi_line38(i)
                       npoi_matrix(39,i) = npoi_line39(i)
                       npoi_matrix(40,i) = npoi_line40(i)
                       npoi_matrix(41,i) = npoi_line41(i)
                       npoi_matrix(42,i) = npoi_line42(i)
                       npoi_matrix(43,i) = npoi_line43(i)
                       npoi_matrix(44,i) = npoi_line44(i)
                       npoi_matrix(45,i) = npoi_line45(i)
                       npoi_matrix(46,i) = npoi_line46(i)
                       npoi_matrix(47,i) = npoi_line47(i)
                       npoi_matrix(48,i) = npoi_line48(i)
                       npoi_matrix(49,i) = npoi_line49(i)
                       npoi_matrix(50,i) = npoi_line50(i)
                       npoi_matrix(51,i) = npoi_line51(i)
                       npoi_matrix(52,i) = npoi_line52(i)
                       npoi_matrix(53,i) = npoi_line53(i)
                       npoi_matrix(54,i) = npoi_line54(i)
                       npoi_matrix(55,i) = npoi_line55(i)
                       npoi_matrix(56,i) = npoi_line56(i)
                       npoi_matrix(57,i) = npoi_line57(i)
                       npoi_matrix(58,i) = npoi_line58(i)
                       npoi_matrix(59,i) = npoi_line59(i)
                       npoi_matrix(60,i) = npoi_line60(i)
                       npoi_matrix(61,i) = npoi_line61(i)
                       npoi_matrix(62,i) = npoi_line62(i)
                       npoi_matrix(63,i) = npoi_line63(i)
                       npoi_matrix(64,i) = npoi_line64(i)
777             CONTINUE

      end

c JAIR: Old    - Chamada dentro de canini (inicialmente essa rotina estava em physiology)
c JAIR: Update - Criar wrapper

c ---------------------------------------------------------------------
      subroutine stomata(iter,time,jday,
     >                   npoi,
     >                   npft,
     >                   scpft,
     >                   ecpft,
     >                   beta3,
     >                   beta4,
     >                   betac3,
     >                   betac4,
     >                   alpha3,
     >                   alpha4,
     >                   cimax,
     >                   co2conc,
     >                   coefbc3,
     >                   coefbc4,
     >                   coefbl3,
     >                   coefbl4,
     >                   coefbls,
     >                   coefbub,
     >                   coefbuc,
     >                   coefmc3,
     >                   coefmc4,
     >                   coefml3,
     >                   coefml4,
     >                   coefmls,
     >                   coefmub,
     >                   coefmuc,
     >                   epsilon,
     >                   dtime,
     >                   gammac3,
     >                   gammac4,
     >                   gammal3,
     >                   gammal4,
     >                   gammals,
     >                   gammaub,
     >                   gammauc,
     >                   gsc3min,
     >                   gsc4min,
     >                   gsl3min,
     >                   gsl4min,
     >                   gslsmin,
     >                   gsubmin,
     >                   gsucmin,
     >                   kc15,
     >                   ko15,
     >                   o2conc,
     >                   tau15,
     >                   theta3,
     >                   theta4,
     >                   thetac3,
     >                   thetac4,
     >                   cic3,
     >                   cic4,
     >                   cil3,
     >                   cil4,
     >                   cils,
     >                   ciub,
     >                   ciuc,
     >                   fl,
     >                   csc3,
     >                   csc4,
     >                   csl3,
     >                   csl4,
     >                   csls,
     >                   csub,
     >                   csuc,
     >                   fwetl,
     >                   fwetu,
     >                   a10daylightl,
     >                   a10daylightu,
     >                   a10scalparaml,
     >                   a10scalparamu,
     >                   agcc3,
     >                   agcc4,
     >                   agcl3,
     >                   agcl4,
     >                   agcls,
     >                   agcub,
     >                   agcuc,
     >                   ancc3,
     >                   ancc4,
     >                   ancl3,
     >                   ancl4,
     >                   ancls,
     >                   ancub,
     >                   ancuc,
     >                   psurf,
     >                   q12,
     >                   q34,
     >                   sl,
     >                   tu,
     >                   stresstl,
     >                   stresstu,
     >                   su,
     >                   t12,
     >                   t34,
     >                   tl,
     >                   topparl,
     >                   topparu,
     >                   totcondc3,
     >                   totcondc4,
     >                   totcondl3,
     >                   totcondl4,
     >                   totcondls,
     >                   totcondub,
     >                   totconduc,
     >                   greenfracl3,
     >                   greenfracl4,
     >                   gsc3,
     >                   gsc4,
     >                   gsl3,
     >                   gsl4,
     >                   gsls,
     >                   gsub,
     >                   gsuc,
     >                   f1,
     >                   f2,
     >                   exist,
     >                   grnfraccrop,
     >                   hitemp,
     >                   lai,
     >                   lotemp,
     >                   sai,
     >                   scalcoefl,
     >                   scalcoefu,
     >                   stressn,
     >                   terml,
     >                   termu,
     >                   use,
     >                   vmax_pft,
     >                   croplive)
c ---------------------------------------------------------------------
c
c common blocks
c
       implicit none
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsno.h'
c      include 'comsoi.h'
c      include 'comveg.h'
c      include 'com1d.h'
c      include 'comsum.h'
c      include 'comcrop.h'
c      include 'comnitr.h'
c      include 'compft.h'
c
c global variables

        ! 4
        integer npoi
        integer npft
        integer scpft
        integer ecpft

        ! 46
        double precision beta3
        double precision beta4
        double precision betac3
        double precision betac4
        double precision alpha3
        double precision alpha4
        double precision cimax
        double precision co2conc
        double precision coefbc3
        double precision coefbc4
        double precision coefbl3
        double precision coefbl4
        double precision coefbls
        double precision coefbub
        double precision coefbuc
        double precision coefmc3
        double precision coefmc4
        double precision coefml3
        double precision coefml4
        double precision coefmls
        double precision coefmub
        double precision coefmuc
        double precision epsilon
        double precision dtime
        double precision gammac3
        double precision gammac4
        double precision gammal3
        double precision gammal4
        double precision gammals
        double precision gammaub
        double precision gammauc
        double precision gsc3min
        double precision gsc4min
        double precision gsl3min
        double precision gsl4min
        double precision gslsmin
        double precision gsubmin
        double precision gsucmin
        double precision kc15
        double precision ko15
        double precision o2conc
        double precision tau15
        double precision theta3
        double precision theta4
        double precision thetac3
        double precision thetac4

        ! 64
        double precision cic3(npoi)
        double precision cic4(npoi)
        double precision cil3(npoi)
        double precision cil4(npoi)
        double precision cils(npoi)
        double precision ciub(npoi)
        double precision ciuc(npoi)
        double precision fl(npoi)
        double precision csc3(npoi)
        double precision csc4(npoi)
        double precision csl3(npoi)
        double precision csl4(npoi)
        double precision csls(npoi)
        double precision csub(npoi)
        double precision csuc(npoi)
        double precision fwetl(npoi)
        double precision fwetu(npoi)
        double precision a10daylightl(npoi)
        double precision a10daylightu(npoi)
        double precision a10scalparaml(npoi)
        double precision a10scalparamu(npoi)
        double precision agcc3(npoi)
        double precision agcc4(npoi)
        double precision agcl3(npoi)
        double precision agcl4(npoi)
        double precision agcls(npoi)
        double precision agcub(npoi)
        double precision agcuc(npoi)
        double precision ancc3(npoi)
        double precision ancc4(npoi)
        double precision ancl3(npoi)
        double precision ancl4(npoi)
        double precision ancls(npoi)
        double precision ancub(npoi)
        double precision ancuc(npoi)
        double precision psurf(npoi)
        double precision q12(npoi)
        double precision q34(npoi)
        double precision sl(npoi)
        double precision tu(npoi)
        double precision stresstl(npoi)
        double precision stresstu(npoi)
        double precision su(npoi)
        double precision t12(npoi)
        double precision t34(npoi)
        double precision tl(npoi)
        double precision topparl(npoi)
        double precision topparu(npoi)
        double precision totcondc3(npoi)
        double precision totcondc4(npoi)
        double precision totcondl3(npoi)
        double precision totcondl4(npoi)
        double precision totcondls(npoi)
        double precision totcondub(npoi)
        double precision totconduc(npoi)
        double precision greenfracl3(npoi)
        double precision greenfracl4(npoi)
        double precision gsc3(npoi)  
        double precision gsc4(npoi)
        double precision gsl3(npoi)
        double precision gsl4(npoi)
        double precision gsls(npoi)
        double precision gsub(npoi)
        double precision gsuc(npoi)

        double precision f1(npft)
        double precision f2(npft)
        double precision exist(npoi,npft)
        double precision grnfraccrop(npoi,npft)
        double precision hitemp(npft)
        double precision lai(npoi,2)
        double precision lotemp(npft)
        double precision sai(npoi,2)
        double precision scalcoefl(npoi,4)
        double precision scalcoefu(npoi,4)
        double precision stressn(npoi,npft)
        double precision terml(npoi,7)
        double precision termu(npoi,7)
        double precision use(4,24)
        double precision vmax_pft(npft)
        double precision croplive(npoi,npft)

c local variables
c
      integer i, j, idc,iter,jday,
     >        iday,
     >        imonth,
     >        iyear
c
      double precision rwork,  ! 3.47e-03 - 1. / tu(i)
     >     tau, time,    ! 
     >     tleaf,  ! leaf temp in celcius
     >     tempvm, !
     >     zweight !

      double precision esat12, ! vapor pressure in upper canopy air 
     >     qsat12, ! specific humidity in upper canopy air
     >     rh12,   ! relative humidity in upper canopy air 
     >     esat34, ! vapor pressure in lower canopy air
     >     qsat34, ! specific humidity in lower canopy air 
     >     rh34,   ! relative humidity in lower canopy air 
     >     gbco2u, ! bound. lay. conductance for CO2 in upper canopy
     >     gbco2l, ! bound. lay. conductance for CO2 in lower canopy
     >     gscub,  ! 
     >     gscuc,  !
     >     gscls,  !
     >     gscl3,  !
     >     gscl4,  !
     >     gscc3,  !
     >     gscc4   !  
      double precision vmax, vmaxub, vmaxuc, vmaxls, vmaxl3, vmaxl4 
      double precision rdarkub, rdarkuc, rdarkls, rdarkl3, rdarkl4, rdarkc3, rdarkc4
      double precision agub, aguc, agls, agl3, agl4, agc3, agc4
      double precision anub, anuc, anls, anl3, anl4, anc3, anc4
      double precision duma, dumb, dumc, dume, dumq, dump
      double precision pxaiu, plaiu, pxail, plail
      double precision cscub, cscuc, cscls, cscl3, cscl4, cscc3, cscc4
      double precision extpar, scale
      double precision stressc3c, stressc4c
c
      double precision kc,     ! co2 kinetic parameter (mol/mol)
     >     ko,     ! o2  kinetic parameter (mol/mol)
*    >     ko15,   ! o2  kinetic parameter (mol/mol) at 15 degrees C
     >     kco2,   ! initial c4 co2 efficiency (mol-co2/m**2/s)
     >     je,     ! 'light limited' rate of photosynthesis (mol-co2/m**2/s)
     >     jc,     ! 'rubisco limited' rate of photosynthesis (mol-co2/m**2/s)
     >     js,     ! 'sucrose limited' rate of photosynthesis (mol-co2/m**2/s)
     >     ji,     ! 'co2 limited' rate of photosynthesis (mol-co2/m**2/s)
     >     jp,     ! model-intermediate rate of photosynthesis (mol-co2/m**2/s)
     >     gamstar,! gamma*, the co2 compensation points for c3 plants
     >     q10,
     >     tkco2
c
c model parameters
c
c intrinsic quantum efficiency for c3 and c4 plants (dimensionless)
c
*      real alpha3, alpha4
c
*      data alpha3  /0.080/
*      data alpha4  /0.050/
c
c co2/o2 specificity ratio at 15 degrees C (dimensionless)
c
*      real tau15
c
*      data tau15 /4500.0/     
c
c o2/co2 kinetic parameters (mol/mol)
c
*      real kc15
c
c     data kc15 /1.5e-04/ 
c     data ko15 /2.5e-01/ 
c
c leaf respiration coefficients
c
*      real gammaub, gammauc, gammals, gammal3, gammal4,
*     >     gammac3, gammac4
c
*      data gammaub /0.0150/   ! broadleaf trees
*      data gammauc /0.0150/   ! conifer trees
*      data gammals /0.0150/   ! shrubs
*      data gammal3 /0.0150/   ! c3 grasses
*      data gammal4 /0.0300/   ! c4 grasses
*      data gammac3 /0.0150/   ! c3 crops
*      data gammac4 /0.0100/   ! c4 crops - corn  Amthor, 1984
c
c 'm' coefficients for stomatal conductance relationship
c
*      real coefmub, coefmuc, coefmls, coefml3, coefml4,
*     >     coefmc3, coefmc4
c
*     data coefmub /10.0/     ! broadleaf trees
*     data coefmuc / 6.0/     ! conifer trees
*     data coefmls / 9.0/     ! shrubs
*     data coefml3 / 9.0/     ! c3 grasses
*     data coefml4 / 4.0/     ! c4 grasses
*     data coefmc3 / 9.0/     ! c3 crops
*     data coefmc4 / 4.0/     ! c4 crops - corn (Collatz et al. 1992)
c
c 'b' coefficients for stomatal conductance relationship 
c (minimum conductance when net photosynthesis is zero)
c
*      real coefbub, coefbuc, coefbls, coefbl3, coefbl4,
*     >     coefbc3, coefbc4
c
*      data coefbub /0.010/    ! broadleaf trees
*      data coefbuc /0.010/    ! conifer trees
*      data coefbls /0.010/    ! shrubs
*      data coefbl3 /0.010/    ! c3 grasses
*      data coefbl4 /0.040/    ! c4 grasses
*      data coefbc3 /0.010/    ! c3 crops - soybean
c      data coefbc4 /0.080/    ! c4 crops - corn (Collatz et al. 1992)
*      data coefbc4 /0.030/    ! c4 crops - corn - Cupid model
c
c absolute minimum stomatal conductances
c
*      real gsubmin, gsucmin, gslsmin, gsl3min, gsl4min,
*     >     gsc3min, gsc4min
c
*      data gsubmin /0.00001/  ! broadleaf trees
*      data gsucmin /0.00001/  ! conifer trees
*      data gslsmin /0.00001/  ! shrubs
*      data gsl3min /0.00001/  ! c3 grasses
*      data gsl4min /0.00001/  ! c4 grasses
*      data gsc3min /0.00001/  ! c3 crops
*      data gsc4min /0.00001/  ! c4 crops
c
c photosynthesis coupling coefficients (dimensionless)
c
*      real theta3, theta4, beta4, thetac4, betac4,
*     >     thetac3, betac3
c
*      data theta3 /0.950/     ! c3 photosynthesis
*      data beta3  /0.990/     ! c3 photosynthesis
*      data theta4 /0.970/     ! c4 photosynthesis (not crops)
*      data beta4  /0.800/     ! c4 photosynthesis (not crops)
c
c photosynthesis coupling coefficients for c4 crops 
c from Collatz et al. 1992
c
c
*      data thetac4 /0.970/    ! c4 crop photosynthesis - maize 
*      data betac4  /0.800/    ! c4 crop photosynthesis - maize 
c
c photosynthesis coupling coefficients for c3 crops
c from Cupid model (Norman, 1983)
c
*      data thetac3 /0.950/    ! c3 crop photosynthesis - soybean 
*      data betac3  /0.990/    ! c3 crop photosynthesis - soybean
c
c
c maximum values for ci (for model stability)
c
*     real cimax
c
*     data cimax /2000.e-06/  ! maximum values for ci
c
c crop parameters
c
*      real
*     >     lotemp(npft),      ! low temperature threshold in tempvm equation
*     >     hitemp(npft),      ! high temperature threshold in tempvm equation 
*     >     drought(npft),     ! crop sensitivity to drought parameter 
*     >     f1(npft),          ! constant used in tempvm equations 
*     >     f2(npft)           ! constant used in tempvm equations
c   
c include water vapor functions
c
c     include 'comsat.h'
      double precision asat0, asat1, asat2, asat3, asat4, asat5, asat6
c
      parameter (asat0 =  6.1078000,
     >           asat1 =  4.4365185e-1,
     >           asat2 =  1.4289458e-2,
     >           asat3 =  2.6506485e-4,
     >           asat4 =  3.0312404e-6,
     >           asat5 =  2.0340809e-8,
     >           asat6 =  6.1368209e-11 )
c
      double precision bsat0, bsat1, bsat2, bsat3, bsat4, bsat5, bsat6
c
      parameter (bsat0 =  6.1091780,
     >           bsat1 =  5.0346990e-1,
     >           bsat2 =  1.8860134e-2,
     >           bsat3 =  4.1762237e-4,
     >           bsat4 =  5.8247203e-6,
     >           bsat5 =  4.8388032e-8,
     >           bsat6 =  1.8388269e-10 )
c
      double precision csat0, csat1, csat2, csat3, csat4, csat5, csat6
c
      parameter (csat0 =  4.4381000e-1,
     >           csat1 =  2.8570026e-2,
     >           csat2 =  7.9380540e-4,
     >           csat3 =  1.2152151e-5,
     >           csat4 =  1.0365614e-7,
     >           csat5 =  3.5324218e-10,
     >           csat6 = -7.0902448e-13 )
c
      double precision dsat0, dsat1, dsat2, dsat3, dsat4, dsat5, dsat6
c
      parameter (dsat0 =  5.0303052e-1,
     >           dsat1 =  3.7732550e-2,
     >           dsat2 =  1.2679954e-3,
     >           dsat3 =  2.4775631e-5,
     >           dsat4 =  3.0056931e-7,
     >           dsat5 =  2.1585425e-9,
     >           dsat6 =  7.1310977e-12 )

      double precision t,        ! temperature argument of statement function 
     >     tair,     ! temperature argument of statement function 
     >     p1,       ! pressure argument of function 
     >     e1,       ! vapor pressure argument of function
     >     q1,       ! saturation specific humidity argument of function
     >     tsatl,    ! statement function
     >     tsati,    ! 
     >     esat,     !
     >     desat,    !
     >     qsat,     ! 
     >     dqsat,    ! 
     >     hvapf,    ! 
     >     hsubf,    !
     >     cvmgt     ! function

      tsatl(t) = min (100., max (t-273.16, 0.))
      tsati(t) = max (-60., min (t-273.16, 0.))

      esat (t) = 
     >  100.*(
     >    cvmgt (asat0, bsat0, t.ge.273.16)
     >    + tsatl(t)*(asat1 + tsatl(t)*(asat2 + tsatl(t)*(asat3
     >    + tsatl(t)*(asat4 + tsatl(t)*(asat5 + tsatl(t)* asat6)))))
     >    + tsati(t)*(bsat1 + tsati(t)*(bsat2 + tsati(t)*(bsat3
     >    + tsati(t)*(bsat4 + tsati(t)*(bsat5 + tsati(t)* bsat6)))))
     >  )

      desat (t) =
     >  100.*(
     >    cvmgt (csat0, dsat0, t.ge.273.16)
     >    + tsatl(t)*(csat1 + tsatl(t)*(csat2 + tsatl(t)*(csat3
     >    + tsatl(t)*(csat4 + tsatl(t)*(csat5 + tsatl(t)* csat6)))))
     >    + tsati(t)*(dsat1 + tsati(t)*(dsat2 + tsati(t)*(dsat3
     >    + tsati(t)*(dsat4 + tsati(t)*(dsat5 + tsati(t)* dsat6)))))
     >  )

       qsat (e1, p1) = 0.622 * e1 /
     >               max ( p1 - (1.0 - 0.622) * e1, 0.622 * e1 )

       dqsat (t, q1) = desat(t) * q1 * (1. + q1*(1./0.622 - 1.)) /
     >                 esat(t)         

c
c ---------------------------------------------------------------------
c * * * upper canopy physiology calculations * * *
c ---------------------------------------------------------------------
c
      do 100 i = 1, npoi
c
c only perform calculations if crops are not planted
c
c       if (icropsum(i) .eq. 0.) then
c calculate physiological parameter values which are a function of temperature
c
        rwork = 3.47e-03 - 1. / tu(i)
c
        tau = tau15 * exp(-4500.0 * rwork)
        kc  = kc15  * exp( 6000.0 * rwork)
        ko  = ko15  * exp( 1500.0 * rwork)
c
        tleaf = tu(i) - 273.16
c
        tempvm = exp(3500.0 * rwork ) /
     >           ((1.0 + exp(0.40 * (  5.0 - tleaf))) * 
     >            (1.0 + exp(0.40 * (tleaf - 50.0))))
c
c upper canopy gamma-star values (mol/mol)
c
        gamstar = o2conc / (2. * tau)
c
c constrain ci values to acceptable bounds -- to help ensure numerical stability
c
        ciub(i) = max (1.05 * gamstar, min (cimax, ciub(i)))
        ciuc(i) = max (1.05 * gamstar, min (cimax, ciuc(i)))
c
c calculate boundary layer parameters (mol/m**2/s) = su / 0.029 * 1.35
c
        gbco2u = min (10.0, max (0.1, su(i) * 25.5))
c 
c calculate the relative humidity in the canopy air space
c with a minimum value of 0.30 to avoid errors in the 
c physiological calculations
c
        esat12 = esat (t12(i))
        qsat12 = qsat (esat12, psurf(i))
        rh12   = max (0.30, q12(i) / qsat12)
c
c ---------------------------------------------------------------------
c broadleaf (evergreen & deciduous) tree physiology 
c ---------------------------------------------------------------------
c 
c nominal values for vmax of top leaf at 15 C (mol-co2/m**2/s)
c
c tropical broadleaf trees          60.0 e-06 mol/m**2/sec
c warm-temperate broadleaf trees    40.0 e-06 mol/m**2/sec
c temperate broadleaf trees         25.0 e-06 mol/m**2/sec
c boreal broadleaf trees            25.0 e-06 mol/m**2/sec
c
        if (exist(i,1).gt.0.5) then
          vmaxub = vmax_pft(1)     
        else if (exist(i,3).gt.0.5) then
          vmaxub = vmax_pft(3)     
        else 
          vmaxub = vmax_pft(5) 
        endif
c
c vmax and dark respiration for current conditions
c
        vmax  = vmaxub * tempvm * stresstu(i)
        rdarkub = gammaub * vmaxub * tempvm
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparu(i) * 4.59e-06 * alpha3 * (ciub(i) - gamstar) / 
     >       (ciub(i) + 2. * gamstar)
c
c 'rubisco limited' rate of photosynthesis (mol/m**2/s)
c
        jc = vmax * (ciub(i) - gamstar) / 
     >       (ciub(i) + kc * (1. + o2conc / ko))
c
c solution to quadratic equation
c
        duma = theta3
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c       calculate the intermediate photosynthesis rate (mol/m**2/s)
c
        jp = min (dumq/duma, dumc/dumq)
c
c 'sucrose synthesis limited' rate of photosynthesis (mol/m**2/s)
c
        js = vmax / 2.2
c
c solution to quadratic equation
c
        duma = beta3
        dumb = jp + js
        dumc = jp * js
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agub = min (dumq/duma, dumc/dumq)
        anub = agub - rdarkub
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
        csub(i) = 0.5 * (csub(i) + co2conc - anub / gbco2u)
        csub(i) = max (1.05 * gamstar, csub(i))
c
c calculate new value of gs using implicit scheme
c
        gsub(i) = 0.5 * (gsub(i)  +  (coefmub * anub * rh12 / csub(i) + 
     >                                coefbub * stresstu(i)))
c
        gsub(i) = max (gsubmin, coefbub * stresstu(i), gsub(i))
c
c calculate new value of ci using implicit scheme
c
        ciub(i) = 0.5 * (ciub(i) + csub(i) - 1.6 * anub / gsub(i))
        ciub(i) = max (1.05 * gamstar, min (cimax, ciub(i)))
c
c ---------------------------------------------------------------------
c conifer tree physiology 
c ---------------------------------------------------------------------
c 
c nominal values for vmax of top leaf at 15 C (mol-co2/m**2/s)
c
c temperate conifer trees           30.0 e-06 mol/m**2/sec
c boreal conifer trees              20.0 e-06 mol/m**2/sec
c
        if (exist(i,4).gt.0.5) then
          vmaxuc = vmax_pft(4)
        else 
          vmaxuc = vmax_pft(6)
        endif
c
c vmax and dark respiration for current conditions
c
        vmax  = vmaxuc * tempvm * stresstu(i)
        rdarkuc = gammauc * vmaxuc * tempvm
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparu(i) * 4.59e-06 * alpha3 * (ciuc(i) - gamstar) / 
     >       (ciuc(i) + 2. * gamstar)
c
c 'rubisco limited' rate of photosynthesis (mol/m**2/s)
c
        jc = vmax * (ciuc(i) - gamstar) / 
     >       (ciuc(i) + kc * (1. + o2conc / ko))
c
c solution to quadratic equation
c
        duma = theta3
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the intermediate photosynthesis rate (mol/m**2/s)
c
        jp = min (dumq/duma, dumc/dumq)
c       
c 'sucrose synthesis limited' rate of photosynthesis (mol/m**2/s)
c       
        js = vmax / 2.2
c 
c solution to quadratic equation
c
        duma = beta3
        dumb = jp + js
        dumc = jp * js
c       
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        aguc = min (dumq/duma, dumc/dumq) 
        anuc = aguc - rdarkuc
c
c       if (i .eq. 1 .and. anuc .gt. 0.0) write(*,*) anuc*1e+06 
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
        csuc(i) = 0.5 * (csuc(i) + co2conc - anuc / gbco2u)
        csuc(i) = max (1.05 * gamstar, csuc(i))
c
c calculate new value of gs using implicit scheme
c
        gsuc(i) = 0.5 * (gsuc(i)  +  (coefmuc * anuc * rh12 / csuc(i) + 
     >                                coefbuc * stresstu(i)))
c
        gsuc(i) = max (gsucmin, coefbuc * stresstu(i), gsuc(i))
c
c calculate new value of ci using implicit scheme
c
        ciuc(i) = 0.5 * (ciuc(i) + csuc(i) - 1.6 * anuc / gsuc(i))
        ciuc(i) = max (1.05 * gamstar, min (cimax, ciuc(i)))
c
c ---------------------------------------------------------------------
c upper canopy scaling
c ---------------------------------------------------------------------
c
c the canopy scaling algorithm assumes that the net photosynthesis
c is proportional to absored par (apar) during the daytime. during night,
c the respiration is scaled using a 10-day running-average daytime canopy
c scaling parameter.
c
c apar(x) = A exp(-k x) + B exp(-h x) + C exp(h x)
c an(x) is proportional to apar(x)
c
c therefore, an(x) = an(0) * apar(x) / apar(0)
c an(x) = an(0) * (A exp(-k x) + B exp(-h x) + C exp(h x)) / 
c                 (A + B + C)
c
c this equation is further simplified to
c an(x) = an(0) * exp (-extpar * x)
c
c an(0) is calculated for a sunlit leaf at the top of the canopy using
c the full-blown plant physiology model (Farquhar/Ball&Berry, Collatz).
c then the approximate par extinction coefficient (extpar) is calculated
c using parameters obtained from the two-stream radiation calculation.
c
c an,canopy avg.= integral (an(x), from 0 to xai) / lai
c               = an(0) * (1 - exp (-extpar * xai )) / (extpar * lai)
c
c the term '(1 - exp (-extpar * xai )) / lai)' scales photosynthesis from leaf
c to canopy level (canopy average) at day time. A 10-day running mean of this
c scaling parameter (weighted by light) is then used to scale the respiration
c during night time.
c
c once canopy average photosynthesis is calculated, then the canopy average
c stomatal conductance is calculated using the 'big leaf approach',i.e. 
c assuming that the canopy is a big leaf and applying the leaf-level stomatal
c conductance equations to the whole canopy.
c
c calculate the approximate par extinction coefficient:
c
c extpar = (k * A + h * B - h * C) / (A + B + C)
c
        extpar = (termu(i,6) * scalcoefu(i,1) +
     >            termu(i,7) * scalcoefu(i,2) -
     >            termu(i,7) * scalcoefu(i,3)) /
     >            max (scalcoefu(i,4), epsilon)
c
        extpar = max (1.e-1, min (1.e+1, extpar))
c
c calculate canopy average photosynthesis (per unit leaf area):
c
        pxaiu = extpar * (lai(i,2) + sai(i,2))
        plaiu = extpar *  lai(i,2)
c
c scale is the parameter that scales from leaf-level photosynthesis to
c canopy average photosynthesis
c CD : replaced 24 (hours) by 86400/dtime for use with other timestep
c
         zweight = exp(-1. / (10.0 * 86400. / dtime))
c
c for non-zero lai
c
        if (plaiu.gt.0.0) then
c
c day-time conditions, use current scaling coefficient
c
          if (topparu(i).gt.10.) then
c
            scale = (1. - exp(-pxaiu)) / plaiu
c
c update 10-day running mean of scale, weighted by light levels
c
            a10scalparamu(i) = zweight * a10scalparamu(i) + 
     >                         (1. - zweight) * scale * topparu(i)
c
            a10daylightu(i)  = zweight * a10daylightu(i) + 
     >                         (1. - zweight) * topparu(i)
c
c night-time conditions, use long-term day-time average scaling coefficient
c
          else
c
            scale = a10scalparamu(i) / a10daylightu(i)
c
          endif
c
c if no lai present
c
        else
c
          scale = 0.0
c
        endif
c
c perform scaling on all carbon fluxes from upper canopy
c
        agcub(i) = agub * scale
        agcuc(i) = aguc * scale
c
        ancub(i) = anub * scale
        ancuc(i) = anuc * scale
c
c calculate diagnostic canopy average surface co2 concentration 
c (big leaf approach)
c
        cscub = max (1.05 * gamstar, co2conc - ancub(i) / gbco2u)
        cscuc = max (1.05 * gamstar, co2conc - ancuc(i) / gbco2u)
c
c calculate diagnostic canopy average stomatal conductance (big leaf approach)
c
        gscub = coefmub * ancub(i) * rh12 / cscub +
     >          coefbub * stresstu(i)
c
        gscuc = coefmuc * ancuc(i) * rh12 / cscuc +
     >          coefbuc * stresstu(i)
c
        gscub = max (gsubmin, coefbub * stresstu(i), gscub)
        gscuc = max (gsucmin, coefbuc * stresstu(i), gscuc)
c
c calculate total canopy and boundary-layer total conductance for 
c water vapor diffusion
c
        rwork = 1. / su(i)
        dump  = 1. / 0.029
c
        totcondub(i) = 1. / (rwork + dump / gscub)
        totconduc(i) = 1. / (rwork + dump / gscuc)
c
c multiply canopy photosynthesis by wet fraction - this calculation is
c done here and not earlier to avoid using within canopy conductance
c
        rwork = 1 - fwetu(i)
c
        agcub(i) = rwork * agcub(i)
        agcuc(i) = rwork * agcuc(i)
c
        ancub(i) = rwork * ancub(i)
        ancuc(i) = rwork * ancuc(i)
c
c      endif  ! crop existence
c
 100  continue
c
c ---------------------------------------------------------------------
c * * * lower canopy physiology calculations * * *
c ---------------------------------------------------------------------
c
      do 200 i = 1, npoi
c
c calculate physiological parameter values which are a function of temperature
c
        rwork = 3.47e-03 - 1. / tl(i)
c
        tau = tau15 * exp(-5000.0 * rwork)
        kc  = kc15  * exp( 6000.0 * rwork)
        ko  = ko15  * exp( 1400.0 * rwork)
c
        tleaf = tl(i) - 273.16
c
        tempvm = exp(3500.0 * rwork ) /
     >           ((1.0 + exp(0.40 * (  5.0 - tleaf))) * 
     >            (1.0 + exp(0.40 * (tleaf - 50.0))))
c
c lower canopy gamma-star values (mol/mol)
c
        gamstar = o2conc / (2. * tau)
c
c constrain ci values to acceptable bounds -- to help ensure numerical stability
c
        cils(i) = max (1.05 * gamstar, min (cimax, cils(i)))
        cil3(i) = max (1.05 * gamstar, min (cimax, cil3(i)))
        cil4(i) = max (0.0           , min (cimax, cil4(i)))
c
c constrain ci value to acceptable bounds for crops
c
        cic3(i) = max (1.05 * gamstar, min (cimax, cic3(i)))
        cic4(i) = max (0.0           , min (cimax, cic4(i)))
c
c calculate boundary layer parameters (mol/m**2/s) = sl / 0.029 * 1.35

csant - original -    
	gbco2l = min (10.0, max (0.1, sl(i) * 25.5))

csant- I think should be-> m.s-1*kg/m3 = kg/m2.s to (mol/m**2/s) = sl * (1./ 0.029) -> (1/M) = (mol/kg)
c        gbco2l = min (10.0, max (0.1, sl(i) * (1./0.029)))
c 
c calculate the relative humidity in the canopy air space
c with a minimum value of 0.30 to avoid errors in the 
c physiological calculations
c
        esat34 = esat (t34(i))
        qsat34 = qsat (esat34, psurf(i))
        rh34   = max (0.30, q34(i) / qsat34)
c
c only perform calculations below if crops are not planted
c
c      if (icropsum(i) .eq. 0.) then
c
c ---------------------------------------------------------------------
c shrub physiology
c ---------------------------------------------------------------------
c 
c nominal values for vmax of top leaf at 15 C (mol-co2/m**2/s)
c
        vmaxls = vmax_pft(9) 
c 
c vmax and dark respiration for current conditions
c
        vmax  = vmaxls * tempvm * stresstl(i)
        rdarkls = gammals * vmaxls * tempvm
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparl(i) * 4.59e-06 * alpha3 * (cils(i) - gamstar) / 
     >       (cils(i) + 2. * gamstar)
c
c 'rubisco limited' rate of photosynthesis (mol/m**2/s)
c
        jc = vmax * (cils(i) - gamstar) / 
     >       (cils(i) + kc * (1. + o2conc / ko))
c
c solution to quadratic equation
c
        duma = theta3
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the intermediate photosynthesis rate (mol/m**2/s)
c
        jp = min (dumq/duma, dumc/dumq)
c       
c 'sucrose synthesis limited' rate of photosynthesis (mol/m**2/s)
c       
        js = vmax / 2.2
c 
c solution to quadratic equation
c
        duma = beta3
        dumb = jp + js
        dumc = jp * js
c       
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agls = min (dumq/duma, dumc/dumq)
        anls = agls - rdarkls
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
        csls(i) = 0.5 * (csls(i) + co2conc - anls / gbco2l)
        csls(i) = max (1.05 * gamstar, csls(i))
c
c calculate new value of gs using implicit scheme
c
        gsls(i) = 0.5 * (gsls(i) + coefmls * anls * rh34 / csls(i) +
     >                             coefbls * stresstl(i))
c
        gsls(i) = max (gslsmin, coefbls * stresstl(i), gsls(i))
c
c calculate new value of ci using implicit scheme
c
        cils(i) = 0.5 * (cils(i) + csls(i) - 1.6 * anls / gsls(i))
        cils(i) = max (1.05 * gamstar, min (cimax, cils(i)))
c
c ---------------------------------------------------------------------
c c3 grass physiology
c ---------------------------------------------------------------------
c 
c nominal values for vmax of top leaf at 15 C (mol-co2/m**2/s)
c
        vmaxl3 = vmax_pft(12)
c 
c vmax and dark respiration for current conditions
c
        vmax  = vmaxl3 * tempvm * stresstl(i)
        rdarkl3 = gammal3 * vmaxl3 * tempvm
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparl(i) * 4.59e-06 * alpha3 * (cil3(i) - gamstar) / 
     >       (cil3(i) + 2. * gamstar)
c
c 'rubisco limited' rate of photosynthesis (mol/m**2/s)
c
        jc = vmax * (cil3(i) - gamstar) / 
     >       (cil3(i) + kc * (1. + o2conc / ko))
c
c solution to quadratic equation
c
        duma = theta3
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the intermediate photosynthesis rate (mol/m**2/s)
c
        jp = min (dumq/duma, dumc/dumq)
c       
c 'sucrose synthesis limited' rate of photosynthesis (mol/m**2/s)
c       
        js = vmax / 2.2
c 
c solution to quadratic equation
c
        duma = beta3
        dumb = jp + js
        dumc = jp * js
c       
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agl3 = min (dumq/duma, dumc/dumq)
        anl3 = agl3 - rdarkl3
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
        csl3(i) = 0.5 * (csl3(i) + co2conc - anl3 / gbco2l)
        csl3(i) = max (1.05 * gamstar, csl3(i))
c
c calculate new value of gs using implicit scheme
c
        gsl3(i) = 0.5 * (gsl3(i) + coefml3 * anl3 * rh34 / csl3(i) +
     >                   coefbl3 * stresstl(i))
c
        gsl3(i) = max (gsl3min, coefbl3 * stresstl(i), gsl3(i))
c
c calculate new value of ci using implicit scheme
c
        cil3(i) = 0.5 * (cil3(i) + csl3(i) - 1.6 * anl3 / gsl3(i))
        cil3(i) = max (1.05 * gamstar, min (cimax, cil3(i)))
c
c ---------------------------------------------------------------------
c c4 grass physiology
c ---------------------------------------------------------------------
c
c nominal values for vmax of top leaf at 15 C (mol-co2/m**2/s)
c
        vmaxl4 = vmax_pft(11)
c
c calculate the parameter values which are a function of temperature
c
        rwork = 3.47e-03 - 1. / tl(i)
c
        tleaf = tl(i) - 273.16
c
        tempvm = exp(3500.0 * rwork ) /
     >           ((1.0 + exp(0.40 * ( 10.0 - tleaf))) * 
     >            (1.0 + exp(0.40 * (tleaf - 50.0))))
c
c vmax and dark respiration for current conditions
c
        vmax  = vmaxl4 * tempvm * stresstl(i)
        rdarkl4 = gammal4 * vmaxl4 * tempvm
c
c initial c4 co2 efficiency (mol/m**2/s)
c
        kco2 = 18.0e+03 * vmax
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparl(i) * 4.59e-06 * alpha4
c
c 'rubisco limited' rate of photosynthesis
c
        jc = vmax
c
c solve for intermediate photosynthesis rate
c
        duma = theta4
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
        jp = min (dumq/duma, dumc/dumq)
c
c 'carbon dioxide limited' rate of photosynthesis (mol/m**2/s)
c
        ji = kco2 * cil4(i)
c
c solution to quadratic equation
c
        duma = beta4
        dumb = jp + ji
        dumc = jp * ji
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agl4 = min (dumq/duma, dumc/dumq)
        anl4 = agl4 - rdarkl4
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c CD: For numerical stability (to avoid division by zero in gsl4), 
c csl4 is limited to 1e-8 mol_co2/mol_air.
c  
        csl4(i) = 0.5 * (csl4(i) + co2conc - anl4 / gbco2l)
        csl4(i) = max (1.e-8, csl4(i))
c
c calculate new value of gs using implicit scheme
c
        gsl4(i) = 0.5 * (gsl4(i) + coefml4 * anl4 * rh34 / csl4(i) +
     >                   coefbl4 * stresstl(i))
c
        gsl4(i) = max (gsl4min, coefbl4 * stresstl(i), gsl4(i))
c
c calculate new value of ci using implicit scheme
c
        cil4(i) = 0.5 * (cil4(i) + csl4(i) - 1.6 * anl4 / gsl4(i))
        cil4(i) = max (0.0, min (cimax, cil4(i)))
c
c      endif ! crop existence
c
c --------------------------------------------------------------------
c identify crops that are planted 
c ---------------------------------------------------------------------
c
c temperature response curve constants which vary
c
*       f1(13) = 0.40
*       f1(14) = 0.40
*       f1(15) = 0.40
*       f2(13) = 0.40
*       f2(14) = 0.40
*       f2(15) = 0.40
c
c hi and low temperature thresholds (C)
c  
*       lotemp(13) = 5.0
*       lotemp(14) = 6.0
*       lotemp(15) = 0.0 
c
*       hitemp(13) = 40.0
*       hitemp(14) = 50.0
*       hitemp(15) = 38.0
c
c vmax values 15 C base
c
*       vmax_pft(13) = 65.0e-06  ! soybean vmax  Wullschleger, 1993
*       vmax_pft(14) = 70.0e-06  ! maize vmax
*       vmax_pft(15) = 60.0e-06  ! wheat vmax    Wullschleger, 1993 
c
c drought sensitivity - to account for differences between
c soybeans and other cereal crops (e.g., maize, wheat)
c
*       drought(13)  = 1.25
*       drought(14)  = 1.00
*       drought(15)  = 1.00
c       
       idc = 0
       do 250 j = scpft, ecpft 
c
c get index for current cropping practice - could have more than
c one crop existing in grid cell during the year for multiple
c cropping - but only one would be in live vegetation stage 
c
         if (exist(i,j) .eq. 1. .and. croplive(i,j) .gt. 0) then
            idc = j 
         endif
 250   continue 
c
c --------------------------------------------------------------------
c c3 crops physiology (soybean, wheat)
c ---------------------------------------------------------------------
       if (idc .eq. 13 .or. idc .eq. 15) then  ! soybean or wheat  
c
         rwork = 3.47e-03 - 1. / tl(i)
c
         tleaf = tl(i) - 273.16
c
         q10 = 2.0
c
c vmax and dark respiration for current conditions
c
c         tempvm = exp(3500.0 * rwork ) /
c     >            ((1.0 + exp(0.40 * (  lotemp(idc) - tleaf))) * 
c     >            (1.0 + exp(0.40 * (tleaf - hitemp(idc)))))
c
        tempvm = q10**((tleaf-15.0)/10.0) /    ! Collatz approach
     >           ((1.0 + exp(f1(idc) * (lotemp(idc) - tleaf))) * 
     >            (1.0 + exp(f2(idc) * (tleaf - hitemp(idc)))))

c
c adjust drystress factor imposed on soybeans - on a scale of
c 0 (less) - 1.0 (more), these have a 0.8 rating compared to 0.65 for maize 
c and for wheat
c from Penning de Vries, "Simulation of ecophysiological processes of
c growth in several annual crops"
c
c make average stress factor 25% higher to account for difference 
c
c       stressc3c = min(1.0, stresstl(i) * drought(idc))
c       NEW CJK 9-24-04
c
c	stressc3c = min(1.0, drought(idc))
        stressc3c = 1.0
        vmax      = max(0., vmax_pft(idc) * tempvm * 
     >              min(stressc3c, stressn(i,idc), croplive(i,idc)))
c       vmax      = max(0., vmax_pft(idc) * tempvm * 
c    >              min(stressc3c, croplive(i,idc)))
c       vmax      = max(0., vmax_pft(idc) * tempvm * croplive(i,idc))
c       vmax      = max(0., vmax_pft(idc) * tempvm * stressc3c * croplive(i,idc))
        rdarkc3   = gammac3 * vmax_pft(idc) * tempvm * croplive(i,idc)

c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
        je = topparl(i) * 4.59e-06 * alpha3 * (cic3(i) - gamstar) / 
     >       (cic3(i) + 2. * gamstar)
c
c 'rubisco limited' rate of photosynthesis (mol/m**2/s)
c
        jc = vmax * (cic3(i) - gamstar) / 
     >       (cic3(i) + kc * (1. + o2conc / ko))
c
        duma = thetac3
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the intermediate photosynthesis rate (mol/m**2/s)
c
        jp = min (dumq/duma, dumc/dumq)
c       
c 'sucrose synthesis limited' rate of photosynthesis (mol/m**2/s)
c       
        js = vmax / 2.2
c 
c solution to quadratic equation
c
        duma = betac3
        dumb = jp + js
        dumc = jp * js
c       
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agc3 = min (dumq/duma, dumc/dumq)
        anc3 = agc3 - rdarkc3
c
c apply stress functions to net photosynthesis rate
c
        anc3 = anc3 * stresstl(i)  ! CJK 6/20/2004  
c        if (i.eq.4) write(23,30) je*1e+06, vmax*1e+06, jc*1e+06, anc3*1e+06, tleaf
c30      format(5f8.2) 
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
        csc3(i) = 0.5 * (csc3(i) + co2conc - anc3 / gbco2l)
        csc3(i) = max (1.05 * gamstar, csc3(i))
c
c calculate new value of gs using implicit scheme
c
        gsc3(i) = 0.5 * (gsc3(i) + coefmc3 * anc3 * rh34 / csc3(i) +
     >                   coefbc3 * stressc3c)
c
        gsc3(i) = max (gsc3min, coefbc3 * stressc3c, gsc3(i))
c
c calculate new value of ci using implicit scheme
c
        cic3(i) = 0.5 * (cic3(i) + csc3(i) - 1.6 * anc3 / gsc3(i))
        cic3(i) = max (1.05 * gamstar, min (cimax, cic3(i)))
c
      else
        agc3    = 0.
        anc3    = 0.     
        csc3(i) = 0.
        gsc3(i) = 0.
        cic3(i) = 0.
c
      endif  ! c3 crops
   
c
c ---------------------------------------------------------------------
c c4 crop physiology (corn and sugarnace)
c
c modified by CJK to follow Collatz et al. (1992) parameterizations
c that were based on corn data in the field
c ---------------------------------------------------------------------
c
      if (idc .eq. 14 .or. idc.eq.16 ) then   ! maize or sugarcane    
c
c calculate the parameter values which are a function of temperature
c
c-original Maize         q10   = 2.0
              q10   = 2.5
        rwork = 3.47e-03 - 1. / tl(i)
        tleaf = tl(i) - 273.16
c
c       tempvm = exp(3500.0 * rwork ) /        ! original Agro-IBIS 1/26/2006
        tempvm = q10**((tleaf-15.0)/10.0) /    ! Collatz approach
     >           ((1.0 + exp(f1(idc) * (lotemp(idc) - tleaf))) * 
     >            (1.0 + exp(f2(idc) * (tleaf - hitemp(idc)))))
c
c temperature effect on dark respiration - changed to 15 C base
c 
c         tresp  = q10**((tleaf - 15.0)/10.0) /
c     >            (1.0 + exp(1.30 * (tleaf - 55.0)))  
c
c vmax and dark respiration for current conditions
c add nitrogen stress to c4 crops 
c
c       stressc4c = min(1.0, stresstl(i) * drought(idc))
c       stressc4c = min(1.0, drought(idc))
        stressc4c = 1.0   ! CJK 6/20/2004 
c       vmax    = max(0., vmax_pft(idc) * tempvm * 
c    >           min(stressc4c, croplive(i,idc)))

c      if(i.eq.1.and.iter.eq.3.and.idc.eq.16) write(223,*)jday,stressn(i,idc)
	if(idc.eq.16) stressn(i,idc)=1.0  !even aplying 150Kg/ha the stress is too large ate end of season

        vmax    = max(0., vmax_pft(idc) * tempvm * 
     >            min(stressc4c, stressn(i,idc), croplive(i,idc)))
c       vmax    = max(0., vmax_pft(idc)   * tempvm * stresstl(i) * croplive(i,idc))
c
        rdarkc4 = gammac4 * vmax_pft(idc) * tempvm * croplive(i,idc)
c       rdarkc4 = gammac4 * vmax_pft(idc) * tempvm 
c
csant-	print*,jday,time/3600.,vmax*1e+06,tempvm,rdarkc4*1e+06
c from Collatz et al. (1992) - changed to 15 C base 
c equation 5B
c
csant -   replace the original by the CLM3.0 scheme	
csant         tkco2 = q10**((tleaf - 15.0)/10)
c initial c4 co2 efficiency (mol/m**2/s)
c
c       kco2 = 18.0e+03 * vmax          
c        kco2 = 18.0e+03 * tkco2 * vmax  !original
csant- based on clm3.0
        kco2 = 4.0e+03  * vmax   !clm 3.5

csant- using this above eq. and coefm as 4.0 (original) - give the "same" result for NEE but with more AET.
c
c 'light limited' rate of photosynthesis (mol/m**2/s)
c
c       je = topparl(i) * 4.59e-06 * alpha4 ! original C4 all plants - collatz 
        je = topparl(i) * 4.59e-06  * 0.067 ! needed to increase efficiency of corn plants 
c
c 'rubisco limited' rate of photosynthesis
c
        jc = vmax
c
c solve for intermediate photosynthesis rate
c
        duma = thetac4
        dumb = je + jc
        dumc = je * jc
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
        jp = min (dumq/duma, dumc/dumq)
c
c 'carbon dioxide limited' rate of photosynthesis (mol/m**2/s)
c
        ji = kco2 * cic4(i)
c
        duma = betac4
        dumb = jp + ji
        dumc = jp * ji
c
        dume = max (dumb**2 - 4. * duma * dumc, 0.)
        dumq = 0.5 * (dumb + sqrt(dume)) + 1.e-15
c
c calculate the net photosynthesis rate (mol/m**2/s)
c
        agc4 = min (dumq/duma, dumc/dumq)
        anc4 = agc4 - rdarkc4

c	if(i.eq.1.and.iter.eq.3.and.time.ge.3600*8.and.time.le.18*3600) then
c	 use(1,int(time/3600.))= vmax*1e+06   !,tempvm,rdarkc4*1e+06
c	elseif(i.eq.1.and.iter.eq.3.and.time.eq.19*3600) then
c	write(23,134),jday,lai(i,1)*fl(i)*grnfraccrop(i,idc)
c     > ,use(1,8),use(1,9),use(1,10),use(1,11),use(1,12),use(1,13)
c     >,use(1,14),use(1,15),use(1,16),use(1,17),use(1,18),grnfraccrop(i,idc)
c	endif
c 134    format (i3,1x,f3.1,11(1x,f6.2),1x,f3.1)	

 	if(i.eq.1.and.iter.eq.3.and.time.eq.3600*13) then
        write(23,134),jday,time/3600.,lai(i,1)*fl(i)*grnfraccrop(i,idc),
     > vmax*1e+06,je*1e+06, ji*1e+06, anc4*1e+06, tleaf,tempvm
	endif
 134    format (1x,i3,1x,11(1x,f6.2))	

c
c apply stress functions to net photosynthesis rate
csant--- test remove water stress, and see that the winter low production is related with water stress!!

        anc4 = anc4 * max(0.0, stresstl(i))  ! CJK 6/20/2004 
c
c calculate co2 concentrations and stomatal condutance values
c using simple iterative procedure
c
c weight results with the previous iteration's values -- this
c improves convergence by avoiding flip-flop between diffusion
c into and out of the stomatal cavities
c
c calculate new value of cs using implicit scheme
c
csant- using this original equation the csc4 increase along the day, and it should actually decrease...
csant- should include a equation to define co2con at za, and make the transport to z12, and z34!!!
        csc4(i) = 0.5 * (csc4(i) + co2conc - anc4 / gbco2l)
csant- I include this modification, but a complete set of physical eqs. should replace it.
c         csc4(i) = (1./13.) * (csc4(i)*12 + co2conc - anc4 / gbco2l)

        csc4(i) = max (0.0, csc4(i))

c
c calculate new value of gs using implicit scheme
c
        gsc4(i) = 0.5 * (gsc4(i) + coefmc4 * anc4 * rh34 / csc4(i) +  
     >                   coefbc4 * stressc4c)
csant- this modification gives a better result, even though it should be discussed before implemented
csant- gs as function of lai (based on stomatal and hydraulic for sugarcane...pdf) 
c	if(lai(i,1).lt.1.2) then  !obs - lai(i,1) is always >= 1. (it is fractional lai)
c        gsc4(i) = 0.5 * (gsc4(i) + 3 * anc4 * rh34 / csc4(i) +  
c     >                   coefbc4 * stressc4c)
c	else
c        gsc4(i) = 0.5 * (gsc4(i) + 2.4 * anc4 * rh34 / csc4(i) +  
c     >                   coefbc4 * stressc4c)
csant	endif



c

        gsc4(i) = max (gsc4min, coefbc4 * stressc4c, gsc4(i)) 

csant      if(i.eq.1.and.iter.eq.3.and.time.eq.43200) print*,'gsc4_antes',time,jday,gsc4(i)
c
c calculate new value of ci using implicit scheme
c
        cic4(i) = (1/3.) * (cic4(i)*2. + csc4(i) - 1.6 * anc4 / gsc4(i))
        cic4(i) = max (0.0, min (cimax, cic4(i)))
c
      else
        agc4    = 0.
        anc4    = 0.     
        csc4(i) = 0.
        gsc4(i) = 0.
        cic4(i) = 0.
c
      endif    ! c4 crops
c ---------------------------------------------------------------------
c lower canopy scaling
c ---------------------------------------------------------------------
c
c calculate the approximate extinction coefficient
c
        extpar = (terml(i,6) * scalcoefl(i,1) + 
     >            terml(i,7) * scalcoefl(i,2) -
     >            terml(i,7) * scalcoefl(i,3)) /
     >            max (scalcoefl(i,4), epsilon)
c
        extpar = max (1.e-1, min (1.e+1, extpar))
c
c calculate canopy average photosynthesis (per unit leaf area):
c
        pxail = extpar * (lai(i,1) + sai(i,1))  !csant -lai(i,1) = avglail / fl(i)
        plail = extpar *  lai(i,1)

c
c scale is the parameter that scales from leaf-level photosynthesis to
c canopy average photosynthesis
c CD : replaced 24 (hours) by 86400/dtime for use with other timestep
c
        zweight = exp(-1. / (10.0 * 86400. / dtime))
c
c for non-zero lai
c
        if (plail.gt.0.0) then
c
c day-time conditions, use current scaling coefficient
c
          if (topparl(i).gt.10.) then
c
            scale = (1. - exp(-pxail)) / plail  !csant - for example - if lai=4, scale is +- 0.2

csant	if(i.eq.1) print*,'sai(i,1),lai(i,1),extpar',sai(i,1),lai(i,1),scale 
c
c update 10-day running mean of scale, weighted by light levels
c
            a10scalparaml(i) = zweight * a10scalparaml(i) + 
     >                         (1. - zweight) * scale * topparl(i)
c
            a10daylightl(i)  = zweight * a10daylightl(i) + 
     >                         (1. - zweight) * topparl(i)
c
c night-time conditions, use long-term day-time average scaling coefficient
c
          else
c
            scale = a10scalparaml(i) / a10daylightl(i)
c
          endif
c
c if no lai present
c
        else
c
          scale = 0.0
c
        endif
c
c perform scaling on all carbon fluxes from lower canopy
c
        agcls(i) = agls * scale
        agcl4(i) = agl4 * scale
        agcl3(i) = agl3 * scale
        agcc3(i) = agc3 * scale
        agcc4(i) = agc4 * scale
c
        ancls(i) = anls * scale
        ancl4(i) = anl4 * scale
        ancl3(i) = anl3 * scale
        ancc3(i) = anc3 * scale


        ancc4(i) = anc4 * scale
c
c calculate canopy average surface co2 concentration
c CD: For numerical stability (to avoid division by zero in gscl4),
c cscl4 is limited to 1e-8 mol_co2/mol_air.
c
        cscls = max (1.05 * gamstar, co2conc - ancls(i) / gbco2l)
        cscl3 = max (1.05 * gamstar, co2conc - ancl3(i) / gbco2l)
        cscl4 = max (1.0e-08       , co2conc - ancl4(i) / gbco2l)
c
        if (idc .eq. 13 .or. idc .eq. 15) then
          cscc3 = max (1.05 * gamstar, co2conc - ancc3(i) / gbco2l)
          cscc4 = 0.
        else if (idc .eq. 14 .or. idc.eq.16) then
          cscc4 = max (1.0e-08       , co2conc - ancc4(i) / gbco2l)
          cscc3 = 0.
        endif
c
c calculate canopy average stomatal conductance
c
        gscls = coefmls * ancls(i) * rh34 / cscls +
     >          coefbls * stresstl(i)
c
        gscl3 = coefml3 * ancl3(i) * rh34 / cscl3 +
     >          coefbl3 * stresstl(i)
c
        gscl4 = coefml4 * ancl4(i) * rh34 / cscl4 +
     >          coefbl4 * stresstl(i)
c
c c3/c4 crop physiology
c
        if (idc .eq. 13 .or. idc .eq. 15) then
          gscc3 = coefmc3 * ancc3(i) * rh34 / cscc3 +
     >            coefbc3 * stressc3c
          gscc4 = 0.
c
        else if (idc .eq. 14 .or. idc.eq.16) then
          gscc4 = coefmc4 * ancc4(i) * rh34 / cscc4 +
     >            coefbc4 * stressc4c
          gscc3 = 0.  
        else
          gscc3 = 0.
          gscc4 = 0.
        endif
c
        gscls = max (gslsmin, coefbls * stresstl(i), gscls)
        gscl3 = max (gsl3min, coefbl3 * stresstl(i), gscl3)
        gscl4 = max (gsl4min, coefbl4 * stresstl(i), gscl4)
c
c c3/c4 crop physiology
c
        if (idc .eq. 13 .or. idc .eq. 15) then
          gscc3 = max (gsc3min, coefbc3 * stressc3c, gscc3)
          gscc3 = gscc3 * grnfraccrop(i,idc)
          gscc4 = 0.
        else if (idc.eq.14 .or. idc.eq.16) then
csant- i include the greenfrac factor on photoss as well. IMPORTANT: after confirm, apply over C3 as well.
        ancc4(i) = ancc4(i) * grnfraccrop(i,idc)
        agcc4(i) = agcc4(i) * grnfraccrop(i,idc)

          gscc4 = max (gsc4min, coefbc4 * stressc4c, gscc4)
          gscc4 = gscc4 * grnfraccrop(i,idc)
          gscc3 = 0.
        else
          gscc3 = 0.
          gscc4 = 0.
        endif

c	if(i.eq.1.and.iter.eq.3.and.
c     >(time.le.5*3600.or.time.ge.20*3600))then  !.and.ustar(1).ge.0.25) then
	if(i.eq.1.and.iter.eq.3.and.time.ge.3600*8.and.time.le.18*3600) then

	 use(2,int(time/3600.))=csc4(i)*1e+06  !     gscc4  *lai(i,1)*fl(i)*frac(i,16)  
csant- 	sl = sl*rhoa - (m s-1*kg m-3) -> para passar para mol.m-2.s-1 multiplica por *(1/0.029 mol/kg) -> 
c	 use(2,int(time/3600.))=sl(i)*(1./0.029)!*lai(i,1)*fl(i)*frac(i,16)  
c	 use(2,int(time/3600.))=-frac(i,16)*ancc4(i)*lai(i,1)*fl(i)*1e+06*grnfraccrop(i,idc)
c	 use(2,int(time/3600.))=rh34 
	
c       print*,jday,cic4(i)*1e+06,lai(i,1)*fl(i)*grnfraccrop(i,idc),gscc4*lai(i,1)*fl(i)*frac(i,16) !only grn make photo
c     > ,ancc4(i)*(10**6)*frac(i,16)*lai(i,1)*fl(i),rh34

c      print*,jday,time/3600,lai(i,1)*fl(i)*grnfraccrop(i,idc),sl(i)*(1./0.029) 
c     > ,ancc4(i)*(10**6)*frac(i,16)*lai(i,1)*fl(i),sl(i)*(1./0.029)*lai(i,1)*fl(i)*frac(i,16)

c       print*,jday,time/3600.,lai(i,1)*fl(i)*grnfraccrop(i,idc)
c     > ,-ancc4(i)*(10**6)*frac(i,16)*lai(i,1)*fl(i),ta(i)-273.18,tleaf,tempvm 


	elseif(i.eq.1.and.iter.eq.3.and.time.eq.19*3600) then
	write(223,132),jday,lai(i,1)*fl(i)*grnfraccrop(i,idc),use(2,8),use(2,9),use(2,10)
     >,use(2,11),use(2,12),use(2,13),use(2,14),use(2,15),use(2,16),use(2,17),use(2,18)

	endif

 132    format (1x,i3,1x,f4.2,11(1x,f6.2))	

c
c The following adjusts the above calculated values of ancl3, ancl4,
c agcl3, agcl4, gscl3, and gscl4 according to what percentage of the
c lower canopy is green by weighting the above calculations by greenfrac
c terms. Only the green portion of the canopy performs photosynthesis.
c Shrubs that have leaves have only green leaves since they are allowed
c to drop their leaves in the fall. C3 and C4 grasses may be either green
c or brown so they are the only terms that are adjusted.
c
c Scale value of ancl3, ancl4, gscl3, and gscl4 according to what fraction
c of the canopy is green
c
        ancl3(i) = ancl3(i) * greenfracl3(i)
        ancl4(i) = ancl4(i) * greenfracl4(i)
c
        agcl3(i) = agcl3(i) * greenfracl3(i)
        agcl4(i) = agcl4(i) * greenfracl4(i)
c
        gscl3 = gscl3 * greenfracl3(i)
        gscl4 = gscl4 * greenfracl4(i)
c
c calculate canopy and boundary-layer total conductance for water vapor diffusion
c
        rwork = 1. / sl(i)   !csant- rb = 1/gb (gb is the leaf boundary-layer conduct) but sl=gb*rhoa (m s-1 * kg m-3)
        dump =  1. / 0.029   !
c
        totcondls(i) = 1. / (rwork + dump / gscls)
c
c Make sure that the calculation does not divide by zero if gscl3 or
c gscl4 are equal to zero
c
        if (gscl3 .gt. 0) then
          totcondl3(i) = 1. / ( rwork + dump / gscl3 )
        else
          totcondl3(i) = 0
        endif
c
        if (gscl4 .gt. 0) then
          totcondl4(i) = 1. / ( rwork + dump / gscl4 )
        else
          totcondl4(i) = 0
        endif
c
        if (gscc3 .gt. 0) then
          totcondc3(i) = 1. / ( rwork + dump / gscc3 )
        else
          totcondc3(i) = 0
        endif
c
        if (gscc4 .gt. 0) then
          totcondc4(i) = 1. / ( rwork + dump / gscc4 )
        else
          totcondc4(i) = 0
        endif

csant	if(i.eq.1.and.iter.eq.3.and.time.eq.43200) print*,'totcondc4=sl+gscc4',totcondc4(i),sl(i),gscc4
c
c
c multiply canopy photosynthesis by wet fraction -- this calculation is
c done here and not earlier to avoid using within canopy conductance
c
c	print*,fwetl(i)
        rwork = 1. - fwetl(i)
c
        agcls(i) = rwork * agcls(i)
        agcl3(i) = rwork * agcl3(i)
        agcl4(i) = rwork * agcl4(i)
        agcc3(i) = rwork * agcc3(i)
        agcc4(i) = rwork * agcc4(i)
c
        ancls(i) = rwork * ancls(i)
        ancl3(i) = rwork * ancl3(i)
        ancl4(i) = rwork * ancl4(i)
        ancc3(i) = rwork * ancc3(i)
        ancc4(i) = rwork * ancc4(i)
c
 200  continue
c
c return to main program
c

      return
      end


      subroutine drystressWrapper(time, jday,
     >                            dim_params,
     >                            npoi_matrix,
     >                            sfield,
     >                            stressl,
     >                            stressu,
     >                            swilt,
     >                            wisoi,
     >                            wsoi,
     >                            froot)

            integer jday
            
            double precision time

            integer dim_params(2)

            double precision npoi_matrix(2, dim_params(1))

            double precision npoi_line1(dim_params(1))
            double precision npoi_line2(dim_params(1))

            double precision sfield(dim_params(1), dim_params(2))
            double precision stressl(dim_params(1), dim_params(2))
            double precision stressu(dim_params(1), dim_params(2))
            double precision swilt(dim_params(1), dim_params(2))
            double precision wisoi(dim_params(1), dim_params(2))
            double precision wsoi(dim_params(1), dim_params(2))
            double precision froot(dim_params(2), 2)

            integer i

c 	    Inicializando es double de tamanho npoi
            DO 102, i = 1, dim_params(1), 1
                        npoi_line1(i)  = npoi_matrix(1, i)
                        npoi_line2(i)  = npoi_matrix(2, i)
102         CONTINUE

            call drystress(time, jday, dim_params(1), dim_params(2), 
     >                     npoi_line1, npoi_line2,
     >                     sfield,
     >                     stressl,
     >                     stressu,
     >                     swilt,
     >                     wisoi,
     >                     wsoi,
     >                     froot)
            
c 	    Inicializando es double de tamanho npoi
            DO 777, i = 1, dim_params(1), 1
                npoi_matrix(1, i) = npoi_line1(i)
                npoi_matrix(2, i) = npoi_line2(i)
777         CONTINUE

      end

c JAIR: Old    - Chamada dentro de canini (inicialmente essa rotina estava em physiology)
c JAIR: Update - Chamar individualmente. Retornar para physiology?

c ---------------------------------------------------------------------
      subroutine drystress(time, jday,
     >                     npoi,
     >                     nsoilay,
     >                     stresstl,
     >                     stresstu,
     >                     sfield,
     >                     stressl,
     >                     stressu,
     >                     swilt,
     >                     wisoi,
     >                     wsoi,
     >                     froot)

c ---------------------------------------------------------------------
c
c modified by CJK 1/25/2005 for dynamic root water uptake adjustment
c approach of K. Li
c
c common blocks
c
      implicit none
c
c      include 'compar.h'
c      include 'comsoi.h'
c      include 'comveg.h'

c global variables

      integer npoi
      integer nsoilay

      double precision stresstl(npoi)
      double precision stresstu(npoi)

      double precision sfield(npoi,nsoilay)
      double precision stressl(npoi,nsoilay)
      double precision stressu(npoi,nsoilay)
      double precision swilt(npoi,nsoilay)
      double precision wisoi(npoi,nsoilay)
      double precision wsoi(npoi,nsoilay)
      double precision froot(nsoilay,2)

c local variables
c
      integer i, k,   ! loop indices
     >     jday,   klay    ! layer of soil that is one third of total depth 
c
      double precision time  

      double precision stressfac, ! to calculate moisture stress factor 
     >     awc,       ! available water content (fraction)
     >     znorm,     ! normalizing factor
     >     zwilt,     ! function of awc, =1 if awc = 1 (no stress)
     >     f1,
     >     f2,
     >     foptawc,
     >     afact,
     >     lambda,    ! exponent in root water uptake equation
     >     sdep,      ! total soil depth calculation
     >     tthird,    ! top third of soil depth
     >     botawc,
     >     topawc,
     >     botmxw,
     >     topmxw,
     >     topw,
     >     botw,
     >     mxwrate
c
c stressfac determines the 'strength' of the soil moisture
c stress on physiological processes
c
c strictly speaking, stresst* is multiplied to net photosynthesis 
c parameters used in the photosynthesis calculations
c
c stressfac determines the shape of the soil moisture response
c
c      stressfac = -5.0
c
c      znorm = 1.0 - exp(stressfac)
c

      do 100 i = 1, npoi
c
c initialize stress parameter
c
        stresstl(i) = 0.0
        stresstu(i) = 0.0
c
c set values of lambda
c
c        sdep = 0.    ! total soil depth
c        do 105 k = 1, nsoilay
c          sdep = sdep + hsoi(k) 
c 105    continue 
c
c        tthird = sdep * 0.3333   ! calculate 1/3 of total depth
c        sdep = 0.
c        klay = 0
c        do 110 k = 1, nsoilay
c          sdep = sdep + hsoi(k)
c          if (sdep .le. tthird) klay = k
c 110    continue      
c
c        topw   = 0.
c        botw   = 0.
c        topmxw = 0.
c        botmxw = 0.
c        topawc = 0.
c        botawc = 0.
c        do 120 k = 1, klay 
c          topw   = topw + (wsoi(i,k)*(1-wisoi(i,k)) - swilt(i,k)) 
c          topmxw = topmxw + (sfield(i,k) - swilt(i,k)) 
c 120    continue
c        topawc = topw / topmxw 
c
c        do 130 k = klay+1, nsoilay 
c          botw   = botw + (wsoi(i,k)*(1-wisoi(i,k)) - swilt(i,k)) 
c          botmxw = botmxw + (sfield(i,k) - swilt(i,k)) 
c 130    continue
c        botawc = botw / botmxw 
c      
c assign lambda values
c
c        if (topawc .lt. 0.20 .and. botawc .gt. 0.5) then
c          lambda = 0.50 
c        else if (topawc .lt. 0.20 .and.
c     >           (botawc .ge. 0.2 .and. botawc .le. 0.5)) then
c          lambda = 0.75
c        else if ((topawc .ge. 0.2 .and. topawc .le. 0.5) .and.
c     >            botawc .gt. 0.5) then
c          lambda = 0.75         
c        else if ((topawc .ge. 0.2 .and. topawc .le. 0.5) .and.
c     >            botawc .lt. 0.2) then
c          lambda = 1.25         
c        else if (topawc .gt. 0.5 .and.
c     >           ( botawc .ge. 0.2 .and. botawc .le. 0.5)) then
c          lambda = 1.25         
c        else if (topawc .gt. 0.5 .and. botawc .lt. 0.2) then
c          lambda = 1.50         
c        else
c          lambda = 1.00
c        endif
c
c fraction of soil water uptake in each layer
c
        do 200 k = 1, nsoilay
c
	

          awc = min (1.0, max (0.0,
     >              (wsoi(i,k)*(1 - wisoi(i,k))   - swilt(i,k)) /
     >              (sfield(i,k) - swilt(i,k))
     >              )         )
c
c original IBIS formulation for zwilt 
c
c         zwilt = (1. - exp(stressfac * awc)) / znorm
c
c 
c J. Norman soil water availability factor (zwilt) 
c 1/25/2004
c
           zwilt = 1.0 - (dlog(DBLE(1)+DBLE(900.0)*exp(-20.0*awc))/dlog(DBLE(900.)))  
c
c update for each layer
c
           stressl(i,k) = froot(k,1) * max (0.0, min (1.0, zwilt))
           stressu(i,k) = froot(k,2) * max (0.0, min (1.0, zwilt))
c
c          stressl(i,k) = froot(k,1)**lambda * max (0.0, min (1.0, zwilt))
c          stressu(i,k) = froot(k,2)**lambda * max (0.0, min (1.0, zwilt))
c
c calculate maximum dimensionless water uptake rate (ranging from 0-1)
c
c          mxwrate = 1. - (1 + 1.3 * awc)**(-bex(i,k)) 
c          stressl(i,k) = stressl(i,k) * mxwrate
c          stressu(i,k) = stressu(i,k) * mxwrate
c
c integral over rooting profile
c
          stresstl(i) = stresstl(i) + stressl(i,k)

          stresstu(i) = stresstu(i) + stressu(i,k)
c         stresstl(i) = 1.0   !No Stress effect due to moisture
c
 200    continue
c
 100  continue

c sant	print*, sfield(1,1), swilt(1,1),sfield(1,9), swilt(1,9)
c
c	if(time.eq.39600)
c     >  write(225,34)jday,wsoi(1,1),wsoi(1,2),wsoi(1,3),wsoi(1,4),wsoi(1,5),wsoi(1,6),
c     >wsoi(1,7),wsoi(1,8),wsoi(1,9),wsoi(1,10),wsoi(1,11), sfield(1,11),swilt(1,11)

c	if(time.eq.39600)
c     >write(225,34)jday,stressl(1,1),stressl(1,2),stressl(1,3),stressl(1,4),stressl(1,5)
c     >,stressl(1,6),stressl(1,7),stressl(1,8),stressl(1,9),stressl(1,10),stressl(1,11),stresstl(1)

c 34   format (1x,i3,1x,13(1x,f4.2))
c
c return to main program
c
      return
      end
c
c


c JAIR: Old    - Chamada dentro de turvap

c ---------------------------------------------------------------------
      subroutine impexp (wimp, tveg, ch, wliq, wsno, iter,
     >                   epsilon,
     >                   hfus,
     >                   npoi,
     >                   tmelt)
c ---------------------------------------------------------------------
c
c sets the implicit vs explicit fraction in turvap calcs for
c upper leaves, upper stems or lower veg. this is to account for
c temperatures of freezing/melting intercepted h2o constrained
c at the melt point. if a purely implicit calc is used for such
c a surface, the predicted temperature would be nearly the atmos
c equil temp with little sensible heat input, so the amount of
c freezing or melting is underestimated. however, if a purely
c explicit calc is used with only a small amount of intercepted
c h2o, the heat exchange can melt/freeze all the h2o and cause
c an unrealistic huge change in the veg temp. the algorithm
c below attempts to avoid both pitfalls
c
c common blocks
c
      implicit none
c     include 'compar.h'
c global variables

      integer npoi
      double precision epsilon
      double precision hfus
      double precision tmelt
c input/output variables
c
      integer iter  ! current iteration number (supplied)
c
      double precision      
     >  wimp(npoi), ! implicit/explicit fraction (0 to 1) (returned)
     >  tveg(npoi), ! temperature of veg (previous iteration's soln) (supp)
     >  ch(npoi),   ! heat capacity of veg (supplied)
     >  wliq(npoi), ! veg intercepted liquid (supplied)
     >  wsno(npoi)  ! veg intercepted snow (supplied)
c
c local variables
c
      integer i
      integer j
c
      double precision h, z, winew
c
c for first iteration, set wimp to fully implicit, and return
c
      if (iter.eq.1) then     
c
        do 101 j = 1, npoi
            wimp(j) = 1.0
 101    continue
c        call const(wimp, npoi, 1.000000000)
        return

      endif
c
c for second and subsequent iterations, estimate wimp based on
c the previous iterations's wimp and its resulting tveg.
c
c calculate h, the "overshoot" heat available to melt any snow
c or freeze any liquid. then the explicit fraction is taken to
c be the ratio of h to the existing h2o's latent heat (ie, 100%
c explicit calculation if not all of the h2o would be melted or
c frozen). so winew, the implicit amount, is 1 - that ratio.
c but since we are using the previous iteration's t* results
c for the next iteration, to ensure convergence we need to damp
c the returned estimate wimp by averaging winew with the 
c previous estimate. this works reasonably well even with a
c small number of iterations (3), since for instance with large
c amounts of h2o so that wimp should be 0., a good amount of 
c h2o is melted or frozen with wimp = .25
c
      do 100 i = 1, npoi
c
        h = ch(i) * (tveg(i) - tmelt)
        z = max (abs(h), epsilon)
c
        winew = 1.0
c
        if (h.gt.epsilon)  winew = 1. - min (1., hfus * wsno(i) / z)
        if (h.lt.-epsilon) winew = 1. - min (1., hfus * wliq(i) / z)
c
        wimp(i) = 0.5 * (wimp(i) + winew)
c
  100 continue
c
      return
      end
c
c

c JAIR: Chamada dentro de turvap

c ---------------------------------------------------------------------
      subroutine impexp2 (wimp, t, told, iter,
     >                    epsilon,
     >                    npoi,
     >                    tmelt)
c ---------------------------------------------------------------------
c
c sets the implicit vs explicit fraction in turvap calcs for
c seaice or snow skin temperatures, to account for temperatures
c of freezing/melting surfaces being constrained at the melt
c point
c
c unlike impexp, don't have to allow for all h2o 
c vanishing within the timestep
c
c wimp   = implicit fraction (0 to 1) (returned)
c
      implicit none
c      include 'compar.h'
c
c global variables

      double precision epsilon
      integer npoi
      double precision tmelt

c input variables
c
      integer iter
      double precision 
     >  wimp(npoi), t(npoi), told(npoi)
c
c local variables
c
      integer j  
      integer i    ! loop indice
c
c for first iteration, set wimp to fully implicit, and return
c
      if (iter.eq.1) then

        do 101 j = 1, npoi
            wimp(j) = 1.0
 101    continue
c        call const(wimp, npoi, 1.0)
        return
      endif
c
      do 100 i = 1, npoi
c
        if ((t(i)-told(i)).gt.epsilon) wimp(i) = (tmelt - told(i)) / 
     >                                           (t(i)  - told(i))
        wimp(i) = max (0.0, min (1.0, wimp(i)))
c
 100  continue
c
      return
      end
c


c ---------------------------------------------------------------------
      subroutine const (arr, nar, value)
c ---------------------------------------------------------------------
c
c sets all elements of real vector arr to value
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
      subroutine linsolve (arr, rhs, vec, mplate, nd, npoi)
c ---------------------------------------------------------------------
c
c solves multiple linear systems of equations, vectorizing
c over the number of systems. basic gaussian elimination is 
c used, with no pivoting (relies on all diagonal elements
c being and staying significantly non-zero)
c
c a template array mplate is used to detect when an operation 
c is not necessary (element already zero or would add zeros),
c assuming that every system has the same pattern of zero
c elements
c
c this template is first copied to mplatex since it 
c must be updated during the procedure in case an original-zero
c pattern location becomes non-zero
c
c the first subscript in arr, rhs, vec is over the multiple
c systems, and the others are the usual row, column subscripts
c
      implicit none
c
c     include 'compar.h'
c
      integer npoi

c Arguments (input-output)
c
      integer nd                  ! number of equations (supplied)
c
      integer  mplate(nd,nd)      ! pattern of zero elements of arr (supplied)
c
      double precision arr(npoi,nd,nd),       ! equation coefficients (supplied, overwritten)
     >     rhs(npoi,nd),          ! equation right-hand sides (supplied, overwritten) 
     >     vec(npoi,nd)           ! solution (returned)
c 
c local variables
c
      integer ndx,                ! Max number of equations
     >        j, i, id, m         ! loop indices
c
c     JAIR: Adicionado indice para laço que substitui const
      integer jb  

      parameter (ndx=9)
c
      integer mplatex(ndx,ndx)
c
      double precision f(npoi)
c
      if (nd.gt.ndx) then
         write(*,900) nd, ndx
  900    format(/' *** fatal error ***'/
     >          /' number of linsolve eqns',i4,' exceeds limit',i4)
c        call endrun
      endif
c
c copy the zero template so it can be changed below
c
      do 6 j=1,nd
        do 5 i=1,nd
          mplatex(i,j) = mplate(i,j)
    5   continue
    6 continue
c
c zero all array elements below the diagonal, proceeding from
c the first row to the last. note that mplatex is set non-zero
c for changed (i,j) locations, in loop 20
c
      do 10 id=1, nd-1
         do 12 i=id+1,nd
c
            if (mplatex(i,id).ne.0) then
               do 14 m=1,npoi
                  f(m) = arr(m,i,id) / arr(m,id,id)
   14          continue
c
               do 20 j=id,nd
                  if (mplatex(id,j).ne.0) then
                     do 22 m=1,npoi
                        arr(m,i,j) = arr(m,i,j) - f(m)*arr(m,id,j)
   22                continue
                     mplatex(i,j) = 1
                  endif
   20          continue
c
               do 30 m=1,npoi
                  rhs(m,i) = rhs(m,i) - f(m)*rhs(m,id)
   30          continue
            endif
c
   12    continue
   10 continue
c
c all array elements below the diagonal are zero, so can
c immediately solve the equations in reverse order
c
      do 50 id=nd,1,-1
c         
         do 999 jb = 1, npoi        
             f(jb) = 0.0   
999      continue
c         call const (f, npoi, 0.0)

         if (id.lt.nd) then
            do 52 j=id+1,nd
               if (mplatex(id,j).ne.0) then
                  do 54 m=1,npoi
                     f(m) = f(m) + arr(m,id,j)*vec(m,j)
   54             continue
               endif
   52       continue
         endif
c
         do 56 m=1,npoi
            vec(m,id) = (rhs(m,id) - f(m)) / arr(m,id,id)
   56    continue
c
   50 continue
c


      return
      end



c ------------------------------------------------------------------------
      double precision function twet3(tak, q, p,
     >                    cair,
     >                    hvap,
     >                    cvap,
     >                    ch2o)
c ------------------------------------------------------------------------
c
c twet3.f last update 8/30/2000 C Molling
c
c This function calculates the wet bulb temperature given
c air temp, specific humidity, and air pressure.  It needs the function esat
c in order to work (in comsat.h).  The function is an approximation to
c the actual wet bulb temperature relationship.  It agrees well with the
c formula in the Smithsonian Met. Tables for moderate humidities, but differs
c by as much as 1 K in extremely dry or moist environments.
c
c INPUT
c     tak - air temp in K
c     q - specific humidity in kg/kg
c     p - air pressure in Pa (Pa = 100 * mb)
c
c OUTPUT
c     twet3 - wet bulb temp in K, accuracy?
c
      implicit none
c     include 'compar.h'
c
      integer i
c
      double precision p, q, tak, ta, twk, twold, diff
c
c     include 'comsat.h'

c globals

      double precision cair
      double precision hvap
      double precision cvap
      double precision ch2o

            double precision asat0, asat1, asat2, asat3, asat4, asat5, asat6
c
      parameter (asat0 =  6.1078000,
     >           asat1 =  4.4365185e-1,
     >           asat2 =  1.4289458e-2,
     >           asat3 =  2.6506485e-4,
     >           asat4 =  3.0312404e-6,
     >           asat5 =  2.0340809e-8,
     >           asat6 =  6.1368209e-11 )
c
      double precision bsat0, bsat1, bsat2, bsat3, bsat4, bsat5, bsat6
c
      parameter (bsat0 =  6.1091780,
     >           bsat1 =  5.0346990e-1,
     >           bsat2 =  1.8860134e-2,
     >           bsat3 =  4.1762237e-4,
     >           bsat4 =  5.8247203e-6,
     >           bsat5 =  4.8388032e-8,
     >           bsat6 =  1.8388269e-10 )
c
      double precision csat0, csat1, csat2, csat3, csat4, csat5, csat6
c
      parameter (csat0 =  4.4381000e-1,
     >           csat1 =  2.8570026e-2,
     >           csat2 =  7.9380540e-4,
     >           csat3 =  1.2152151e-5,
     >           csat4 =  1.0365614e-7,
     >           csat5 =  3.5324218e-10,
     >           csat6 = -7.0902448e-13 )
c
      double precision dsat0, dsat1, dsat2, dsat3, dsat4, dsat5, dsat6
c
      parameter (dsat0 =  5.0303052e-1,
     >           dsat1 =  3.7732550e-2,
     >           dsat2 =  1.2679954e-3,
     >           dsat3 =  2.4775631e-5,
     >           dsat4 =  3.0056931e-7,
     >           dsat5 =  2.1585425e-9,
     >           dsat6 =  7.1310977e-12 )

      double precision t,        ! temperature argument of statement function 
     >     tair,     ! temperature argument of statement function 
     >     p1,       ! pressure argument of function 
     >     e1,       ! vapor pressure argument of function
     >     q1,       ! saturation specific humidity argument of function
     >     tsatl,    ! statement function
     >     tsati,    ! 
     >     esat,     !
     >     desat,    !
     >     qsat,     ! 
     >     dqsat,    ! 
     >     hvapf,    ! 
     >     hsubf,    !
     >     cvmgt     ! function

      tsatl(t) = min (100., max (t-273.16, 0.))
      tsati(t) = max (-60., min (t-273.16, 0.))

      esat (t) = 
     >  100.*(
     >    cvmgt (asat0, bsat0, t.ge.273.16)
     >    + tsatl(t)*(asat1 + tsatl(t)*(asat2 + tsatl(t)*(asat3
     >    + tsatl(t)*(asat4 + tsatl(t)*(asat5 + tsatl(t)* asat6)))))
     >    + tsati(t)*(bsat1 + tsati(t)*(bsat2 + tsati(t)*(bsat3
     >    + tsati(t)*(bsat4 + tsati(t)*(bsat5 + tsati(t)* bsat6)))))
     >  )

      desat (t) =
     >  100.*(
     >    cvmgt (csat0, dsat0, t.ge.273.16)
     >    + tsatl(t)*(csat1 + tsatl(t)*(csat2 + tsatl(t)*(csat3
     >    + tsatl(t)*(csat4 + tsatl(t)*(csat5 + tsatl(t)* csat6)))))
     >    + tsati(t)*(dsat1 + tsati(t)*(dsat2 + tsati(t)*(dsat3
     >    + tsati(t)*(dsat4 + tsati(t)*(dsat5 + tsati(t)* dsat6)))))
     >  )

       qsat (e1, p1) = 0.622 * e1 /
     >               max ( p1 - (1.0 - 0.622) * e1, 0.622 * e1 )

       dqsat (t, q1) = desat(t) * q1 * (1. + q1*(1./0.622 - 1.)) /
     >                 esat(t)  
c
c temperatures in twet3 equation must be in C
c pressure in qsat function must be in Pa
c temperatures in esat,hvapf functions must be in K
c
c     Air temp in C
c     -------------
      ta = tak - 273.16
c
c     First guess for wet bulb temp in C, K
c     -------------------------------------
      twet3 = ta * q / qsat(esat(tak),p)
      twk = twet3 + 273.16

c     hvapf(twk,tak)
c     Iterate to converge
c     -------------------
      do 100 i = 1, 20
         twold = twk - 273.16
         twet3 = ta - ((hvap + cvap*(tak-273.16) - ch2o*(twk-273.16)) / cair) * ( qsat( esat(twk),p )-q )
         diff = twet3 - twold
c
c below, the 0.2 is the relaxation parameter that works up to 40C (at least)
c
         twk = twold + 0.2 * diff + 273.16
         if (abs(twk-273.16-twold) .lt. 0.02) goto 999
 100  continue
c
      print *, 'Warning, twet3 failed to converge after 20 iterations!'
      print *, 'twet3, twetold: ', twk, twold + 273.16
      print *, 'twetbulb is being set to the air temperature'
c
      twet3 = tak
c
c     Return wet bulb temperature in K
c     --------------------------------
 999  twet3 = twk
c
      return
      end
c