c
c #    #  ######   ####   ######   #####    ##     #####     #     ####   #    #
c #    #  #       #    #  #          #     #  #      #       #    #    #  ##   #
c #    #  #####   #       #####      #    #    #     #       #    #    #  # #  #
c #    #  #       #  ###  #          #    ######     #       #    #    #  #  # #
c  #  #   #       #    #  #          #    #    #     #       #    #    #  #   ##
c   ##    ######   ####   ######     #    #    #     #       #     ####   #    #
c
c ---------------------------------------------------------------------

           subroutine phenoWrapper(local_params, dim_params, double_params,
     >                             logical_matrix,
     >                             npoi_matrix, 
     >                             frac,
     >                             plai,
     >                             zbot,
     >                             ztop,
     >                             lai)

               implicit none

               integer          local_params
               integer          dim_params(2)
               double precision double_params

               double precision frac(dim_params(1), dim_params(2))
               double precision plai(dim_params(1), dim_params(2))
               double precision zbot(dim_params(1),2)
               double precision ztop(dim_params(1),2)
               double precision lai(dim_params(1), 2)

               logical          logical_matrix(6, dim_params(1))
        
               logical          npoi_logical_line1(dim_params(1))
               logical          npoi_logical_line2(dim_params(1)) 
               logical          npoi_logical_line3(dim_params(1))
               logical          npoi_logical_line4(dim_params(1))
               logical          npoi_logical_line5(dim_params(1))
               logical          npoi_logical_line6(dim_params(1))
      
               double precision npoi_matrix(35, dim_params(1))

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

               integer i

c 	  Inicializando vetores logical de tamanho npoi
               DO 101, i = 1, dim_params(1), 1
                        npoi_logical_line1(i) = logical_matrix(1, i)
                        npoi_logical_line2(i) = logical_matrix(2, i)
                        npoi_logical_line3(i) = logical_matrix(3, i)
                        npoi_logical_line4(i) = logical_matrix(4, i)
                        npoi_logical_line5(i) = logical_matrix(5, i)
                        npoi_logical_line6(i) = logical_matrix(6, i)
101            CONTINUE

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
102             CONTINUE

                call pheno(local_params,
     >                     dim_params(1), dim_params(2),
     >                     double_params,
     >                     frac,
     >                     plai,
     >                     zbot,
     >                     ztop,
     >                     lai,
     >                     npoi_logical_line1, npoi_logical_line2, npoi_logical_line3,
     >                     npoi_logical_line4, npoi_logical_line5, npoi_logical_line6,
     >                     npoi_line1, npoi_line2, npoi_line3, npoi_line4,
     >                     npoi_line5, npoi_line6, npoi_line7, npoi_line8, 
     >                     npoi_line9, npoi_line10, npoi_line11, npoi_line12, 
     >                     npoi_line13, npoi_line14, npoi_line15, npoi_line16, 
     >                     npoi_line17, npoi_line18, npoi_line19, npoi_line20, 
     >                     npoi_line21, npoi_line22, npoi_line23, npoi_line24,  
     >                     npoi_line25, npoi_line26, npoi_line27, npoi_line28,  
     >                     npoi_line29, npoi_line30, npoi_line31, npoi_line32,  
     >                     npoi_line33, npoi_line34, npoi_line35)

c 	  Retornando valores para as variáveis
               DO 999, i = 1, dim_params(1), 1
                        logical_matrix(1, i) = npoi_logical_line1(i)
                        logical_matrix(2, i) = npoi_logical_line2(i)
                        logical_matrix(3, i) = npoi_logical_line3(i)
                        logical_matrix(4, i) = npoi_logical_line4(i)
                        logical_matrix(5, i) = npoi_logical_line5(i)
                        logical_matrix(6, i) = npoi_logical_line6(i)
999            CONTINUE

                DO 888, i = 1, dim_params(1), 1
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
888             CONTINUE
             

      end

c JAIR: rotina1 - OK!

      subroutine pheno(jday,
     >                 npoi, !dim
     >                 npft, !dim
     >                 epsilon, 
     >                 frac,! others 
     >                 plai,
     >                 zbot,
     >                 ztop,
     >                 lai,
     >                 onflagu, ! array npoi logical
     >                 onflagl3,
     >                 onflagl4,
     >                 offflagu,
     >                 offflagl3,
     >                 offflagl4,
     >                 PPTavgann, ! array npoi double (35 variáveis)
     >                 PPTsumcrit,
     >                 precip, 
     >                 precipsum, 
     >                 stresstl, 
     >                 stsuml, 
     >                 stsumu, 
     >                 sumcom ,
     >                 Tavgann ,
     >                 tc ,
     >                 td,
     >                 templ , 
     >                 templs ,
     >                 tempu ,
     >                 tminavgann, 
     >                 a10ancl3 ,
     >                 a10ancl4 ,
     >                 a10ancls ,
     >                 a10ancub ,
     >                 a10td ,
     >                 a11soiltd, 
     >                 a3tdmin ,
     >                 agddl ,
     >                 agddu ,
     >                 daylength, 
     >                 dropl3 ,
     >                 dropl4 ,
     >                 dropls ,
     >                 dropu ,
     >                 fl ,
     >                 fu ,
     >                 greenfracl, 
     >                 greenfracl3, 
     >                 greenfracl4 ,
     >                 icropsum)

c --------------------->------------------------------------------------
c>
c common blocks>
c
c      include 'implicit.h'
c
c      include 'compar.h'
c      include 'comatm.h'
c      include 'comsoi.h'
c      include 'comsum.h'
c      include 'comveg.h'
c      include 'comcrop.h'
c
c local variables
c
      integer
     >  i,j,          !
     >  jday, vegscheme   !csant- I'v been testing the impact of these two diferent pheno over Amazon
c
      double precision
     >  ddays,        !
     >  ddfac,        !
     >  avglaiu,      ! average lai of upper canopy 
     >  avglail,      ! average lai of lower canopy 
     >  sthreshold,   !  
     >  sumcrop,
     >  tthreshold,   ! temperature threshold for budburst and senescence
     >  gthreshold,   ! temperature threshold for budburst and senescence
**** DTP 2000/06/28 Modified this following discussion with Navin. We now
*    retain fu(i) derived from dynaveg and constrain it to a local value
*    "fu_phys" in the range 0.25 to 0.975 used in the canopy physics calcs.
     >  fu_phys       ! Local value of fu(i) constrained to range 0.25 to 0.975
                      ! to keep physics calculations stable.


c global variables as parameters

        integer npoi, npft

        double precision epsilon
        
        ! others
        double precision frac(npoi, npft)
        double precision plai(npoi, npft)
        double precision zbot(npoi,2)
        double precision ztop(npoi,2)
        double precision lai(npoi, 2)

        ! array npoi logical
        logical onflagu(npoi)
        logical onflagl3(npoi)
        logical onflagl4(npoi)
        logical offflagu(npoi)
        logical offflagl3(npoi)
        logical offflagl4(npoi)
        
        ! array npoi double (35 variáveis)
        double precision PPTavgann(npoi)
        double precision PPTsumcrit(npoi)
        double precision precip(npoi)
        double precision precipsum(npoi)
        double precision stresstl(npoi)
        double precision stsuml(npoi)
        double precision stsumu(npoi)
        double precision sumcom(npoi)
        double precision Tavgann(npoi)
        double precision tc(npoi)
        double precision td(npoi)
        double precision templ(npoi)
        double precision templs(npoi)
        double precision tempu(npoi)
        double precision tminavgann(npoi)
        double precision a10ancl3(npoi)
        double precision a10ancl4(npoi)
        double precision a10ancls(npoi)
        double precision a10ancub(npoi)
        double precision a10td(npoi)
        double precision a11soiltd(npoi)
        double precision a3tdmin(npoi)
        double precision agddl(npoi)
        double precision agddu(npoi)
        double precision daylength(npoi)
        double precision dropl3(npoi)
        double precision dropl4(npoi)
        double precision dropls(npoi)
        double precision dropu(npoi)
        double precision fl(npoi)
        double precision fu(npoi)
        double precision greenfracl(npoi)
        double precision greenfracl3(npoi)
        double precision greenfracl4(npoi)
        double precision icropsum(npoi)

c
c define 'drop days' -- number of days to affect phenology change
c
      ddays = 15.0
      ddfac = 1.0 / ddays


	vegscheme = 1

	if(vegscheme.eq.1 ) then  !new - Chris


c
c soil temperature threshold for budburst
c soil temperature threshold is assumed to be 0 degrees C
c
      sthreshold = 273.16
c
c initialize this year's values of stsumu, stsuml, precipsum
c
      if (jday.eq.1) then
c
        call const (stsumu, npoi, 0.)
        call const (stsuml, npoi, 0.)
        call const (precipsum, npoi, 0.)
        call logicf (onflagu, npoi)
        call logicf (onflagl3, npoi)
        call logicf (onflagl4, npoi)
        call logict (offflagu, npoi)
        call logict (offflagl3, npoi)
        call logict (offflagl4, npoi)
c
      endif
c
c begin global grid
c
      do 100 i = 1, npoi
c
c only do if crops are not growing
c
        if (icropsum(i) .eq. 0.0) then
c
c ---------------------------------------------------------------------
c * * * upper canopy onset phenology * * *
c ---------------------------------------------------------------------
c
        if ((offflagu(i)) .AND. (jday .lt. 243)) then
c
c sumcom is the threshold for upper canopy onset
c
          sumcom(i) = EXP(4.395 + 0.129 * Tavgann(i))
c
c determine if soil temperature summation is initiated
c if so, calculate onset summation stsumu and stsuml
c
          if (a11soiltd(i).lt.sthreshold) then
            stsumu(i)  = stsumu(i)
          else
            stsumu(i) = stsumu(i) + a11soiltd(i) - sthreshold
          endif
c
c determine if onset has occured
c
          if (stsumu(i).ge.sumcom(i)) then
            onflagu(i) = .TRUE.
            offflagu(i) = .FALSE.
          endif
c
        endif
c
c if onset has occured then determine leaf display
c
        if (onflagu(i)) then
          tempu(i) = min (1., tempu(i) + ddfac)
        endif
c
c ---------------------------------------------------------------------
c * * * upper canopy offset phenology * * *
c ---------------------------------------------------------------------
c
        if (onflagu(i)) then
          if (jday .ge. 243) then
            if (((daylength(i).le.685.) .AND. (a11soiltd(i).le.(17.15+273.16)))
     >        .OR. (a11soiltd(i).le.(2.+273.16))) then
c
              offflagu(i) = .TRUE.
              onflagu(i) = .FALSE.
c
            endif
          endif
        endif
c
c if offset has occured then determine leaf display
c
        if (offflagu(i)) then
          tempu(i) = max (0., tempu(i) - ddfac)
        endif
c
c ---------------------------------------------------------------------
c * * * lower canopy onset phenology * * *
c ---------------------------------------------------------------------
c
c C3 and C4 grasses in the lower canopy act a little different than
c the trees found above. These grasses keep their leaves all year so
c their LAI is constant and they do not have a temp factor in the following
c algorithm. Instead, leaves are either green (living during the growing
c season) or brown (dead during the winter). Greenness is determined by
c the terms greenfracl3, for C3 grasses, or greenfracl4, for C4 grasses.
c The greenfrac terms act similarly to the temp factors. When onset occurs,
c greenfrac increases from 0 to 1 (all brown to all green leaves). When
c offset occurs, greenfrac decreases from 1 to 0.
c Greenfracl3 and greenfracl4 are then passed to stomata (physiology.f)
c and twoset (radiation.f) where they affect photosynthesis and leaf optical
c properties, respectively
c
        if ((offflagl3(i) .OR. offflagl4(i)) .AND. (jday .lt. 243)) then
c
c accumulate precipitation summation
c
          precipsum(i) = precipsum(i) + precip(i)
          PPTsumcrit(i) = 0.15 * PPTavgann(i)
c
c determine if soil temperature summation is initiated
c if so, calculate onset summation
c
          if (a11soiltd(i).lt.sthreshold) then
            stsuml(i)  = stsuml(i)
          else
            stsuml(i) = stsuml(i) + a11soiltd(i) - sthreshold
          endif
c
c
c for warm grasses, summation threshold is 1320
c for cool grasses, summation threshold is 420
c
          if (precipsum(i) .ge. PPTsumcrit(i)) then
            if (stsuml(i) .ge. 420.) then
              onflagl3(i) = .TRUE.
              offflagl3(i) = .FALSE.
            endif
            if (stsuml(i) .ge. 1320.) then
              onflagl4(i) = .TRUE.
              offflagl4(i) = .FALSE.
            endif
          endif
        endif
c
c if onset has occured then determine leaf color
c templs is retained so that deciduous shrubs may lose their leaves
c
        if (onflagl3(i)) then
           greenfracl3(i) = min (1., greenfracl3(i) + ddfac)
        endif
        if (onflagl4(i)) then
           greenfracl4(i) = min (1., greenfracl4(i) + ddfac)
        endif
c
c ---------------------------------------------------------------------
c * * * lower canopy offset phenology * * *
c ---------------------------------------------------------------------
c
c This is tuned White et al. version that looks at the stress of the plants
c and determines if 'cold' conditions are met
c
        if (onflagl3(i) .OR. onflagl4(i)) then
          if (jday .ge. 243) then
            if ((stresstl(i) .lt. 0.27) .OR.
     >        ((a3tdmin(i)-273.16).le.tminavgann(i))) then
c
              offflagl3(i) = .TRUE.
              offflagl4(i) = .TRUE.
              onflagl3(i) = .FALSE.
              onflagl4(i) = .FALSE.
c
            endif
          endif
        endif
c
c if offset has occured then determine leaf display
c
        if (offflagl3(i)) then
           greenfracl3(i) = max (0., greenfracl3(i) - ddfac)
        endif
c
        if (offflagl4(i)) then
           greenfracl4(i) = max (0., greenfracl4(i) - ddfac)
        endif
c
c ---------------------------------------------------------------------
c * * * update lai and canopy fractions * * *
c ---------------------------------------------------------------------
c
c Here the leaf display of shrubs, templs, is set equal to that of
c trees, tempu, since it was determined that even though shrubs are
c in the lower canopy, their leaf display follows more closely that
c of trees than grasses.
c
        templs(i) = tempu(i)
c
c upper canopy single sided leaf area index (area-weighted)
c
        avglaiu = plai(i,1)             +
     >            plai(i,2)             +
     >            plai(i,3)             +
     >            plai(i,4)             +
     >            plai(i,5) * tempu(i)  +
     >            plai(i,6)             +
     >            plai(i,7) * tempu(i)  +
     >            plai(i,8) * tempu(i)
c
c upper canopy fractions
c
        frac(i,1) = plai(i,1)            / max (avglaiu, epsilon)
        frac(i,2) = plai(i,2)            / max (avglaiu, epsilon)
        frac(i,3) = plai(i,3)            / max (avglaiu, epsilon)
        frac(i,4) = plai(i,4)            / max (avglaiu, epsilon)
        frac(i,5) = plai(i,5) * tempu(i) / max (avglaiu, epsilon)
        frac(i,6) = plai(i,6)            / max (avglaiu, epsilon)
        frac(i,7) = plai(i,7) * tempu(i) / max (avglaiu, epsilon)
        frac(i,8) = plai(i,8) * tempu(i) / max (avglaiu, epsilon)
c
c lower canopy single sided leaf area index (area-weighted)
c
        avglail = plai(i,9)                  +
     >            plai(i,10) *     templs(i) +
     >            plai(i,11)                 +
     >            plai(i,12)
c
c lower canopy fractions
c templs is included in frac(i,10) to allow
c deciduous shrubs to drop their leaves
c All other pfts keep leaves year-round
c
        frac(i,9)  = plai(i,9)                        /
     >               max (avglail, epsilon)
c
        frac(i,10) = plai(i,10) * templs(i)           /
     >               max (avglail, epsilon)
c
        frac(i,11) = plai(i,11)                        /
     >               max (avglail, epsilon)
c
        frac(i,12) = plai(i,12)                        /
     >               max (avglail, epsilon)


c
c Find an average fraction of green vegetation in the lower canopy
c to be used in stomata and twoset subroutines
c
        greenfracl(i) = frac(i,9) + frac(i,10)                  +
     >                              greenfracl4(i) * frac(i,11) +
     >                              greenfracl3(i) * frac(i,12)
c
c calculate the canopy leaf area index using the fractional vegetation cover
c
csant - Ive included this fl update here - as LAI is updated every day.

        avglaiu = max (0.025, avglaiu)
        fu(i) = avglaiu  / 1.0
        fu(i) = max (0.025, min (0.975, fu(i)))

        avglail = max (0.025, avglail)
        fl(i) = avglail  / 1.0
        fl(i) = max (0.025, min (0.975, fl(i)))

csant - end of modifications

        lai(i,1) = avglail / fl(i)
        lai(i,2) = avglaiu / fu(i)
c
c put a fix on canopy lais to avoid problems in physics
c

        lai(i,1) = max(0.025, min (lai(i,1), 12.0) )
        lai(i,2) = max(0.025, min (lai(i,2), 12.0) )

c-original        lai(i,1) = min (lai(i,1), 12.0) 
c-original        lai(i,2) = min (lai(i,2), 12.0) 


csan- orignal        lai(i,1) = min (lai(i,1), 12.0) 
c        lai(i,2) = min (lai(i,2), 12.0) 
csant - original        fl(i) = max (0.25, min (0.975, fl(i)))

c	print*,i,jday,'in Pheno- fl(i) = ',fl(i),' lai ',lai(i,1)
c
c ---------------------------------------------------------------------
c * * * update canopy height parameters * * *
c ---------------------------------------------------------------------
c
c update lower canopy height parameters
c
c note that they are based on vegetation fraction and not
c averaged over the entire gridcell
c
        zbot(i,1)   =  0.05
        ztop(i,1)   =  max (0.25, lai(i,1) * 0.25)
c        
c constrain ztop to be at least 0.5 meter lower than 
c zbot for upper canopy
c
        ztop(i,1) = min (ztop(i,1), zbot(i,2) - 0.5)

c	if(i.eq.12)print*,'vegetation.f  ',fl(i),lai(i,1),fu(i),lai(i,2)
c
c
      endif ! crop existence check 
c end of loop
c
 100  continue
	

c*******************************************************************
	else  !between new (above) and old (below) schemes
c*******************************************************************

c
c begin global grid
c
      do 101 i = 1, npoi
c
c ---------------------------------------------------------------------
c * * * upper canopy winter phenology * * *
c ---------------------------------------------------------------------
c
c temperature threshold for budburst and senescence
c
c temperature threshold is assumed to be 0 degrees C 
c or 5 degrees warmer than the coldest monthly temperature
c
        tthreshold = max (0.0         + 273.16,
     >                    tc(i) + 5.0 + 273.16)
c
c gdd threshold temperature for leaf budburst
c with a growing degree threshold of 100 units
c
        gthreshold = 0.0 + 273.16
c 
c determine if growing degree days are initiated
c
        if (a10td(i).lt.gthreshold) then
          agddu(i)  = 0.0
        else
          agddu(i) = agddu(i) + td(i) - gthreshold
        endif
c
c determine leaf display
c
        if (a10td(i).lt.tthreshold) then
          tempu(i)  = max (0.0, tempu(i) - ddfac)
        else
          tempu(i) = min (1., max (0.0, agddu(i) - 100.0) / 50.0)
        endif
csant	print*,	tempu(i),agddu(i),a10td(i)-273,tthreshold-273
c
c ---------------------------------------------------------------------
c * * * lower canopy winter phenology * * *
c ---------------------------------------------------------------------
c
c temperature threshold for budburst and senescence
c
c temperature threshold is assumed to be 0 degrees C 
c
        tthreshold = 0.0 + 273.16
c
c gdd threshold temperature for leaf budburst
c with a growing degree threshold of 150 units
c
        gthreshold = -5.0 + 273.16
c 
c determine if growing degree days are initiated
c
        if (a10td(i).lt.gthreshold) then
          agddl(i)  = 0.0
        else
          agddl(i) = agddl(i) + td(i) - gthreshold
        endif
c
c determine leaf display
c
        if (a10td(i).lt.tthreshold) then
          templ(i)  = max (0.0, templ(i) - ddfac)
        else
          templ(i) = min (1., max (0.0, agddl(i) - 150.0) / 50.0)
        endif
c
c ---------------------------------------------------------------------
c * * * drought canopy winter phenology * * *
c ---------------------------------------------------------------------
c
        if (a10ancub(i).lt.0.0) dropu(i) = max (0.1, dropu(i) - ddfac)
        if (a10ancub(i).ge.0.0) dropu(i) = min (1.0, dropu(i) + ddfac)
c
        if (a10ancls(i).lt.0.0) dropls(i) = max (0.1, dropls(i) - ddfac)
        if (a10ancls(i).ge.0.0) dropls(i) = min (1.0, dropls(i) + ddfac)
c
        if (a10ancl4(i).lt.0.0) dropl4(i) = max (0.1, dropl4(i) - ddfac)
        if (a10ancl4(i).ge.0.0) dropl4(i) = min (1.0, dropl4(i) + ddfac)
c
        if (a10ancl3(i).lt.0.0) dropl3(i) = max (0.1, dropl3(i) - ddfac)
        if (a10ancl3(i).ge.0.0) dropl3(i) = min (1.0, dropl3(i) + ddfac)
c
c ---------------------------------------------------------------------
c * * * update lai and canopy fractions * * *
c ---------------------------------------------------------------------
c
c upper canopy single sided leaf area index (area-weighted)
c
        avglaiu = plai(i,1)             +
     >            plai(i,2) * dropu(i)  +
     >            plai(i,3)             +
     >            plai(i,4)             +
     >            plai(i,5) * tempu(i)  +
     >            plai(i,6)             +
     >            plai(i,7) * tempu(i)  +
     >            plai(i,8) * tempu(i)
c
c upper canopy fractions
c
        frac(i,1) = plai(i,1)            / max (avglaiu, epsilon)
        frac(i,2) = plai(i,2) * dropu(i) / max (avglaiu, epsilon)
        frac(i,3) = plai(i,3)            / max (avglaiu, epsilon)
        frac(i,4) = plai(i,4)            / max (avglaiu, epsilon)
        frac(i,5) = plai(i,5) * tempu(i) / max (avglaiu, epsilon)
        frac(i,6) = plai(i,6)            / max (avglaiu, epsilon)
        frac(i,7) = plai(i,7) * tempu(i) / max (avglaiu, epsilon)
        frac(i,8) = plai(i,8) * tempu(i) / max (avglaiu, epsilon)
c
c lower canopy single sided leaf area index (area-weighted)
c
        avglail = plai(i,9)                              +
     >            plai(i,10) * min (templ(i), dropls(i)) +
     >            plai(i,11) * min (templ(i), dropl4(i)) +
     >            plai(i,12) * min (templ(i), dropl3(i))
c
c lower canopy fractions
c
        frac(i,9)  = plai(i,9)                              /
     >               max (avglail, epsilon)
c
        frac(i,10) = plai(i,10) * min (templ(i), dropls(i)) /
     >               max (avglail, epsilon)
c
        frac(i,11) = plai(i,11) * min (templ(i), dropl4(i)) /
     >               max (avglail, epsilon)
c
        frac(i,12) = plai(i,12) * min (templ(i), dropl3(i)) /
     >               max (avglail, epsilon)

csant - original version (Foley) - doesnot have this green 
c Find an average fraction of green vegetation in the lower canopy
c to be used in stomata and twoset subroutines
c
	greenfracl4(i)= 1.0 
	greenfracl3(i)= 1.0

        greenfracl(i) = frac(i,9) + frac(i,10)                  +
     >                              greenfracl4(i) * frac(i,11) +
     >                              greenfracl3(i) * frac(i,12)


c
c calculate the canopy leaf area index using the fractional vegetation cover
c
        lai(i,1) = avglail / fl(i)

**** DTP 2000/06/28 Modified this following discussion with Navin. We now
*    retain fu(i) derived from dynaveg and constrain it to a local value
*    "fu_phys" in the range 0.25 to 0.975 used in the canopy physics calcs.

        fu_phys = max (0.25, min (0.975, fu(i)))
        lai(i,2) = avglaiu / fu_phys
csant        lai(i,2) = avglaiu / fu(i)
c
c put a fix on canopy lais to avoid problems in physics
c
csant- original        lai(i,1) = min (lai(i,1), 12.0)
csant- original        lai(i,2) = min (lai(i,2), 12.0)
        lai(i,1) = max(0.05, min (lai(i,1), 12.0) )
        lai(i,2) = max(0.05, min (lai(i,2), 12.0) )
c
c ---------------------------------------------------------------------
c * * * update canopy height parameters * * *
c ---------------------------------------------------------------------
c
c update lower canopy height parameters
c
c note that they are based on vegetation fraction and not
c averaged over the entire gridcell
c
        zbot(i,1)   =  0.05
        ztop(i,1)   =  max (0.25, lai(i,1) * 0.25)
c        
c constrain ztop to be at least 0.5 meter lower than 
c zbot for upper canopy
c
        ztop(i,1) = min (ztop(i,1), zbot(i,2) - 0.5)

c
c end of loop
c
 101  continue

	endif

c
c return to main program
c 
      return
      end


        subroutine dynavegWrapper(local_params, dim_params, double_params,
     >                            npoi_matrix,
     >                            npft_matrix,
     >                            zbot, 
     >                            ztop, 
     >                            ayanpp, 
     >                            aynpp, 
     >                            biomass, 
     >                            cbiol, 
     >                            cbior, 
     >                            cbiow, 
     >                            exist, 
     >                            plai, 
     >                            sai) 


                integer          local_params
                integer          dim_params(4)
                double precision double_params(2)

                double precision zbot(dim_params(1),2)
                double precision ztop(dim_params(1),2)
                double precision ayanpp(dim_params(1),dim_params(2))
                double precision aynpp(dim_params(1),dim_params(2))
                double precision biomass(dim_params(1),dim_params(2))
                double precision cbiol(dim_params(1), dim_params(2))
                double precision cbior(dim_params(1),dim_params(2))
                double precision cbiow(dim_params(1),dim_params(2))
                double precision exist(dim_params(1),dim_params(2))
                double precision plai(dim_params(1), dim_params(2))
                double precision sai(dim_params(1),2)

                double precision npoi_matrix(21, dim_params(1))

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

                double precision npft_matrix(8, dim_params(2))

                double precision npft_line1(dim_params(2))
                double precision npft_line2(dim_params(2))
                double precision npft_line3(dim_params(2))
                double precision npft_line4(dim_params(2))
                double precision npft_line5(dim_params(2))
                double precision npft_line6(dim_params(2))
                double precision npft_line7(dim_params(2))
                double precision npft_line8(dim_params(2))

                integer i

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
102             CONTINUE

                DO 101, i = 1, dim_params(1), 1
                        npft_line1(i) = npft_matrix(1, i)
                        npft_line2(i) = npft_matrix(2, i)
                        npft_line3(i) = npft_matrix(3, i)
                        npft_line4(i) = npft_matrix(4, i)
                        npft_line5(i) = npft_matrix(5, i)
                        npft_line6(i) = npft_matrix(6, i)
                        npft_line7(i) = npft_matrix(7, i)
                        npft_line8(i) = npft_matrix(8, i)
101             CONTINUE

                call dynaveg(local_params, dim_params(1), dim_params(2), dim_params(3), dim_params(4),
     >                       double_params(1), double_params(2), 
     >                       zbot, !others
     >                       ztop, 
     >                       ayanpp, 
     >                       aynpp, 
     >                       biomass, 
     >                       cbiol, 
     >                       cbior, 
     >                       cbiow, 
     >                       exist, 
     >                       plai, 
     >                       sai,       
     >                       npoi_line1, !npoi vector
     >                       npoi_line2,
     >                       npoi_line3,
     >                       npoi_line4,
     >                       npoi_line5,
     >                       npoi_line6,
     >                       npoi_line7,
     >                       npoi_line8,
     >                       npoi_line9,
     >                       npoi_line10,
     >                       npoi_line11,
     >                       npoi_line12,
     >                       npoi_line13,
     >                       npoi_line14,
     >                       npoi_line15,
     >                       npoi_line16,
     >                       npoi_line17,
     >                       npoi_line18,
     >                       npoi_line19,
     >                       npoi_line20,
     >                       npoi_line21,          
     >                       npft_line1,
     >                       npft_line2,
     >                       npft_line3,
     >                       npft_line4,
     >                       npft_line5,
     >                       npft_line6,
     >                       npft_line7,
     >                       npft_line8)

c               RETORNANDO VALORES PARA VARIAVEIS

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
777             CONTINUE

                DO 555, i = 1, dim_params(1), 1
                        npft_matrix(1, i) = npft_line1(i)
                        npft_matrix(2, i) = npft_line2(i)
                        npft_matrix(3, i) = npft_line3(i)
                        npft_matrix(4, i) = npft_line4(i)
                        npft_matrix(5, i) = npft_line5(i)
                        npft_matrix(6, i) = npft_line6(i)
                        npft_matrix(7, i) = npft_line7(i)
                        npft_matrix(8, i) = npft_line8(i)
555             CONTINUE


        end
        



c JAIR: rotina2 - OK!
c TODO: Verificar se real*4 altera algo ao chamar a rotina do R
c
c
c ---------------------------------------------------------------------
      subroutine dynaveg (isimfire,
     >                    npoi,
     >                    npft,
     >                    ecpft,
     >                    scpft,
     >                    woodnorm,
     >                    xminlai,
     >                    zbot, !others
     >                    ztop, 
     >                    ayanpp, 
     >                    aynpp, 
     >                    biomass, 
     >                    cbiol, 
     >                    cbior, 
     >                    cbiow, 
     >                    exist, 
     >                    plai, 
     >                    sai, 
     >                    ayanpptot, !npoi vectors
     >                    ayneetot, 
     >                    cdisturb, 
     >                    disturbf, 
     >                    disturbo, 
     >                    falll, 
     >                    fallr, 
     >                    fallw, 
     >                    fl, 
     >                    fu, 
     >                    icropsum,
     >                    sapfrac, 
     >                    totbiol, 
     >                    totbiou, 
     >                    totlail, 
     >                    totlaiu,
     >                    firefac,
     >                    totlit,
     >                    gdd0,
     >                    gdd5,
     >                    vegtype0,
     >                    specla,  ! npft vectors
     >                    tauleaf, 
     >                    tauroot, 
     >                    tauwood, 
     >                    tauwood0, 
     >                    aleaf, 
     >                    aroot, 
     >                    awood)
c ---------------------------------------------------------------------
c
      implicit none
c
c      include 'compar.h'
c      include 'comsoi.h'
c      include 'comsum.h'
c      include 'comveg.h'
c      include 'compft.h'
c      include 'comcrop.h'
c
c Arguments
c
      integer isimfire   ! fire switch
c
c local variables
c
      integer
     >  i, j           ! gridcell counter
c
      real
     >  sapspeed,      ! in mm/day
     >  trans,         ! (2.5 mm/day) 
     >  saparea,       ! in m**2
     >  sapvolume,     ! in m**3
     >  denswood,      ! kg/m**3
     >  wood,          ! total amount of woody biomass in gridcell
     >  taufin         !


c global variables

        integer          npft
        integer          npoi
        integer          ecpft
        integer          scpft

        double precision woodnorm
        double precision xminlai

        !others
        double precision zbot(npoi,2)
        double precision ztop(npoi,2)
        double precision ayanpp(npoi,npft)
        double precision aynpp(npoi,npft)
        double precision biomass(npoi,npft)
        double precision cbiol(npoi,npft)
        double precision cbior(npoi,npft)
        double precision cbiow(npoi,npft)
        double precision exist(npoi,npft)
        double precision plai(npoi,npft)
        double precision sai(npoi,2)

        
        !npoi vectors
        double precision ayanpptot(npoi)
        double precision ayneetot(npoi)
        double precision cdisturb(npoi)
        double precision disturbf(npoi)
        double precision disturbo(npoi)
        double precision falll(npoi)
        double precision fallr(npoi)
        double precision fallw(npoi)
        double precision fl(npoi)
        double precision fu(npoi)
        double precision icropsum(npoi)
        double precision sapfrac(npoi)
        double precision totbiol(npoi)
        double precision totbiou(npoi)
        double precision totlail(npoi)
        double precision totlaiu(npoi)
        double precision firefac(npoi)
        double precision totlit(npoi)
        double precision gdd0(npoi)
        double precision gdd5(npoi)
        double precision vegtype0(npoi)

        ! npft vectors
        double precision specla(npft)
        double precision tauleaf(npft)
        double precision tauroot(npft)
        double precision tauwood(npft)
        double precision tauwood0(npft)
        double precision aleaf(npft)
        double precision aroot(npft)
        double precision awood(npft)

        


*    >  xminlai        !
c
c      real
c     >  aleaf(npft),   ! allocation fraction to leaves
c     >  aroot(npft),   ! allocation fraction to fine roots
c     >  awood(npft),   ! allocation fraction to wood
c     >  tauleaf(npft), ! turnover time of carbon in leaves (years)
c     >  tauroot(npft), ! turnover time of carbon in fine roots (years)
c     >  tauwood(npft), ! turnover time of carbon in wood (years)
c     >  tauwood0(npft) ! normal (unstressed) turnover time
c
c ibis uses a small number of plant functional types:
c
c  1: tropical broadleaf evergreen tree
c  2: tropical broadleaf drought-deciduous trees
c  3: warm-temperate broadleaf evergreen tree
c  4: temperate conifer evergreen tree
c  5: temperate broadleaf cold-deciduous tree
c  6: boreal conifer evergreen tree
c  7: boreal broadleaf cold-deciduous tree
c  8: boreal conifer cold-deciduous tree
c  9: evergreen shrub
c 10: deciduous shrub
c 11: warm (c4) grass
c 12: cool (c3) grass
c 13: soybeans
c 14: maize 
c 15: spring and winter wheat
c 16: sugarcane
c
c ---------------------------------------------------------------------
c * * * specify biomass turnover parameters (years) * * *
c ---------------------------------------------------------------------
c
*      data tauleaf / 1.01,   ! tropical broadleaf evergreen trees
*     >               1.00,   ! tropical broadleaf drought-deciduous trees
*     >               1.00,   ! warm-temperate broadleaf evergreen trees
*     >               2.00,   ! temperate conifer evergreen trees
*     >               1.00,   ! temperate broadleaf cold-deciduous trees
*     >               2.50,   ! boreal conifer evergreen trees
*     >               1.00,   ! boreal broadleaf cold-deciduous trees
*     >               1.00,   ! boreal conifer cold-deciduous trees
*     >               1.50,   ! evergreen shrubs
*     >               1.00,   ! deciduous shrubs
*     >               1.25,   ! warm (c4) grasses
*     >               1.50,   ! cool (c3) grasses
*     >               999.0,  ! soybean
*     >               999.0,  ! maize 
*     >               999.0 / ! wheat
c
*      data tauwood0 / 25.0,  ! tropical broadleaf evergreen trees
*     >                25.0,  ! tropical broadleaf drought-deciduous trees
*     >                25.0,  ! warm-temperate broadleaf evergreen trees
*     >                50.0,  ! temperate conifer evergreen trees
*     >                50.0,  ! temperate broadleaf cold-deciduous trees
*     >               100.0,  ! boreal conifer evergreen trees
*     >               100.0,  ! boreal broadleaf cold-deciduous trees
*     >               100.0,  ! boreal conifer cold-deciduous trees
*     >                 5.0,  ! evergreen shrubs
*     >                 5.0,  ! deciduous shrubs
*     >               999.0,  ! warm (c4) grasses
*     >               999.0,  ! cool (c3) grasses
*     >               999.0,  ! soybean
*     >               999.0,  ! maize 
*     >               999.0 / ! wheat 
c
c begin global grid
c
      do 100 i = 1, npoi
c
        if (icropsum(i) .eq. 0.0) then
c
c ---------------------------------------------------------------------
c * * * initialize vegetation dynamics pools * * *
c ---------------------------------------------------------------------
c
c zero out litter fall fields
c
        falll(i) = 0.0
        fallr(i) = 0.0
        fallw(i) = 0.0
c
c zero out carbon lost due to disturbance
c 
        cdisturb(i) = 0.0
c
        wood = 0.001
c
c ---------------------------------------------------------------------
c * * * update npp, and pool losses  * * *
c ---------------------------------------------------------------------
c
c go through all the pfts
c
        do 110 j = 1, npft
c
c apply this year's existence arrays to npp
c
          aynpp(i,j)  = exist(i,j) * aynpp(i,j)
c
c determine above-ground npp for each plant type
c
          ayanpp(i,j) = (aleaf(j) + awood(j)) * aynpp(i,j)
c
c determine turnover rates for woody biomass:
c
c if pft can exist,    then tauwood = tauwood0 (normal turnover),
c if pft cannot exist, then tauwood = taufin years (to kill off trees)
c
c          taufin     = 5.0
           taufin     = tauwood0(j)/2.0
c
          tauwood(j) = tauwood0(j) - (tauwood0(j) - taufin) *
     >                               (1.0 - exist(i,j))
c
c assume a constant fine root turnover time
c
          tauroot(j) = 1.0
c
c determine litter fall rates
c
          falll(i) = falll(i) + cbiol(i,j) / tauleaf(j)
          fallr(i) = fallr(i) + cbior(i,j) / tauroot(j)
          fallw(i) = fallw(i) + cbiow(i,j) / tauwood(j)
c
c ---------------------------------------------------------------------
c * * * update biomass pools  * * *
c ---------------------------------------------------------------------
c
c update carbon reservoirs using an analytical solution
c to the original carbon balance differential equation
c
          cbiol(i,j) = cbiol(i,j) * exp(-1./tauleaf(j))  +
     >                 aleaf(j) * tauleaf(j) * max (0., aynpp(i,j)) *
     >                 (1. - exp(-1./tauleaf(j)))
c
          cbiow(i,j) = cbiow(i,j) * exp(-1./tauwood(j))  +
     >                 awood(j) * tauwood(j) * max (0., aynpp(i,j)) *
     >                 (1. - exp(-1./tauwood(j)))
c
          cbior(i,j) = cbior(i,j) * exp(-1./tauroot(j))  +
     >                 aroot(j) * tauroot(j) * max (0., aynpp(i,j)) *
     >                 (1. - exp(-1./tauroot(j)))
c
          if (j.le.8) wood = wood + max (0.0, cbiow(i,j))
c
 110    continue
c
c ---------------------------------------------------------------------
c * * * apply disturbances * * *
c ---------------------------------------------------------------------
c
c set fixed disturbance regime
c
        disturbf(i) = 0.005
        disturbo(i) = 0.005
c
c call fire disturbance routine
c
        if (isimfire.eq.1) call fire(npoi, disturbf, firefac, totlit)
        
c
        do 120 j = 1, npft
c
c calculate biomass (vegetations) carbon lost to atmosphere   
c used to balance net ecosystem exchange  
c
          cdisturb(i) = cdisturb(i) + 
     >                  cbiol(i,j) * (disturbf(i) + disturbo(i)) +
     >                  cbiow(i,j) * (disturbf(i) + disturbo(i)) +
     >                  cbior(i,j) * (disturbf(i) + disturbo(i))                  
c          
c adjust biomass pools due to disturbances
c
          cbiol(i,j) = cbiol(i,j) * (1. - disturbf(i) - disturbo(i))
          cbiow(i,j) = cbiow(i,j) * (1. - disturbf(i) - disturbo(i))
          cbior(i,j) = cbior(i,j) * (1. - disturbf(i) - disturbo(i))
c
c constrain biomass fields to be positive
c
          cbiol(i,j) = max (0.0, cbiol(i,j))
          cbiow(i,j) = max (0.0, cbiow(i,j))
          cbior(i,j) = max (0.0, cbior(i,j))
c
c maintain minimum value of leaf carbon in areas that plants exist
c
c         xminlai = 0.010
c
          cbiol(i,j) = max (exist(i,j) * xminlai / specla(j),
     >                      cbiol(i,j))
c
c update vegetation's physical characteristics
c
          plai(i,j)    = cbiol(i,j) * specla(j)
          biomass(i,j) = cbiol(i,j) + cbiow(i,j) + cbior(i,j)
c
 120    continue
c
c ---------------------------------------------------------------------
c * * * update annual npp, lai, and biomass * * *
c ---------------------------------------------------------------------
c
c adjust annual net ecosystem exchange (calculated in stats.f) 
c by loss of carbon to atmosphere due to biomass burning (fire)
c
        ayneetot(i) = ayneetot(i) - cdisturb(i)
c
c determine total ecosystem above-ground npp
c
        ayanpptot(i) = ayanpp(i,1)  + ayanpp(i,2) +
     >                 ayanpp(i,3)  + ayanpp(i,4) +
     >                 ayanpp(i,5)  + ayanpp(i,6) +
     >                 ayanpp(i,7)  + ayanpp(i,8) +
     >                 ayanpp(i,9)  + ayanpp(i,10) +
     >                 ayanpp(i,11) + ayanpp(i,12)
c
c update total canopy leaf area
c
        totlaiu(i) = plai(i,1)  + plai(i,2) +
     >               plai(i,3)  + plai(i,4) +
     >               plai(i,5)  + plai(i,6) +
     >               plai(i,7)  + plai(i,8)
c
        totlail(i) = plai(i,9)  + plai(i,10) +
     >               plai(i,11) + plai(i,12)
c
c update total biomass
c
        totbiou(i) = biomass(i,1) +
     >               biomass(i,2) +
     >               biomass(i,3) +
     >               biomass(i,4) +
     >               biomass(i,5) +
     >               biomass(i,6) +
     >               biomass(i,7) +
     >               biomass(i,8)
c
        totbiol(i) = biomass(i,9)  +
     >               biomass(i,10) +
     >               biomass(i,11) +
     >               biomass(i,12)
c
c ---------------------------------------------------------------------
c * * * update fractional cover and vegetation height parameters * * *
c ---------------------------------------------------------------------
c
c update fractional cover of forest and herbaceous canopies:
c 
csant - changed in 2010
	 totlaiu(i)=max(0.25,totlaiu(i))
	 totlail(i)=max(0.25,totlail(i))


        fu(i) = (1.0 - exp(-wood)) / (1.0 - exp(-woodnorm))
c
        fl(i) = totlail(i) / 1.0
c
c apply disturbances to fractional cover
c
        fu(i) = fu(i) * (1. - disturbf(i) - disturbo(i))
        fl(i) = fl(i) * (1. - disturbf(i) - disturbo(i))
c
c constrain the fractional cover
c
        fu(i) = max (0.025, min (0.975, fu(i)))
csant - original        fl(i) = max (0.25, min (0.975, fl(i)))
        fl(i) = max (0.025, min (0.975, fl(i)))

c	print*,i,'Lower canopy cover - fl(i) = ',fl(i)
c
c annual update upper canopy height parameters
c should be calculated based on vegetative fraction and not the
c average over the entire grid cell
c
        zbot(i,2) = 3.0
        ztop(i,2) = max(zbot(i,2) + 1.00, 2.50 *
     >                  totbiou(i) / fu(i) * 0.75)
c
c ---------------------------------------------------------------------
c * * * update stem area index and sapwood fraction * * *
c ---------------------------------------------------------------------
c
c estimate stem area index (sai) as a fraction of the lai
c
csan-original        sai(i,1) = 0.050 * totlail(i)
csan-original        sai(i,2) = 0.250 * totlaiu(i)

        sai(i,1) = max(0.05,0.050 * totlail(i))
        sai(i,2) = max(0.25,0.250 * totlaiu(i))

c
c estimate sapwood fraction of woody biomass
c
        sapspeed  = 25.0                        ! (m/day)
        trans     = 0.0025                      ! (2.5 mm/day) 
        saparea   = (trans / sapspeed)          ! m**2
c
        sapvolume = saparea * ztop(i,2) * 0.75  ! m**3
c
        denswood  = 400.0                       ! kg/m**3
c
        sapfrac(i) = min (0.50, max (0.05, sapvolume * denswood / wood))
c
       endif  ! check for crop existence 
c
 100  continue
c
c ---------------------------------------------------------------------
c * * * map out vegetation classes for this year * * *
c ---------------------------------------------------------------------
c
      call vegmap(npoi, npft,
     >            biomass,
     >            ecpft,
     >            gdd0,
     >            gdd5,
     >            plai,
     >            scpft,
     >            totlail,
     >            totlaiu,
     >            vegtype0)
c
c return to the main program
c
      return
      end


c JAIR: rotina3 - OK
c
c
c ---------------------------------------------------------------------
      subroutine fire(npoi,
     >                disturbf,
     >                firefac,
     >                totlit)
c ---------------------------------------------------------------------
c
      implicit none
c
c      include 'compar.h'
c      include 'comveg.h'
c
c local variables
c
      integer i
c
      real burn

c global variables
      
      integer  npoi
 
      double precision disturbf(npoi)
      double precision firefac(npoi)
      double precision totlit(npoi)

c
c begin global grid
c
      do 100 i = 1, npoi
c
        burn = firefac(i) * min (1.0, totlit(i) / 0.200)
c
        disturbf(i) = 1.0 - exp(-0.5 * burn)
c
        disturbf(i) = max (0.0, min (1.0, disturbf(i)))
c
 100  continue
c
      return
      end


c JAIR: rotina4 - OK

c
c ---------------------------------------------------------------------
         subroutine vegmap(npoi, npft,
     >                    biomass,
     >                    ecpft,
     >                    gdd0,
     >                    gdd5,
     >                    plai,
     >                    scpft,
     >                    totlail,
     >                    totlaiu,
     >                    vegtype0)
c ---------------------------------------------------------------------
c
      implicit none
c
c      include 'compar.h'
c      include 'comveg.h'
c
c local variables
c
      integer 
     >     i, j,            ! loop indice
     >     domtree          ! dominant tree
c
      real maxlai,          ! maximum lai
     >     totlai,          ! total ecosystem lai
     >     grassfrac,       ! fraction of total lai in grasses
     >     treefrac,        ! fraction of total lai in trees
     >     treelai,         ! lai of trees
     >     shrublai,        ! lai of shrubs
     >     grasslai,        ! lai of grass
     >     ratio,
     >     cropbio

c global variables
         integer  npoi, npft
         double precision biomass(npoi, npft)
         integer  ecpft
         double precision gdd0(npoi)
         double precision gdd5(npoi)
         double precision plai(npoi,npft)
         integer  scpft
         double precision totlail(npoi)
         double precision totlaiu(npoi)
         double precision vegtype0(npoi)

c
c classify vegetation cover into standard ibis vegetation classes 
c
c ---------------------------------------------------
c  1: tropical evergreen forest / woodland
c  2: tropical deciduous forest / woodland
c  3: temperate evergreen broadleaf forest / woodland
c  4: temperate evergreen conifer forest / woodland
c  5: temperate deciduous forest / woodland
c  6: boreal evergreen forest / woodland
c  7: boreal deciduous forest / woodland
c  8: mixed forest / woodland
c  9: savanna
c 10: grassland / steppe 
c 11: dense shrubland
c 12: open shrubland
c 13: tundra
c 14: desert 
c 15: polar desert / rock / ice
c 16: croplands
c ---------------------------------------------------
c
c begin global grid
c
      do 100 i = 1, npoi
c
c determine total lai and tree, shrub, and grass fractions
c
        treelai   = totlaiu(i) 
        shrublai  = plai(i,9)  + plai(i,10)
        grasslai  = plai(i,11) + plai(i,12)
c
c crop biomass -- as used as an overriding condition for
c determining a vegetation class
c
        cropbio  = 0.
        do 105 j = scpft, ecpft
         cropbio   = cropbio + biomass(i,j)
 105    continue
c 
c
        totlai    = max (0.01, totlail(i) + totlaiu(i))
c
c determine dominant tree type by lai dominance
c
        domtree = 0
        maxlai = 0.0
c
        do 110 j = 1, 8
          if (plai(i,j).gt.maxlai) then
            domtree = j
            maxlai = plai(i,j)
          endif
 110    continue
c
c assign initial vegetation type
c
        vegtype0(i) = -999.99
c
c dominant type:  tropical broadleaf evergreen tree
c
        if (domtree.eq.1) then
          if (treelai.gt.2.5)         vegtype0(i) =  1.0  ! tropical evergreen forest / woodland
          if (treelai.le.2.5)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.0.5) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c dominant type:  tropical broadleaf drought-deciduous tree
c
        if (domtree.eq.2) then
          if (treelai.gt.2.5)         vegtype0(i) =  2.0  ! tropical deciduous forest / woodland
          if (treelai.le.2.5)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.0.5) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c dominant type:  warm-temperate broadleaf evergreen tree
c
        if (domtree.eq.3) then
          if (treelai.gt.2.5)         vegtype0(i) =  3.0  ! temperate evergreen broadleaf forest / woodland
          if (treelai.le.2.5)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.0.5) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c dominant type:  temperate conifer evergreen tree
c
        if (domtree.eq.4) then
          if (treelai.gt.1.5)         vegtype0(i) =  4.0  ! temperate evergreen conifer forest / woodland
          if (treelai.le.1.5)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.0.5) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c dominant type:  temperate broadleaf deciduous tree
c
        if (domtree.eq.5) then
          if (treelai.gt.1.5)         vegtype0(i) =  5.0  ! temperate deciduous forest / woodland
          if (treelai.le.1.5)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.0.5) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c dominant type:  boreal conifer evergreen tree
c
        if (domtree.eq.6)             vegtype0(i) =  6.0  ! boreal evergreen forest / woodland
c
c       if (domtree.eq.6) then
c         if (treelai.gt.1.0)         vegtype0(i) =  6.0  ! boreal evergreen forest / woodland
c         if (treelai.le.1.0) then
c           if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
c           if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
c         endif
c       endif
c
c dominant type:  boreal broadleaf cold-deciduous tree
c
        if (domtree.eq.7)             vegtype0(i) =  7.0  ! boreal deciduous forest / woodland
c
c       if (domtree.eq.7) then
c         if (treelai.gt.1.0)         vegtype0(i) =  7.0  ! boreal deciduous forest / woodland
c         if (treelai.le.1.0) then
c           if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
c           if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
c         endif
c       endif
c
c dominant type:  boreal conifer cold-deciduous tree
c
        if (domtree.eq.8)             vegtype0(i) =  7.0  ! boreal deciduous forest / woodland
c
c       if (domtree.eq.8) then
c         if (treelai.gt.1.0)         vegtype0(i) =  7.0  ! boreal deciduous forest / woodland
c         if (treelai.le.1.0) then
c           if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
c           if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
c         endif
c       endif
c
c temperate/boreal forest mixtures
c
        if ((domtree.ge.4).and.(domtree.le.8)) then
          ratio = (plai(i,5) + plai(i,7) + plai(i,8)) / 
     >            (plai(i,4) + plai(i,5) + plai(i,6) + 
     >             plai(i,7) + plai(i,8))
          if (treelai.gt.1.0) then
            if ((ratio.gt.0.45).and.(ratio.lt.0.55)) vegtype0(i) = 8.
          endif
          if ((domtree.le.5).and.(treelai.le.1.0)) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c no tree is dominant
c
        if (domtree.eq.0) then
          if (treelai.gt.1.0)         vegtype0(i) =  9.0  ! savanna
          if (treelai.le.1.0) then
            if (grasslai.ge.shrublai) vegtype0(i) = 10.0  ! grassland
            if (shrublai.ge.grasslai) vegtype0(i) = 11.0  ! closed shrubland
          endif
        endif
c
c overriding vegtation classifications
c
        if (totlai.lt.1.0)            vegtype0(i) = 12.0  ! open shrubland
        if (totlai.le.0.4)            vegtype0(i) = 14.0  ! desert
c
c overriding climatic rules
c
        if (gdd5(i).lt.350.0) then
          if (totlai.ge.0.4)          vegtype0(i) = 13.0  ! tundra
          if (totlai.lt.0.4)          vegtype0(i) = 15.0  ! polar desert
        endif
c
        if (gdd0(i).lt.100.0)         vegtype0(i) = 15.0  ! polar desert
c
        if (cropbio .gt. 0.0)         vegtype0(i) = 16.0  ! croplands
c
 100  continue
c
c return to the main program
c
      return
      end
c


c ---------------------------------------------------------------------
      subroutine logict (arrl, nar)
c ---------------------------------------------------------------------
      logical arrl(nar)
c
      do 100 j = 1, nar
        arrl(j) = .TRUE.
 100  continue
c
      return
      end



c ---------------------------------------------------------------------
      subroutine logicf (arrl, nar)
c ---------------------------------------------------------------------
      logical arrl(nar)
c
      do 100 j = 1, nar
        arrl(j) = .FALSE.
 100  continue
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
      real value
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
