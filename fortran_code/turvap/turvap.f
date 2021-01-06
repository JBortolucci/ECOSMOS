      
      subroutine turvapWrapper(iter, niter, dim_params,
     >                         double_params, npoi_matrix,
     >                         local_matrix,
     >                         stressl,
     >                         stressu,
     >                         suction,
     >                         swilt,
     >                         tsno,
     >                         tsoi,
     >                         upsoil,
     >                         upsoiu,
     >                         wisoi,
     >                         rhosoi,
     >                         hsno,
     >                         frac,
     >                         hsoi,
     >                         poros,
     >                         lai,
     >                         sai,
     >                         csoi,
     >                         consoi,
     >                         bex,
     >                         wsoi)

                implicit none

                integer iter, niter

                integer dim_params(4)

                double precision double_params(20)

                double precision npoi_matrix(85, dim_params(1))

                double precision local_matrix(23, dim_params(1))

                double precision local_line1(dim_params(1))
                double precision local_line2(dim_params(1))
                double precision local_line3(dim_params(1))
                double precision local_line4(dim_params(1))
                double precision local_line5(dim_params(1))
                double precision local_line6(dim_params(1))
                double precision local_line7(dim_params(1))
                double precision local_line8(dim_params(1))
                double precision local_line9(dim_params(1))
                double precision local_line10(dim_params(1))
                double precision local_line11(dim_params(1))
                double precision local_line12(dim_params(1))
                double precision local_line13(dim_params(1))
                double precision local_line14(dim_params(1))
                double precision local_line15(dim_params(1))
                double precision local_line16(dim_params(1))
                double precision local_line17(dim_params(1))
                double precision local_line18(dim_params(1))
                double precision local_line19(dim_params(1))
                double precision local_line20(dim_params(1))
                double precision local_line21(dim_params(1))
                double precision local_line22(dim_params(1))
                double precision local_line23(dim_params(1))

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
                double precision npoi_line65(dim_params(1))
                double precision npoi_line66(dim_params(1))
                double precision npoi_line67(dim_params(1))
                double precision npoi_line68(dim_params(1))
                double precision npoi_line69(dim_params(1))
                double precision npoi_line70(dim_params(1))
                double precision npoi_line71(dim_params(1))
                double precision npoi_line72(dim_params(1))
                double precision npoi_line73(dim_params(1))
                double precision npoi_line74(dim_params(1))
                double precision npoi_line75(dim_params(1))
                double precision npoi_line76(dim_params(1))
                double precision npoi_line77(dim_params(1))
                double precision npoi_line78(dim_params(1))
                double precision npoi_line79(dim_params(1))
                double precision npoi_line80(dim_params(1))
                double precision npoi_line81(dim_params(1))
                double precision npoi_line82(dim_params(1))
                double precision npoi_line83(dim_params(1))
                double precision npoi_line84(dim_params(1))
                double precision npoi_line85(dim_params(1))

                double precision stressl(dim_params(1),dim_params(3))
                double precision stressu(dim_params(1),dim_params(3))
                double precision suction(dim_params(1),dim_params(3))
                double precision swilt(dim_params(1),dim_params(3))
                double precision tsno(dim_params(1),dim_params(4))
                double precision tsoi(dim_params(1),dim_params(3))
                double precision upsoil(dim_params(1),dim_params(3))
                double precision upsoiu(dim_params(1),dim_params(3))
                double precision wisoi(dim_params(1),dim_params(3))
                double precision rhosoi(dim_params(1),dim_params(3))
                double precision hsno(dim_params(1),dim_params(4))
                double precision frac(dim_params(1),dim_params(2))
                double precision hsoi(dim_params(3)+1)
                double precision poros(dim_params(1),dim_params(3))
                double precision lai(dim_params(1),2)
                double precision sai(dim_params(1),2)
                double precision csoi(dim_params(1),dim_params(3))
                double precision consoi(dim_params(1),dim_params(3))
                double precision bex(dim_params(1),dim_params(3))
                double precision wsoi(dim_params(1),dim_params(3))
                
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
                       npoi_line65(i) = npoi_matrix(65,i)
                       npoi_line66(i) = npoi_matrix(66,i)
                       npoi_line67(i) = npoi_matrix(67,i)
                       npoi_line68(i) = npoi_matrix(68,i)
                       npoi_line69(i) = npoi_matrix(69,i)
                       npoi_line70(i) = npoi_matrix(70,i)
                       npoi_line71(i) = npoi_matrix(71,i)
                       npoi_line72(i) = npoi_matrix(72,i)
                       npoi_line73(i) = npoi_matrix(73,i)
                       npoi_line74(i) = npoi_matrix(74,i)
                       npoi_line75(i) = npoi_matrix(75,i)
                       npoi_line76(i) = npoi_matrix(76,i)
                       npoi_line77(i) = npoi_matrix(77,i)
                       npoi_line78(i) = npoi_matrix(78,i)
                       npoi_line79(i) = npoi_matrix(79,i)
                       npoi_line80(i) = npoi_matrix(80,i)
                       npoi_line81(i) = npoi_matrix(81,i)
                       npoi_line82(i) = npoi_matrix(82,i)
                       npoi_line83(i) = npoi_matrix(83,i)
                       npoi_line84(i) = npoi_matrix(84,i)
                       npoi_line85(i) = npoi_matrix(85,i)

                       local_line1(i) = local_matrix(1,i)
                       local_line2(i) = local_matrix(2,i)
                       local_line3(i) = local_matrix(3,i)
                       local_line4(i) = local_matrix(4,i)
                       local_line5(i) = local_matrix(5,i)
                       local_line6(i) = local_matrix(6,i)
                       local_line7(i) = local_matrix(7,i)
                       local_line8(i) = local_matrix(8,i)
                       local_line9(i) = local_matrix(9,i)
                       local_line10(i) = local_matrix(10,i)
                       local_line11(i) = local_matrix(11,i)
                       local_line12(i) = local_matrix(12,i)
                       local_line13(i) = local_matrix(13,i)
                       local_line14(i) = local_matrix(14,i)
                       local_line15(i) = local_matrix(15,i)
                       local_line16(i) = local_matrix(16,i)
                       local_line17(i) = local_matrix(17,i)
                       local_line18(i) = local_matrix(18,i)
                       local_line19(i) = local_matrix(19,i)
                       local_line20(i) = local_matrix(20,i)
                       local_line21(i) = local_matrix(21,i)
                       local_line22(i) = local_matrix(22,i)
                       local_line23(i) = local_matrix(23,i)
102             CONTINUE

                call turvap(iter, niter, dim_params(1), dim_params(2),
     >                      dim_params(3), dim_params(4),
     >                      double_params(1), double_params(2), double_params(3),
     >                      double_params(4), double_params(5), double_params(6),
     >                      double_params(7), double_params(8), double_params(9),
     >                      double_params(10), double_params(11), double_params(12),
     >                      double_params(13), double_params(14), double_params(15),
     >                      double_params(16), double_params(17), double_params(18),
     >                      double_params(19), double_params(20),      
     >                      npoi_line1, npoi_line2, npoi_line3, npoi_line4, npoi_line5,
     >                      npoi_line6, npoi_line7, npoi_line8, npoi_line9, npoi_line10,
     >                      npoi_line11, npoi_line12, npoi_line13, npoi_line14, npoi_line15,
     >                      npoi_line16, npoi_line17, npoi_line18, npoi_line19, npoi_line20,
     >                      npoi_line21, npoi_line22, npoi_line23, npoi_line24, npoi_line25,
     >                      npoi_line26, npoi_line27, npoi_line28, npoi_line29, npoi_line30,
     >                      npoi_line31, npoi_line32, npoi_line33, npoi_line34, npoi_line35,
     >                      npoi_line36, npoi_line37, npoi_line38, npoi_line39, npoi_line40,
     >                      npoi_line41, npoi_line42, npoi_line43, npoi_line44, npoi_line45,
     >                      npoi_line46, npoi_line47, npoi_line48, npoi_line49, npoi_line50,
     >                      npoi_line51, npoi_line52, npoi_line53, npoi_line54, npoi_line55,
     >                      npoi_line56, npoi_line57, npoi_line58, npoi_line59, npoi_line60,
     >                      npoi_line61, npoi_line62, npoi_line63, npoi_line64, npoi_line65,
     >                      npoi_line66, npoi_line67, npoi_line68, npoi_line69, npoi_line70,
     >                      npoi_line71, npoi_line72, npoi_line73, npoi_line74, npoi_line75,
     >                      npoi_line76, npoi_line77, npoi_line78, npoi_line79, npoi_line80,
     >                      npoi_line81, npoi_line82, npoi_line83, npoi_line84, npoi_line85,
     >                      stressl,
     >                      stressu,
     >                      suction,
     >                      swilt,
     >                      tsno,
     >                      tsoi,
     >                      upsoil,
     >                      upsoiu,
     >                      wisoi,
     >                      rhosoi,
     >                      hsno,
     >                      frac,
     >                      hsoi,
     >                      poros,
     >                      lai,
     >                      sai,
     >                      csoi,
     >                      consoi,
     >                      bex,
     >                      wsoi,
     >                      local_line1,
     >                      local_line2,
     >                      local_line3,
     >                      local_line4,
     >                      local_line5,
     >                      local_line6,
     >                      local_line7,
     >                      local_line8,
     >                      local_line9,
     >                      local_line10,
     >                      local_line11,
     >                      local_line12,
     >                      local_line13,
     >                      local_line14,
     >                      local_line15,
     >                      local_line16,
     >                      local_line17,
     >                      local_line18,
     >                      local_line19,
     >                      local_line20,
     >                      local_line21,
     >                      local_line22,
     >                      local_line23)


c 	        RETORNANDO VALOR PARA MATRIZ
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
                       npoi_matrix(65,i) = npoi_line65(i)
                       npoi_matrix(66,i) = npoi_line66(i)
                       npoi_matrix(67,i) = npoi_line67(i)
                       npoi_matrix(68,i) = npoi_line68(i)
                       npoi_matrix(69,i) = npoi_line69(i)
                       npoi_matrix(70,i) = npoi_line70(i)
                       npoi_matrix(71,i) = npoi_line71(i)
                       npoi_matrix(72,i) = npoi_line72(i)
                       npoi_matrix(73,i) = npoi_line73(i)
                       npoi_matrix(74,i) = npoi_line74(i)
                       npoi_matrix(75,i) = npoi_line75(i)
                       npoi_matrix(76,i) = npoi_line76(i)
                       npoi_matrix(77,i) = npoi_line77(i)
                       npoi_matrix(78,i) = npoi_line78(i)
                       npoi_matrix(79,i) = npoi_line79(i)
                       npoi_matrix(80,i) = npoi_line80(i)
                       npoi_matrix(81,i) = npoi_line81(i)
                       npoi_matrix(82,i) = npoi_line82(i)
                       npoi_matrix(83,i) = npoi_line83(i)
                       npoi_matrix(84,i) = npoi_line84(i)
                       npoi_matrix(85,i) = npoi_line85(i)

                       local_matrix(1,i) = local_line1(i)
                       local_matrix(2,i) = local_line2(i)
                       local_matrix(3,i) = local_line3(i)
                       local_matrix(4,i) = local_line4(i)
                       local_matrix(5,i) = local_line5(i)
                       local_matrix(6,i) = local_line6(i)
                       local_matrix(7,i) = local_line7(i)
                       local_matrix(8,i) = local_line8(i)
                       local_matrix(9,i) = local_line9(i)
                       local_matrix(10,i) = local_line10(i)
                       local_matrix(11,i) = local_line11(i)
                       local_matrix(12,i) = local_line12(i)
                       local_matrix(13,i) = local_line13(i)
                       local_matrix(14,i) = local_line14(i)
                       local_matrix(15,i) = local_line15(i)
                       local_matrix(16,i) = local_line16(i)
                       local_matrix(17,i) = local_line17(i)
                       local_matrix(18,i) = local_line18(i)
                       local_matrix(19,i) = local_line19(i)
                       local_matrix(20,i) = local_line20(i)
                       local_matrix(21,i) = local_line21(i)
                       local_matrix(22,i) = local_line22(i)
                       local_matrix(23,i) = local_line23(i)

777             CONTINUE


      end 

      subroutine turvap (iter, niter,
     >                   npoi,
     >                   npft,
     >                   nsoilay,
     >                   nsnolay,
     >                   dtime,
     >                   epsilon,
     >                   ch2o,
     >                   chl,
     >                   chs,
     >                   chu,
     >                   cice,
     >                   cvap,
     >                   hsub,
     >                   grav,
     >                   hsnotop,
     >                   hvap,
     >                   rhow,
     >                   rvap,
     >                   stef,
     >                   tfac,
     >                   tmelt,
     >                   wpudmax,
     >                   consno,
     >                   hfus,
     >                   cp,
     >                   cl,
     >                   cu,
     >                   fi,
     >                   firb,
     >                   firg,
     >                   firi,
     >                   firl,
     >                   firs,
     >                   firu,
     >                   fl,
     >                   fsena,
     >                   fseng,
     >                   fseni,
     >                   fsenl,
     >                   fsens,
     >                   fsenu,
     >                   fu,
     >                   fvapa,
     >                   fvapg,
     >                   fvapi,
     >                   fvaplt,
     >                   fvaplw,
     >                   fvaps,
     >                   fvaput,
     >                   fvapuw,
     >                   fwetl,
     >                   fwetlx,
     >                   fwets,
     >                   fwetsx,
     >                   fwetu,
     >                   fwetux,
     >                   ginvap,
     >                   greenfracl,
     >                   gsuvap,
     >                   gtrans,
     >                   gtransl,
     >                   gtransu,
     >                   hvasug,
     >                   hvasui,
     >                   pfluxl,
     >                   pfluxs,
     >                   pfluxu,
     >                   psurf,
     >                   q12,
     >                   q34,
     >                   qa,
     >                   rliql,
     >                   rliqs,
     >                   rliqu,
     >                   sg,
     >                   si,
     >                   sl,
     >                   solg,
     >                   soli,
     >                   soll,
     >                   sols,
     >                   solu,
     >                   ss,
     >                   stresstl,
     >                   stresstu,
     >                   su,
     >                   t12,
     >                   t34,
     >                   ta,
     >                   tg,
     >                   ti,
     >                   tl,
     >                   totcondc3,
     >                   totcondc4,
     >                   totcondl3,
     >                   totcondl4,
     >                   totcondls,
     >                   totcondub,
     >                   totconduc,
     >                   ts,
     >                   tu,
     >                   wipud,
     >                   wliql,
     >                   wliqs,
     >                   wliqu,
     >                   wpud,
     >                   wsnol,
     >                   wsnos,
     >                   wsnou,
     >                   stressl,
     >                   stressu,
     >                   suction,
     >                   swilt,
     >                   tsno,
     >                   tsoi,
     >                   upsoil,
     >                   upsoiu,
     >                   wisoi,
     >                   rhosoi,
     >                   hsno,
     >                   frac,
     >                   hsoi,
     >                   poros,
     >                   lai,
     >                   sai,
     >                   csoi,
     >                   consoi,
     >                   bex,
     >                   wsoi,
     >                   xu, ! inicia variáveis locais que precisam manter o valor
     >                   xs,
     >                   xl,
     >                   chux,
     >                   chsx,
     >                   chlx,
     >                   chgx,
     >                   wlgx,
     >                   wigx,
     >                   cog,
     >                   coi,
     >                   zirg,
     >                   ziri,
     >                   wu,
     >                   ws,
     >                   wl,
     >                   wg,
     >                   wi,
     >                   tuold,
     >                   tsold,
     >                   tlold,
     >                   tgold,
     >                   tiold)

c ---------------------------------------------------------------------
c
c solves canopy system with linearized implicit sensible heat and
c moisture fluxes
c
c first, assembles matrix arr of coeffs in linearized equations
c for tu,ts,tl,t12,t34,q12,q34,tg,ti and assembles the right hand
c sides in the rhs vector
c
c then calls linsolve to solve this system, passing template mplate of
c zeros of arr 
c 
c finally calculates the implied fluxes and stores them 
c for the agcm, soil, snow models and budget calcs
c
c common blocks
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
c global variables
      
       ! 4
       integer          npoi
       integer          npft
       integer          nsoilay
       integer          nsnolay
       double precision dtime
       double precision epsilon
       double precision ch2o
       double precision chl
       double precision chs
       double precision chu
       double precision cice
       double precision cvap
       double precision hsub
       double precision grav
       double precision hsnotop
       double precision hvap
       double precision rhow
       double precision rvap
       double precision stef
       double precision tfac
       double precision tmelt
       double precision wpudmax
       double precision consno
       double precision hfus
       double precision cp(npoi)
       double precision cl(npoi)
       double precision cu(npoi)
       double precision fi(npoi)
       double precision firb(npoi)
       double precision firg(npoi)
       double precision firi(npoi)
       double precision firl(npoi)
       double precision firs(npoi)
       double precision firu(npoi)
       double precision fl(npoi)
       double precision fsena(npoi)
       double precision fseng(npoi)
       double precision fseni(npoi)
       double precision fsenl(npoi)
       double precision fsens(npoi)
       double precision fsenu(npoi)
       double precision fu(npoi)
       double precision fvapa(npoi)
       double precision fvapg(npoi)
       double precision fvapi(npoi)
       double precision fvaplt(npoi)
       double precision fvaplw(npoi)
       double precision fvaps(npoi)
       double precision fvaput(npoi)
       double precision fvapuw(npoi)
       double precision fwetl(npoi)
       double precision fwetlx(npoi)
       double precision fwets(npoi)
       double precision fwetsx(npoi)
       double precision fwetu(npoi)
       double precision fwetux(npoi)
       double precision ginvap(npoi)
       double precision greenfracl(npoi)
       double precision gsuvap(npoi)
       double precision gtrans(npoi)
       double precision gtransl(npoi)
       double precision gtransu(npoi)
       double precision hvasug(npoi)
       double precision hvasui(npoi)
       double precision pfluxl(npoi)
       double precision pfluxs(npoi)
       double precision pfluxu(npoi)
       double precision psurf(npoi)
       double precision q12(npoi)
       double precision q34(npoi)
       double precision qa(npoi)
       double precision rliql(npoi)
       double precision rliqs(npoi)
       double precision rliqu(npoi)
       double precision sg(npoi)
       double precision si(npoi)
       double precision sl(npoi)
       double precision solg(npoi)
       double precision soli(npoi)
       double precision soll(npoi)
       double precision sols(npoi)
       double precision solu(npoi)
       double precision ss(npoi)
       double precision stresstl(npoi)
       double precision stresstu(npoi) 
       double precision su(npoi)
       double precision t12(npoi)
       double precision t34(npoi)
       double precision ta(npoi)
       double precision tg(npoi)
       double precision ti(npoi)
       double precision tl(npoi)
       double precision totcondc3(npoi)
       double precision totcondc4(npoi)
       double precision totcondl3(npoi)
       double precision totcondl4(npoi)
       double precision totcondls(npoi)
       double precision totcondub(npoi)
       double precision totconduc(npoi)
       double precision ts(npoi)
       double precision tu(npoi)
       double precision wipud(npoi)
       double precision wliql(npoi)
       double precision wliqs(npoi)
       double precision wliqu(npoi)
       double precision wpud(npoi)
       double precision wsnol(npoi)
       double precision wsnos(npoi)
       double precision wsnou(npoi)

       double precision stressl(npoi,nsoilay)
       double precision stressu(npoi,nsoilay)
       double precision suction(npoi,nsoilay)
       double precision swilt(npoi,nsoilay)
       double precision tsno(npoi,nsnolay)
       double precision tsoi(npoi,nsoilay)
       double precision upsoil(npoi,nsoilay)
       double precision upsoiu(npoi,nsoilay)
       double precision wisoi(npoi,nsoilay)
       double precision rhosoi(npoi,nsoilay)
       double precision hsno(npoi,nsnolay)
       double precision frac(npoi,npft)
       double precision hsoi(nsoilay+1)
       double precision poros(npoi,nsoilay)
       double precision lai(npoi,2)
       double precision sai(npoi,2)
       double precision csoi(npoi,nsoilay)
       double precision consoi(npoi,nsoilay)
       double precision bex(npoi,nsoilay)
       double precision wsoi(npoi,nsoilay)


      
c Arguments (input)
c
      integer niter,      ! total # of iteration
     >        iter        ! # of iteration
c
c local variables
c
      integer i, k
c
      double precision rwork, zwtot, rwork2, tgav, tiav, tuav, e,  
     >     tsav, tlav, quav, qsav, qlav, qgav, qiav, zwpud, zwsoi,
     >     psig, hfac, hfac2, zwopt, zwdry, betaw, emisoil, qs1,
     >     dqs1, xnumer, xdenom, betafac, betas
c
      double precision
     >  xu(npoi),
     >  xs(npoi),
     >  xl(npoi),
     >  chux(npoi), 
     >  chsx(npoi), 
     >  chlx(npoi),
     >  chgx(npoi),
     >  wlgx(npoi),
     >  wigx(npoi),
     >  fradu(npoi), 
     >  frads(npoi), 
     >  fradl(npoi),
     >  wu(npoi),
     >  ws(npoi),
     >  wl(npoi),      
     >  wg(npoi),
     >  wi(npoi), 
     >  qu(npoi),
     >  qs(npoi),  
     >  ql(npoi),  
     >  qg(npoi),  
     >  qi(npoi),
     >  dqu(npoi),
     >  dqs(npoi), 
     >  dql(npoi), 
     >  dqg(npoi), 
     >  dqi(npoi),
     >  tuold(npoi),
     >  tsold(npoi),
     >  tlold(npoi),
     >  tgold(npoi), 
     >  tiold(npoi),
     >  tupre(npoi),
     >  tspre(npoi),
     >  tlpre(npoi),
     >  tgpre(npoi),
     >  tipre(npoi),
     >  suw(npoi),
     >  ssw(npoi),
     >  slw(npoi),
     >  sut(npoi),
     >  slt(npoi),
     >  slt0(npoi),
     >  suh(npoi),
     >  ssh(npoi), 
     >  slh(npoi),
     >  cog(npoi),
     >  coi(npoi),
     >  zirg(npoi),
     >  ziri(npoi),
     >  qgfac(npoi),
     >  qgfac0(npoi)
c
c      save 
c     >  xu,
c     >  xs,
c     >  xl,
c     >  chux, 
c     >  chsx, 
c     >  chlx,
c     >  chgx,
c     >  wlgx,
c     >  wigx,
c     >  cog,
c     >  coi,
c     >  zirg,
c     >  ziri,
c     >  wu,  
c     >  ws,    
c     >  wl,    
c     >  wg, 
c     >  wi,
c     >  tuold, 
c     >  tsold, 
c     >  tlold, 
c     >  tgold, 
c     >  tiold
cc
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


c     Variáveis do laço que substitui a rotina const
      integer j, p, q  

      integer nqn
c
      parameter (nqn=9)
c
      double precision arr(npoi,nqn,nqn),      !    
     >     rhs(npoi,nqn),          ! right hand side
     >     vec(npoi,nqn)           ! 
c
      integer  mplate(nqn,nqn)
cc
c                  tu  ts  tl t12 t34 q12 q34  tg  ti 
c                  ----------------------------------
      data mplate / 1,  0,  0,  1,  0,  1,  0,  0,  0, !tu
     >              0,  1,  0,  1,  0,  1,  0,  0,  0, !ts
     >              0,  0,  1,  0,  1,  0,  1,  0,  0, !tl
     >              1,  1,  0,  1,  1,  0,  0,  0,  0, !t12
     >              0,  0,  1,  1,  1,  0,  0,  1,  1, !t34
     >              1,  1,  0,  0,  0,  1,  1,  0,  0, !q12
     >              0,  0,  1,  0,  0,  1,  1,  1,  1, !q34
     >              0,  0,  0,  0,  1,  0,  1,  1,  0, !tg
     >              0,  0,  0,  0,  1,  0,  1,  0,  1  !ti
     >            /
c
      
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

       hvapf(t,tair) = hvap + cvap*(tair-273.16) - ch2o*(t-273.16)
       hsubf(t,tair) = hsub + cvap*(tair-273.16) - cice*(t-273.16)
c
c if first iteration, save original canopy temps in t*old
c (can use tsoi,tsno for original soil/snow skin temps), for
c rhs heat capacity terms in matrix soln, and for adjustment
c of canopy temps after each iteration
c
c also initialize soil/snow skin temps tg, ti to top-layer temps
c
c the variables t12, t34, q12, q34, for the first iteration
c are saved via global arrays from the previous gcm timestep,
c this is worth doing only if the agcm forcing is
c smoothly varying from timestep to timestep
c
      if (iter.eq.1) then
c
c weights for canopy coverages
c
        do 10 i = 1, npoi
          xu(i) = 2.0 * lai(i,2) * fu(i)
          xs(i) = 2.0 * sai(i,2) * fu(i)
          xl(i) = 2.0 * (lai(i,1) + sai(i,1)) * fl(i) * (1.0 - fi(i))
 10     continue 
c
c specific heats per leaf/stem area
c
        do 20 i = 1, npoi
          chux(i) = chu + ch2o * wliqu(i) + cice * wsnou(i)
          chsx(i) = chs + ch2o * wliqs(i) + cice * wsnos(i)
          chlx(i) = chl + ch2o * wliql(i) + cice * wsnol(i)
 20     continue
c
        do 30 i = 1, npoi 
c
          rwork = poros(i,1) * rhow
c
          chgx(i) = ch2o * wpud(i) + cice * wipud(i)
     >              + ((1.-poros(i,1))*csoi(i,1)*rhosoi(i,1)
     >              + rwork*(1.-wisoi(i,1))*wsoi(i,1)*ch2o
     >              + rwork*wisoi(i,1)*cice
     >                ) * hsoi(1)
c
          wlgx(i) = wpud(i) +
     >              rwork * (1. - wisoi(i,1)) *
     >              wsoi(i,1) * hsoi(1)
c
          wigx(i) = wipud(i) + rwork * wisoi(i,1) * hsoi(1)
c
 30     continue 
c
c conductivity coeffs between ground skin and first layer
c
        do 40 i = 1, npoi
          cog(i) = consoi(i,1) / (0.5 * hsoi(1))
          coi(i) = consno      / (0.5 * max (hsno(i,1), hsnotop))
 40     continue
c
c d(ir emitted) / dt for soil
c
        rwork = 4. * 0.95 * stef
c
        do 50 i = 1, npoi
          zirg(i) = rwork * (tg(i)**3)
          ziri(i) = rwork * (ti(i)**3)
 50     continue
c
c updated temperature memory
c
        do 60 i = 1, npoi
          tuold(i) = tu(i)
          tsold(i) = ts(i)
          tlold(i) = tl(i)
          tgold(i) = tg(i)
          tiold(i) = ti(i)
 60     continue
c
      endif

        

c
c set implicit/explicit factors w* (0 to 1) for this iteration
c w* is 1 for fully implicit, 0 for fully explicit
c for first iteration, impexp and impexp2 set w* to 1
c
      call impexp (wu, tu, chux, wliqu, wsnou, iter,
     >                   epsilon,
     >                   hfus,
     >                   npoi,
     >                   tmelt)
      
      
      call impexp (ws, ts, chsx, wliqs, wsnos, iter,
     >                   epsilon,
     >                   hfus,
     >                   npoi,
     >                   tmelt)
      
      call impexp (wl, tl, chlx, wliql, wsnol, iter,
     >                   epsilon,
     >                   hfus,
     >                   npoi,
     >                   tmelt)
      call impexp (wg, tg, chgx, wlgx,  wigx,  iter,
     >                   epsilon,
     >                   hfus,
     >                   npoi,
     >                   tmelt)
c
c call impexp2 for snow model
c
      call impexp2 (wi, ti, tiold, iter,
     >                    epsilon,
     >                    npoi,
     >                    tmelt)

c
c adjust t* for this iteration 
c
c in this routine we are free to choose them, 
c since they are just the central values about which the 
c equations are linearized - heat is conserved in the matrix
c solution because t*old are used for the rhs heat capacities
c
c here, let t* represent the previous soln if it was fully
c implicit, but weight towards t*old depending on the amount
c (1-w*) the previous soln was explicit
c
c this weighting is necessary for melting/freezing surfaces, for which t*
c is kept at t*old, presumably at or near tmelt
c
      do 80 i = 1, npoi
        tu(i) = wu(i) * tu(i) + (1.0 - wu(i)) * tuold(i)
        ts(i) = ws(i) * ts(i) + (1.0 - ws(i)) * tsold(i)
        tl(i) = wl(i) * tl(i) + (1.0 - wl(i)) * tlold(i)
        tg(i) = wg(i) * tg(i) + (1.0 - wg(i)) * tgold(i)
        ti(i) = wi(i) * ti(i) + (1.0 - wi(i)) * tiold(i)
 80   continue 
c
c save current "central" values for final flux calculations
c
      do 90 i = 1, npoi
        tupre(i) = tu(i)
        tspre(i) = ts(i)
        tlpre(i) = tl(i)
        tgpre(i) = tg(i)
        tipre(i) = ti(i)
 90   continue
c
c calculate various terms occurring in the linearized eqns,
c using values of t12, t34, q12, q34 from
c the previous iteration
c
c specific humidities for canopy and ground, and derivs wrt t
c for canopy
c
c limit derivs to avoid -ve implicit q's below,
c as long as d(temp)s in one iteration are le 10 deg k
c
      do 100 i = 1, npoi
c
        e      = esat((tu(i)))
        qu(i)  = qsat ((e), (psurf(i)))
        dqu(i) = dqsat ((tu(i)), (qu(i)))
        dqu(i) = min (dqu(i), qu(i) * 0.1)
c
        e      = esat((ts(i)))
        qs(i)  = qsat ((e), (psurf(i)))
        dqs(i) = dqsat ((ts(i)), (qs(i)))
        dqs(i) = min (dqs(i), qs(i) * 0.1)
c
        e      = esat((tl(i)))
        ql(i)  = qsat ((e), (psurf(i)))
        dql(i) = dqsat ((tl(i)), (ql(i)))
        dql(i) = min (dql(i), ql(i) * 0.1)
c
        e      = esat((tg(i)))
        qg(i)  = qsat ((e), (psurf(i)))
        dqg(i) = dqsat ((tg(i)), (qg(i)))
        dqg(i) = min (dqg(i), qg(i) * 0.1)
c
        e      = esat((ti(i)))
        qi(i)  = qsat ((e), (psurf(i)))
        dqi(i) = dqsat ((ti(i)), (qi(i)))
        dqi(i) = min (dqi(i), qi(i) * 0.1)
c
 100  continue
c
c set qgfac0, factor by which soil surface specific humidity
c is less than saturation
c
c it is important to note that the qgfac expression should
c satisfy timestep cfl criterion for upper-layer soil moisture
c for small wsoi(i,1)
c
c for each iteration, qgfac is set to qgfac0, or to 1 if
c condensation onto soil is anticipated (loop 110 in canopy.f)
c
c Evaporation from bare soil is calculated using the "beta method"
c (e.g., eqns 5 & 7 of Mahfouf and Noilhan 1991, JAM 30 1354-1365),
c but converted to the "alpha method" (eqns 2 & 3 of M&N), to match
c the structure in IBIS. The conversion from the beta to alpha
c method is through the relationship:
c   alpha * qgs - q34 = beta * (hfac * qgs - q34),
c from which one solves for alpha (which is equal to qgfac0):
c   qgfac0 = alpha = (beta * hfac) + (1 - beta)*(q34/qgs)
c
        do 105 i = 1, npoi
c
c first calculate the total saturated fraction at the soil surface
c (including puddles ... see soil.f)
c
          zwpud = max (0.0, min (0.5, 0.5*(wpud(i)+wipud(i))/wpudmax) )
          zwsoi = (1.0 - wisoi(i,1)) * wsoi(i,1) + wisoi(i,1)
          zwtot = zwpud + (1. - zwpud) * zwsoi
c
c next calculate the matric potential (from eqn 9.3 of Campbell and
c Norman), multiply by gravitational acceleration to get in units
c of J/kg, and calculate the relative humidity at the soil water
c surface (i.e., within the soil matrix), based on thermodynamic
c theory (eqn 4.13 of C&N)
c
          psig = -grav * suction(i,1) * (zwtot ** (-bex(i,1)))
          hfac = exp(psig/(rvap*tg(i)))
c
c then calculate the relative humidity of the air (relative to
c saturation at the soil temperature). Note that if hfac2 > 1
c (which would imply condensation), then qgfac is set to 1
c later in the code (to allow condensation to proceed at the
c "potential rate")
c
          hfac2 = q34(i)/qg(i)
c
c set the "beta" factor and then calculate "alpha" (i.e., qgfac0)
c as the beta-weighted average of the soil water RH and the "air RH"
c First calculate beta_w:
c
          zwopt = 1.0
          zwdry = swilt(i,1)
          betaw = max(0.0, min(1., (zwtot - zwdry)/(zwopt - zwdry)) )
c
c limit evap if soil is frozen or snow-covered
          if (tg(i).le.273.16.or.fi(i).gt.0.0) then
            betaw = 0.01
          endif
c
c Next convert beta_w to beta_s (see Milly 1992, JClim 5 209-226):
c
          emisoil = 0.95
          e      = esat((t34(i)))
          qs1    = qsat((e), (psurf(i)))
          dqs1   = dqsat((t34(i)), (qs1))
          xnumer = hvap * dqs1
          xdenom = cp(i) + (4.0 * emisoil * stef * (t34(i))**3) / sg(i)
          betafac = xnumer / xdenom
          betas = betaw / (1.0 + betafac * (1.0 - betaw))
c
c Combine hfac and hfac2 into qgfac0 ("alpha") using beta_s
c
          qgfac0(i) = betas * hfac + (1. - betas) * hfac2
  105   continue
c
c set fractions covered by intercepted h2o to 1 if dew forms
c
c these fwet*x are used only in turvap, and are distinct from
c the real fractions fwet* that are set in fwetcal
c
c they must be exactly 1 if q12 > qu or q34 > ql, to zero transpiration
c by the factor 1-fwet[u,l]x below, so preventing "-ve" transp
c
c similarly, set qgfac, allowing for anticipated dew formation
c to avoid excessive dew formation (which then infiltrates) onto
c dry soils
c
      do 110 i = 1, npoi
c
        fwetux(i) = fwetu(i)
        if (q12(i).gt.qu(i)) fwetux(i) = 1.0
c
        fwetsx(i) = fwets(i)
        if (q12(i).gt.qs(i)) fwetsx(i) = 1.0
c
        fwetlx(i) = fwetl(i)
        if (q34(i).gt.ql(i)) fwetlx(i) = 1.0
c
        qgfac(i) = qgfac0(i)
        if (q34(i).gt.qg(i)) qgfac(i) = 1.0
c
c set net absorbed radiative fluxes for canopy components
c
        fradu(i) = 0.0
c
        if (lai(i,2).gt.epsilon)
     >     fradu(i) = (solu(i) + firu(i)) / (2.0 * lai(i,2))
c
        frads(i) = 0.0
c
        if (sai(i,2).gt.epsilon)
     >     frads(i) = (sols(i) + firs(i)) / (2.0 * sai(i,2))
c
        fradl(i) = 0.0
c
        if ((lai(i,1)+sai(i,1)).gt.epsilon)
     >     fradl(i) = (soll(i) + firl(i)) /
     >                (2.0 * (lai(i,1) + sai(i,1)))
c
 110  continue
c
c calculate canopy-air moisture transfer coeffs for wetted
c leaf/stem areas, and for dry (transpiring) leaf areas
c
c the wetted-area coeffs suw,ssw,slw are constrained to be less
c than what would evaporate 0.8 * the intercepted h2o mass in 
c this timestep (using previous iteration's q* values)
c
c this should virtually eliminate evaporation-overshoots and the need
c for the "negative intercepted h2o"  correction in steph2o2
c        
      do 200 i = 1, npoi
c
c coefficient for evaporation from wet surfaces in the upper canopy:
c
        
        suw(i) = min ( fwetux(i) * su(i), 
     >                 0.8 * (wliqu(i) + wsnou(i)) /
     >                 max (dtime * (qu(i) - q12(i)), epsilon))
c
c coefficient for transpiration from average upper canopy leaves:
c
        sut(i) = (1.0 - fwetux(i)) * 0.5 *
     >           ( totcondub(i) * frac(i,1) +
     >             totcondub(i) * frac(i,2) +
     >             totcondub(i) * frac(i,3) +
     >             totconduc(i) * frac(i,4) +
     >             totcondub(i) * frac(i,5) +
     >             totconduc(i) * frac(i,6) +
     >             totcondub(i) * frac(i,7) +
     >             totcondub(i) * frac(i,8) )
c
        sut(i) = max (0.0, sut(i))
c
c coefficient for sensible heat flux from upper canopy:
c
        suh(i) = suw(i) * (rliqu(i)  * hvapf(tu(i),ta(i))  +
     >                 (1.-rliqu(i)) * hsubf(tu(i),ta(i))) +
     >           sut(i) *              hvapf(tu(i),ta(i))
c
c coefficient for evaporation from wet surfaces on the stems:
c
        ssw(i) = min (fwetsx(i) * ss(i), 
     >                0.8 * (wliqs(i) + wsnos(i))
     >                / max (dtime * (qs(i) - q12(i)), epsilon))
c
c coefficient for sensible heat flux from stems:
c
        ssh(i) = ssw(i) * (rliqs(i)  * hvapf(ts(i),ta(i)) +
     >                 (1.-rliqs(i)) * hsubf(ts(i),ta(i)))

c
c coefficient for evaporation from wet surfaces in the lower canopy:
c
c for the regional U.S. version, this is multiplied by the green
c fraction to reduce interception evaporation during the winter
c and spring, which otherwise seems too high. There is some
c physical basis for this, especially in snow covered regions
c where snow cover can cause masking and/or compaction of grass,
c thereby reducing interception. (This should be accounted for
c by fi(i) in ginvap (see below), but areal snow cover fraction
c is usually significantly underestimated in IBIS.)
c
c        slw(i) = min (fwetlx(i) * sl(i),
        slw(i) = min (fwetlx(i) * sl(i) * min(0.1, greenfracl(i)),
     >                0.8 * (wliql(i) + wsnol(i))
     >                / max (dtime * (ql(i) - q34(i)), epsilon))
c
c coefficient for transpiration from average lower canopy leaves:
c
        slt0(i) = (1. - fwetlx(i)) * 0.5 *
     >            ( totcondls(i) * frac(i,9)  +
     >              totcondls(i) * frac(i,10) +
     >              totcondl4(i) * frac(i,11) +
     >              totcondl3(i) * frac(i,12) +
     >              totcondc3(i) * frac(i,13) +
     >              totcondc4(i) * frac(i,14) +
     >              totcondc3(i) * frac(i,15) +
     >              totcondc4(i) * frac(i,16) )

c
        slt0(i) = max (0., slt0(i))

c
c averaged over stems and lower canopy leaves:
c 
        slt(i) = slt0(i) * lai(i,1) / max (lai(i,1)+sai(i,1), epsilon)
c
c coefficient for sensible heat flux from lower canopy:
c
        slh(i) = slw(i) * (  rliql(i)  * hvapf(tl(i),ta(i))  +
     >                   (1.-rliql(i)) * hsubf(tl(i),ta(i))) +
     >           slt(i) *                hvapf(tl(i),ta(i))
c
	
csant	if(i.eq.1)print*,slt(i),slh(i)

 200  continue

c
c set the matrix of coefficients and the right-hand sides
c of the linearized equations
c

c      JAIR: Substitui rotina const por laço       
       do 777 j = 1,npoi
           do 999 p = 1,nqn
              do 555 q = 1,nqn
                  arr(j, p, q) = 0
555           continue    
999        continue
777    continue
c     call const(arr, npoi*nqn*nqn, 0)

       do 888 j = 1,npoi
         do 111 p = 1, nqn
                rhs(j, p) = 0
111      continue       
888    continue 
c     call const(rhs, npoi*nqn, 0)
     
c
      rwork = 1. / dtime
c
c upper leaf temperature tu
c 

      do 300 i = 1, npoi
c
        rwork2 = su(i)*cp(i)
        arr(i,1,1) = chux(i)*rwork
     >             + wu(i)*rwork2
     >             + wu(i)*suh(i)*dqu(i)
        arr(i,1,4) = -rwork2
        arr(i,1,6) = -suh(i)
        rhs(i,1) = tuold(i)*chux(i)*rwork
     >           - (1.-wu(i))*rwork2*tu(i)
     >           - suh(i) * (qu(i)-wu(i)*dqu(i)*tu(i))
     >           + fradu(i) - pfluxu(i)
c 
 300  continue

c
c upper stem temperature ts
c
      do 310 i = 1, npoi
c
        rwork2 = ss(i)*cp(i)
        arr(i,2,2) = chsx(i)*rwork
     >             + ws(i)*rwork2
     >             + ws(i)*ssh(i)*dqs(i)
        arr(i,2,4) = -rwork2
        arr(i,2,6) = -ssh(i)
        rhs(i,2) = tsold(i)*chsx(i)*rwork
     >           - (1.-ws(i))*rwork2*ts(i)
     >           - ssh(i) * (qs(i)-ws(i)*dqs(i)*ts(i))
     >           + frads(i) - pfluxs(i)
c
 310  continue
c
c lower veg temperature tl
c
      do 320 i = 1, npoi
c
        rwork2 = sl(i)*cp(i)
        arr(i,3,3) = chlx(i)*rwork
     >             + wl(i)*rwork2
     >             + wl(i)*slh(i)*dql(i)
        arr(i,3,5) = -rwork2
        arr(i,3,7) = -slh(i)
        rhs(i,3) = tlold(i)*chlx(i)*rwork
     >           - (1.-wl(i))*rwork2*tl(i)
     >           - slh(i) * (ql(i)-wl(i)*dql(i)*tl(i))
     >           + fradl(i) - pfluxl(i)
c
 320  continue
c
c upper air temperature t12
c
      do 330 i = 1, npoi
c
        rwork = xu(i)*su(i)
        rwork2 = xs(i)*ss(i)
        arr(i,4,1) = -wu(i)*rwork
        arr(i,4,2) = -ws(i)*rwork2
        arr(i,4,4) = cu(i) + cl(i) + rwork + rwork2
        arr(i,4,5) = -cl(i)
        rhs(i,4) = cu(i)*ta(i)*tfac
     >           + (1.-wu(i))*rwork*tu(i)
     >           + (1.-ws(i))*rwork2*ts(i)
c
 330  continue
c
c lower air temperature t34
c
      do 340 i = 1, npoi
c
        rwork = xl(i)*sl(i)
        rwork2 = fi(i)*si(i)
        arr(i,5,3) = -wl(i)*rwork
        arr(i,5,4) = -cl(i)
        arr(i,5,5) = cl(i) + rwork
     >             + (1.-fi(i))*sg(i) + rwork2
        arr(i,5,8) = -wg(i)*(1.-fi(i))*sg(i)
        arr(i,5,9) = -wi(i)*rwork2
        rhs(i,5) = (1.-wl(i))*rwork           *tl(i)
     >           + (1.-wg(i))*(1.-fi(i))*sg(i)*tg(i)
     >           + (1.-wi(i))*rwork2          *ti(i)
c
 340  continue
c
c upper air specific humidity q12
c
      do 350 i = 1, npoi
c
        rwork = xu(i)*(suw(i)+sut(i))
        rwork2 = xs(i)*ssw(i)
        arr(i,6,1) = -wu(i)*rwork *dqu(i)
        arr(i,6,2) = -ws(i)*rwork2*dqs(i)
        arr(i,6,6) = cu(i) + cl(i)
     >             + rwork + rwork2
        arr(i,6,7) = -cl(i)
        rhs(i,6) = cu(i)*qa(i)
     >           + rwork  * (qu(i)-wu(i)*dqu(i)*tu(i))
     >           + rwork2 * (qs(i)-ws(i)*dqs(i)*ts(i))
c
  350 continue
c
c lower air specific humidity q34
c
      do 360 i = 1, npoi
c
        rwork  = xl(i)*(slw(i)+slt(i))
        rwork2 = (1.-fi(i))*sg(i)
        arr(i,7,3) = -wl(i)*rwork*dql(i)
        arr(i,7,6) = -cl(i)
        arr(i,7,7) = cl(i) + rwork
     >             + rwork2 +fi(i)*si(i)
        arr(i,7,8) = -wg(i)*rwork2*qgfac(i)*dqg(i)
        arr(i,7,9) = -wi(i)*fi(i)*si(i)*dqi(i)
        rhs(i,7)= rwork           *(ql(i)-wl(i)*dql(i)*tl(i))
     >          + rwork2*qgfac(i) *(qg(i)-wg(i)*dqg(i)*tg(i))
     >          + fi(i) *si(i)    *(qi(i)-wi(i)*dqi(i)*ti(i))
c
  360 continue
c
c soil skin temperature
c
c (there is no wg in this eqn since it solves for a fully
c implicit tg. wg can be thought of as the fractional soil
c area using a fully implicit soln, and 1-wg as that using a
c fully explicit soln. the combined soil temperature is felt
c by the lower air, so wg occurs in the t34,q34 eqns above.)
c
      do 370 i = 1, npoi
c
        rwork  = sg(i)*cp(i)
        rwork2 = sg(i)*hvasug(i)
        arr(i,8,5) = -rwork
        arr(i,8,7) = -rwork2
        arr(i,8,8) = rwork + rwork2*qgfac(i)*dqg(i)
     >             + cog(i) + zirg(i)
        rhs(i,8) = -rwork2*qgfac(i)*(qg(i)-dqg(i)*tg(i))
     >           + cog(i)*tsoi(i,1)
     >           + solg(i) + firg(i) + zirg(i) * tgold(i)
c
  370 continue
c
c snow skin temperature
c
c (there is no wi here, for the same reason as for wg above.)
c
      do 380 i = 1, npoi
c
	rwork  = si(i)*cp(i)
        rwork2 = si(i)*hvasui(i)
        arr(i,9,5) = -rwork
        arr(i,9,7) = -rwork2
        arr(i,9,9) = rwork + rwork2*dqi(i)
     >             + coi(i) + ziri(i)
        rhs(i,9) = -rwork2*(qi(i)-dqi(i)*ti(i))
     >           + coi(i)*tsno(i,1)
     >           + soli(i) + firi(i) + ziri(i) * tiold(i)
c
  380 continue
c
c solve the systems of equations
c

      call linsolve (arr, rhs, vec, mplate, nqn, npoi)

c
c copy this iteration's solution to t*, q12, q34
c
      do 400 i = 1, npoi
c
        tu(i)  = vec(i,1)
        ts(i)  = vec(i,2)
        tl(i)  = vec(i,3)
        t12(i) = vec(i,4)
        t34(i) = vec(i,5)
        tg(i)  = vec(i,8)
        ti(i)  = vec(i,9)
c
        q12(i) = vec(i,6)
        q34(i) = vec(i,7)
c
  400 continue
c
c all done except for final flux calculations,
c so loop back for the next iteration (except the last)
c
      if (iter .lt. niter) return
c
c evaluate sensible heat and moisture fluxes (per unit
c leaf/stem/snow-free/snow-covered area as appropriate)
c
c *******************************
c diagnostic sensible heat fluxes
c *******************************
c
     

      do 500 i = 1, npoi
c
        fsena(i) = cp(i) * cu(i) * (ta(i)*tfac - t12(i))
c
        tgav = wg(i)*tg(i) + (1.-wg(i))*tgpre(i)
        fseng(i) = cp(i) * sg(i) * (tgav - t34(i))
c
        tiav = wi(i)*ti(i) + (1.-wi(i))*tipre(i)
        fseni(i) = cp(i) * si(i) * (tiav - t34(i))

        tuav = wu(i)*tu(i) + (1. - wu(i))*tupre(i)
        fsenu(i) = cp(i) * su(i) * (tuav - t12(i))
c
        tsav = ws(i)*ts(i) + (1. - ws(i))*tspre(i)
        fsens(i) = cp(i) * ss(i) * (tsav - t12(i))
c
        tlav = wl(i)*tl(i) + (1. - wl(i))*tlpre(i)
        fsenl(i) = cp(i) * sl(i) * (tlav - t12(i))
c
 500  continue


      
c
c *************************
c calculate moisture fluxes
c *************************
c
      do 510 i = 1, npoi
c
c total evapotranspiration from the entire column
c
        fvapa(i)  = cu(i) * (qa(i)-q12(i))
c
csant  if(i.eq.1)print*,fvapa(i)
	
c evaporation from wet surfaces in the upper canopy
c and transpiration per unit leaf area - upper canopy
c
        quav = qu(i) + wu(i)*dqu(i)*(tu(i)-tupre(i))
        fvapuw(i) = suw(i) * (quav-q12(i))
        fvaput(i) = max (0.0, sut(i) * (quav-q12(i)))
c
c evaporation from wet surfaces on stems
c
        qsav = qs(i) + ws(i)*dqs(i)*(ts(i)-tspre(i))
        fvaps(i) = ssw(i) * (qsav-q12(i))
c
c evaporation from wet surfaces in the lower canopy
c and transpiration per unit leaf area - lower canopy
c
        qlav = ql(i) + wl(i)*dql(i)*(tl(i)-tlpre(i))
        fvaplw(i) = slw(i)  * (qlav-q34(i))
        fvaplt(i) = max (0.0, slt0(i) * (qlav-q34(i)))
c
c evaporation from the ground
c
        qgav = qg(i) + wg(i)*dqg(i)*(tg(i)-tgpre(i))
        fvapg(i) = sg(i) * (qgfac(i)*qgav - q34(i))
c
c evaporation from the snow
c
        qiav = qi(i) + wi(i)*dqi(i)*(ti(i)-tipre(i))
        fvapi(i) = si(i) * (qiav-q34(i))
c
 510  continue
c 
c adjust ir fluxes
c
      do 520 i = 1, npoi
c
        firg(i) = firg(i) - wg(i)*zirg(i)*(tg(i) - tgold(i))
        firi(i) = firi(i) - wi(i)*ziri(i)*(ti(i) - tiold(i))
        firb(i) = firb(i) + (1.-fi(i))*wg(i)*zirg(i)*(tg(i)-tgold(i))
     >                    +     fi(i) *wi(i)*ziri(i)*(ti(i)-tiold(i))
c
c impose constraint on skin temperature
c
        ti(i) = min (ti(i), tmelt)
c
 520  continue
c
c set upsoi[u,l], the actual soil water uptake rates from each
c soil layer due to transpiration in the upper and lower stories,
c for the soil model 
c
      do 600 k = 1, nsoilay
        do 610 i = 1, npoi
c
          upsoiu(i,k) = fvaput(i) * 2.0 * lai(i,2) * fu(i) *
     >                  stressu(i,k) / max (stresstu(i), epsilon)
c
          upsoil(i,k) = fvaplt(i) * 2.0 * lai(i,1) * fl(i) *
     >                  (1. - fi(i)) *
     >                  stressl(i,k) / max (stresstl(i), epsilon)
c
 610    continue
 600  continue
c
c set net evaporation from intercepted water, net evaporation
c from the surface, and net transpiration rates
c
      do 700 i = 1, npoi
c
c evaporation from intercepted water
c
        ginvap(i) = fvapuw(i) * 2.0 * lai(i,2) * fu(i) +
     >              fvaps (i) * 2.0 * sai(i,2) * fu(i) +
     >              fvaplw(i) * 2.0 * (lai(i,1) + sai(i,1)) * 
     >                                 fl(i) * (1. - fi(i))
c
c evaporation from soil and snow surfaces
c
        gsuvap(i) = fvapg(i)  * (1. - fi(i)) + fvapi(i)  * fi(i)
c
c transpiration
c
        gtrans(i) = fvaput(i) * 2.0 * lai(i,2) * fu(i) +
     >              fvaplt(i) * 2.0 * lai(i,1) * fl(i) * (1.-fi(i))
c
        gtransu(i) = fvaput(i) * 2.0 * lai(i,2) * fu(i)
        gtransl(i) = fvaplt(i) * 2.0 * lai(i,1) * fl(i) * (1.-fi(i))
c
 700  continue
c
      
      return
      end

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