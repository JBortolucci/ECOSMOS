# ---------------------------------------------------------------------
#  subroutine existence  
# ---------------------------------------------------------------------
#
# this routine determines which plant functional types (pft's) are allowed
# to exist in each gridcell, based on a simple set of climatic criteria
# the logic here is based on the biome3 model of haxeltine and prentice
#
# plant functional types:
#
# 1)  tropical broadleaf evergreen trees
# 2)  tropical broadleaf drought-deciduous trees
# 3)  warm-temperate broadleaf evergreen trees
# 4)  temperate conifer evergreen trees
# 5)  temperate broadleaf cold-deciduous trees
# 6)  boreal conifer evergreen trees
# 7)  boreal broadleaf cold-deciduous trees
# 8)  boreal conifer cold-deciduous trees
# 9)  evergreen shrubs
# 10) deciduous shrubs
# 11) warm (c4) grasses
# 12) cool (c3) grasses
# 13) c3 crop (soybean)
# 14) c4 crop (corn)
# 15) c3 crop (wheat)
# 16) c4 crop (sugarcane)
# 17) c3 crop (Euca)
# 18) c4 crop (Oil Palm)
#
# ---------------------------------------------------------------------
# Globals:
#
# exist
# GDD
# gdd0
# gdd5
# imaize
# ipast
# irotation
# isgc
# isoybean
# iwheat
# 1
# overveg
# tcmin
# TminL
# TminU
# tw
# Twarm
# xinveg
# ---------------------------------------------------------------------

existence <- function() {
  
  # # initialize exist matrix with 0's
  # exist    <- matrix(0, nrow = 1, ncol = npft)
  # 
  # # determine which plant types can exist in a given gridcell
  # 
  # # 1) tropical broadleaf evergreen trees
  # # tcmin > 0.0
  # if (tcmin > TminL[1]) exist[1] <- 1
  # 
  # # 2) tropical broadleaf drought-deciduous trees
  # # tcmin > 0.0
  # if (tcmin > TminL[2]) exist[2] <- 1
  # 
  # # 3) warm-temperate broadleaf evergreen trees
  # # tcmin <   0.0 and
  # # tcmin > -10.0
  # if((tcmin < TminU[3]) && (tcmin > TminL[3])) exist[3] <- 1
  # 
  # # 4) temperate conifer evergreen trees
  # # tcmin <    0.0 and
  # # cmin >  -45.0 and
  # # gdd5  > 1200.0
  # if ((tcmin < TminU[4]) && (tcmin > TminL[4]) && (gdd5 > GDD[4])) exist[4] <- 1
  # 
  # 
  # # 5) temperate broadleaf cold-deciduous trees
  # # tcmin <    0.0 and
  # # tcmin >  -45.0 and
  # # gdd5  > 1200.0
  # if ((tcmin < TminU[5]) && (tcmin > TminL[5]) && (gdd5 > GDD[5])) exist[5]  <- 1
  # 
  # 
  # # 6) boreal conifer evergreen trees
  # # tcmin <  -45.0 or gdd5 < 1200.0, and
  # # tcmin >  -57.5 and
  # # gdd5  >  350.0
  # if(((tcmin < TminU[6]) || (gdd5 < GDD[4])) && (tcmin > TminL[6]) && (gdd5 > GDD[6]))  exist[6] <- 1
  # 
  # 
  # # 7) boreal broadleaf cold-deciduous trees
  # # tcmin <  -45.0 or gdd5 < 1200.0, and
  # # tcmin >  -57.5 and
  # # gdd5  >  350.0
  # if (((tcmin < TminU[7]) || (gdd5 < GDD[5])) && (tcmin > TminL[7]) && (gdd5 > GDD[7])) exist[7] <- 1
  # 
  # # 8) boreal conifer cold-deciduous trees
  # # tcmin <  -45.0 or gdd5 < 1200.0, and
  # # gdd5  >  350.0
  # if (((tcmin < TminU[8]) || (gdd5 < TminL[4])) && (gdd5 > GDD[8])) exist[8] <- 1
  # 
  # 
  # # 9) evergreen shrubs
  # # gdd0 > 100.0
  # if (gdd0 > GDD[9]) exist[9] <- 1
  # 
  # 
  # # 10) deciduous shrubs
  # # gdd0 > 100.0
  # if (gdd0 > GDD[10])  exist[10] <- 1
  # 
  # 
  # # 11) warm (c4) grasses
  # # tw   >  22.0 and
  # # gdd0 > 100.0
  # if ((tw > Twarm[11]) && (gdd0 > GDD[11])) exist[11] <- 1
  # 
  # 
  # # 12) cool (c3) grasses
  # # gdd0 > 100.0
  # if (gdd0 > GDD[12])  exist[12] <- 1
  # 
  # # == C. Kucharik 6.12.01 ==
  # # if override natural vegetation competition (overveg = 1)
  # # this code is used to override existence parameterizations for potential
  # # vegetation distribution based on climatic constraints. Instead
  # # we only allow PFTs to compete in each grid cell
  # # based on land cover dataset and classification found in that region 
  # # override those pfts that are not desired but might have exist = 1.0
  # # from above initialization - this essentially limits vegetation competition
  # # during spin-up periods so that vegetation growing there is confined to
  # # what is typically observed today (potential vegetation).  If doing 
  # # climate change scenarios, overveg should be set to 0 so full 
  # # vegetation dynamics are used, if desired.
  # if(overveg == 1) {
  #   
  #   inveg <- xinveg
  #   
  #   if(inveg>=2 && inveg<=13) exist[1:npft] <- 0      
  # 
  #   
  #   # wherever vegetation is allowed to exist, check the rules from above 
  #   # tropical deciduous
  #   if (inveg == 2) {
  # 
  #     exist[2] <- 1
  #     exist[1] <- 0
  # 
  #     #temperate conifers        
  #   } else if(inveg == 4) {
  #     
  #     exist[4] <- 1
  #     
  #     # temperate deciduous
  #   } else if(inveg == 5) {
  #     
  #     exist[5] <- 1
  #     
  #     
  #     # boreal conifers      
  #   } else if(inveg == 6) {
  #     
  #     exist[6] <- 1
  #     
  #     
  #     
  #     # boreal deciduous                
  #   } else if(inveg == 7) {
  #     
  #     exist[7:8]   <- 1
  #   
  #     
  #     # mixed forest exception:
  #     # let existence rules determine whether temperate or boreal species
  #     # can exist at this location
  #   } else if(inveg == 8) {
  #     
  #     exist[4:8]  <- 1
  # 
  #     # savana    
  #   } else if(inveg == 9) {
  #     
  #     exist[5]  <- 1
  #     exist[11] <- 1
  #     exist[12] <- 1
  #     
  #     
  #     # grassland
  #   } else if(inveg == 10) {
  #     
  #     exist[11] <- 1
  #     exist[12] <- 1
  #     
  #    
  #     
  #     # dense shrubland
  #   } else if(inveg == 11) {
  #     
  #     exist[9]  <- 1
  #     exist[10] <- 1
  #     exist[11] <- 1
  #     exist[12] <- 1
  #     
  #     
  #     # open shrubland
  #   } else if(inveg == 12) {
  #     
  #     exist[9:12]   <- 1
  # 
  #     # tundra
  #   } else if(inveg == 13) {
  #     
  #     exist[9:12]  <- 1
  #     
  #   }
  #   
  # } # endif
  # 
  # # == SV. Cuadra ==
  # # grassland are currently applyed everywhere
  # if(ipast > 0) {
  #   exist[1:npft] <- 0
  #   exist[11]   <- 1
  # }
  # 
  # 
  # 
  # if(cropsums > 0) exist[1:12] <- 0
  # 
  # #to do: Jair, pensar em como aliminar a dependencia do "j"
  # 
  # # 13) c3 crop - soybean
  # if (isoybean == 1) exist[13] <- 1
  # 
  # # 14) c4 crop - corn
  # if (imaize == 1) exist[14] <- 1
  # 
  # # 15) c3 crop - wheat (spring and winter varieties) 
  # if (iwheat > 0) exist[15] <- 1
  # 
  # # 16) c4 crop - sugarcane
  # if (isgc == 1) exist[16] <- 1
  # 
  # # 17) c3 crop - eucalyptus
  # if (ieuca == 1) exist[17] <- 1
  # 
  # # 18) c3 crop - oil palm
  # 
  # if (ipalm == 1) exist[18] <- 1
  
  exist[1] <- 1
  
  assign("exist",  exist, envir = env)

}


