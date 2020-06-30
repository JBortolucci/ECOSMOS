# plant functional types:
#
# 10) deciduous shrubs

DeciduousShrubsExistence <- function() {
  
  i <- 1
  # determine which plant types can exist in a given gridcell

  # 10) deciduous shrubs
  # gdd0 > 100.0
  if (gdd0 > GDD[i])  exist[i] <- 1
  
  # == C. Kucharik 6.12.01 ==
  # if override natural vegetation competition (overveg = 1)
  # this code is used to override existence parameterizations for potential
  # vegetation distribution based on climatic constraints. Instead
  # we only allow PFTs to compete in each grid cell
  # based on land cover dataset and classification found in that region
  # override those pfts that are not desired but might have exist = 1.0
  # from above initialization - this essentially limits vegetation competition
  # during spin-up periods so that vegetation growing there is confined to
  # what is typically observed today (potential vegetation).  If doing
  # climate change scenarios, overveg should be set to 0 so full
  # vegetation dynamics are used, if desired.
  if(overveg == 1) {

    inveg <- xinveg

    if(inveg>=2 && inveg<=13) exist[i] <- 0

    if(inveg == 11) {

      exist[i] <- 1

          } else if(inveg == 12) {

      exist[i]   <- 1

    } else if(inveg == 13) {

      exist[i]  <- 1

    }

  } # endif

  # == SV. Cuadra ==
  # grassland are currently applyed everywhere
  if(ipast > 0) {
    exist[i] <- 0
  }

  if(cropsums > 0) exist[i] <- 0
  
  assign("exist",  exist, envir = env)
}


