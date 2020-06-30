###########################################
#  subroutine irrigation(iday, imonth)
###########################################
# routine that calculates the amount of water that is applied to a
# managed ecosystem on a daily basis (mm / day)
#
# based on average daily water content in the soil
#
# this amount will be evenly applied in timestep increments (start and stop time)
# based on the duration of the event - similar to the way precipitation
# events are handled

# Global Vars:
# a5td(npoi)               # 5-day average daily air temperature (K)
# adwisoilay(npoi,nsoilay) # daily average soil ice content for each soil layer
# adwsoilay(npoi,nsoilay)  # daily average soil moisture for each soil layer
# hsoi(nsoilay+1)          # soil layer thickness (m)
# cropsums(npoi)           # index - number of crop types planted in each grid cell
# npoi                     # total number of land points
# poros(npoi,nsoilay)      # porosity (mass of h2o per unit vol at sat / rhow)
# sfield(npoi,nsoilay)     # field capacity soil moisture value (fraction of pore space)
# swilt(npoi,nsoilay)      # wilting soil moisture value (fraction of pore space)
# tmin(npoi)               # minimum daily temperature (K)
# totirrig(npoi)           # annual total irrigation applied (mm/yr)
# xirrig(npoi)             # irrigated water application rate (mm/day) to crops

irrigation <- function (iday, imonth) {
  

  if(iday  ==  1  &&  imonth  ==  1) {
    totirrig <- 0.0
  }
  
  awcmax <- 0.0
  awc    <- 0.0 
  
  # top 5 soil layers <- 100 cm 
  for(k in 1:5) { 
    # calculate the maximum available water in top 1 m (root zone) at field capacity
    awcmax <- awcmax + max(0.0, (sfield[k] - swilt[k])) * hsoi[k] * poros[k] * 100
    # calculate actual amount of water available to plant - current water content
    # based on daily average water and ice content in soil layers down to a meter 
    awc <- awc + max(0.0, (adwisoilay[k] + (1. - adwisoilay[k]) * adwsoilay[k]) - swilt[k]) * hsoi[k] * poros[k] * 100
  }    
  
  #    irrigation will occur if :
  #
  # * the crop has been planted
  # * the minimum daily temperature is > 5 C
  # * the 5 - day running mean temperature is > 10 C 
  # * the actual soil water content in the top 1 m of soil
  #    is less than or equal to 50% of the maximum value at field capacity,
  
  if( (awc  <=  0.50 * awcmax) && (tmin  >  278.16  &&  a5td >  283.16)  ) {
    #
    # irrigation (applied above the canopy)  is used to make up the difference to field capacity
    # convert awc values from cm to mm - this is a per day value 
    # so it is consistent with the values used in weather.f (diurnal) for precipitation 
    #
    # set upper limit based on literature search typical of what a farmer could
    # apply in a typical day of irrigation and what rates of application are
    xirrig <- min(150.0, max(0.0, awcmax - awc) * 10.0)
  } else { 
    xirrig <- 0.0
  }
  
  
  assign("totirrig", totirrig, envir = env)
  assign("xirrig", xirrig, envir = env)
}
