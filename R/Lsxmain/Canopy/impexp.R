# epsilon  # small quantity to avoid zero-divides and other
# hfus     # latent heat of fusion of water (J kg-1)
# tmelt    # freezing point of water (K)

impexp <- function (wimp, tveg, ch, wliq, wsno, iter) {
  
  # sets the implicit vs explicit fraction in turvap calcs for
  # upper leaves, upper stems or lower veg. this is to account for
  # temperatures of freezing / melting intercepted h2o constrained
  # at the melt point. if a purely implicit calc is used for such
  # a surface, the predicted temperature would be nearly the atmos
  # equil temp with little sensible heat input, so the amount of
  # freezing or melting is underestimated. however, if a purely
  # explicit calc is used with only a small amount of intercepted
  # h2o, the heat exchange can melt / freeze all the h2o and cause
  # an unrealistic huge change in the veg temp. the algorithm
  # below attempts to avoid both pitfalls
  
  # for first iteration, set wimp to fully implicit, and return
  
  if(iter == 1) {
    wimp[] <- 1
    return(wimp)
  }
  
  # for second and subsequent iterations, estimate wimp based on
  # the previous iterations's wimp and its resulting tveg.
  #
  # calculate h, the "overshoot" heat available to melt any snow
  # or freeze any liquid. then the explicit fraction is taken to
  # be the ratio of h to the existing h2o's latent heat (ie, 100%
  # explicit calculation if not all of the h2o would be melted or
  # frozen). so winew, the implicit amount, is 1 - that ratio.
  # but since we are using the previous iteration's t * results
  # for the next iteration, to ensure convergence we need to damp
  # the returned estimate wimp by averaging winew with the 
  # previous estimate. this works reasonably well even with a
  # small number of iterations (3), since for instance with large
  # amounts of h2o so that wimp should be 0, a good amount of 
  # h2o is melted or frozen with wimp <- .25
  h <- ch * (tveg - tmelt)
  z <- max (abs(h), epsilon)
  
  winew <- 1
  
  if (h > epsilon)  winew <- 1 - min (1, hfus * wsno / z)
  if (h <  -epsilon) winew <- 1 - min (1, hfus * wliq / z)
  
  wimp <- 0.5 * (wimp + winew)
  
  return(wimp)
}