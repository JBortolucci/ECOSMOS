# ---------------------------------------------------------------------
# polynomials for svp(t), d(svp)/dt over water and ice are from
# lowe(1977),jam,16,101-103.
# ---------------------------------------------------------------------
asat0 <- 6.1078000
asat1 <- 4.4365185e-1
asat2 <- 1.4289458e-2
asat3 <- 2.6506485e-4
asat4 <- 3.0312404e-6
asat5 <- 2.0340809e-8
asat6 <- 6.1368209e-11

bsat0 <- 6.1091780
bsat1 <- 5.0346990e-1
bsat2 <- 1.8860134e-2
bsat3 <- 4.1762237e-4
bsat4 <- 5.8247203e-6
bsat5 <- 4.8388032e-8
bsat6 <- 1.8388269e-10

csat0 <- 4.4381000e-1
csat1 <- 2.8570026e-2
csat2 <- 7.9380540e-4
csat3 <- 1.2152151e-5
csat4 <- 1.0365614e-7
csat5 <- 3.5324218e-10
csat6 <- -7.0902448e-13

dsat0 <-  5.0303052e-1
dsat1 <-  3.7732550e-2
dsat2 <-  1.2679954e-3
dsat3 <-  2.4775631e-5
dsat4 <-  3.0056931e-7
dsat5 <-  2.1585425e-9
dsat6 <-  7.1310977e-12

# ---------------------------------------------------------------------
# statement functions tsatl,tsati are used below so that lowe's
# polyomial for liquid is used if t gt 273.16, or for ice if 
# t lt 273.16. also impose range of validity for lowe's polys.
# ---------------------------------------------------------------------
# TODO: verify
tsatl <- function(t) {
  min(100, max(t-273.16, 0))
}

# TODO: verify
tsati <- function(t) {
  max (-60, min(t-273.16, 0))
}

# ---------------------------------------------------------------------
# statement function esat is svp in n/m**2, with t in deg k.
# (100 * lowes poly since 1 mb = 100 n/m**2.)
# ---------------------------------------------------------------------
# TODO: verify
esat <- function(t) {
  100 * (
    ifelse(t >= 273.16, asat0, bsat0) + 
      tsatl(t) * (asat1 + tsatl(t) * (asat2 + tsatl(t)*(asat3 + tsatl(t) * (asat4 + tsatl(t)*(asat5 + tsatl(t)* asat6))))) + 
      tsati(t) * (bsat1 + tsati(t) * (bsat2 + tsati(t)*(bsat3 + tsati(t) * (bsat4 + tsati(t)*(bsat5 + tsati(t)* bsat6)))))
  )
}
# ---------------------------------------------------------------------
# statement function desat is d(svp)/dt, with t in deg k.
# (100 * lowes poly since 1 mb = 100 n/m**2.)
# ---------------------------------------------------------------------
# TODO: verify
desat <- function(t) {
  100 * (
    ifelse(t >= 273.16, csat0, dsat0) + 
      tsatl(t)*(csat1 + tsatl(t)*(csat2 + tsatl(t)*(csat3 + tsatl(t)*(csat4 + tsatl(t)*(csat5 + tsatl(t)* csat6))))) +
      tsati(t)*(dsat1 + tsati(t)*(dsat2 + tsati(t)*(dsat3 + tsati(t)*(dsat4 + tsati(t)*(dsat5 + tsati(t)* dsat6)))))
  )
}
# ---------------------------------------------------------------------
# statement function qsat is saturation specific humidity,
# with svp e1 and ambient pressure p in n/m**2. impose an upper
# limit of 1 to avoid spurious values for very high svp
# and/or small p1
# ---------------------------------------------------------------------
# TODO: verify
qsat <- function(e1, p1) {
  0.622 * e1 / max ( p1 - (1.0 - 0.622) * e1, 0.622 * e1 )
}

# ---------------------------------------------------------------------
# statement function dqsat is d(qsat)/dt, with t in deg k and q1
# in kg/kg (q1 is *saturation* specific humidity)
# ---------------------------------------------------------------------
# TODO: verify
dqsat <- function(t, q1) {
  desat(t) * q1 * (1. + q1*(1./0.622 - 1.)) / esat(t)
}

# ---------------------------------------------------------------------
# statement functions hvapf, hsubf correct the latent heats of
# vaporization (liquid-vapor) and sublimation (ice-vapor) to
# allow for the concept that the phase change takes place at
# 273.16, and the various phases are cooled/heated to that 
# temperature before/after the change. this concept is not
# physical but is needed to balance the "black-box" energy 
# budget. similar correction is applied in convad in the agcm
# for precip. needs common comgrd for the physical constants.
# argument t is the temp of the liquid or ice, and tair is the
# temp of the delivered or received vapor.
# ---------------------------------------------------------------------

# TODO: verify
hvapf <- function(t,tair) {
  hvap + cvap*(tair-273.16) - ch2o*(t-273.16)
}

# TODO: verify
hsubf <- function(t,tair) {
  hsub + cvap*(tair-273.16) - cice*(t-273.16)
}











