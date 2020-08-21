#=======================================================================
#  TABEX, Function
#
#  Look up utility routine
#-----------------------------------------------------------------------
#  FUNCTION TABEX(VAL,ARG,DUMMY,K)
#TODO: VERIFICAR

TABEX <- function (VAL,ARG,DUMMY,K) {
  
  #IMPLICIT NONE
  #INTEGER K,J
  
  #REAL VAL(K),ARG(K),DUMMY,TABEX
  J <- 2
  while(DUMMY > ARG[J] && J <= K) J <- J + 1
  
  return ( (DUMMY-ARG[J-1])*(VAL[J]-VAL[J-1])/(ARG[J]-ARG[J-1])+VAL[J-1] )
  
}
#END FUNCTION TABEX


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# CURV function
# from 'Utilities/UTILS.for'

#FUNCTION CURV(CTYPE,XB,X1,X2,XM,X)
#
#CHARACTER*3 CTYPE
#REAL CURV,XB,X1,X2,XM,X
#
#CURV = 1.0
#IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN

CURV <- function(CTYPE,XB,X1,X2,XM,X) {
  
  CURV = 1.0
  
  # orignal code says if NON returns 1.0 
  #  IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN
  
  #-------------------------------------------------------------------------------
  #     Linear
  #-------------------------------------------------------------------------------
  if (CTYPE == 'LIN' | CTYPE == 'lin') {
    CURV = 0.
    if (X > XB & X < X1) {
      CURV = (X-XB)/(X1-XB)
    }
    if (X >= X1 & X <= X2) {
      CURV = 1.
    }
    if (X > X2 & X < XM) {
      CURV = 1.0 - (X-X2)/(XM-X2)
    }
    CURV = max(CURV,0.0)
    CURV = min(CURV,1.0)
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  #     Quadratic
  #-------------------------------------------------------------------------------
  if(CTYPE == 'QDR' | CTYPE == 'qdr') {
    CURV = 0.
    if (X > XB & X < X1) {
      CURV = 1. -((X1-X)/(X1-XB))^2
    }
    if(X >= X1 & X <= X2) {
      CURV = 1.
    }
    if(X > X2 & X < XM) {
      CURV = 1. - ((X-X2)/(XM-X2))^2
    }
    CURV = max(CURV,0.0)
    CURV = min(CURV,1.0)
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  #     Curve type INL is the inverse linear with a minimum for use in photoperiod
  #     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
  #-------------------------------------------------------------------------------
  if(CTYPE == 'INL' | CTYPE == 'inl') {
    CURV = 1.0
    if(X > X1 & X < X2) {
      CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
    }
    if(X >= X2) {
      CURV = XM       #CHP per Stu Rymph 10/8/2004
    }
    CURV = max(CURV,XM)
    CURV = min(CURV,1.0)
  }
  
  #-------------------------------------------------------------------------------
  #     Curve type SHO for use with short day plants.
  #     The curve is the inverse linear with a minimum for use in photoperiod
  #     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
  #-------------------------------------------------------------------------------
  if (CTYPE == 'SHO' | CTYPE == 'sho') {
    if (X <= X1) {
      CURV = 1.0
    } else if ((X > X1) & (X < X2)) {
      CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
    } else if (X >= X2) {
      CURV = XM
    }
    CURV = max(CURV,XM)
    CURV = min(CURV,1.0)
  }
  
  #-------------------------------------------------------------------------------
  #     Curve type LON for use with long day plants.
  #     The curve is the inverse linear with a minimum for use in photoperiod
  #     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
  #-------------------------------------------------------------------------------
  if(CTYPE == 'LON' | CTYPE == 'lon') {
    if (X < X2) {
      CURV = XM
    } else if ((X >= X2) & (X < X1)) {
      CURV = 1.-(1.-XM)*((X1-X)/(X1-X2))
    } else {
      CURV = 1.0
    }
    CURV = max(CURV,XM)
    CURV = min(CURV,1.0)
  }
  
  #-------------------------------------------------------------------------------
  #
  #-------------------------------------------------------------------------------
  if(CTYPE == 'SIN' | CTYPE == 'sin') {
    CURV = 0.
    if (X > XB & X < X1) {
      CURV = 0.5*(1.+COS(2.*22./7.*(X-X1)/(2.*(X1-XB))))
    }
    if(X >= X1 & X <= X2){
      CURV = 1.
    }
    if(X > X2 & X < XM) {
      CURV = 0.5*(1.+COS(2.*22./7.*(X2-X)/(2.*(XM-X2))))
    }
    CURV = max(CURV,0.0) #todo checar se ambos fecham dps do if anterior
    CURV = min(CURV,1.0)
  }
  
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------C     Curve type REV - Reversible process - used for cold hardening
  #	Rate of cold hardening increases as TMIN decreases from X1 to XB
  #	Cold hardening reverses at an increasing rate as TMIN increases from X1 to X2
  #     Process at maximum rate at or below XB
  #	Rate decreases linearly to 0 at X1
  #	Process reverses at a linear rate from X1 to X2
  #	XM is the maximum absolute rate
  #-------------------------------------------------------------------------------
  if(CTYPE == 'REV' | CTYPE == 'rev') {
    CURV = 1.
    if(X > XB & X < X1) {
      CURV = 1.0-((X-XB)/(X1-XB))
    }
    if(X >= X1 & X <= X2) {
      CURV = 0.0-((X-X1)/(X2-X1))
    }
    if(X > X2 ) {
      CURV = -1.0
    }
    CURV = max(CURV,-1.0) #todo checar se ambos fecham dps do if anterior
    CURV = min(CURV,1.0)
    CURV = CURV * XM
  }
  
  #-------------------------------------------------------------------------------
  #     Curve type DHD - used for cold dehardening in spring
  #	No cold dehardening below XB (rate=0)
  #	Rate of cold dehardening increases as TMIN increases from XB to X1
  #     Process at maximum rate at or above X1
  #	X2 is not used
  #	XM is the maximum absolute rate
  #-------------------------------------------------------------------------------
  if(CTYPE == 'DHD' | CTYPE == 'dhd') {
    CURV = 0.
    if(X > XB & X < X1) {
      CURV = (X-XB)/(X1-XB)
    }
    if(X >= X1 & X <= X2) {
      CURV = 1
    }
    if(X > X2 ) {
      CURV = 1
    }
    CURV = max(CURV,0.0)
    CURV = min(CURV,1.0)
    CURV = CURV * XM
  }
  
  #-------------------------------------------------------------------------------
  #     Curve type DRD - used for reducing rates of processes as dormancy advances
  #	Multiply rates by this factor to reduce them on short days,
  #	no effect on long days
  #	XM is the maximum reduction factor at full dormancy (daylength=XB)
  #	Less reduction as daylength gets longer
  #     Process at maximum rate at or above X1
  #	X2 is not used
  #-------------------------------------------------------------------------------
  if(CTYPE == 'DRD' | CTYPE == 'drd') {
    CURV = X2
    if(X > XB & X < X1) {
      CURV = X2+(XM-X2)*(X-XB)/(X1-XB)
    }
    if(X >= X1 ){
      CURV = XM
    }
    CURV = max(CURV,X2)
    CURV = min(CURV,XM)
  }
  
  #-------------------------------------------------------------------------------
  #    Curve type CDD - used for reducing rates of processes as dormancy advances
  #	Multiply rates by this factor to reduce them on short days,
  #	Long day effect depends on value of XM
  #	X2 is the maximum reduction factor at full dormancy (daylength=XB)
  #	Less reduction as daylength gets longer
  #    Process at maximum rate at or above X1
  #	Curvilinear version of DRD
  #-------------------------------------------------------------------------------
  if(CTYPE == 'CDD' | CTYPE == 'cdd') {
    CURV = X2
    if(X > XB & X < X1){
      CURV = XM-((XM-X2)*((X1-X)/(X1-XB))^2)
    }
    if(X >= X1) {
      CURV = XM
    }
    CURV = max(CURV,X2)
    CURV = min(CURV,XM)
  }
  
  #-------------------------------------------------------------------------------
  #	Curve type EXK - generic exponential function with "k"
  #	XB sets the amplitude of the curve (max Y value)
  #	X1/XM sets the amount of curvature (k) and shape of the curve (+ or -)
  #	X2 shifts the curve left (- X2) or right (+X2) on the X axis
  #	If X1/XM is positive, X2 is the X-intercept
  #-------------------------------------------------------------------------------
  if(CTYPE == 'EXK' | CTYPE == 'exk') {
    CURV = XB - EXP(X1*(X-X2)/XM)
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  #     Curve type VOP - Variable Order Polynomial
  #	Polynomial order (quadratic, cubic, etc.) is continuously variable
  #	(not discete steps)
  #	XB=T0, the lower temperature where the function scales to 0
  #	XM=T0, the upper temperature where the function scales to 0 #TO'
  #	X1=Tref, reference temperature at which the functio scales to 1.0
  #	X2=qft, variable that sets the order of the polynomial
  #	Set X2=1 the function is quadratic, X2=2 cubic, X2=3 quartic, etc.
  #     X2 does not have to be an integer
  #	Function scales to 0 below XB and above XM
  #	Minimum CURV value =0.0, maximum can exceed 1.0
  #	Can use mft, a multiplier, to set the scale of the function
  #	Read mft in from file and apply in main section of code (ex. mft*CURV)
  #-------------------------------------------------------------------------------
  if(CTYPE == 'VOP' | CTYPE == 'vop') {
    CURV=0.0
    if(X > XB & X < XM) {
      CURV = (((X-XB)^X2)*(XM-X))/(((X1-XB)^X2)*(XM-X1))
    }
    if(X >= XM ) {
      CURV = 0.0
    }
    CURV = max(CURV,0.0)
  }
  
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  #     Curve type Q10 - basic Q10 function
  #	XB=Tref, reference temperature
  #	X1=k, te response at Tref
  #	X2= Q10 increase in the response for every 10�K increase in temperature
  #	XM is not used
  #-------------------------------------------------------------------------------
  if(CTYPE == 'Q10' | CTYPE == 'q10') {
    CURV=X1*(X2^((X-XB)/10))
  }
  
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  # Curve type PWR - basic function raising X to some power with scaling
  #	XB=multiplier for main function
  #	X1=power to raise X to
  #	X2= scaling multiplier to scale reults to a given range
  #	XM is not used
  #	Added condition for negative values of X - was generating NaN with
  #	negative values of X and fractional vlaues of X1
  #	(ex. Temp=-1.8C and X1=1.5905).  Now uses value for 0.0 when X<0.0
  #-------------------------------------------------------------------------------
  if(CTYPE == 'PWR' | CTYPE == 'pwr') {
    if (X < 0.0) {
      CURV=X2*XB*(0^X1)
    } else {
      CURV=X2*XB*(X^X1)
    }
  }
  
  #-------------------------------------------------------------------------------
  
  
  return(CURV)
}

