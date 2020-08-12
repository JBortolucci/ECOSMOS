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
