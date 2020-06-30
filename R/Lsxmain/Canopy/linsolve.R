linsolve <- function (arr, rhs, vec, mplate, nd) {
  # solves multiple linear systems of equations, vectorizing
  # over the number of systems. basic gaussian elimination is 
  # used, with no pivoting (relies on all diagonal elements
  # being and staying significantly non - zero)
  #
  # a template array mplate is used to detect when an operation 
  # is not necessary (element already zero or would add zeros),
  # assuming that every system has the same pattern of zero
  # elements
  #
  # this template is first copied to mplatex since it 
  # must be updated during the procedure in case an original - zero
  # pattern location becomes non - zero
  #
  # the first subscript in arr, rhs, vec is over the multiple
  # systems, and the others are the usual row, column subscripts
  #
  # Arguments (input - output)
  
  ndx <- 9
  
  mplatex <- matrix(0, ndx, ndx)
  
  f <- 0
  
  if(nd > ndx) {
    stop(sprintf("Number of linsolve eqns %i exceeds limit %i", nd, ndx))
  }
  
  # copy the zero template so it can be changed below
  
  for(j in 1:nd) { 
    for(i in 1:nd) { 
      mplatex[i,j] <- mplate[i,j]
    }
  }
  
  # zero all array elements below the diagonal, proceeding from
  # the first row to the last. note that mplatex is set non - zero
  # for changed (i,j) locations, in loop 20
  
  for(id in 1:(nd - 1)) { 
    for(i in (id + 1):nd) { 
      if(mplatex[i,id] != 0) {
          f <- arr[i,id] / arr[id,id]
        for(j in id:nd) { 
          if(mplatex[id,j] != 0) {
            arr[i,j] <- arr[i,j] - f * arr[id,j]
            mplatex[i,j] <- 1
          }
        }
        
        rhs[i] <- rhs[i] - f * rhs[id]
        
      }
      
    }
  }
  
  # all array elements below the diagonal are zero, so can
  # immediately solve the equations in reverse order
  for(id in nd:1) { 
    f <- 0
    if(id < nd) {
      for(j in (id + 1):nd) {
        if(mplatex[id,j] != 0) {
          f <- f + arr[id,j] * vec[j]
        }
      }
    }
    
    vec[id] <- (rhs[id] - f) / arr[id,id]
    
  }
  
  return(list(vec = vec, rhs = rhs))
}