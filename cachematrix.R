# the following functions sorta take the example mean functions, hollow
# them out, and swap in some stuff to compute the inverse of a matrix.
# something gets cached along the way too.


# this here function makes the thing out of the other thing

makeCacheMatrix <- function(A = matrix()) {

  A_inv <- NULL
  
  set <- function(B) {
    A <<- B
    A_inv <<- NULL
  }
  get <- function() A
  set_inv <- function(inv) A_inv <<- inv
  get_inv <- function() A_inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}


# this one right here does the other part.  the prefilled comment said:
# "Return a matrix that is the inverse of 'x'".  we'll assume it does that.

cacheSolve <- function(thing, ...) {

  A_inv <- thing$get_inv()
  
  if (!is.null(A_inv)) {
    message("gettin', boss")
    return(A_inv)
  }
  
  A <- thing$get()
  A_inv <- solve(A)
  thing$set_inv(A_inv)
  A_inv
  
}


# some code entered into the command line:

# > source("cachematrix.R")
# > M <- matrix(1:4, 2, 2)
# > M
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > thang <- makeCacheMatrix(M)
# > thang$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > thang$get_inv()
# NULL
# > cacheSolve(thang)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > thang$get_inv()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > solve(M)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > N <- matrix(5:8, 2, 2)
# > thang$set(N)
# > thang$get_inv()
# NULL
# > cacheSolve(thang)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > solve(N)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

