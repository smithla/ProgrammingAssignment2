# - updated for 2nd try at class

## makeCacheMatrix - this function acts as a wrapper for the provided
## matrix. It caches the matrix and provides a local variable for 
## the matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) {
    matInv <<- inv
  }
  getInverse <- function() matInv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve - this function takes a matrix wrapper function and
## returns the inverse of it's contained matrix. If the inverse has
## not been caculated yet, it is calculated and the inverse is stored
## in the wrapper. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInverse()
  if(!is.null(matInv)) {
    message("getting maxtrix inverse")
    return(matInv)
  }
  mat <- x$get()
  matInv <- solve(mat)
  x$setInverse(matInv)
  matInv 
}
