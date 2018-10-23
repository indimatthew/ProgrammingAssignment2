## The goal of this function set is to create a cache of the inverse of a matrix, 
##  in order to avoid repeatedly computing the matrix inverse unnecessarily.  

## macheCacheMatrix is a function that takes a matrix as its input and creates 
## a set of functions to set and return the matrix inverse.  
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  #Sets the  matrix x in the parent environment to the argument, aMatrix.
  #Sets the matrix inverse value to NULL. 
  set <- function(aMatrix) {
    x <<- aMatrix
    xInverse <<- NULL
  }
  
  #Returns the matrix x
  get <- function() x
  
  #Sets the inverse value of the matrix
  setInverse <- function(inv) xInverse <<- inv
  
  #Returns the value of the matrix inverse
  getInverse <- function() xInverse
  
  #Creates a list of these matrix functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function takes a matrix x as its argument and returns the 
## inverse of the matrix. If the matrix inverse value has already been set, 
##  it returns the cached matrix inverse.  Otherwise, the inverse of the matrix is 
##  computed and returned. 
cacheSolve <- function(x, ...) {

  #Get the matrix inverse 
  xInverse <- x$getInverse()
  
  #If a matrix inverse value exists, return cached value
  if(!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  #If a xInverse is NULL, compute and return the matrix inverse
  data <- x$get()
  xInverse <- solve(data, ...)
  x$setInverse(xInverse)
  xInverse
}