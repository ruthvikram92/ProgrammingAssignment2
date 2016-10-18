## The below 2 functions('makeCacheMatrix' and 'cacheSolve') computes the inverse of a matrix given as an argument to the function.
## The inverse is saved in the cache. If a user provides same matrix values later on, the saved cache value is returned without computation

##The function below will create and cache an inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  b <- NULL
  setMatrix <- function(b) {
    # Assigning the Variables b to x in the parent envirnoment
    x <<- b
    a <<- NULL
  }
  getMatrix <- function() x
  
  # Setting the cache value a to inverse of matrix x provided by the user 
  setInverse <- function(inverseVal) a <<- inverseVal
  
  # Returning the cached inverse of x(input)
  getInverse <- function() a
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)
}

##The below function, calculates the inverse, checks wether the matrix is in cache already. 
##If it already exists, then it returns the previosly caluclated inverse else computes it and returns it to the user.          
                   
cacheSolve <- function(x, ...) {
          a <- x$getInverse()
  
  # Check whether the matrix is already present in cache
  if(!is.null(a)) {
    if(x$setMatrix() == x$getMatrix())
      message("getting the cached data")
    return(a)
  }
  b <- x$getMatrix()
  x$setMatrix(b)
  a <- solve(b, ...)
  x$setInverse(a)
  a
        ## Finally returns the inverse matrix
}
