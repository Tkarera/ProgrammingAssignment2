makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix  creates a special “matrix” object that can cache its inverse
  ## x is a square matrix as the ordinary inverse is defined only for square matrices.
  ## behind the scene, makeChacheMatrix will return a list containing functions to:
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  ## the list is used as the input to cacheSolve() 
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ##cacheSolve will output the inverse of the “matrix” returned by makeCacheMatrix() 
  inv <- x$getInverse()
  ## sets the value of the inverse in the cache via the setinv function.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
