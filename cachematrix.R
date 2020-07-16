## Two functions that cache the inverse of a matrix.

## This function returns a matrix object the can cache is inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solvematrix) inv <<- solvematrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of the matrix returned from makeCachMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          print("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
