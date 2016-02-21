## The following functions help in speeding up the computation of inverse of a matrix using Lexical Scoping in R.
## If the input matrix does not change, the inverse of it is read from cache which speeds up the computation.
## The inverse is computed only if the input matrix changes.

##This function creates a list containg functions to 
##1. set the matrix
##2. get the matrix
##3. set the inverse of a matrix
##4. get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## The following function returns the inverse of the special vecotr created by the above function.
## It first checks if the inverse is already computed. If so, it returns the cached value without again performing
## the computation. If the inverse is not available in the cache, it does the computation and sets the inverse using setInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
