
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a matrix containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via 
## the setmatrix function.

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
      }
      aMatrix <- x$get()
      m <- solve(aMatrix, ...)
      x$setmatrix(m)
      m
}
