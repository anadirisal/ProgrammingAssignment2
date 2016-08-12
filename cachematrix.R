## Put comments here that give an overall description of what your
## functions do

## This function provides an inverse of a matrix that is already in cache

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <-function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## If not in cache, then this function creates and inverse to a matrix by calling Solve


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if (!is.null(m)){
       message("Getting Inverse")
       return (m)
     }
     mat <- x$get()
     m <- solve(mat, ...)
     x$setInverse(m)
     m
}
