##===========================================================================
## This file contains two functions: 
##   1. makeCacheMatrix is a helper function that facilitates caching.
##   2. cacheSolve takes a list returned by makeCacheMatrix and calculates the
##      inverse of a matrix on its first invocation.
##===========================================================================
##===========================================================================
## This function is a helper function that facilitates caching of lengthy
## computations.
##===========================================================================
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##===========================================================================
## This function caches the inverse of a function the first time it is invoked.
## Subsequent invocations will returned the cached value.
##===========================================================================
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

