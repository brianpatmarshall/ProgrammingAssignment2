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
        #--------------------------------------------------------------------
        # Retrieve the original matrix 
        #--------------------------------------------------------------------
        get <- function() x

        #--------------------------------------------------------------------
        # Store the cached inverse matrix
        #--------------------------------------------------------------------
        setsolve <- function(solve) m <<- solve

        #--------------------------------------------------------------------
        # Retrieve the cached inverse matrix
        #--------------------------------------------------------------------
        getsolve <- function() m
        #--------------------------------------------------------------------
        # return the list containing the 4 functions defined in this function
        #--------------------------------------------------------------------
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
        #--------------------------------------------------------------------
        # If the inverse is cached, return the cached value without re-
        # computing it.
        #--------------------------------------------------------------------
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #--------------------------------------------------------------------
        # The inverse matrix is not cached.  So retrieve the matrix ...
        #--------------------------------------------------------------------
        data <- x$get()
        #--------------------------------------------------------------------
        # ... compute this matrix's inverse (NOTE: we assume the matrix has
        # an inverse, and so the call to solve will NOT fail) ... 
        #--------------------------------------------------------------------
        m <- solve(data, ...)
        #--------------------------------------------------------------------
        # ... cache the inverse ...
        #--------------------------------------------------------------------
        x$setsolve(m)
        #--------------------------------------------------------------------
        # ... and finally return it.
        #--------------------------------------------------------------------
        m
}

