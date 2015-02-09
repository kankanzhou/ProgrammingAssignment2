## Created By Kankan Zhou
## On 9 Feb 2015


makeCacheMatrix <- function(x = numeric()) { 
        ##  This function creates a special "matrix" object that can cache its inverse.

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


cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
		##If the inverse has already been calculated (and the matrix has not changed), 
		##then the cachesolve will retrieve the inverse from the cache.
        
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

