## Functions solve an invertible matrix once, cache the result
## then maintain that cached result until given a new matrix to invert

## Provides functions for handling the caching and retrieval operations

makeCacheMatrix <- function(x = matrix()) {
        myinverse <- NULL
        set <- function(y) {
                x <<- y
                myinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) myinverse <<- inverse
        getinverse <- function() myinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Retrieves cached result if possible, otherwise calculates inverse
## and caches result. Returns inverse of matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myinverse <- x$getinverse()
        if(!is.null(myinverse)) {
                message("getting cached data")
                return(myinverse)
        }
        data <- x$get()
        myinverse <- solve(data, ...)
        x$setinverse(myinverse)
        myinverse
}
