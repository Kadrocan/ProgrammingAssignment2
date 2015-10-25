## These funtions are for caching inverse of a matix which is a potentially
## time-consuming computation. Instead of computing twice, they cache the result
## and use it if matrix is same and inverse is already computed.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
            x <<- y
            cachedInverse <<- NULL
    }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invFunc <- x$getInverse()
        if(!is.null(invFunc)) {
            message("getting cached data")
            return(invFunc)
    }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}