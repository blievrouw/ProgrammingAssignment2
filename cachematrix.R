## Functions for caching of matrix computations (e.g. inverse)

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inversedMatrix) inverse <<- inversedMatrix
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inversedMatrix <- x$getInverse()
    if (!is.null(inversedMatrix)) {
        print("Getting cached inverse matrix")
        return(inversedMatrix)
    }
    originalMatrix <- x$get()
    inversedMatrix <- solve(originalMatrix, ...)
    x$setInverse(inversedMatrix)
    inversedMatrix
}
