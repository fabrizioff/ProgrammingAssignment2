## Caching the Inverse of a Matrix in order to get
## benefits during computation

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
