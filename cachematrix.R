## Matrix inversion is usually a costly computation. So the pair of functions 
## can cache the inverse of a matrix rather than compute it repeatedly.

## The first function, makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) im <<- inversion
    getinversion <- function() im
    list(set = set, get = get, 
         setinversion = setinversion,
         getinversion = getinversion)
}


## The following function cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinversion()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinversion(im)
    im
}
