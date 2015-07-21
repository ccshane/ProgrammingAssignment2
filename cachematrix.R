## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##message("starting to solve")
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve,x) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, 
        setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## attempt to get the inverse from the cache
    m <- x$getsolve()
    ## if the inverse is collected from the cache, return it
    if (!is.null(m)) {
        message("getting cached result")
        return(m)
    }
    ## otherwise, need to calculate the inverse...
    data <- x$get()
    ## and cache the result..
    m <- solve(data, ...)
    x$setsolve(m)
    ## and return the result
    m
}
