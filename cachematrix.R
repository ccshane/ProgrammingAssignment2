## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##message("starting to solve")
    m <- NULL
    ## cache the function for later retrieval
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## retrieve the function from the cache
    get <- function() x
    ## cache the result of the inverse for later retrieval
    setsolve <- function(solve,x) m <<- solve
    ## retrieve the inverse
    getsolve <- function() m
    ## the list of available functions
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
