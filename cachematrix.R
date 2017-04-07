## R Programming - programming assignment #2
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## 
## These functions calculate the inverse of a square matrix.
##
## The cpu-intensive inverse is calculated once, and stored.  If requested
##  to compute again, the stored result is returned.
##
## This "memoize" or "caching" concept trades off memory for cpu time.

## Create a matrix inverse cache list
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculate (the first time only) or retrieve the matrix inverse
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
