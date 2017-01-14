## The cachesolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cachesolve should retrieve the inverse from the cache.
## It is assumed that the matrix supplied to the makeCacheMatrix 
## is always invertible

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## To see this with a little error handling,
    ## run install.packages("matrixcalc") 
    ## [if it is not already installed]
    ## and uncomment the code below
    
    ## invertible.error(x)

    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) im <<- invmatrix
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of an invertible matrix or 
## returns the cached inverse if one exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinv(im)
    return(im)
}

## This function checks if a matrix is invertible
## if not, stop is called

invertible.error <- function(x) {
    library(matrixcalc)
    if (is.singular.matrix(x)) {
        stop("This matrix is not invertible!")
    } 
}
