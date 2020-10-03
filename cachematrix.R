## makeCacheMatrix and cacheSolve are a pair of functions used for 
## catching the inverse of a matrix

############################################################################
## makeCacheMatrix
##
## Takes as input a matrix and creates a list of the following functions:
##
## set       funtion to set the matrix passed as an argument
## get       function to get the matrix previously set.
## setinv    function to set the inverse matrix via the solve function
## getinv    function to get the invrese matrix
##
## <<- operator is used to indicate R to search in all environments until it
## finds a variable with that name
##
## Note: Function assumes that an inversable matrix is passed as an argument
#############################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


############################################################################
## cacheSolve
##
## Checks if the inverse of a matrix has been previously calculated via the
## makeCacheMatrix function. If the inverse of a matrix has been stored, then
## the function returns it. If not, it calculates the inverse of a matrix via
## the solve function and returns it
#############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}