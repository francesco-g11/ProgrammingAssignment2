## This couple of functions allows skipping recalculation of the inverse matrix
## when it is already known and no item has been changed on the initial matrix

## This first function create the special matrix needed to run.
## It's actually a list, with room to cache the inverse, if already calculated.
## It contains functions to put and retrieve the base matrix (set/get) and the same
## for the inverse (setinverse/getinverse)
## Notice The set function, on the body, that NULLifies the ivs variable in order to
## force new recalculation when the base matrix changes

makeCacheMatrix <- function(x = matrix()) {

    ivs <- NULL
    set <- function(y) {
        x <<- y
        ivs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ivs <<- inverse
    getinverse <- function() ivs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## This function calculate the inverse of the special matrix passed as parameter,
## only if we never did before or in case some items change.
## The key is testing the ivs variable, coming from the "special" getinverse function
## If not NULL (see also first function), then no calculation is needed because the inverse
## matrix is cached. We only need to get its value from ivs. Otherwise, the solve function
## gives the inverse and store it into ivs for future use.
## In both cases the result comes to the user (with return function or directly)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ivs <- x$getinverse()
    if(!is.null(ivs)) {
        message("getting cached data")
        return(ivs)
    }
    data <- x$get()
    ivs <- solve(data, ...)
    x$setinverse(ivs)
    ivs
}
