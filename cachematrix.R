## makeCacheMatrix returns an object with 2 fields: 
##    1) x: the matrix
##              setter: get()
##            accessor: either constructor, or set(y)
##    2) inv: the inverse of x
##              setter: setinv(newInv)
##            accessor: getinv()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newInv) inv <<- newInv
    getinv <- function() inv
    list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)

}


## Takes an object create with makeCacheMatrix and returns its inverse
## (calculated only the first time and cached for subsequent use)
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
