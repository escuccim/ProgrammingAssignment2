## Functions to get and cache the inverse of a matrix

## Creates an object containing a matrix and it's cached inverse
## Returns a list of functions to perform the operations to set and get the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## For a CacheMatrix will return the cached inverse, if it exists.
## Otherwise will generate the inverse, cache it and return it.
## For non-invertible matrices will print error and return NA

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- tryCatch({
        solve(data, ...)
    }, error = function(error){
        print(error)
        return(NA)
    })
    x$setInverse(inv)
    inv
}
