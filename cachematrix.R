## Matrix Inverse Cache
## Matrix inversion consumes a lot of computation power but caching it makes a shortcut to compute it.
## Here are some codes that do the trick

## This is a function that creates a matrix object to cache the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function tries to compute the inverse of the matrix object created on the function above.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
