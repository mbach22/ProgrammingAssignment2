## These functions allow the user to cache the inverse of a matrix, to allow retrieval of the original result
## rather than continuously repeating the computation.

## This function takes a matrix and returns a "cache-able" matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function either computes the inverse of a "cache-able" matrix, or returns the already computed inverse.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("Retrieving already calculated inverse")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
}
