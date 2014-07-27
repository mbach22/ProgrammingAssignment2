## These functions allow the user to cache the inverse of a matrix, to allow retrieval of the original result
## rather than continuously repeating the computation.

## This function takes a matrix and returns a "cache-able" matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ##If you change the matrix, it makes sure to force you to recompute the inverse 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ##Returns the matrix
    get <- function() x
    
    ##Sets the inverse
    setInverse <- function(inverse) i <<- inverse
    
    ##Retrieves the inverse
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function either computes the inverse of a "cache-able" matrix, or returns the already computed inverse.

cacheSolve <- function(x, ...) {
    ##Retrieves the inverse of x
    i <- x$getInverse()
    
    ##The inverse will be null if it has not been calculated yet. This runs if i has been set
    if(!is.null(i)) {
        message("Retrieving already calculated inverse")
        return(i)
    }
    
    ##Otherwise retrieves the matrix and calculates the inverse
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
}
