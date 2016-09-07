## makeCacheMatrix is used to store matrix and its inverse
## cacheSolve is used to compute a matrix's invese if it is not computed before

## makeCacheMatrix takes in x which is a matrix. Its inverse is stored in the variable i which is defaulted with value NULL

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #the inverse is set to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, 
	 get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is used to compute the inverse of the input matrix, generated using makeCacheMatrix
## If the inverse was not previously computed, it would be computed and stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
