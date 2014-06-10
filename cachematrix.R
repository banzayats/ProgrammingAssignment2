## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. 

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Initializing inverse of x to NULL
    inverse <- NULL
    # Matrix must be square
    if (ncol(x) != nrow(x)) {
        stop("Matrix must be square!")
    }
    # Subfunction for setting the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Subfunction for getting the matrix
    get <- function() x
    # Setting the calculated inverse of a matrix
    setinverse <- function(matrixInverse) inverse <<- matrixInverse
    # Getting the calculated inverse of a matrix
    getinverse <- function() inverse
    #
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Getting the calculated inverse of x
    inverse <- x$getinverse()
    ## Checking the value and returning it if it is not NULL
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    ## Otherwise:  
    ## Getting the matrix
    data <- x$get()
    ## Calculating the inverse of the matrix
    inverse <- solve(data, ...)
    # Setting the calculated inverse of the matrix
    x$setinverse(inverse)
    ## and returning result
    inverse
}
