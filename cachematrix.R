## This is yborofsky's solution Programming Assignment 2.
## These functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function () x
        setMatrixInverse <- function(MatrixInverse) im <<- MatrixInverse
        getMatrixInverse <- function() im
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache (and say, "getting cached data").

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getMatrixInverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setMatrixInverse(im)
        im
}

