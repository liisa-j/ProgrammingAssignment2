## The aim of these functions is to calculate an inverse of a matrix, 
## cache the result and in case the same calculation on the same matrix
## is required, the functions output the previously calculated and cached
## result, hence making the process faster. 

## Here we create a special "matrix" object that can cache its inverse
## matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function calculates the inverse matrix of the matrix outputted
## by the above function or, if the inverse has already been calculated
## and the matrix in unchanged, it will return the cached inverse matrix. 

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("Getting cached inverse matrix")
                return(inverseMatrix)
        }
        dat <- x$get()
        inverseMatrix <- solve(dat, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
        
        ## Return a matrix that is the inverse of 'x'
}
