## These functions will help to cache a matrix inversion. We will develop two funtions 
## to support this functionality

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the cache value
        matrixInversion <- NULL
        
        # Function to set the passed in variable
        set <- function(y) {
                x <<- y
                matrixInversion <<- NULL
        }
        
        # Function to get the variable
        get <- function() x
        
        # Function to set the cached value
        setMatrixInvertion <- function(solve) {
                matrixInversion <<- solve
        }
        
        # Function to get the cached value
        getMatrixInvertion <- function() {
                matrixInversion
        }
        
        # List of functions that will be retured
        list(set = set, get = get,
             setMatrixInvertion = setMatrixInvertion,
             getMatrixInvertion = getMatrixInvertion)
        
}


## This function checks if the inverted matrix exists in cache
## If it does it returns the value
## If not it calculates the value using the solve function and stores it in cache
## During the first exection there will be a "data not yet in cache" message
## Subsequent executions will see the message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                message("data not yet in cache")  
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInvertion(m)
        m
}
