## These functions will help to cache a matrix inversion. We will develop two funtions 
## to support this functionality

## Steps to test 
## source('~/git/ProgrammingAssignment2/cachematrix.R')
## my_matrix <- matrix(1:4,2,2)
## my_matrix2<-makeCacheMatrix(my_matrix)
## my_matrix3<-cacheSolve(my_matrix2)
## When executed first time, it prints "data not yet in cache"
## Subsequent executions will print getting cached data
## The final test will be to multiply my_matrix %*% my_matrix3 
## This should print a matrix ((1,0),(0,1))

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
        setMatrixInversion <- function(solve) {
                matrixInversion <<- solve
        }
        
        # Function to get the cached value
        getMatrixInversion <- function() {
                matrixInversion
        }
        
        # List of functions that will be retured
        list(set = set, get = get,
             setMatrixInversion = setMatrixInversion,
             getMatrixInversion = getMatrixInversion)
        
}


## This function checks if the inverted matrix exists in cache
## If it does it returns the value
## If not it calculates the value using the solve function and stores it in cache
## During the first exection there will be a "data not yet in cache" message
## Subsequent executions will see the message "getting cached data"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if the value exists in cache. If it exists return the value
        matrixInversion <- x$getMatrixInversion()
        if(!is.null(matrixInversion)) {
                message("getting cached data")
                return(matrixInversion)
        } else {
                message("data not yet in cache")  
        }
        
        ## Get the data component
        data <- x$get()
        
        ## Call the solve function to invert the matrix and cache it
        matrixInversion <- solve(data, ...)
        x$setMatrixInversion(matrixInversion)
        
        ## Return the value
        matrixInversion
}



