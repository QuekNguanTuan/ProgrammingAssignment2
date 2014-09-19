## Assignment 2 is to write an R script which  is to  write 2
## functions: makeCacheMatrix, setinverse

## makeCacheMatrix takes in a matrix, computse and caches its 
## inverse. 
## cacheSolve checks the "inverse" matrix has been cashed then 
## it computes its inverse.


## First start with makeCacheMatrix: this function takes in a 
## "special" matrix object to cache
## its inverse. It returns a list of functions: set, get, 
## setinverse,getinverse functions.  

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the "special" matrix object, inverse
        inverse <- NULL
        
        ## function set to set x
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## function get to get x
        get <- function() x
        
        ## function setinverse to set inverse
        setinverse <- function(inverseValue) 
                inverse <<- inverseValue
        
        ## function getinverse to get inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The 2nd function is to compute the inverse of the matrix
## returned by makeCacheMatrix above.
## If the inverse has already been calculated then the 
## cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## retrieve inverse from makeCacheMatrix
        inverse <- x$getinverse()
        
        ## if inverse exist, put up a message and return inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## if inverse does not exist, get the input matrix,
        ## compute the inverse,
        ## set the inverse and return the inverse
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
