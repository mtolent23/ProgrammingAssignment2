## Matrix inversion is usually a costly computation and their may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. This script file writes a pair
## of functions that cache the inverse of a matrix: makeCacheMatrix & cacheSolve

# This function creates a special "matrix" that returns a list containing a function to
# (1) Set the value of the matrix, (2) Get the value of the matrix,
# (3) Set the value of the inverse, and (4) Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # initialise cached inverse matrix variable
        inv_x <- NULL
        
        # function to set the value of the matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        # function to get the value of the matrix
        get <- function() x
        
        # function to set the value of the inverse
        setinverse<- function(inverse) inv_x <<-inverse
        
        # function to get the value of the inverse
        getinverse <- function() inv_x
        
        # Return the matrix with our 4 functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" object returned by the makeCacheMatrix function. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        # get the inverse from cache
        inv_x <- x$getinverse()
        
        # check if inverse has been calculated
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                # return value
                return(inv_x)
        } else {
                # calculate inverse
                inv_x <- solve(x$get())
                # cache inverse
                x$setinverse(inv_x)
                #return value
                return(inv_x)
        }
        
}
