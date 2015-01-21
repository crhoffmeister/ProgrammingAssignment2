## The following two functions create a matrix object that can 
## cache its inverse using the solve function.

## Creates a special matrix object, which is really 

makeCacheMatrix <- function(x = matrix()) {
    
    # Set value of matrix    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the inverse of the matrix
    setinverse <- function(solve) i <<- solve
    
    # Get the inverse of the inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function creates the inverse of the matrix created by the obove function

cacheSolve <- function(x, ...) {
    
    # Check to see if inverse has been created
    i <- x$getmean()
    
    # If so, it gets the inverse from the cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # If not, it calculates the inverse of the data ...
    data <- x$get()
    i <- solve(data, ...)
    
    # ... and sets the inverse in the cache via the setinverse function
    x$setinverse(i)
    i    ## Returns a matrix that is the inverse of 'x'
}
