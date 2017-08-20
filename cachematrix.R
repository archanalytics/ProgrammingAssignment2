## A pair of functions that work together to implement a caching mechanism for 
## efficiently working out the inverse of an invertible matrix
## The cache is especially useful when the calculation needs to be
## peformed multiple times, e.g. in a loop

makeCacheMatrix <- function(x = matrix()) {
    ## creates a special "vector", which is a list containing functions to 
    ## set and get the values of the vector
    ## set and get the inverse of an invertible matrix
    
    i = NULL
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    ## Returns the inverse of a matrix (x), either from the cache if it exists, 
    ## or calculated by solve, in which case save back to cache for next time
    
    inverse = x$getinverse()

    if (!is.null(inverse)){
        message("getting cached data") ## inverse was calculated before and exists in the cache
        return(inverse)
    }
    
    # else calculate
    m = x$get()
    inverse = solve(m, ...)
    
    x$setinverse(inverse)  # set the value in the cache for next time
    
    return(inverse)
}

## To run and test the functions, define a square (invertible) matrix:
## m = matrix(c(2, 4, 3, 1, 5, 7,4,7,10), nrow=3, ncol=3) 

## Then set up the cache:
## m <- makeCacheMatrix(M)

## Then calculate the inverse for the first time. This will return a value, but with no message "getting cached data"
## cacheSolve(m)

## Now re-run on the same matrix. This will return a value with the message "getting cached data"
## cacheSolve(m)


