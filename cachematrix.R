## This file contains a pair of functions that can be used for creation and manipulation of a matrix object and its inverse.


## The makeCacheMatrix function creates a matrix that can cache its inverse. 
## It returns a list 4 functions for its manipulation. Two related to the matrix and two related to the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    ##Set the matrix and reset its inverse to NULL 
    ## since it has not been calculated for the new matrix
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    
    ##Return the matrix object
    get <- function() x
    
    ##Set and cache the inverse of the matrix
    setinverse <- function(i) inverse <<- i
    ##Get the cached inverse of the matrix
    getinverse <- function() inverse
    
    #Return a list of the functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of a cached matrix as created by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ##If there exists cached inverse of this matrix then return its cached inverse
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        print("Returning inverse from cache")
        return(inverse)
    }
    
    ##Otherwise calculate, cache and then return the inverse
    data <- x$get()
    print("Calculating inverse")
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}
