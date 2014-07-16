## ***************************************************************************************************
## Use these functions to calculate the inverse of a matrix and store it to the cache
## To use these functions: 
##   Step 1: set a variable to an instance of makeCacheMatrix 
##		 For example: a <- makeCacheMatrix(data.frame(1:2, 5:6)) 
##   Step 2: Call cacheSolve passing in the name of that variable to return the inverse of the matrix
##		 For example: cacheSolve(a)
## What it Does:
## 	- The first time you call the function, it will need to calculate the inverse of the matrix
## 	- The next time it will retrieve the value from cache, without calculating it again.
## Assumption: the matrix supplied is always invertible.
## ***************************************************************************************************


## ***************************************************************************************************
## This function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## This special "matrix", is really a list containing functions to:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse
##	4. get the value of the inverse
## ***************************************************************************************************
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setinverse <- function(solve) { m <<- solve }
        getinverse <- function() { m }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## ***************************************************************************************************
## This function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## ***************************************************************************************************
cacheSolve <- function(x, ...) {

	  ## get the current value of m 
        m <- x$getinverse()
	 
	  ## if m has already been set and cached, return a message, and return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  ## if m is not cached, then calculate and cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
