## Following code defines two functions that are used to create a special
## object that stores a square and invertible matrix and caches its inverse


## This function creates a special object to store matrix x and returns
## a list containing a function to do each of the following:
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. compute the inverse of the matrix
## 4. retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    xinv <- NULL
  
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
  
    get <- function() x
    setinverse <- function(xinverse) xinv <<- xinverse
    getinverse <- function() xinv
  
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function uses the special matrix object returned by makeCacheMatrix. 
## If inverse of x has been computed, cacheSolve retrieves the same from cache.
## Otherwise, it computes and caches the inverse.
## This function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
    
    xinv <- x$getinverse()
  
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
  
    mdata <- x$get()
    xinv <- solve(mdata)
    x$setinverse(xinv)
  
    xinv   
}
