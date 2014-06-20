## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function does following:
## - set - sets the value of the matrix,
## - get - gets the value of the matrix,
## - setinverse - sets the value of the inverse,
## - getinverse - gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
        myInv <- NULL
        
        set <- function(y) {
          x <<- y
          myInv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(val) myInv <<- val
        
        getinverse <- function() myInv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
## If inverse of matrix was already calculated and cashed, then return cashed results
## Else calculate it and return the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInv <- x$getinverse()
        if(!is.null(myInv)) {
          message("getting cached data")
          return(myInv)
        }
        
        message("No cached data")
        
        data <- x$get()
        myInv <- solve(data, ...)
        x$setinverse(myInv)
        return(myInv)  
        
}
