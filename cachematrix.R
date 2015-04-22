##These functions cache the inverse of a matrix. Matrix from cache can be retrieved rather than computing them repeatedly.

##This function creates a special "matrix" object that can cache its inverse.
##The first function, makeCacheVector creates a list containing a function to
##  1 set the value of the matrix
##  2 get the value of the matrix
##  3 set the value of the inverse
##  4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse      
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,   ## creating a list of functions provided by makeCacheMatrix function
       setinverse = setinverse,
       getinverse = getinverse)  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    
  inv <- x$getinverse()
  if(!is.null(inv)) {    ##checking if inverse of matrix exist in cache or not
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()  ## retrieving the matrix whose inverse is to be calculated
  inv <- solve(data)     ## calculating inverse of a matrix
  x$setinverse(inv)      ## setting the inverse of matrix to cache
  inv                    ## Return a matrix that is the inverse of 'x' 
}
