## a pair of functions to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL ## Initially inverse  == 'NULL'   			
  setmatrix <- function(y) {			
    x <<- y 	## set matrix x			
    inverse <<- NULL
  }
  getmatrix <- function() x       ##return the matrix x
  setinverse <- function(solve) inverse <<- solve   ##cache inverse	
  getinverse <- function() inverse 		   ##return inverse value
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {  ## Return the inverse of 'x' as a matrix
  
  inverse <- x$getinverse()			
  if(!is.null(inverse)) {				## Check if the inverse has already been calculated
    message("getting cached data")			
    return(inverse)            ## get the inverse
  }
  
  data <- x$getmatrix()			## Get the Matrix		
  inverse <- solve(data, ...)			## compute inverse
  x$setinverse(inverse)				##cache the inverse
  inverse 	                ## return inverse
  
}

