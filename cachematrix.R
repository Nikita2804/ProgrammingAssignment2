## This code consists of 2 functions that take a square invertible matrix as input and return the inverse of the matrix. 
## It makes use of two functions - makeCacheMatrix and cacheSolve


## This function takes a square invertible matrix as input and returns a list consistsing of 
## 4 functions to - set the matrix,get the matrix,set the inverse, and get the inverse. These 4 functions from the list output are used by the second function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  ## '<<-' used to assign value to object in an environment different from the current environment
    
        x <<- y
         m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Input to this function is the output of makeCacheMatrix() function. The output is the inverse of the original matrix input to the makeCacheMatrix funciton

cacheSolve <- function(x=matrix(), ...) {
  
  m <- x$getinverse()
  ## If inverse has already been calculated, use the inverse value from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else, calculate the inverse
  
  mat.data <- x$get()
  m <- solve(mat.data)
  ## Set value of inverse in cache
  
  x$setinverse(m)
  
  return(m)
}

