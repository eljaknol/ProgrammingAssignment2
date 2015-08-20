## This function creates a matrix. This function is containing a list to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
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


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ## if inverse is already calculated, otherwise see below:
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}

