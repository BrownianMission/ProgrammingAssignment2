## This code is more or less identical to the example code that was given.
## The difference is that we are dealing with matrices and their inverses rather
## than vectors and their means.
## I assume that this is what the instructor had intended.
## So, below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really
## a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheInverse calculates the inverse of the special "matrix" returned by 
## the makeCacheMatrix function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the 
## inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}