## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix ceates a list of 4 functions, 
## set the matrix, gett the value of the matrix
## set the inverse of the matrix and get the inverse of the matix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix. It checks first to
## see whether it's in the cache (calculated previously). If so
## it returns from the cache. Otherwise it returns the
## calculated result (and caches it for possible retrieval)
## Note - assumesmatrix is always reversible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## exmaple of how to run/test this code
## z = rbind(c(1, -1/8), c(-1/8, 1))
## s = makeCacheMatrix(z)
## s$get()
## cacheSolve(s)
