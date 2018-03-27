## a pair of functions that cache the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## It is assumed that the matrix supplied is always invertible.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
      inv_m <- NULL
      ## define a function that caches the inverse of the matrix
      set <- function(y) {
            x <<- y
            inv_m <<- NULL
      }
      get <- function() x
      set_inv  <- function(m)  inv_m <<- m 
      get_inv <- function () inv_m
      list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$get_inv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set_inv(m)
      m
      
}


