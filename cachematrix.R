## Homework solution to Programming Assignment 2: Lexical Scoping from 
## Coursera R Programming by Roger D. Peng, Jeff Leek, and Brian Caffo

## makeCacheMatrix: function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
    ## Define function to set the value of the matrix. It also clears the cache
    set <- function(y) {
      x <<- y    
      m <<- NULL 
    }
    ## Define function to get the value of the matrix and set the inverse 
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
  
    ## Define function to get the inverse
    getInverse <- function() m
  
    ## Return a list with the above four functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve: function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## if cache is not empty then retrieve from cache
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## cache is empty so calculate inverse, cache, and return
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
