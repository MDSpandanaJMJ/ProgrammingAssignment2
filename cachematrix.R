## makeCacheMatrix and cacheSolve functions return a matrix


## makeCacheMatrix creates a matrix and calculates the inverse of the matrix using solve function.
## It stores the original matrix and inverse of the matrix using caching

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(solve) m <<- solve
    
    get_inverse <- function() m
    
    list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
    
}


## This function checks if inverse of the given matrix is already present and if not present,
## it calcualtes the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  else {
    data <- x$get()
    m <- solve(data,...)
    x$set_inverse(m)
    m
  }
  
  
}
