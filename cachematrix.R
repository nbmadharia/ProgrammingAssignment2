makeCacheMatrix <- function( m = matrix() ) {
  ## Initialize inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  ## inverse of 'x'
  m <- x$getInverse()
  
  ## return if already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix
  data <- x$get()
  
  ## Calc inverse
  m <- solve(data) %*% data
  
  ## Set inverse to object
  x$setInverse(m)
  m
}
