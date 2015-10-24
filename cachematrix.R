makeCacheMatrix <- function(x = matrix()) {
  
  # set the matrix
  cache_ <- NULL
  set_ <- function(y){
    x <<- y
    cache_ <<- NULL
  }
  
  # get the matrix
  get_m <- function() x
  
  # set the inverted matrix
  set_m <- function(inverse) cache_ <<- inverse
  
  # get the inverted matrix
  get_im <-function() cache_
  
  list(set_ = set_, get_m = get_m, set_m = set_m, get_im = get_im)
}


cacheSolve <- function(x, ...) {
  
  # get the inverted matrix
  inv_ <- x$get_im()
  if(!is.null(inv_)){
    message("getting cached data")
    return(inv_)
  }
  
  # calculates the inverted matrix if it hasn't already 
  # been calculated
  m_ <- x$get_m()
  inv_ <- solve(m_, ...)
  x$set_m(inv_) # set the inverded matrix in the cache
  return(inv_)
}