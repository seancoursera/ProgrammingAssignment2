makeCacheMatrix <- function(x = matrix()) {
  cc <- NULL  # set cache to NULL
  set <- function(y) {
    x <<- y
    cc <<- NULL
  }
  
  # set cache
  get <- function() x
  setcache <- function(cache) cc <<- cache
  getcache <- function() cc
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}

cacheSolve <- function(x, ...) {
  cc <- x$getcache
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(cc)) {
    message("getting cached data")
    return(cc)
  }
  data <- x$get()
  cc <- solve(data, ...)
  x$setcache(cc)
  cc
}
