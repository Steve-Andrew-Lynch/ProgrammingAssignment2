## The purpose of this function is to return the inverse of a matrix AND cache one
## instance of that value 

## makeCacheMatrix instantiates a matrix-like that provides a list of ("method-like")
## functions that can be accessed by a second wrapper function "cacheSolve" which then
## accesses the caching matrix to detemine the inverse of matrices.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setcache <- function(solve) m <<- solve
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}



## This function operates on makeCacheMatrix to 1) retrieve the cache when present2) 

cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m      
}
