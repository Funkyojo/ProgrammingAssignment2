## The two functions here calculates the inverse of a given matrix.
## Furthermore, the inverse matrix will be cached, so whenever the same result
## should be used, the time spent on calculation will be saved.

## makeCacheMatrix creates four functions
## together they cashes the matrix given and defines variables for
## the to-be-cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ans <- NULL
  set <- function(y=matrix()) {
    x <<- y
    ans <<- NULL
    ## whenever a new matrix is "set", previously cached inverse matrix would be cleared.
  }
  get <- function() x
  setsolve <- function(w) ans <<-w
  getsolve <- function() ans
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates returns cached inverse matrix
## if the inverse has not been generated, this function calculates it, 
## caches it, and returns it.

cacheSolve <- function(x, ...) {
  ans <- x$getsolve()
  if(!is.null(ans)) {
  ##check if there is cached data
    message("getting cached data")
    return(ans)
  }
  ##if not, calculate the inverse matrix...
  data <- x$get()
  ans <- solve(data, ...)
  x$setsolve(ans)
  ## and cache it
  ans
  ## Return a matrix that is the inverse of 'x'
}
        
