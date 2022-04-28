## Return a matrix that is the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  if (!det(x)==0) {
  setsolve <- function(solve) mat <<- solve}
  
  getsolve <- function() mat
  ylist(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
  mat <- x$getsolve()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setsolve(mat)
  mat
}
}
