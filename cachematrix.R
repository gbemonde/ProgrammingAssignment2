## The first function, makeCacheMatrix creates a  inverse of a "matrix", such that matrix* inverse (matrix)=1

makeCacheMatrix <- function(x = matrix()) {
mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  if (!det(x)==0) {
  setinv <- function(solve) mat <<- solve}
  
  getinv <- function() mat
  ylist(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates the inverse of a "matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
