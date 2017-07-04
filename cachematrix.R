## The two function takes an invertible matrix and stores the resulting inverse of the matrix
## in cache. If the matrix does not change, then the functions pull from previously
## calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
    }
  get <- function() x
  setInv <- function(solve) Inv <<- solve
  getInv <- function() Inv
  list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
