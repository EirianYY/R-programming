## makeCacheMatrix creates a matrix
makeCacheMatrix <- function(x = matrix()) {
## set the matrix
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
## get the matrix
    get <- function() x
## set the inverse
    setmy_solve <- function(my_solve) m <<- solve
## get the inverse
    getmy_solve <- function() m
    list(set = set, get = get,
         setmy_solve = setmy_solve,
         getmy_solve = getmy_solve)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    m <- x$getmy_solve()
## if the inverse has already been calculated, then get it from the cache and skips the computation
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
##  calculate the inverse of the matrix and set the value in the cache via the setmy_solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setmy_solve(m)
    m
}
