## makeCacheMatrix will return an matrix object capable of caching it's inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  solve_result <- NULL
  set <- function(matrix_param) {
    matrix <<- matrix_param
    solve_result <<- NULL  ## once the matrix has changed set it to NULL
  }
  get <- function() matrix
  setinverse <- function(inverse) solve_result <<- inverse
  getinverse <- function() solve_result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will calculate it and store in case it wasn't calculated yet
cacheSolve <- function(matrix, ...) {
  ## retrieving inverse object
  solve_result <- matrix$getinverse()
  if(!is.null(solve_result)) {
    ## if it exists, warn that it is a cached value and return it.
    message("getting cached data")
    return(solve_result)
  } else {
    ## otherwise, apply solve() on matrix, set cache and return result.
    message("calculating and caching inverse...")
    data <- matrix$get()
    solve_result <- solve(data, ...)
    matrix$setinverse(solve_result)
    solve_result
  }
}
