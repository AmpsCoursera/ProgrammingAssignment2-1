## Create the matrix capable of inverse cache
makeCacheMatrix <- function(x = matrix()) {
  ## Starting the inverse
  humps <- NULL
  ## Setting the inverse
  set <- function(y) {
    x <<- y
    humps <<- NULL
  }
  ## Getting the matrix
  get <- function() x
  ## Setting the inverse
  setInverse <- function(inverse) humps <<- inverse
  ## Getting the inverse
  getInverse <- function() humps
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Returning the matrix of inverse x
  humps <- x$getInverse()
  ## Returning the matrix
  if(!is.null(humps)) {
    message("getting cached data")
    return(humps)
  }
  ## Retrieving the matrix
  data <- x$get()
  humps <- Inverse(data, ...)
  ## Setting the inverse to object
  x$setInverse(humps)
  ## Returning the matrix
  humps
}