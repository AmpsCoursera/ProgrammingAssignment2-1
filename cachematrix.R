makeVector <- function(x = matrix()) {
  humps <- NULL
  set <- function(y) {
    x <<- y
    humps <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) humps <<- mean
  getInverse <- function() humps
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheInverse <- function(x, ...) {
  humps <- x$getmean()
  if(!is.null(humps)) {
    message("getting cached data")
    return(humps)
  }
  data <- x$get()
  humps <- Inverse(data, ...)
  x$setInverse(humps)
  humps
}