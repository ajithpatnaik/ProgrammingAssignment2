## This function creates a matrix which is basically capable to stoe the cached form of what we wish to store
##It uses a special operator to store objects in an other environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setmatrix <- function (inverse) m <<- solve
  getmatrix <- function() m
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## This function takes the cached matrix that is stored using the above function and inverses it.
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message ("getting cached data")
    return (m)
  }
  matrix<- x$get()
  m <- solve (matrix, ...)
  x$setmatrix (m)
  m
}
