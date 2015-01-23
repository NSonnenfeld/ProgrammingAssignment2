##Function: makeCacheMatrix ;
##Purpose: A square matrix (set) and its inverse (setinverse) are stored ;
##         as functions in the global environment.     ;
##         They are called in the functions get and getinverse;
##         Finally, the function makeInverse calls all 4 functions;

makeCacheMatrix <- function(x =matrix()) {
  m <- NULL
  set <- function(i) {
    x <<- i
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set= set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

##Function: cacheSolve ;
##Purpose: Retrieves the matrix and its inverse  ;
##         and returns the inverse     ;


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}