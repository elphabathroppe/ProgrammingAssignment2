## Matrix file - containining matrix creation and caching functions

## create a matrix object with get set functions 

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   
   set <- function(y){
   	x <<- y
  	m <<- NULL
   }
   
   get <- function() x
   setInverse <- function(solve) m <<- solve
   getInverse <- function() m
   
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## return cached matrix inverse (or create if cache doesnt exist)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
