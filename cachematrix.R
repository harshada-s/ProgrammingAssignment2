## The makeCacheMatrix function creates a matrix object and returns a list of functions to be used on input matrix to get inverse as output
## The cacheSolve function actually calculates and caches the inverse of given matrix. If it is already cached it directly returns the inverse.

## makeCacheMatrix takes matrix as an argument. It creates a matrix object which returns a list of described functions.

makeCacheMatrix <- function(x = matrix()) {
  i=matrix()
  set <- function(y) {
    x <<- y
    i=matrix()
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the matrix object created by previous function as an argument and returns the inverse of the actual matrix.
## initially inverse is calculated and cached. After that the cached value can be returned directly without computing inverse each time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  for(n in i){
  if(is.na(n)) {
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
    return(i)
  }
  }
  message("getting cached data")
  return(i)
}
  