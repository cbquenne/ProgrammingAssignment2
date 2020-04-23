#The two functions below create, solve, and cache a "matrix" object
#The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function() {
    x
    }
  setInverse <- function(Inverse) {
    m <<- Inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
  
#The second function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated and the matrix has not changed then the 
#cacheSolve should retrieve the inverse from the cache
  
cacheSolve <- function(x, ...) {
  ##Returns a matrix that is the inverse of 'x' 
  m <- x$getInverse()
  
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
  data <- x$get()
  #print(data)
  
  m <- solve(data, ...)
  x$setInverse(m)
  
  m
}
