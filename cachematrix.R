## TThis function creates a special "matrix" object that can cache its inverse
## hence getting faster results for repitative input from cached value## The 
## This funciton creates a vector, which variable value can be used in subsequent function calls

makeCacheMatrix <- function(x=matrix()) {
  ## creating functions which will cache the values
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  setSolve <- function(solve) m<<-solve
  getSolve <- function() m  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(! is.null(m)){
    print("getting cached Matrix")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix)
  x$setSolve(m)
  m
}

# Executing the function
# a <- makeCacheMatrix(matrix(4:7,2))
# a$get()
# a$getSolve()
# a$set(matrix(5:8,2))
# a$get()
# cacheSolve(a)
# cacheSolve(a)
# a$getSolve()
# b = a$getSolve()
# a$get() %*% b