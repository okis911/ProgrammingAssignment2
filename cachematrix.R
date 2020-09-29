## this function fetches the matrix and set the inverse function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {m <<- inverse}
  getInv <- function() {m}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## the following fucntion cache the data if there already are null values and hence gives inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

A <- matrix(c(1,2,3,4),2,2)
#solve(B) #We pretend that this cant't happen xD

A1 <- makeCacheMatrix(A)
cacheSolve(A1) #inverse returned after computation

cacheSolve(A1) #inverse returned from cache
