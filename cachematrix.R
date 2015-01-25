## This code follows the skeleton of the example, only that it uses the function
## solve to calculate the inverse of the matrix, also added a message indicating when
## was the matrix initialized

## This functions creates a closure that contains the data about the matrix,
## in its first call, it doesn't store the inverse, note that when a new
## matrix is set, the cached inverse is deleted

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    print("Initalizing matrix...")
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the previous closure as an argument and
## checks if the inverse was already calculated, if so, it returns;
## if the inverse isn't cached it solves the matrix and stores the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    print("Getting cached inverse...")
    return(i)
  }
  print("Solving matrix and caching...")
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
