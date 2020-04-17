## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setminv <- function(inv){m <<- inv}
  getminv <- function(){m}
  list(set = set, get = get, setminv = setminv, getminv = getminv)
}


## Write a short comment describing this function 

cacheSolve <- function(x,...){y <- x$getminv()
if (!is.null(y)){
  message("getting cached inverse matrix")
  return(y)
}
ma <- x$get()
y <- solve(ma, ...)
x$setminv(y)
y
}
