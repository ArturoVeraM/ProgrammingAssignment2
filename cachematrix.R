## This code corresponds to the second assigment of the R programming course 
## on Coursera and consists of the elaboration of two two functions that 
## store the inverse of a matrix in cache

## Author: Arturo Vera

## This function stores in the cache a matrix that it receives as input

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


## This function verifies if the inverse of the array that was received as input has 
## already been calculated and is located in the cache, in which case in which case it 
## is called, otherwise the function calculates the inverse matrix and stores it in cache

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
