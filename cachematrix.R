## This code corresponds to the second assigment of the R programming course
## on Coursera and consists of the elaboration of two two functions that
## store the inverse of a matrix in cache 
 
## Author: Arturo Vera 

## This function stores in the cache a matrix that it receives as input 
 
makeCacheMatrix <- function(x = matrix()){  #Start of te function 
  m <- NULL                            
  set <- function(y){                       #Start the auxiliar function
    x <<- y                                 #operator used to define y to x in cache
    m <<- NULL                              #operator used to define NULL to m in cache
  } 
  get <- function(){x}                      #get the x variable
  setminv <- function(inv){m <<- inv}       #set the inv variable to m in cache 
  getminv <- function(){m}                  #get the m variable
  list(set = set, get = get, setminv = setminv, getminv = getminv) 
} 


## This function verifies if the inverse of the array that was received as input has
## already been calculated and is located in the cache, in which case in which case it 
## is called, otherwise the function calculates the inverse matrix and stores it in cache 

cacheSolve <- function(x,...){                #Start the function
  y <- x$getminv()                            #Assing the value of getminv (makeCacheMatrix function) to y variable
  if (!is.null(y)){                           #If y in cache, just call it
    message("getting cached inverse matrix") 
    return(y) 
  } 
  ma <- x$get()                                
  y <- solve(ma, ...)                         #otherwise compute the inverse matrix
  x$setminv(y)                                #and set in cache
  y 
} 
 
## End of the program 