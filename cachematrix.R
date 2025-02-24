## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # use temp as a temporary place to store the matrix
  temp <- NULL
  
  # this function used to store new matrix x
  setting <- function(y) {
    x <<- y       
    temp <<- NULL
  }
  
  # return the function x 
  receive <- function() {
    x
  }
  
  # store the inverse matrix into temp
  set_inverse <- function(inverse_value) {
    temp <<- inverse_value 
  }
  
  # return the inverse matrix, if nothing then return null
   get_inverse <- function() {
    temp
  }
  
  # return a list including four arguments
  list(setting = setting, receive = receive,
       set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # receive inverse matrix
  temporary <- x$get_inverse()
  
  # if exist then return
  if (!is.null(temporary)) {
    return(temporary)
  }
  
  # or receive from original matrix
  data <- x$receive()
  
  # get the inverse matrix
  temporary <- solve(data, ...)
  
  x$set_inverse(temporary)
  
  temporary
}
