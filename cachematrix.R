## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The first function, 'makeCacheMatrix' creates a list containing a
#function to

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#The following function calculates the inverse of the matrix created 
#with the above function. It first checks to see if the inverse has 
#already been calculated. If so, it 'getinverse' from the cache and skips
#the computation. Otherwise, it calculates the mean of the data and 
#sets the inverse of the matrix in the cache via the 'setinverse' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  
  data <-x$get()
  
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
