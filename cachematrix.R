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
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  return(list(set = set, 
              get = get, 
              setinverse = setinverse,
              getinverse = getinverse))
}


## Write a short comment describing this function

#The following function calculates the inverse of the matrix created 
#with the above function. It first checks to see if the inverse has 
#already been calculated. If so, it 'getinverse' from the cache and skips
#the computation. Otherwise, it calculates the mean of the data and 
#sets the inverse of the matrix in the cache via the 'setinverse' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  
  data <-x$get()
  
  i <- solve(data, ...)
  x$setinverse(i)
  
  return (i)
}
