## R Script containing functions to cache matrix inverse function

## This function creates a special matrix object capable o caching its inverse
## @param x: A matrix containing the initial data
makeCacheMatrix <- function(x = matrix()) {
  #Initialize a variable that will contain the inverse using NULL value
  i <- NULL
  #Setter function to asign matrix value, when matrix value changes inverse should be null
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #Getter function to return matrix value
  get <- function(){ x }
  #Inverse setter function to assign inverse value to local cache variable
  setinverse <- function(inverse){ i <<- inverse }
  #Inverse getter function to return inverse value from local cache variable
  getinverse <- function(){ i }
  #Return special matrix methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a matrix created with makeCacheMatrix function
## @param x: A matrix created with makeCacheMatrix function
## @param ...: Aditional params to compute the inverse
cacheSolve <- function(x, ...) {
  #Get inverse from special matrix 
  i <- x$getinverse()
  #If theres a cached value return it
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #Get special matrix data
  data <- x$get()
  #Compute special matrix inverse
  i <- solve(data, ...)
  #Cache inverse on special matrix
  x$setinverse(i)
  #Return inverse
  i
}
