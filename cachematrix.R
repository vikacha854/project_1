

#Create makeCacheMatrix function 
makeCacheMatrix <- function( mtrx = matrix() ) {
  
  #Set the matrix
  i <- NULL
  set <- function( matrix ) {
    mtrx <<- matrix
    inv <<- NULL
  }
  
  #Get the matrix
  get <- function() {
    mtrx
  }
  
  #Set the inverse of the matrix 
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  #Get the inverse of  the matrix
  getInverse <- function() {
    inv
  }
  
  #List of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Create cacheSolve function
cacheSolve <- function(x, ...) {
  
  #Matrix that is the inverse of 'x'
  mtrx <- x$getInverse()
  
  if( !is.null(mtrx) ) {
    message("getting cached data")
    return(mtrx)
  }
  
  #Get the matrix from our object
  data <- x$get()
  
  #Calculate the inverse using matrix multiplication
  mtrx <- solve(data) %*% data
  
  #Set the inverse to the object
  x$setInverse(mtrx)
  
  #Matrix
  mtrx
}