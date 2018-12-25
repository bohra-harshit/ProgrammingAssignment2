## set function -- Sets the matrix value
## get function -- gets the maxtrix data
## setInverse , getInverse -- sets and gets the inverse of the matrix

## makeCacheMatrix creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  MatrixInverse <- NULL
  set <- function(y) {
    x <<- y
    MatrixInverse <<- NULL
  }
  
  get <- function() x                             
  setInverse <- function(invertedMatrix) MatrixInverse <<- invertedMatrix 
  getInverse <- function() MatrixInverse                    
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)

}


## computes inverse of matrix if not already cached. if cached then it brings value from cache

cacheSolve <- function(x, ...) {
  MatrixInverse <- x$getInverse()
  if(!is.null(MatrixInverse)) {                       
    message("Getting Inverted Matrix from Cache")    
    return(MatrixInverse)
  }
  
  UserMatrix <- x$get()                      
  MatrixInverse <- solve(UserMatrix, ...)             
  x$setInverse(MatrixInverse)                          
  return(MatrixInverse)
}
