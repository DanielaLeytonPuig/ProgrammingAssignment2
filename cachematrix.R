##This function creates a special "matrix" object that can cache its inverse.
##Takes as an argument a Matrix "x"
##Returns a list of 4 objects, all of them functions



makeCacheMatrix <- function(x = matrix()) {
  
  InverseMatrix <- NULL  
  
  setMatrix <- function(newMatrix) {
      x <<- newMatrix
      InverseMatrix <<- NULL 
  }  
              
  getMatrix <- function() x
  
  setInverse <- function(inverse) InverseMatrix <<- inverse
      
  getInverse <- function() InverseMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix().If the inverse has already been calculated (and the 
##matrix has not changed), then it retrieves the inverse from the cache.

##gets as an argument X which is a special matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  InverseMatrix <- x$getInverse() 
  
  if(!is.null(InverseMatrix)) {   
    message("getting cached data")
    return(InverseMatrix)         
  }
  
  data <- x$getMatrix()          
  
  InverseMatrix <- solve(data, ...) 
  
  x$setInverse(InverseMatrix)    
  
  InverseMatrix                 
}