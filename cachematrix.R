

## MakeCacheMatrix creates a special "matrix", which is a
##list of 4 functions (set, get, setinversematrix,getinversematrix)
##

  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                           
    set <- function(y) {  ##Set changes the matrix stored in the main function                
      x <<- y             ## <<- substitutes the matrix in the main function                
      inv <<- NULL        ## Restores to NULL the value on inv                
      }
        get <- function() x ##Get returns the matrix x stored in the main function                
       
      ##Setinversematrix and getinversematrix are similar to set and get
      ##they store and return the value of the variable in the main function
  setinversematrix <- function(inverse) inv <<- inverse  
  getinversematrix <- function() inv    
  ## The funcion list() is needed in order to store the 4 functions in makecacheMatrix so
  ##when we assign makecacheMatrix to an objetc, the object has the 4 functions.
  list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)   
  
  }


## cacheSolve calculates the inverse of the matrix

  cacheSolve <- function(x, ...) {
    
    inv <- x$getinversematrix()
    if(!is.null(inv)) {    ##It first checks if the inverse has been calcualed
        message("getting cached data") ##if so it gets the inverse
        return(inv)
      }
    data <- x$get()
    inv <- solve(data, ...)  ## otherwise,it makes the computations 
    x$setinversematrix(inv)
    inv
  }
