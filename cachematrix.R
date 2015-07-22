## this pair of functions will work together to calculate and store the inverse of a matrix passed as an argument to the first of them

## makeCacheMatrix takes a matrix as an argument and creates a list containing 4 functions: set, get, setinv and getinv (see comments below)

makeCacheMatrix <- function(x = matrix()) { #takes a matrix as an argument
  
  inv <- NULL ##creates an object 'inv' (which will later store the matrix' inverse) and sets its value to null
  
  set <- function(y) { 
    x <<- y            #changes the stored matrix into another one
    inv <<- NULL  # resets the 'inv' to null (important, because if the inverse of an 'old' matrix has already been stored under 'inv'...
  }  #... it no longer applies after the matrix has been changed.
  
  
  get <- function() x #(gets a matrix passed as an argument to makeCacheMatrix)
  setinv <- function(inverse) inv <<- inverse #sets the value of 'inv' equal to the value of an object passed to setinv as an argument
  getinv <- function() inv #(gets the value of 'inv')
  list(get = get, setinv = setinv, getinv = getinv) #list of the 3 functions is returned as the output of makeCacheMatrix

}


## cacheSolve calculates and stores the inverse of a matrix stored in the object created by makeCacheMatrix, unless it has already been done
## returns the inverse

cacheSolve <- function(x, ...) { #takes as an argument a list created by makeCacheMatrix
        
  inv <- x$getinv() 
  if(!is.null(inv)) { #checks the output of makeCacheMatrix (passed to cacheSolve as an argument) for the matrix' inverse
    message("getting cached data")
    return(inv) ## if the inverse is already cached there, returns it and tells us so :)
  }
  data <- x$get() ## if the inverse's hasn't been calculated yet, gets the matrix stored in the argument
  inv <- solve(data, ...) #... calculates its inverse
  x$setinv(inv) #...modifies its original argument (an output of makeCacheMatrix) by passing the calculated inverse as an argument to the 
  #setinv function there
  inv #...and returns the inverse
  
  }
