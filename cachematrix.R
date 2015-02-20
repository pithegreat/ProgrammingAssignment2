## The overall purpose is to store the inverse of a function, and if it
## is not calulated then to do it to cave time

## This function retrieves and create variables that creates a cached matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #sets value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #retrieves the value of the matrix
  setinverse <- function(inverse) m <<- inverse  #sets the inverse of the matrix
  getinverse <- function() m #retreives that inverse of the matrix
  list(set = set, get = get,  #returns the values of the previous functions in a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function sees if a inverse has already been solved, and if not solves one

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #gets inverse value
  if(!is.null(m)) { # if inverse value is already there then returns it
    message("getting cached data")
    return(m)
  }
  data <- x$get() #otherwise it gets value of x
  m <- solve(data, ...) #then it finds the inverse of x
  x$setinverse(m) #then it sets that inverse to m
  m #finally it returns the inverse of x
}

