## This process creates a matrix and caches its inverse in order to reduce runtime 
## associated with the calculation

# Create the matrix with four functions in a list 

makeCacheMatrix <- function(x = matrix()) {
  # initialize m
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return the value of the matrix 
  get <- function() x
  
  # assign the inverse
  setinverse <- function(inverse) m <<- inverse
  
  # return the inverse
  getinverse <- function() m
  
  #create list of funtions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Check if inverse is cached, calculate it if it isnt and return the result 

cacheSolve <- function(x, ...) {
  
  # assign the inverse to m 
  m <- x$getinverse()
  
  # if m is already cached return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # assign the matrix to data
  data <- x$get()
  
  # calculate the inverse of the matrix and asign it to m 
  m <- solve(data)
  
  x$setinverse(m)
  
  # return m
  m
}

