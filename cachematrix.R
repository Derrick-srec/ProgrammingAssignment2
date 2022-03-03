## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                             ## m holds the value of matrix inverse 
  set <- function(y) {                    ## to assign new
    x <<- y                             ## value of matrix in parent environment
    m <<- NULL                        ## When a new matrix is passed the m value is set to null
  }
  get <- function() x                     ## it returns the value of the matrix
  
  setinverse <- function(inverse) m <<- inverse  ## m value is obtained from parent environment
  getinverse <- function() m                     ## m value is returned
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()                #previous inverse matrix value is received if there is already generated
  if(!is.null(m)) {                  #checks if the value of m is null or not
    message("getting cached data")   
    return(m)                        #if not null m is returned directly
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
