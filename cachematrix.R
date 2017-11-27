##This function will make a matrix to be cached
makeCacheMatrix<-function(x = matrix()){
    m <- NULL                                         ##This initializes m as NULL
      set <- function(y){                             ## define the set function
        x <<- y
        m <<- NULL
      }
    get <- function () x                              ## define the get function
    setinverse <- function(inverse)  inv <<- inverse  ##assigns inv for parent
    getinverse <- function () m                       ## get the value of inv
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) ## needed for $ functions
    
}
  
## this computes the inverse of the special matrix if not in cache already
cacheSolve <- function(x, ...){
                                                      ## Give a matrix that is the inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setinverse(inv)
  }
