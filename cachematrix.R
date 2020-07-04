
## This function creates a special matrix, which is really a function to
##      1. Set the value of the matrix
##      2. Get the value of the matrix
##      3. Set the value of the matrix inverse
##      4. Get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the special matrix. 
## It first check to see if the matrix inverse has already been calculated
## If so, it gets the matrix inverse from the cache and skips the computation
## Otherwise, it calculates the matrix inverse and sets the value
## of the matrix inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
              message("getting cache data")
              return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
