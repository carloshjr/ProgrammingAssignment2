## First the function creates a matrix to cache its inverse and then the second 
## function calculates the inverse of the matrix provided by the first function.
## In the case the inverse has already been calculated (or even stored) in the fisrt function, 
## the second one just retrieves it with a message

## With this function one can store the original matrix as well as its inverse and 
## retrieve both at any time

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y){
              x <<- y
              m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() {m}
            
            list(set=set, get=get,
                 setinverse = setinverse,
                 getinverse = getinverse)
          }


## This function first retrieves the inverse value if stored previously. If there's 
## nothing stored then it calculates the inverse of the input matrix provided 

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
