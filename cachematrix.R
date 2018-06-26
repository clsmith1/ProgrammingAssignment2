## Put comments here that give an overall description of what your
## functions do
## These functions expedite the process of finding the inverse of a matrix
## by caching previously calculated inversions for future retrieval.

## Write a short comment describing this function
#### The first function (makeCacheMatrix) creates a matrix and saves the inverse of the given matrix for future retrievel. 

makeCacheMatrix <- function(x = matrix()){
      i <<- NULL
      set<- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## Write a short comment describing this function
## The second function searches the matrix made with the first function to see 
## if the inverse of the matrix has already been calculated; if true, the function returns the known inverse of the matrix.
cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)){
            print("retrieving cached value")
            return(i)
      }
      data <- x$get()
      i<- solve(data, ...)
      x$setInverse (i)
      print(i)
}
