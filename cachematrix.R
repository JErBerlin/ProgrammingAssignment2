## Show the use of scope programming and super-assignment operator
## by implementing a build function and a get or compute function


## Build the object and its associated methods (get, set)

makeCacheMatrix <- function(X = matrix()) {

     ## The variable 'X' to store the matrix is not explicit,
     ## and it is handeled by the 'lazy' principle
     ## The variable 'Mi' stores the inverse, which is computed
     ## by another function
     
     Mi <- NULL
     set <- function(y) {
          X <<- y
          Mi <<- NULL
     }
     get <- function() X
     setinv <- function(Z) Mi <<- Z
     getinv <- function() Mi
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
     
}

## Computes the inverse matrix or recalls it from previous calls

cacheSolve <- function(X, ...) {   
     
     ## Return a matrix 'Mi' that is the inverse of 'X'
     ## if 'Mi' is not yet computed, the function does it now
     
     Mi <- X$getinv()
     if(!is.null(Mi)) {
          message("getting cached data")
          return(Mi)
     }
     data <- X$get()
     Mi <- solve(data, ...)
     X$setinv(Mi)
     Mi
}


### Programming assignment num.2 of coursera course 'R programming'. JErBerlin. August 2014.