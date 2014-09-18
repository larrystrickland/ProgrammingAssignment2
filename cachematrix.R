## This .R file contains two functions:
#     makeCacheMatrix and cacheSolve
# These two functions are used in conjunction with to cache the inverse (solve) of a matrix.  
# The makeCacheMatrix is used to create the matrix, and set up the storage for the solve
# To use these functions:
#     1. Create a matrix using the makeCacheMatrix function, by passing in a Matrix
#     2. To get/set the created matrix, call teh $get/$set within makeCacheMatrix
#     2. Call the cacheSolve function passing in the result of makeCacheMatrix to get the solve 
#        for the matrix
#            - If the cachSolve function has been previously called and the matrix has not changed, 
#              the result will be the solve of the matrix with a message "getting cached data"
#            - If the cacheSolve function has not been called since the matrix was created or changed,
#              the result will be the solve of the matrix.


#########################################
# makeCacheMatrix(x=matrix())
# This function creates a matrix and the opportunity to store the solve of the matrix
# The functions $set and $get can be used to set and get the result
# The functions $getsolve and $setsolve are used by the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL # s - or solve, will hold the solve of the matrix
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}

#########################################
# cacheSoleve(x=makeCacheMatrix(y=matrix()))
# This function returns the solve for the object created by the makeCacheMatrix function
# It will return the solve stored in teh makeCacheMatrix object if it has already been calculated
# If it has not been previously calculated it will calcuate the solve, return the result and store
# the result in the makeCacheMatrix object.  


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
