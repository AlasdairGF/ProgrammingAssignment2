## Assignment 2 - Caching matrix inversion
## R Programming - rprog-004
##
## Consists of 2 functions - one for getting & setting the cached matrix,
## the other for performing the actual matrix inversion.
##
## Probably too many comments...!
##

## makeCacheMatrix
## This returns 4 functions as a list which will set up a matrix object,
## report its value, store the inverse (calculated in second fn) and also
## report the inverted value

makeCacheMatrix <- function(x = matrix()) {
      
      cachedMatrix <- NULL
      
      set <- function (incomingMatrix) {
            # assign incoming matrix to x which exists in scope of makeCacheMatrix
            # not in the set function (hence the <<- bit)
            x <<- incomingMatrix 
            cachedMatrix <<- NULL # clear matrix as re-calc will be needed
      }
      get <- function() x # just return the matrix
      setMatrix <- function(invertedMatrix) cachedMatrix <<- invertedMatrix
      getMatrix <- function() cachedMatrix
      
      # makeCacheMatrix fn returns the list of 4 functions:
      list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)

}


## cacheSolve
## Checks to see if the solution already exists - if so, return cached,
## if not, then solve & cache solution.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inversion <- x$getMatrix()
      if (!is.null(inversion)) {
            ## so if inversion has already been calculated, don't re-do it
            ## just return the value already cached in the variable
            message("Returning cached inversion...")
            return(inversion)
      }
      workingMatrix <- x$get() # local variable to play with
      
      message ("Calculating inversion...")

      ## magic box where we calculate inverted matrix
      ## stored in local variable 'inversion'
      
      x$setMatrix(inversion) # cache the solution for next time
      inversion # return the solution
}
