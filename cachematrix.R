# The makeCacheMatrix is a function that stores 
# a list of get, set, getInverse, and setInverse functions
makeCacheMatrix <- function(x = matrix()) {
   # initialize im.val variable that stores inverse of square matrix "x" to null
   im.val <- NULL
   # set value of matrix
   set <- function(y) {
      # Substitute the matrix x with y 
      x <<- y
      # reset inverse value stored in im.val variable to NULL
      im.val <<- NULL
   }
   # get value of matrix
   get <- function() x
   # set value of inverse
   setInverse <- function(inverse.val) im.val <<- inverse.val
   # get value of inverse
   getInverse <- function() im.val
   # return list of functions
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


# cacheSolve function calculates inverse of matrix created with makeCacheMatrix function. 
# However it first checks to see if the inverse has been calculated. 
# If so, it gets inverse from the cache and skip computation.
# Otherwise, it calculates inverse of matrix using solve function 
# and sets value of the inverse in the cache via "setInverse" function
cacheSolve <- function(x, ...) {
   # get inverse from cache
   im.val <- x$getInverse()
   # If inverse has been calculated, get cached data and return 
   if (!is.null(im.val)){ 
      message("getting cached data")
      # return inverse from cache
      return(im.val)
   }
   
   # get matrix to be computed from cache and put value in mat variable
   mat <- x$get()
   ## check if matrix has an inverse 
   # determinant must not be zero if it has an inverse
   det.val <- tryCatch(det(mat), error = function(e) { 
                           print(e) # print error
                           return (NULL) # return NULL 
                     })
   # if it is not square matrix OR determinant is null, stop 
   if (nrow(mat) != ncol(mat) | is.null(det.val)) {
      stop(paste(nrow(mat), "x", ncol(mat), " matrix is not invertible."))
   } 
   # compute inversion of matrix
   im.val <- solve(mat) 
   # call setInverse function to cache inverse value
   x$setInverse(im.val)
   ## Return a matrix that is the inverse of "mat"
   im.val 
}
