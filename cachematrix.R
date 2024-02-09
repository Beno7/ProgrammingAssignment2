## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix returns a combination of functions in a form of a list object.
# The functions from the list facilitates matrix storage and inverse caching.
# 
# cacheSolve calculates the inverse of a matrix within the object returned from 
# makeCacheMatrix.
# First attempts to retrieve cached data. If data is not present in cache,
# calculates inverse instead, then subsequently caches the inverse
# into the makeCacheMatrix object instance.
#
# Included in this file are few test cases (commented out) validating the
# behavior of the functions above.
# The tests utilizes function testFn (also commented out).

## Write a short comment describing this function

# makeCacheMatrix is a factory that creates a matrix with setters / getters
# methods and inverse value added to the managing object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Setter for setting matrix y into x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # setter for setting / caching the inverse of matrix x
  setinv <- function(a) inv <<- a
  # getter for getting / caching the inverse of matrix x
  getinv <- function() inv
  # Returning setters and getters packed together in a list (managing obj)
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# Returns the inverse of an occurrence of the special factory
# created via the factory makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Get cached inverse
  data <- x$getinv()
  if (is.null(data)) { # Calculate inverse if not yet cached
    data = x$get()
    data <- solve(data, ...)
    x$setinv(data) # Cache Inverse
  }
  data # Return inverse
}


# A function that tests the functionalities of makeCacheMatrix and cacheSolve
# testFn <- function(testm) {
#   print('Testing ======== ')
#   print(testm)
#   testout <- makeCacheMatrix(testm)
#   stopifnot(all.equal(testm, testout$get()))
#   stopifnot(is.null(testout$getinv()))
#   stopifnot(all.equal(solve(testm), cacheSolve(testout)))
#   stopifnot(all.equal(solve(testm), testout$getinv()))
#   print(testout$getinv())
#   print('End ========')
# }
# testFn(matrix( c(2, 2, 3, 2), nrow = 2, ncol = 2)) # c(-1, 1, 1.5, -1)
# testFn(matrix( c(-1, 1, 1.5, -1), nrow = 2, ncol = 2)) # c(2, 2, 3, 2)
# testFn(matrix( c(-3, 5, 1, 0), nrow = 2, ncol = 2)) # c(1, 0, 0, 1)
# testFn(matrix( c(1, 0, 0, 1), nrow = 2, ncol = 2)) # c(-3, 5, 1, 0)





