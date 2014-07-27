## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a "matrix", which is a list containing functions
## to set and get the matrix and to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    # sets the matrix
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
  
    # returns the matrix
    get <- function() x
  
    # sets the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
  
    # gets the inverse of the matrix
    getInverse <- function() inv
  
    # combines everything into a list
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)

}


## checks wheter the inverse of the matrix is cached or not. If it's cached
## return that value, if not calculate the inverse, cache and return it

cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <-x$getInverse()
    
    # check if it is null, if not, return the value found
    if (!is.null(inv)) {
        message("cached inverse found")
        return(inv)
    }
    
    # get the matrix and solve if
    data <- x$get()
    inv <- solve(data)
    
    # cache the result
    x$setInverse(inv)
    
    # return it
    inv
}
