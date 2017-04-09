## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. My assignment is to write a pair of functions that cache the inverse of a matrix.

##  makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    # set the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # get the matrix
    get <- function() x

    # set the inverse
    setInverse <- function(inverse) inv <<- inverse

    # get the inverse
    getInverse <- function() inv

    # list is used as the input to cacheSolve()
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##  cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
    # return: inverse of the original matrix input to makeCacheMatrix()
    inv <- x$getInverse()

    # if the inverse has already been caculated, get it from the cache and skips the computation
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    # otherwise, caculate the inverse
    # get the info an save it as an object, mat
    mat <- x$get()
    
    # solve, function to compute the inverse
    inv <- solve(mat, ...)

    # set the value of the inverse in the cache via the setInverse function
    x$setInverse(inv)
    
    # print it out
    return(inv)
}