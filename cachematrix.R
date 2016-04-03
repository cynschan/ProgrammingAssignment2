### cachematrix has a pair of functions that cache the inverse of a a matrix


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## takes a matrix argument
## returns a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setInv <- function(solve) invm <<- solve
    getInv <- function() invm
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## assumes a square matrix
## if the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    invm <- x$getInv()
    if(!is.null(invm)){
        message ("getting cached data")
        return (invm)
    }
    data <- x$get()
    invm <- solve(data,...)
    x$setInv(invm)
    invm
}
