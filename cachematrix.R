## This function contains two sub-functions: 
## 1. makeCacheMatrix
## 2. cacheSolve
## they intend to calculate the inverse of a matrix in a way:
## -- if it's calculated before, use the cached result
## -- otherwise, calculate it and cache it for next calculation

## To run the script:
## 1. source(''cachematrix.R)
## 2. matrix <- makeCacheMatrix(matrix(c(2, 1, 0, 2), c(2, 2)))
## 3. cacheSolve(matrix)
##       [,1] [,2]
## [1,]  0.50  0.0
## [2,] -0.25  0.5


## makeCacheMatrix creates a special matrix, 
## which contains the function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inversed matrix
## 4. get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## variable which holds the cached matrix
    cache <- NULL
    
    ## set the value of matrix
    set <- function(y) {
        x <<- y
        # flush the cache after assignment
        cache <<- NULL
    }
    
    ## get cached matrix
    get <- function() x
    
    ## set the value of inversed matrix
    setInverse <- function(inverse) cache <<- inverse
    
    ## get the value of inversed matrix
    getInverse <- function() cache
    
    ## return the list
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
    ## firstly check if result is cached or not
    inverse <- x$getInverse()
    # if cached, return it
    if(!is.null(inverse)) {
        message("get cached matrix")
        return(inverse)
    }
    
    ## if not cached, get the inverse of matrix
    ## and cache it
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    
    ## return the reversed matrix
    inverse
}
