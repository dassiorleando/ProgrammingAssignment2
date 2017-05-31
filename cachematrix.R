## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that helps to cache inverse of a matrix
makeCacheMatrix <- function(mat = matrix()) {
    invmat <- NULL
    setmatrix <- function(y){
        mat <<- y
        invmat <<- NULL
    }
    getmatrix <- function() mat
    setinvmatrix <- function(matrix) invmat <<- matrix
    getinvmatrix <- function() invmat
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## Caculate the inverse of the special matrix created with the makeCacheMatrix function
## If the inverse has already been calculated we return the cached value
## If not we calculate it and cached the new value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinvmatrix()
    
    if(!is.null(invmat)){
        message("getting cached inversed matrix")
        return(invmat)
    }
    data <- x$getmatrix()
    newinvmat <- solve(data, ...)
    x$setinvmatrix(newinvmat)
    newinvmat
}
