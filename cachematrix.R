## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that helps to cache inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) mat <<- matrix
    getmatrix <- function() mat
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Caculate the inverse of the special matrix created with the makeCacheMatrix function
## If the inverse has already been calculated we return the cached value
## If not we calculate it and cached the new value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getmatrix()
    
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data)
    x$setmatrix(mat)
    mat
}
