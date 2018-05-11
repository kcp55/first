## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The functon makeCacheMatrix takes as an input a matrix provided by the user,
##sets the value of the matrix, uses this to set the inverse of the matrix
##and finally retrieves the inverse matrix. The matrix object is cached.
#takes matrix as its input
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL ##initializes value of inv
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x ##gets the value of the matrix
    setInverse <- function(solveMatrix) inv <<- solveMatrix ##sets the value of the invertible matrix
    getInverse <- function() inv ##retrieves the value of the invertible matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##The function cacheSolve uses the output of makeCacheMatrix() as an input
##First it checks whether or not this input matrix has values, and if it does, 
##cacheSolve returns the message "getting cached data" and returns the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv     
}
