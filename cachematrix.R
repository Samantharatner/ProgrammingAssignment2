## Put comments here that give an overall description of what your
## functions do

## This function will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    #solve(x) gives the inverse of the matrix, so I am using the solve function
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the matrix or retrieves the inverse
## if it has already been calculated

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}

#Test matrix to ensure the function works
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
my_matrix <- makeCacheMatrix(m1)
cacheSolve(my_matrix)
