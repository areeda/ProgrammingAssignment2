## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a support function that returns a list of functions to
# support matrix inversion by caching the previous matrix and its inverse
# thus if called twice with the same matrix it will quickly return the
# previous calculation

makeCacheMatrix <- function(m = matrix())
{
    inputMatrix <- NULL
    set <- function(y)
    {
        inputMatrix <<- m
        inverseMatrix <<- NULL
    }
    get <- function() inputMatrix
    setinverse <- function(solve) inverseMatrix <<- solve
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
