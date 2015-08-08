## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a support function that returns a list of functions to
# support matrix inversion by caching the previous matrix and its inverse
# thus if called twice with the same matrix it will quickly return the
# previous calculation

makeCacheMatrix <- function(m = matrix())
{
    inputMatrix <- NULL
    set <- function(m)
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

## Find the inverse of the matrix in the cache

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
    x$setinverse(m)
    m
}

# test the cache system
test <- function(n=100, ...)
{
    # initialize the function list
    fn.list <- makeCacheMatrix()

    # get a test matrix
    a <- matrix(rnorm(n*n), nrow=n)

    # set the matrix in the cache manager
    fn.list$set(a)

    # get the inverse, timing it
    start.time <- Sys.time();
    b=cacheSolve(fn.list)
    end.time <- Sys.time()
    nocache.time <- end.time - start.time

    # now time it again, this time answer is in cache
    start.time <- Sys.time();
    b=cacheSolve(fn.list)
    end.time <- Sys.time()
    cache.time <- end.time - start.time

    # show timing difference
    speedup = as.numeric(nocache.time) / as.numeric(cache.time)
    sprintf("no cache: %.5f sec, cache: %.5f, speed up: %.1fX",
            nocache.time, cache.time, speedup)

}
