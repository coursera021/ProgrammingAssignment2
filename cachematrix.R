# Computes the inverse of a matrix, it uses an auxiliary structure to cache
# the inverse matrices commonly used in sequence. It is assumed that this is
# the common case, so it is worth it to optimize and avoid recompute the
# same inverse matrix multiple times

# This function is a container of the matrix and its inverse matrix
# x: original matrix
# inv: its inverse matrix (when set by setinverse())
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL;
    
    # sets a matrix and resets its cache
    set <- function(m)
    {
        x   <<- m
        inv <<- NULL
    }
    
    # gets the matrix
    get <- function() x
    
    # set the inverse matrix
    setinverse <- function(m) inv <<- m
    
    # gets the inverse matrix stored in the func's cache
    getinverse <- function() inv

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Returns the inverse matrix of matrix x
cacheSolve <- function(x, ...)
{
    ret <- x$getinverse()
    if(!is.null(ret))
    {
        message("cached")
        return(ret)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
