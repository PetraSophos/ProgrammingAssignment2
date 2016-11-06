## This function accepts a matrix, calculates the inverse, and caches it

## This returns a list of functions: set, get, setinv, and getinverse. Additionally, this
## object takes advantage of lexical scoping by storing the variable values inside the list for
## future reuse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get,
              setinv = setinv,
              getinverse = getinverse)
}


## This function requires input to be in the format returned by makeCacheMatrix
## It makes use of the cached value if one exists for the matrix specified in the aforementioned function.

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


