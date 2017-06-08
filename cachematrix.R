# 'makeCacheMatrix' takes a given matrix as argument and creates 4 other functions (set, get, setsolve, getsolve).
# Those functions are then combined into a list and returned, so that 'cacheSolve' can access all of them.
# 'makeCacheMatrix' is used as environment to set up, save and load the inverted matrices.

makeCacheMatrix <- function(x = matrix()) {
    # z gets set up as NULL. This is a placeholder for an eventually cached matrix.
    z <- NULL
    # set is actually never called in 'cacheSolve' and not really required for the functions to work.
    # It just lets you change the given matrix and sets up z again.
    set <- function(y) x <<- y; z <<- NULL
    # get just returns the matrix into 'cacheSolve' if it's a new matrix.
    get <- function() x
    # setsolve actually saves a matrix that has been been solved the first time during 'cacheSolve' to z.
    setsolve <- function(solve) z <<- solve
    # getsolve just returns z. If setsolve hasn't already saved a solved matrix it just returns the initial NULL.
    getsolve <- function() z
    # Finally, a list of all the previous functions is created and returned. 
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# Now cacheSolve gets the returned list as argument x and can call anyone of these functions specifically via x$...
# cacheSolve then proceeds to either get a cached inversed matrix or computes it if none is cached.

cacheSolve <- function(x, ...) {
    # m gets assigned the return-value of the getsolve function (NULL or a cached matrix)
    m <- x$getsolve()
    # If m does not get assigned NULL, the cached matrix gets returned and this function is finished. 
    if(!is.null(m)) {
        message("Retreiving cached data")
        return(m)
    }
    # If m got assigned NULL, the given matrix gets assigned to the data-variable
    data <- x$get()
    # Now 'cacheSolve' actually solves the matrix and stores the outcome in m
    m <- solve(data, ...)
    # Finally, the setsolve-function gets called and given the solved matrix (m) as its solve-argument.
    # This saves the solved matrix as z in the 'makeCacheMatrix' environment.
    x$setsolve(m)
    # Lastly, the solved matrix m is returned and the function is finished.
    m
}
