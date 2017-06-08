# 'makeCacheMatrix' takes the given matrix as argument and creates 3 other functions (get, setsolve, getsolve).
# Those functions are then combined into a list and returned, so that 'cacheSolve' can access all of them.

makeCacheMatrix <- function(x = matrix()) {
    # z gets set up.
    z <- NULL
    # The get function just returns the matrix again to get the data into 'cacheSolve' if it's a new matrix.
    get <- function() x
    # setsolve actually saves a matrix that has been been solved the first time during 'cacheSolve' to z.
    setsolve <- function(solve) z <<- solve
    # getsolve just returns z. If setsolve hasn't already saved a solved matrix it just returns the initial NULL.
    getsolve <- function() z
    # Finally, a list of all the previous functions is created and returned. 
    list(get = get, setsolve = setsolve, getsolve = getsolve)
}

# Now cacheSolve gets this list as argument x and can call one of these functions specifically via x$...

cacheSolve <- function(x, ...) {
    # m gets assigned the return-value of the getsolve function.
    # This can either be NULL if there is no cached matrix available...
    # ...or it can return a cached matrix if 'cacheSolve' has already solved the given matrix before.
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
    # So, next time said function gets called it assigns this solved matrix to z and it's not NULL anymore.
    # This leads to m also not being NULL when z is returned via x$getsolve().
    # Hence 'cacheSolve' just returns the saved matrix due to the if-clause being true.
    x$setsolve(m)
    # Lastly, the solved matrix m is returned and the function is finished.
    m
}
