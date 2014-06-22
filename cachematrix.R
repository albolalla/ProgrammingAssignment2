## makeCacheMAtrix: It's a list of functions that will be asigned to a 
## variable, which, in this case, will be a matrix.
## cacheSolve: It's a function which gets the matrix from makeCacheMatrix
##and calculates its inverse. It'll look for it in the environment, and if 
##it's already been done, it'll return the same result.


##We could say its a list of functions. Some of them work as arguments, not as proper 
##functions. 
##It assigns this list to a variable and creates a matrix within.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x                        
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


##It gets the inverse of the matrix created before, calculates its inverse or retrieves
##it from the cache if it's been calculated the previous time.
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
