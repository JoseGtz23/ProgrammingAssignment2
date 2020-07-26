## The first function set a list of functions that combined with the
## second function print the inverse matrix and save it in the
## first function.
## Sorry for my english, it is not my native lenguage

## This function creates a list of functions of the given matrix
## that can be used with other function to get and set information

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inv_mtx) im <<- inv_mtx
        getinv <- function() im
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## Given a list of functions of the function makeCacheMatrix, this function
## print the inverted matrix and saves it in the list of functions created.
## If it was used with the same list, it gives a message that indicates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached inverted matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setinv(im)
        im
}
