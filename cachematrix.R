## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    newx <- TRUE
    set <- function(y) {
        x <<- y
        invx <<- NULL
        newx <<-TRUE
    }
    get <- function() x
    setInverse <- function(invM) {
        invx <<- invM
        newx <<- FALSE
    }
    getInverse <- function() {
        if(newx==FALSE) invx 
        else NULL
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx <- x$getInverse()
    if(!is.null(invx)) {
        message("getting cached matrix inverse")
        return(invx)
    }
    A <- x$get()
    invA <- solve(A)
    x$setInverse(invA)
    invA
}
