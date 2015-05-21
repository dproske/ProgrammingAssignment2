## makeCacheMatrix() and cacheSolve() together provide a means for
## caclulating and caching matrices and their inverses.

## makeCacheMatrix() takes a matrix as input and outputs a list
## of functions that get and set the matrix and its inverse, 
## Both of the set functions use the '<<-' operator so values can
## be assigned to the x and inv variables in other environments. 
## When created, the inv variable is set to NULL signifying that 
## the inverse has not yet been calculated. Also, if the matrix
## value changes (set() is called), the inverse is again set to 
## NULL. This is important because the value of inv determines 
## whether cacheSolve() recaculates the inverse or merely finds 
## the cached value. We only want the inverse to be calculated 
## when the matrix has changed.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(newInv) inv <<- newInv
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve() takes a list of functions defined in 
## makeCacheMatrix() as input and calculates the inverse of the
## matrix if it has not already been calculated (if inv is null)
## or outputs the existing matrix inverse which has been cached.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}


## Below is a sample script for testing the the two functions.

## INPUT
##
## m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
## flist <- makeCacheMatrix(m)
## cacheSolve(flist)

## EXPECTED OUTPUT
##
##      [,1] [,2]
## [1,]  -2  1.5
## [2,]   1 -0.5

## INPUT
##
## cacheSolve(flist)

## EXPECTED OUTPUT
##
## getting cached data
##      [,1] [,2]
## [1,]  -2  1.5
## [2,]   1 -0.5
