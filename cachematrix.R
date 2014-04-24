## These functions will cache the time consuming calculations
## of the inverse of a matrix. If the matrix is submitted for
## the first time the inverse will be calculated and stored in the cache. 
## If the invrse of the matrix is requested again the inverse matrix
## will be ´retrieved from the cache

## makeCacheMatrix
## this  function will create a set of functions that are 
## returned as a list (1. set the value of the matrix, 2. get the value 
## of the matrix, 3. set the value of the inverted matrix, 4. get the 
## value of the inverted matrix). 
## makeCacheMatrix also creates and stores
## variables in another environment (by searching up through the parent envirenment(s)) 
## using the <<- assignment operator. This is used to store the matrix (argument x) and the
## calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## create a variable to store the inverse
        ## this is in the parent environment of the functions defined below
        ## so that the <<- operator will find it
        invrt <- NULL

        ## function to set the matrix
        set <- function(y) {
                ## if the matrix y is identical to the one in the cache
                ## then skip (Assignment 2 asks not to recalculate the inverse
                ## if the matrix hasn't changed
                if (identical(x, y)){
                        message("matrix is identical to cached matrix")
                        return()
                }
                x <<- y
                invrt <<- NULL ## assigns NULL to inverted matrix
                               ## since a new or different matrix
                               ## has been set
        }

        ## function to get the matrix
        get <- function() x

        ## function to set the inverted matrix
        setInverse <- function(I) invrt <<- I

        ## function to get the inverted matrix
        getInverse <- function() invrt

        ## return list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve
## This function takes the object created by makeCacheMatrix,
## checks if the inverse matrix has already been calculated and is found in the cache,
## and calculates the inverse if necessary and places it in the cache

cacheSolve <- function(x, ...) {

        ## check if the inverse matrix has already been calculated
        inv <- x$getInverse()
        if(!is.null(inv)) {
                ## if so return it from the cache
                message("getting cached data")
                return(inv)
        }

        ## if nothing is in the cache retrieve the matrix
        ## and calculate the inverse using solve(X)
        data <- x$get()
        inv <- solve(data, ...)

        ## place the result into the cache
        x$setInverse(inv)

        ## return the result
        inv
}
