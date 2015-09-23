## R-Programming - Assignment 2
## Greg Potts
## Rev  Date            Comments
## 01   09/21/2015      Implemented 2 functions per Assignment 2 notes
## 02   09/22/2015      Modified code for matrix & inversion vs. vector & mean
## 03   09/23/2015      Updated comments
##
## This file contains two functions makeCacheMatrix and cacheSolve.
## The intent is to take a user provided matrix and compute the inverse.
## For large matrices this can be time consuming so we prefer to calculate
## once and then store the result for future reference.
##
## The first function performs four tasks
## 1) set - Provides option for user to redefine input matrix instead of matrix
##          passed to the makeCacheMatrix function
## 2) get - Returns the user defined matrix x that was passed to makeCacheMatrix
## 3) setinvmat - Stores the inverted matrix im
## 4) getinvmat - Retrieves the inverted matrix im

## The makeCacheMatrix defines the four functions described above.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmat <- function(solve) im <<- solve
        getinvmat <- function() im
        list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
## NOTE: Using list of the four functions makes them available for use.
}

## The function cacheSolve takes the input variable tied to the makeCacheMatrix
## defined above, checks to see if the inverse matrix has been calculated,
## (using the getinvmat function defined in the makeCacheMatrix function)
## returns a message to the screen if the inverse matrix is in memory, else
## it will take the input matrix and calculate the inverse and then use the
## function setinvmat to store the result.

cacheSolve <- function(x, ...) {
        im <- x$getinvmat()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvmat(im)
        im
        ## Return a matrix that is the inverse of 'x'
}

