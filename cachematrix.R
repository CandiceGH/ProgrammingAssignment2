## Assignment 2: Sep 2015
## These R functions are able to cache potentially time-consuming computations. 
## If the contents of a matrix are not changing, it may make sense to cache the 
## computation again so that when we need it again, it can be looked up in the 
## cache rather than recomputed. This function takes advantage of the scoping 
## rules of the R language and how they can be manipulated to preserve state 
## inside of an R object.

## Below are two functions that are used to create a special object that stores 
## a cache matrix and its inverse.

## The first function, makeCacheMatrix, creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set the matrix inverse variable to NULL
        xinv <<- NULL
        ## create the set function to set the value of the matrix
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        ## create the get function to get/recall the value of the matrix
        get <- function() x
        ## create the getInv function to get/recall the Inverse of the matrix
        getInv <- function(get) xinv
        ## create a list of function for the object function 'makeCacheMatrix'
        list(set = set, get = get,
             getInv = getInv)
}

## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has previously been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from the cache
## and prints "getting cached data".

cacheSolve <- function(x, ...) {
        ## Calls the getInv function to checks if the inverse of 'x' has already 
        ## been calculated 
        xinv <<- x$getInv()
        ## If the inverse of 'x' has been calculated (i.e. xinv is not 'null'), print 
        ## message and return the inverse matrix of 'x'
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        ## If the matrix of 'x' is new and not resolved/inverted, continue and 
        ## calculate the inverse of the matrix of 'x' using the Solve function
        data <- x$get()
        ## assign the result to 'xinv' variable/matrix
        xinv <<- solve(data, ...)
        ## return the function matrix 'xinv' that is the inverse of 'x'
        xinv
}






