##
## cachematrix.R
##
## author: Kimberly Pickard
## date:   11/01/2020
##
## Programming Assignment #2: Lexical Scoping 
##
## ---------------------------------------------------------------------------------
## Overview:
##
## It is easy to have a matrix X, and compute the inverse of that matrix,
## INV_X, by using the function solve(). However, solve() runs slowly when
## X is a large matrix.
##
## To address this issue, we define the functions makeCacheMatrix() and
## cacheSolve() to cache the large matrix, compute its inverse 
## in cache, and return the cached result. 
## ---------------------------------------------------------------------------------


##
## makeCacheMatrix() puts the matrix 'X' into cache.
## Subsequent calls to cacheSolve() will invert the matrix in cache
## if it has not been solved previously.
##
## Arguments: X - a matrix that is invertible.
##
## Returns: A special list holding the following internal functions:
##              $set()        defines the cached matrix X 
##              $get()        used by cacheSolve() to get the cached matrix
##              $setsolve()   used by cacheSolve() to run the cached version of solve() 
##              $getsolve()   returns the cached inverse (default value is NULL)
## 
## The user takes this list and passes it as an argument to cacheSolve().
## ---------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
   
}


## 
## cacheSolve() solves for the inverse of a matrix, executing solve() within
## cached memory.
##
## Arguments: Call cacheSolve() with the list, which is returned by
## makeCacheMatrix().
##
## Returns: A matrix P, where P is the cached inverse of the matrix represented by
## the cached input matrix.
## 
## If the inverse has already been computed, then the function simply returns 
## the cached matrix, otherwise solve() is called and the work is done within
## cache.
## ---------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    p <- x$getsolve()
    if(!is.null(p)) {
        message("Already solved .... getting cached data")
        return(p)
    }
    data <- x$get()
    p <- solve(data, ...)
    x$setsolve(p)
    p
}












#####################################################################3

##example: 

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}



## example:
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

