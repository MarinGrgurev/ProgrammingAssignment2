## Title: Caching inverse of a matrix'
## Author: Marin Grgurev
## Date: August 27, 2014
##-----------------------
## As a requirement for the Programming Assignment 2: Lexical scoping from the 
## Coursera course "R Programming"

## Caching inverse of a matrix  creates a special "matrix" object that can cache its 
## inverse and actually consist of two functions. First function (makeCacheMatrix) creates
## a list with functions to set and get matrix to calculate as well as its cached inverse
## values. Second function computes the inverse of the special "matrix" returned by 
## first function.

## Function: makeCacheMatrix
## makeCacheMatrix takes as argument matrix and creates list with following functions:
##      1) function setMatrix() which resets cache to NULL and define function for
##         storing matrix
##      2) function getStoredMatrix() to get stored matrix entered as an argument
##      3) function setInverseMatrix() to calculate inverse matrix and store it in 
##         the cache
##      4) function getInverseMatrix() to return the inverse matrix value from cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getStoredMatrix <- function() x  
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        list(setMatrix=setMatrix, getStoredMatrix=getStoredMatrix, 
             setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}

## Function: cacheSolve
## cacheSolve function computes the inverse of the special "matrix" object returned by 
## function makeCacheMatrix. If the matrix inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve will return the inverse from the 
## cache. It first get the current status of cache and checks if inverse matrix already 
## exists in cache. If it exists it returns its value and function ends with message 
## "getting cached data". If inverse matrix is not found in cache function further gets
## the original matrix passed to the function, calculates inverse matrix, stores it 
## in the cache and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getStoredMatrix()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}