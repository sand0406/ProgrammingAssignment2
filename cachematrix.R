## Put comments here that give an overall description of what your
## functions do

## First, run the function makeCacheMatrix to create an environment, with the nessecary functions 
## to store the inverse matrix. NOTE that this function does not produce the inverse matrix.
## The makeCacheMatrix function takes an invertible matrix as an input, i used:
## test_matrix <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2), as my test argument.
## This matrix was sugested by Alan E. Berger on the week 3 forum.
## Store the output from makeCacheMatrix under a variable of choice, this will serve as argument in the second function.

## Secondly, run the function cacheSolve, this function first tries to fetch the inverse matrix
## from makeCacheMatrix, if there is no saved inverse matrix, the function will proceed
## to fetch the data (the input matrix "test_matrix") from makeCacheMatrix and produce the 
## inverse matrix. 
## before returning the inverse matrix, it is also stored in the first function makeCacheMatrix.

## Write a short comment describing this function
## Takes a invertible matrix as input and subsequently stores inverted version of this matrix

makeCacheMatrix <- function(x = matrix()) {
     im <- NULL
     set <- function(y) {
          x <<- y
          im <<- NULL
     }
     get <- function() x
     saveinverse_m <- function(solve) im <<- solve
     fetchinverse_m <- function() im
     list(set = set, get = get,
          saveinverse_m = saveinverse_m,
          fetchinverse_m = fetchinverse_m)
}

## Write a short comment describing this function
## Takes the output of the first function as an argument, checks it environment for a saved 
## inverted matrix, fetches it if its there, or otherwise fetches the data and calculates the 
## inverse matrix, then stores it.

cacheSolve <- function(x, ...) {
     im <- x$fetchinverse_m()
     if(!is.null(im)) {
          print("fetching inverse matrix from cache")
          return(im)
     }
     data <- x$get()
     im <- solve(data, ...)
     x$saveinverse_m(im)
     im
     ## Return a matrix that is the inverse of 'x'
}
