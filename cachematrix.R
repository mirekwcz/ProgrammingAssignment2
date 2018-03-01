## Matrix inversion operation can be computationally intensive and there may be
## some benefit to caching the inverse of a matrix, rather than compute it
## repeatedly. The following two functions compute and then cache the inverse
## of a given matrix.
## IMPORTANT: it is assumed here that the matrix supplied is always invertible.

## Function #1: this function creates a special "matrix" object that can cache
## its inverse. Specifically, the "matrix" object is a list containing
## the following functions:
## 1. set_matrix() and get_matrix() - these functions set and get the value of a supplied
##    matrix, respectively.
## 2. set_inverse() and get_inverse() - similarly, these functions set and get
##    the inverse of the supplied matrix.

makeCacheMatrix <- function(x = matrix())
{
    inv_mat <- NULL
    set_mat <- function(mat)
    {
        x <<- mat
        inv_mat <<- NULL
    }
    get_mat <- function()
    {
        x
    }
    set_inv <- function(inv)
    {
        inv_mat <<- inv
    }
    get_inv <- function()
    {
        inv_mat
    }
    list(set_matrix=set_mat, get_matrix=get_mat, set_inverse=set_inv, get_inverse=get_inv)
}

## Function #2: this function calculates and returns a matrix that is
## the inverse of a user provided matrix 'x'.
## Before calculating the inverse matrix, however, cacheSolve() first
## verifies, if the inverse matrix has already been calculated for the current
## matrix. If so, cacheSolve() skips the computation and retrieves
## its cached value instead.

cacheSolve <- function(x, ...)
{
    inv_mat <- x$get_inverse()
    if (!is.null(inv_mat))
    {
        message("accessing cached data...")
        return (inv_mat)
    }
    message("computing inverse matrix...")
    mat <- x$get_matrix()
    inv_mat <- solve(mat, ...)
    x$set_inverse(inv_mat)
    inv_mat
}

# Usage example:
# source("cachematrix.R")
#
# m <- makeCacheMatrix(matrix(1:4, 2, 2))
# m$get_matrix()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# m$get_inverse()
# NULL
#
# cacheSolve(m)
# computing inverse matrix...
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# call the same function again...
# cacheSolve(m)
# accessing cached data...
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# m$get_inverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# setting a new matrix...
# m$set_matrix(matrix(c(1, 2, 1, 3, 2, 3, 7, 5, 2), 3, 3))
# m$get_matrix()
#      [,1] [,2] [,3]
# [1,]    1    3    7
# [2,]    2    2    5
# [3,]    1    3    2
#
# m$get_inverse()
# NULL
#
# cacheSolve(m)
# computing inverse matrix...
#       [,1]  [,2]  [,3]
# [1,] -0.55  0.75  0.05
# [2,]  0.05 -0.25  0.45
# [3,]  0.20  0.00 -0.20
 
