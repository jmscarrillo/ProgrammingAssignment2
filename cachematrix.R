## Coursera - Data Science - Universidad Johns Hopkins
## R Programming - Week 3
## Programming Assignment 2: Lexical Scoping
## José Mª Sebastián Carrillo

## Function that stores a matrix and a calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Author <- "José Mª Sebastián Carrillo"
    
    # Initialize the variable with the inverse matrix (Default NULL)
    inverseMatrix <- NULL
    
    # Define a function called "set", which store the outside matrix and reset
    #   the value of the calculated inverse matrix
    set <- function(newMatrix) {
        # Store the outside matrix
        x <<- newMatrix
        # Default value (NULL) to the inverse matrix
        inverseMatrix <<- NULL
    }
    
    # Define a function called "get", which returns the outside stored matrix
    get <- function() x
    
    # Define a function called "setInverse", which stores the calculated inverse matrix
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    # Define a function called "get", which returns the calculated inverse matrix
    getInverse <- function() inverseMatrix
    
    # Constructs the list of internal functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that computes inverse matrix from one given, but if it's calculated
##   and stored previously, shows the stored data.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    # Author <- "José Mª Sebastián Carrillo"
    
    # Stores the outside matrix result using the function getInverse(), from makeCacheMatrix()
    inverseMatrix <- x$getInverse()
    
    # Determine if it was a previously data result stored...
    if (!is.null(inverseMatrix)) {
        # ...And so shows it, exiting the function (calculates anything)
        message("Getting cached data...")
        return(inverseMatrix)
    }
    
    # Stores the outside matrix using the function get(), from makeCacheMatrix()
    newMatrix <- x$get()
    
    # Only square matrix can calculate an inverse (columns == rows)...
    if (ncol(newMatrix) != nrow(newMatrix)) {
        message("Matrix is not square!")
        return(NULL)
    }
    
    # ...And only matrix with determinat != 0 can calculate an inverse.
    if (det(newMatrix) == 0) {
        message("Matrix determinant equals to 0!")
        return(NULL)
    }
    
    # Calculates the inverse of the outside matrix... 
    inverseMatrix <- solve(newMatrix, ...)
    # ...And stores it!
    x$setInverse(inverseMatrix)
    
    # Finally, returns the inverse of the original matrix
    inverseMatrix
    
}

## CONSOLE TEST CASES
# > testMatrix <- makeCacheMatrix(matrix(c(3,2,0,0,0,1,2,-2,1), 3, 3))
# > testMatrix$get()
# [,1] [,2] [,3]
# [1,]    3    0    2
# [2,]    2    0   -2
# [3,]    0    1    1
# > testMatrix$getInverse()
# NULL
# > cacheSolve(testMatrix)
# [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0
# > testMatrix$get()
# [,1] [,2] [,3]
# [1,]    3    0    2
# [2,]    2    0   -2
# [3,]    0    1    1
# > testMatrix$getInverse()
# [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0
# > cacheSolve(testMatrix)
# Getting cached data...
# [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0
# > testMatrix <- makeCacheMatrix(matrix(c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1), 4, 4))
# > testMatrix$get()
# [,1] [,2] [,3] [,4]
# [1,]    4    0    0    0
# [2,]    0    0    2    0
# [3,]    0    1    2    0
# [4,]    1    0    0    1
# > testMatrix$getInverse()
# NULL
# > cacheSolve(testMatrix)
# [,1] [,2] [,3] [,4]
# [1,]  0.25  0.0    0    0
# [2,]  0.00 -1.0    1    0
# [3,]  0.00  0.5    0    0
# [4,] -0.25  0.0    0    1
# > testMatrix$get()
# [,1] [,2] [,3] [,4]
# [1,]    4    0    0    0
# [2,]    0    0    2    0
# [3,]    0    1    2    0
# [4,]    1    0    0    1
# > testMatrix$getInverse()
# [,1] [,2] [,3] [,4]
# [1,]  0.25  0.0    0    0
# [2,]  0.00 -1.0    1    0
# [3,]  0.00  0.5    0    0
# [4,] -0.25  0.0    0    1
# > cacheSolve(testMatrix)
# Getting cached data...
# [,1] [,2] [,3] [,4]
# [1,]  0.25  0.0    0    0
# [2,]  0.00 -1.0    1    0
# [3,]  0.00  0.5    0    0
# [4,] -0.25  0.0    0    1
# > testMatrix <- makeCacheMatrix(matrix(1:6, 2, 3))
# > cacheSolve(testMatrix)
# Matrix is not square!
#     NULL
# > testMatrix <- makeCacheMatrix(matrix(1:9, 3, 3))
# > cacheSolve(testMatrix)
# Matrix determinant equals to 0!
#     NULL
