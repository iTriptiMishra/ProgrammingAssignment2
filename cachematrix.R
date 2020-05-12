#Objective : To cache the inverse of a matrix

# Following functions are created to achieve the objective:
        
# 1. `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

# 2. `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.


# Function 1 : makeCacheMatrix

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        #set the value of the Matrix
        #`<<-` operator which can be used to assign a value to an object in an environment that is 
        #different from the current environment.
        setMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        #getting value of original matrix
        getMatrix <- function() x      
        
        #setting value of the inverse of matrix
        setInverse <- function(inverse) inverseMatrix <<- inverse  
        
        #getting value of the inverse matrix
        getInverse <- function() inverseMatrix    
        
        #returning list containing values of the functions created above
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse, 
             getInverse = getInverse)
}


# Function 2 : cacheSolve

# The second function calculates the inverse of the special matrix
# created with the above function. However, it first checks to see if the
# inverse of the matrix has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setInverse`
# function.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
        inverseMatrix <- x$getInverse()
        
        #If the inverse matrix is NOT empty and 
        #the inverse of the matrix has already been calculated
        if(!is.null(inverseMatrix)) {
                message("Getting cached inverse matrix for ya!")
        }
        
        #If the inverse matrix is empty and 
        #the inverse of the matrix has not been calculated yet
        else {
                #getting the original matrix
                dataMatrix <- x$getMatrix()
                
                # Computing the inverse of a square matrix can be done with the `solve` function in R
                inverseMatrix <- solve(dataMatrix, ...)
                
                # Setting the value with inverse matrix
                x$setInverse(inverseMatrix)
        }
        return(inverseMatrix)
}


#Testing the functions
# > originalMatrix <- matrix(c(1,2,3,4),2,2)
# > originalMatrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cachedMatrix <- makeCacheMatrix(originalMatrix)
# > cachedMatrix$getMatrix()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cachedMatrix$getInverse()
# NULL
# > cacheSolve(cachedMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cachedMatrix)
# Getting cached inverse matrix for ya!
#         [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

