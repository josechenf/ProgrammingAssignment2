## This code assumes that the supplied matrix is always invertible

#############################################################################


##makeCacheMatrix(MatrixObject)

## This function creates a special "matrix" that can cache its inverse to save
## computation times.

## The initialized "matrix" is actually a list that contains functions to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

        i <- NULL #initialize i to NULL, this will contain the inversed matrix
        
        ##1. This sub-function sets the values of the matrix
        set <- function(y) { 
                
                ##assigns values to objects in another environment through <<-
                x <<- y         
                i <<- NULL 
        }
        
        
        ## 2. This sub-function gets the value of the matrix
        get <- function() x
        
        
        ## 3. This sub-function sets the values of the inverse matrix
        setinv <- function(solve) i <<- solve
        
        
        ## 4. This sub-function gets the values of the inverse matrix
        getinv <- function() i
        
        
        ## This is the list of functions that get created for the matrix "x"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}




#######################################################################

##cacheSolve(MatrixObject)

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## This "matrix" must have the 4 functions defined above.

## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## First read cache for any stored data for matrix "x"
        i <- x$getinv() ##getinv() was defined above to return saved "i" (A.K.A. inverse) value in cache
        ##   x$getinv() will find a saved value "i" for a matrix "x"
        
        if(!is.null(i)) { ##if "i" is not NULL it means the matrix "x" has been solved before
                message("getting cached data") 
                return(i) ##return saved value to save computational time
        }
        
        
        ## if there is no saved value "i" for matrix "x"
        
        data <- x$get() ##make a new matrix called "data" that stores the matrix "x"        
        i <- solve(data, ...) ##solve the matrix "data" for its inverse and store it in "i" 
        x$setinv(i) ##store the inversed matrix into cache
        ## this is done by using the function setinv(i) which stores "i" into an object of another environment
        
        i ## return i (the inverse)   
        
}

###############################################################################333
## To run these functions:

## 1. Initialize a matrix (must be invertible). Example: matrix <- matrix(c(1, 0, 0, 1),2,2)
## In this example the matrix is the 2x2 identity matrix, and the inverse is the same matrix.

## 2. Create the special matrix and its functions. Example: m <- makeCacheMatrix(matrix)

## 3. Find the inverse of the special matrix a first time through the cacheSolve function. 
## Example: cacheSolve(m)

## 4. Now this result has been saved to the cache. If we were to calculate the inverse for the same matrix:
## Example: cacheSolve(m)
## The answer would be read from cache (we get the message saying "getting cached data")

#############################################################################################


## As a conclusion, what we are actually doing is the following:

## Step 1:
## Creating a new object (or "matrix") that stores important values and functions as a list.
## This new object holds the following:
## 1. original matrix, accessed through object_name$get()
## 2. previously calculated inverse matrix in memory (or NULL if new), accessed through object_name$getinv()
## 3. A function to save inverse matrices into memory, accessed through object_name$setinv(inverse_matrix)
## 4. A function to replace the original matrix, accessed through object_name$set(replacement_matrix)


## Step 2:
## When calculating an inverse, first checking if there is an inverse stored in memory: object_name$getinv()
## If there is, return that, and say "getting cached data"

## If there is nothing stored in memory (NULL), 
##              Save the original matrix into environment: data <- object_name$get()
##              Get the inverse through normal computations: i<-solve(data)
##              Save that into memory: object_name$setinv(i)
##              Return the inverse that was just calculated.


#### Note that: The memory that it is actually being stored in is inside the object created through makeCacheMatrix
#### Even if a matrix is numerically equal, there will be no cache stored for this new matrix
#### And the inverse will be calculated normally.
