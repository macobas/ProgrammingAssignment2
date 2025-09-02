## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix is a function that creates a matrix object that can store its inverse,
#so the inverse doesn't have to be calculated again.
makeCacheMatrix <- function(x = matrix()) {
      
                inv <- NULL  # Step 1: Initialize the inverse as NULL (no value stored yet)
                
               
                set <- function(y) {  # set() Updates the matrix x and resets the cached inverse to NULL
                        x <<- y #<<- assigns to variables in the parent environment
                        inv <<- NULL  # Resets the inverse if the matrix is changed.
                }
                
                get <- function() x #Returns the current matrix.
                
                setinverse <- function(inverse) inv <<- inverse #Stores the calculated inverse
                
                getinverse <- function() inv. #Returns the cached inverse
                
                list(set = set,
                     get = get,
                     setinverse = setinverse,      #Returns a list of the above functions
                     getinverse = getinverse)
        }
        


##cacheSolve

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()  # try to get the cached inverse of the matrix
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv) #If it exits, return it (no need to recalculated)
                }
                
                mat <- x$get() #Get the original matrix
                inv <- solve(mat, ...)  #Calculate the inverse of the matrix using solve() function
                x$setinverse(inv)     #Stores the newly inverse for the future use
                inv #Return the new inverse
        }
        

