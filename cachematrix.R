## makeCacheMatrix takes a matrix as input and outputs a list. It clears the cache for the calculated
## inverse when it is called, and the list itself contains functions for setting the inverse and storing
## it for later retrieval.
## cacheSolve takes the output of makeCacheMatrix and computes the inverse of the matrix. If the inverse
## has been recently computed without recalling the makeCacheMatrix function, it retrieves the stored daa
## for inverse. Note that for changing the matrices, the $set function can be called to change matrices
## and will reset the stored value for the inverse. Keep in mind that if the matrix is changed without using
## the $set function or without recalling makeCacheMatrix, the stored inverse value will not change.

## Input a matrix and outputs a list

makeCacheMatrix <- function(x = matrix()) {
        
        
        i <<- NULL ## Set i equal to NULL whenever this function is called
        
        get <- x  ## Sets input matrix as 'get'
        
        set <- function(y) { ## Allows you to change the matrix with x$set('new matrix')
                x <<- y
                i <<- NULL
        }

        setinverse <- function(inverse){  ## When setinverse is called it sets the inverse as i
                i <<- inverse
        }
        
        getinverse <- function() { ## When getinverse is called it displays i
                i
        }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## Sets up a list so that functions
                                                                          ##can be called within an object
        
}


## Takes the output of makeCacheMatrix and calculates the inverse. If inverse already exists, the function
## takes the stored value and displays that instead. 

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse() ## Return a matrix that is the inverse of 'x'if 
                            ## it exists.
        
        if(!is.null(i)) {  ## If inverse has been calculated already, display a message and the inverse
                message("getting cached data")
                return(i)
        } else { 
                data <- x$get   ## retrieve original matrix stored in $get
                i <- solve(data)  ## Apply solve function and store results in i
                x$setinverse(i) ## run setinverse function for i
                i                 ## Display i
        }

      
}
