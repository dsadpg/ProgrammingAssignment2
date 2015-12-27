# makeCacheMatrix creates m as an empty or NULL matrix.
# It also creates its four children functions
# and returns them as a callable list of functions.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL # this is where the result of inversion is stored
        
        set <- function(y) { # changes the matrix stored in the main function
                x <<- y
                m <<- NULL
        }
        
        get <- function() x     # returns the matrix x stored in the main function.
        # Doesn't require any input.
        
        setInv <- function(inv) m <<- inv # set the inversed matrix
        
        getInv <- function() m  # return the inversed matrix
        
        list(set = set, get = get, # return a list that contains these functions
             setInv = setInv,
             getInv = getInv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) { # the input of cacheSolve is the object where makeCacheMatrix is stored.
        
        m <- x$getInv() # get the inversed matrix from object x
        
        if(!is.null(m)) { # verify if the value m exists and is not NULL
                message("getting cached data")
                print(m)
                return(m) # return the calculated inversion matrix
        }
        
        objeto <- x$get() # if not, we need to get the matrix object
        
        m <- solve(objeto) # "solve" computes the inverse of the matrix 
        
        x$setInv(m) # we then set it to the object
        
        print(m)
        
        m # return the result
}