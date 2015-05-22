## The function makeCacheMatrix() creates a list of getter and setter
#  methods for the matrix given in the functions parmeter x.
#  The method provides getter and setter functions the change or retrive 
#  the matrix capsuled by the function.
#  It also allows getter and setter functions for the inverted version of the
#  matrix capsuled by the function.
#  The setter functions for the inverted matrix is meant to be used by 
#  the function cacheSolve (see below).
#  To cache a valid inverted matrix the function cacheSolve has to be
#  called at least once for an change in the capsuled matrix.

makeCacheMatrix <- function(x = matrix()) {
    # init i
    i <- NULL
    
    #getters and setters for the base matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    #getters and setters for the inverted matrix
    setInverted <- function(inverted) i <<- inverted
    getInverted <- function() i
    
    #returns a list of getter and setter functions
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}




## cacheSolve() works on the list of getter and setter methods as a parameter provided 
#  by the function makeCacheMatrix(). The function checks if the inverted matrix 
#  of the capsuled basic matrix is already stored. It returns the stored inverted matrix
#  if it had been already calculated. Otherwise the inverted matrix is calculated first 
#  and than returned.

cacheSolve <- function(x, ...) {
    i <- x$getInverted()
    
    # check if inverted matrix is already chached
    if(!is.null(i)) {
        message("getting cached inverted matrix")
        # if YES return the chached inverted matrix
        return(i)
    }
    # if NO calculate the inverted matrix of the capsuled basic matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setInverted(i)
    i    
}



