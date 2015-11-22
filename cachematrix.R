## makeCacheMatrix stores a list of functions
makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        
        ##sets the matrix to be solved and assigns it to the environment
        set_m <- function(y){
                z <<- y
                m_inv <<- NULL
        }
        
        ##returns the matrix
        get_m <- function() z
        
        ##solves the matrix and assigns to environment
        set_m_inv <- function() m_inv <<- solve(z)
        
        ##returns the solved matrix
        get_m_inv <- function() m_inv
        
        ##creates the list of functions that can be called 
        list(set_m = set_m, 
             get_m = get_m,
             set_m_inv = set_m_inv,
             get_m_inv = get_m_inv)
}

## Calculates the inverse of matrix mat_in
## First checks if mat_in is same as cached matrix. If same retrieves
##  cached inverse.
cacheSolve <- function(mat_in, ...) {
        
        #function that solves for the matrix, to be used as needed
        solv_m <- function (matrix){
                m$set_m(mat_in)
                message("solving matrix")
                mat_inv <- m$set_m_inv()
        }
        
        #gets the cached matrix from makeCacheMatrix
        #code will break here if makeCacheMatrix not initialized with
        #name "m"
        mat_ca <- m$get_m()
        
        #if ncol & nrow of mat_in & mat_ca are not equal
        #solve mat_in
        if(nrow(mat_ca) != nrow(mat_in) || ncol(mat_ca) != ncol(mat_in)){
                mat_inv <- solv_m(mat_in)
                return(mat_inv)
        }
        #if mat_in & mat_col are equal, get cached-solved matrix 
        if (all(mat_in == mat_ca)) { 
                message("getting cached data")
                mat_inv <- (m$get_m_inv())
                return(mat_inv)
        }
        #solve mat_in
        mat_inv <- solv_m(mat_in)
        mat_inv
}
